(ns freememo.quick-sync
  "Background Quick Sync (Cmd-Shift-Opt-X): runs an Anki push using last-used
   settings without opening the modal. Reuses panels/AnkiSyncExecutor verbatim
   and reports progress via toasts. Own namespace per the JVM 64KB method-limit
   convention (Electric v3 e/defn). The hidden proxy button mounts only inside
   the toolbar, so the shortcut no-ops when no document is open."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.anki-sync-panels :as panels]
   [freememo.keyboard :as keyboard]
   #?(:clj [freememo.anki-sync-server :as sync])
   #?(:clj [freememo.toasts :as toasts])))

;; --- server wrappers (keep the reactive graph free of #?(:clj ...)) ---

(defn push-toast!*
  "Push a toast to the user's queue. Errors auto-sticky (toasts/push! default)."
  [user-id level message]
  #?(:clj (do (toasts/push! user-id {:level level :message message}) nil)
     :cljs nil))

(defn load-prefs*
  "Resolve last-used prefs the same way the modal's auto-load does:
   per-item mode → per-doc preset, falling back to global; otherwise global."
  [user-id root-topic-id auto-load-mode]
  #?(:clj (if (= auto-load-mode "per-item")
            (or (sync/load-item-preset user-id root-topic-id)
              (:prefs (sync/load-anki-preferences user-id)))
            (:prefs (sync/load-anki-preferences user-id)))
     :cljs nil))

;; --- headless executor ---
;;
;; The run is split across three sub-executors so each compiles to its own JVM
;; method, staying under the 64KB bytecode cap (see CLAUDE.md / electric-skill
;; "Method code too large"). QuickSyncExecutor is a thin parent that mounts all
;; three plus panels/AnkiSyncExecutor; the groups are reactively independent so
;; mount order does not matter.

(e/defn QuickSyncStartToasts
  "Start/busy toasts. One start toast per `!trigger` increment; one busy toast
   per `!busy-bump` increment (re-trigger while a run is in flight)."
  [user-id !trigger !busy-bump]
  (e/client
    (let [trigger (e/watch !trigger)
          busy-bump (e/watch !busy-bump)]
      (when (pos? trigger)
        (let [[t _] (e/Token [:qs-start trigger])]
          (when t
            (case (e/server (push-toast!* user-id :info "Syncing to Anki…")) (t)))))
      (when (pos? busy-bump)
        (let [[t _] (e/Token [:qs-busy busy-bump])]
          (when t
            (case (e/server (push-toast!* user-id :info "Sync already in progress")) (t))))))))

(e/defn QuickSyncConnectAndPush
  "Steps a–d: connect → apply prefs → resolve fields → (barrier) start push.
   Sets (:!phase sync) :pushing once fields resolve, handing off to
   panels/AnkiSyncExecutor."
  [user-id selected-doc conn form sync !trigger !prefs-applied?]
  (e/client
    (let [trigger (e/watch !trigger)
          prefs-applied? (e/watch !prefs-applied?)
          conn-status (e/watch (:!status conn))
          decks (e/watch (:!decks conn))
          models (e/watch (:!models conn))
          basic-model (e/watch (:!basic-model conn))
          cloze-model (e/watch (:!cloze-model conn))
          basic-fields (e/watch (:!basic-fields form))
          cloze-fields (e/watch (:!cloze-fields form))
          sync-phase (e/watch (:!phase sync))
          root-id (e/server (panels/get-root-topic-id* selected-doc))
          auto-load-mode (e/server (panels/get-anki-auto-load-mode* user-id))
          prefs (e/server (load-prefs* user-id root-id auto-load-mode))
          preferred-basic-fields (e/server (panels/resolve-preferred-fields* user-id root-id :basic))
          preferred-cloze-fields (e/server (panels/resolve-preferred-fields* user-id root-id :cloze))]

      ;; (a) Connect — fetch decks/models/tags once per trigger.
      (when (pos? trigger)
        (let [[t _] (e/Token [:qs-connect trigger])]
          (when t
            (case (helpers/run-fetch-config! conn) (t)))))

      ;; (b) Connected → apply last-used prefs once (fields owned by step c).
      (when (and (= conn-status :connected) (not prefs-applied?))
        (let [[t _] (e/Token [:qs-prefs trigger])]
          (when t
            (panels/apply-prefs! (dissoc prefs :basic-fields :cloze-fields)
              conn form decks models)
            (reset! !prefs-applied? true)
            (t))))

      ;; (c) Resolve field ordering against the (now final) models.
      (when (and prefs-applied? (= conn-status :connected) basic-model)
        (let [[t _] (e/Token [:qs-basic-fields trigger basic-model])]
          (when t
            (case (helpers/run-fetch-fields! basic-model (:!basic-fields form)
                    preferred-basic-fields) (t)))))
      (when (and prefs-applied? (= conn-status :connected) cloze-model)
        (let [[t _] (e/Token [:qs-cloze-fields trigger cloze-model])]
          (when t
            (case (helpers/run-fetch-fields! cloze-model (:!cloze-fields form)
                    preferred-cloze-fields) (t)))))

      ;; (d) Barrier — start push only after fields resolve to non-empty vectors
      ;; (initial [] / :loading both block; see plans/quick-sync.md risk note).
      (when (and prefs-applied?
              (vector? basic-fields) (seq basic-fields)
              (vector? cloze-fields) (seq cloze-fields)
              (nil? sync-phase))
        (let [[t _] (e/Token [:qs-push trigger])]
          (when t
            (reset! (:!phase sync) :pushing)
            (t)))))))

(e/defn QuickSyncOutcome
  "Steps f–g: success/error outcome toasts and connection-failure toast. Each
   clears !running? so the next shortcut press can start a fresh run."
  [user-id conn sync !trigger !running?]
  (e/client
    (let [trigger (e/watch !trigger)
          conn-status (e/watch (:!status conn))
          conn-error (e/watch (:!error conn))
          sync-phase (e/watch (:!phase sync))
          sync-result (e/watch (:!result sync))
          sync-error (e/watch (:!error sync))]
      (when (= sync-phase :done)
        (let [[t _] (e/Token [:qs-done trigger])]
          (when t
            (let [n (count (:pairs sync-result))
                  msg (if (zero? n) "Already up to date" (str "Synced " n " to Anki"))]
              (case (e/server (push-toast!* user-id :success msg))
                (do (reset! !running? false) (t)))))))
      (when (= sync-phase :error)
        (let [[t _] (e/Token [:qs-error trigger])]
          (when t
            (case (e/server (push-toast!* user-id :error (str "Anki sync failed: " sync-error)))
              (do (reset! !running? false) (t))))))
      (when (= conn-status :error)
        (let [[t _] (e/Token [:qs-conn-error trigger])]
          (when t
            (case (e/server (push-toast!* user-id :error (str "Cannot connect to Anki: " conn-error)))
              (do (reset! !running? false) (t)))))))))

(e/defn QuickSyncExecutor
  "Drives one background push per `!trigger` increment by mounting three
   sub-executors plus the shared phase machine (step e):
   start toasts → connect/apply-prefs/resolve-fields/start-push →
   panels/AnkiSyncExecutor → outcome toasts.
   Run-scoped flags (!trigger/!running?/!prefs-applied?/!busy-bump) are owned by
   QuickSyncButton so the click handler can reset them at start."
  [user-id selected-doc current-pdf-page conn form sync
   !trigger !running? !prefs-applied? !busy-bump]
  (e/client
    (QuickSyncStartToasts user-id !trigger !busy-bump)
    (QuickSyncConnectAndPush user-id selected-doc conn form sync !trigger !prefs-applied?)
    ;; (e) Reuse the modal's DOM-free phase machine: fetch cards → push → finalize.
    (panels/AnkiSyncExecutor user-id selected-doc current-pdf-page conn form sync)
    (QuickSyncOutcome user-id conn sync !trigger !running?)))

(e/defn QuickSyncButton
  "Hidden proxy button registered as keyboard/!quick-sync-btn-ref. Clicking it
   (via Cmd-Shift-Opt-X) starts a background push. Mounts the headless executor."
  [user-id selected-doc current-pdf-page card-type unsynced-count]
  (e/client
    (let [conn {:!status (atom :idle) :!error (atom nil) :!decks (atom [])
                :!models (atom []) :!selected-deck (atom nil)
                :!basic-model (atom nil) :!cloze-model (atom nil) :!all-tags (atom [])}
          form {:!scope (atom "Current Page") :!allow-dupes (atom false)
                :!use-header (atom false) :!header-text (atom "") :!use-tags (atom false)
                :!tags (atom []) :!basic-fields (atom []) :!cloze-fields (atom [])}
          sync {:!phase (atom nil) :!result (atom nil) :!error (atom nil) :!push-pairs (atom nil)}
          !trigger (atom 0)
          !running? (atom false)
          !prefs-applied? (atom false)
          !busy-bump (atom 0)]
      (dom/button
        (dom/props {:style {:display "none"} :aria-hidden "true" :tabindex "-1"
                    :data-quick-sync "1"})
        (reset! keyboard/!quick-sync-btn-ref dom/node)
        (e/on-unmount (fn [] (reset! keyboard/!quick-sync-btn-ref nil)))
        (dom/On "click"
          (fn [_]
            (if @!running?
              (swap! !busy-bump inc)
              (do
                (reset! !running? true)
                (reset! !prefs-applied? false)
                ;; Fresh connection + sync state for this run.
                (reset! (:!status conn) :connecting)
                (reset! (:!error conn) nil)
                (reset! (:!decks conn) [])
                (reset! (:!models conn) [])
                (reset! (:!all-tags conn) [])
                (reset! (:!basic-fields form) [])
                (reset! (:!cloze-fields form) [])
                (reset! (:!phase sync) nil)
                (reset! (:!result sync) nil)
                (reset! (:!error sync) nil)
                (reset! (:!push-pairs sync) nil)
                (swap! !trigger inc))))
          nil))
      (QuickSyncExecutor user-id selected-doc current-pdf-page conn form sync
        !trigger !running? !prefs-applied? !busy-bump))))
