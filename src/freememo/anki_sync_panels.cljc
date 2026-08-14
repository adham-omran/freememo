(ns freememo.anki-sync-panels
  "Executor + error/connected panels for Anki sync — separate namespace to stay
   below the JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.anki-sync-form :as form]
   [freememo.doc-context :as dctx]
   [freememo.modal-shell :as modal-shell]
   [freememo.command-bus :as bus]
   [freememo.logging :as log]
   #?(:clj [freememo.anki-sync-server :as sync])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.db :as db])))

(defn save-last-used-prefs!*
  "Persist the modal's current prefs as global last-used so the background Quick
   Sync executor picks them up. CLJ-only; keeps #?(:clj) out of the reactive graph."
  [user-id prefs-map]
  #?(:clj (do (settings/save-anki-sync-settings user-id prefs-map) nil)
     :cljs nil))

(defn trigger-quick-sync!
  "Dispatch the :quick-sync command to run a background push.
   No-op if the toolbar — hence its invoker handle — isn't mounted."
  []
  #?(:cljs (bus/dispatch! :quick-sync)
     :clj nil))

(defn get-anki-auto-load-mode* [user-id]
  #?(:clj (settings/get-anki-auto-load-mode user-id)
     :cljs nil))

(defn get-root-topic-id* [selected-doc]
  #?(:clj (db/get-root-topic-id selected-doc)
     :cljs nil))

(defn get-app-base-url* []
  #?(:clj settings/app-base-url
     :cljs nil))

(defn resolve-modal-prefs* [user-id root-topic-id]
  #?(:clj (sync/resolve-modal-prefs user-id root-topic-id)
     :cljs nil))

;; `AnkiSyncExecutor` lived here: it resolved a scope, fetched that scope's
;; cards, ran `do-anki-push!` over all of them and finalized. Every card it
;; touched moved the same direction, which is what overwrote Anki-side edits.
;; `anki-reconcile-panels/ReconcileExecutor` replaces it — same phase-machine
;; shape, but the direction is decided per card from each side's own baseline,
;; and the run is collection-wide because a deletion owed to Anki is not
;; discoverable from whichever document happens to be open.

(e/defn AnkiSyncErrorPanel
  "Error state with retry and cancel buttons.
   conn = {:!status :!error :!decks :!models :!selected-deck :!all-tags}"
  [conn !show-modal]
  (e/client
    (let [conn-error (e/watch (:!error conn))]
      (dom/div
        (dom/props {:style {:text-align "center" :padding "20px"}})
        (dom/div
          (dom/props {:style {:color "var(--color-danger-text)" :margin-bottom "var(--sp-3)"}})
          (dom/text (or conn-error "Connection failed")))
        (dom/div
          (dom/props {:style {:font-size "14px" :color "var(--color-text-secondary)" :margin-bottom "var(--sp-4)"}})
          (dom/text "Make sure Anki is running with the AnkiConnect plugin installed."))
        (dom/button
          (dom/props {:class "btn btn-primary" :style {:font-size "14px"}})
          (dom/text "Retry")
          (dom/On "click"
            (fn [_]
              (reset! (:!status conn) :connecting)
              (reset! (:!error conn) nil)
              (helpers/run-fetch-config! conn))
            nil))
        (dom/button
          (dom/props {:class "btn btn-secondary" :style {:font-size "14px" :margin-left "var(--sp-2)"}})
          (dom/text "Cancel")
          (dom/On "click" (fn [_] (reset! !show-modal false)) nil))))))

(defn pick
  "The preferred value when it's a valid option, else the first option.
   Owns the first-item fallback that run-fetch-config! used to do eagerly."
  [preferred options]
  (if (some #{preferred} options) preferred (first options)))

(defn apply-prefs!
  "Apply a resolved preferences map to form/conn atoms. deck is set to the
   preferred value if valid against the available list, else the first list
   item (pick) — run-fetch-config! no longer defaults it. Note types are
   app-owned, so no model/field selection is applied.
   `models` is accepted (call-site symmetry) but unused."
  [prefs conn form decks _models]
  (when (:scope prefs) (reset! (:!scope form) (:scope prefs)))
  (reset! (:!selected-deck conn) (pick (:deck prefs) decks))
  (when (some? (:allow-dupes prefs))
    (reset! (:!allow-dupes form) (:allow-dupes prefs)))
  ;; Header is no longer a form atom — it's per-PDF, loaded/saved by
  ;; HeaderSettings directly. apply-prefs! no longer touches it.
  (when (some? (:use-tags prefs))
    (reset! (:!use-tags form) (:use-tags prefs)))
  (when (:tags prefs)
    (reset! (:!tags form) (:tags prefs))))

(e/defn AnkiSyncConnectedPanel
  "Connected state: preset auto-load (per the user's auto-load mode), form, and status.
   conn = {:!decks :!selected-deck ...}
   form = {:!scope :!allow-dupes :!use-tags :!tags ...}
   sync = {:!phase :!result :!error :!push-pairs}"
  [user-id selected-doc conn form sync !show-modal]
  (e/client
    (let [decks (e/watch (:!decks conn))
          root-id (e/server (e/Offload #(get-root-topic-id* selected-doc)))
          ;; One-shot resolved prefs (preset/Settings/last-used) — single read.
          prefs (e/server (e/Offload #(resolve-modal-prefs* user-id root-id)))
          !ready? (atom false)
          ready? (e/watch !ready?)]

      ;; Resolve once the deck list (AnkiConnect) and prefs (server) are present;
      ;; apply the resolved selection, then release the readiness gate. The
      ;; `case` forces apply-prefs! to evaluate — as a bare non-last statement
      ;; its discarded return value is work-skipped by Electric (CLAUDE.md).
      (when (and (not ready?) (seq decks))
        (case (apply-prefs! prefs conn form decks nil)
          (reset! !ready? true)))

      (if-not ready?
        ;; Whole-form spinner until the selection is resolved — never paint a
        ;; wrong default first.
        (dom/div
          (dom/props {:style {:text-align "center" :padding "var(--sp-5)" :color "var(--color-text-secondary)"}})
          (dom/span (dom/props {:class "spinner"}))
          (dom/text "Loading…"))
        (dom/div
          ;; Indicator only when a per-doc preset was actually applied.
          (when (and (= (:mode prefs) "per-item") (:preset? prefs))
            (dom/div
              (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                                  :margin-bottom "var(--sp-2)" :font-style "italic"
                                  :animation "fade-in 0.3s ease-in"}})
              (dom/text "Using saved settings for this document")))

          (form/AnkiSyncForm user-id root-id conn form)
          (form/AnkiSyncStatus sync !show-modal conn))))))

(e/defn AnkiSyncModalDom
  "Modal overlay + inner dialog; delegates to error/connected/connecting panels.
   conn = {:!status :!error ...}  sync = {:!phase ...}"
  [user-id selected-doc !show-modal conn form sync]
  (e/client
    (let [conn-status (e/watch (:!status conn))
          sync-phase (e/watch (:!phase sync))]
      (dom/div
        (dom/props {:class "modal-backdrop" :style {:background "rgba(0,0,0,0.5)"}
                    :role "dialog" :aria-modal "true" :aria-label "Anki sync"
                    :tabindex "-1"})
        (modal-shell/FocusReturn)
        ;; rAF focus-on-mount: HTML :autofocus is inert on dynamically
        ;; inserted elements (see modal-shell/focus-on-mount!). e/snapshot
        ;; fires once at mount; the rAF defer lets FocusReturn's
        ;; synchronous opener-capture win before focus moves in.
        (e/snapshot (modal-shell/focus-on-mount! dom/node))
        (dom/On "click" (fn [_] (when-not sync-phase (reset! !show-modal false))) nil)
        (dom/On "keydown"
          (fn [e]
            (cond
              (and (helpers/escape-key? e) (not sync-phase))
              (reset! !show-modal false)

              (and (= (.-key e) "Enter") (or (.-metaKey e) (.-ctrlKey e)))
              (when-let [btn @(:!push-btn sync)]
                (.preventDefault e)
                (.click btn))

              (= (.-key e) "Tab")
              (modal-shell/trap-tab! (.-currentTarget e) e)))
          nil)
        (dom/div
          (dom/props {:class "modal-content modal-lg" :style {:width "620px" :max-height "80vh" :overflow-y "auto"}})
          (dom/On "click" (fn [e] (.stopPropagation e)) nil)
          (dom/h3 (dom/props {:style {:margin-top "0" :margin-bottom "20px"}})
            (dom/text "Anki Sync"))
          (cond
            (= conn-status :connecting)
            (dom/div
              (dom/props {:style {:text-align "center" :padding "var(--sp-5)" :color "var(--color-text-secondary)"}})
              (dom/span (dom/props {:class "spinner"}))
              (dom/text "Connecting to Anki..."))
            (= conn-status :error)
            (AnkiSyncErrorPanel conn !show-modal)
            (= conn-status :connected)
            (AnkiSyncConnectedPanel user-id selected-doc conn form sync !show-modal)))))))

(e/defn AnkiSyncSyncBody
  "Sync state, executor, and modal DOM. Note types are app-owned, so there is
   no model/field resolution here.
   conn = {:!status :!selected-deck ...}
   form = {:!scope :!allow-dupes :!use-tags :!tags ...}"
  [user-id selected-doc !show-modal conn form]
  (e/client
    (let [!sync-phase (atom nil)
          !sync-result (atom nil)
          !sync-error (atom nil)
          !push-pairs (atom nil)
          !push-btn (atom nil)
          sync {:!phase !sync-phase
                :!result !sync-result
                :!error !sync-error
                :!push-pairs !push-pairs
                :!push-btn !push-btn}]
      ;; §4 optimistic: the Push button sets :pushing; instead of running the
      ;; executor in-modal, persist the modal's prefs as last-used, close, and
      ;; fire the headless QuickSync (re-connects, pushes, toasts the outcome).
      ;; The in-modal AnkiSyncExecutor is intentionally not mounted here.
      ;; See plans/optimistic-updates.md §4.
      (when (= (e/watch !sync-phase) :pushing)
        (let [[t _] (e/Token :anki-handoff)]
          (when t
            ;; Build prefs on the CLIENT (deref the client atoms here); only the
            ;; resulting plain map crosses to the server — never the atoms, which
            ;; are unserializable. (Mirrors AnkiSyncExecutor's prefs-map handling.)
            (let [prefs {:scope @(:!scope form)
                         :deck @(:!selected-deck conn)
                         :allow-dupes @(:!allow-dupes form)
                         :use-tags @(:!use-tags form)
                         :tags @(:!tags form)}]
              (case (e/server (e/Offload #(save-last-used-prefs!* user-id prefs)))
                (do (reset! !show-modal false)
                    (reset! !sync-phase nil)
                    (trigger-quick-sync!)
                    (t)))))))
      (AnkiSyncModalDom user-id selected-doc !show-modal conn form sync))))
