(ns freememo.content-toolbar
  "Unified card generation toolbar for both page-level and extract-level content.
   Orchestrator — delegates to ToolbarSettings, ToolbarGenerate, and ToolbarActions."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.content-toolbar-helpers :as helpers]
   [freememo.content-toolbar-settings :as settings]
   [freememo.content-toolbar-generate :as generate]
   [freememo.content-toolbar-actions :as actions]
   [freememo.content-toolbar-extract :refer [ExtractActions]]
   #?(:clj [freememo.settings :as user-settings])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])
   [freememo.keyboard :as keyboard]
   [freememo.util :refer [mac-platform?]]))

;; State map keys:
;; {:user-id       — user identifier
;;  :enc-key       — encryption key
;;  :topic-id      — the entity (page or extract topic) where cards are created
;;  :root-topic-id — the root topic for scoping (PDF root, standalone root, etc.)
;;  :page-number   — current page number (for page-level context queries, nil for non-PDF)
;;  :content-text  — full text (page text for :page mode, extract text for :extract mode)
;;  :parent-content — parent topic's content (for extract context, nil for page mode)
;;  :context-mode  — :page or :extract
;;  :context-tooltip — string for the Context checkbox title attribute
;;  :llm-enabled?  — whether LLM features are enabled
;; }

(e/defn ContentToolbar [state refresh]
  (e/client
    (let [mod-key (if (mac-platform?) "Cmd" "Ctrl")
          {:keys [user-id enc-key topic-id root-topic-id page-number content-text
                  parent-content context-mode context-tooltip llm-enabled?
                  extract-status navigate! origin]} state
          ;; Fetch source reference from root topic for card propagation
          source-ref (e/server (db/get-topic-source root-topic-id))
          ;; Unsynced card count — uses refresh value for reactivity
          unsynced-count (e/server (helpers/get-unsynced-count* refresh topic-id))
          ;; Load settings from server
          server-context-enabled (e/server (user-settings/get-context-enabled user-id))
          server-context-pages (e/server (user-settings/get-context-pages user-id))
          server-card-type (e/server (user-settings/get-card-type user-id))
          server-card-count (e/server (user-settings/get-card-count user-id))

          ;; Shared settings atoms (used by both ToolbarSettings and ToolbarGenerate/Actions)
          !use-context (atom server-context-enabled)
          use-context (e/watch !use-context)
          !context-window (atom server-context-pages)
          context-window (e/watch !context-window)
          !card-type (atom server-card-type)
          card-type (e/watch !card-type)
          !card-count (atom server-card-count)
          card-count-val (e/watch !card-count)

          ;; Generation status (per-user, keyed by topic-id)
          card-gen-status (e/server (get (e/watch (us/get-atom user-id :card-gen-status)) topic-id))
          gen-pending (or (:pending card-gen-status) 0)
          gen-active? (or (some? (:active-id card-gen-status)) (pos? gen-pending))
          gen-error (:error card-gen-status)

          ;; Unique radio group name
          radio-name (if (= context-mode :extract) "extract-card-type" "card-type")

          ;; Overflow menu state (mobile)
          !overflow-open (atom false)
          overflow-open (e/watch !overflow-open)]

      (dom/div
        (dom/props {:class (if overflow-open "toolbar-container overflow-open" "toolbar-container")})

        (dom/div
          (dom/props {:class "toolbar"})

          ;; Overflow panel — display:contents on desktop (items flow inline),
          ;; dropdown on mobile (toggled via .overflow-open on container).
          ;; Always mounted so settings e/Token handlers stay active.
          (dom/div
            (dom/props {:class "toolbar-overflow-panel"})

            ;; Settings controls (context, card type, card count)
            ;; Atoms passed as positional args — Electric can't serialize atoms inside maps
            (settings/ToolbarSettings
              {:user-id user-id :context-tooltip context-tooltip :radio-name radio-name
               :llm-enabled? llm-enabled?
               :use-context use-context :context-window context-window
               :card-type card-type :card-count-val card-count-val}
              !use-context !context-window !card-type !card-count)

            ;; Separator between settings and action buttons (mobile only)
            (dom/div (dom/props {:class "toolbar-overflow-panel-separator"}))

            ;; Proxy action buttons — hidden on desktop, visible in dropdown on mobile.
            ;; Each .click()s the hidden real button via ref (same pattern as keyboard shortcuts).
            (when llm-enabled?
              (dom/button
                (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-panel-action"})
                (dom/text "Generate with Prompt...")
                (dom/On "click" (fn [_]
                                  (when-let [btn (deref keyboard/!gen-prompt-btn-ref)]
                                    (.click btn))
                                  (reset! !overflow-open false)) nil)))
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-panel-action"})
              (dom/text "Add new")
              (dom/On "click" (fn [_]
                                (when-let [btn (deref keyboard/!add-new-btn-ref)]
                                  (.click btn))
                                (reset! !overflow-open false)) nil))
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-panel-action"})
              (dom/text (if (and unsynced-count (pos? unsynced-count))
                          (str "Export (" unsynced-count ")...")
                          "Export..."))
              (dom/On "click" (fn [_]
                                (when-let [btn (deref keyboard/!export-btn-ref)]
                                  (.click btn))
                                (reset! !overflow-open false)) nil))
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-panel-action"})
              (dom/text "Pull from Anki")
              (dom/On "click" (fn [_]
                                (when-let [btn (deref keyboard/!pull-anki-btn-ref)]
                                  (.click btn))
                                (reset! !overflow-open false)) nil))
            (when extract-status
              (dom/button
                (dom/props {:class "btn btn-sm btn-danger toolbar-overflow-panel-action"})
                (dom/text "Delete...")
                (dom/On "click" (fn [_]
                                  (when-let [btn (deref keyboard/!delete-btn-ref)]
                                    (.click btn))
                                  (reset! !overflow-open false)) nil))))

          ;; Generate buttons + processors + prompt dialog
          ;; All generate/prompt atoms are LOCAL to ToolbarGenerate (not in this map)
          ;; to avoid reactive loops from map reconstruction.
          (generate/ToolbarGenerate
            (assoc state
              :mod-key mod-key :source-ref source-ref
              :card-type card-type :card-count-val card-count-val
              :use-context use-context :context-window context-window
              :gen-active? gen-active? :gen-pending gen-pending :gen-error gen-error))

          ;; Extract, Add, Export, Anki Sync
          (actions/ToolbarActions
            {:user-id user-id :topic-id topic-id :root-topic-id root-topic-id
             :page-number page-number :context-mode context-mode :mod-key mod-key
             :source-ref source-ref :unsynced-count unsynced-count :card-type card-type})

          ;; Done/Restore + Delete — extract topics only (separate e/defn for bytecode limit)
          (ExtractActions {:user-id user-id :topic-id topic-id :extract-status extract-status
                           :navigate! navigate! :origin origin})

          ;; Overflow trigger — visible only on mobile/tablet via CSS
          (dom/div
            (dom/props {:class "toolbar-overflow-trigger"})
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary"
                          :style {:font-weight "bold" :padding "4px 10px" :font-size "16px" :line-height "1"}})
              (dom/text "\u22EE")
              (dom/On "click" (fn [_] (swap! !overflow-open not)) nil))))

        ;; Backdrop — outside .toolbar to avoid scroll clipping
        (when overflow-open
          (dom/div
            (dom/props {:class "toolbar-overflow-backdrop"})
            (dom/On "click" (fn [_] (reset! !overflow-open false)) nil)))))))
