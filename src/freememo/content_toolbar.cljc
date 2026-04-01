(ns freememo.content-toolbar
  "Unified card generation toolbar for both page-level and extract-level content.
   Orchestrator — delegates to ToolbarSettings and ToolbarActions sub-components."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.content-toolbar-helpers :as helpers]
   [freememo.content-toolbar-settings :as settings]
   [freememo.content-toolbar-generate :as generate]
   [freememo.content-toolbar-actions :as actions]
   #?(:clj [freememo.settings :as user-settings])
   #?(:clj [freememo.db :as db])
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

(e/defn ContentToolbar [state !refresh]
  (e/client
    (let [mod-key (if (mac-platform?) "Cmd" "Ctrl")
          {:keys [user-id enc-key topic-id root-topic-id page-number content-text
                  parent-content context-mode context-tooltip llm-enabled?]} state
          ;; Fetch source reference from root topic for card propagation
          source-ref (e/server (db/get-topic-source root-topic-id))
          ;; Unsynced card count — uses refresh wrapper for reactivity
          toolbar-refresh (e/server (e/watch !refresh))
          unsynced-count (e/server (helpers/get-unsynced-count* toolbar-refresh topic-id))
          ;; Load settings from server
          server-context-enabled (e/server (user-settings/get-context-enabled user-id))
          server-context-pages (e/server (user-settings/get-context-pages user-id))
          server-card-type (e/server (user-settings/get-card-type user-id))
          server-card-count (e/server (user-settings/get-card-count user-id))
          server-prompt-history (e/server (user-settings/get-pre-prompt-history user-id))

          ;; Initialize atoms with server values
          !use-context (atom server-context-enabled)
          use-context (e/watch !use-context)
          !context-window (atom server-context-pages)
          context-window (e/watch !context-window)
          !card-type (atom server-card-type)
          card-type (e/watch !card-type)
          !card-count (atom server-card-count)
          card-count-val (e/watch !card-count)
          !prompt-history (atom server-prompt-history)
          !history-save-trigger (atom nil)
          history-save-trigger (e/watch !history-save-trigger)

          ;; Pre-prompt state
          !pre-prompt (atom "")
          !show-prompt-dialog (atom false)
          show-prompt-dialog (e/watch !show-prompt-dialog)
          !prompt-dialog-kind (atom nil)
          prompt-dialog-kind (e/watch !prompt-dialog-kind)
          !captured-selection (atom nil)
          captured-selection (e/watch !captured-selection)
          !prompt-submit (atom nil)
          prompt-submit (e/watch !prompt-submit)
          !gen-click (atom nil)
          gen-click (e/watch !gen-click)

          ;; Generation status (global processor, keyed by topic-id)
          card-gen-status (e/server (get (e/watch helpers/!card-gen-status) topic-id))
          gen-pending (or (:pending card-gen-status) 0)
          gen-active? (or (some? (:active-id card-gen-status)) (pos? gen-pending))
          gen-error (:error card-gen-status)

          ;; Unique radio group name to avoid collision when both page and extract toolbars exist
          radio-name (if (= context-mode :extract) "extract-card-type" "card-type")]

      ;; Persist prompt history to server when Generate is clicked
      (when (some? history-save-trigger)
        (e/server (user-settings/save-pre-prompt-history user-id history-save-trigger)))

      (dom/div
        (dom/props {:class "toolbar"})

        ;; Settings controls (context, card type, card count)
        (settings/ToolbarSettings
          {:user-id user-id :context-tooltip context-tooltip :radio-name radio-name
           :llm-enabled? llm-enabled?
           :!use-context !use-context :use-context use-context
           :!context-window !context-window :context-window context-window
           :!card-type !card-type :card-type card-type
           :!card-count !card-count :card-count-val card-count-val})

        ;; Generate buttons + processors + prompt dialog
        (generate/ToolbarGenerate
          (assoc state
            :mod-key mod-key :source-ref source-ref
            :card-type card-type :card-count-val card-count-val
            :use-context use-context :context-window context-window
            :gen-active? gen-active? :gen-pending gen-pending :gen-error gen-error
            :gen-click gen-click :prompt-submit prompt-submit
            :captured-selection captured-selection
            :show-prompt-dialog show-prompt-dialog
            :prompt-dialog-kind prompt-dialog-kind
            :!gen-click !gen-click
            :!captured-selection !captured-selection
            :!show-prompt-dialog !show-prompt-dialog
            :!prompt-dialog-kind !prompt-dialog-kind
            :!prompt-submit !prompt-submit
            :!pre-prompt !pre-prompt
            :!prompt-history !prompt-history
            :!history-save-trigger !history-save-trigger)
          !refresh)

        ;; Extract, Add, Export, Anki Sync
        (actions/ToolbarActions
          {:user-id user-id :topic-id topic-id :root-topic-id root-topic-id
           :page-number page-number :context-mode context-mode :mod-key mod-key
           :source-ref source-ref :unsynced-count unsynced-count :card-type card-type}
          !refresh)))))
