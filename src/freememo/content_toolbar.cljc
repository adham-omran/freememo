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
   #?(:clj [freememo.settings :as user-settings])
   #?(:clj [freememo.user-state :as us])
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

(e/defn ContentToolbar [state refresh]
  (e/client
    (let [mod-key (if (mac-platform?) "Cmd" "Ctrl")
          {:keys [user-id enc-key topic-id root-topic-id page-number content-text
                  parent-content context-mode context-tooltip llm-enabled?]} state
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
          radio-name (if (= context-mode :extract) "extract-card-type" "card-type")]

      (dom/div
        (dom/props {:class "toolbar"})

        ;; Settings controls (context, card type, card count)
        ;; Atoms passed as positional args — Electric can't serialize atoms inside maps
        (settings/ToolbarSettings
          {:user-id user-id :context-tooltip context-tooltip :radio-name radio-name
           :llm-enabled? llm-enabled?
           :use-context use-context :context-window context-window
           :card-type card-type :card-count-val card-count-val}
          !use-context !context-window !card-type !card-count)

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
           :source-ref source-ref :unsynced-count unsynced-count :card-type card-type})))))
