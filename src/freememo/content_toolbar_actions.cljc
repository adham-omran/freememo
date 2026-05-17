(ns freememo.content-toolbar-actions
  "Extract, Add, Export, Pull from Anki, and Anki Sync buttons for ContentToolbar.
   Each button is its own e/defn (separate namespace) to stay under the JVM
   64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.anki-sync :refer [AnkiSyncButton]]
   [freememo.anki-pull-button :refer [PullFromAnkiButton]]
   [freememo.extract-topic-button :refer [ExtractTopicButton]]
   [freememo.add-card-button :refer [AddCardButton]]
   [freememo.export-button :refer [ExportButton]]))

(e/defn ToolbarActions [cfg]
  (e/client
    (let [{:keys [user-id topic-id root-topic-id page-number
                  context-mode mod-key unsynced-count
                  card-type]} cfg]
      ;; IR Tools group: Extract + Add new
      (ExtractTopicButton user-id topic-id context-mode mod-key)
      (AddCardButton user-id topic-id root-topic-id card-type)
      ;; Group boundary → Sync group. Divider's tier-collapse class mirrors
      ;; Sync group's last-to-collapse item (Pull/Sync, .toolbar-collapse-early)
      ;; so the divider hides exactly when its right-hand group is empty —
      ;; avoids the "| |" orphan when Export/Pull/Sync all moved to overflow.
      (dom/span (dom/props {:class "toolbar-collapse-early toolbar-group-divider"}))
      ;; Sync group: Export + Pull + Sync
      (ExportButton user-id topic-id root-topic-id unsynced-count)
      (PullFromAnkiButton user-id root-topic-id)
      (AnkiSyncButton user-id root-topic-id page-number card-type unsynced-count))))
