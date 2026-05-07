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
                  context-mode mod-key source-ref unsynced-count
                  card-type]} cfg]
      (ExtractTopicButton user-id topic-id context-mode mod-key)
      (AddCardButton user-id topic-id root-topic-id source-ref card-type)
      (dom/span (dom/props {:class "toolbar-overflow-item" :style {:color "var(--color-border)"}}) (dom/text "|"))
      (ExportButton user-id topic-id root-topic-id unsynced-count)
      (dom/span (dom/props {:class "toolbar-overflow-item" :style {:color "var(--color-border)"}}) (dom/text "|"))
      (PullFromAnkiButton user-id root-topic-id)
      (AnkiSyncButton user-id root-topic-id page-number card-type unsynced-count))))
