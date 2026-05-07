(ns freememo.content-toolbar-actions
  "Extract, Add, Export, Pull from Anki, and Anki Sync buttons for ContentToolbar.
   Extracted to stay under JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.rich-text-editor :as editor]
   [freememo.anki-sync :refer [AnkiSyncButton]]
   [freememo.anki-pull-button :refer [PullFromAnkiButton]]
   [freememo.extract-topic-button :refer [ExtractTopicButton]]
   [freememo.card-modals :refer [ExportModal AddCardModal]]
   [freememo.keyboard :as keyboard]))

(e/defn ToolbarActions [cfg]
  (e/client
    (let [{:keys [user-id topic-id root-topic-id page-number
                  context-mode mod-key source-ref unsynced-count
                  card-type]} cfg]

      (ExtractTopicButton user-id topic-id context-mode mod-key)

      ;; Add new card button
      (let [!show-add (atom false)
            show-add (e/watch !show-add)
            !card-kind (atom card-type)
            !captured-selection (atom "")]
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-item" :style {:font-weight "500"}})
          (dom/text "Add new")
          (reset! keyboard/!add-new-btn-ref dom/node)
          (e/on-unmount (fn [] (reset! keyboard/!add-new-btn-ref nil)))
          (dom/On "click"
            (fn [_]
              (reset! !captured-selection (or (editor/get-selected-text!) ""))
              (reset! !show-add true))
            nil))
        (when show-add
          (AddCardModal !show-add !card-kind !captured-selection topic-id root-topic-id source-ref user-id)))

      ;; Separator
      (dom/span (dom/props {:class "toolbar-overflow-item" :style {:color "var(--color-border)"}}) (dom/text "|"))

      ;; Export button + modal
      (let [!show-export (atom false)
            show-export (e/watch !show-export)]
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-item" :style {:font-weight "500"}})
          (dom/text (if (pos? unsynced-count)
                      (str "Export (" unsynced-count ")...")
                      "Export..."))
          (reset! keyboard/!export-btn-ref dom/node)
          (e/on-unmount (fn [] (reset! keyboard/!export-btn-ref nil)))
          (dom/On "click" (fn [_] (reset! !show-export true)) nil))
        (when show-export
          (ExportModal !show-export topic-id root-topic-id user-id)))

      ;; Separator
      (dom/span (dom/props {:class "toolbar-overflow-item" :style {:color "var(--color-border)"}}) (dom/text "|"))

      (PullFromAnkiButton user-id root-topic-id)

      ;; Anki Sync button
      (AnkiSyncButton user-id root-topic-id page-number card-type unsynced-count))))
