(ns freememo.add-card-button
  "Add-new-card toolbar button + modal trigger. Extracted from
   content_toolbar_actions so each e/defn stays under the JVM 64KB limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.rich-text-editor :as editor]
   [freememo.card-modals :refer [AddCardModal]]
   [freememo.keyboard :as keyboard]))

(e/defn AddCardButton [user-id topic-id root-topic-id source-ref card-type]
  (e/client
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
        (AddCardModal !show-add !card-kind !captured-selection topic-id root-topic-id source-ref user-id)))))
