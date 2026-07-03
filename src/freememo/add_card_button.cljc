(ns freememo.add-card-button
  "Add-new-card toolbar button + modal trigger. Extracted from
   content_toolbar_actions so each e/defn stays under the JVM 64KB limit."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.rich-text-editor :as editor]
   [freememo.card-modals :refer [AddCardModal]]
   [freememo.command-bus :as bus]))

(e/defn AddCardButton [user-id topic-id root-topic-id card-type]
  (e/client
    (let [!show-add (atom false)
          show-add (e/watch !show-add)
          !card-kind (atom card-type)
          !captured-selection (atom "")]
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-item"
                    :style {:font-weight "500"}
                    :aria-label "Add new card"
                    :data-tooltip "Add new card"})
        (icons/Icon :plus :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text "Add new card"))
        (let [node dom/node]
          (bus/publish-invoker! :add-new (fn [] (.click node)))
          (e/on-unmount (fn [] (bus/retract-invoker! :add-new))))
        (dom/On "click"
          (fn [_]
            (let [{:keys [html text]} (editor/get-selection-html!)]
              (reset! !captured-selection (if (str/blank? text) "" html)))
            (reset! !show-add true))
          nil))
      (when show-add
        (AddCardModal !show-add !card-kind !captured-selection topic-id root-topic-id user-id)))))
