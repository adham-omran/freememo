(ns freememo.export-button
  "Export toolbar button + modal trigger. Extracted from content_toolbar_actions
   so each e/defn stays under the JVM 64KB limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.card-modals :refer [ExportModal]]
   [freememo.keyboard :as keyboard]))

(e/defn ExportButton [user-id topic-id root-topic-id unsynced-count]
  (e/client
    (let [!show-export (atom false)
          show-export (e/watch !show-export)]
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary"
                    :style {:font-weight "500"}
                    :aria-label "Export"
                    :data-tooltip "Export"})
        (icons/Icon :download :size 16)
        (dom/span (dom/props {:class "icon-label"})
          (dom/text (if (pos? unsynced-count)
                      (str "Export (" unsynced-count ")...")
                      "Export...")))
        (reset! keyboard/!export-btn-ref dom/node)
        (e/on-unmount (fn [] (reset! keyboard/!export-btn-ref nil)))
        (dom/On "click" (fn [_] (reset! !show-export true)) nil))
      (when show-export
        (ExportModal !show-export topic-id root-topic-id user-id)))))
