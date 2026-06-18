(ns freememo.bottom-panel
  "ContentToolbar + ContentCardTable composed under the editor / PDF area.

   Owns its own derivation of live-content from editor/!dirty-html so the
   caller doesn't need to thread dirty state through props."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.rich-text-editor :as editor]
   [freememo.content-toolbar :refer [ContentToolbar]]
   [freememo.content-card-table :refer [ContentCardTable]]))

(e/defn BottomPanel
  "Shared bottom region for both PDF-page and extract topic views.

   props keys:
     :user-id :enc-key :topic-id :root-topic-id :page-number
     :static-content       string fallback shown when editor has no dirty html for this topic
     :context-mode         :page or :extract
     :context-tooltip      string
     :llm-enabled?         bool
     :extract-status       string (extract mode only)
     :navigate!            optional fn (extract mode only)
     :origin               keyword tab (extract mode only)
     :on-done!             optional 0-arg fn (queue contexts only); invoked
                           after Done's server mutation completes — advances
                           !queue-idx in /learn and subset-review
     :card-font-size       int
   refresh: combined card-refresh value (refresh + sync-mutations + card-mutations)."
  [{:keys [user-id enc-key topic-id audio? root-topic-id page-number
           static-content
           context-mode context-tooltip llm-enabled?
           extract-status navigate! origin on-done! card-font-size]} refresh]
  (e/client
    (dom/div
      ;; min-width: 0 lets this flex item shrink below its intrinsic content
      ;; width — required so the toolbar's container reports a clientWidth that
      ;; reflects the visible pane width (not the natural content width).
      (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                          :min-width "0" :min-height "0"}})

      (let [dirty (e/watch editor/!dirty-html)
            live-content (if (and dirty (= (:topic-id dirty) topic-id))
                           (:html dirty)
                           static-content)
            toolbar-props (cond-> {:user-id user-id
                                   :enc-key enc-key
                                   :topic-id topic-id
                                   :audio? audio?
                                   :root-topic-id root-topic-id
                                   :page-number page-number
                                   :content-text live-content
                                   :context-mode context-mode
                                   :context-tooltip context-tooltip
                                   :llm-enabled? llm-enabled?}
                            extract-status (assoc :extract-status extract-status)
                            navigate! (assoc :navigate! navigate!)
                            origin (assoc :origin origin)
                            on-done! (assoc :on-done! on-done!))]
        (ContentToolbar toolbar-props refresh))

      (ContentCardTable {:topic-id topic-id
                         :card-font-size card-font-size
                         :user-id user-id}
        refresh))))
