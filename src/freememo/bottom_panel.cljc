(ns freememo.bottom-panel
  "Two split regions of a topic view:
     ToolbarBar  — the full-width command toolbar, rendered ABOVE the content.
     BottomPanel — the ContentCardTable, rendered BELOW the content.

   ToolbarBar owns the live-content derivation from editor/!dirty-html (only the
   toolbar consumes it for card-gen context), so the caller doesn't thread dirty
   state through props."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.rich-text-editor :as editor]
   [freememo.content-toolbar :refer [ContentToolbar]]
   [freememo.content-card-table :refer [ContentCardTable]]))

(e/defn ToolbarBar
  "Full-width command toolbar, rendered as a top bar above the content.
   Derives live-content from the editor's dirty html (only the toolbar consumes
   it for card-gen context).

   props keys:
     :user-id :enc-key :topic-id :root-topic-id :page-number
     :static-content       string fallback shown when editor has no dirty html for this topic
     :context-mode         :page or :extract
     :context-tooltip      string
     :llm-enabled?         bool
     :extract-status       string (extract mode only)
     :navigate!            optional fn (extract mode only)
     :origin               keyword tab (extract mode only)
     :on-done!             optional 0-arg fn (queue contexts only)
     :citation             bibliography citation string-or-nil (DocumentMetaGroup)
     :page-info            page-progress {:done :total :remaining-tooltip}-or-nil
     :pdf-root?            true at a PDF root (gates Mark-PDF-Done)
     :pdf-status           PDF root status string-or-nil
   refresh: combined card-refresh value (refresh + sync-mutations + card-mutations).
   !show-bib: modal-open atom (positional — atoms cannot ride in a props map)."
  [{:keys [user-id enc-key topic-id audio? root-topic-id page-number
           static-content
           context-mode context-tooltip llm-enabled?
           extract-status navigate! origin on-done!
           citation page-info pdf-root? pdf-status reading-mode?]} refresh !show-bib]
  (e/client
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
                                 :llm-enabled? llm-enabled?
                                 :citation citation
                                 :page-info page-info
                                 :pdf-root? pdf-root?
                                 :pdf-status pdf-status
                                 :reading-mode? reading-mode?}
                          extract-status (assoc :extract-status extract-status)
                          navigate! (assoc :navigate! navigate!)
                          origin (assoc :origin origin)
                          on-done! (assoc :on-done! on-done!))]
      (ContentToolbar toolbar-props refresh !show-bib))))

(e/defn BottomPanel
  "Bottom card-table region for both PDF-page and extract topic views.

   props keys: :user-id :topic-id :card-font-size
   refresh: combined card-refresh value."
  [{:keys [user-id topic-id card-font-size]} refresh]
  (e/client
    (dom/div
      ;; min-width: 0 lets this flex item shrink below its intrinsic content
      ;; width (keeps the card table from forcing the column wider).
      (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                          :min-width "0" :min-height "0"}})
      (ContentCardTable {:topic-id topic-id
                         :card-font-size card-font-size
                         :user-id user-id}
        refresh))))
