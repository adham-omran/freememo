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
   [freememo.doc-context :as dctx]
   [freememo.rich-text-editor :as editor]
   [freememo.content-toolbar :refer [ContentToolbar]]
   [freememo.content-card-table :refer [ContentCardTable]]))

(e/defn ToolbarBar
  "Full-width command toolbar, rendered as a top bar above the content.
   Derives live-content from the editor's dirty html (only the toolbar consumes
   it for card-gen context) and binds it as dctx/content-text for ContentToolbar.
   Every other input arrives via ambient doc-context (dctx), bound upstream by
   DocumentToolbars / TopicPage."
  []
  (e/client
    (let [topic-id       dctx/topic-id
          static-content dctx/static-content
          dirty          (e/watch editor/!dirty-html)
          live-content   (if (and dirty (= (:topic-id dirty) topic-id))
                           (:html dirty)
                           static-content)]
      (binding [dctx/content-text live-content]
        (ContentToolbar)))))

(e/defn BottomPanel
  "Bottom card-table region for both PDF-page and extract topic views.

   props keys: :user-id :topic-id :card-font-size
   refresh: combined card-refresh value."
  []
  (e/client
    (let [user-id dctx/user-id topic-id dctx/page-topic-id
          card-font-size dctx/card-font-size refresh dctx/card-refresh]
      (dom/div
        ;; min-width: 0 lets this flex item shrink below its intrinsic content
        ;; width (keeps the card table from forcing the column wider).
        (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                            :min-width "0" :min-height "0"}})
        (ContentCardTable)))))
