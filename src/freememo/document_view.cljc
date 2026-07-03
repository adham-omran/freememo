(ns freememo.document-view
  "View sub-components of TopicPage, split into their own namespace because
   Electric inlines *same-namespace* e/defn calls into the caller's compiled
   method — so a large page must cross a namespace boundary (a real frame
   boundary) to stay under the JVM's 64KB-per-method limit. TopicPage owns data
   resolution and passes everything in as a prop map; these render it."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.hierarchy-side-panel :refer [HierarchySidePanel]]
   [freememo.pin-side-panel :refer [PinSidePanel]]
   [freememo.pdf-pane :refer [PdfPane]]
   [freememo.pdf-toolbar :refer [PdfToolbar]]
   [freememo.score-toolbar :refer [ScoreToolbar ScoreWaveformStrip]]
   [freememo.editor-pane :refer [EditorPane]]
   [freememo.bottom-panel :refer [ToolbarBar]]
   [freememo.bibliography-form :as bibform :refer [BibliographyForm]]
   [freememo.navigation :as nav]
   [freememo.util :as util]))

(e/defn PdfPaneFrame
  "The PDF pane itself (PdfPane + its sized wrapper). Own e/defn for the 64KB
   method cap."
  []
  (e/client
    (let [user-id dctx/user-id pdf-root-id dctx/pdf-root-id initial-page dctx/initial-page
          target-page dctx/target-page is-live? dctx/is-live? pdf-has-file? dctx/pdf-has-file?
          pdf-page-count dctx/pdf-page-count top-bottom? dctx/top-bottom?
          top-split-pct dctx/top-split-pct left-pct dctx/left-pct
          !current-page dctx/!current-page !total dctx/!total
          pdf-style (if top-bottom?
                      {:height (str top-split-pct "%")
                       :min-height "0" :overflow "hidden"}
                      {:width (str left-pct "%")
                       :min-width "0" :overflow "hidden"})]
      (dom/div
        (dom/props {:style pdf-style})
        (reset! !current-page
          (binding [dctx/has-file? pdf-has-file?
                    dctx/reload-nonce pdf-page-count
                    dctx/on-page-change! (fn [p] (reset! !current-page p))
                    dctx/on-total! (fn [n] (reset! !total n))]
            (PdfPane)))))))

(e/defn PaneDragHandle
  "Drag handle between the PDF pane and the editor. Own e/defn for the 64KB
   method cap."
  []
  (e/client
    (let [top-bottom? dctx/top-bottom? !top-split-pct dctx/!top-split-pct
          !left-pct dctx/!left-pct
          top-split-pct dctx/top-split-pct left-pct dctx/left-pct]
      (dom/div
        (dom/props {:class (if top-bottom? "split-divider-v" "split-divider-h")
                    :title "Drag to resize panels"
                    :role "separator"
                    :aria-orientation (if top-bottom? "horizontal" "vertical")
                    :aria-label "Resize PDF and editor panes" :tabindex "0"
                    :aria-valuenow (str (int (or (if top-bottom? top-split-pct left-pct) 0)))})
        (dom/On "pointerdown"
          (fn [e]
            (if top-bottom?
              (util/start-drag! e :y !top-split-pct)
              (util/start-drag! e :x !left-pct)))
          nil)
        (dom/On "keydown"
          (fn [e]
            (if top-bottom?
              (util/key-resize-pct! e :y !top-split-pct nil)
              (util/key-resize-pct! e :x !left-pct nil)))
          nil)))))

(e/defn PdfSplitPane
  "The PDF half of a document split: PdfPaneFrame + PaneDragHandle. Renders
   nothing for non-PDF topics. Thin shell — work lives in its two children so no
   single e/defn exceeds Electric's 64KB-per-method cap."
  []
  (e/client
    (when dctx/is-pdf?
      (PdfPaneFrame)
      (PaneDragHandle))))

(e/defn ContentColumn
  "Middle content column of a document view: the PdfPane/EditorPane split, laid
   out as a row (left-right) or column (top-bottom). Own e/defn for the 64KB
   method cap. Reads ambient doc-context; PdfSplitPane reads it too."
  []
  (e/client
    (let [user-id dctx/user-id page-topic-id dctx/page-topic-id kind dctx/kind
          is-pdf? dctx/is-pdf? top-bottom? dctx/top-bottom?
          effective-content dctx/effective-content navigate! dctx/navigate!]
      (dom/div
        (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                            :min-width "0" :min-height "0" :overflow "hidden"}})
        ;; Score topics: waveform strip across the top of the content column;
        ;; the audio segment picked here pairs with notation rects for cards.
        (when dctx/is-score?
          (ScoreWaveformStrip))
        (dom/div
          (dom/props {:style {:flex "1" :display "flex"
                              :flex-direction (if (and is-pdf? top-bottom?)
                                                "column"
                                                "row")
                              :min-height "0" :overflow "hidden"}})
          (PdfSplitPane)
          (binding [dctx/topic-id page-topic-id
                    dctx/audio-topic-id (when (= kind "audio") page-topic-id)
                    dctx/is-pdf-page? is-pdf?
                    dctx/static-content effective-content
                    dctx/on-imported-navigate!
                    (fn [tid]
                      (when navigate!
                        (navigate! :viewer (nav/nav-topic tid nil))))]
            (EditorPane)))))))

(e/defn DocumentColumns
  "Resizable top region of a document view: hierarchy sidebar | content column
   (ContentColumn) | pin sidebar. Split across ContentColumn/PdfSplitPane so no
   single e/defn exceeds Electric's 64KB-per-method cap. Behaviour unchanged;
   the columns are reactively independent siblings. Reads ambient doc-context."
  []
  (e/client
    (let [user-id dctx/user-id page-topic-id dctx/page-topic-id
          root-topic-id dctx/root-topic-id navigate! dctx/navigate!
          is-pdf? dctx/is-pdf? top-pct dctx/top-pct reading-mode? dctx/reading-mode?
          target-page dctx/target-page !nav-target dctx/!nav-target]
      (dom/div
        ;; reading-mode? hides the card table below, so the top region takes the
        ;; full height instead of the split percentage.
        (dom/props {:style {:height (if reading-mode? "100%" (str top-pct "%"))
                            :display "flex" :flex-direction "row"
                            :min-height "0" :overflow "hidden"}})

        ;; Clear nav-target after deriving target-page, so the viewer can navigate
        ;; away manually without being snapped back.
        (when target-page (reset! !nav-target nil))

        ;; LEFT: hierarchy side panel. reading-mode? (mobile learn) hides it —
        ;; navigation is the linear queue (Next), not the tree.
        (when-not reading-mode?
          (HierarchySidePanel user-id page-topic-id root-topic-id navigate!
            (when is-pdf? !nav-target)))

        ;; MIDDLE: content column.
        (ContentColumn)

        ;; RIGHT: pin side panel (collapsible). Hidden in reading-mode?.
        (when-not reading-mode?
          (PinSidePanel page-topic-id root-topic-id user-id))))))


;; TopicPage shell
;; ---------------------------------------------------------------------------

(e/defn DocumentToolbars
  "Above-the-content chrome: ToolbarBar (command ribbon), the PDF-only
   PdfToolbar, the one-shot biblio auto-open, and the bibliography modal.
   Renders into the caller's dom parent. Own e/defn for Electric's 64KB
   method cap. card-refresh/!show-bib are passed positionally to ToolbarBar."
  []
  (e/client
    (let [user-id dctx/user-id enc-key dctx/enc-key page-topic-id dctx/page-topic-id
          kind dctx/kind pdf-root-id dctx/pdf-root-id root-topic-id dctx/root-topic-id
          is-pdf? dctx/is-pdf? current-page dctx/current-page effective-content dctx/effective-content
          llm-enabled? dctx/llm-enabled? extract-status dctx/extract-status
          navigate! dctx/navigate! queue-ctx dctx/queue-ctx citation dctx/citation
          page-info dctx/page-info pdf-root? dctx/pdf-root? pdf-status dctx/pdf-status
          reading-mode? dctx/reading-mode? card-refresh dctx/card-refresh !show-bib dctx/!show-bib
          show-bib? dctx/show-bib? total dctx/total layout dctx/layout is-live? dctx/is-live?
          scan-dpi dctx/scan-dpi scanning-pages dctx/scanning-pages ocr-errors dctx/ocr-errors
          phone? dctx/phone? !current-page dctx/!current-page toggle-layout! dctx/toggle-layout!
          bib-topic-id dctx/bib-topic-id]
        ;; TOP TOOLBAR (full width): command bar directly under the global
        ;; nav, above the bibliography header and content — like a Word/Excel
        ;; ribbon. Its actions target the topic/document, not the card table,
        ;; so it sits above everything. The card table stays at the bottom.
        ;; Derive/remap the toolbar sub-tree's ambient values; pass-throughs
        ;; (user-id, enc-key, llm-enabled?, navigate!, citation, page-info,
        ;; pdf-root?, pdf-status, reading-mode?, card-refresh, !show-bib) are
        ;; already bound at the root.
        (binding [dctx/topic-id page-topic-id
                  dctx/audio? (= kind "audio")
                  dctx/root-topic-id (or pdf-root-id root-topic-id)
                  dctx/page-number (when is-pdf? current-page)
                  dctx/static-content effective-content
                  dctx/context-mode (if is-pdf? :page :extract)
                  dctx/context-tooltip (if is-pdf?
                                         "Include context for better cards. With a selection: current page + N previous pages. Without: N previous pages."
                                         "Include context for better cards. With a selection: extract text. Without: original page text.")
                  dctx/extract-status (when-not is-pdf? extract-status)
                  dctx/origin (:origin queue-ctx)
                  dctx/on-done! (:on-done! queue-ctx)]
          (ToolbarBar))

        ;; SECOND BAR (full width, PDF only): all PDF-scoped controls —
        ;; page-nav, zoom, layout-toggle, done-checkbox, and the AI/extract
        ;; action buttons. Shown only when a PDF item is in view.
        (when is-pdf?
          (binding [dctx/page-number current-page
                    dctx/on-page-change! (fn [p] (reset! !current-page p))
                    dctx/on-layout-toggle! toggle-layout!]
            (PdfToolbar)))

        ;; THIRD BAR (Score only): pending-selection status + Add-card
        ;; dropdown; also hosts the notation-rect snapshot modal.
        (when dctx/is-score?
          (ScoreToolbar))

        ;; No third formatting bar (E1): the document editor uses Quill's
        ;; bubble theme, whose formatting controls float on text selection
        ;; instead of living in a fixed toolbar.

        ;; Auto-open biblio modal once after a fresh import. The Offload
        ;; returns the topic-id it CLAIMED (or nil) — so the e/for-by keys on
        ;; the claimed topic, NOT the live bib-topic-id. This is immune to the
        ;; latest-wins HOLD: during navigation the Offload holds the prior
        ;; claimed-id, whose frame is already mounted (no spurious re-open);
        ;; keying on the live bib-topic-id would remount on every nav while
        ;; the held value is still true. claim-show? clears the mark, so it
        ;; fires exactly once per imported (user, topic).
        (let [claimed-id (e/server
                           (e/Offload
                             #(when (bibform/claim-pending-biblio-show?* user-id bib-topic-id)
                                bib-topic-id)))]
          (e/for-by identity [_k (when claimed-id [claimed-id])]
            (let [opened (reset! !show-bib true)]
              (when opened nil))))

        ;; Bibliography modal — overlays everything when shown
        (when show-bib?
          (BibliographyForm !show-bib user-id bib-topic-id)))
    ))
