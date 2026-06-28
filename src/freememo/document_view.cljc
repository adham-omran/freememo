(ns freememo.document-view
  "View sub-components of TopicPage, split into their own namespace because
   Electric inlines *same-namespace* e/defn calls into the caller's compiled
   method — so a large page must cross a namespace boundary (a real frame
   boundary) to stay under the JVM's 64KB-per-method limit. TopicPage owns data
   resolution and passes everything in as a prop map; these render it."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.hierarchy-side-panel :refer [HierarchySidePanel]]
   [freememo.pin-side-panel :refer [PinSidePanel]]
   [freememo.pdf-pane :refer [PdfPane]]
   [freememo.pdf-toolbar :refer [PdfToolbar]]
   [freememo.editor-pane :refer [EditorPane]]
   [freememo.bottom-panel :refer [ToolbarBar]]
   [freememo.bibliography-form :as bibform :refer [BibliographyForm]]
   [freememo.navigation :as nav]
   [freememo.util :as util]))

(e/defn PdfPaneFrame
  "The PDF pane itself (PdfPane + its sized wrapper). Own e/defn for the 64KB
   method cap."
  [{:keys [user-id pdf-root-id initial-page target-page is-live?
           pdf-has-file? pdf-page-count top-bottom? top-split-pct left-pct
           !current-page !total]}]
  (e/client
    (let [pdf-style (if top-bottom?
                      {:height (str top-split-pct "%")
                       :min-height "0" :overflow "hidden"}
                      {:width (str left-pct "%")
                       :min-width "0" :overflow "hidden"})]
      (dom/div
        (dom/props {:style pdf-style})
        (reset! !current-page
          (PdfPane {:user-id user-id
                    :pdf-root-id pdf-root-id
                    :initial-page initial-page
                    :target-page target-page
                    :is-live? is-live?
                    :has-file? pdf-has-file?
                    :reload-nonce pdf-page-count
                    :on-page-change! (fn [p] (reset! !current-page p))
                    :on-total! (fn [n] (reset! !total n))}))))))

(e/defn PaneDragHandle
  "Drag handle between the PDF pane and the editor. Own e/defn for the 64KB
   method cap."
  [{:keys [top-bottom? !top-split-pct !left-pct]}]
  (e/client
    (dom/div
      (dom/props {:class (if top-bottom? "split-divider-v" "split-divider-h")
                  :title "Drag to resize panels"})
      (dom/On "pointerdown"
        (fn [e]
          (if top-bottom?
            (util/start-drag! e :y !top-split-pct)
            (util/start-drag! e :x !left-pct)))
        nil))))

(e/defn PdfSplitPane
  "The PDF half of a document split: PdfPaneFrame + PaneDragHandle. Renders
   nothing for non-PDF topics. Thin shell — work lives in its two children so no
   single e/defn exceeds Electric's 64KB-per-method cap."
  [{:keys [is-pdf?] :as props}]
  (e/client
    (when is-pdf?
      (PdfPaneFrame props)
      (PaneDragHandle props))))

(e/defn ContentColumn
  "Middle content column of a document view: the PdfPane/EditorPane split, laid
   out as a row (left-right) or column (top-bottom). Own e/defn for the 64KB
   method cap. Forwards the full prop map to PdfSplitPane."
  [{:keys [user-id page-topic-id kind is-pdf? top-bottom? effective-content navigate!]
    :as props}]
  (e/client
    (dom/div
      (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                          :min-width "0" :min-height "0" :overflow "hidden"}})
      (dom/div
        (dom/props {:style {:flex "1" :display "flex"
                            :flex-direction (if (and is-pdf? top-bottom?)
                                              "column"
                                              "row")
                            :min-height "0" :overflow "hidden"}})
        (PdfSplitPane props)
        (EditorPane
          {:user-id user-id
           :topic-id page-topic-id
           :audio-topic-id (when (= kind "audio") page-topic-id)
           :is-pdf-page? is-pdf?
           :static-content effective-content
           :on-imported-navigate!
           (fn [tid]
             (when navigate!
               (navigate! :viewer (nav/nav-topic tid nil))))})))))

(e/defn DocumentColumns
  "Resizable top region of a document view: hierarchy sidebar | content column
   (ContentColumn) | pin sidebar. Split across ContentColumn/PdfSplitPane so no
   single e/defn exceeds Electric's 64KB-per-method cap. Behaviour unchanged;
   the columns are reactively independent siblings. Forwards the prop map down."
  [{:keys [user-id page-topic-id root-topic-id navigate!
           is-pdf? top-pct reading-mode? target-page !nav-target]
    :as props}]
  (e/client
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
      (ContentColumn props)

      ;; RIGHT: pin side panel (collapsible). Hidden in reading-mode?.
      (when-not reading-mode?
        (PinSidePanel page-topic-id root-topic-id user-id)))))


;; TopicPage shell
;; ---------------------------------------------------------------------------

(e/defn DocumentToolbars
  "Above-the-content chrome: ToolbarBar (command ribbon), the PDF-only
   PdfToolbar, the one-shot biblio auto-open, and the bibliography modal.
   Renders into the caller's dom parent. Own e/defn for Electric's 64KB
   method cap. card-refresh/!show-bib are passed positionally to ToolbarBar."
  [{:keys [user-id enc-key page-topic-id kind pdf-root-id root-topic-id
           is-pdf? current-page effective-content llm-enabled? extract-status
           navigate! queue-ctx citation page-info pdf-root? pdf-status
           reading-mode? card-refresh !show-bib show-bib? total layout is-live?
           scan-dpi scanning-pages ocr-errors phone? !current-page toggle-layout!
           bib-topic-id]}]
  (e/client
        ;; TOP TOOLBAR (full width): command bar directly under the global
        ;; nav, above the bibliography header and content — like a Word/Excel
        ;; ribbon. Its actions target the topic/document, not the card table,
        ;; so it sits above everything. The card table stays at the bottom.
        (ToolbarBar
          {:user-id user-id
           :enc-key enc-key
           :topic-id page-topic-id
           :audio? (= kind "audio")
           :root-topic-id (or pdf-root-id root-topic-id)
           :page-number (when is-pdf? current-page)
           :static-content effective-content
           :context-mode (if is-pdf? :page :extract)
           :context-tooltip (if is-pdf?
                              "Include context for better cards. With a selection: current page + N previous pages. Without: N previous pages."
                              "Include context for better cards. With a selection: extract text. Without: original page text.")
           :llm-enabled? llm-enabled?
           :extract-status (when-not is-pdf? extract-status)
           :navigate! navigate!
           :origin (:origin queue-ctx)
           :on-done! (:on-done! queue-ctx)
           :citation citation
           :page-info page-info
           :pdf-root? pdf-root?
           :pdf-status pdf-status
           :reading-mode? reading-mode?}
          card-refresh
          !show-bib)

        ;; SECOND BAR (full width, PDF only): all PDF-scoped controls —
        ;; page-nav, zoom, layout-toggle, done-checkbox, and the AI/extract
        ;; action buttons. Shown only when a PDF item is in view.
        (when is-pdf?
          (PdfToolbar
            {:user-id user-id
             :enc-key enc-key
             :pdf-root-id pdf-root-id
             :page-number current-page
             :total total
             :layout layout
             :is-live? is-live?
             :scan-dpi scan-dpi
             :llm-enabled? llm-enabled?
             :scanning-pages scanning-pages
             :ocr-errors ocr-errors
             :phone? phone?
             :on-page-change! (fn [p] (reset! !current-page p))
             :on-layout-toggle! toggle-layout!}))

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
          (BibliographyForm !show-bib user-id bib-topic-id))
    ))
