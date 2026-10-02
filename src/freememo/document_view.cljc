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
   [freememo.right-side-panel :refer [RightSidePanel]]
   [freememo.pdf-pane :refer [PdfPane]]
   [freememo.pdf-toolbar :refer [PdfToolbar]]
   [freememo.score-toolbar :refer [ScoreToolbar ScoreWaveformStrip]]
   [freememo.video-pane :refer [VideoPane]]
   [freememo.video-transcript :refer [VideoTranscriptPane]]
   [freememo.code-pane :refer [CodePane]]
   [freememo.repo-update-bar :refer [RepoUpdateBar]]
   [freememo.editor-pane :refer [EditorPane]]
   [freememo.poem-pane :refer [PoemPane]]
   [freememo.bottom-panel :refer [ToolbarBar]]
   [freememo.bibliography-form :as bibform :refer [BibliographyForm]]
   [freememo.document-options :refer [DocumentOptionsModal]]
   [freememo.document-options-targets :as doc-options-targets]
   [freememo.navigation :as nav]
   [freememo.util :as util]))

(e/defn PdfPaneFrame
  "The PDF pane itself (PdfPane + its sized wrapper). Own e/defn for the 64KB
   method cap.

   Pre : dctx/current-page holds the page the reader is on.
   Post: the viewer opens at that page, and dctx/!current-page keeps its value
         across a mount of this frame."
  []
  (e/client
    (let [user-id dctx/user-id pdf-root-id dctx/pdf-root-id
          target-page dctx/target-page is-live? dctx/is-live? pdf-has-file? dctx/pdf-has-file?
          pdf-page-count dctx/pdf-page-count top-bottom? dctx/top-bottom?
          top-split-pct dctx/top-split-pct left-pct dctx/left-pct
          !current-page dctx/!current-page !total dctx/!total
          ;; The page the viewer opens at. `dctx/initial-page` is resolved ONCE
          ;; per topic (URL anchor, else the persisted last page), and both
          ;; PdfPane and PdfViewerComponent seed their page cursor from it. That
          ;; was the live page while this frame mounted exactly once per topic.
          ;; Focus reader unmounts and remounts it, so on the second mount that
          ;; value is stale — and PdfPane's return then overwrites
          ;; dctx/!current-page with it, throwing away the page the reader had
          ;; navigated to while the pane was gone.
          ;;
          ;; e/snapshot reads the live page at THIS mount and freezes it: a
          ;; later flip must not re-navigate the viewer, which owns the cursor
          ;; from then on. On a first mount the two agree, because
          ;; DocumentViewState seeds !current-page from initial-page.
          resume-page (e/snapshot (or dctx/current-page dctx/initial-page 1))
          pdf-style (if top-bottom?
                      {:height (str top-split-pct "%")
                       :min-height "0" :overflow "hidden"}
                      {:width (str left-pct "%")
                       :min-width "0" :overflow "hidden"})]
      (dom/div
        (dom/props {:style pdf-style})
        (reset! !current-page
          ;; on-page-change! is NOT bound here. DocumentRoot binds the one
          ;; funnel that DocumentViewState owns, so the viewer's page changes
          ;; and the toolbar's reach the same fn — and both request the
          ;; last-page write. Two identical local copies of it recorded
          ;; neither.
          (binding [dctx/has-file? pdf-has-file?
                    dctx/initial-page resume-page
                    dctx/reload-nonce pdf-page-count
                    dctx/on-total! (fn [n] (reset! !total n))]
            (PdfPane)))))))

(e/defn PaneDragHandle
  "One resize handle between two panes. Own e/defn for the 64KB method cap.

   Axis, target atom and label are ARGUMENTS rather than derived from
   dctx/top-bottom?: a video document mounts two handles in one view (the
   video|editor column split and the video-stack|transcript split), so a single
   ambient switch can no longer identify which split a handle drives.

   Pre:  `axis` is :x or :y; `!pct` holds a number 0-100; `pct` is its current
         value; `label` names the two panes for assistive tech.
   Post: pointer drag and arrow keys move `!pct` along `axis`.
   Invariant: the handle mutates only `!pct` — it owns no layout state itself."
  [axis !pct pct label]
  (e/client
    (let [vertical-split? (= axis :y)]
      (dom/div
        (dom/props {:class (if vertical-split? "split-divider-v" "split-divider-h")
                    :title "Drag to resize panels"
                    :role "separator"
                    :aria-orientation (if vertical-split? "horizontal" "vertical")
                    :aria-label label :tabindex "0"
                    :aria-valuenow (str (int (or pct 0)))})
        (dom/On "pointerdown" (fn [e] (util/start-drag! e axis !pct)) nil)
        (dom/On "keydown" (fn [e] (util/key-resize-pct! e axis !pct nil)) nil)))))

(e/defn PdfSplitPane
  "The PDF half of a document split: PdfPaneFrame + PaneDragHandle. Renders
   nothing for non-PDF topics. Thin shell — work lives in its two children so no
   single e/defn exceeds Electric's 64KB-per-method cap.

   dctx/focus? (focus reader) removes the pane and its handle together, which
   destroys the PDF.js viewer (PdfViewerComponent's e/on-unmount). The editor
   then takes the whole content column. Page nav survives the teardown:
   viewer/go-to-page! is a silent no-op with no viewer, the editor text keys on
   the page NUMBER, and !total is seeded from the server page count so the
   arrow buttons do not disable (see topic-state/DocumentViewState)."
  []
  (e/client
    (when (and dctx/is-pdf? (not dctx/focus?))
      (PdfPaneFrame)
      (if dctx/top-bottom?
        (PaneDragHandle :y dctx/!top-split-pct dctx/top-split-pct
          "Resize PDF and editor panes")
        (PaneDragHandle :x dctx/!left-pct dctx/left-pct
          "Resize PDF and editor panes")))))

(e/defn VideoSplitPane
  "The video half of a video document: player, waveform strip, a vertical
   handle, then the transcript — stacked in a column sized by left-pct, with the
   editor beside it. Renders nothing for non-video topics, mirroring
   PdfSplitPane. Own e/defn for the 64KB method cap.

   The transcript lives here rather than in RightSidePanel: it is read against
   the video, so it belongs in the video's column. This reverses the rationale
   previously recorded in ContentColumn.

   Pre:  dctx/video-topic-id is bound by the caller (VideoTranscriptPane and
         VideoPane both read it).
   Post: DOM order is player, waveform, handle, transcript."
  []
  (e/client
    (when dctx/is-video?
      (dom/div
        (dom/props {:style {:width (str dctx/left-pct "%")
                            :display "flex" :flex-direction "column"
                            :min-width "0" :min-height "0" :overflow "hidden"}})
        ;; The stack's share of the column height; the transcript below is
        ;; flex:1 and takes the remainder.
        (dom/div
          (dom/props {:style {:height (str dctx/transcript-pct "%")
                              :display "flex" :flex-direction "column"
                              :min-height "0" :overflow "hidden"}})
          (VideoPane))
        (PaneDragHandle :y dctx/!transcript-pct dctx/transcript-pct
          "Resize video and transcript")
        (VideoTranscriptPane))
      (PaneDragHandle :x dctx/!left-pct dctx/left-pct
        "Resize video and editor panes"))))

(e/defn CodeSplitPane
  "The code half of a file imported from a git forge: CodePane in a column sized
   by left-pct, then its handle, with the editor beside it. Renders nothing for
   any other topic, mirroring PdfSplitPane. Own e/defn for the 64KB method cap
   (plans/git-forge-integration.md §4.4 4.3)."
  []
  (e/client
    (when dctx/is-code?
      (dom/div
        (dom/props {:style {:width (str dctx/left-pct "%")
                            :display "flex" :flex-direction "column"
                            :min-width "0" :min-height "0" :overflow "hidden"}})
        (CodePane))
      (PaneDragHandle :x dctx/!left-pct dctx/left-pct
        "Resize code and editor panes"))))

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
        ;; Repository nodes: the Update control across the top, the one place it
        ;; exists (plans/git-forge-integration.md §4.6 6.1).
        (when (= kind "repo")
          (RepoUpdateBar))
        (dom/div
          ;; The focus-reader class is the ONLY scope for the centered-column
          ;; CSS. `.ql-editor` also styles card fields and every other Quill
          ;; surface, so an unscoped padding rule would reach a card modal.
          (dom/props {:class (if (and dctx/focus-available? dctx/focus?) "focus-reader-column" "")
                      :style {:flex "1" :display "flex"
                              :flex-direction (if (and is-pdf? top-bottom?)
                                                "column"
                                                "row")
                              :min-height "0" :overflow "hidden"}})
          (PdfSplitPane)
          ;; Video topics: the video column (player, waveform, transcript) sits
          ;; INSIDE this row, so the editor is beside it rather than below it.
          ;; All four surfaces of §4.8 8.1 remain visible and independently
          ;; usable; only their arrangement changed.
          (binding [dctx/video-topic-id page-topic-id]
            (VideoSplitPane))
          ;; Files from a git forge: read-only source beside the notes editor.
          (CodeSplitPane)
          (binding [dctx/topic-id page-topic-id
                    dctx/audio-topic-id (when (= kind "audio") page-topic-id)
                    dctx/is-pdf-page? is-pdf?
                    dctx/static-content effective-content
                    dctx/on-imported-navigate!
                    (fn [tid]
                      (when navigate!
                        (navigate! :viewer (nav/nav-topic tid nil))))]
            ;; A poem gets the read-only reader INSTEAD of the editor, not
            ;; beside it (spec row 3.2.1). EditorPane owns the Quill instance
            ;; and the auto-save that writes topics.content on every
            ;; text-change, so mounting it for a poem would make the poem
            ;; editable and let one keystroke overwrite the rendered fallback
            ;; the importer stored. A poem page has no .ql-editor.
            (if dctx/is-poem? (PoemPane) (EditorPane))))))))

(e/defn DocumentColumns
  "Resizable top region of a document view: left panel, content, and optional
   assistant panel. Split across ContentColumn/PdfSplitPane so no single e/defn
   exceeds Electric's 64KB-per-method cap. The columns are independent reactive
   siblings. Reads ambient doc-context."
  [show-assistant? card-table-open?]
  (e/client
    (let [user-id dctx/user-id page-topic-id dctx/page-topic-id
          root-topic-id dctx/root-topic-id navigate! dctx/navigate!
          is-pdf? dctx/is-pdf? top-pct dctx/top-pct
          target-page dctx/target-page !nav-target dctx/!nav-target]
      (dom/div
        ;; The content region fills the workspace when the card table is closed.
        ;; The height lives in CSS, not inline, so a phone rule can release it
        ;; when the card table is empty.
        (dom/props {:class (str "document-columns"
                             (when card-table-open? " document-columns--split"))
                    :style {:--top-pct top-pct}})

        ;; Clear nav-target after deriving target-page, so the viewer can navigate
        ;; away manually without being snapped back.
        (when target-page (reset! !nav-target nil))

        ;; LEFT: hierarchy side panel. Viewer commands can reveal it at every
        ;; viewport, including the mobile Learn reader.
        (HierarchySidePanel user-id page-topic-id root-topic-id navigate!
          (when is-pdf? !nav-target))

        ;; MIDDLE: content column.
        (ContentColumn)

        ;; RIGHT: collapsible assistant panel.
        (when show-assistant?
          (RightSidePanel page-topic-id root-topic-id user-id dctx/is-poem?))))))


;; TopicPage shell
;; ---------------------------------------------------------------------------

(e/defn DocumentToolbars
  "Above-the-content chrome: ToolbarBar (command ribbon), the PDF-only
   PdfToolbar, the one-shot biblio auto-open, and the bibliography modal.
   Renders into the caller's dom parent. Own e/defn for Electric's 64KB
   method cap. !show-bib is passed positionally to ToolbarBar."
  []
  (e/client
    (let [user-id dctx/user-id enc-key dctx/enc-key page-topic-id dctx/page-topic-id
          kind dctx/kind pdf-root-id dctx/pdf-root-id root-topic-id dctx/root-topic-id
          is-pdf? dctx/is-pdf? current-page dctx/current-page effective-content dctx/effective-content
          llm-enabled? dctx/llm-enabled? extract-status dctx/extract-status
          navigate! dctx/navigate! queue-ctx dctx/queue-ctx citation dctx/citation
          page-info dctx/page-info pdf-root? dctx/pdf-root? pdf-status dctx/pdf-status
          reading-mode? dctx/reading-mode? !show-bib dctx/!show-bib
          show-bib? dctx/show-bib? total dctx/total layout dctx/layout is-live? dctx/is-live?
          scan-dpi dctx/scan-dpi scanning-pages dctx/scanning-pages ocr-errors dctx/ocr-errors
          phone? dctx/phone? toggle-layout! dctx/toggle-layout!
          bib-topic-id dctx/bib-topic-id
          !doc-options-open dctx/!doc-options-open doc-options-open? dctx/doc-options-open?
          ;; The toolbar sub-tree's ids, bound below and read by the Document
          ;; options mount at the end.
          toolbar-root-id (or pdf-root-id root-topic-id)
          context-mode (if is-pdf? :page :extract)
          toolbar-extract-status (when-not is-pdf? extract-status)]
        ;; URL anchor write-back: reflect the live page as /viewer/topic/<root>/<page>
        ;; (real PDF root only — score/page kinds don't honour a URL page on
        ;; restore). replaceState: no history entry, no router re-resolve, no
        ;; viewer remount. Reads current-page, so it re-runs on scroll or toolbar
        ;; nav alike.
        ;; No restore guard needed here: current-page never reflects a restore or
        ;; its reflow drift — pdf-viewer-component drops those pagechanging events
        ;; at the boundary (viewer/restoring?), so current-page only ever holds a
        ;; page the user actually navigated to.
        (when (and pdf-root? current-page)
          (nav/set-url-page! pdf-root-id current-page))

        ;; TOP TOOLBAR (full width): command bar directly under the global
        ;; nav, above the bibliography header and content — like a Word/Excel
        ;; ribbon. Its actions target the topic/document, not the card table,
        ;; so it sits above everything. The card table stays at the bottom.
        ;; Derive/remap the toolbar sub-tree's ambient values; pass-throughs
        ;; (user-id, enc-key, llm-enabled?, navigate!, citation, page-info,
        ;; pdf-root?, pdf-status, reading-mode?, !show-bib) are
        ;; already bound at the root.
        (binding [dctx/topic-id page-topic-id
                  dctx/audio? (= kind "audio")
                  dctx/root-topic-id toolbar-root-id
                  dctx/page-number (when is-pdf? current-page)
                  dctx/static-content effective-content
                  dctx/context-mode context-mode
                  dctx/context-tooltip (if is-pdf?
                                         "Include context for better cards. With a selection: current page + N previous pages. Without: N previous pages."
                                         "Include context for better cards. With a selection: extract text. Without: original page text.")
                  dctx/extract-status toolbar-extract-status
                  dctx/origin (:origin queue-ctx)
                  dctx/on-done! (:on-done! queue-ctx)]
          (ToolbarBar))

        ;; SECOND BAR (full width, PDF only): all PDF-scoped controls —
        ;; page-nav, zoom, layout-toggle, done-checkbox, and the AI/extract
        ;; action buttons. Shown only when a PDF item is in view.
        (when is-pdf?
          ;; on-page-change! comes from DocumentRoot — see PdfPaneFrame.
          (binding [dctx/page-number current-page
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
            (reset! !show-bib true)))

        ;; Document options — mounted here, outside the content toolbar, whose
        ;; collapse tiers hide every .icon-label inside .toolbar-container
        ;; (plans/document-options-autosave-sheet.md 1.2). It never shows with
        ;; the bibliography modal: its Edit button closes it first.
        (when doc-options-open?
          (DocumentOptionsModal user-id
            (doc-options-targets/targets
              {:context-mode context-mode :extract-status toolbar-extract-status
               :root-topic-id toolbar-root-id :topic-id page-topic-id})
            is-pdf? !doc-options-open !show-bib true))

        ;; Bibliography modal — overlays everything when shown
        (when show-bib?
          (BibliographyForm !show-bib user-id bib-topic-id)))
    ))
