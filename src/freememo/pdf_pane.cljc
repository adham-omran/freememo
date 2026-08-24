(ns freememo.pdf-pane
  "PDF viewer pane — wraps PdfViewerComponent + page-state.

   Split layout (PDF vs Editor) is owned by the caller (TopicPage). PdfPane
   fills 100% of its container. Page-nav, zoom and the layout toggle live in
   PdfToolbar (the second bar), not here.

   Last-page persistence is NOT here. It lived here until focus reader, which
   unmounts this pane — and a writer that unmounts stops recording the pages
   the reader turns. DocumentViewState owns the request atom and DocumentBody
   writes the setting; this pane only reports the page through
   `dctx/on-page-change!`."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.pdf-viewer-component :refer [PdfViewerComponent]]))

(e/defn PdfPane
  "Renders the PDF.js viewer and reports page changes upward.

   Reads from doc-context:
     :pdf-root-id     — root topic id for the PDF document
     :initial-page    — int page to start at. PdfPaneFrame binds this to the
                        LIVE page at mount, not the topic's resolved anchor, so
                        a remount does not send the reader back.
     :on-page-change! — called with the page on every real user navigation.
                        DocumentViewState owns it; it records the page as
                        current AND requests the last-page write.

   Returns the current page number (int)."
  []
  (e/client
    (let [pdf-root-id dctx/pdf-root-id initial-page dctx/initial-page
          target-page dctx/target-page is-live? dctx/is-live? has-file? dctx/has-file?
          reload-nonce dctx/reload-nonce on-page-change! dctx/on-page-change!
          on-total! dctx/on-total!
          !current-page   (atom (or initial-page 1))
          current-page    (e/watch !current-page)]

      (dom/div
        (dom/props {:style {:height "100%" :width "100%"
                            :min-width "0" :min-height "0" :overflow "hidden"}})

        ;; No e/for-by: the viewer persists across pdf-root-id changes and swaps
        ;; the document in-place (PdfViewerComponent → viewer/set-document!).
        ;; Remounting here caused the :diff-corruption WS crash on learn-session
        ;; advance.
        (reset! !current-page
          (binding [dctx/document-id pdf-root-id
                    dctx/on-navigate! (fn [p]
                                        ;; Reached only for real user nav: restore /
                                        ;; reflow-drift pagechanging is dropped upstream
                                        ;; at pdf-viewer-component (viewer/restoring?).
                                        (when on-page-change! (on-page-change! p)))
                    dctx/on-total! (fn [n] (when on-total! (on-total! n)))]
            (PdfViewerComponent))))

      current-page)))
