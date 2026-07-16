(ns freememo.pdf-pane
  "PDF viewer pane — wraps PdfViewerComponent + page-state + last-page persistence.

   Split layout (PDF vs Editor) is owned by the caller (TopicPage). PdfPane
   fills 100% of its container. Page-nav, zoom, and the layout toggle live in
   PdfToolbar (the second bar), not here."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.logging :as log]
   [freememo.pdf-viewer-component :refer [PdfViewerComponent]]
   #?(:clj [freememo.settings :as settings])))

(e/defn PdfPane
  "Renders the PDF.js viewer and persists last-page on navigation.

   Props:
     :user-id         — identity used for last-page persistence
     :pdf-root-id     — root topic id for the PDF document
     :initial-page    — int page to start at
     :on-page-change! — fn called with current-page int when user navigates
     :on-total!       — fn called with the PDF page count when known/changed

   Returns the current page number (int)."
  []
  (e/client
    (let [user-id dctx/user-id pdf-root-id dctx/pdf-root-id initial-page dctx/initial-page
          target-page dctx/target-page is-live? dctx/is-live? has-file? dctx/has-file?
          reload-nonce dctx/reload-nonce on-page-change! dctx/on-page-change! on-total! dctx/on-total!
          !current-page   (atom (or initial-page 1))
          current-page    (e/watch !current-page)
          !page-to-save   (atom nil)
          page-to-save    (e/watch !page-to-save)
          [?page-token _] (e/Token page-to-save)]

      (when-some [token ?page-token]
        (log/log-debug (str "PdfPane: saving last page=" page-to-save
                            " doc=" pdf-root-id))
        (case (e/server (e/Offload #(settings/save-last-page user-id pdf-root-id page-to-save)))
          (token)))

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
                                        (reset! !page-to-save p)
                                        (when on-page-change! (on-page-change! p)))
                    dctx/on-total! (fn [n] (when on-total! (on-total! n)))]
            (PdfViewerComponent))))

      current-page)))
