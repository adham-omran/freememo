(ns freememo.pdf-pane
  "PDF viewer pane — wraps PdfViewerComponent + page-state + last-page persistence.

   Split layout (PDF vs Editor) is owned by the caller (TopicPage). PdfPane
   fills 100% of its container. Page-nav, zoom, and the layout toggle live in
   PdfToolbar (the second bar), not here."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
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
  [{:keys [user-id pdf-root-id initial-page target-page
           is-live? has-file? reload-nonce
           on-page-change! on-total!]}]
  (e/client
    (let [!current-page   (atom (or initial-page 1))
          current-page    (e/watch !current-page)
          !page-to-save   (atom nil)
          page-to-save    (e/watch !page-to-save)
          [?page-token _] (e/Token page-to-save)]

      (when-some [token ?page-token]
        (log/log-debug (str "PdfPane: saving last page=" page-to-save
                            " doc=" pdf-root-id))
        (e/server (settings/save-last-page user-id pdf-root-id page-to-save))
        (token))

      (dom/div
        (dom/props {:style {:height "100%" :width "100%"
                            :min-width "0" :min-height "0" :overflow "hidden"}})

        ;; No e/for-by: the viewer persists across pdf-root-id changes and swaps
        ;; the document in-place (PdfViewerComponent → viewer/set-document!).
        ;; Remounting here caused the :diff-corruption WS crash on learn-session
        ;; advance.
        (reset! !current-page
          (PdfViewerComponent
            {:document-id pdf-root-id
             :initial-page initial-page
             :target-page target-page
             :is-live? is-live?
             :has-file? has-file?
             :reload-nonce reload-nonce
             :on-navigate! (fn [p]
                             (reset! !page-to-save p)
                             (when on-page-change! (on-page-change! p)))
             :on-total! (fn [n] (when on-total! (on-total! n)))})))

      current-page)))
