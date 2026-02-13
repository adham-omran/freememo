(ns electric-starter-app.pdf-page
  "PDF upload and management UI."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]
    #?(:clj [electric-starter-app.pdf :as pdf])))

#?(:clj (defonce !refresh (atom 0)))  ; Server-side refresh trigger

;; Query wrapper: takes refresh arg to create Electric reactive dependency
#?(:clj (defn list-pdfs* [_refresh] (pdf/list-pdfs)))

;; Helper: format bytes to human-readable
#?(:clj
   (defn format-bytes [bytes]
     (if (nil? bytes)
       "Unknown size"
       (cond
         (< bytes 1024) (str bytes " B")
         (< bytes (* 1024 1024)) (format "%.1f KB" (double (/ bytes 1024)))
         :else (format "%.1f MB" (double (/ bytes 1024 1024)))))))

;; PDF list item component
(e/defn PdfListItem [doc]
  (e/client
    (let [id (e/server (:documents/id doc))
          filename (e/server (:documents/filename doc))
          file-size (e/server (:documents/file_size doc))
          uploaded-at (e/server (str (:documents/uploaded_at doc)))

          !deleting (atom false)
          deleting (e/watch !deleting)]

      (dom/li
        (dom/div
          (dom/strong (dom/text filename))
          (dom/text " ")
          (dom/span
            (dom/props {:style {:color "gray"}})
            (dom/text "(" (e/server (format-bytes file-size)) ", " uploaded-at ")")))

        (dom/button
          (dom/props {:disabled deleting})
          (dom/text (if deleting "Deleting..." "Delete"))
          (let [click-event (dom/On "click" identity nil)
                [?token ?error] (e/Token click-event)]

            (when ?error
              (dom/text " ")
              (dom/span
                (dom/props {:style {:color "red"}})
                (dom/text ?error)))

            (when-some [token ?token]
              (reset! !deleting true)
              (let [result (e/server (pdf/delete-pdf id))]
                (if (:success result)
                  (do
                    (e/server (swap! !refresh inc))  ; Trigger server-side refresh
                    (token))  ; Close token to reset button state
                  (do
                    (reset! !deleting false)
                    (token (:error result))))))))))))

;; Main PDF page component
(e/defn PdfPage []
  (e/client
    (dom/div
      (dom/h1 (dom/text "PDF Documents"))

      ;; Upload form using traditional POST
      (dom/p (dom/text "To upload a PDF, use the form below:"))
      (dom/form
        (dom/props {:action "/api/upload-pdf" :method "post" :enctype "multipart/form-data"})
        (dom/input
          (dom/props {:type "file" :name "file" :accept "application/pdf" :required true}))
        (dom/button
          (dom/props {:type "submit"})
          (dom/text "Upload")))
      (dom/p
        (dom/props {:style {:font-size "0.9em" :color "gray"}})
        (dom/text "Note: Page will reload after upload."))

      ;; Document list
      (dom/h2 (dom/text "Uploaded Documents"))
      (e/server
        (let [refresh (e/watch !refresh)
              docs-result (list-pdfs* refresh)]
          (e/client
            (if (:success docs-result)
              (if (seq (:documents docs-result))
                (dom/ul
                  (e/server
                    (e/for-by :documents/id [doc (:documents docs-result)]
                      (e/client (PdfListItem doc)))))
                (dom/p (dom/text "No documents uploaded yet.")))
              (dom/div
                (dom/props {:style {:color "red"}})
                (dom/text "Error loading documents: " (:error docs-result))))))))))
