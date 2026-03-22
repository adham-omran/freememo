(ns electric-starter-app.pdf
  "Business logic for PDF document management."
  (:require
    [electric-starter-app.db :as db]
    [electric-starter-app.ocr :as ocr]))

(defn save-pdf [user-id filename file-bytes]
  (try
    (let [file-size (alength file-bytes)]
      ;; Validate file size (100MB limit matching WebSocket config)
      (when (> file-size (* 100 1024 1024))
        (throw (ex-info "File too large" {:size file-size})))

      (let [page-count (ocr/get-page-count file-bytes)
            topic (db/create-pdf-topic! user-id filename file-bytes file-size page-count)
            topic-id (:topics/id topic)]
        {:success true :id topic-id}))
    (catch Exception e
      (println "ERROR [save-pdf]:" (.getMessage e))
      {:success false :error (str "Failed to save PDF: " (.getMessage e))})))

(defn list-pdfs [user-id]
  (try
    (let [docs (db/get-root-topics user-id)]
      {:success true :documents docs})
    (catch Exception e
      (println "ERROR [list-pdfs]:" (.getMessage e))
      {:success false :error "Failed to load PDFs"})))

(defn delete-pdf [user-id id]
  (try
    (db/delete-topic-for-user! user-id id)
    {:success true}
    (catch Exception e
      (println "ERROR [delete-pdf]:" (.getMessage e))
      {:success false :error "Failed to delete PDF"})))
