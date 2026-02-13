(ns electric-starter-app.pdf
  "Business logic for PDF document management."
  (:require
    [electric-starter-app.db :as db]))

(defn save-pdf [filename file-bytes]
  (try
    (let [file-size (alength file-bytes)
          mime-type "application/pdf"]
      ;; Validate file size (100MB limit matching WebSocket config)
      (when (> file-size (* 100 1024 1024))
        (throw (ex-info "File too large" {:size file-size})))

      ;; Save to database
      (let [result (db/save-document filename file-bytes file-size mime-type)]
        {:success true :id (:documents/id (first result))}))
    (catch Exception e
      (println "ERROR [save-pdf]:" (.getMessage e))
      {:success false :error (str "Failed to save PDF: " (.getMessage e))})))

(defn list-pdfs []
  (try
    (let [docs (db/get-documents)]
      {:success true :documents docs})
    (catch Exception e
      (println "ERROR [list-pdfs]:" (.getMessage e))
      {:success false :error "Failed to load PDFs"})))

(defn delete-pdf [id]
  (try
    (db/delete-document id)
    {:success true}
    (catch Exception e
      (println "ERROR [delete-pdf]:" (.getMessage e))
      {:success false :error "Failed to delete PDF"})))
