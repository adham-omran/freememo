(ns freememo.pdf
  "Business logic for PDF document management."
  (:require
    [freememo.db :as db]
    [freememo.ocr :as ocr]
    [taoensso.telemere :as tel]))

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
      (tel/error! {:id ::save-pdf} e)
      {:success false :error (str "Failed to save PDF: " (.getMessage e))})))

(defn list-pdfs [user-id]
  (try
    (let [docs (db/get-root-topics user-id)]
      {:success true :documents docs})
    (catch Exception e
      (tel/error! {:id ::list-pdfs} e)
      {:success false :error "Failed to load PDFs"})))

(defn delete-pdf [user-id id]
  (try
    (db/delete-topic-for-user! user-id id)
    {:success true}
    (catch Exception e
      (tel/error! {:id ::delete-pdf} e)
      {:success false :error "Failed to delete PDF"})))
