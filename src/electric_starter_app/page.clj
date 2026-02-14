(ns electric-starter-app.page
  "Business logic for PDF page OCR operations."
  (:require
    [electric-starter-app.db :as db]
    [electric-starter-app.ocr :as ocr]))

(defn extract-page-text
  "Extract text from a specific page of a document and save to database.
   Returns {:success true :text ...} or {:success false :error ...}"
  [document-id page-number]
  (try
    ;; Get the PDF bytes from database
    (let [docs (db/get-documents-by-id document-id)
          doc (first docs)]
      (if-not doc
        {:success false :error "Document not found"}
        (let [pdf-bytes (:documents/file_data doc)
              ;; Extract text using OCR
              result (ocr/extract-text pdf-bytes (dec page-number))]  ; Convert to 0-indexed
          (if (:success result)
            (do
              ;; Save to database
              (db/save-page-text document-id page-number (:text result))
              {:success true :text (:text result)})
            result))))
    (catch Exception e
      (println "ERROR [extract-page-text]:" (.getMessage e))
      {:success false :error (str "Failed to extract text: " (.getMessage e))})))

(defn get-page-text
  "Retrieve saved OCR text for a page."
  [document-id page-number]
  (try
    (if-let [page (db/get-page-text document-id page-number)]
      {:success true :text (:pages/text page)}
      {:success false :error "No text found for this page"})
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn list-extracted-pages
  "List all pages that have been OCR processed for a document."
  [document-id]
  (try
    (let [pages (db/list-pages document-id)]
      {:success true :pages pages})
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn save-page-html-impl
  "Save edited OCR text (as HTML) for a page. Returns {:success true} or {:success false :error msg}."
  [document-id page-number html]
  (try
    (db/save-page-text document-id page-number html)  ; Reuse existing function, now stores HTML
    {:success true}
    (catch Exception e
      (println "ERROR [save-page-html-impl]:" (.getMessage e))
      {:success false :error "Failed to save text"})))
