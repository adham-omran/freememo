(ns freememo.page-ocr
  "Business logic for PDF page OCR operations."
  (:require
   [freememo.db :as db]
   [freememo.ocr :as ocr]
   [freememo.text :as text]
   [taoensso.telemere :as tel]))

(def empty-text-message "No text extracted — try OCR")

(defn extract-page-text
  "Extract text from a specific page of a PDF topic and save to database.
   topic-id is the root PDF topic ID.
   Returns {:success true :text ...} or {:success false :error ...}
   DPI defaults to 150 if not provided."
  ([user-id topic-id page-number enc-key]
   (extract-page-text user-id topic-id page-number enc-key 150))
  ([user-id topic-id page-number enc-key dpi]
   (try
     (let [file-row (db/get-topic-file topic-id)]
       (if-not file-row
         {:success false :error "Document not found"}
         (let [pdf-bytes (:topic_files/file_data file-row)
               result (ocr/extract-text user-id pdf-bytes (dec page-number) enc-key dpi)]
           (if (:success result)
             (do
               (db/save-page-text! topic-id page-number (:text result))
               {:success true :text (:text result)})
             result))))
     (catch Exception e
       (tel/error! {:id ::extract-page-text} e)
       {:success false :error (str "Failed to extract text: " (.getMessage e))}))))

(defn extract-page-text-pdfbox
  "Extract text from a page via PDFBox (no LLM). Wraps the result in <p> blocks
   and saves to the page topic. Returns {:success false :error ...} when no text
   could be extracted (typical for scanned PDFs without a text layer)."
  [topic-id page-number]
  (try
    (let [file-row (db/get-topic-file topic-id)]
      (if-not file-row
        {:success false :error "Document not found"}
        (let [pdf-bytes (:topic_files/file_data file-row)
              result (ocr/extract-text-pdfbox pdf-bytes page-number)]
          (if-not (:success result)
            result
            (let [html (text/text->paragraph-html (:text result))]
              (if (clojure.string/blank? html)
                {:success false :error empty-text-message}
                (do
                  (db/save-page-text! topic-id page-number html)
                  {:success true :text html})))))))
    (catch Exception e
      (tel/error! {:id ::extract-page-text-pdfbox} e)
      {:success false :error (str "Failed to extract text: " (.getMessage e))})))

(defn save-pdfjs-text!
  "Persist text already extracted client-side via PDF.js. Wraps in <p> blocks
   and saves. Returns {:success false :error ...} when the input is empty after
   normalization (e.g., scanned page with no text layer)."
  [topic-id page-number raw-text]
  (try
    (let [html (text/text->paragraph-html raw-text)]
      (if (clojure.string/blank? html)
        {:success false :error empty-text-message}
        (do
          (db/save-page-text! topic-id page-number html)
          {:success true :text html})))
    (catch Exception e
      (tel/error! {:id ::save-pdfjs-text!} e)
      {:success false :error (str "Failed to save text: " (.getMessage e))})))

(defn get-page-text
  "Retrieve saved OCR text for a page."
  [parent-id page-number]
  (try
    (if-let [page (db/get-page-text parent-id page-number)]
      {:success true :text (or (:topics/content page) "")}
      {:success false :error "Page not found"})
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn list-extracted-pages
  "List all pages that have been OCR processed for a document."
  [parent-id]
  (try
    (let [pages (db/list-pages parent-id)]
      {:success true :pages pages})
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn save-page-html-impl
  "Save edited OCR text (as HTML) for a page. Returns {:success true} or {:success false :error msg}."
  [parent-id page-number html]
  (try
    (db/save-page-text! parent-id page-number html)
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-page-html-impl} e)
      {:success false :error "Failed to save text"})))
