(ns freememo.page-ocr
  "Business logic for PDF page OCR operations."
  (:require
   [freememo.db :as db]
   [freememo.ocr :as ocr]
   [freememo.ocr-models :as ocr-models]
   [freememo.text :as text]
   [taoensso.telemere :as tel])
  (:import
   [java.util.concurrent Semaphore]))

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
               result (ocr/extract-text user-id topic-id pdf-bytes (dec page-number) enc-key dpi)]
           (if (:success result)
             (do
               (db/save-page-text! topic-id page-number (:text result))
               {:success true :text (:text result)})
             result))))
     (catch Exception e
       (tel/error! {:id ::extract-page-text} e)
       {:success false :error (str "Failed to extract text: " (.getMessage e))}))))

(defonce ^:private compare-ocr-limiter
  ;; Caps concurrent compare-modal OCR API calls (fan-out bound). The modal runs
  ;; one call per allowed model; unbounded, N models hit OpenRouter — and the JVM
  ;; heap — all at once. 2 keeps the compare feeling live without a stampede.
  (Semaphore. 2))

(defn prepare-compare-page
  "Load the topic file ONCE and render page `page-number` (1-based) into the OCR
   encodings the `model-ids` need, decoding the PDF a single time. Shared across
   the compare modal's models, so the document is read+decoded once per open — not
   once per model.
   Pre:  topic-id has a 'main' file; 1 <= page-number <= page count; dpi > 0.
   Post: {:success true :prepared {:image-url .. :pdf-file-b64 ..}}
         | {:success false :error msg}. The :prepared payload stays server-side."
  [topic-id model-ids page-number dpi]
  (try
    (let [file-row (db/get-topic-file topic-id)]
      (if-not file-row
        {:success false :error "Document not found"}
        (let [shapes (into #{} (keep #(:shape (ocr-models/resolve-model %)) model-ids))]
          {:success true
           :prepared (ocr/prepare-ocr-page (:topic_files/file_data file-row)
                       (dec page-number) dpi shapes)})))
    (catch Exception e
      (tel/error! {:id ::prepare-compare-page} e)
      {:success false :error (str "Failed to render page: " (.getMessage e))})))

(defn ocr-preview-prepared
  "Run one model's OCR on an already-rendered page WITHOUT saving (bills the call,
   like a normal scan). Bounded by `compare-ocr-limiter` so the modal's N models
   don't OCR all at once.
   Pre:  `prepared` (from prepare-compare-page) carries model-id's shape encoding.
   Post: {:success true :text html} | {:success false :error msg}."
  [user-id model-id prepared]
  (try
    (.acquire compare-ocr-limiter)
    (try
      (ocr/extract-text-with-model user-id model-id prepared)
      (finally (.release compare-ocr-limiter)))
    (catch Exception e
      (tel/error! {:id ::ocr-preview-prepared} e)
      {:success false :error (str "Failed to OCR: " (.getMessage e))})))

(defn compare-ocr-page
  "OCR page `page-number` (1-based) of `topic-id` with every model in `model-ids`,
   for the compare modal. Renders the page ONCE (prepare-compare-page) and shares
   it across models (C); each model runs concurrently but bounded by
   `compare-ocr-limiter` (B). WITHOUT saving; each model call bills like a scan.
   Pre:  topic-id has a 'main' file; 1 <= page-number <= page count.
   Post: a vector of {:model-id id :result {:success ..}} in `model-ids` order."
  [user-id topic-id model-ids page-number dpi]
  (let [prep (prepare-compare-page topic-id model-ids page-number dpi)]
    (if-not (:success prep)
      (mapv (fn [mid] {:model-id mid :result prep}) model-ids)
      (let [prepared (:prepared prep)]
        (->> model-ids
          (mapv (fn [mid]
                  (future {:model-id mid
                           :result (ocr-preview-prepared user-id mid prepared)})))
          (mapv deref))))))

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
            (let [html (-> (:text result)
                         text/normalize-extracted-text
                         text/text->paragraph-html)]
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
    (let [html (-> raw-text
                 text/normalize-extracted-text
                 text/text->paragraph-html)]
      (if (clojure.string/blank? html)
        {:success false :error empty-text-message}
        (do
          (db/save-page-text! topic-id page-number html)
          {:success true :text html})))
    (catch Exception e
      (tel/error! {:id ::save-pdfjs-text!} e)
      {:success false :error (str "Failed to save text: " (.getMessage e))})))

(defn preview-pdfbox-html
  "Extract a page's text via PDFBox → normalized paragraph HTML, WITHOUT saving.
   For the Copy-text compare modal (Remote/B). Returns {:success true :text html}
   or {:success false :error ...} (e.g. scanned page, no text layer)."
  [topic-id page-number]
  (try
    (let [file-row (db/get-topic-file topic-id)]
      (if-not file-row
        {:success false :error "Document not found"}
        (let [result (ocr/extract-text-pdfbox (:topic_files/file_data file-row) page-number)]
          (if-not (:success result)
            result
            (let [html (-> (:text result) text/normalize-extracted-text text/text->paragraph-html)]
              (if (clojure.string/blank? html)
                {:success false :error empty-text-message}
                {:success true :text html}))))))
    (catch Exception e
      (tel/error! {:id ::preview-pdfbox-html} e)
      {:success false :error (str "Failed to extract text: " (.getMessage e))})))

(defn preview-pdfjs-html
  "Normalize PDF.js raw text (extracted client-side) → paragraph HTML, WITHOUT
   saving. For the Copy-text compare modal (Client/A). Same shape as above."
  [raw-text]
  (try
    (let [html (-> (or raw-text "") text/normalize-extracted-text text/text->paragraph-html)]
      (if (clojure.string/blank? html)
        {:success false :error empty-text-message}
        {:success true :text html}))
    (catch Exception e
      (tel/error! {:id ::preview-pdfjs-html} e)
      {:success false :error (str "Failed to normalize text: " (.getMessage e))})))

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
