(ns freememo.ocr
  "OCR text extraction. Vision chat lane (OpenAI/Gemini via OpenRouter) renders
   the page to an image; the Mistral OCR lane sends a single-page PDF through
   OpenRouter's file-parser plugin and reads the parsed markdown back."
  (:require
   [freememo.settings :as settings]
   [freememo.credits :as credits]
   [freememo.config :as config]
   [freememo.ocr-models :as models]
   [freememo.html-cleaner :as cleaner]
   [freememo.text :as text]
   [freememo.openrouter :as openrouter]
   [clojure.string :as str]
   [taoensso.telemere :as tel])
  (:import
   [org.apache.pdfbox Loader]
   [org.apache.pdfbox.pdmodel PDDocument]
   [org.apache.pdfbox.rendering PDFRenderer]
   [org.apache.pdfbox.text PDFTextStripper]
   [java.awt.image BufferedImage]
   [javax.imageio ImageIO]
   [java.io ByteArrayOutputStream]
   [java.util Base64]))

(defn pdf-page->image
  "Extract a single page from PDF bytes as BufferedImage.
   Page numbers are 0-indexed. DPI defaults to 150 if not provided."
  ([pdf-bytes page-number] (pdf-page->image pdf-bytes page-number 150))
  ([pdf-bytes page-number dpi]
   (let [doc (Loader/loadPDF pdf-bytes)
         renderer (PDFRenderer. doc)
         image (.renderImageWithDPI renderer page-number (float dpi))]
     (.close doc)
     image)))

(defn get-page-count
  "Return the number of pages in a PDF byte array using PDFBox."
  [pdf-bytes]
  (let [doc (Loader/loadPDF pdf-bytes)]
    (try (.getNumberOfPages doc)
      (finally (.close doc)))))

(defn pdf-page->single-page-pdf-bytes
  "Extract one page (1-based) of `pdf-bytes` as a standalone single-page PDF.
   The Mistral OCR lane consumes a PDF (not an image); this is its page input.
   Pre:  1 <= page-number <= (get-page-count pdf-bytes).
   Post: a byte[] that loads as a 1-page PDF. importPage deep-copies the page's
         resources, so the output carries only that page's fonts/images."
  [pdf-bytes page-number]
  (let [doc (Loader/loadPDF pdf-bytes)]
    (try
      (let [out (PDDocument.)]
        (try
          (.importPage out (.getPage doc (dec page-number)))
          (let [baos (ByteArrayOutputStream.)]
            (.save out baos)
            (.toByteArray baos))
          (finally (.close out))))
      (finally (.close doc)))))

(defn mistral-ocr-text
  "Markdown page text from an OpenRouter file-parser (engine mistral-ocr) response.
   The dedicated OCR output is in the first :file annotation's :content blocks,
   wrapped by <file …> / </file> sentinel text blocks which are dropped.
   Pre:  `response` is a parsed OpenRouter chat-completion body.
   Post: trimmed markdown string, or nil when no :file annotation is present
         (caller MUST treat nil as an extraction failure, not empty text)."
  [response]
  (some->> (-> response :choices first :message :annotations)
    (filter #(= "file" (:type %)))
    first :file :content
    (map :text)
    (remove #(re-matches #"</?file[^>]*>" (str/trim (or % ""))))
    (str/join "")
    str/trim))

(defn image->base64
  "Convert BufferedImage to base64 data URL for OpenAI API."
  [^BufferedImage image]
  (let [baos (ByteArrayOutputStream.)]
    (ImageIO/write image "png" baos)
    (.flush baos)
    (let [bytes (.toByteArray baos)
          b64 (.encodeToString (Base64/getEncoder) bytes)]
      (str "data:image/png;base64," b64))))

(defn extract-text-pdfbox
  "Extract text from a single PDF page using PDFBox's native text layer.
   page-number is 1-based. No network, no API key. Returns empty/short text
   for scanned pages with no text layer."
  [pdf-bytes page-number]
  (try
    (let [doc (Loader/loadPDF pdf-bytes)]
      (try
        (let [stripper (doto (PDFTextStripper.)
                         (.setSortByPosition true)
                         (.setStartPage page-number)
                         (.setEndPage page-number))
              text (.getText stripper doc)]
          {:success true :text (or text "")})
        (finally (.close doc))))
    (catch Exception e
      (tel/error! {:id ::extract-text-pdfbox} e)
      {:success false :error (str "PDFBox extraction failed: " (.getMessage e))})))

(defn allowed-ocr-model-ids
  "OCR-model :ids a user may pick (single source of truth for resolver + UI).
   Credits-mode: config/ocr-model-allowlist when non-empty, else ALL registry ids
   (an unset allow-list means \"no restriction\", not \"openai only\"). Self-host:
   all registry ids.
   Post: a non-empty vector of registry ids (registry is never empty)."
  []
  (let [all (mapv :id models/registry)]
    (if (config/credits-enabled?)
      (let [allow (config/ocr-model-allowlist)]
        (if (seq allow) (vec allow) all))
      all)))

(defn effective-ocr-model
  "Registry entry to use for OCR of `topic-id` by `user-id`.
   Precedence among allowed models: per-document selection → user's global default
   → registry default → first allowed.
   Pre:  user-id; topic-id may be nil (→ no per-doc lookup).
   Post: a non-nil registry entry (design invariant: allowed list is non-empty)."
  [user-id topic-id]
  (let [picked (or (when topic-id (settings/get-ocr-model user-id topic-id))
                   (settings/get-ocr-model-default user-id))
        allowed (allowed-ocr-model-ids)
        id (cond
             (some #{picked} allowed) picked
             (some #{models/default-id} allowed) models/default-id
             :else (first allowed))]
    (models/resolve-model id)))

(def ^:private plugin-downstream-model
  "Cheapest verified downstream model for the file-parser plugin. Its completion
   is discarded — the OCR result is read from the file annotation — so it exists
   only to satisfy the plugin's required chat model."
  "google/gemini-3-flash-preview")

(defn- openrouter-post!
  "POST a chat-completion to OpenRouter. Returns {:status :body}; never throws on
   HTTP status (4xx/5xx come back as data so the caller maps the error)."
  [api-key body]
  (openrouter/post! api-key "/chat/completions" body))

(defn- strip-code-fences
  "Drop a leading ```html / ``` fence and trailing ``` the model may wrap around
   HTML output, then trim."
  [s]
  (-> s
    (str/replace #"^```html\s*\n?" "")
    (str/replace #"^```\s*\n?" "")
    (str/replace #"\n?```\s*$" "")
    str/trim))

(defn- run-ocr
  "OCR one page via OpenRouter per registry `entry`. No credits/DB side effects.
   Pre:  api-key non-nil; 0 <= page-idx < (get-page-count pdf-bytes).
   Post: {:text <sanitized-html> :cost-usd <double-or-nil> :usage <map>}; throws
         ex-info on a non-200 status or an empty/absent OCR result."
  [entry api-key pdf-bytes page-idx dpi prompt]
  (let [resp (case (:shape entry)
               :chat
               (openrouter-post! api-key
                 {:model (:openrouter-model entry)
                  :messages [{:role "user"
                              :content [{:type "text" :text prompt}
                                        {:type "image_url"
                                         :image_url {:url (image->base64 (pdf-page->image pdf-bytes page-idx dpi))}}]}]})
               :plugin
               (openrouter-post! api-key
                 {:model plugin-downstream-model
                  :messages [{:role "user"
                              :content [{:type "text" :text "ok"}
                                        {:type "file"
                                         :file {:filename "page.pdf"
                                                :file_data (str "data:application/pdf;base64,"
                                                             (.encodeToString (Base64/getEncoder)
                                                               (pdf-page->single-page-pdf-bytes pdf-bytes (inc page-idx))))}}]}]
                  :plugins [{:id "file-parser" :pdf {:engine "mistral-ocr"}}]}))
        body (:body resp)]
    (when-not (= 200 (:status resp))
      (throw (ex-info (or (get-in body [:error :message]) (str "OpenRouter HTTP " (:status resp)))
               {:status (:status resp)})))
    (let [raw (case (:shape entry)
                :chat   (some-> (-> body :choices first :message :content) strip-code-fences)
                :plugin (some-> (mistral-ocr-text body) text/markdown->html))]
      (when (str/blank? raw)
        (throw (ex-info "Empty OCR response" {:shape (:shape entry)})))
      ;; Model output is untrusted — sanitize before persistence.
      {:text (cleaner/clean-html raw) :cost-usd (get-in body [:usage :cost]) :usage (:usage body)})))

(defn extract-text-with-model
  "OCR a PDF page with an explicit registry `model-id` (no model resolution).
   Bills from OpenRouter's reported cost; does NOT save. The shared OCR path for
   both the normal scan (model resolved by the caller) and the compare modal
   (each candidate model run explicitly).
   Pre:  model-id is a registry id; 0 <= page-idx < page count; page-idx 0-based.
   Post: {:success true :text html} or {:success false :error msg [:error-type]}."
  [user-id model-id pdf-bytes page-idx dpi]
  (try
    (let [entry (or (models/resolve-model model-id)
                    (throw (ex-info (str "Unknown OCR model: " model-id) {})))
          api-key (settings/get-openrouter-api-key user-id)
          _ (when (empty? api-key)
              (throw (ex-info "OpenRouter API key not configured" {})))
          _ (let [gate (credits/check-cost-billed-balance! user-id)]
              (when-not (:ok gate)
                (throw (ex-info (:error gate) {:type ::insufficient-credits}))))
          prompt (settings/get-ocr-prompt user-id)
          t-start (System/nanoTime)
          {:keys [text cost-usd usage]} (run-ocr entry api-key pdf-bytes page-idx dpi prompt)
          duration-ms (long (/ (- (System/nanoTime) t-start) 1000000))]
      (tel/log! {:level :info :id ::ocr-completion
                 :data {:user-id user-id :model (:id entry) :shape (:shape entry)
                        :openrouter-model (:openrouter-model entry)
                        :cost-usd cost-usd :duration-ms duration-ms
                        :prompt-tokens (:prompt_tokens usage)
                        :completion-tokens (:completion_tokens usage)}}
        "OCR completion")
      ;; Bill the completed action from OpenRouter's reported cost (no-op in
      ;; self-host). Total: a billing failure logs ::credit-charge-failed and
      ;; returns nil, never discarding a successful OCR result.
      (credits/record-cost-charge! user-id :ocr.extract (:id entry) cost-usd)
      {:success true :text text})
    (catch Exception e
      ;; Out-of-credits refusal is a normal business outcome, not a pipeline
      ;; failure — keep it off the :error channel (alert email noise).
      (if (= ::insufficient-credits (:type (ex-data e)))
        (tel/log! {:level :info :id ::spend-refused :data {:user-id user-id}}
          "OCR refused: out of credits")
        (tel/error! {:id ::extract-text} e))
      {:success false
       :error (let [msg (.getMessage e)]
                (cond
                  (re-find #"(?i)API key not configured" (str msg))
                  "OCR is not configured. Set the OpenRouter key in Settings."
                  (re-find #"(?i)401|unauthorized" (str msg))
                  "Invalid OpenRouter API key."
                  (re-find #"(?i)429|rate.?limit" (str msg))
                  "Rate limit reached. Wait a moment and try again."
                  :else (str msg)))
       :error-type (when (= ::insufficient-credits (:type (ex-data e)))
                     :insufficient-credits)})))

(defn extract-text
  "OCR a PDF page through the user's resolved model (per-doc → global → default).
   Returns {:success true :text html} or {:success false :error msg}.
   `page-idx` is 0-based. `enc-key` is reserved for future per-user BYOK and is
   currently unused — all OCR uses the platform OpenRouter key. DPI defaults to 150."
  ([user-id topic-id pdf-bytes page-idx enc-key]
   (extract-text user-id topic-id pdf-bytes page-idx enc-key 150))
  ([user-id topic-id pdf-bytes page-idx _enc-key dpi]
   (extract-text-with-model user-id (:id (effective-ocr-model user-id topic-id))
     pdf-bytes page-idx dpi)))
