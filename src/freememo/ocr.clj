(ns freememo.ocr
  "OCR text extraction using OpenAI Vision API."
  (:require
   [freememo.settings :as settings]
   [freememo.html-cleaner :as cleaner]
   [wkok.openai-clojure.api :as api]
   [taoensso.telemere :as tel])
  (:import
   [org.apache.pdfbox Loader]
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

(defn extract-text
  "Extract text from a PDF page using OpenAI Vision API.
   Returns HTML with semantic formatting (h1-h6, p, ul, ol, strong, em).
   DPI defaults to 150 if not provided."
  ([user-id pdf-bytes page-number enc-key]
   (extract-text user-id pdf-bytes page-number enc-key 150))
  ([user-id pdf-bytes page-number enc-key dpi]
   (try
     (let [api-key (settings/get-openai-api-key user-id enc-key)
           _ (when (empty? api-key)
               (throw (ex-info "OpenAI API key not configured" {})))
           image (pdf-page->image pdf-bytes page-number dpi)
           base64-image (image->base64 image)
           prompt (settings/get-ocr-prompt user-id)
           t-start (System/nanoTime)
           response (api/create-chat-completion
                      {:model "gpt-5.1"
                       :messages [{:role "user"
                                   :content [{:type "text" :text prompt}
                                             {:type "image_url"
                                              :image_url {:url base64-image}}]}]}
                      {:api-key api-key})
           duration-ms (long (/ (- (System/nanoTime) t-start) 1000000))
           usage (:usage response)
           _ (tel/log! {:level :info :id ::openai-completion
                        :data {:user-id user-id
                               :model "gpt-5.1"
                               :endpoint :ocr.extract
                               :prompt-tokens (:prompt_tokens usage)
                               :completion-tokens (:completion_tokens usage)
                               :cached-tokens (get-in usage [:prompt_tokens_details :cached_tokens])
                               :reasoning-tokens (get-in usage [:completion_tokens_details :reasoning_tokens])
                               :duration-ms duration-ms
                               :attempt 1}}
               "OpenAI completion")
           raw-text (-> response :choices first :message :content)
           _ (when-not raw-text
               (throw (ex-info "Empty response from OpenAI"
                        {:status (get response :status)
                         :error (get-in response [:error :message])})))
           ;; Strip markdown code fences if the model adds them anyway
           text (-> raw-text
                  (clojure.string/replace #"^```html\s*\n?" "")
                  (clojure.string/replace #"^```\s*\n?" "")
                  (clojure.string/replace #"\n?```\s*$" "")
                  clojure.string/trim)
           ;; Vision output is untrusted — sanitize before persistence.
           cleaned (cleaner/clean-html text)]
       {:success true :text cleaned})
     (catch Exception e
       (tel/error! {:id ::extract-text} e)
       {:success false :error (let [msg (.getMessage e)]
                                (cond
                                  (re-find #"(?i)API key not configured" (str msg))
                                  "No API key configured. Set one in Settings."
                                  (re-find #"(?i)401|unauthorized" (str msg))
                                  "Invalid API key. Check your key in Settings."
                                  (re-find #"(?i)429|rate.?limit" (str msg))
                                  "Rate limit reached. Wait a moment and try again."
                                  :else (str msg)))}))))
