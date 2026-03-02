(ns electric-starter-app.ocr
  "OCR text extraction using OpenAI Vision API."
  (:require
    [electric-starter-app.settings :as settings]
    [wkok.openai-clojure.api :as api])
  (:import
    [org.apache.pdfbox Loader]
    [org.apache.pdfbox.rendering PDFRenderer]
    [java.awt.image BufferedImage]
    [javax.imageio ImageIO]
    [java.io ByteArrayOutputStream]
    [java.util Base64]))

(defn pdf-page->image
  "Extract a single page from PDF bytes as BufferedImage.
   Page numbers are 0-indexed."
  [pdf-bytes page-number]
  (let [doc (Loader/loadPDF pdf-bytes)
        renderer (PDFRenderer. doc)
        image (.renderImageWithDPI renderer page-number (float 150))]
    (.close doc)
    image))

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

(defn extract-text
  "Extract text from a PDF page using OpenAI Vision API.
   Returns HTML with semantic formatting (h1-h6, p, ul, ol, strong, em)."
  [user-id pdf-bytes page-number enc-key]
  (try
    (let [api-key (settings/get-openai-api-key user-id enc-key)
          _ (when (empty? api-key)
              (throw (ex-info "OpenAI API key not configured" {})))
          image (pdf-page->image pdf-bytes page-number)
          base64-image (image->base64 image)
          prompt "Extract all text from this image and return it as clean, semantic HTML. Use:
- <h1>, <h2>, <h3> for headings
- <p> for paragraphs
- <ul><li> and <ol><li> for lists
- <strong> for bold/important text
- <em> for italic/emphasized text
- <br> for line breaks within paragraphs
Preserve the reading order and document structure. Return only the HTML body content (no <html> or <body> tags).
IMPORTANT: Do NOT wrap the HTML in markdown code fences (```html or ```). Return raw HTML only."
          response (api/create-chat-completion
                     {:model "gpt-4o"
                      :messages [{:role "user"
                                  :content [{:type "text" :text prompt}
                                            {:type "image_url"
                                             :image_url {:url base64-image}}]}]}
                     {:api-key api-key})
          raw-text (-> response :choices first :message :content)
          _ (when-not raw-text
              (throw (ex-info "Empty response from OpenAI"
                              {:status (get response :status)
                               :error  (get-in response [:error :message])})))
          ;; Strip markdown code fences if GPT-4o adds them anyway
          text (-> raw-text
                   (clojure.string/replace #"^```html\s*\n?" "")
                   (clojure.string/replace #"^```\s*\n?" "")
                   (clojure.string/replace #"\n?```\s*$" "")
                   clojure.string/trim)]
      {:success true :text text})
    (catch Exception e
      (println "ERROR [extract-text]:" (.getMessage e))
      {:success false :error (.getMessage e)})))
