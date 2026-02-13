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
   Returns plain text (no HTML formatting for v1)."
  [pdf-bytes page-number]
  (try
    (let [api-key (settings/get-openai-api-key)
          _ (when (empty? api-key)
              (throw (ex-info "OpenAI API key not configured" {})))
          image (pdf-page->image pdf-bytes page-number)
          base64-image (image->base64 image)
          prompt "Extract all text from this image. Return only the text content, preserving the reading order and structure."
          response (api/create-chat-completion
                     {:model "gpt-4o"
                      :messages [{:role "user"
                                  :content [{:type "text" :text prompt}
                                            {:type "image_url"
                                             :image_url {:url base64-image}}]}]}
                     {:api-key api-key})
          text (-> response :choices first :message :content)]
      {:success true :text text})
    (catch Exception e
      (println "ERROR [extract-text]:" (.getMessage e))
      {:success false :error (.getMessage e)})))
