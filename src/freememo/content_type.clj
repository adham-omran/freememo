(ns freememo.content-type
  "Content-Type classification, filename derivation, and charset extraction
   for the import pipeline. Pure CLJ; server-only."
  (:require [clojure.string :as str]))

(defn- strip-params
  "Strip MIME params after `;`; case-insensitive lowercase return."
  [content-type]
  (when content-type
    (str/lower-case (str/trim (first (str/split content-type #";"))))))

(defn classify-http
  "Pre:  header-value is a string or nil (Content-Type header from an HTTP response).
   Post: returns [:html nil] | [:pdf nil] | [:epub nil] | [:reject <msg>].
   Invariant: prefix-tolerant, case-insensitive, params after `;` ignored;
              missing header is treated as HTML (lenient)."
  [header-value]
  (let [base (strip-params header-value)]
    (cond
      (nil? base)
      [:html nil]

      (or (= base "text/html") (= base "application/xhtml+xml"))
      [:html nil]

      (= base "application/pdf")
      [:pdf nil]

      (= base "application/epub+zip")
      [:epub nil]

      (str/starts-with? base "image/")
      [:reject "This URL points to an image, which can't be imported as an article."]

      (str/starts-with? base "audio/")
      [:reject "This URL points to an audio file, which can't be imported as an article."]

      (str/starts-with? base "video/")
      [:reject "This URL points to a video, which can't be imported as an article."]

      (= base "application/json")
      [:reject "This URL returns JSON data, not an article."]

      (or (= base "application/xml") (= base "text/xml"))
      [:reject "This URL returns XML data, not an article."]

      (= base "text/plain")
      [:reject "This URL is plain text. Try the Paste tab to import as Markdown."]

      (= base "application/zip")
      [:reject "This URL is a ZIP archive."]

      (= base "application/octet-stream")
      [:reject "This URL returns an unknown binary file."]

      :else
      [:reject (str "This URL returns " base " content, which can't be imported.")])))

(defn extract-charset
  "Pre:  header-value is a string or nil.
   Post: returns a non-nil charset name string; default UTF-8."
  [header-value]
  (or (when header-value
        (some-> (re-find #"(?i)charset=([^;\s]+)" header-value) second))
      "UTF-8"))

(defn parse-content-disposition-filename
  "Pre:  header is a string or nil (Content-Disposition value).
   Post: returns the filename string or nil.
   Handles `filename=\"...\"` quoted and `filename=...` unquoted forms.
   RFC 5987 `filename*=` form not handled (out of scope per spec)."
  [header]
  (when header
    (or (some-> (re-find #"(?i)filename=\"([^\"]+)\"" header) second)
        (some-> (re-find #"(?i)filename=([^;\s]+)" header) second))))

(defn- url-path-tail [url]
  (when (string? url)
    (let [path (-> url
                   (str/split #"\?" 2) first
                   (str/split #"#" 2) first
                   (str/split #"/")
                   last)]
      (when-not (str/blank? path) path))))

(defn derive-filename
  "Pick a filename for a fetched URL.
   Pre:  flow is :pdf|:epub|:html|:markdown; url is a string;
         content-disposition is a string or nil.
   Post: returns a non-blank string; precedence Content-Disposition →
         URL path tail → \"Untitled.<ext>\" per flow.
   Invariant: pure; never throws."
  [content-disposition url flow]
  (let [from-cd (parse-content-disposition-filename content-disposition)
        from-url (url-path-tail url)
        ext (case flow :pdf ".pdf" :epub ".epub" :html ".html" :markdown ".md" "")
        fallback (str "Untitled" ext)]
    (or from-cd from-url fallback)))

(defn- pdf-magic? [^bytes b]
  (and b (>= (alength b) 5)
       (= (byte 0x25) (aget b 0))
       (= (byte 0x50) (aget b 1))
       (= (byte 0x44) (aget b 2))
       (= (byte 0x46) (aget b 3))
       (= (byte 0x2D) (aget b 4))))

(defn- zip-magic? [^bytes b]
  (and b (>= (alength b) 4)
       (= (byte 0x50) (aget b 0))
       (= (byte 0x4B) (aget b 1))
       (= (byte 0x03) (aget b 2))
       (= (byte 0x04) (aget b 3))))

(defn classify-multipart
  "Pre:  filename and content-type are strings (possibly nil); bytes is a byte
         array of the multipart-uploaded file.
   Post: returns [:pdf nil] | [:epub nil] | [:html nil] | [:markdown nil] |
         [:audio nil] | [:reject <msg>]. Magic bytes win over extension wins
         over content-type. EPUB magic check is structural (ZIP header);
         EPUB-specific validation is deferred to the handler via
         `epub/epub-magic-bytes?`. Audio has no reliable cross-container magic,
         so it resolves by extension/content-type only."
  [filename content-type ^bytes bytes]
  (let [lower-name (when filename (str/lower-case filename))
        ext (cond
              (nil? lower-name) nil
              (str/ends-with? lower-name ".pdf") :pdf
              (str/ends-with? lower-name ".epub") :epub
              (str/ends-with? lower-name ".html") :html
              (str/ends-with? lower-name ".htm") :html
              (str/ends-with? lower-name ".md") :markdown
              (str/ends-with? lower-name ".markdown") :markdown
              (some #(str/ends-with? lower-name %)
                    [".mp3" ".m4a" ".mp4" ".wav" ".webm" ".ogg" ".oga" ".flac" ".mpeg" ".mpga"]) :audio
              :else nil)
        ct (strip-params content-type)
        ct-kind (cond
                  (= ct "application/pdf") :pdf
                  (= ct "application/epub+zip") :epub
                  (or (= ct "text/html") (= ct "application/xhtml+xml")) :html
                  (or (= ct "text/markdown") (= ct "text/x-markdown")) :markdown
                  (and ct (str/starts-with? ct "audio/")) :audio
                  :else nil)
        magic-kind (cond
                     (pdf-magic? bytes) :pdf
                     (zip-magic? bytes) :epub
                     :else nil)
        kind (or magic-kind ext ct-kind)]
    (cond
      (= kind :pdf)
      (if (pdf-magic? bytes) [:pdf nil] [:reject "Not a valid PDF file."])

      (= kind :epub)
      [:epub nil]

      (= kind :html)
      [:html nil]

      (= kind :markdown)
      [:markdown nil]

      (= kind :audio)
      [:audio nil]

      :else
      [:reject "Unsupported file type. Supported: PDF, EPUB, HTML, Markdown, audio."])))
