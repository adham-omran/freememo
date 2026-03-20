(ns electric-starter-app.epub
  "EPUB parsing: extracts metadata and HTML content from .epub files.
   An EPUB is a ZIP archive containing XHTML chapters, images, and metadata."
  (:require [electric-starter-app.html-cleaner :as cleaner]
            [clojure.string :as str])
  (:import [java.util.zip ZipInputStream ZipEntry]
           [java.io ByteArrayInputStream ByteArrayOutputStream]
           [org.jsoup Jsoup]
           [org.jsoup.nodes Document Element]
           [java.util Base64]))

;; ── ZIP reading ──────────────────────────────────────────────────

(defn- read-zip-entries
  "Read all entries from a ZIP byte array into a map of {path -> byte-array}."
  [file-bytes]
  (with-open [zis (ZipInputStream. (ByteArrayInputStream. file-bytes))]
    (loop [entries {}]
      (if-let [^ZipEntry entry (.getNextEntry zis)]
        (if (.isDirectory entry)
          (recur entries)
          (let [baos (ByteArrayOutputStream.)
                buf  (byte-array 8192)]
            (loop []
              (let [n (.read zis buf)]
                (when (pos? n)
                  (.write baos buf 0 n)
                  (recur))))
            (recur (assoc entries (.getName entry) (.toByteArray baos)))))
        entries))))

;; ── XML parsing helpers ──────────────────────────────────────────

(defn- parse-xml
  "Parse XML bytes into a Jsoup Document in XML mode."
  [^bytes xml-bytes]
  (Jsoup/parse (String. xml-bytes "UTF-8") "" (org.jsoup.parser.Parser/xmlParser)))

(defn- parse-xml-str
  "Parse XML string into a Jsoup Document in XML mode."
  [^String xml-str]
  (Jsoup/parse xml-str "" (org.jsoup.parser.Parser/xmlParser)))

;; ── OPF path resolution ─────────────────────────────────────────

(defn- find-opf-path
  "Extract the OPF package document path from META-INF/container.xml."
  [entries]
  (when-let [container-bytes (get entries "META-INF/container.xml")]
    (let [doc (parse-xml container-bytes)
          rootfile (.selectFirst doc "rootfile[full-path]")]
      (when rootfile
        (.attr rootfile "full-path")))))

;; ── Path resolution ─────────────────────────────────────────────

(defn- resolve-path
  "Resolve a relative href against a base directory path.
   E.g., (resolve-path \"OEBPS/content.opf\" \"chapter1.xhtml\") => \"OEBPS/chapter1.xhtml\"
         (resolve-path \"OEBPS/content.opf\" \"../images/cover.png\") => \"images/cover.png\""
  [base-file rel-href]
  (let [base-dir (if (str/includes? base-file "/")
                   (str/join "/" (butlast (str/split base-file #"/")))
                   "")
        ;; Decode percent-encoded characters in href
        decoded-href (try (java.net.URLDecoder/decode rel-href "UTF-8")
                          (catch Exception _ rel-href))
        ;; Strip fragment identifiers
        clean-href (first (str/split decoded-href #"#"))
        full-path (if (str/blank? base-dir)
                    clean-href
                    (str base-dir "/" clean-href))
        ;; Normalize .. segments
        parts (str/split full-path #"/")
        normalized (reduce (fn [acc part]
                             (cond
                               (= part "..") (if (seq acc) (pop acc) acc)
                               (= part ".") acc
                               :else (conj acc part)))
                           []
                           parts)]
    (str/join "/" normalized)))

;; ── DRM detection ───────────────────────────────────────────────

(defn- drm-protected?
  "Check if the EPUB has DRM encryption (not just font obfuscation)."
  [entries]
  (when-let [enc-bytes (get entries "META-INF/encryption.xml")]
    (let [doc (parse-xml enc-bytes)
          encrypted-data (.select doc "EncryptedData")]
      ;; Font obfuscation uses Algorithm="http://www.idpf.org/2008/embedding"
      ;; Real DRM uses other algorithms
      (some (fn [^Element ed]
              (let [method (.selectFirst ed "EncryptionMethod")
                    algo (when method (.attr method "Algorithm"))]
                (and algo
                     (not= algo "http://www.idpf.org/2008/embedding")
                     ;; Also check if it encrypts non-font content
                     (let [cipher-ref (.selectFirst ed "CipherReference")
                           uri (when cipher-ref (.attr cipher-ref "URI"))]
                       (when uri
                         (not (or (str/ends-with? (str/lower-case uri) ".ttf")
                                  (str/ends-with? (str/lower-case uri) ".otf")
                                  (str/ends-with? (str/lower-case uri) ".woff")
                                  (str/ends-with? (str/lower-case uri) ".woff2"))))))))
            encrypted-data))))

;; ── OPF parsing ─────────────────────────────────────────────────

(defn- parse-opf
  "Parse the OPF package document. Returns metadata, manifest, and spine."
  [opf-bytes opf-path]
  (let [doc (parse-xml opf-bytes)
        ;; Metadata
        title (some-> (.selectFirst doc "metadata dc\\:title, metadata title") .text str/trim)
        creator (some-> (.selectFirst doc "metadata dc\\:creator, metadata creator") .text str/trim)
        ;; Manifest: id -> {:href :media-type}
        manifest-els (.select doc "manifest item")
        manifest (into {}
                   (map (fn [^Element item]
                          [(.attr item "id")
                           {:href (.attr item "href")
                            :media-type (.attr item "media-type")}]))
                   manifest-els)
        ;; Spine: ordered list of manifest IDs
        spine-els (.select doc "spine itemref")
        spine-ids (mapv (fn [^Element itemref] (.attr itemref "idref")) spine-els)]
    {:title title
     :creator creator
     :manifest manifest
     :spine-ids spine-ids
     :opf-path opf-path}))

;; ── Image inlining ──────────────────────────────────────────────

(defn- mime-for-extension
  "Guess MIME type from file extension."
  [path]
  (let [lower (str/lower-case (or path ""))]
    (cond
      (str/ends-with? lower ".png")  "image/png"
      (str/ends-with? lower ".jpg")  "image/jpeg"
      (str/ends-with? lower ".jpeg") "image/jpeg"
      (str/ends-with? lower ".gif")  "image/gif"
      (str/ends-with? lower ".svg")  "image/svg+xml"
      (str/ends-with? lower ".webp") "image/webp"
      :else "application/octet-stream")))

(defn- inline-images
  "Replace img src attributes with base64 data URIs using ZIP entries."
  [^Document html-doc xhtml-path entries manifest opf-path]
  (let [imgs (.select html-doc "img[src]")]
    (doseq [^Element img imgs]
      (let [src (.attr img "src")]
        (when-not (str/starts-with? src "data:")
          ;; Resolve image path relative to the XHTML file
          (let [img-path (resolve-path xhtml-path src)
                img-bytes (get entries img-path)]
            (if img-bytes
              (let [mime (mime-for-extension img-path)
                    b64 (.encodeToString (Base64/getEncoder) img-bytes)
                    data-uri (str "data:" mime ";base64," b64)]
                (.attr img "src" data-uri))
              ;; Image not found — remove the broken img
              (.remove img))))))))

;; ── Main processing ─────────────────────────────────────────────

(defn process-epub
  "Extract text and metadata from an EPUB file.
   Returns {:title :author :html} on success, or {:error \"...\"} on failure."
  [file-bytes]
  (try
    (let [entries (read-zip-entries file-bytes)]
      ;; Check for DRM
      (if (drm-protected? entries)
        {:error "This EPUB is DRM-protected and cannot be imported. Please use a DRM-free version."}

        ;; Find and parse OPF
        (if-let [opf-path (find-opf-path entries)]
          (if-let [opf-bytes (get entries opf-path)]
            (let [{:keys [title creator manifest spine-ids]} (parse-opf opf-bytes opf-path)
                  ;; Process each spine item
                  html-parts
                  (reduce
                    (fn [parts spine-id]
                      (if-let [{:keys [href media-type]} (get manifest spine-id)]
                        (let [xhtml-path (resolve-path opf-path href)
                              xhtml-bytes (get entries xhtml-path)]
                          (if (and xhtml-bytes
                                   (or (str/includes? (or media-type "") "xhtml")
                                       (str/includes? (or media-type "") "html")))
                            (let [doc (Jsoup/parse (String. xhtml-bytes "UTF-8"))
                                  _ (inline-images doc xhtml-path entries manifest opf-path)
                                  body (.selectFirst doc "body")]
                              (if body
                                (conj parts (.html body))
                                parts))
                            ;; Skip non-HTML spine items (e.g., images)
                            parts))
                        parts))
                    []
                    spine-ids)
                  ;; Concatenate and clean
                  combined-html (str/join "\n<hr>\n" html-parts)
                  cleaned-html (cleaner/clean-html combined-html)]
              {:title (or title "Untitled EPUB")
               :author creator
               :html cleaned-html})

            {:error "Could not read package document (OPF file)"})
          {:error "Invalid EPUB: no container.xml or missing rootfile"})))
    (catch Exception e
      (println "ERROR [epub/process-epub]:" (.getMessage e))
      {:error (str "Failed to parse EPUB: " (.getMessage e))})))
