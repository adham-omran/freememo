(ns freememo.epub
  "EPUB parsing: extracts metadata and HTML content from .epub files.
   An EPUB is a ZIP archive containing XHTML chapters, images, and metadata."
  (:require [freememo.html-cleaner :as cleaner]
            [taoensso.telemere :as tel]
            [clojure.string :as str])
  (:import [java.util.zip ZipInputStream ZipEntry]
           [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.awt.image BufferedImage]
           [java.awt RenderingHints]
           [javax.imageio ImageIO IIOImage ImageWriteParam]
           [org.jsoup Jsoup]
           [org.jsoup.nodes Document Element]
           [java.util Base64]))

(defn epub-magic-bytes?
  "True iff bytes look like an EPUB: ZIP local-file-header (PK\\x03\\x04) and
   the first ZIP entry is an uncompressed `mimetype` whose body equals
   `application/epub+zip` (per EPUB spec)."
  [^bytes b]
  (and (>= (alength b) 4)
    (= (byte 0x50) (aget b 0))   ;; P
    (= (byte 0x4B) (aget b 1))   ;; K
    (= (byte 0x03) (aget b 2))
    (= (byte 0x04) (aget b 3))
    (try
      (with-open [zis (ZipInputStream. (ByteArrayInputStream. b))]
        (when-let [entry (.getNextEntry zis)]
          (and (= "mimetype" (.getName entry))
            (let [baos (ByteArrayOutputStream.)
                  buf (byte-array 256)]
              (loop []
                (let [n (.read zis buf)]
                  (when (pos? n)
                    (.write baos buf 0 n)
                    (recur))))
              (= "application/epub+zip"
                (str/trim (String. (.toByteArray baos) "UTF-8")))))))
      (catch Exception _ false))))

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
                buf (byte-array 8192)]
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

;; ── Image handling ──────────────────────────────────────────────

(def ^:private max-thumbnail-width 600)

(defn- mime-for-extension
  "Guess MIME type from file extension."
  [path]
  (let [lower (str/lower-case (or path ""))]
    (cond
      (str/ends-with? lower ".png") "image/png"
      (str/ends-with? lower ".jpg") "image/jpeg"
      (str/ends-with? lower ".jpeg") "image/jpeg"
      (str/ends-with? lower ".gif") "image/gif"
      (str/ends-with? lower ".svg") "image/svg+xml"
      (str/ends-with? lower ".webp") "image/webp"
      :else "application/octet-stream")))

(defn- svg? [mime]
  (= mime "image/svg+xml"))

(defn- resize-image-bytes
  "Resize image bytes to max-width, re-encoding as JPEG. Returns resized bytes or nil."
  [^bytes img-bytes max-width]
  (try
    (let [^BufferedImage src-img (ImageIO/read (ByteArrayInputStream. img-bytes))]
      (when src-img
        (let [src-w (.getWidth src-img)
              src-h (.getHeight src-img)]
          (if (<= src-w max-width)
            ;; Already small enough — re-encode as JPEG to save space
            (let [baos (ByteArrayOutputStream.)]
              (ImageIO/write src-img "jpg" baos)
              (.toByteArray baos))
            ;; Scale down
            (let [scale (/ (double max-width) src-w)
                  dst-w max-width
                  dst-h (max 1 (int (* src-h scale)))
                  dst-img (BufferedImage. dst-w dst-h BufferedImage/TYPE_INT_RGB)
                  g (.createGraphics dst-img)]
              (.setRenderingHint g RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BILINEAR)
              (.drawImage g src-img 0 0 dst-w dst-h nil)
              (.dispose g)
              (let [baos (ByteArrayOutputStream.)]
                (ImageIO/write dst-img "jpg" baos)
                (.toByteArray baos)))))))
    (catch Exception e
      (tel/log! {:level :warn :id ::resize-image} (.getMessage e))
      nil)))

(defn- process-images
  "Process img elements based on image-mode (:reduce or :strip)."
  [^Document html-doc xhtml-path entries image-mode]
  (let [imgs (.select html-doc "img[src]")]
    (doseq [^Element img imgs]
      (if (= image-mode :strip)
        (.remove img)
        ;; :reduce mode
        (let [src (.attr img "src")]
          (if (str/starts-with? src "data:")
            nil ;; already inline, leave it
            (let [img-path (resolve-path xhtml-path src)
                  img-bytes (get entries img-path)]
              (if img-bytes
                (let [mime (mime-for-extension img-path)]
                  (if (svg? mime)
                    ;; SVGs are already small text — inline as-is
                    (let [b64 (.encodeToString (Base64/getEncoder) img-bytes)]
                      (.attr img "src" (str "data:" mime ";base64," b64)))
                    ;; Raster image — resize then inline
                    (if-let [resized (resize-image-bytes img-bytes max-thumbnail-width)]
                      (let [b64 (.encodeToString (Base64/getEncoder) resized)]
                        (.attr img "src" (str "data:image/jpeg;base64," b64)))
                      (.remove img))))
                ;; Image not found
                (.remove img)))))))))


;; ── Main processing ─────────────────────────────────────────────

(defn- extract-chapter-title
  "Extract a title from the first heading in an XHTML body, or use the filename."
  [^Element body href]
  (let [heading (some #(.selectFirst body %) ["h1" "h2" "h3"])]
    (if heading
      (let [text (str/trim (.text heading))]
        (if (str/blank? text)
          (last (str/split (or href "chapter") #"/"))
          text))
      (-> (or href "chapter")
        (str/split #"/")
        last
        (str/replace #"\.(xhtml|html|htm)$" "")))))

(defn process-epub
  "Extract text and metadata from an EPUB file.
   image-mode is :reduce (resize to thumbnails, default) or :strip (remove all images).
   Returns {:title :author :chapters [{:html :title} ...]} on success,
   or {:error \"...\"} on failure."
  [file-bytes image-mode]
  (try
    (let [entries (read-zip-entries file-bytes)
          image-mode (or image-mode :reduce)]
      ;; Check for DRM
      (if (drm-protected? entries)
        {:error "This EPUB is DRM-protected and cannot be imported. Please use a DRM-free version."}

        ;; Find and parse OPF
        (if-let [opf-path (find-opf-path entries)]
          (if-let [opf-bytes (get entries opf-path)]
            (let [{:keys [title creator manifest spine-ids]} (parse-opf opf-bytes opf-path)
                  ;; Process each spine item into a chapter
                  chapters
                  (reduce
                    (fn [chs spine-id]
                      (if-let [{:keys [href media-type]} (get manifest spine-id)]
                        (let [xhtml-path (resolve-path opf-path href)
                              xhtml-bytes (get entries xhtml-path)]
                          (if (and xhtml-bytes
                                (or (str/includes? (or media-type "") "xhtml")
                                  (str/includes? (or media-type "") "html")))
                            (let [doc (Jsoup/parse (String. xhtml-bytes "UTF-8"))
                                  _ (process-images doc xhtml-path entries image-mode)
                                  body (.selectFirst doc "body")]
                              (if body
                                (let [body-html (.html body)
                                      cleaned (cleaner/clean-html body-html)]
                                  (if (str/blank? (str/trim (or cleaned "")))
                                    chs ;; skip empty chapters
                                    (conj chs {:title (extract-chapter-title body href)
                                               :html cleaned})))
                                chs))
                            chs))
                        chs))
                    []
                    spine-ids)]
              (if (seq chapters)
                {:title (or title "Untitled EPUB")
                 :author creator
                 :chapters chapters}
                {:error "No readable content found in this EPUB"}))

            {:error "Could not read package document (OPF file)"})
          {:error "Invalid EPUB: no container.xml or missing rootfile"})))
    (catch Exception e
      (tel/error! {:id ::process-epub} e)
      {:error (str "Failed to parse EPUB: " (.getMessage e))})))
