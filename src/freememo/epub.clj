(ns freememo.epub
  "EPUB parsing: extracts metadata and HTML content from .epub files.
   An EPUB is a ZIP archive containing XHTML chapters, images, and metadata.

   **Source is a byte[] OR a File.** Every public fn takes either, and
   `with-epub-entries` is the single place that branch lives. The File arity
   exists because a chunked upload lands books that cannot be represented as a
   byte[] at all — a JVM array is int-indexed, so 2 GiB is the hard ceiling, and
   the prod heap puts the practical limit far below that. Same rule, and the same
   reasoning, as `freememo.archive`.

   **Nothing proportional to the source is ever in heap.** The reader is
   random-access (commons-compress `ZipFile` over a File or a seekable channel),
   so one entry at a time crosses into memory. An earlier version read every
   entry into a `{path -> byte[]}` map before doing anything, because
   `ZipInputStream` is sequential-only and `process-epub` looks entries up by
   path. The map was a consequence of the reader, not of the data.

   See plans/large-epub-import.md."
  (:require [freememo.html-cleaner :as cleaner]
            [freememo.csl-util :as csl]
            [taoensso.telemere :as tel]
            [clojure.string :as str])
  (:import [java.util.zip ZipInputStream]
           [java.io ByteArrayInputStream ByteArrayOutputStream File InputStream]
           [java.awt.image BufferedImage]
           [java.awt RenderingHints]
           [javax.imageio ImageIO ImageReader ImageReadParam]
           [org.apache.commons.compress.archivers.zip ZipArchiveEntry ZipFile]
           [org.apache.commons.compress.utils SeekableInMemoryByteChannel]
           [org.jsoup Jsoup]
           [org.jsoup.nodes Document Element]
           [java.util Base64]))

(def max-epub-bytes
  "Hard ceiling on one declared EPUB upload, independent of the user's quota.

   Default 4 GB. A bound on how much a single bad request can reserve, not a
   product limit — the quota is what gates normal use. Same role, and the same
   reasoning, as `video/max-video-bytes` and `archive/max-archive-bytes`.

   Lives here rather than in `epub-http`, for the reason `video/upload-limits`
   records: the import UI needs this number, and a UI namespace requiring an
   HTTP-handler namespace is backwards."
  (or (some-> (System/getenv "EPUB_MAX_BYTES") parse-long)
    (* 4 1024 1024 1024)))

(defn- epub-magic-stream?
  "True iff the stream's first ZIP entry is an uncompressed `mimetype` whose body
   equals `application/epub+zip` (per EPUB spec).

   Pre : `in` is positioned at the start of a ZIP. Post: reads only the first
         entry — never the whole archive, which is what makes this safe to run
         against a multi-gigabyte upload."
  [^InputStream in]
  (try
    (with-open [zis (ZipInputStream. in)]
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
    (catch Exception _ false)))

(defn epub-magic-bytes?
  "True iff `src` looks like an EPUB: ZIP local-file-header (PK\\x03\\x04) and a
   first entry per `epub-magic-stream?`.

   Pre : `src` is a byte[] or a File.
   Post: a boolean. Never throws.
   Invariant: reads a BOUNDED PREFIX, whatever the source's size. The File arity
         exists so a chunk-uploaded book can be validated at finalize without
         first pulling gigabytes into an array."
  [src]
  (if (instance? File src)
    (let [^File f src]
      (and (>= (.length f) 4)
        (with-open [in (java.io.FileInputStream. f)]
          (let [head (byte-array 4)]
            (and (= 4 (.read in head))
              (= (byte 0x50) (aget head 0))   ;; P
              (= (byte 0x4B) (aget head 1))   ;; K
              (= (byte 0x03) (aget head 2))
              (= (byte 0x04) (aget head 3)))))
        (with-open [in (java.io.FileInputStream. f)]
          (epub-magic-stream? in))))
    (let [^bytes b src]
      (and (>= (alength b) 4)
        (= (byte 0x50) (aget b 0))   ;; P
        (= (byte 0x4B) (aget b 1))   ;; K
        (= (byte 0x03) (aget b 2))
        (= (byte 0x04) (aget b 3))
        (epub-magic-stream? (ByteArrayInputStream. b))))))

;; ── ZIP reading ──────────────────────────────────────────────────

(def default-limits
  "Decompression-bomb caps. A .epub only needs to pass `epub-magic-bytes?` to
   reach the reader, so a tiny upload must not be able to inflate without bound.

   `:max-inflation-ratio` is the bomb guard, and it is a ratio rather than a
   fixed byte count on purpose. A bomb's defining property is that it expands far
   beyond its compressed size; a large book's does not. A fixed cap cannot tell
   the two apart — the previous 300 MB total refused every book over about
   300 MB while still permitting a 1 MB file to reach it. `:max-total-bytes` is
   the absolute backstop for a source so large that even a modest ratio is too
   much.

   **Only entries the parse actually READS count against the total.** The reader
   is lazy, so bytes hidden in entries nobody asks for are never inflated and
   cannot hurt us. This is a different guarantee from the eager reader's, and a
   stronger one: the old total bounded what the archive CONTAINED, this bounds
   what we SPEND.

   Per-flow overrides belong at the call site, as `freememo.archive`'s do."
  {:max-entries 50000
   :max-entry-bytes (* 64 1024 1024)
   :max-inflation-ratio 10
   :max-total-bytes (* 8 1024 1024 1024)})

(def ^:private copy-buffer-bytes
  "Transfer granularity when draining one entry. Matches `archive/copy-buffer-bytes`
   so an extraction and an EPUB read move bytes in the same size steps."
  262144)

(defn- source-length
  "Pre : `src` is a File or a byte[]. Post: its length in bytes.
   Blame: caller bug on any other type — the message names the contract."
  ^long [src]
  (cond
    (instance? File src) (.length ^File src)
    (bytes? src) (alength ^bytes src)
    :else (throw (ex-info "EPUB source must be a File or a byte[]"
                   {:type ::unsupported-source :got (type src)}))))

(defn- open-zip
  "The one place the File/byte[] branch lives.
   Pre : `src` is a File or a byte[] holding a ZIP.
   Post: an open random-access ZipFile. The caller MUST close it."
  ^ZipFile [src]
  (let [b (ZipFile/builder)]
    (if (instance? File src)
      (.setFile b ^File src)
      (.setSeekableByteChannel b (SeekableInMemoryByteChannel. ^bytes src)))
    (.get b)))

(defn with-epub-entries
  "Open `src` as a random-access ZIP and call `f` with a read-entry fn.

   Pre : `src` is a File or a byte[] holding a ZIP. `limits` is a
         `default-limits`-shaped map.
   Post: returns `f`'s value. The archive is closed before returning, on every
         path including a throw.
   Invariant: heap high-water is one entry, bounded by `:max-entry-bytes`.
         Nothing proportional to `src` is materialized.
   Throws ex-info {:type ::zip-too-large} on any cap breach. Callers
         (`web-import/confirm-staged-upload!*`) already catch it and return an
         import-failed response.

   `read-entry` is (fn [path] -> byte[] or nil). Blame: a caller that retains it
   past `f`'s return gets a closed-archive error, and that is a caller bug."
  [src limits f]
  (let [{:keys [max-entries max-entry-bytes max-inflation-ratio max-total-bytes]} limits
        total-cap (min (long max-total-bytes)
                    (* (long max-inflation-ratio) (source-length src)))
        !inflated (atom 0)]
    (with-open [zf (open-zip src)]
      (let [n (count (enumeration-seq (.getEntries zf)))]
        (when (> n (long max-entries))
          (throw (ex-info "EPUB has too many entries"
                   {:type ::zip-too-large :entries n :limit max-entries}))))
      (f (fn read-entry [path]
           (when-let [^ZipArchiveEntry e (.getEntry zf path)]
             (with-open [^InputStream in (.getInputStream zf e)]
               (let [out (ByteArrayOutputStream.)
                     buf (byte-array copy-buffer-bytes)]
                 (loop [w 0]
                   (let [r (.read in buf)]
                     (if (pos? r)
                       (let [w2 (+ w r)]
                         (when (> w2 (long max-entry-bytes))
                           (throw (ex-info "EPUB entry too large"
                                    {:type ::zip-too-large :entry path
                                     :limit max-entry-bytes})))
                         (when (> (+ ^long @!inflated w2) total-cap)
                           (throw (ex-info "EPUB inflates too far past its compressed size"
                                    {:type ::zip-too-large :entry path
                                     :limit total-cap})))
                         (.write out buf 0 r)
                         (recur w2))
                       (do (swap! !inflated + w)
                           (.toByteArray out)))))))))))))

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
  "Extract the OPF package document path from META-INF/container.xml.
   Pre: `read-entry` is a `with-epub-entries` reader, still open."
  [read-entry]
  (when-let [container-bytes (read-entry "META-INF/container.xml")]
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
  "Check if the EPUB has DRM encryption (not just font obfuscation).
   Pre: `read-entry` is a `with-epub-entries` reader, still open."
  [read-entry]
  (when-let [enc-bytes (read-entry "META-INF/encryption.xml")]
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

(def ^:private max-inline-svg-bytes
  "An SVG is inlined verbatim — it is text, so there is nothing to resample.
   That makes it the one image path whose output tracks its input, and a book
   carrying a multi-megabyte SVG would push it whole into chapter HTML. Above
   this the image is dropped."
  (* 512 1024))

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

(defn- decode-subsampled
  "Decode `img-bytes` at the coarsest resolution still at or above `max-width`.

   Pre : `img-bytes` holds an image. Post: a BufferedImage at least `max-width`
         across when the source allows, or nil when no reader handles the format.
   Invariant: heap for the decode tracks the SUBSAMPLED pixel count, not the
         source's. `ImageIO/read` allocates width × height × 4 bytes regardless
         of what the caller intends to do next, so a 100-megapixel cover cost
         ~400 MB to produce a 600px thumbnail. Subsampling reads every Nth pixel
         from the stream instead, so that cost never exists.

   The result is at or above `max-width` rather than exactly it, because the
   reader's step is an integer. `resize-image-bytes` does the exact scale."
  ^BufferedImage [^bytes img-bytes ^long max-width]
  (with-open [iis (ImageIO/createImageInputStream (ByteArrayInputStream. img-bytes))]
    (let [readers (ImageIO/getImageReaders iis)]
      (when (.hasNext readers)
        (let [^ImageReader rdr (.next readers)]
          (try
            (.setInput rdr iis true true)
            (let [w (.getWidth rdr 0)
                  step (max 1 (long (Math/floor (/ (double w) (double max-width)))))
                  ^ImageReadParam p (.getDefaultReadParam rdr)]
              (when (> step 1)
                (.setSourceSubsampling p (int step) (int step) 0 0))
              (.read rdr 0 p))
            (finally (.dispose rdr))))))))

(defn- resize-image-bytes
  "Resize image bytes to max-width, re-encoding as JPEG. Returns resized bytes or nil."
  [^bytes img-bytes max-width]
  (try
    (let [^BufferedImage src-img (decode-subsampled img-bytes max-width)]
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
  "Process img elements based on image-mode (:reduce or :strip).
   Pre: `read-entry` is a `with-epub-entries` reader, still open."
  [^Document html-doc xhtml-path read-entry image-mode]
  (let [imgs (.select html-doc "img[src]")]
    (doseq [^Element img imgs]
      (if (= image-mode :strip)
        (.remove img)
        ;; :reduce mode
        (let [src (.attr img "src")]
          (if (str/starts-with? src "data:")
            nil ;; already inline, leave it
            (let [img-path (resolve-path xhtml-path src)
                  ^bytes img-bytes (read-entry img-path)]
              (if img-bytes
                (let [mime (mime-for-extension img-path)]
                  (if (svg? mime)
                    ;; SVG is text, so there is nothing to resample — inline it
                    ;; as-is, or drop it when it is too big to carry whole.
                    (if (> (alength img-bytes) max-inline-svg-bytes)
                      (.remove img)
                      (let [b64 (.encodeToString (Base64/getEncoder) img-bytes)]
                        (.attr img "src" (str "data:" mime ";base64," b64))))
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

;; `opf->biblio` is defined with the rest of the OPF biblio code below.
;; `process-epub` calls it so one import parses the package document ONCE — see
;; the `:biblio` key on its result.
(declare opf->biblio)

(defn process-epub
  "Extract text and metadata from an EPUB.

   Pre : `src` is a File or a byte[] holding an EPUB. `image-mode` is :reduce
         (resize to thumbnails, the default) or :strip (remove all images).
   Post: {:title :author :chapters [{:html :title} ...] :biblio {...}} on
         success, or {:error \"...\"} on failure. Never throws — a cap breach or
         a malformed archive comes back as :error.
   Invariant: the package document is parsed once, and `:biblio` carries what
         `extract-biblio` would have derived from a second read. `prepare-biblio!`
         takes it from here rather than re-reading the stored file."
  ([src image-mode] (process-epub src image-mode default-limits))
  ([src image-mode limits]
   (try
     (with-epub-entries src limits
       (fn [read-entry]
         (let [image-mode (or image-mode :reduce)]
           ;; Check for DRM
           (if (drm-protected? read-entry)
             {:error "This EPUB is DRM-protected and cannot be imported. Please use a DRM-free version."}

             ;; Find and parse OPF
             (if-let [opf-path (find-opf-path read-entry)]
               (if-let [opf-bytes (read-entry opf-path)]
                 (let [opf-doc (parse-xml opf-bytes)
                       {:keys [title creator manifest spine-ids]} (parse-opf opf-bytes opf-path)
                  ;; Process each spine item into a chapter
                  chapters
                  (reduce
                    (fn [chs spine-id]
                      (if-let [{:keys [href media-type]} (get manifest spine-id)]
                        (let [xhtml-path (resolve-path opf-path href)
                              xhtml-bytes (read-entry xhtml-path)]
                          (if (and xhtml-bytes
                                (or (str/includes? (or media-type "") "xhtml")
                                  (str/includes? (or media-type "") "html")))
                            (let [doc (Jsoup/parse (String. ^bytes xhtml-bytes "UTF-8"))
                                  _ (process-images doc xhtml-path read-entry image-mode)
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
                      :chapters chapters
                      :biblio (opf->biblio opf-doc)}
                     {:error "No readable content found in this EPUB"}))

                 {:error "Could not read package document (OPF file)"})
               {:error "Invalid EPUB: no container.xml or missing rootfile"})))))
     ;; A cap breach is a REJECTED INPUT, not a pipeline defect, and the two must
     ;; not share a log level. `::process-epub` sits in :alert/pipeline-failures
     ;; (logging.cljc), so routing a refused upload there mails the operator for
     ;; something a user did. The same set already draws this line for
     ;; `cards/no-content-exhausted` against a model refusal.
     (catch clojure.lang.ExceptionInfo e
       (if (#{::zip-too-large ::unsupported-source} (:type (ex-data e)))
         (do (tel/log! {:level :warn :id ::rejected :data (ex-data e)} (.getMessage e))
             {:error (str "This EPUB could not be imported: " (.getMessage e))})
         (do (tel/error! {:id ::process-epub} e)
             {:error (str "Failed to parse EPUB: " (.getMessage e))})))
     (catch Exception e
       (tel/error! {:id ::process-epub} e)
       {:error (str "Failed to parse EPUB: " (.getMessage e))}))))

;; ── 1.4 OPF biblio extraction ────────────────────────────────────

(defn- opf-meta-texts
  "Trimmed text content of every element matching `metadata <selector>`."
  [^Document doc selector]
  (->> (.select doc (str "metadata " selector))
       (keep (fn [^Element e]
               (let [t (str/trim (.text e))]
                 (when (seq t) t))))
       vec))

(defn- opf-identifiers
  "All <dc:identifier> as [{:scheme :value} ...]. Scheme from opf:scheme
   attribute or detected from value prefix."
  [^Document doc]
  (->> (.select doc "metadata dc\\:identifier, metadata identifier")
       (mapv (fn [^Element e]
               (let [v      (str/trim (.text e))
                     attr-s (.attr e "opf:scheme")
                     vl     (str/lower-case v)
                     scheme (cond
                              (seq attr-s)                       (str/lower-case attr-s)
                              (str/starts-with? vl "urn:isbn:")  "isbn"
                              (str/starts-with? vl "doi:")       "doi"
                              (re-find #"^10\." v)               "doi"
                              :else                              "unknown")]
                 {:scheme scheme :value v})))))

(defn- year-from-date [s]
  (when (and (string? s) (>= (count s) 4))
    (try (Integer/parseInt (subs s 0 4)) (catch Exception _ nil))))

(def empty-biblio
  "What every biblio path returns when it has nothing. Named so the four
   failure branches cannot drift from each other."
  {:local {} :identifiers {}})

(defn opf->biblio
  "Derive CSL-JSON from an ALREADY-PARSED package document.

   Pre : `doc` is the OPF parsed by `parse-xml`.
   Post: {:local <csl-json> :identifiers {:isbn-13 :doi}}.
   Separated from `extract-biblio` so `process-epub` can call it on the document
   it already holds. Reading and parsing the package a second time is what this
   split removes — see plans/large-epub-import.md §4."
  [^Document doc]
  (let [title        (or (some-> (.selectFirst doc "metadata dc\\:title") .text str/trim)
                         (some-> (.selectFirst doc "metadata title") .text str/trim))
        creators     (opf-meta-texts doc "dc\\:creator, creator")
        contributors (opf-meta-texts doc "dc\\:contributor, contributor")
        publishers   (opf-meta-texts doc "dc\\:publisher, publisher")
        languages    (opf-meta-texts doc "dc\\:language, language")
        subjects     (opf-meta-texts doc "dc\\:subject, subject")
        description  (some-> (.selectFirst doc "metadata dc\\:description, metadata description")
                             .text str/trim)
        dates        (opf-meta-texts doc "dc\\:date, date")
        year         (some year-from-date dates)
        ids          (opf-identifiers doc)
        isbn         (some #(when (= "isbn" (:scheme %))
                              (str/replace (:value %) #"(?i)^urn:isbn:" ""))
                           ids)
        doi          (some #(when (= "doi" (:scheme %))
                              (str/replace (:value %) #"(?i)^doi:" ""))
                           ids)
        local        (cond-> {:type "book"}
                       (seq title)        (assoc :title title)
                       (seq creators)     (assoc :author (mapv #(hash-map :literal %) creators))
                       (seq contributors) (assoc :editor (mapv #(hash-map :literal %) contributors))
                       (seq publishers)   (assoc :publisher (first publishers))
                       (seq languages)    (assoc :language (first languages))
                       (seq subjects)     (assoc :keyword (str/join ", " subjects))
                       (seq description)  (assoc :abstract description)
                       year               (assoc :issued {:date-parts (csl/pad-date-parts [[year]])})
                       isbn               (assoc :ISBN isbn)
                       doi                (assoc :DOI doi))]
    {:local local
     :identifiers (cond-> {} isbn (assoc :isbn-13 isbn) doi (assoc :doi doi))}))

(defn extract-biblio
  "Parse OPF metadata from an EPUB into CSL-JSON shape.

   Pre : `src` is a File or a byte[] holding an EPUB.
   Post: {:local <csl-json> :identifiers {:isbn-13 :doi}} on success,
         `empty-biblio` on any parse failure. Never throws.

   The import path does NOT call this — `process-epub` returns `:biblio` from
   the package document it already parsed. This is the refetch path's entry
   point, which runs long after import with no in-flight parse to reuse."
  ([src] (extract-biblio src default-limits))
  ([src limits]
   (try
     (with-epub-entries src limits
       (fn [read-entry]
         (if-let [opf-path (find-opf-path read-entry)]
           (if-let [opf-bytes (read-entry opf-path)]
             (opf->biblio (parse-xml opf-bytes))
             empty-biblio)
           empty-biblio)))
     (catch Exception e
       (tel/log! {:level :warn :id ::extract-biblio} (.getMessage e))
       empty-biblio))))
