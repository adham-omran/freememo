(ns freememo.biblio-pdf
  "Local biblio harvest from PDF artifacts: PDDocumentInformation (Info dict),
   XMP packet (parsed via jsoup XML mode), and DOI/arXiv/ISBN regex over the
   text of pages 1, 2, and the last page. Pure CLJ; server-only."
  (:require [clojure.string :as str]
            [freememo.ocr :as ocr]
            [taoensso.telemere :as tel])
  (:import [org.apache.pdfbox Loader]
           [org.apache.pdfbox.pdmodel PDDocumentInformation PDDocumentCatalog]
           [org.apache.pdfbox.pdmodel.common PDMetadata]
           [org.jsoup Jsoup]
           [org.jsoup.nodes Document Element]
           [org.jsoup.parser Parser]
           [java.util Calendar]))

;; ── shared helpers ──────────────────────────────────────────────────

(defn- not-empty-str [s]
  (when (and (string? s) (not (str/blank? s))) s))

(defn- cal->year [^Calendar c]
  (when c (.get c Calendar/YEAR)))

(defn- year->issued [year]
  (when year {:date-parts [[year]]}))

(defn- str->author-array [s]
  (when-let [v (not-empty-str s)] [{:literal (str/trim v)}]))

(defn- strs->author-array [xs]
  (let [vs (->> (or xs []) (filter not-empty-str) (mapv str/trim))]
    (when (seq vs) (mapv #(hash-map :literal %) vs))))

;; ── 1.1 Info dict ───────────────────────────────────────────────────

(defn harvest-info-dict
  "Reads the classic PDF Info dict via PDDocumentInformation.
   Pre:  pdf-bytes is a byte array containing a valid PDF.
   Post: map with :title :author :subject :keywords :creator :producer
         :created-year :modified-year (nil entries for absent fields),
         or nil on parse failure."
  [^bytes pdf-bytes]
  (try
    (let [doc (Loader/loadPDF pdf-bytes)]
      (try
        (let [^PDDocumentInformation info (.getDocumentInformation doc)]
          {:title         (not-empty-str (.getTitle info))
           :author        (not-empty-str (.getAuthor info))
           :subject       (not-empty-str (.getSubject info))
           :keywords      (not-empty-str (.getKeywords info))
           :creator       (not-empty-str (.getCreator info))
           :producer      (not-empty-str (.getProducer info))
           :created-year  (cal->year (.getCreationDate info))
           :modified-year (cal->year (.getModificationDate info))})
        (finally (.close doc))))
    (catch Exception e
      (tel/log! {:level :warn :id ::harvest-info-dict} (.getMessage e))
      nil)))

;; ── 1.2 XMP packet ─────────────────────────────────────────────────

(defn- read-xmp-bytes
  "Extract the XMP packet bytes from the PDF catalog, or nil if absent."
  [^bytes pdf-bytes]
  (try
    (let [doc (Loader/loadPDF pdf-bytes)]
      (try
        (let [^PDDocumentCatalog catalog (.getDocumentCatalog doc)
              ^PDMetadata md (.getMetadata catalog)]
          (when md (.toByteArray md)))
        (finally (.close doc))))
    (catch Exception e
      (tel/log! {:level :warn :id ::read-xmp-bytes} (.getMessage e))
      nil)))

(defn- parse-xmp [^bytes xmp-bs]
  (Jsoup/parse (String. xmp-bs "UTF-8") "" (Parser/xmlParser)))

(defn- xmp-prop
  "Look up an XMP property (e.g. \"dc:title\") on the parsed XMP doc.
   Element form first, then attribute form on rdf:Description."
  [^Document doc qname]
  (let [esc (str/replace qname ":" "\\:")]
    (or (some-> (.selectFirst doc esc) .text str/trim not-empty-str)
        (some-> (.selectFirst doc (str "rdf\\:Description[" esc "]"))
                (.attr qname) str/trim not-empty-str))))

(defn- xmp-list
  "List-valued XMP property — rdf:Alt/rdf:Bag/rdf:Seq containers — returns
   vector of trimmed non-blank strings."
  [^Document doc qname]
  (let [esc (str/replace qname ":" "\\:")
        items (->> (.select doc (str esc " rdf\\:li"))
                   (mapv (fn [^Element e] (str/trim (.text e))))
                   (filterv not-empty-str))]
    (or (seq items)
        (when-let [s (xmp-prop doc qname)] [s]))))

(defn harvest-xmp
  "Reads the XMP packet — Dublin Core, AdobePDF, XMP Basic, PRISM namespaces.
   Pre:  pdf-bytes is a byte array containing a valid PDF.
   Post: map with :title :authors :description :keywords :publisher :language
         :doi :issued-year (nil entries for absent fields),
         or nil when no XMP packet or parse fails."
  [^bytes pdf-bytes]
  (try
    (when-let [bs (read-xmp-bytes pdf-bytes)]
      (let [doc (parse-xmp bs)
            create-date (xmp-prop doc "xmp:CreateDate")]
        {:title       (xmp-prop doc "dc:title")
         :authors     (vec (xmp-list doc "dc:creator"))
         :description (xmp-prop doc "dc:description")
         :keywords    (or (xmp-prop doc "pdf:Keywords")
                          (when-let [ks (seq (xmp-list doc "dc:subject"))]
                            (str/join ", " ks)))
         :publisher   (first (xmp-list doc "dc:publisher"))
         :language    (first (xmp-list doc "dc:language"))
         :doi         (xmp-prop doc "prism:doi")
         :issued-year (when create-date
                        (try (Integer/parseInt (subs create-date 0 4))
                             (catch Exception _ nil)))}))
    (catch Exception e
      (tel/log! {:level :warn :id ::harvest-xmp} (.getMessage e))
      nil)))

;; ── 1.3 Identifier scan ────────────────────────────────────────────

(def ^:private doi-re     #"\b10\.\d{4,9}/[^\s\"'<>]+\b")
;; arXiv: prefix required to avoid false positives on dates/page-numbers.
(def ^:private arxiv-re   #"(?i)arXiv[:\s]+(\d{4}\.\d{4,5}(?:v\d+)?|[a-z\-]+/\d{7}(?:v\d+)?)")
(def ^:private isbn-13-re #"(?<![\d\-])(?:97[89](?:[\-\s]?\d){10})(?![\d\-])")
(def ^:private isbn-10-re #"(?<![\d\-])(?:\d[\-\s]?){9}[\dXx](?![\d\-])")

(defn- digits-only [^String s]
  (str/replace s #"[^\dXx]" ""))

(defn- isbn-13-valid? [^String s]
  (let [ds (digits-only s)]
    (and (= 13 (count ds))
         (zero? (mod (reduce + (map-indexed
                                 (fn [i c]
                                   (let [d (Character/digit ^char c 10)]
                                     (if (even? i) d (* 3 d))))
                                 ds))
                     10)))))

(defn- isbn-10-valid? [^String s]
  (let [cs (digits-only s)]
    (and (= 10 (count cs))
         (let [ds (mapv (fn [i c]
                          (if (and (= i 9) (or (= c \X) (= c \x)))
                            10
                            (Character/digit ^char c 10)))
                    (range) cs)]
           (and (every? #(<= 0 % 10) ds)
                (zero? (mod (reduce + (map-indexed (fn [i d] (* d (- 10 i))) ds))
                            11)))))))

(defn- clean-doi [^String s]
  (str/replace s #"[\.,;:)>\]]+$" ""))

(defn- find-first-valid
  "Iterate matches of pattern p over text; return first that satisfies valid?
   (normalized via normalize), or nil."
  [^java.util.regex.Pattern p valid? normalize ^String text]
  (let [m (re-matcher p text)]
    (loop []
      (when (.find m)
        (let [grp (.group m)]
          (if (valid? grp)
            (normalize grp)
            (recur)))))))

(defn scan-identifiers
  "Pre:  text is a string (may be empty).
   Post: {:doi :arxiv :isbn-13 :isbn-10} — nil per absent ID, normalized."
  [^String text]
  (let [t (or text "")]
    {:doi     (when-let [d (re-find doi-re t)] (clean-doi d))
     :arxiv   (when-let [m (re-find arxiv-re t)] (second m))
     :isbn-13 (find-first-valid isbn-13-re isbn-13-valid? digits-only t)
     :isbn-10 (find-first-valid isbn-10-re isbn-10-valid? digits-only t)}))

(defn scan-pdf-identifiers
  "Extract DOI / arXiv / ISBN from text on pages 1, 2, and the last page.
   Pre:  pdf-bytes is a byte array containing a valid PDF.
   Post: same shape as scan-identifiers."
  [^bytes pdf-bytes]
  (try
    (let [page-count (ocr/get-page-count pdf-bytes)
          pages      (distinct [1 (min 2 page-count) page-count])
          combined   (->> pages
                          (keep #(let [r (ocr/extract-text-pdfbox pdf-bytes %)]
                                   (when (:success r) (:text r))))
                          (str/join "\n"))]
      (scan-identifiers combined))
    (catch Exception e
      (tel/log! {:level :warn :id ::scan-pdf-identifiers} (.getMessage e))
      {:doi nil :arxiv nil :isbn-13 nil :isbn-10 nil})))

;; ── Top-level harvest ──────────────────────────────────────────────

(defn- info+xmp->csl
  "Project Info dict + XMP into a CSL-JSON-shaped local map; XMP fields win
   per-key when present."
  [info xmp]
  (let [authors (or (when (seq (:authors xmp)) (strs->author-array (:authors xmp)))
                    (when-let [a (:author info)] (str->author-array a)))
        title   (or (:title xmp) (:title info))
        desc    (or (:description xmp) (:subject info))
        kw      (or (:keywords xmp) (:keywords info))
        year    (or (:issued-year xmp) (:created-year info))
        lang    (:language xmp)
        pub     (:publisher xmp)]
    (cond-> {:type "document"}
      title          (assoc :title title)
      (seq authors)  (assoc :author authors)
      desc           (assoc :abstract desc)
      kw             (assoc :keyword kw)
      year           (assoc :issued (year->issued year))
      lang           (assoc :language lang)
      pub            (assoc :publisher pub))))

(defn harvest-pdf
  "Combine Info dict, XMP, and identifier scan into one harvest result.
   Pre:  pdf-bytes is a byte array containing a valid PDF.
   Post: {:local <csl-shaped map> :identifiers {:doi :arxiv :isbn-13 :isbn-10}}.
   The XMP-extracted DOI fills :doi when the text scan finds none."
  [^bytes pdf-bytes]
  (let [info  (or (harvest-info-dict pdf-bytes) {})
        xmp   (or (harvest-xmp pdf-bytes) {})
        local (info+xmp->csl info xmp)
        ids   (scan-pdf-identifiers pdf-bytes)
        ids+  (cond-> ids
                (and (nil? (:doi ids)) (:doi xmp)) (assoc :doi (:doi xmp)))]
    {:local local :identifiers ids+}))
