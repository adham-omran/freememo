(ns freememo.biblio-web
  "Local biblio harvest from HTML pages. Parses meta tag schemes in priority
   order: Highwire citation_* → JSON-LD → Dublin Core → PRISM → OpenGraph →
   <title>. Pure CLJ; server-only."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [taoensso.telemere :as tel])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Document Element]))

(defn- not-empty-str [s]
  (when (and (string? s) (not (str/blank? s))) s))

(defn- non-blank? [v]
  (cond
    (nil? v) false
    (string? v) (not (str/blank? v))
    (coll? v) (boolean (seq v))
    :else true))

(defn- meta-value
  "Returns content of first <meta name=key> or <meta property=key> (non-blank)."
  [^Document doc key]
  (or (some-> (.selectFirst doc (str "meta[name=\"" key "\"]"))
              (.attr "content") not-empty-str)
      (some-> (.selectFirst doc (str "meta[property=\"" key "\"]"))
              (.attr "content") not-empty-str)))

(defn- meta-values
  "All content values for <meta name=key> or <meta property=key>, non-blank."
  [^Document doc key]
  (vec (keep (fn [^Element e] (not-empty-str (.attr e "content")))
             (.select doc (str "meta[name=\"" key "\"], meta[property=\"" key "\"]")))))

(defn- parse-year [s]
  (when s (when-let [y (re-find #"\d{4}" s)] (Integer/parseInt y))))

(defn- literals [strs]
  (mapv #(hash-map :literal %) (filter not-empty-str strs)))

;; ── Highwire citation_* (1.6 highest priority) ─────────────────────

(defn- harvest-highwire [^Document doc]
  (let [date    (or (meta-value doc "citation_date")
                    (meta-value doc "citation_publication_date")
                    (meta-value doc "citation_cover_date")
                    (meta-value doc "citation_online_date")
                    (meta-value doc "citation_year"))
        year    (parse-year date)
        authors (->> (concat (meta-values doc "citation_author")
                             (meta-values doc "citation_authors"))
                     vec)
        first-p (meta-value doc "citation_firstpage")
        last-p  (meta-value doc "citation_lastpage")]
    (cond-> {}
      (meta-value doc "citation_title")        (assoc :title (meta-value doc "citation_title"))
      (seq authors)                            (assoc :author (literals authors))
      year                                     (assoc :issued {:date-parts [[year]]})
      (meta-value doc "citation_journal_title") (assoc :container-title (meta-value doc "citation_journal_title"))
      (meta-value doc "citation_volume")       (assoc :volume (meta-value doc "citation_volume"))
      (meta-value doc "citation_issue")        (assoc :issue (meta-value doc "citation_issue"))
      first-p                                  (assoc :page (if last-p (str first-p "-" last-p) first-p))
      (meta-value doc "citation_doi")          (assoc :DOI (meta-value doc "citation_doi"))
      (meta-value doc "citation_isbn")         (assoc :ISBN (meta-value doc "citation_isbn"))
      (meta-value doc "citation_issn")         (assoc :ISSN (meta-value doc "citation_issn"))
      (meta-value doc "citation_publisher")    (assoc :publisher (meta-value doc "citation_publisher"))
      (meta-value doc "citation_abstract")     (assoc :abstract (meta-value doc "citation_abstract"))
      (meta-value doc "citation_language")     (assoc :language (meta-value doc "citation_language"))
      (meta-value doc "citation_keywords")     (assoc :keyword (meta-value doc "citation_keywords")))))

;; ── JSON-LD (schema.org Article / ScholarlyArticle / Book) ─────────

(def ^:private article-types
  #{"Article" "NewsArticle" "BlogPosting" "ScholarlyArticle" "TechArticle"
    "ReportageNewsArticle" "AnalysisNewsArticle"})

(def ^:private book-types #{"Book" "Audiobook"})

(defn- jsonld-author [v]
  (cond
    (string? v) [{:literal v}]
    (map? v)    (when-let [n (or (get v "name") (get v :name))]
                  [{:literal n}])
    (sequential? v) (vec (mapcat jsonld-author v))
    :else nil))

(defn- jsonld->local [m]
  (let [t       (get m "@type")
        types   (set (if (sequential? t) t [t]))
        kind    (cond
                  (some article-types types) "article-magazine"
                  (some book-types types)    "book"
                  :else                       "webpage")
        authors (jsonld-author (or (get m "author") (get m "creator")))
        year    (parse-year (or (get m "datePublished") (get m "dateCreated")))
        pub-v   (get m "publisher")
        publisher (cond
                    (string? pub-v) pub-v
                    (map? pub-v)    (or (get pub-v "name") (get pub-v :name)))
        ident   (let [id (get m "identifier")]
                  (cond
                    (string? id) id
                    (map? id)    (get id "value")
                    :else        nil))]
    (cond-> {:type kind}
      (get m "headline")                              (assoc :title (get m "headline"))
      (and (nil? (get m "headline")) (get m "name"))  (assoc :title (get m "name"))
      (seq authors)                                   (assoc :author authors)
      year                                            (assoc :issued {:date-parts [[year]]})
      publisher                                       (assoc :publisher publisher)
      (get m "description")                           (assoc :abstract (get m "description"))
      (get m "url")                                   (assoc :URL (get m "url"))
      (get m "inLanguage")                            (assoc :language (get m "inLanguage"))
      (get m "isbn")                                  (assoc :ISBN (get m "isbn"))
      (and ident (re-find #"(?i)^doi:" ident))        (assoc :DOI (str/replace ident #"(?i)^doi:" "")))))

(defn- jsonld-candidates [^Document doc]
  (mapcat
    (fn [^Element s]
      (try
        (let [parsed (json/parse-string (.data s))
              items  (cond
                       (sequential? parsed) parsed
                       (and (map? parsed) (get parsed "@graph")) (get parsed "@graph")
                       :else [parsed])]
          (filter map? items))
        (catch Exception _ nil)))
    (.select doc "script[type=\"application/ld+json\"]")))

(defn- harvest-json-ld [^Document doc]
  (when-let [item (some (fn [m]
                          (let [t (get m "@type")
                                types (set (if (sequential? t) t [t]))]
                            (when (or (some article-types types)
                                      (some book-types types))
                              m)))
                        (jsonld-candidates doc))]
    (jsonld->local item)))

;; ── Dublin Core ────────────────────────────────────────────────────

(defn- dc-val [^Document doc & keys]
  (some #(meta-value doc %) keys))

(defn- harvest-dc [^Document doc]
  (let [year (parse-year (dc-val doc "DC.date" "DCTERMS.date" "DCTERMS.issued"))
        creators (->> (concat (meta-values doc "DC.creator")
                              (meta-values doc "DCTERMS.creator"))
                      vec)
        ident (dc-val doc "DC.identifier" "DCTERMS.identifier")]
    (cond-> {}
      (dc-val doc "DC.title" "DCTERMS.title")
      (assoc :title (dc-val doc "DC.title" "DCTERMS.title"))

      (seq creators)
      (assoc :author (literals creators))

      year
      (assoc :issued {:date-parts [[year]]})

      (dc-val doc "DC.publisher" "DCTERMS.publisher")
      (assoc :publisher (dc-val doc "DC.publisher" "DCTERMS.publisher"))

      (dc-val doc "DC.description" "DCTERMS.abstract")
      (assoc :abstract (dc-val doc "DC.description" "DCTERMS.abstract"))

      (dc-val doc "DC.language" "DCTERMS.language")
      (assoc :language (dc-val doc "DC.language" "DCTERMS.language"))

      (and ident (re-find #"(?i)^doi:" ident))
      (assoc :DOI (str/replace ident #"(?i)^doi:" ""))

      (and ident (re-find #"^10\." ident))
      (assoc :DOI ident)

      (and ident (re-find #"(?i)^urn:isbn:" ident))
      (assoc :ISBN (str/replace ident #"(?i)^urn:isbn:" "")))))

;; ── PRISM ──────────────────────────────────────────────────────────

(defn- harvest-prism [^Document doc]
  (let [year (parse-year (meta-value doc "prism.publicationDate"))]
    (cond-> {}
      (meta-value doc "prism.publicationName") (assoc :container-title (meta-value doc "prism.publicationName"))
      (meta-value doc "prism.volume")          (assoc :volume (meta-value doc "prism.volume"))
      (meta-value doc "prism.number")          (assoc :issue (meta-value doc "prism.number"))
      (meta-value doc "prism.doi")             (assoc :DOI (meta-value doc "prism.doi"))
      (meta-value doc "prism.issn")            (assoc :ISSN (meta-value doc "prism.issn"))
      year                                     (assoc :issued {:date-parts [[year]]}))))

;; ── OpenGraph + article:* ──────────────────────────────────────────

(defn- harvest-og [^Document doc]
  (let [authors (meta-values doc "article:author")
        year    (parse-year (meta-value doc "article:published_time"))]
    (cond-> {}
      (meta-value doc "og:title")        (assoc :title (meta-value doc "og:title"))
      (meta-value doc "og:description")  (assoc :abstract (meta-value doc "og:description"))
      (meta-value doc "og:url")          (assoc :URL (meta-value doc "og:url"))
      (meta-value doc "og:site_name")    (assoc :container-title (meta-value doc "og:site_name"))
      (seq authors)                       (assoc :author (literals authors))
      year                                (assoc :issued {:date-parts [[year]]}))))

;; ── <title> fallback ───────────────────────────────────────────────

(defn- harvest-title [^Document doc]
  (when-let [t (some-> (.selectFirst doc "title") .text str/trim not-empty-str)]
    {:title t}))

;; ── Field-level precedence merge ───────────────────────────────────

(defn- merge-prefer-first
  "Layer maps; earlier wins per key when its value is non-blank."
  [& maps]
  (reduce (fn [acc m]
            (reduce-kv (fn [a k v]
                         (if (and (non-blank? v) (not (contains? a k)))
                           (assoc a k v)
                           a))
                       acc m))
          {}
          maps))

;; ── Top-level ──────────────────────────────────────────────────────

(defn harvest-html
  "Parse an HTML page for biblio metadata.
   Pre:  html is a non-blank string.
   Post: {:local <csl-shaped map> :identifiers {:doi :isbn}}.
   Precedence: Highwire > JSON-LD > Dublin Core > PRISM > OG > <title>.
   Invariant: no network; no side effects."
  [^String html]
  (try
    (let [doc (Jsoup/parse html)
          hw  (harvest-highwire doc)
          jld (or (harvest-json-ld doc) {})
          dc  (harvest-dc doc)
          pr  (harvest-prism doc)
          og  (harvest-og doc)
          ti  (harvest-title doc)
          local (merge-prefer-first hw jld dc pr og ti)
          ids   {:doi  (or (:DOI hw) (:DOI jld) (:DOI dc) (:DOI pr))
                 :isbn (or (:ISBN hw) (:ISBN jld) (:ISBN dc))}]
      {:local local :identifiers ids})
    (catch Exception e
      (tel/log! {:level :warn :id ::harvest-html} (.getMessage e))
      {:local {} :identifiers {}})))
