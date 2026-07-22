(ns freememo.biblio-resolver
  "Identifier resolvers: CrossRef (DOI), arXiv, OpenLibrary (ISBN), plus a
   dispatcher that picks one in priority DOI > arXiv > ISBN. One shared HTTP
   client; per-call timeout default 5s. All failures return tagged values —
   resolvers never throw."
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.string :as str]
            [taoensso.telemere :as tel]
            [freememo.csl-util :as csl])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Element]
           [org.jsoup.parser Parser]))

(def ^:private default-timeout-ms 5000)

;; CrossRef polite-pool requires a contact email in the User-Agent. Replace
;; the placeholder with a real address to get priority routing.
(def ^:private user-agent "FreeMemo/1.0 (mailto:freememo@example.com)")

;; ── 2.1 Shared HTTP client ─────────────────────────────────────────

(defn- get-string!
  "Single GET. Returns {:status 200 :body str} on success;
   {:status :not-found} on 404; {:status :http-error :code n} on other non-2xx;
   {:status :timeout} on socket timeout; {:status :exception :reason ...}
   on anything else. Never throws."
  [url & [{:keys [timeout-ms accept]}]]
  (let [t (or timeout-ms default-timeout-ms)]
    (try
      (let [resp (http/get url
                   {:headers (cond-> {"User-Agent" user-agent}
                               accept (assoc "Accept" accept))
                    :cookie-policy :none
                    :throw-exceptions false
                    :socket-timeout t
                    :connection-timeout t
                    :max-body 5242880})
            status (:status resp)]
        (cond
          (= 200 status) {:status 200 :body (:body resp)}
          (= 404 status) {:status :not-found}
          :else          {:status :http-error :code status}))
      (catch java.net.SocketTimeoutException _
        (tel/log! {:level :warn :id ::http-get :url url} "socket timeout")
        {:status :timeout})
      (catch java.net.ConnectException _
        (tel/log! {:level :warn :id ::http-get :url url} "connection refused")
        {:status :exception :reason :connect})
      (catch Exception e
        (tel/log! {:level :warn :id ::http-get :url url} (.getMessage e))
        {:status :exception :reason (.getMessage e)}))))

(defn- get-json! [url & opts]
  (let [r (apply get-string! url opts)]
    (if (= 200 (:status r))
      (try {:status 200 :json (json/parse-string (:body r))}
           (catch Exception e
             (tel/log! {:level :warn :id ::parse-json :url url} (.getMessage e))
             {:status :parse-error}))
      r)))

;; ── 2.2 CrossRef ───────────────────────────────────────────────────

(defn- crossref-author [a]
  (let [fam (get a "family")
        giv (get a "given")]
    (cond
      (and fam giv) {:family fam :given giv}
      fam           {:family fam}
      :else         {:literal (or (get a "name") "")})))

(defn- crossref->csl
  "Convert CrossRef `message` payload into CSL-JSON we consume."
  [message]
  (let [authors   (mapv crossref-author (or (get message "author") []))
        issued    (get-in message ["issued" "date-parts"])
        title-v   (get message "title")
        title     (cond (string? title-v) title-v (sequential? title-v) (first title-v))
        cont-v    (get message "container-title")
        container (cond (string? cont-v) cont-v (sequential? cont-v) (first cont-v))
        issn      (let [v (get message "ISSN")] (if (sequential? v) (first v) v))]
    (cond-> {:type (or (get message "type") "article-journal")}
      title                       (assoc :title title)
      (seq authors)               (assoc :author authors)
      issued                      (assoc :issued {:date-parts (csl/pad-date-parts issued)})
      container                   (assoc :container-title container)
      (get message "volume")      (assoc :volume (get message "volume"))
      (get message "issue")       (assoc :issue (get message "issue"))
      (get message "page")        (assoc :page (get message "page"))
      (get message "DOI")         (assoc :DOI (get message "DOI"))
      (get message "URL")         (assoc :URL (get message "URL"))
      issn                        (assoc :ISSN issn)
      (get message "publisher")   (assoc :publisher (get message "publisher"))
      (get message "abstract")    (assoc :abstract (get message "abstract"))
      (get message "language")    (assoc :language (get message "language")))))

(defn crossref-fetch!
  "Pre:  doi is a non-blank string (e.g. \"10.1038/nature12373\").
   Post: {:csl <csl-json>} on success; tagged failure otherwise."
  [doi]
  (let [encoded (java.net.URLEncoder/encode doi "UTF-8")
        url     (str "https://api.crossref.org/works/" encoded)
        r       (get-json! url {:accept "application/json"})]
    (if (and (= 200 (:status r)) (:json r))
      (if-let [msg (get (:json r) "message")]
        {:csl (crossref->csl msg)}
        {:status :parse-error})
      r)))

;; ── 2.3 arXiv ──────────────────────────────────────────────────────

(defn- arxiv-entry->csl [^Element entry]
  (let [title    (some-> (.selectFirst entry "title") .text str/trim)
        summary  (some-> (.selectFirst entry "summary") .text str/trim)
        published (some-> (.selectFirst entry "published") .text str/trim)
        year     (when published
                   (try (Integer/parseInt (subs published 0 4))
                        (catch Exception _ nil)))
        url      (some-> (.selectFirst entry "id") .text str/trim)
        doi      (some-> (.selectFirst entry "arxiv\\:doi") .text str/trim)
        authors  (->> (.select entry "author > name")
                      (mapv (fn [^Element e] {:literal (str/trim (.text e))})))]
    (cond-> {:type "article"}
      title         (assoc :title title)
      (seq authors) (assoc :author authors)
      year          (assoc :issued {:date-parts (csl/pad-date-parts [[year]])})
      url           (assoc :URL url)
      doi           (assoc :DOI doi)
      summary       (assoc :abstract summary))))

(defn arxiv-fetch!
  "Pre:  arxiv-id is a non-blank string (e.g. \"2301.08277\" or \"cs.LG/0309048\").
   Post: {:csl <csl-json>} on success; tagged failure otherwise."
  [arxiv-id]
  (let [url (str "https://export.arxiv.org/api/query?id_list="
                 (java.net.URLEncoder/encode arxiv-id "UTF-8"))
        r   (get-string! url {:accept "application/atom+xml"})]
    (if (= 200 (:status r))
      (try
        (let [doc (Jsoup/parse (:body r) "" (Parser/xmlParser))
              entry (.selectFirst doc "entry")]
          (if entry
            {:csl (arxiv-entry->csl entry)}
            {:status :not-found}))
        (catch Exception e
          (tel/log! {:level :warn :id ::arxiv-parse} (.getMessage e))
          {:status :parse-error}))
      r)))

;; ── 2.4 OpenLibrary ────────────────────────────────────────────────

(defn- openlibrary->csl [data]
  (let [authors    (mapv (fn [a] {:literal (get a "name")})
                         (or (get data "authors") []))
        publishers (keep #(get % "name") (or (get data "publishers") []))
        year       (when-let [d (get data "publish_date")]
                     (when-let [y (re-find #"\d{4}" d)] (Integer/parseInt y)))
        isbn-13    (first (get-in data ["identifiers" "isbn_13"]))
        isbn-10    (first (get-in data ["identifiers" "isbn_10"]))]
    (cond-> {:type "book"}
      (get data "title")    (assoc :title (get data "title"))
      (seq authors)         (assoc :author authors)
      year                  (assoc :issued {:date-parts (csl/pad-date-parts [[year]])})
      (seq publishers)      (assoc :publisher (first publishers))
      (get data "url")      (assoc :URL (get data "url"))
      (or isbn-13 isbn-10)  (assoc :ISBN (or isbn-13 isbn-10)))))

(defn openlibrary-fetch!
  "Pre:  isbn is a digit-string of length 10 or 13.
   Post: {:csl <csl-json>} on success; tagged failure otherwise."
  [isbn]
  (let [url (str "https://openlibrary.org/api/books?bibkeys=ISBN:"
                 isbn "&format=json&jscmd=data")
        r   (get-json! url {:accept "application/json"})]
    (if (and (= 200 (:status r)) (:json r))
      (let [book-key (str "ISBN:" isbn)
            data     (get (:json r) book-key)]
        (if data
          {:csl (openlibrary->csl data)}
          {:status :not-found}))
      r)))

;; ── 2.6 Dispatcher ─────────────────────────────────────────────────

(defn- arxiv-doi? [^String doi]
  (when doi (re-find #"(?i)10\.48550/arXiv\." doi)))

(defn resolve!
  "Pre:  ids is a map with optional :doi :arxiv :isbn-13 :isbn-10 keys.
   Post: {:csl <csl-json> :resolver <:crossref|:arxiv|:openlibrary>} on success;
         {:status :no-id} when no identifier present;
         tagged failure from the underlying resolver otherwise.
   Priority: arXiv-DOI → arXiv resolver; else DOI → CrossRef; else arXiv → arXiv;
   else ISBN → OpenLibrary."
  [{:keys [doi arxiv isbn-13 isbn-10]}]
  (let [isbn (or isbn-13 isbn-10)]
    (cond
      (and doi (arxiv-doi? doi) arxiv)
      (let [r (arxiv-fetch! arxiv)]
        (if (:csl r) (assoc r :resolver :arxiv) r))

      doi
      (let [r (crossref-fetch! doi)]
        (if (:csl r) (assoc r :resolver :crossref) r))

      arxiv
      (let [r (arxiv-fetch! arxiv)]
        (if (:csl r) (assoc r :resolver :arxiv) r))

      isbn
      (let [r (openlibrary-fetch! isbn)]
        (if (:csl r) (assoc r :resolver :openlibrary) r))

      :else {:status :no-id})))
