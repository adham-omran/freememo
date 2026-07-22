(ns freememo.wikipedia
  "Web article fetching — Wikipedia and generic URLs."
  (:require [clj-http.client :as http]
            [clojure.string :as str]
            [taoensso.telemere :as tel]
            [freememo.biblio-web :as bw]
            [freememo.content-type :as ct]
            [freememo.html-cleaner :as cleaner]
            [freememo.url-validate :as url])
  (:import [org.jsoup Jsoup]))

;; Forward decl — fetch-wikipedia-summary is defined after fetch-wikipedia-article.
(declare fetch-wikipedia-summary)

(def ^:private outbound-max-bytes 26214400)  ;; 25 MB
(def ^:private outbound-timeout-ms 30000)    ;; 30 s

(defn- parse-iso-date-parts
  "Parse common publication-date string shapes into a CSL :date-parts vec:
   [[Y]] [[Y M]] or [[Y M D]]. Returns nil on unparseable input.
   Handles: 2023-01-19 · 2023/01/19 · 2023-01 · 2023 · 2023-01-19T18:59:59Z."
  [s]
  (when (and s (string? s) (not (str/blank? s)))
    (let [stripped (-> s str/trim (str/replace #"T.*$" "") (str/replace "/" "-"))
          parts (str/split stripped #"-")
          ints (->> parts
                 (keep #(try (Integer/parseInt %) (catch Exception _ nil)))
                 (take 3)
                 vec)]
      (when (and (seq ints) (<= 1900 (first ints) 2100))
        [ints]))))

(defn- extract-issued-date-parts
  "Mine a CSL :issued date-parts vec from a parsed HTML document by checking
   common publication-date meta tags. Priority order: citation_date (Google
   Scholar / ArXiv abstract pages) → DC.date → DC.date.issued → article:published_time
   (Open Graph) → <time pubdate datetime=…>. Returns nil if none present or
   none parse."
  [^org.jsoup.nodes.Document doc]
  (let [meta-attr (fn [selector]
                    (some-> (.selectFirst doc selector) (.attr "content") not-empty))
        time-attr (some-> (.selectFirst doc "time[pubdate][datetime]")
                    (.attr "datetime") not-empty)
        candidate (or (meta-attr "meta[name=citation_date]")
                      (meta-attr "meta[name=citation_publication_date]")
                      (meta-attr "meta[name=citation_online_date]")
                      (meta-attr "meta[name=DC.date.issued]")
                      (meta-attr "meta[name=DC.date]")
                      (meta-attr "meta[property=article:published_time]")
                      (meta-attr "meta[name=pubdate]")
                      time-attr)]
    (parse-iso-date-parts candidate)))

(defn- redirect-error
  "Build an error result for a 3xx response. Surfaces the Location header so
   the user can paste the final URL directly. The original `safe-url?` check
   ran on `url`; the redirect target is unvalidated, so we never follow it."
  [resp url]
  (let [location (get-in resp [:headers "location"])]
    {:success false
     :error (str "URL " url " redirected"
              (when location (str " to " location))
              " — please provide the final URL directly")}))

(defn- simplify-latex
  "Strip \\displaystyle wrapper and common LaTeX noise from alt text.
   '{\\displaystyle F(g\\circ f)=F(g)\\circ F(f)}' → 'F(g ∘ f) = F(g) ∘ F(f)'"
  [alt]
  (-> alt
      (str/replace #"^\{\\displaystyle\s*" "")
      (str/replace #"\s*\\?\}$" "")
      (str/replace #"\\displaystyle\s*" "")
      (str/replace "\\circ" "∘")
      (str/replace "\\colon" ":")
      (str/replace "\\to" "→")
      (str/replace #"\\mathrm\s*\{([^}]*)\}" "$1")
      (str/replace #"\\operatorname\s*\{([^}]*)\}" "$1")
      (str/replace #"\\(?:left|right)([()\\[\\]|])" "$1")
      (str/replace "\\," "")
      (str/replace "\\!" "")
      (str/replace "\\;" " ")
      (str/replace "\\quad" " ")
      (str/replace #"\\_\{([^}]*)\}" "_{$1}")
      (str/replace #"\\\^\{([^}]*)\}" "^{$1}")
      str/trim))

(defn- preprocess-wikipedia-html
  "Pre-process Wikipedia REST API HTML before sanitization:
   1. Remove <math> elements (prevents Jsoup text promotion of MathML nodes)
   2. Simplify alt text on math <img> fallbacks
   3. Rewrite protocol-relative URLs to https://
   4. Rewrite relative `./<slug>` hrefs to https://<lang>.wikipedia.org/wiki/<slug>
   5. Wrap body contents in a <div> carrying the source <body>'s `dir`/`lang`
      so RTL layout survives `clean-html` (which discards the body element)."
  [html lang]
  (let [doc (Jsoup/parse html)
        wiki-link-prefix (str "https://" lang ".wikipedia.org/wiki/")]
    ;; Phase 0: Strip navigation chrome (navboxes, portalboxes)
    (doseq [nav (.select doc "[role=navigation], nav")]
      (.remove nav))
    ;; Phase 1: Math — remove <math> elements, clean alt on fallback <img>
    (doseq [math-el (.select doc "math")]
      (.remove math-el))
    (doseq [img (.select doc "img.mwe-math-fallback-image-inline, img.mwe-math-fallback-image-display")]
      (let [alt (.attr img "alt")]
        (when (seq alt)
          (.attr img "alt" (simplify-latex alt)))))
    ;; Phase 2: Images — rewrite protocol-relative URLs
    (doseq [img (.select doc "img[src^=//]")]
      (.attr img "src" (str "https:" (.attr img "src"))))
    ;; Phase 3: Links — rewrite relative hrefs to absolute same-wiki URLs
    (doseq [a (.select doc "a[href^=./]")]
      (let [href (.attr a "href")
            slug (subs href 2)] ;; strip "./"
        (.attr a "href" (str wiki-link-prefix slug))))
    ;; Phase 4: Propagate body dir/lang into a wrapping <div>
    (let [body (.body doc)
          body-dir  (.attr body "dir")
          body-lang (.attr body "lang")
          inner     (.html body)]
      (str "<div"
           (when (seq body-dir)  (str " dir=\""  body-dir  "\""))
           (when (seq body-lang) (str " lang=\"" body-lang "\""))
           ">" inner "</div>"))))

;; Matches: <lang>.wikipedia.org, <lang>.m.wikipedia.org, m.<lang>.wikipedia.org.
;; Captures: lang (group 1, ≥2 chars so bare `m.wikipedia.org` doesn't capture
;; "m"), URL-encoded title up to ? or # (group 2).
(def ^:private wiki-url-re
  #"(?i)^https?://(?:m\.)?([a-z]{2,}[a-z0-9-]*)(?:\.m)?\.wikipedia\.org/wiki/([^?#]+)")

(defn parse-wikipedia-url
  "Parse a Wikipedia article URL into {:lang :title}, or nil if not a
   recognised Wikipedia article URL. URL-decodes the title; strips
   fragment and query. Accepts mobile subdomains in either position;
   rejects URLs with no language subdomain."
  [url]
  (when (string? url)
    (when-let [m (re-find wiki-url-re url)]
      {:lang  (str/lower-case (nth m 1))
       :title (java.net.URLDecoder/decode (nth m 2) "UTF-8")})))

(defn- fetch-wikipedia-article
  "Fetch and post-process the Parsoid HTML for a Wikipedia article.
   Pre  : ctx = {:lang :title}; both non-blank strings.
   Post : success → {:title :html :url :source-type \"wikipedia\" :biblio};
          3xx     → {:success false :error <msg>};
          non-200 → {:success false :error <msg>}."
  [{:keys [lang title]}]
  (let [encoded-title (java.net.URLEncoder/encode title "UTF-8")
        api-url (str "https://" lang ".wikipedia.org/api/rest_v1/page/html/" encoded-title)
        resp (http/get api-url
               {:headers {"User-Agent" "FreeMemo/1.0 (incremental reading app)"
                          "Accept" "text/html"}
                :cookie-policy :none
                :redirect-strategy :none
                :throw-exceptions false
                :socket-timeout outbound-timeout-ms
                :connection-timeout outbound-timeout-ms
                :max-body outbound-max-bytes})
        status (:status resp)]
    (cond
      (<= 300 status 399)
      (redirect-error resp api-url)

      (not= 200 status)
      {:success false :error (str "Wikipedia fetch failed (HTTP " status ")")}

      :else
      (let [page-url (str "https://" lang ".wikipedia.org/wiki/" encoded-title)
            summary  (try (fetch-wikipedia-summary {:lang lang :title title}) (catch Exception _ nil))
            biblio   {:local (cond-> {:type "webpage"
                                      :title title
                                      :URL page-url
                                      :container-title "Wikipedia"}
                               (and summary (nil? (:error summary)) (:extract summary))
                               (assoc :abstract (:extract summary)))
                      :identifiers {}}]
        {:title title
         :html (-> (:body resp) (preprocess-wikipedia-html lang) cleaner/clean-html)
         :url page-url
         :source-type "wikipedia"
         :biblio biblio}))))

(defn- cloudflare-blocked?
  "True when the response is a Cloudflare bot challenge — a status 403
   served by Cloudflare or any response carrying `cf-mitigated`. clj-http
   lowercases header names by default."
  [resp]
  (let [headers (:headers resp)
        status  (:status resp)
        server  (or (get headers "server") "")]
    (or (some? (get headers "cf-mitigated"))
        (and (= 403 status)
             (str/includes? (str/lower-case server) "cloudflare")))))

(defn- process-html-body
  "Extract title + cleaned content + biblio from an already-decoded HTML string.
   Pre:  html-str is a non-blank string; url is the originating URL.
   Post: {:title :html :url :source-type \"web\" :biblio {:local :identifiers}}."
  [html-str url]
  (let [biblio (try (bw/harvest-html html-str)
                    (catch Exception _ {:local {} :identifiers {}}))
        doc (Jsoup/parse html-str)
        title (or (some-> (.select doc "title") (.first) (.text))
                  (some-> (.select doc "h1") (.first) (.text))
                  url)
        issued-date-parts (extract-issued-date-parts doc)
        _ (doseq [sel ["nav" "footer" "header" "aside" ".sidebar" ".nav"
                       ".footer" ".header" ".advertisement" ".ad" "#comments"
                       "script" "style" "noscript" "iframe"]]
            (.remove (.select doc sel)))
        main-content (or (some-> (.select doc "article") (.first) (.html))
                         (some-> (.select doc "main") (.first) (.html))
                         (some-> (.select doc "[role=main]") (.first) (.html))
                         (some-> (.select doc ".content") (.first) (.html))
                         (some-> (.select doc "#content") (.first) (.html))
                         (.html (.body doc)))
        html (cleaner/clean-html main-content)
        biblio+url (update biblio :local
                     (fn [m] (cond-> m (not (:URL m)) (assoc :URL url))))]
    (cond-> {:title title :html html :url url :source-type "web" :biblio biblio+url}
      issued-date-parts (assoc :issued-date-parts issued-date-parts))))

(defn- fetch-generic-url
  "Fetch a generic URL as bytes, classify by Content-Type, and either decode +
   process as HTML or return a binary dispatch shape.
   Pre:  url is a non-blank, safe URL.
   Post: one of:
     - {:success true :title :html :url :source-type :biblio} — HTML processed
     - {:success true :dispatch :pdf|:epub :bytes :filename :url} — binary, caller stages
     - {:success false :error <msg>}"
  [url]
  (let [resp (http/get url
               {:headers {"User-Agent" "Mozilla/5.0 (compatible; FreeMemo/1.0)"}
                :as :byte-array
                :cookie-policy :none
                :redirect-strategy :none
                :throw-exceptions false
                :socket-timeout outbound-timeout-ms
                :connection-timeout outbound-timeout-ms
                :max-body outbound-max-bytes})
        status (:status resp)
        content-type (get-in resp [:headers "content-type"])
        content-disp (get-in resp [:headers "content-disposition"])]
    (cond
      (<= 300 status 399)
      (redirect-error resp url)

      (cloudflare-blocked? resp)
      {:success false
       :error (str "This site blocks automated fetching (Cloudflare). "
                "Open the page in your browser, select all (Ctrl/Cmd+A), "
                "copy, and use \"Paste Article\" instead.")}

      (not= 200 status)
      {:success false :error (str "Fetch failed (HTTP " status ")")}

      :else
      (let [[kind reject-msg] (ct/classify-http content-type)
            ^bytes body-bytes (:body resp)]
        (case kind
          :html
          (let [charset (ct/extract-charset content-type)
                html-str (String. body-bytes ^String charset)]
            (assoc (process-html-body html-str url) :success true))

          (:pdf :epub)
          {:success true
           :dispatch kind
           :bytes body-bytes
           :filename (ct/derive-filename content-disp url kind)
           :url url}

          :reject
          {:success false :error reject-msg})))))

(defn- arxiv-pdf->abs
  "Rewrite arxiv.org/pdf/<id>[.pdf] → arxiv.org/abs/<id> so the HTML meta
   extractor can read citation_date etc. The PDF URL itself returns binary,
   which Jsoup would parse as garbage."
  [url]
  (when (string? url)
    (let [m (re-matches #"(?i)^(https?://(?:www\.)?arxiv\.org)/pdf/([^/?#]+?)(?:\.pdf)?(?:[?#].*)?$" url)]
      (when m
        (str (nth m 1) "/abs/" (nth m 2))))))

(defn fetch-url
  "Fetch a URL and return cleaned HTML. Auto-detects Wikipedia URLs.
   Returns {:success true :title :html :url :source-type} or {:success false :error}.
   Rejects non-http(s), loopback, and private-network URLs to prevent SSRF."
  [url]
  (try
    (let [;; ArXiv PDF URLs serve binary; rewrite to the abs page so meta
          ;; extraction (citation_date etc.) works for the publication date.
          url (or (arxiv-pdf->abs url) url)]
      (cond
        (or (nil? url) (str/blank? url))
        {:success false :error "URL is required"}

        (not (url/safe-url? url))
        {:success false :error "URL not allowed (only public http/https addresses)"}

        :else
        (let [wiki-ctx (parse-wikipedia-url url)
              result (if wiki-ctx
                       (fetch-wikipedia-article wiki-ctx)
                       (fetch-generic-url url))]
          ;; Helper may have returned {:success false :error ...} for redirects
          ;; or non-200 statuses — pass through; otherwise mark success.
          (if (false? (:success result))
            (do (tel/log! {:level :warn :id ::fetch-url :data {:url url :error (:error result)}}
                  "URL fetch failed")
                result)
            (assoc result :success true)))))
    (catch Exception e
      (tel/log! {:level :warn :id ::fetch-url :data {:url url}} (.getMessage e))
      {:success false :error (.getMessage e)})))

;; ── 1.5 Wikipedia REST summary endpoint ───────────────────────────

(defn fetch-wikipedia-summary
  "Fetch the structured summary for a Wikipedia article.
   Pre  : ctx = {:lang :title}; both non-blank strings.
   Post : {:extract :description :thumbnail :pageid :wikibase-item :url}
          on 200; {:error <string>} otherwise."
  [{:keys [lang title]}]
  (let [api-url (str "https://" lang ".wikipedia.org/api/rest_v1/page/summary/"
                     (java.net.URLEncoder/encode title "UTF-8"))
        resp (http/get api-url
               {:headers {"User-Agent" "FreeMemo/1.0 (incremental reading app)"
                          "Accept" "application/json"}
                :cookie-policy :none
                :redirect-strategy :default
                :throw-exceptions false
                :socket-timeout outbound-timeout-ms
                :connection-timeout outbound-timeout-ms
                :max-body outbound-max-bytes
                :as :json})
        status (:status resp)]
    (if (= 200 status)
      (let [b (:body resp)]
        {:extract       (:extract b)
         :description   (:description b)
         :thumbnail     (get-in b [:thumbnail :source])
         :pageid        (:pageid b)
         :wikibase-item (:wikibase_item b)
         :url           (get-in b [:content_urls :desktop :page])})
      {:error (str "Wikipedia summary fetch failed (HTTP " status ")")})))
