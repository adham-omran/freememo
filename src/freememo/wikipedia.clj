(ns freememo.wikipedia
  "Web article fetching — Wikipedia and generic URLs."
  (:require [clj-http.client :as http]
            [freememo.desktop-link :as link]
            [clojure.string :as str]
            [taoensso.telemere :as tel]
            [freememo.content-type :as ct]
            [freememo.doc-html :as dh]
            [freememo.html-cleaner :as cleaner]
            [freememo.math :as math]
            [freememo.page-pipeline :as pipeline]
            [freememo.table-lowering :as tl]
            [freememo.url-validate :as url]
            [freememo.youtube-url :as yt])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Element TextNode]))

;; Forward decl — fetch-wikipedia-summary is defined after fetch-wikipedia-article.
(declare fetch-wikipedia-summary)

(def ^:private outbound-max-bytes 26214400)  ;; 25 MB
(def ^:private outbound-timeout-ms 30000)    ;; 30 s

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

(def ^:private wiki-banner-selector
  "Wikipedia's own layout tables — the maintenance banners a reader sees as a
   boxed notice, not as a grid.

   **This pass is redundant today, and that is measured, not assumed.** Across
   four articles, `table.ambox:not([role=presentation])` plus
   `table.metadata:not([role=presentation])` is 0 — every banner also carries
   `role=presentation`, so `table-lowering`'s generic rule already converts all
   of them and this call finds them first only by running earlier.

   It is kept as the one thing the generic rule cannot promise: `role` is an
   attribute Wikipedia chooses to emit, and if a template stops emitting it, a
   banner silently becomes a data table again. The class names are the source's
   own vocabulary and do not move.

   They live HERE and not in `freememo.table-lowering` because that namespace
   owns the generic rule for every source. A Wikipedia class there would make
   every later source pay the same cost again."
  "table.ambox, table.metadata")

(defn- separate-formula-paragraph!
  "Replace one display formula with its own paragraph.
   Pre: `formula` is attached. `text-node` contains stored TeX.
   Post: the formula occupies a separate paragraph.
         Surrounding content retains its order and inline markup.
   Invariant: splitting a paragraph never duplicates its anchor id."
  [^Element formula ^TextNode text-node]
  (let [equation (doto (Element. "p") (.appendChild text-node))]
    (when-let [paragraph (some-> (.parent formula) (.closest "p"))]
      (loop []
        (let [parent (.parent formula)
              following (drop (inc (.siblingIndex formula)) (vec (.childNodes parent)))
              tail (doto (.shallowClone parent) (.removeAttr "id"))]
          (doseq [node following] (.appendChild tail node))
          (.after parent formula)
          (when (pos? (.childNodeSize tail)) (.after formula tail))
          (when (zero? (.childNodeSize parent))
            (when (.hasAttr parent "id") (.attr equation "id" (.id parent)))
            (.remove parent))
          (when-not (identical? parent paragraph) (recur)))))
    (.replaceWith formula equation)))

(defn- wikipedia-math->tex!
  "Convert Wikipedia formula wrappers with nonblank normalized TeX annotations.
   Pre: `root` retains Wikipedia classes and MathML annotations.
   Post: converted wrappers become stored TeX text, with no paired image.
         Return [text-node source] pairs for restoration after layout cleanup.
   Invariant: formulas without annotations and ordinary images remain intact."
  [^Element root]
  (into []
    (keep
      (fn [^Element formula]
        (when-let [annotation (.selectFirst formula "annotation[encoding='application/x-tex']")]
          (let [tex (.wholeText ^Element annotation)]
            (when-not (str/blank? tex)
              (let [source (math/wrap-tex tex)
                    text-node (TextNode. source)
                    block? (or (.hasClass formula "mwe-math-element-block")
                               (some? (.selectFirst formula "math[display=block], img.mwe-math-fallback-image-display")))]
                (if block?
                  (separate-formula-paragraph! formula text-node)
                  (.replaceWith formula text-node))
                [text-node source]))))))
    (.select root (str ".mwe-math-element, .mwe-math-element-inline, "
                   ".mwe-math-element-block, [typeof='mw:Extension/math']"))))

(defn- preprocess-wikipedia-html
  "Pre-process Wikipedia REST API HTML before sanitization:
   0. Strip navigation chrome (navboxes, portalboxes)
   1. Turn maintenance banners into blockquotes — see `wiki-banner-selector`
   2. Convert normalized math annotations to stored TeX and remove remaining
      MathML. Simplify alternative text on retained fallback images.
   3. Rewrite protocol-relative URLs to https://
   4. Rewrite relative `./<slug>` hrefs to https://<lang>.wikipedia.org/wiki/<slug>
   5. Rewrite document layout for Quill, then restore captured TeX whitespace.
   6. Wrap body contents in a <div> carrying the source <body>'s `dir`/`lang`
      so RTL layout survives `clean-html` (which discards the body element).

   The numbers match the phase comments in the body. Phases 0 and 1 read
   Wikipedia's own class names, so both MUST run before `clean-html`, which
   admits neither `ambox` nor `metadata`.

   Pre: `html` is raw Wikipedia HTML and `lang` identifies its wiki.
   Post: return Quill-compatible HTML for `clean-html` to sanitize.
   Invariant: normalized TeX bodies retain their decoded source text."
  [html lang]
  (let [doc (Jsoup/parse html)
        wiki-link-prefix (str "https://" lang ".wikipedia.org/wiki/")]
    (.prettyPrint (.outputSettings doc) false)
    ;; Phase 0: Strip navigation chrome (navboxes, portalboxes)
    (doseq [nav (.select doc "[role=navigation], nav")]
      (.remove nav))
    ;; Phase 1: Maintenance banners are layout tables. They must become
    ;; blockquotes while their classes still exist — `clean-html` admits neither
    ;; `ambox` nor `metadata`, so afterwards nothing can recognize one.
    (tl/tables->blockquotes! (.body doc) wiki-banner-selector)
    ;; Phase 2: Convert formulas before removing MathML and image classes.
    (let [formula-sources (wikipedia-math->tex! (.body doc))]
      (doseq [math-el (.select doc "math")]
        (.remove math-el))
      (doseq [img (.select doc "img.mwe-math-fallback-image-inline, img.mwe-math-fallback-image-display")]
        (let [alt (.attr img "alt")]
          (when (seq alt)
            (.attr img "alt" (simplify-latex alt)))))
      ;; Phase 3: Images — rewrite protocol-relative URLs
      (doseq [img (.select doc "img[src^=//]")]
        (.attr img "src" (str "https:" (.attr img "src"))))
      ;; Phase 4: Links — rewrite relative hrefs to absolute same-wiki URLs
      (doseq [a (.select doc "a[href^=./]")]
        (let [href (.attr a "href")
              slug (subs href 2)] ;; strip "./"
          (.attr a "href" (str wiki-link-prefix slug))))
      ;; Phase 5: Layout cleanup moves text nodes but collapses their whitespace.
      (dh/rewrite-for-quill! (.body doc))
      (doseq [[^TextNode text-node source] formula-sources]
        (.text text-node source))
      ;; Phase 6: Propagate body dir/lang into a wrapping <div>.
      (let [body (.body doc)
            body-dir  (.attr body "dir")
            body-lang (.attr body "lang")
            inner     (.html body)]
        (str "<div"
             (when (seq body-dir)  (str " dir=\""  body-dir  "\""))
             (when (seq body-lang) (str " lang=\"" body-lang "\""))
             ">" inner "</div>")))))

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
  (if-let [offline (link/offline-message "Wikipedia import")]
    {:success false :error offline}
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
           ;; Preprocessing includes document layout conversion before cleanup.
           ;; Captured TeX survives that pass's whitespace normalization.
           :html (-> (:body resp)
                   (preprocess-wikipedia-html lang)
                   cleaner/clean-html)
           :url page-url
           :source-type "wikipedia"
           :biblio biblio})))))

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

(defn- fetch-generic-url
  "Fetch a generic URL as bytes, classify by Content-Type, and either decode +
   process as HTML or return a binary dispatch shape.
   Pre:  url is a non-blank, safe URL.
   Post: one of:
     - {:success true :title :html :url :source-type :biblio} — HTML processed
     - {:success true :dispatch :pdf|:epub :bytes :filename :url} — binary, caller stages
     - {:success false :error <msg>}"
  [url]
  (if-let [offline (link/offline-message "Web import")]
    {:success false :error offline}
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
              (assoc (pipeline/page->document html-str url) :success true))

            (:pdf :epub)
            {:success true
             :dispatch kind
             :bytes body-bytes
             :filename (ct/derive-filename content-disp url kind)
             :url url}

            :reject
            {:success false :error reject-msg}))))))

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

        ;; Before any request: every web-import path and the bibliography
        ;; refetch reach the network only through this fn.
        (yt/youtube-url? url)
        {:success false :error yt/rejection-message}

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
  (if-let [offline (link/offline-message "Wikipedia lookup")]
    {:error offline}
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
        {:error (str "Wikipedia summary fetch failed (HTTP " status ")")}))))
