(ns freememo.wikipedia
  "Web article fetching — Wikipedia and generic URLs."
  (:require [clj-http.client :as http]
            [clojure.string :as str]
            [freememo.html-cleaner :as cleaner])
  (:import [org.jsoup Jsoup]))

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
   3. Rewrite protocol-relative URLs to https://"
  [html]
  (let [doc (Jsoup/parse html)]
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
    ;; Phase 3: Links — rewrite relative hrefs to absolute Wikipedia URLs
    (doseq [a (.select doc "a[href^=./]")]
      (let [href (.attr a "href")
            slug (subs href 2)] ;; strip "./"
        (.attr a "href" (str "https://en.wikipedia.org/wiki/" slug))))
    (.html (.body doc))))

(defn- wikipedia-url? [url]
  (and (string? url)
       (re-find #"(?i)wikipedia\.org/wiki/" url)))

(defn extract-wiki-title
  "Extract the article title from a Wikipedia URL. Returns nil for non-Wikipedia URLs."
  [url]
  (when-let [m (re-find #"wikipedia\.org/wiki/(.+?)(?:#.*)?$" url)]
    (java.net.URLDecoder/decode (second m) "UTF-8")))

(defn- fetch-wikipedia-by-title [title]
  (let [api-url (str "https://en.wikipedia.org/api/rest_v1/page/html/"
                     (java.net.URLEncoder/encode title "UTF-8"))
        resp (http/get api-url
               {:headers {"User-Agent" "FreeMemo/1.0 (incremental reading app)"
                          "Accept" "text/html"}
                :cookie-policy :none})]
    {:title title
     :html (-> (:body resp) preprocess-wikipedia-html cleaner/clean-html)
     :url (str "https://en.wikipedia.org/wiki/" (java.net.URLEncoder/encode title "UTF-8"))
     :source-type "wikipedia"}))

(defn- fetch-generic-url [url]
  (let [resp (http/get url
               {:headers {"User-Agent" "Mozilla/5.0 (compatible; FreeMemo/1.0)"}
                :cookie-policy :none
                :socket-timeout 15000
                :connection-timeout 10000})
        doc (Jsoup/parse (:body resp))
        title (or (some-> (.select doc "title") (.first) (.text))
                  (some-> (.select doc "h1") (.first) (.text))
                  url)
        ;; Remove nav, footer, sidebar, ads, scripts
        _ (doseq [sel ["nav" "footer" "header" "aside" ".sidebar" ".nav"
                       ".footer" ".header" ".advertisement" ".ad" "#comments"
                       "script" "style" "noscript" "iframe"]]
            (.remove (.select doc sel)))
        ;; Try to get just the main content
        main-content (or (some-> (.select doc "article") (.first) (.html))
                         (some-> (.select doc "main") (.first) (.html))
                         (some-> (.select doc "[role=main]") (.first) (.html))
                         (some-> (.select doc ".content") (.first) (.html))
                         (some-> (.select doc "#content") (.first) (.html))
                         (.html (.body doc)))
        html (cleaner/clean-html main-content)]
    {:title title
     :html html
     :url url
     :source-type "web"}))

(defn fetch-url
  "Fetch a URL and return cleaned HTML. Auto-detects Wikipedia URLs.
   Returns {:success true :title :html :url :source-type} or {:success false :error}."
  [url]
  (try
    (let [result (if (wikipedia-url? url)
                   (fetch-wikipedia-by-title (extract-wiki-title url))
                   (fetch-generic-url url))]
      (assoc result :success true))
    (catch Exception e
      {:success false :error (.getMessage e)})))
