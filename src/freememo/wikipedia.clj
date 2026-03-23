(ns freememo.wikipedia
  "Web article fetching — Wikipedia and generic URLs."
  (:require [clj-http.client :as http]
            [freememo.html-cleaner :as cleaner])
  (:import [org.jsoup Jsoup]))

(defn- wikipedia-url? [url]
  (and (string? url)
       (re-find #"(?i)wikipedia\.org/wiki/" url)))

(defn- extract-wiki-title [url]
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
     :html (cleaner/clean-html (:body resp))
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
