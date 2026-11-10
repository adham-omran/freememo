(ns freememo.local-http
  "Helpers that the Local Install namespaces share: HTML escape, the page
   shell of the local pages, query-string parsing, and the daemon scheduler.

   NO db or app requires, so tests load it with PostgreSQL stopped."
  (:require [clojure.string :as str])
  (:import [java.net URLDecoder]
           [java.util.concurrent Executors ScheduledExecutorService ThreadFactory]))

(defn esc
  "Post: `s` as HTML text, safe inside an element or a quoted attribute."
  [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;")
    (str/replace ">" "&gt;") (str/replace "\"" "&quot;")))

(defn page
  "Post: a 200 Ring response with the page shell around `body-html`.
         `opts`: `:title` (default \"FreeMemo\", which the gate marks as Local)
         and `:script` (JavaScript text)."
  [body-html & [{:keys [title script] :or {title "FreeMemo"}}]]
  {:status 200
   :headers {"Content-Type" "text/html; charset=utf-8" "Cache-Control" "no-store"}
   :body (str "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
           "<meta name=\"viewport\" content=\"width=device-width\">"
           "<title>" (esc title) "</title>"
           "<style>body{font-family:system-ui,sans-serif;max-width:32rem;margin:5rem auto;padding:0 1rem}"
           "button,.again,.buy{font-size:1rem;padding:.6rem 1.2rem;cursor:pointer}"
           ".err{color:#b00020}</style></head><body>" body-html
           (when script (str "<script>" script "</script>"))
           "</body></html>")})

(defn query-pairs
  "Post: the `[key value]` pairs of `query-string`, decoded. A key with no `=` has a nil value."
  [query-string]
  (for [pair (str/split (or query-string "") #"&")
        :when (not (str/blank? pair))
        :let [[k v] (str/split pair #"=" 2)]]
    [(URLDecoder/decode ^String k "UTF-8") (some-> v (URLDecoder/decode "UTF-8"))]))

(defn query-param
  "Post: the first value of `k` in the query string of `request`, or nil."
  [request k]
  (some (fn [[pk pv]] (when (= k pk) pv)) (query-pairs (:query-string request))))

(defn daemon-scheduler
  "Post: a single-thread scheduled executor whose thread is a daemon named `thread-name`."
  ^ScheduledExecutorService [thread-name]
  (Executors/newSingleThreadScheduledExecutor
    (reify ThreadFactory
      (newThread [_ r] (doto (Thread. r thread-name) (.setDaemon true))))))
