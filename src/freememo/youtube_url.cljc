(ns freememo.youtube-url
  "Which addresses are YouTube, and which video one names
   (plans/youtube-kind.md 2.2).

   Two questions with two answers. `youtube-url?` says the host is YouTube, for
   any path. `parse` says the URL names one video, and reads its id and start
   time. A link that names a video imports as a `youtube` topic. Any other
   YouTube page is refused, because a fetch of it reads only the site footer, in
   the language YouTube picks for the server's IP
   (plans/feedback-activity-biblio-youtube.md).

   Requires only `clojure.string`, so both peers and a unit test load it with
   nothing else.

   A video id is 11 characters from `[A-Za-z0-9_-]`. That format is a
   convention Google does not document, so the parser and the `topic_youtube`
   CHECK hold the same pattern and nothing else assumes it.

   Accepted video forms:
     youtube.com/watch?v=ID       (also www., m., music.)
     youtu.be/ID
     youtube.com/shorts/ID
     youtube.com/embed/ID
     youtube-nocookie.com/embed/ID
   A playlist is out of scope: a URL with both `list=` and `v=` is the video
   alone, and a URL with only `list=` is not a video."
  (:require [clojure.string :as str]))

(def rejection-message
  "This YouTube page cannot be read as text. Paste a link to one video to import it.")

(def ^:private host-re
  ;; Optional "scheme://" or a bare "//", optional userinfo, then the host up
  ;; to a port, path, query or fragment. A scheme only counts with its "//":
  ;; "youtube.com:443/x" is a host and a port, not the scheme "youtube.com".
  #"^(?:(?:[A-Za-z][A-Za-z0-9+.\-]*:)?//)?(?:[^/?#@]*@)?([^/?#:]+)")

(def ^:private youtube-domains
  ["youtube.com" "youtu.be" "youtube-nocookie.com"])

(defn- host-of [url]
  (some-> (re-find host-re (str/trim url))
    second
    str/lower-case
    (str/replace #"\.$" "")))

(defn youtube-url?
  "True when `url`'s host is a YouTube domain or one of its subdomains, for any
   path. Pre: `url` is a string or nil. Post: a boolean; nil and blank are false."
  [url]
  (boolean
    (when-some [host (when (string? url) (host-of url))]
      (some (fn [domain]
              (or (= host domain) (str/ends-with? host (str "." domain))))
        youtube-domains))))

(def ^:private url-re
  #"(?i)^(?:https?://)?(?:(?:www|m|music)\.)?(youtube\.com|youtube-nocookie\.com|youtu\.be)(/[^?#]*)?(\?[^#]*)?(#.*)?$")

(def ^:private id-re #"^[A-Za-z0-9_-]{11}$")

(defn video-id?
  "True when `s` has the shape of a YouTube video id."
  [s]
  (boolean (and (string? s) (re-matches id-re s))))

(defn- query-params
  "The `k=v` pairs of a query or fragment string, as a map of strings. The
   first value of a repeated key wins."
  [s]
  (reduce (fn [m [_ k v]] (if (contains? m k) m (assoc m k v)))
          {}
          (re-seq #"(?:^|[?#&])([^=&#?]+)=([^&#]*)" (or s ""))))

(defn parse-start-ms
  "A YouTube `t=` or `start=` value in ms, or nil.
   Accepts `90`, `90s`, `1m30s` and `1h2m3s`."
  [s]
  (when (string? s)
    (when-let [[_ h m sec] (re-matches #"(?i)^(?:(\d+)h)?(?:(\d+)m)?(?:(\d+)s?)?$" s)]
      (when (or h m sec)
        (let [n (fn [x] (if x #?(:clj (Long/parseLong x) :cljs (js/parseInt x 10)) 0))
              ms (* 1000 (+ (* 3600 (n h)) (* 60 (n m)) (n sec)))]
          (when (pos? ms) ms))))))

(defn parse
  "The video a URL names, or nil when it names none.

   Post: {:video-id S :start-ms N-or-nil}. :start-ms comes from `t=` or
         `start=` in the query, else from `t=` in the fragment."
  [url]
  (when (string? url)
    (when-let [[_ host path query fragment] (re-matches url-re (.trim ^String url))]
      (let [host (.toLowerCase ^String host)
            path (or path "")
            q (query-params query)
            candidate (cond
                        (= host "youtu.be")
                        (second (re-matches #"^/([^/]+)/?$" path))

                        (re-matches #"(?i)^/watch/?$" path)
                        (get q "v")

                        :else
                        (second (re-matches #"(?i)^/(?:shorts|embed)/([^/]+)/?$" path)))]
        (when (video-id? candidate)
          {:video-id candidate
           :start-ms (or (parse-start-ms (get q "t"))
                         (parse-start-ms (get q "start"))
                         (parse-start-ms (get (query-params fragment) "t")))})))))

(defn watch-url
  "The canonical watch URL for `video-id`, starting at `start-ms` when given.
   YouTube's `t=` takes whole seconds, so the start rounds down."
  ([video-id] (watch-url video-id nil))
  ([video-id start-ms]
   (str "https://www.youtube.com/watch?v=" video-id
        (when (and start-ms (pos? start-ms))
          (str "&t=" (quot (long start-ms) 1000) "s")))))


(defn non-video-youtube-url?
  "True when `url` is a YouTube address that names no single video: a channel,
   a playlist, the home page. Web import refuses exactly these.

   Invariant: for any `url`, at most one of `parse` and this is truthy."
  [url]
  (and (youtube-url? url) (nil? (parse url))))
