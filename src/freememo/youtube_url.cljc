(ns freememo.youtube-url
  "Recognize a YouTube video URL and read its video id and start time
   (plans/youtube-kind.md 2.2).

   No requires, so a unit test reaches it without a browser or a database, and
   both peers can read it.

   A video id is 11 characters from `[A-Za-z0-9_-]`. That format is a
   convention Google does not document, so the parser and the `topic_youtube`
   CHECK hold the same pattern and nothing else assumes it.

   Accepted forms:
     youtube.com/watch?v=ID       (also www., m., music.)
     youtu.be/ID
     youtube.com/shorts/ID
     youtube.com/embed/ID
     youtube-nocookie.com/embed/ID
   A playlist is out of scope: a URL with both `list=` and `v=` is the video
   alone, and a URL with only `list=` is not a video.")

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
