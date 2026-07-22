(ns freememo.image-rehost
  "Re-host <img> sources into the media table at web-import time.

   `rehost-html-images!` is the entry point: given fetched HTML and the source
   URL, it absolutizes every relative <img src> against the source URL, then
   downloads each image (SSRF-guarded, throttled, size/count-capped) and
   rewrites the src to /api/media/<id>. Images that can't be re-hosted keep
   their absolutized URL (hotlink fallback) so they still load.

   Throttle: a global Semaphore caps total concurrent outbound image fetches; a
   per-host Semaphore caps concurrency to any single host. Shared across all
   imports, so concurrent imports of the same host stay polite."
  (:require
   [clj-http.client :as http]
   [clojure.string :as str]
   [freememo.db :as db]
   [freememo.url-validate :as url]
   [taoensso.telemere :as tel])
  (:import
   [java.net URI URLDecoder]
   [java.util Base64]
   [java.util.concurrent ConcurrentHashMap Semaphore]
   [java.util.function Function]
   [org.jsoup Jsoup]
   [org.jsoup.nodes Element]))

;; ---------------------------------------------------------------------------
;; Limits
;; ---------------------------------------------------------------------------

(def ^:const max-image-bytes
  "Reject a single image larger than this (bytes)."
  (* 10 1024 1024))

(def ^:const image-timeout-ms
  "Socket and connection timeout per image fetch."
  10000)

(def ^:const max-images-per-import
  "Attempt to re-host at most this many images per imported page."
  50)

(def ^:const max-total-bytes-per-import
  "Stop storing once one import's re-hosted images exceed this total (bytes)."
  (* 50 1024 1024))

(def ^:const per-host-concurrency
  "Max concurrent outbound image fetches to any single host."
  3)

(def ^:const global-concurrency
  "Max concurrent outbound image fetches across all hosts and imports."
  6)

(def ^:private user-agent "Mozilla/5.0 (compatible; FreeMemo/1.0)")

;; ---------------------------------------------------------------------------
;; Throttle — global + per-host semaphores
;; ---------------------------------------------------------------------------

(def ^:private global-sem (Semaphore. global-concurrency true))

(def ^:private host-sems
  "host (String) -> Semaphore(per-host-concurrency)."
  (ConcurrentHashMap.))

(defn- host-sem ^Semaphore [^String host]
  (.computeIfAbsent host-sems host
    (reify Function
      (apply [_ _] (Semaphore. per-host-concurrency true)))))

(defn- with-throttle
  "Run `thunk` holding a global slot and a `host` slot.
   Pre: host is a non-blank string. Post: both slots released.
   Acquire global->host, release host->global (consistent order, no deadlock)."
  [^String host thunk]
  (.acquire global-sem)
  (let [hs (host-sem host)]
    (.acquire hs)
    (try (thunk)
         (finally (.release hs) (.release global-sem)))))

;; ---------------------------------------------------------------------------
;; data: URI decoding
;; ---------------------------------------------------------------------------

(defn- parse-data-uri
  "Parse a data: URI into {:mime-type s :bytes byte-array}, or nil if invalid.
   Pre: caller has checked the src starts with \"data:\"."
  [data-uri]
  (try
    (let [without-scheme (subs data-uri 5)
          comma-idx (str/index-of without-scheme ",")]
      (when comma-idx
        (let [header (subs without-scheme 0 comma-idx)
              data   (subs without-scheme (inc comma-idx))
              [mime-part & flags] (str/split header #";")
              base64? (some #(= "base64" %) flags)
              mime-type (if (str/blank? mime-part) "text/plain" mime-part)]
          (if base64?
            {:mime-type mime-type :bytes (.decode (Base64/getDecoder) data)}
            {:mime-type mime-type
             :bytes (.getBytes (URLDecoder/decode data "UTF-8") "UTF-8")}))))
    (catch Exception e
      (tel/log! {:level :warn :id ::parse-data-uri-failed :data {:error (.getMessage e)}}
        "Failed to parse data URI")
      nil)))

;; ---------------------------------------------------------------------------
;; Remote image fetch — SSRF-guarded, throttled, capped
;; ---------------------------------------------------------------------------

(defn fetch-image!
  "Download an image. Returns {:bytes byte-array :mime-type s} or nil.
   Returns nil unless: url passes safe-url? (SSRF guard), status 200, body
   present, and Content-Type is image/*. Bounded by per-host + global throttle,
   image-timeout-ms, and max-image-bytes (oversize fetch aborts -> nil)."
  [url]
  (when (url/safe-url? url)
    (let [host (try (.getHost (URI. url)) (catch Exception _ nil))]
      (when (and host (not (str/blank? host)))
        (with-throttle host
          (fn []
            (try
              (let [resp (http/get url
                           {:as :byte-array
                            :throw-exceptions false
                            :socket-timeout image-timeout-ms
                            :connection-timeout image-timeout-ms
                            :max-body max-image-bytes
                            :redirect-strategy :none
                            :headers {"User-Agent" user-agent}})
                    ct (some-> (get-in resp [:headers "content-type"])
                         (str/split #";") first str/trim)]
                (when (and (= 200 (:status resp))
                        (:body resp)
                        (string? ct)
                        (str/starts-with? ct "image/"))
                  {:bytes (:body resp) :mime-type ct}))
              (catch Exception e
                (tel/log! {:level :warn :id ::fetch-image-failed
                           :data {:url url :error (.getMessage e)}}
                  "Image fetch failed — skipping")
                nil))))))))

;; ---------------------------------------------------------------------------
;; Per-image: fetch/decode, store, charge byte budget
;; ---------------------------------------------------------------------------

(defn- already-media-url?
  "True when src already points to /api/media/<id>."
  [src]
  (and (string? src) (re-matches #"/api/media/\d+" src)))

(defn- store-image!
  "Fetch/decode `src` for `user-id`, store in media, return /api/media/<id>.
   Returns nil on any failure or when storing would push `budget` (an atom of
   bytes stored so far) past max-total-bytes-per-import.
   Pre: src is a data: URI or an absolute http(s) URL."
  [user-id src budget]
  (try
    (let [{:keys [^bytes bytes mime-type source-url]}
          (cond
            (str/starts-with? src "data:")
            (some-> (parse-data-uri src) (assoc :source-url nil))

            (re-matches #"https?://.*" src)
            (some-> (fetch-image! src) (assoc :source-url src))

            :else nil)]
      (when (and bytes mime-type)
        (let [new-total (swap! budget + (alength bytes))]
          (when (<= new-total max-total-bytes-per-import)
            (str "/api/media/"
              (db/upsert-media! {:user-id user-id
                                 :kind "image"
                                 :bytes bytes
                                 :mime-type mime-type
                                 :source-url source-url}))))))
    (catch Exception e
      (tel/log! {:level :warn :id ::store-image-failed
                 :data {:user-id user-id
                        :src (subs (or src "") 0 (min 120 (count (or src ""))))
                        :error (.getMessage e)}}
        "Failed to store image — keeping hotlink")
      nil)))

;; ---------------------------------------------------------------------------
;; Entry point: rewrite all <img> in a page
;; ---------------------------------------------------------------------------

(defn rehost-html-images!
  "Absolutize and re-host every <img> in `html`.
   Pre  : html is an HTML string (may be blank); base-url is the source page
          URL used as the Jsoup base for resolving relative srcs; user-id owns
          the resulting media.
   Post : returns {:html s :uploaded n :hotlinked n :skipped n}. Every <img>
          src is one of: /api/media/<id> (re-hosted), an absolute URL (hotlink
          fallback), or untouched (already /api/media or unresolvable).
   Invariant: never throws — on internal error returns the input html unchanged."
  [{:keys [html base-url user-id]}]
  (if (str/blank? html)
    {:html html :uploaded 0 :hotlinked 0 :skipped 0}
    (try
      (let [doc (Jsoup/parse html (or base-url ""))
            imgs (vec (.select doc "img[src]"))
            ;; Pass 1 (sequential): set the hotlink floor (absolutized src) and
            ;; collect downloadable [img abs] pairs, skipping already-media srcs.
            work (reduce
                   (fn [acc ^Element img]
                     (let [orig (.attr img "src")]
                       (if (already-media-url? orig)
                         acc
                         (let [abs (if (str/starts-with? orig "data:")
                                     orig
                                     (.absUrl img "src"))]
                           (if (str/blank? abs)
                             acc
                             (do (.attr img "src" abs)
                                 (conj acc [img abs])))))))
                   [] imgs)
            attempt (vec (take max-images-per-import work))
            budget (atom 0)
            ;; Pass 2 (bounded-parallel): download. Concurrency is gated by the
            ;; global + per-host semaphores inside fetch-image!.
            results (mapv deref
                      (mapv (fn [[img abs]]
                              (future [img (store-image! user-id abs budget)]))
                        attempt))
            uploaded (count (filter (fn [[_ m]] (some? m)) results))]
        ;; Pass 3 (sequential): apply re-hosted srcs.
        (doseq [[^Element img media-url] results]
          (when media-url (.attr img "src" media-url)))
        {:html (.html (.body doc))
         :uploaded uploaded
         :hotlinked (- (count work) uploaded)
         :skipped (- (count imgs) (count work))})
      (catch Exception e
        (tel/log! {:level :warn :id ::rehost-failed :data {:error (.getMessage e)}}
          "Image re-host failed — keeping original HTML")
        {:html html :uploaded 0 :hotlinked 0 :skipped 0}))))
