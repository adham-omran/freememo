(ns freememo.media-migration
  "On-startup idempotent migration: scans all topic content HTML, fetches any
   <img> that isn't already /api/media/<id>, stores bytes into the media table,
   and rewrites the src attribute.

   Entry point: `migrate-images-to-media-table!`

   Wikimedia throttle: shared Semaphore capped at 3 concurrent HTTP requests to
   *.wikimedia.org. Exported as `fetch-with-wikimedia-throttle` so that
   wikipedia.clj can reuse it for new imports."
  (:require
   [freememo.db :as db]
   [taoensso.telemere :as tel]
   [clj-http.client :as http]
   [clojure.string :as str]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]
   [honey.sql :as sql])
  (:import
   [org.jsoup Jsoup]
   [java.util Base64]
   [java.util.concurrent Semaphore]))

;; ---------------------------------------------------------------------------
;; Wikimedia throttle — max 3 concurrent requests to *.wikimedia.org
;; ---------------------------------------------------------------------------

(def ^:private wikimedia-semaphore
  "Semaphore limiting concurrent HTTP requests to *.wikimedia.org to 3."
  (Semaphore. 3 true))

(defn wikimedia-url?
  "True when `url` targets a wikimedia.org host."
  [url]
  (and (string? url)
       (re-find #"(?i)wikimedia\.org" url)))

(defn fetch-with-wikimedia-throttle
  "Fetch `url` bytes via clj-http. For wikimedia.org hosts, acquires the
   3-slot semaphore before requesting and releases it after.
   Returns {:bytes byte-array :mime-type string} or throws on failure."
  [url]
  (let [wikimedia? (wikimedia-url? url)
        _ (when wikimedia?
            (tel/log! {:level :debug :id ::throttle-acquire :data {:url url}}
              "Acquiring Wikimedia throttle slot")
            (.acquire wikimedia-semaphore))]
    (try
      (let [resp (http/get url
                   {:as :byte-array
                    :throw-exceptions false
                    :socket-timeout 30000
                    :connection-timeout 30000
                    :headers {"User-Agent" "FreeMemo/1.0 (image migration)"}})]
        (when wikimedia?
          (tel/log! {:level :debug :id ::throttle-release :data {:url url}}
            "Releasing Wikimedia throttle slot"))
        (if (= 200 (:status resp))
          {:bytes (:body resp)
           :mime-type (or (some-> (get-in resp [:headers "content-type"])
                                  (str/split #";")
                                  first
                                  str/trim)
                          "application/octet-stream")}
          (throw (ex-info (str "HTTP " (:status resp) " fetching " url)
                          {:url url :status (:status resp)}))))
      (finally
        (when wikimedia?
          (.release wikimedia-semaphore))))))

;; ---------------------------------------------------------------------------
;; Data-URI helpers
;; ---------------------------------------------------------------------------

(defn- parse-data-uri
  "Parse a data: URI string into {:mime-type string :bytes byte-array}.
   Returns nil if the URI isn't a valid data: URI."
  [data-uri]
  (when (and (string? data-uri) (str/starts-with? data-uri "data:"))
    (try
      ;; data:[<mediatype>][;base64],<data>
      (let [without-scheme (subs data-uri 5)
            comma-idx (str/index-of without-scheme ",")]
        (when comma-idx
          (let [header (subs without-scheme 0 comma-idx)
                data   (subs without-scheme (inc comma-idx))
                [mime-part & flags] (str/split header #";")
                base64? (some #(= "base64" %) flags)
                mime-type (if (str/blank? mime-part) "text/plain" mime-part)]
            (if base64?
              {:mime-type mime-type
               :bytes (.decode (Base64/getDecoder) data)}
              ;; URL-encoded — decode as UTF-8 bytes
              {:mime-type mime-type
               :bytes (.getBytes (java.net.URLDecoder/decode data "UTF-8") "UTF-8")}))))
      (catch Exception e
        (tel/log! {:level :warn :id ::parse-data-uri-failed :data {:error (.getMessage e)}}
          "Failed to parse data URI")
        nil))))

;; ---------------------------------------------------------------------------
;; Core: rewrite a single <img> src to /api/media/<id>
;; ---------------------------------------------------------------------------

(defn- already-media-url?
  "True when src already points to /api/media/<id>."
  [src]
  (and (string? src)
       (re-matches #"/api/media/\d+" src)))

(defn- process-img!
  "Fetch or decode image at `src` for `user-id`, store in media table, and
   return the new /api/media/<id> src. Returns nil on failure (caller skips)."
  [user-id src]
  (try
    (let [{:keys [^bytes bytes mime-type source-url]}
          (cond
            (str/starts-with? src "data:")
            (let [parsed (parse-data-uri src)]
              (when parsed
                (assoc parsed :source-url nil)))

            (re-matches #"https?://.*" src)
            (let [{:keys [^bytes bytes mime-type]} (fetch-with-wikimedia-throttle src)]
              {:bytes bytes :mime-type mime-type :source-url src})

            :else nil)]
      (when (and bytes mime-type)
        (let [media-id (db/upsert-media!
                         {:user-id   user-id
                          :kind      "image"
                          :bytes     bytes
                          :mime-type mime-type
                          :source-url source-url})]
          (str "/api/media/" media-id))))
    (catch Exception e
      (tel/log! {:level :warn :id ::process-img-failed
                 :data {:user-id user-id :src (subs (or src "") 0 (min 120 (count (or src ""))))
                        :error (.getMessage e)}}
        "Failed to process image — skipping")
      nil)))

;; ---------------------------------------------------------------------------
;; Per-topic rewrite
;; ---------------------------------------------------------------------------

(defn- rewrite-topic-images!
  "Scan topic's HTML content, rewrite all non-media <img> srcs.
   Returns [n-uploaded n-skipped] for that topic."
  [{:keys [topics/id topics/user_id topics/content]}]
  (when (and content (seq content))
    (let [doc (Jsoup/parseBodyFragment content)
          imgs (.select doc "img[src]")
          non-media-imgs (remove #(already-media-url? (.attr % "src")) imgs)]
      (when (seq non-media-imgs)
        (let [counts (mapv (fn [img]
                             (let [src (.attr img "src")
                                   new-src (process-img! user_id src)]
                               (if new-src
                                 (do (.attr img "src" new-src) [:uploaded])
                                 [:skipped])))
                           non-media-imgs)
              uploaded (count (filter #(= [:uploaded] %) counts))
              skipped  (count (filter #(= [:skipped] %) counts))
              new-html (.html (.body doc))]
          ;; Only update if we actually changed something
          (when (pos? uploaded)
            (jdbc/execute-one! db/ds
              (sql/format {:update :topics
                           :set {:content new-html}
                           :where [:= :id id]})))
          [uploaded skipped])))))

;; ---------------------------------------------------------------------------
;; Main entry point
;; ---------------------------------------------------------------------------

(defn migrate-images-to-media-table!
  "Scan every topic with non-null content. For each <img> that is NOT already
   /api/media/<id>: store bytes in `media` table and rewrite src.
   Idempotent — a second run is a no-op.
   Failures on individual images are warned and skipped (does not abort)."
  []
  (tel/log! :info "Starting image-to-media migration")
  (try
    ;; Fetch all topics that have content (no user filter — this is a global migration)
    (let [topics (jdbc/execute! db/ds
                   (sql/format {:select [:id :user_id :content]
                                :from   [:topics]
                                :where  [:and
                                         [:is-not :content nil]
                                         [:<> :content ""]]})
                   {:builder-fn rs/as-unqualified-lower-maps})
          topic-count (count topics)
          ;; Rough image count estimate by scanning for <img in content
          est-imgs (reduce (fn [acc {:keys [content]}]
                             (+ acc (count (re-seq #"<img" (or content "")))))
                           0 topics)]
      (tel/log! {:level :info :id ::migration-start
                 :data {:topic-count topic-count :est-image-count est-imgs}}
        (str "Migration: " topic-count " topics, ~" est-imgs " images to inspect"))

      (let [total-uploaded (atom 0)
            total-skipped  (atom 0)
            processed      (atom 0)]
        (doseq [topic topics]
          (try
            (when-let [[up sk] (rewrite-topic-images! topic)]
              (swap! total-uploaded + up)
              (swap! total-skipped  + sk)
              (let [done (swap! processed inc)]
                (when (zero? (mod done 25))
                  (tel/log! {:level :info :id ::migration-progress
                             :data {:processed done :uploaded @total-uploaded :skipped @total-skipped}}
                    (str "Migration progress: " done "/" topic-count " topics")))))
            (catch Exception e
              (tel/log! {:level :warn :id ::migration-topic-failed
                         :data {:topic-id (:id topic) :error (.getMessage e)}}
                "Migration: topic processing failed — skipping"))))

        (tel/log! {:level :info :id ::migration-complete
                   :data {:uploaded @total-uploaded :skipped @total-skipped :topics topic-count}}
          (str "Migration complete: " @total-uploaded " images uploaded, "
               @total-skipped " skipped"))))
    (catch Exception e
      (tel/log! {:level :error :id ::migration-failed :data {:error (.getMessage e)}}
        "Image migration failed"))))
