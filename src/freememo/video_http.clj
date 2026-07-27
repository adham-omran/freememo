(ns freememo.video-http
  "HTTP surface for video: chunked upload (§4.3) and ranged playback (§4.5).

   Both exist because video breaks assumptions the other blob routes never had
   to face. Upload: a 700 MB file cannot go through a 100 MB request cap, so it
   arrives as an `init`/`chunk`*/`finalize` sequence and each chunk is appended
   into the large object with `lo_write`. Playback: `<video>` seeking requires
   206 Partial Content, and every other blob route in this app answers 200 with
   the whole body.

   The route table in `freememo.api` classifies /api/video/chunk as `:body
   :upload`, so an oversized single chunk is rejected by the middleware and
   never reaches a handler here."
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [freememo.commands :as commands]
   [freememo.db :as db]
   [freememo.largeobj :as lo]
   [freememo.quota :as quota]
   [freememo.storage-meter :as meter]
   [freememo.video :as video]
   [freememo.video-format :refer [format-bytes]]
   [taoensso.telemere :as tel]))

(defn- json-response [status body]
  {:status status
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string body)})

(defn- require-auth [request]
  (get-in request [:session :user-id]))

(defn- quota-error->response [data]
  (case (:reason data)
    :file-too-large (json-response 413 {:success false
                                        :error (str "Video exceeds per-upload limit ("
                                                 (:limit data) " bytes)")
                                        :code "file-too-large"
                                        :limit (:limit data) :incoming (:incoming data)})
    :over-quota (let [free (max 0 (- (or (:limit data) 0) (or (:used data) 0)))]
                  (json-response 507
                    {:success false
                     ;; Name the shortfall, not just the failure — "needs 700 MB,
                     ;; 312 MB free" tells the user how much to delete.
                     :error (str "Not enough storage: this video needs "
                              (format-bytes (:incoming data)) " and only "
                              (format-bytes free) " is free")
                     :code "over-quota"
                     :used (:used data) :limit (:limit data)
                     :incoming (:incoming data)}))
    (json-response 500 {:success false :error "Quota check failed"})))

(defn- body-bytes
  "Raw request body as a byte array. The chunk route posts
   application/octet-stream, which no param middleware consumes, so the stream
   is intact here. Bounded upstream by `wrap-route-body-size`."
  ^bytes [request]
  (when-let [^java.io.InputStream in (:body request)]
    (let [baos (java.io.ByteArrayOutputStream.)]
      (io/copy in baos)
      (.toByteArray baos))))

;; ── Upload ─────────────────────────────────────────────────────────────────

(defn init-handler
  "POST /api/video/init — {filename, mime_type, total_bytes, parent_id?}.

   Reserves the bytes and creates the (empty) large object in one transaction,
   so a rejected init leaves nothing behind (§4.3 3.1.1).

   Post: {:success true :session_id S :lo_oid N :chunk_size N}, or 4xx/507."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [params (:params request)
            filename (or (not-empty (str (get params "filename"))) "video.mp4")
            declared-mime (some-> (get params "mime_type") str str/trim not-empty)
            mime (or (when (video/supported-mime? declared-mime) declared-mime)
                   (video/filename->mime filename))
            total (some-> (get params "total_bytes") str parse-long)
            parent-id (some-> (get params "parent_id") str parse-long)]
        (cond
          (or (nil? total) (not (pos? total)))
          (json-response 400 {:success false :error "Missing or invalid total_bytes"})

          (> total video/max-video-bytes)
          (json-response 413 {:success false
                              :error (str "Video is larger than the "
                                       (format-bytes video/max-video-bytes)
                                       " per-video limit")
                              :code "file-too-large" :limit video/max-video-bytes :incoming total})

          (and parent-id (nil? (db/get-topic-for-user user-id parent-id)))
          (json-response 404 {:success false :error "Playlist not found"})

          :else
          (let [r (db/init-video-upload! user-id filename mime total parent-id)]
            (json-response 200 {:success true
                                :session_id (:session-id r)
                                :chunk_size (min quota/request-max-bytes
                                              (* 8 1024 1024))}))))
      (catch clojure.lang.ExceptionInfo e
        (let [data (ex-data e)]
          (if (quota/quota-error? data)
            (quota-error->response data)
            (do (tel/error! {:id ::video-init} e)
                (json-response 500 {:success false :error "Upload could not be started"})))))
      (catch Exception e
        (tel/error! {:id ::video-init} e)
        (json-response 500 {:success false :error "Upload could not be started"})))
    (json-response 401 {:success false :error "Not authenticated"})))

(defn chunk-handler
  "POST /api/video/chunk?session=S — raw bytes appended to the session's object.

   The session id travels in the query string, not the body, because the body
   IS the payload. Returns the running total so a client can verify or resume."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [session-id (get-in request [:params "session"])
            ^bytes buf (body-bytes request)]
        (cond
          (str/blank? session-id)
          (json-response 400 {:success false :error "Missing session"})

          (or (nil? buf) (zero? (alength buf)))
          (json-response 400 {:success false :error "Empty chunk"})

          :else
          (let [r (db/append-video-chunk! user-id session-id buf (alength buf))]
            (if (:ok r)
              (json-response 200 {:success true :received (:received r) :total (:total r)})
              (json-response (if (= "not-found" (:code r)) 404 400)
                {:success false :error (:error r) :code (:code r)})))))
      (catch Exception e
        (tel/error! {:id ::video-chunk} e)
        (json-response 500 {:success false :error "Chunk upload failed"})))
    (json-response 401 {:success false :error "Not authenticated"})))

(defn finalize-handler
  "POST /api/video/finalize — {session_id}.

   Creates the topic, hands the large object over to it, and kicks off the
   ffmpeg/Whisper pipeline in the background. The pipeline is deliberately not
   awaited: it runs for minutes, and the user should reach a playable video
   immediately."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [session-id (get-in request [:params "session_id"])]
        (if (str/blank? session-id)
          (json-response 400 {:success false :error "Missing session_id"})
          (let [r (db/finalize-video-upload! user-id session-id)]
            (if-not (:ok r)
              (json-response 400 {:success false :error (:error r)})
              (do
                (commands/bump! user-id :import-document)
                (video/start-processing! user-id (:topic-id r))
                (json-response 200 {:success true :doc_id (:topic-id r)}))))))
      (catch Exception e
        (tel/error! {:id ::video-finalize} e)
        (json-response 500 {:success false :error "Could not finish the upload"})))
    (json-response 401 {:success false :error "Not authenticated"})))

(defn abort-handler
  "POST /api/video/abort — {session_id}. Client-driven cancellation (§4.3 3.1.4).
   The sweep would reap the session eventually; this frees the bytes now."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [session-id (get-in request [:params "session_id"])]
        (if (str/blank? session-id)
          (json-response 400 {:success false :error "Missing session_id"})
          (do (db/abort-video-upload! user-id session-id)
              (json-response 200 {:success true}))))
      (catch Exception e
        (tel/error! {:id ::video-abort} e)
        (json-response 500 {:success false :error "Abort failed"})))
    (json-response 401 {:success false :error "Not authenticated"})))

(defn status-handler
  "GET /api/video/status?session=S — the resume cursor (§4.3 3.2).
   A client that lost its connection asks how many bytes we actually hold and
   restarts from there instead of re-sending everything."
  [request]
  (if-let [user-id (require-auth request)]
    (let [session-id (get-in request [:params "session"])
          row (when-not (str/blank? session-id)
                (db/get-video-upload-session user-id session-id))]
      (if row
        (json-response 200 {:success true
                            :received (:received_bytes row)
                            :total (:total_bytes row)})
        (json-response 404 {:success false :error "Upload session not found"})))
    (json-response 401 {:success false :error "Not authenticated"})))

(defn position-handler
  "POST /api/video/position — form-encoded {topic_id, pos_ms}. §4.8 8.7.

   An HTTP route rather than an Electric server call because the write must
   survive teardown: the client fires it with `navigator.sendBeacon` on pause
   AND on unmount, and a beacon is queued by the browser independently of the
   page, where an in-flight Electric effect is cancelled with the frame that
   issued it. Best-effort by nature — a lost resume position costs one scrub.

   Ownership is enforced in the UPDATE's WHERE clause, so a forged topic_id
   writes nothing. Always 200: a beacon has no error channel to report into."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [topic-id (some-> (get-in request [:params "topic_id"]) str parse-long)
            pos-ms (some-> (get-in request [:params "pos_ms"]) str parse-long)]
        (when (and topic-id pos-ms)
          (db/save-video-position! user-id topic-id pos-ms))
        (json-response 200 {:success true}))
      (catch Exception e
        (tel/error! {:id ::video-position} e)
        (json-response 200 {:success false})))
    (json-response 401 {:success false :error "Not authenticated"})))

;; ── Ranged playback (§4.5) ─────────────────────────────────────────────────

(def ^:private range-re #"^bytes=(\d*)-(\d*)$")

(defn parse-range
  "Parse a single-range `Range` header against a known `size`.

   Post: [start end] inclusive and clamped, `:unsatisfiable`, or nil when the
   header is absent or a form we do not serve.

   Only the single-range forms are handled — `bytes=N-`, `bytes=N-M`, and the
   suffix form `bytes=-N`. Multi-range would require a multipart/byteranges
   body; no media element asks for one, and answering 200 with the whole file
   is the correct fallback for anything unparsed."
  [header ^long size]
  (when-let [[_ from to] (some->> header str/trim (re-matches range-re))]
    (let [from* (not-empty from)
          to* (not-empty to)]
      (cond
        ;; bytes=-N — the last N bytes
        (and (nil? from*) to*)
        (let [n (parse-long to*)]
          (if (or (nil? n) (zero? n) (zero? size))
            :unsatisfiable
            [(max 0 (- size n)) (dec size)]))

        (nil? from*) nil

        :else
        (let [start (parse-long from*)
              end (if to* (parse-long to*) (dec size))]
          (cond
            (or (nil? start) (nil? end)) nil
            (>= start size) :unsatisfiable
            :else [start (min end (dec size))]))))))

(defn get-video-handler
  "GET /api/video/:id — the playback route.

   Answers 206 with `Content-Range` for a ranged request and 200 with
   `Accept-Ranges: bytes` otherwise; either way the body streams straight out of
   the large object, so a seek to the middle of a 700 MB file transfers only the
   window asked for.

   Blocked during storage grace (§4.6 6.3) with 402: the transcript, extracts,
   cards and schedule all keep working — only the bytes are withheld.

   Blame: 401 unauthenticated → 302 (matches get-pdf-handler); 404 unknown or
   another user's id; 402 out of credits; 416 unsatisfiable range."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [topic-id (-> (:uri request) (str/split #"/") last parse-long)
            row (when topic-id (db/get-topic-video-for-user user-id topic-id))]
        (cond
          (nil? row)
          {:status 404 :body "Video not found"}

          (meter/playback-blocked? user-id)
          {:status 402
           :headers {"Content-Type" "text/plain"}
           :body "Storage credit exhausted — top up to resume playback"}

          :else
          (let [size (long (:byte_size row))
                parsed (parse-range (get-in request [:headers "range"]) size)
                base {"Content-Type" (or (:mime_type row) "video/mp4")
                      "Accept-Ranges" "bytes"
                      ;; Settled bytes behind a topic id never change (no append
                      ;; path for video), so a long private cache is safe and
                      ;; spares repeated ranged round-trips on replay.
                      ;;
                      ;; While `remux_pending` is set that guarantee does not
                      ;; hold: the pipeline may replace this object with a
                      ;; remuxed MP4 (§12.4 3.1), and a client that cached the
                      ;; pre-remux container would keep playing it for a day —
                      ;; including in browsers that cannot decode it at all.
                      ;; `no-store` for that window costs one re-fetch and keeps
                      ;; the invariant the `max-age` branch rests on true.
                      "Cache-Control" (if (:remux_pending row)
                                        "no-store"
                                        "private, max-age=86400")}]
            (cond
              (= :unsatisfiable parsed)
              {:status 416
               :headers (assoc base "Content-Range" (str "bytes */" size))
               :body ""}

              (nil? parsed)
              {:status 200
               :headers (assoc base "Content-Length" (str size))
               :body (lo/open-range-stream! db/ds (:lo_oid row) 0 size)}

              :else
              (let [[start end] parsed
                    len (inc (- end start))]
                {:status 206
                 :headers (assoc base
                            "Content-Range" (str "bytes " start "-" end "/" size)
                            "Content-Length" (str len))
                 :body (lo/open-range-stream! db/ds (:lo_oid row) start len)})))))
      (catch Exception e
        (tel/error! {:id ::get-video-handler} e)
        {:status 500 :body "Internal server error"}))
    {:status 302 :headers {"Location" "/"} :body ""}))
