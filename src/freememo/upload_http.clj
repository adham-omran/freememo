(ns freememo.upload-http
  "Flow-blind half of the chunked upload protocol (plans/incremental-video.md
   §4.3, plans/supermemo-import-large-archives.md §6.1).

   `init` and `finalize` differ per flow — video creates a playable topic and
   keeps the large object, an archive extracts it and gives every reserved byte
   back. Everything between them does not differ at all: appending a chunk,
   asking where to resume, and giving up. Those three live here so the video
   and archive routes share one implementation instead of two that drift.

   The route table in `freememo.api` classifies every `chunk` path as `:body
   :upload`, so an oversized single chunk is rejected by the middleware and
   never reaches `chunk-handler`."
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [freememo.db :as db]
   [freememo.video-format :as vfmt]
   [taoensso.telemere :as tel]))

(defn json-response [status body]
  {:status status
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string body)})

(defn require-auth [request]
  (get-in request [:session :user-id]))

(defn quota-error->response
  "Translate `quota/quota-error` ex-data into the HTTP answer for it.

   Lives here rather than per flow because the status codes and the `code`
   strings are a client contract: the import modal and the video uploader both
   switch on \"over-quota\" and \"file-too-large\". Two copies would let one
   flow's codes drift out from under a shared client. `noun` is the only thing
   that varies — it names what the user tried to upload.

   Pre:  `data` is ex-data from `quota/quota-error`; `noun` is a lower-case
         singular noun.
   Post: a 413 for :file-too-large, a 507 for :over-quota, a 500 otherwise.
         The :over-quota body names the shortfall, because \"needs 700 MB,
         312 MB free\" tells the user how much to delete."
  [noun data]
  (case (:reason data)
    :file-too-large (json-response 413 {:success false
                                        :error (str (str/capitalize noun)
                                                 " exceeds per-upload limit ("
                                                 (:limit data) " bytes)")
                                        :code "file-too-large"
                                        :limit (:limit data) :incoming (:incoming data)})
    :over-quota (let [free (max 0 (- (or (:limit data) 0) (or (:used data) 0)))]
                  (json-response 507
                    {:success false
                     :error (str "Not enough storage: this " noun " needs "
                              (vfmt/format-bytes (:incoming data)) " and only "
                              (vfmt/format-bytes free) " is free")
                     :code "over-quota"
                     :used (:used data) :limit (:limit data)
                     :incoming (:incoming data)}))
    (json-response 500 {:success false :error "Quota check failed"})))

(defn body-bytes
  "Raw request body as a byte array.

   The chunk route posts application/octet-stream, which no param middleware
   consumes, so the stream is intact here. Bounded upstream by
   `wrap-route-body-size`: one chunk is at most `quota/request-max-bytes`, which
   is what keeps this array small no matter how large the whole upload is."
  ^bytes [request]
  (when-let [^java.io.InputStream in (:body request)]
    (let [baos (java.io.ByteArrayOutputStream.)]
      (io/copy in baos)
      (.toByteArray baos))))

(defn chunk-handler
  "POST …/chunk?session=S — raw bytes appended to the session's large object.

   The session id travels in the query string, not the body, because the body
   IS the payload. Returns the running total so a client can verify or resume.

   Pre:  authenticated; `session` names a session the caller owns; the body is
         non-empty and would not push the object past its declared total.
   Post: 200 {received, total}, or 400/404 with a `code` naming which
         precondition failed."
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
          (let [r (db/append-upload-chunk! user-id session-id buf (alength buf))]
            (if (:ok r)
              (json-response 200 {:success true :received (:received r) :total (:total r)})
              (json-response (if (= "not-found" (:code r)) 404 400)
                {:success false :error (:error r) :code (:code r)})))))
      (catch Exception e
        (tel/error! {:id ::upload-chunk} e)
        (json-response 500 {:success false :error "Chunk upload failed"})))
    (json-response 401 {:success false :error "Not authenticated"})))

(defn abort-handler
  "POST …/abort — {session_id}. Client-driven cancellation (§4.3 3.1.4).
   The reap sweep would release the session eventually; this frees the bytes now.

   Pre:  authenticated. An unknown session id is a no-op, not an error — abort
         is what a failing client calls, and it must not fail in turn.
   Post: always 200 on a well-formed request. No large object, no materialized
         archive and no reservation survive for that session."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [session-id (get-in request [:params "session_id"])]
        (if (str/blank? session-id)
          (json-response 400 {:success false :error "Missing session_id"})
          (do (db/release-upload-session! user-id session-id)
              (json-response 200 {:success true}))))
      (catch Exception e
        (tel/error! {:id ::upload-abort} e)
        (json-response 500 {:success false :error "Abort failed"})))
    (json-response 401 {:success false :error "Not authenticated"})))

(defn status-handler
  "GET …/status?session=S — the resume cursor (§4.3 3.2).
   A client that lost its connection asks how many bytes we actually hold and
   restarts from there instead of re-sending everything.

   Pre:  authenticated; `session` names a session the caller owns.
   Post: 200 {received, total}, or 404 when no such session exists for them."
  [request]
  (if-let [user-id (require-auth request)]
    (let [session-id (get-in request [:params "session"])
          row (when-not (str/blank? session-id)
                (db/get-upload-session user-id session-id))]
      (if row
        (json-response 200 {:success true
                            :received (:received_bytes row)
                            :total (:total_bytes row)})
        (json-response 404 {:success false :error "Upload session not found"})))
    (json-response 401 {:success false :error "Not authenticated"})))
