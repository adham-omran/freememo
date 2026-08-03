(ns freememo.archive-http
  "HTTP surface for chunked archive upload — the `init` and `finalize` halves
   (plans/supermemo-import-large-archives.md §6.1, §6.4).

   Why this exists: a SuperMemo collection reaches 7.5 GB, and
   `/api/upload-file` reads its whole multipart body into a byte[] before doing
   anything with it. A JVM array is int-indexed, so 2 GiB is the hard ceiling,
   and against the prod heap the practical limit is far below that — an
   oversized upload does not fail the request, it kills the JVM, because
   `-XX:+ExitOnOutOfMemoryError` is set. Video already solved this shape once
   for the same reason, so the archive flow reuses its transport: `init`,
   `chunk`*, `finalize`, with chunk and abort shared through
   `freememo.upload-http`.

   What differs from video is what `finalize` means. Video keeps the large
   object — the bytes ARE the artifact. An archive is a container: `finalize`
   only says which import flow it holds, and the import that follows extracts
   it, stores what it needs, and gives every reserved byte back.

   `finalize` deliberately does NOT import. A 7.5 GB extraction runs for
   minutes, and the import already runs off the request path inside `e/Offload`
   in an Electric service. Moving it onto an HTTP request would put a
   multi-minute job behind a proxy timeout."
  (:require
   [clojure.string :as str]
   [freememo.content-type :as ct]
   [freememo.db :as db]
   [freememo.quota :as quota]
   [freememo.upload-http :as uh :refer [json-response require-auth]]
   [freememo.video-format :refer [format-bytes]]
   [taoensso.telemere :as tel]))

(def max-archive-bytes
  "Hard ceiling on one declared archive upload, independent of the user's quota.

   Default 16 GB. A bound on how much a single bad request can reserve, not a
   product limit — the quota is what gates normal use. Sized above
   `web-import/supermemo-extract-limits`'s total, because an archive that cannot
   possibly extract should be refused at `init` rather than after the user waits
   for the upload."
  (or (some-> (System/getenv "ARCHIVE_MAX_BYTES") parse-long)
    (* 16 1024 1024 1024)))

(def ^:private chunk-bytes
  "Chunk size handed to the client. Bounded by `quota/request-max-bytes`,
   because each chunk is one HTTP request and `wrap-route-body-size` rejects
   anything past that cap before a handler runs."
  (min quota/request-max-bytes (* 8 1024 1024)))

(defn init-handler
  "POST /api/archive/init — {filename, total_bytes}.

   Reserves the bytes and creates the empty large object in one transaction, so
   a rejected init leaves nothing behind.

   Pre:  authenticated; total_bytes > 0 and within `max-archive-bytes`; the
         user's remaining quota covers total_bytes.
   Post: 200 {session_id, chunk_size}, or 400 / 413 / 507. A 507 names the
         shortfall so the user knows how much to free."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [params (:params request)
            filename (or (not-empty (str (get params "filename"))) "collection.zip")
            total (some-> (get params "total_bytes") str parse-long)]
        (cond
          (or (nil? total) (not (pos? total)))
          (json-response 400 {:success false :error "Missing or invalid total_bytes"})

          (> total max-archive-bytes)
          (json-response 413 {:success false
                              :error (str "Archive is larger than the "
                                       (format-bytes max-archive-bytes)
                                       " per-upload limit")
                              :code "file-too-large"
                              :limit max-archive-bytes :incoming total})

          :else
          (let [r (db/init-upload-session! user-id "archive" filename
                    "application/octet-stream" total nil)]
            (json-response 200 {:success true
                                :session_id (:session-id r)
                                :chunk_size chunk-bytes}))))
      (catch clojure.lang.ExceptionInfo e
        (let [data (ex-data e)]
          (if (quota/quota-error? data)
            (uh/quota-error->response "archive" data)
            (do (tel/error! {:id ::archive-init} e)
                (json-response 500 {:success false :error "Upload could not be started"})))))
      (catch Exception e
        (tel/error! {:id ::archive-init} e)
        (json-response 500 {:success false :error "Upload could not be started"})))
    (json-response 401 {:success false :error "Not authenticated"})))

(defn finalize-handler
  "POST /api/archive/finalize — {session_id}. Resolves the import flow.

   Materializes the large object to local disk once and reads entry names off
   it. That read is what the import then reuses through the session's
   `archive_path` cache, so a successful upload reads the object once, not
   twice.

   A rejected archive releases the session here rather than leaving the user to
   abort: an archive that holds no importable flow will never be confirmed, and
   holding its reservation open for the reap TTL would bill them for it.

   Pre:  authenticated; session_id names a complete session the caller owns
         whose flow is 'archive'.
   Post: 200 {session_id, flow, filename} where flow is \"supermemo\" or
         \"repo\", or 400 with the reason. The session survives a 200 — the
         Electric confirm service consumes it."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [session-id (get-in request [:params "session_id"])]
        (if (str/blank? session-id)
          (json-response 400 {:success false :error "Missing session_id"})
          (let [m (db/materialize-upload-archive! user-id session-id)]
            (if-not (:ok m)
              (json-response 400 {:success false :error (:error m)})
              (let [[flow reject-msg] (ct/classify-archive (:filename m) (:file m))]
                (if reject-msg
                  (do (db/release-upload-session! user-id session-id)
                      (json-response 400 {:success false :error reject-msg
                                          :code "invalid-file-type"}))
                  (json-response 200 {:success true
                                      :session_id session-id
                                      :flow (name flow)
                                      :filename (:filename m)})))))))
      (catch Exception e
        (tel/error! {:id ::archive-finalize} e)
        (json-response 500 {:success false :error "Could not finish the upload"})))
    (json-response 401 {:success false :error "Not authenticated"})))
