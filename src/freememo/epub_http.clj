(ns freememo.epub-http
  "HTTP surface for chunked EPUB upload — the `init` and `finalize` halves.

   Why this exists: `/api/upload-file` reads its whole multipart body into a
   byte[] before doing anything with it, and one HTTP request is capped at
   `quota/request-max-bytes` (100 MB in production). A large book cleared neither
   bound. Video and archive already solved this shape, so the EPUB flow reuses
   their transport: `init`, `chunk`*, `finalize`, with chunk, abort and status
   shared through `freememo.upload-http`.

   What differs is what `finalize` means. Video keeps the large object — the
   bytes ARE the artifact. An archive is a container, and its import extracts it
   and refunds every reserved byte. An EPUB is BOTH: the chapters become topics,
   and the book itself is kept so bibliography can be re-derived later. So
   finalize validates and classifies only, and the confirm step that follows
   parses the book and hands the SAME large object to the new topic — the bytes
   are never copied.

   `finalize` deliberately does NOT parse. A 600 MB book takes tens of seconds to
   read, and the import already runs off the request path inside `e/Offload` in
   an Electric service. Moving it onto an HTTP request would put that behind a
   proxy timeout. Same division as `archive-http`.

   See plans/large-epub-import.md §7.5."
  (:require
   [clojure.string :as str]
   [freememo.db :as db]
   [freememo.epub :refer [max-epub-bytes] :as epub]
   [freememo.quota :as quota]
   [freememo.upload-http :as uh :refer [json-response require-auth]]
   [freememo.video-format :refer [format-bytes]]
   [taoensso.telemere :as tel]))

(def ^:private chunk-bytes
  "Chunk size handed to the client. Bounded by `quota/request-max-bytes`,
   because each chunk is one HTTP request and `wrap-route-body-size` rejects
   anything past that cap before a handler runs."
  (min quota/request-max-bytes (* 8 1024 1024)))

(defn init-handler
  "POST /api/epub/init — {filename, total_bytes}.

   Reserves the bytes and creates the empty large object in one transaction, so
   a rejected init leaves nothing behind.

   Pre:  authenticated; total_bytes > 0 and within `max-epub-bytes`; the user's
         remaining quota covers total_bytes.
   Post: 200 {session_id, chunk_size}, or 400 / 413 / 507. A 507 names the
         shortfall so the user knows how much to free.
   Invariant: gated by `quota/reserve-bytes!`, never `check-and-bump!` — the
         latter enforces `upload_max_bytes`, which is the single-request ceiling
         that chunking exists to get past."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [params (:params request)
            filename (or (not-empty (str (get params "filename"))) "book.epub")
            total (some-> (get params "total_bytes") str parse-long)]
        (cond
          (or (nil? total) (not (pos? total)))
          (json-response 400 {:success false :error "Missing or invalid total_bytes"})

          (> total max-epub-bytes)
          (json-response 413 {:success false
                              :error (str "EPUB is larger than the "
                                       (format-bytes max-epub-bytes)
                                       " per-upload limit")
                              :code "file-too-large"
                              :limit max-epub-bytes :incoming total})

          :else
          (let [r (db/init-upload-session! user-id "epub" filename
                    "application/epub+zip" total nil)]
            (json-response 200 {:success true
                                :session_id (:session-id r)
                                :chunk_size chunk-bytes}))))
      (catch clojure.lang.ExceptionInfo e
        (let [data (ex-data e)]
          (if (quota/quota-error? data)
            (uh/quota-error->response "EPUB" data)
            (do (tel/error! {:id ::epub-init} e)
                (json-response 500 {:success false :error "Upload could not be started"})))))
      (catch Exception e
        (tel/error! {:id ::epub-init} e)
        (json-response 500 {:success false :error "Upload could not be started"})))
    (json-response 401 {:success false :error "Not authenticated"})))

(defn finalize-handler
  "POST /api/epub/finalize — {session_id}. Validates that the bytes are an EPUB.

   Materializes the large object to local disk once and reads its magic off it.
   That file is cached on the session (`archive_path`), so the confirm step that
   follows reads the object once, not twice.

   A rejected upload releases the session here rather than leaving the user to
   abort: a file that is not an EPUB will never be confirmed, and holding its
   reservation open for the reap TTL would bill them for it.

   Pre:  authenticated; session_id names a complete session the caller owns
         whose flow is 'epub'.
   Post: 200 {session_id, flow \"epub\", filename}, or 400 with the reason.
         The session SURVIVES a 200 — the Electric confirm service consumes it.
   Invariant: this does not parse the book. Only the magic prefix is read."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [session-id (get-in request [:params "session_id"])]
        (if (str/blank? session-id)
          (json-response 400 {:success false :error "Missing session_id"})
          (let [m (db/materialize-upload-session! user-id session-id "epub")]
            (if-not (:ok m)
              (json-response 400 {:success false :error (:error m)})
              (if-not (epub/epub-magic-bytes? (:file m))
                (do (db/release-upload-session! user-id session-id)
                    (json-response 400 {:success false
                                        :error "Not a valid EPUB file"
                                        :code "invalid-file-type"}))
                (json-response 200 {:success true
                                    :session_id session-id
                                    :flow "epub"
                                    :filename (:filename m)}))))))
      (catch Exception e
        (tel/error! {:id ::epub-finalize} e)
        (json-response 500 {:success false :error "Could not finish the upload"})))
    (json-response 401 {:success false :error "Not authenticated"})))
