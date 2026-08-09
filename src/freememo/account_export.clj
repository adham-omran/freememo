(ns freememo.account-export
  "Stream one user's whole account out as a transfer archive.

   plans/user-data-export-import.md §5.3. The archive is a ZIP written straight
   onto the HTTP response stream, so an account larger than the heap exports
   without a temp file. `freememo.account-transfer-format` owns the paths and the
   manifest keys. This namespace owns the queries and the writing order.

   **PostgreSQL encodes every row.** Each entity query selects `to_jsonb(t)` and
   the writer emits that text verbatim, one row per line. Nothing is parsed on
   the way out, and `freememo.account-import` hands the same text back to
   `jsonb_populate_record`, which reads the target's own column types. A JVM type
   map between the two would be a third definition of the schema, free to drift
   from both.

   **The manifest is entry 0, so its counts come from `count(*)` first.** An
   importer must be able to refuse a version it cannot read without downloading
   the whole archive, which rules out appending the manifest once the counts are
   known. The counts are therefore advisory: they are read before the rows, and a
   concurrent mutation can move a number between the two reads. Nothing in import
   fails on a mismatch — the import report states what it actually wrote.

   **Blob bytes never travel inside a row.** `to_jsonb` renders `bytea` as a hex
   string, which would multiply a 100 MB PDF by two inside a JSON line, so the
   three blob-bearing tables drop those keys and their bytes become separate
   archive entries."
  (:require [freememo.account-transfer-format :as fmt]
            [freememo.db :as db]
            [freememo.largeobj :as lo]
            [freememo.settings :as settings]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [ring.core.protocols :as ring-protocols]
            [taoensso.telemere :as tel])
  (:import [java.io InputStream OutputStream]
           [java.time Instant]
           [java.time.format DateTimeFormatter]
           [java.util.zip Deflater ZipEntry ZipOutputStream]))

;; ── Scope ──────────────────────────────────────────────────────────
;;
;; `fmt/owned-topics` and `fmt/entity-scopes` live in
;; `freememo.account-transfer-format`, not here: this namespace requires
;; `freememo.db`, whose pool is built at namespace load, so a scope-coverage
;; test that loaded this file could not run without PostgreSQL. This namespace
;; owns everything that touches the database.

(defn- scope-of
  "The scope entry for `table`.
   Pre : `table` is a member of `fmt/entity-tables`.
   Post: the entry map.
   Throws when the precondition fails, so a table added to `entity-tables`
   without a scope cannot silently export zero rows. This is the runtime
   backstop; `account-transfer-format-test/every-exported-table-has-a-scope`
   is what fails first."
  [table]
  (or (get fmt/entity-scopes table)
    (throw (ex-info (str "No export scope for " (pr-str table))
             {:type ::missing-entity-scope :table table}))))

(defn- row-json-expr
  "The `to_jsonb` expression for one row of `table`, minus its blob columns.

   The `::text` on each key is required: PostgreSQL has `jsonb - text`,
   `jsonb - integer` and `jsonb - text[]`, so an untyped literal is ambiguous.
   The caller must wrap the result in `CAST(… AS text)` — appending `::text`
   instead would bind to the last key literal, not to the expression, and the
   query would return a jsonb the writer cannot treat as a string."
  [table]
  (reduce (fn [expr k] (str expr " - '" k "'::text"))
    "to_jsonb(t)"
    (:drop-keys (scope-of table))))

(defn- count-query [table user-id]
  (let [{:keys [where params]} (scope-of table)]
    (into [(str "SELECT count(*) AS n FROM " (name table) " t WHERE " where)]
      (repeat params user-id))))

(defn- rows-query [table user-id]
  (let [{:keys [where params order]} (scope-of table)]
    (into [(str "SELECT CAST(" (row-json-expr table) " AS text) AS row FROM " (name table)
             " t WHERE " where " ORDER BY " order)]
      (repeat params user-id))))

;; ── Counts (§5.1.2) ────────────────────────────────────────────────

(defn entity-counts
  "Row count per exported table.

   Pre : `user-id` identifies an existing user.
   Post: a map from every member of `fmt/entity-tables` to a non-negative long.
         Advisory — see the namespace docstring. A table with no rows is present
         with 0, so a reader can tell \"empty\" from \"this build did not export
         that table\"."
  [ds user-id]
  (into {}
    (map (fn [table]
           [table (long (or (:n (jdbc/execute-one! ds (count-query table user-id))) 0))]))
    fmt/entity-tables))

;; ── Blob inventory ─────────────────────────────────────────────────

(defn- topic-file-blobs
  "Exported topic_files blobs as [{:id :topic_id :role :file_size} …], ordered."
  [ds user-id]
  (jdbc/execute! ds
    [(str "SELECT t.id, t.topic_id, t.role, t.file_size FROM topic_files t"
       " WHERE t.topic_id IN (" fmt/owned-topics ") ORDER BY t.id") user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn- media-blobs
  "Exported media blobs as [{:id :byte_size} …], ordered."
  [ds user-id]
  (jdbc/execute! ds
    ["SELECT t.id, t.byte_size FROM media t WHERE t.user_id = ? ORDER BY t.id" user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn- video-blobs
  "Exported topic_videos blobs as [{:topic_id :lo_oid :byte_size} …], ordered."
  [ds user-id]
  (jdbc/execute! ds
    [(str "SELECT t.topic_id, t.lo_oid, t.byte_size FROM topic_videos t"
       " WHERE t.topic_id IN (" fmt/owned-topics ") ORDER BY t.topic_id") user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn- epub-blobs
  "Exported topic_epubs blobs as [{:topic_id :lo_oid :byte_size} …], ordered.
   Same shape as `video-blobs` and written by the same streaming writer — a
   large object does not care what the book inside it is."
  [ds user-id]
  (jdbc/execute! ds
    [(str "SELECT t.topic_id, t.lo_oid, t.byte_size FROM topic_epubs t"
       " WHERE t.topic_id IN (" fmt/owned-topics ") ORDER BY t.topic_id") user-id]
    {:builder-fn rs/as-unqualified-maps}))

;; ── Writing ────────────────────────────────────────────────────────

(def ^:private copy-buffer-bytes
  "Transfer granularity when moving blob bytes into the archive. Matches
   `largeobj/io-buffer-bytes` and `archive/copy-buffer-bytes`."
  262144)

(defn- write-entry!
  "Open `path`, call `write!` with the stream, close the entry.
   Pre : `zos` has no open entry. Post: the entry is closed."
  [^ZipOutputStream zos ^String path write!]
  (.putNextEntry zos (ZipEntry. path))
  (write! zos)
  (.closeEntry zos))

(defn- write-manifest!
  "Write `manifest-path` as entry 0.
   Pre : `zos` is a fresh stream with no entry written yet.
   Post: the manifest is the archive's first entry, and
         `fmt/validate-manifest` accepts what it holds."
  [^ZipOutputStream zos manifest]
  (write-entry! zos fmt/manifest-path
    (fn [^OutputStream out]
      (.write out (.getBytes (pr-str manifest) "UTF-8")))))

(defn- write-entity-rows!
  "Stream one table's rows into its archive entry, one JSON object per line.

   Pre : `zos` has no open entry.
   Post: the entry exists, holding one line per row in `:order` order. Returns
         the number of lines written, which is what the import report compares
         against the manifest's advisory count.
   Invariant: heap high-water is one row. `jdbc/plan` with `:fetch-size` keeps
         the driver from materializing the result set, so a million-row table
         costs the same as ten."
  ^long [^ZipOutputStream zos ds user-id table]
  (let [newline-bytes (.getBytes "\n" "UTF-8")
        written (volatile! 0)]
    (write-entry! zos (fmt/entity-path table)
      (fn [^OutputStream out]
        (reduce (fn [_ row]
                  (.write out (.getBytes ^String (:row row) "UTF-8"))
                  (.write out newline-bytes)
                  (vswap! written inc)
                  nil)
          nil
          (jdbc/plan ds (rows-query table user-id)
            {:builder-fn rs/as-unqualified-maps :fetch-size 500 :concurrency :read-only
             :cursors :close :result-type :forward-only}))))
    @written))

(defn- write-bytea-blob!
  "Write one `bytea` column's value as an archive entry.

   Pre : `sql` selects exactly one row with one bytea column, given `id`.
   Post: the entry holds those bytes, or nothing is written when the row is gone.
   Invariant: heap high-water is the value's own size. A `bytea` cannot be
         range-read, so unlike a large object it has no streaming path — the
         bound is the biggest single upload the quota permits."
  [^ZipOutputStream zos ds ^String path sql id]
  (when-let [^bytes b (some-> (jdbc/execute-one! ds [sql id]
                                {:builder-fn rs/as-unqualified-maps})
                       vals first)]
    (write-entry! zos path (fn [^OutputStream out] (.write out b)))
    true))

(defn- write-video-blob!
  "Write one large object as an archive entry.

   Pre : `oid` names a live large object of `byte-size` bytes.
   Post: the entry holds exactly those bytes.
   Invariant (§5.3.5): the stream is opened and closed inside this call, so at
         most one large-object connection is held at a time. `open-range-stream!`
         owns its own connection and transaction until the stream closes, and
         opening one per video at once would exhaust the pool."
  ;; byte-size carries no ^long hint: five args plus a primitive hint is more
  ;; than the JVM's primitive-arg fn arity, and the error names the arity rather
  ;; than the hint.
  [^ZipOutputStream zos ds ^String path oid byte-size]
  (write-entry! zos path
    (fn [^OutputStream out]
      (with-open [in ^InputStream (lo/open-range-stream! ds oid 0 (long byte-size))]
        (let [buf (byte-array copy-buffer-bytes)]
          (loop []
            (let [n (.read in buf)]
              (when (pos? n)
                (.write out buf 0 n)
                (recur)))))))))

(defn- write-blobs!
  "Write every blob entry. Returns a count per kind.

   Pre : `zos` has no open entry.
   Post: one entry per live blob, at the path `fmt/blob-path` gives it.
   Invariant: blobs go last, so a truncated download still yields a readable
         metadata prefix — `ZipInputStream` reads local headers in order and does
         not need the central directory."
  [^ZipOutputStream zos ds user-id]
  (.setLevel zos Deflater/NO_COMPRESSION)   ; PDFs, images and video: deflate burns CPU for nothing
  (let [files (reduce (fn [n {:keys [id topic_id role]}]
                        (if (write-bytea-blob! zos ds
                              (fmt/blob-path :topic_files topic_id role)
                              "SELECT file_data FROM topic_files WHERE id = ?" id)
                          (inc n) n))
                0 (topic-file-blobs ds user-id))
        media (reduce (fn [n {:keys [id]}]
                        (if (write-bytea-blob! zos ds
                              (fmt/blob-path :media id nil)
                              "SELECT bytes FROM media WHERE id = ?" id)
                          (inc n) n))
                0 (media-blobs ds user-id))
        videos (reduce (fn [n {:keys [topic_id lo_oid byte_size]}]
                         (write-video-blob! zos ds (fmt/blob-path :topic_videos topic_id nil)
                           lo_oid (long byte_size))
                         (inc n))
                 0 (video-blobs ds user-id))
        epubs (reduce (fn [n {:keys [topic_id lo_oid byte_size]}]
                        (write-video-blob! zos ds (fmt/blob-path :topic_epubs topic_id nil)
                          lo_oid (long byte_size))
                        (inc n))
                0 (epub-blobs ds user-id))]
    {:topic_files files :media media :topic_videos videos :topic_epubs epubs}))

(defn- manifest-for
  "The manifest map for this export.
   Pre : `counts` covers every member of `fmt/entity-tables`.
   Post: a map `fmt/validate-manifest` accepts."
  [user-id counts ^Instant now instance-url]
  {:format-version fmt/format-version
   :export-id (str (java.util.UUID/randomUUID))
   :exported-at (str now)
   :source-instance instance-url
   :source-user-id user-id             ; provenance only; import never reads it
   :counts counts
   :tiers #{:video}})                  ; this build always carries video bytes

(defn write-archive!
  "Write `user-id`'s whole account as a ZIP onto `out`.

   Pre  : `user-id` identifies an existing user. `out` is open. The caller has
          already authorized this user to read this account.
   Post : a complete ZIP is written and `out` is finished, in this order:
          the manifest, one entity file per table in `fmt/entity-tables`, then
          every blob. Returns {:export-id S :counts M :rows M :blobs M}, where
          `:rows` is what was actually written and `:counts` what the manifest
          claimed.
   Invariant: heap high-water is one entity row, or one `bytea` value while
          blobs are being written — never the archive.
   Blame  : a throw part-way leaves a truncated ZIP on `out`. The caller must not
          close the stream on that path, so the adapter can still report."
  [ds user-id ^OutputStream out instance-url]
  (let [counts (entity-counts ds user-id)
        manifest (manifest-for user-id counts (Instant/now) instance-url)
        zos (ZipOutputStream. out)]
    (write-manifest! zos manifest)
    (let [rows (into {} (map (fn [table] [table (write-entity-rows! zos ds user-id table)]))
                 fmt/entity-tables)
          blobs (write-blobs! zos ds user-id)]
      (.finish zos)
      (tel/log! {:level :info :id ::account-exported
                 :data {:user-id user-id :export-id (:export-id manifest)
                        :rows (reduce + (vals rows)) :blobs blobs}}
        "Account export written")
      {:export-id (:export-id manifest) :counts counts :rows rows :blobs blobs})))

;; ── Ring response body (§5.3.1) ────────────────────────────────────

(defrecord AccountArchiveBody [ds user-id instance-url])

(extend-protocol ring-protocols/StreamableResponseBody
  AccountArchiveBody
  (write-body-to-stream [this _response ^OutputStream out]
    ;; Ring closes the stream on the success path only — it leaves it open on a
    ;; throw so the adapter can still report. `write-archive!` therefore does not
    ;; close it either.
    (write-archive! (:ds this) (:user-id this) out (:instance-url this))
    (.close out)))

(def ^:private filename-date
  (.withZone (DateTimeFormatter/ofPattern "yyyy-MM-dd") java.time.ZoneOffset/UTC))

(defn archive-filename
  "Download filename for `now`.
   Post: a name with no path separator and no character a shell or a
         Content-Disposition parser treats specially."
  [^Instant now]
  (str "freememo-export-" (.format filename-date now) ".zip"))

(defn download-handler
  "GET the authenticated user's account archive (§5.3.3).

   Pre  : `request` carries a Ring session. Nothing in the request names the
          account — the session does, so one user can never export another's.
   Post : 200 whose body streams the archive, or 401 when unauthenticated.
          `Content-Type: application/zip` is required, not cosmetic (§5.3.9):
          Jetty's GzipHandler decides by mime type, and a generic type would put
          the archive through the compressor.
   Invariant: no `Content-Length`. The size is unknown until the archive is
          written, so the response is chunked and the browser shows no
          percentage (§5.3.8).
   Blame  : a throw inside the body surfaces as a truncated download, because the
          status and headers are long gone by then. Nothing recoverable happens
          after the first entry."
  [request]
  (if-let [user-id (get-in request [:session :user-id])]
    {:status 200
     :headers {"Content-Type" "application/zip"
               "Content-Disposition" (str "attachment; filename=\""
                                       (archive-filename (Instant/now)) "\"")
               "Cache-Control" "no-store"}
     :body (->AccountArchiveBody db/ds user-id settings/app-base-url)}
    {:status 401 :body "Not authenticated"}))
