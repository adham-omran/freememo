(ns freememo.largeobj
  "Postgres large-object primitives — the storage substrate for video bytes.

   Why not BYTEA: `bytea` caps at 1 GB and every read materializes the whole
   value in JVM heap. Large objects reach 4 TB, and `lo_read`/`lo_write` are
   seekable, so an HTTP Range request touches only the requested window and a
   700 MB upload is appended one chunk at a time.

   Contract shared by every fn here: large-object descriptors are only valid
   inside a transaction. Callers passing `tx` MUST be inside
   `jdbc/with-transaction`. `open-range-stream!` is the one exception. It takes
   a pool and reads with `lo_get`, which needs no descriptor, one chunk per
   query, so it holds no connection while the client reads.

   Orphan hazard: Postgres does NOT unlink a large object when the row holding
   its OID is deleted. Every deletion path must call `unlink!` explicitly, and
   `freememo.db/purge-orphan-video-objects!` sweeps what slips through."
  (:require
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]
   [freememo.chunk-stream :as chunk-stream])
  (:import
   [java.io InputStream OutputStream]
   [java.sql Connection SQLException]
   [org.postgresql PGConnection]
   [org.postgresql.largeobject LargeObject LargeObjectManager]))

(def ^:const io-buffer-bytes
  "Transfer granularity for streaming reads/writes. 256 KiB is a compromise
   between round-trip count and per-call heap."
  262144)

(defn- ^LargeObjectManager lom
  "Large-object API for a JDBC connection. Unwraps the Hikari proxy."
  [^Connection conn]
  (.getLargeObjectAPI ^PGConnection (.unwrap conn PGConnection)))

(defn create!
  "Allocate an empty large object.
   Pre:  tx is an open transaction connection.
   Post: returns its OID (long); the object exists but is zero-length."
  ^long [^Connection tx]
  (.createLO (lom tx) LargeObjectManager/READWRITE))

(defn unlink!
  "Delete large object `oid`. Idempotent from the caller's perspective: a
   missing OID is swallowed, because every call site is a cleanup path where
   'already gone' is the desired end state.
   Pre:  tx is an open transaction connection.
   Post: no `pg_largeobject` rows remain for `oid`."
  [^Connection tx oid]
  (when oid
    (try
      (.unlink (lom tx) (long oid))
      (catch java.sql.SQLException _ nil))))

(defn size
  "Byte length of large object `oid`, or nil when it does not exist."
  [^Connection tx oid]
  (when oid
    (try
      (with-open [^LargeObject lo (.open (lom tx) (long oid) LargeObjectManager/READ)]
        (.size64 lo))
      (catch java.sql.SQLException _ nil))))

(defn append!
  "Append the first `n` bytes of `buf` to large object `oid`.
   Pre:  tx is an open transaction connection; oid exists; 0 <= n <= (alength buf).
   Post: returns the object's new size in bytes.
   Invariant: writes at the object's current end, so concurrent appenders to
   the same OID would interleave — callers serialize by holding the upload
   session row `FOR UPDATE`."
  ^long [^Connection tx oid ^bytes buf ^long n]
  (with-open [^LargeObject lo (.open (lom tx) (long oid) LargeObjectManager/WRITE)]
    (.seek64 lo 0 LargeObject/SEEK_END)
    (.write lo buf 0 (int n))
    (.size64 lo)))

(defn copy-to-file!
  "Stream large object `oid` into `file`, `io-buffer-bytes` at a time.
   Pre:  tx is an open transaction connection; file is writable.
   Post: file holds the object's bytes; heap high-water is one buffer.
   Returns the number of bytes written."
  ^long [^Connection tx oid ^java.io.File file]
  (with-open [^LargeObject lo (.open (lom tx) (long oid) LargeObjectManager/READ)
              out (java.io.BufferedOutputStream. (java.io.FileOutputStream. file))]
    (let [buf (byte-array io-buffer-bytes)]
      (loop [total 0]
        (let [n (.read lo buf 0 io-buffer-bytes)]
          (if (pos? n)
            (do (.write ^OutputStream out buf 0 n)
                (recur (+ total n)))
            (do (.flush ^OutputStream out) total)))))))

(defn write-file!
  "Replace large object `oid`'s contents with `file`'s bytes.
   Pre:  tx is an open transaction connection; oid exists.
   Post: returns bytes written; the object is truncated to exactly that length."
  ^long [^Connection tx oid ^java.io.File file]
  (with-open [^LargeObject lo (.open (lom tx) (long oid) LargeObjectManager/WRITE)
              in (java.io.BufferedInputStream. (java.io.FileInputStream. file))]
    (.truncate64 lo 0)
    (let [buf (byte-array io-buffer-bytes)]
      (loop [total 0]
        (let [n (.read ^InputStream in buf 0 io-buffer-bytes)]
          (if (pos? n)
            (do (.write lo buf 0 n) (recur (+ total n)))
            total))))))

(def ^:private byte-versions
  "For each table that holds a large-object OID: a SQL expression over `t` that
   changes when the object's bytes change.

   It is not `xmin`. `save-video-position!` updates a `topic_videos` row while
   the video plays, and an `xmin` check would stop playback at each save.

   The one in-place rewrite is `db/replace-video-bytes!`, at the end of a
   remux. It writes the new `byte_size` and sets `remux_pending` to FALSE. The
   flag is not always TRUE before it: a file sent as `video/mp4` that is really
   another container starts with the flag FALSE and is remuxed all the same.
   So `byte_size` is the guard that matters. A remux that gives a file of the
   same size, with the flag already FALSE, does not show. Every other write of
   the bytes gives a new OID. No code rewrites an EPUB in place
   (plans/largeobj-chunked-streaming.md)."
  {"topic_videos" "concat_ws(':', t.lo_oid, t.byte_size, t.remux_pending)"
   "topic_epubs" "concat_ws(':', t.lo_oid, t.byte_size)"})

(defn topic-row
  "The `row` of `open-range-stream!` for the `table` row of `topic-id`."
  [table topic-id]
  {:table table :where "t.topic_id = ?" :params [topic-id]})

(defn open-range-stream!
  "InputStream over `[offset, offset+len)` of the large object that `row`
   holds in its `lo_oid` column.

   `row` is `{:table :where :params}`:
   - `table` is a table of `byte-versions`.
   - `where` is SQL over the alias `t` that selects one row, with `params` for
     its placeholders. `table` and `where` come from code, never from a
     request.
   `topic-row` makes one for `topic_videos` and `topic_epubs`.

   The open reads the object's real size and the row's byte version in one
   statement. It throws when the object is too short to give the range. It
   does not compare the real size with the row's `byte_size`, because a row
   that sync wrote holds the size its client sent.

   Each chunk is one `lo_get` query on a connection that is checked out for
   that query only, in autocommit (`freememo.chunk-stream`). The stream holds
   no connection while the client reads. When the bytes change during the
   transfer (`byte-versions`), or the row or the object is gone, the next read
   throws an IOException. A change between the caller's size read and the open
   does not show. That window is one request long.

   Pre:  0 <= offset and 0 <= len.
   Post: reading the stream yields exactly `len` bytes, then EOF.
   Throws: SQLException at the open when the row or the object is missing, or
           the object has fewer than offset+len bytes."
  ^InputStream [ds {:keys [table where params]} ^long offset ^long len]
  (let [version (or (get byte-versions table)
                    (throw (IllegalArgumentException. (str "No byte version for the table " table))))
        from (format "FROM %s t WHERE %s" table where)
        ;; One statement in autocommit is one transaction, so `lo_open` is
        ;; valid here. 262144 is INV_READ. A missing object makes `lo_open`
        ;; throw.
        found (jdbc/execute-one! ds
                (into [(format "SELECT lo_lseek64(lo_open(t.lo_oid, 262144), 0, 2) AS n, %s AS x %s"
                         version from)]
                  params)
                {:builder-fn rs/as-unqualified-lower-maps})
        chunk-sql (format "SELECT lo_get(t.lo_oid, ?::bigint, ?::int) AS v, %s AS x %s" version from)]
    (cond
      (nil? found)
      (throw (SQLException. (str "No " table " row for this large object")))

      (< (long (:n found)) (+ offset len))
      (throw (SQLException. (str "The large object has " (:n found) " bytes, too few for "
                              len " bytes from byte " offset ".")))

      :else
      (chunk-stream/input-stream ds (fn [o n] (into [chunk-sql o n] params)) offset len (:x found)))))
