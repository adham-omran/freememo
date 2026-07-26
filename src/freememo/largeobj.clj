(ns freememo.largeobj
  "Postgres large-object primitives — the storage substrate for video bytes.

   Why not BYTEA: `bytea` caps at 1 GB and every read materializes the whole
   value in JVM heap. Large objects reach 4 TB, and `lo_read`/`lo_write` are
   seekable, so an HTTP Range request touches only the requested window and a
   700 MB upload is appended one chunk at a time.

   Contract shared by every fn here: large-object descriptors are only valid
   inside a transaction. Callers passing `tx` MUST be inside
   `jdbc/with-transaction`; `open-range-stream!` is the one exception — it owns
   its own connection and transaction, released when the returned stream closes.

   Orphan hazard: Postgres does NOT unlink a large object when the row holding
   its OID is deleted. Every deletion path must call `unlink!` explicitly, and
   `freememo.db/purge-orphan-video-objects!` sweeps what slips through."
  (:require
   [next.jdbc :as jdbc])
  (:import
   [java.io InputStream OutputStream]
   [java.sql Connection]
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

(defn- bounded-lo-stream
  "InputStream yielding exactly `len` bytes from an already-positioned
   LargeObject, then EOF. `release` runs once on close."
  ^InputStream [^LargeObject lo ^long len release]
  (let [remaining (volatile! len)
        closed (volatile! false)]
    (proxy [InputStream] []
      (read
        ([]
         (let [b (byte-array 1)
               n (.read ^InputStream this b 0 1)]
           (if (pos? n) (bit-and (aget b 0) 0xff) -1)))
        ([^bytes b]
         (.read ^InputStream this b 0 (alength b)))
        ([^bytes b off l]
         (let [want (min (long l) (long @remaining))]
           (if-not (pos? want)
             -1
             (let [n (.read lo b (int off) (int want))]
               (if (pos? n)
                 (do (vswap! remaining - n) n)
                 -1))))))
      (available [] (int (min (long @remaining) (long Integer/MAX_VALUE))))
      (close []
        (when-not @closed
          (vreset! closed true)
          (release))))))

(defn open-range-stream!
  "InputStream over `[offset, offset+len)` of large object `oid`.

   Unlike every other fn here this one does NOT take a `tx` — it checks out its
   own pooled connection and opens a transaction, because a Ring response body
   is consumed after the handler has returned. Closing the stream commits and
   returns the connection to the pool; Ring's InputStream body protocol closes
   it unconditionally, including on a client disconnect.

   Pre:  `oid` exists; 0 <= offset and offset+len <= its size.
   Post: reading the stream yields exactly `len` bytes, then EOF.
   Throws: SQLException if the object is gone — the caller answers 404."
  ^InputStream [ds oid ^long offset ^long len]
  (let [^Connection conn (jdbc/get-connection ds)]
    (try
      (.setAutoCommit conn false)
      (let [^LargeObject lo (.open (lom conn) (long oid) LargeObjectManager/READ)]
        (.seek64 lo offset LargeObject/SEEK_SET)
        (bounded-lo-stream lo len
          (fn []
            (try (.close lo) (catch Exception _ nil))
            (try (.commit conn) (catch Exception _ nil))
            (try (.setAutoCommit conn true) (catch Exception _ nil))
            (try (.close conn) (catch Exception _ nil)))))
      (catch Throwable t
        (try (.rollback conn) (catch Exception _ nil))
        (try (.setAutoCommit conn true) (catch Exception _ nil))
        (try (.close conn) (catch Exception _ nil))
        (throw t)))))
