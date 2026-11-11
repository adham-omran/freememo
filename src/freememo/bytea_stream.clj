(ns freememo.bytea-stream
  "Stream one stored `bytea` value (`topic_files.file_data`, `media.bytes`) in
   chunks, so a transfer never holds the whole file in the heap.

   Why: a plain SELECT of a `bytea` reads the whole value at once. The driver
   gets it as hex text and decodes it, so one read of a 37.5 MiB file
   allocated 112 MiB. Four routes did this, and concurrent large reads could
   fill the heap (plans/bytea-streaming-memory.md).

   Each chunk is one `substring` query on a connection that is checked out for
   that query only. The stream does not hold a pool connection or a transaction
   while the client reads, so a slow client cannot use up the pool (10
   connections) or hold back vacuum. Each chunk query also reads the row's
   `xmin`. When the row changed after the open (a Live Document writes its
   file again), the next chunk throws an IOException, and the transfer stops
   short of its Content-Length. The client never gets a mix of two versions.

   Videos and EPUBs are large objects. `freememo.largeobj/open-range-stream!`
   streams those."
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [java.io InputStream IOException]
           [java.sql PreparedStatement ResultSet]
           [org.postgresql PGStatement]))

(def ^:const chunk-bytes
  "Bytes per chunk query. One chunk costs about this much heap."
  (* 1024 1024))

(defn- chunk-sql [{:keys [table column where]}]
  (format "SELECT substring(t.%s FROM ?::int FOR ?::int) AS v, t.xmin::text AS x FROM %s t WHERE %s"
    column table where))

(defn- fetch-chunk
  "The `[offset, offset+n)` slice of the value and the row's xmin, or nil when
   the row is gone. A threshold of -1 makes the driver use binary transfer from
   the first execution. Text transfer sends hex, which costs three times the
   chunk."
  [ds spec ^long offset ^long n]
  (with-open [conn (jdbc/get-connection ds)
              ^PreparedStatement ps (jdbc/prepare conn (into [(chunk-sql spec) (inc offset) n] (:params spec)))]
    (.setPrepareThreshold ^PGStatement (.unwrap ps PGStatement) -1)
    (with-open [^ResultSet r (.executeQuery ps)]
      (when (.next r)
        {:bytes (.getBytes r "v") :xmin (.getString r "x")}))))

(defn- chunk-stream
  "An InputStream of `length` bytes that fetches each chunk when the reader
   reaches it."
  ^InputStream [ds spec ^long length xmin]
  (let [offset (volatile! 0)   ; the offset of the next chunk to fetch
        ^bytes empty (byte-array 0)
        buf (volatile! empty)
        pos (volatile! 0)
        fill! (fn []
                (let [{:keys [bytes] x :xmin} (fetch-chunk ds spec @offset (min chunk-bytes (- length @offset)))]
                  (when (or (nil? bytes) (not= x xmin) (zero? (alength ^bytes bytes)))
                    (throw (IOException. (str "The stored file changed or was deleted during the transfer, at byte "
                                           @offset " of " length "."))))
                  (vswap! offset + (alength ^bytes bytes))
                  (vreset! buf bytes)
                  (vreset! pos 0)))]
    (proxy [InputStream] []
      (read
        ([]
         (let [b (byte-array 1)
               n (.read ^InputStream this b 0 1)]
           (if (pos? n) (bit-and (aget b 0) 0xff) -1)))
        ([^bytes b]
         (.read ^InputStream this b 0 (alength b)))
        ([^bytes b off len]
         (when (and (pos? (long len)) (>= @pos (alength ^bytes @buf)) (< @offset length))
           (fill!))
         (let [left (- (alength ^bytes @buf) @pos)]
           (cond
             (zero? (long len)) 0
             (pos? left)
             (let [n (min left (long len))]
               (System/arraycopy @buf @pos b off n)
               (vswap! pos + n)
               n)
             :else -1))))
      (available [] (int (- (alength ^bytes @buf) @pos)))
      (close [] (vreset! buf empty) (vreset! offset length)))))

(defn open!
  "Open a stream over the `bytea` column `column` of the one row of `table`
   that `where` selects. `where` is SQL over the alias `t`, with `params` for
   its placeholders. `table` and `column` come from code, never from a request.

   Returns `{:length n :stream in}`, or nil when no row matches or the value is
   NULL. The open reads only the length. The caller sends the length as
   Content-Length, and Ring closes the stream."
  [ds {:keys [table column where params] :as spec}]
  (when-let [{:keys [n x]} (jdbc/execute-one! ds
                             (into [(format "SELECT octet_length(t.%s) AS n, t.xmin::text AS x FROM %s t WHERE %s"
                                      column table where)]
                               params)
                             {:builder-fn rs/as-unqualified-lower-maps})]
    (when n
      {:length n :stream (chunk-stream ds spec (long n) x)})))
