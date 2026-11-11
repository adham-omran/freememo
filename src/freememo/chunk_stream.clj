(ns freememo.chunk-stream
  "An InputStream over one stored file that reads it from the database one
   chunk at a time, with no connection held between chunks.

   Each chunk is one query on a connection that is checked out for that query
   only. A slow client therefore cannot use up the pool (10 connections) or
   hold a transaction open. `freememo.bytea-stream` uses this for `bytea`
   columns, and `freememo.largeobj/open-range-stream!` for large objects
   (plans/bytea-streaming-memory.md, plans/largeobj-chunked-streaming.md).

   With no transaction, there is no snapshot across chunks. So each chunk query
   also reads a version of the row that holds the file, and the stream compares
   it with the version read at the open. When the version differs, or the row
   is gone, the next read throws an IOException, and the transfer stops short
   of its Content-Length. The client never gets a mix of two versions."
  (:require [next.jdbc :as jdbc])
  (:import [java.io InputStream IOException]
           [java.sql PreparedStatement ResultSet SQLException]
           [org.postgresql PGStatement]))

(def ^:const chunk-bytes
  "Bytes per chunk query. One chunk costs about this much heap."
  (* 1024 1024))

(defn- fetch
  "Run one chunk query. Returns `{:bytes :version}` from its columns `v` and
   `x`, or nil when it gives no row. A threshold of -1 makes the driver use
   binary transfer from the first execution. Text transfer sends hex, which
   costs three times the chunk."
  [ds sql-params]
  (with-open [conn (jdbc/get-connection ds)
              ^PreparedStatement ps (jdbc/prepare conn sql-params)]
    (.setPrepareThreshold ^PGStatement (.unwrap ps PGStatement) -1)
    (with-open [^ResultSet r (.executeQuery ps)]
      (when (.next r)
        {:bytes (.getBytes r "v") :version (.getString r "x")}))))

(defn input-stream
  "An InputStream of the `length` bytes from `start`. It fetches each chunk
   when the reader reaches it.

   `chunk-query` is `(fn [offset n] [sql & params])`. Its query gives the
   bytes `[offset, offset+n)` as column `v` and the row's version as column
   `x`, as text. `version` is the value of `x` at the open."
  ;; `start` and `length` carry no ^long hint: five args with a primitive hint
  ;; are more than the JVM's primitive-arg fn arity allows.
  ^InputStream [ds chunk-query start length version]
  (let [end (+ (long start) (long length))
        offset (volatile! (long start))   ; the offset of the next chunk to fetch
        ^bytes empty (byte-array 0)
        buf (volatile! empty)
        pos (volatile! 0)
        at (fn [] (str ", at byte " @offset " of " end "."))
        fill! (fn []
                (let [{:keys [bytes] v :version}
                      (try (fetch ds (chunk-query @offset (min chunk-bytes (- end @offset))))
                           (catch SQLException e
                             (throw (IOException. (str "The stored file could not be read" (at)) e))))]
                  (when (or (nil? bytes) (not= v version) (zero? (alength ^bytes bytes)))
                    (throw (IOException. (str "The stored file changed or was deleted during the transfer" (at)))))
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
         (when (and (pos? (long len)) (>= @pos (alength ^bytes @buf)) (< @offset end))
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
      (close [] (vreset! buf empty) (vreset! offset end)))))
