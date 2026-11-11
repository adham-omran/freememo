(ns freememo.bytea-stream
  "Stream one stored `bytea` value (`topic_files.file_data`, `media.bytes`) in
   chunks, so a transfer never holds the whole file in the heap.

   Why: a plain SELECT of a `bytea` reads the whole value at once. The driver
   gets it as hex text and decodes it, so one read of a 37.5 MiB file
   allocated 112 MiB. Four routes did this, and concurrent large reads could
   fill the heap (plans/bytea-streaming-memory.md).

   Each chunk is one `substring` query, and its version is the row's `xmin`
   (`freememo.chunk-stream` holds the stream and the version check). Every
   write of an existing `topic_files` row also rewrites `file_data`, and no
   code updates a `media` row, so `xmin` changes only when the file did.

   Videos and EPUBs are large objects. `freememo.largeobj/open-range-stream!`
   streams those."
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.chunk-stream :as chunk-stream]))

(defn- chunk-query [{:keys [table column where params]}]
  (let [sql (format "SELECT substring(t.%s FROM ?::int FOR ?::int) AS v, t.xmin::text AS x FROM %s t WHERE %s"
              column table where)]
    (fn [offset n] (into [sql (inc offset) n] params))))

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
      {:length n :stream (chunk-stream/input-stream ds (chunk-query spec) 0 (long n) x)})))
