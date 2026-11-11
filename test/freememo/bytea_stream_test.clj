(ns freememo.bytea-stream-test
  "A stored file goes out in chunks, so a transfer never holds the whole file in
   the heap. Before this, one read of a 37.5 MiB `bytea` allocated 112 MiB at
   once (plans/bytea-streaming-memory.md).

   Each test reads the whole body and records the largest allocation of one
   read call, the open included. A chunk of 1 MiB costs about 1 MiB in binary
   transfer, so 4 MiB is the limit. The old routes allocated 112 MiB in the
   handler call.

   Needs PostgreSQL with the server schema. The tests skip when it is down.
   They insert a user, so point them at a database of your own (`DB_PORT`)."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.api :as api]
            [freememo.bytea-stream :as bytea-stream]
            [freememo.db :as db]
            [freememo.sync-server :as server])
  (:import [java.io InputStream IOException]
           [java.lang.management ManagementFactory]
           [java.security MessageDigest]))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(def ^:private limit (* 4 1024 1024))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1 FROM topic_files LIMIT 1"]) true (catch Exception _ false)))

(defn- allocated ^long []
  (.getThreadAllocatedBytes ^com.sun.management.ThreadMXBean (ManagementFactory/getThreadMXBean)
    (.getId (Thread/currentThread))))

(defn- measure
  "Run `(open)`, which returns an InputStream, and read it to the end.
   Returns the SHA-256 of the bytes, their count, and the largest allocation of
   one step (the open, or one read)."
  [open]
  (let [a0 (allocated)
        ^InputStream in (open)
        worst (volatile! (- (allocated) a0))
        md (MessageDigest/getInstance "SHA-256")
        buf (byte-array 65536)]
    (with-open [in in]
      (loop [total 0]
        (let [a (allocated)
              n (.read in buf)]
          (vswap! worst max (- (allocated) a))
          (if (neg? n)
            {:sha (format "%064x" (BigInteger. 1 (.digest md))) :bytes total :worst @worst}
            (do (.update md buf 0 n) (recur (+ total n)))))))))

(defn- fixture!
  "A user with a PDF topic, an audio topic and a media row. Each holds the same
   37.5 MiB file. Returns the ids and the file's SHA-256."
  []
  (let [u (:id (jdbc/execute-one! db/ds ["INSERT INTO users (username) VALUES (?) RETURNING id"
                                         (str "bytea-stream-" (System/nanoTime))] opts))
        topic! (fn [kind] (:id (jdbc/execute-one! db/ds ["INSERT INTO topics (user_id, title, kind) VALUES (?, ?, ?) RETURNING id"
                                                         u (str "bytea " kind) kind] opts)))
        pdf (topic! "pdf")
        audio (topic! "audio")]
    ;; md5 gives 32 hex characters, which decode to 16 bytes: 37.5 MiB in all.
    (jdbc/execute! db/ds ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
                           SELECT ?, d, octet_length(d), 'application/pdf'
                           FROM (SELECT decode(repeat(md5(?), 75*1024*1024/32), 'hex') AS d) x"
                          pdf (str u)])
    (jdbc/execute! db/ds ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
                           SELECT ?, file_data, file_size, 'audio/mpeg' FROM topic_files WHERE topic_id = ?"
                          audio pdf])
    (let [media (:id (jdbc/execute-one! db/ds ["INSERT INTO media (user_id, kind, bytes, mime_type, sha256, byte_size)
                                                SELECT ?, 'file', file_data, 'application/pdf',
                                                       encode(sha256(file_data), 'hex'), file_size
                                                FROM topic_files WHERE topic_id = ? RETURNING id"
                                               u pdf] opts))
          sha (:s (jdbc/execute-one! db/ds ["SELECT encode(sha256(file_data), 'hex') AS s FROM topic_files WHERE topic_id = ?" pdf] opts))]
      {:user u :pdf pdf :audio audio :media media :sha sha})))

(defn- drop! [{:keys [user]}]
  (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" user]))

(def ^:private size (quot (* 75 1024 1024) 2))

(defn- check [what {:keys [sha bytes worst]} expected-sha]
  (testing what
    (is (= expected-sha sha) "the bytes are the stored file")
    (is (= size bytes))
    (is (< worst limit) (str "no step allocates " limit " bytes or more, got " worst))))

(deftest a-stored-file-is-read-in-chunks
  (if-not (db-up?)
    (println "SKIP a-stored-file-is-read-in-chunks: no database")
    (let [f (fixture!)]
      (try
        (let [where {:table "topic_files" :column "file_data"
                     :where "t.topic_id = ? AND t.role = ?" :params [(:pdf f) "main"]}]
          (is (= size (:length (bytea-stream/open! db/ds where))))
          (check "the helper" (measure #(:stream (bytea-stream/open! db/ds where))) (:sha f))
          (testing "a missing row gives nil"
            (is (nil? (bytea-stream/open! db/ds (assoc where :params [-1 "main"])))))
          (testing "a change to the row during the transfer stops it with an error"
            (let [{:keys [^InputStream stream]} (bytea-stream/open! db/ds where)]
              (with-open [in stream]
                (.read in (byte-array 10))
                (jdbc/execute! db/ds ["UPDATE topic_files SET file_size = file_size WHERE topic_id = ?" (:pdf f)])
                (is (thrown? IOException (.transferTo in (java.io.OutputStream/nullOutputStream))))))))
        (finally (drop! f))))))

(defn- web-request [user uri]
  {:request-method :get :uri uri :session {:user-id user}})

(defn- body-of [response]
  (is (= 200 (:status response)))
  (is (= (str size) (get-in response [:headers "Content-Length"])))
  (:body response))

(deftest the-web-routes-stream-a-stored-file
  (if-not (db-up?)
    (println "SKIP the-web-routes-stream-a-stored-file: no database")
    (let [f (fixture!)]
      (try
        (check "GET /api/pdf"
          (measure #(body-of (api/get-pdf-handler (web-request (:user f) (str "/api/pdf/" (:pdf f)))))) (:sha f))
        (check "GET /api/audio"
          (measure #(body-of (api/get-audio-handler (web-request (:user f) (str "/api/audio/" (:audio f)))))) (:sha f))
        (check "GET /api/media"
          (measure #(body-of (api/get-media-handler (web-request (:user f) (str "/api/media/" (:media f)))))) (:sha f))
        (testing "another account gets no file"
          (is (= 404 (:status (api/get-pdf-handler (web-request -1 (str "/api/pdf/" (:pdf f)))))))
          (is (= 403 (:status (api/get-media-handler (web-request -1 (str "/api/media/" (:media f))))))))
        (finally (drop! f))))))

(deftest the-sync-route-streams-a-stored-file
  (if-not (db-up?)
    (println "SKIP the-sync-route-streams-a-stored-file: no database")
    (let [f (fixture!)
          blob (fn [user t k c]
                 (with-redefs [server/with-access (fn [_ _ handler] (handler {:user-id user} nil nil))]
                   (server/blob-get-handler {:request-method :get :params {"t" t "k" k "c" c}})))
          file-key (:id (jdbc/execute-one! db/ds ["SELECT id FROM topic_files WHERE topic_id = ?" (:pdf f)] opts))]
      (try
        (check "a topic file"
          (measure #(body-of (blob (:user f) "topic_files" (str "{\"id\":" file-key "}") "file_data"))) (:sha f))
        (check "a media row"
          (measure #(body-of (blob (:user f) "media" (str "{\"id\":" (:media f) "}") "bytes"))) (:sha f))
        (testing "another account gets no file"
          (is (= 404 (:status (blob -1 "media" (str "{\"id\":" (:media f) "}") "bytes")))))
        (finally (drop! f))))))
