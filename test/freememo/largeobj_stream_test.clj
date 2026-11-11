(ns freememo.largeobj-stream-test
  "A video or EPUB goes out in chunks, and no pool connection is held while the
   client reads. Before this, `open-range-stream!` held one of the pool's 10
   connections for the whole transfer, so a few slow downloads could stop the
   site (plans/largeobj-chunked-streaming.md).

   Needs PostgreSQL with the server schema. The tests skip when it is down.
   They insert a user, so point them at a database of your own (`DB_PORT`)."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.connection :as connection]
            [next.jdbc.result-set :as rs]
            [freememo.account-export :as account-export]
            [freememo.account-transfer-format :as fmt]
            [freememo.db :as db]
            [freememo.largeobj :as lo]
            [freememo.sync-server :as server]
            [freememo.video-http :as video-http])
  (:import [com.zaxxer.hikari HikariDataSource]
           [java.io ByteArrayOutputStream InputStream IOException OutputStream]
           [java.security MessageDigest]
           [java.util.zip ZipInputStream]))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1 FROM topic_videos LIMIT 1"]) true (catch Exception _ false)))

;; md5 gives 32 hex characters, which decode to 16 bytes. 229,383 of them make
;; 3,670,128 bytes: three whole chunks of 1 MiB and a part chunk.
(def ^:private repeats 229383)
(def ^:private size (* 16 repeats))

(defn- sha-of ^String [^InputStream in]
  (let [md (MessageDigest/getInstance "SHA-256")
        buf (byte-array 65536)]
    (with-open [in in]
      (loop []
        (let [n (.read in buf)]
          (when (pos? n) (.update md buf 0 n) (recur)))))
    (format "%064x" (BigInteger. 1 (.digest md)))))

(defn- stored-sha
  "SHA-256 of `[offset, offset+len)` of the object, computed by PostgreSQL."
  [oid offset len]
  (:s (jdbc/execute-one! db/ds ["SELECT encode(sha256(lo_get(?::oid, ?::bigint, ?::int)), 'hex') AS s" oid offset len] opts)))

(defn- fixture!
  "A user with a video topic and an EPUB topic. Each holds its own large object
   of `size` bytes. The video's remux is still pending."
  []
  (let [u (:id (jdbc/execute-one! db/ds ["INSERT INTO users (username) VALUES (?) RETURNING id"
                                         (str "lo-stream-" (System/nanoTime))] opts))
        topic! (fn [kind] (:id (jdbc/execute-one! db/ds ["INSERT INTO topics (user_id, title, kind) VALUES (?, ?, ?) RETURNING id"
                                                         u (str "lo " kind) kind] opts)))
        object! (fn [seed] (:o (jdbc/execute-one! db/ds ["SELECT lo_from_bytea(0, decode(repeat(md5(?), ?::int), 'hex'))::bigint AS o"
                                                         seed repeats] opts)))
        video (topic! "video")
        epub (topic! "epub")
        video-oid (object! (str "video" u))
        epub-oid (object! (str "epub" u))]
    (jdbc/execute! db/ds ["INSERT INTO topic_videos (topic_id, lo_oid, byte_size, mime_type, remux_pending)
                           VALUES (?, ?, ?, 'video/mp4', TRUE)" video video-oid size])
    (jdbc/execute! db/ds ["INSERT INTO topic_epubs (topic_id, lo_oid, byte_size) VALUES (?, ?, ?)" epub epub-oid size])
    {:user u :video video :epub epub :video-oid video-oid :epub-oid epub-oid}))

(defn- drop! [{:keys [user]}]
  (jdbc/execute! db/ds ["SELECT lo_unlink(o) FROM (SELECT lo_oid AS o FROM topic_videos v JOIN topics t ON t.id = v.topic_id WHERE t.user_id = ?
                         UNION SELECT lo_oid FROM topic_epubs e JOIN topics t ON t.id = e.topic_id WHERE t.user_id = ?) x
                         WHERE o IN (SELECT oid FROM pg_largeobject_metadata)" user user])
  (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" user]))

(defn- video-row [f] (lo/topic-row "topic_videos" (:video f)))
(defn- epub-row [f] (lo/topic-row "topic_epubs" (:epub f)))

(defn- one-connection-pool ^HikariDataSource []
  (connection/->pool HikariDataSource (assoc db/db-config :maximumPoolSize 1 :connectionTimeout 1000)))

(deftest a-slow-reader-holds-no-connection
  (if-not (db-up?)
    (println "SKIP a-slow-reader-holds-no-connection: no database")
    (let [f (fixture!)]
      (try
        (with-open [pool (one-connection-pool)
                    ^InputStream in (lo/open-range-stream! pool (video-row f) 0 size)]
          (.read in (byte-array 10))
          (is (= 1 (:n (jdbc/execute-one! pool ["SELECT 1 AS n"] opts)))
            "another request gets the pool's one connection while the client reads")
          (.transferTo in (OutputStream/nullOutputStream)))
        (finally (drop! f))))))

(deftest a-large-object-is-read-in-chunks
  (if-not (db-up?)
    (println "SKIP a-large-object-is-read-in-chunks: no database")
    (let [f (fixture!)
          oid (:video-oid f)]
      (try
        (testing "the whole object and ranges at chunk edges and at the end"
          (doseq [[offset len] [[0 size] [0 1] [1048570 20] [1048576 1048576] [(- size 5) 5] [100 (- size 100)]]]
            (is (= (stored-sha oid offset len) (sha-of (lo/open-range-stream! db/ds (video-row f) offset len)))
              (str "offset " offset ", length " len))))
        (testing "an EPUB"
          (is (= (stored-sha (:epub-oid f) 0 size) (sha-of (lo/open-range-stream! db/ds (epub-row f) 0 size)))))
        (testing "a position save during the transfer does not stop it"
          (with-open [^InputStream in (lo/open-range-stream! db/ds (video-row f) 0 size)]
            (.read in (byte-array 10))
            (db/save-video-position! (:user f) (:video f) 5000)
            (is (= (- size 10) (.transferTo in (OutputStream/nullOutputStream))))))
        (testing "a range past the end of the object throws at the open"
          (is (thrown? java.sql.SQLException (lo/open-range-stream! db/ds (video-row f) 0 (inc size)))))
        (testing "a missing row throws at the open"
          (is (thrown? java.sql.SQLException (lo/open-range-stream! db/ds (assoc (video-row f) :params [-1]) 0 1))))
        (testing "the end of a remux with no new bytes stops a transfer once"
          (with-open [^InputStream in (lo/open-range-stream! db/ds (video-row f) 0 size)]
            (.read in (byte-array 10))
            (db/clear-video-remux-pending! (:video f))
            (is (thrown? IOException (.transferTo in (OutputStream/nullOutputStream)))))
          (is (= (stored-sha oid 0 size) (sha-of (lo/open-range-stream! db/ds (video-row f) 0 size)))
            "the next transfer reads the whole file"))
        (testing "a remux during the transfer stops it, also when the flag was already FALSE"
          (let [file (java.io.File/createTempFile "lo-stream" ".mp4")]
            (try
              (spit file "remuxed")
              (with-open [^InputStream in (lo/open-range-stream! db/ds (video-row f) 0 size)]
                (.read in (byte-array 10))
                (db/replace-video-bytes! (:user f) (:video f) file "video/mp4" nil)
                (is (thrown? IOException (.transferTo in (OutputStream/nullOutputStream)))))
              (finally (.delete file)))))
        (testing "a deleted row stops the transfer"
          (with-open [^InputStream in (lo/open-range-stream! db/ds (epub-row f) 0 size)]
            (.read in (byte-array 10))
            (jdbc/execute! db/ds ["DELETE FROM topic_epubs WHERE topic_id = ?" (:epub f)])
            (is (thrown? IOException (.transferTo in (OutputStream/nullOutputStream)))))
          (jdbc/execute! db/ds ["SELECT lo_unlink(?::oid)" (:epub-oid f)]))
        (testing "a missing object throws at the open"
          (jdbc/execute! db/ds ["SELECT lo_unlink(?::oid)" oid])
          (is (thrown? java.sql.SQLException (lo/open-range-stream! db/ds (video-row f) 0 1))))
        (finally (drop! f))))))

(defn- web-request [user uri headers]
  {:request-method :get :uri uri :session {:user-id user} :headers headers})

(deftest the-routes-send-a-large-object
  (if-not (db-up?)
    (println "SKIP the-routes-send-a-large-object: no database")
    (let [f (fixture!)
          oid (:video-oid f)
          uri (str "/api/video/" (:video f))]
      (try
        (testing "GET /api/video, whole"
          (let [r (video-http/get-video-handler (web-request (:user f) uri {}))]
            (is (= 200 (:status r)))
            (is (= (stored-sha oid 0 size) (sha-of (:body r))))))
        (testing "GET /api/video, one range"
          (let [r (video-http/get-video-handler (web-request (:user f) uri {"range" "bytes=1048570-1048589"}))]
            (is (= 206 (:status r)))
            (is (= (stored-sha oid 1048570 20) (sha-of (:body r))))))
        (testing "GET /api/sync/blob, a video and an EPUB"
          (let [blob (fn [t k] (with-redefs [server/with-access (fn [_ _ handler] (handler {:user-id (:user f)} nil nil))]
                                 (server/blob-get-handler {:request-method :get :params {"t" t "k" k "c" "lo_oid"}})))]
            (let [r (blob "topic_videos" (str "{\"topic_id\":" (:video f) "}"))]
              (is (= 200 (:status r)))
              (is (= (str size) (get-in r [:headers "Content-Length"])))
              (is (= (stored-sha oid 0 size) (sha-of (:body r)))))
            (let [r (blob "topic_epubs" (str "{\"topic_id\":" (:epub f) "}"))]
              (is (= (stored-sha (:epub-oid f) 0 size) (sha-of (:body r)))))))
        (testing "the account export"
          (let [out (ByteArrayOutputStream.)
                _ (account-export/write-archive! db/ds (:user f) out "http://localhost")
                want {(fmt/blob-path :topic_videos (:video f) nil) (stored-sha oid 0 size)
                      (fmt/blob-path :topic_epubs (:epub f) nil) (stored-sha (:epub-oid f) 0 size)}
                got (with-open [zin (ZipInputStream. (java.io.ByteArrayInputStream. (.toByteArray out)))]
                      (loop [acc {}]
                        (if-let [e (.getNextEntry zin)]
                          (recur (cond-> acc (contains? want (.getName e))
                                   (assoc (.getName e) (sha-of (proxy [java.io.FilterInputStream] [zin] (close []))))))
                          acc)))]
            (is (= want got))))
        (finally (drop! f))))))
