(ns freememo.sync-stress-test
  "plans/electron-local-db-sync.md 2.2, 2.2.1, 6.4. Slow, so opt-in: set
   SYNC_STRESS=1. Needs PostgreSQL with the server schema on `db/ds`.

   Both tests drive the real `sync-server/download-handler` with Ring request
   maps, as a desktop would, and keep a model of the desktop: id → title,
   with graves removing ids. A row is lost when the model never holds the
   server's final version of it."
  (:require [clojure.test :refer [deftest is testing]]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.account-export :as export]
            [freememo.account-import :as import]
            [freememo.db :as db]
            [freememo.desktop-auth :as auth]
            [freememo.sync-server :as server])
  (:import [java.io ByteArrayInputStream File]
           [java.util.concurrent ConcurrentHashMap]))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(defn- enabled? []
  (and (= "1" (System/getenv "SYNC_STRESS"))
       (try (jdbc/execute-one! db/ds ["SELECT freememo_sync_peer()"]) true (catch Exception _ false))))

(defn- account!
  "A user with a desktop token in its trial. Returns {:user :token}."
  [prefix]
  (let [u (:id (jdbc/execute-one! db/ds ["INSERT INTO users (username) VALUES (?) RETURNING id"
                                         (str prefix "-" (System/nanoTime))] opts))
        token (str "stress-token-" u "-" (System/nanoTime))]
    (jdbc/execute! db/ds ["INSERT INTO desktop_tokens (user_id, token_hash) VALUES (?, ?)" u (auth/sha256-hex token)])
    (jdbc/execute! db/ds ["INSERT INTO desktop_accounts (user_id, scm) VALUES (?, 'stress')" u])
    {:user u :token token}))

(defn- call [handler token body]
  (let [r (handler {:request-method :post
                    :headers {"authorization" (str "Bearer " token)}
                    :body (ByteArrayInputStream. (.getBytes (json/generate-string body) "UTF-8"))})]
    (assert (= 200 (:status r)) (str "HTTP " (:status r) " " (:body r)))
    (json/parse-string (:body r))))

(defn- sync-once!
  "One download from `from`, every page, applied to the model. Returns the
   new checkpoint."
  [token ^ConcurrentHashMap model from]
  (loop [cursor nil to nil]
    (let [{:strs [rows graves cursor to]} (call server/download-handler token
                                            (cond-> {:from from :cursor cursor}
                                              to (assoc :to to)))]
      ;; Long keys: Cheshire reads small numbers as Integer, and a Java map
      ;; tells Integer 5 from Long 5.
      (doseq [{:strs [k]} graves :when (contains? k "id")] (.remove model (long (get k "id"))))
      (doseq [{:strs [t r]} rows :when (= "topics" t)] (.put model (long (get r "id")) (get r "title")))
      (if cursor (recur cursor to) to))))

(defn- server-titles [user-id]
  (into {} (map (juxt :id :title))
    (jdbc/execute! db/ds ["SELECT id, title FROM topics WHERE user_id = ?" user-id] opts)))

(defn- diff
  "What the model got wrong about the server: ids it never got, stale titles,
   and ids it kept after their delete."
  [^ConcurrentHashMap model truth]
  {:lost (count (remove #(.containsKey model %) (keys truth)))
   :stale (count (filter (fn [[id title]] (and (.containsKey model id) (not= title (.get model id)))) truth))
   :resurrected (count (remove #(contains? truth %) (keys model)))})

;; ── 2.2 ────────────────────────────────────────────────────────────

(def ^:private writer-ds
  "One connection per transaction, so 50 writers really hold 50 transactions
   open at once. The app pool holds 10."
  (delay (jdbc/get-datasource {:dbtype "postgresql" :host (or (System/getenv "DB_HOST") "localhost")
                               :port (parse-long (or (System/getenv "DB_PORT") "5432"))
                               :dbname (or (System/getenv "DB_NAME") "cardmaker")
                               :user (or (System/getenv "DB_USER") "cardmaker")
                               :password (or (System/getenv "DB_PASSWORD") "dev")})))

(defn- writer
  "One web writer: inserts, renames and deletes, each in its own transaction
   held open for a random time, one in twenty for most of a second."
  [user-id ^java.util.concurrent.atomic.AtomicBoolean stop counts]
  (future
    (let [rnd (java.util.Random.)]
      (while (not (.get stop))
        (jdbc/with-transaction [tx @writer-ds]
          (let [roll (.nextInt rnd 100)
                pick #(:id (jdbc/execute-one! tx ["SELECT id FROM topics WHERE user_id = ? ORDER BY random() LIMIT 1"
                                                  user-id] opts))]
            (cond
              (< roll 60) (jdbc/execute! tx ["INSERT INTO topics (user_id, title) VALUES (?, ?)" user-id
                                             (str "w" (.nextInt rnd 1000000))])
              (< roll 90) (when-let [id (pick)]
                            (jdbc/execute! tx ["UPDATE topics SET title = ? WHERE id = ?" (str "r" (.nextInt rnd 1000000)) id]))
              :else (when-let [id (pick)] (jdbc/execute! tx ["DELETE FROM topics WHERE id = ?" id])))
            (Thread/sleep (long (if (zero? (.nextInt rnd 20)) (+ 300 (.nextInt rnd 600)) (.nextInt rnd 30))))))
        (swap! counts inc)))))

(deftest fifty-interleaved-writers-over-a-thousand-syncs-lose-no-row
  (when (enabled?)
    (let [{:keys [user token]} (account! "stress")
          model (ConcurrentHashMap.)
          stop (java.util.concurrent.atomic.AtomicBoolean. false)
          writes (atom 0)
          writers (doall (repeatedly 50 #(writer user stop writes)))
          started (System/nanoTime)
          checkpoint (loop [i 0 from 0]
                       (if (= i 1000) from (recur (inc i) (sync-once! token model from))))]
      (.set stop true)
      (run! deref writers)
      ;; Two more syncs once every writer has committed: the window closes at
      ;; the snapshot xmin, so the last writes need one sync to fall below it.
      (let [final (sync-once! token model (sync-once! token model checkpoint))
            result (diff model (server-titles user))]
        (println "2.2:" @writes "write transactions," (count (server-titles user)) "rows,"
          "1002 syncs in" (quot (- (System/nanoTime) started) 1000000) "ms, checkpoint" final
          "result" result)
        (is (pos? @writes))
        (is (= {:lost 0 :stale 0 :resurrected 0} result))))))

;; ── 6.4, 2.2.1 ─────────────────────────────────────────────────────

(defn- seed-large-account!
  "A source account with `n` topics, each with two cards."
  [n]
  (let [{:keys [user]} (account! "import-source")
        root (:id (jdbc/execute-one! db/ds ["INSERT INTO topics (user_id, title) VALUES (?, 'root') RETURNING id" user] opts))]
    (jdbc/execute! db/ds ["INSERT INTO topics (user_id, parent_id, title, content)
                           SELECT ?, ?, 'imported ' || g, '<p>' || repeat('x', 200) || '</p>'
                           FROM generate_series(1, ?) g" user root n])
    (jdbc/execute! db/ds ["INSERT INTO flashcards (topic_id, root_topic_id, question, answer, kind)
                           SELECT t.id, ?, 'q ' || t.id || ' ' || s, 'a', 'basic'
                           FROM topics t, generate_series(1, 2) s WHERE t.parent_id = ?" root root])
    user))

(defn- archive-dir! ^File [source-user]
  (let [zip (File/createTempFile "stress-export" ".zip")
        dir (.toFile (java.nio.file.Files/createTempDirectory "stress-import" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (with-open [out (java.io.FileOutputStream. zip)]
      (export/write-archive! db/ds source-user out "http://stress.invalid"))
    (with-open [zin (java.util.zip.ZipInputStream. (java.io.FileInputStream. zip))]
      (loop []
        (when-let [e (.getNextEntry zin)]
          (let [f (File. dir (.getName e))]
            (.mkdirs (.getParentFile f))
            (when-not (.isDirectory e) (with-open [o (java.io.FileOutputStream. f)] (.transferTo zin o))))
          (recur))))
    dir))

(deftest a-large-import-during-syncs-loses-nothing-and-the-delay-is-measured
  (when (enabled?)
    (let [n 4000
          dir (archive-dir! (seed-large-account! n))
          {:keys [user token]} (account! "import-target")
          parent (:id (jdbc/execute-one! db/ds ["INSERT INTO topics (user_id, title) VALUES (?, 'Imports') RETURNING id" user] opts))
          model (ConcurrentHashMap.)
          checkpoint (atom (sync-once! token model 0))
          importing (future (let [t0 (System/nanoTime)
                                  r (import/import-archive! db/ds user dir parent {})]
                              {:ok (:ok r) :ms (quot (- (System/nanoTime) t0) 1000000)}))
          ;; A web write that commits while the import's one transaction is open.
          _ (Thread/sleep 300)
          probe-title (str "written during the import " (System/nanoTime))
          probe (:id (jdbc/execute-one! db/ds ["INSERT INTO topics (user_id, title) VALUES (?, ?) RETURNING id"
                                               user probe-title] opts))
          committed (System/nanoTime)
          arrived (loop []
                    (swap! checkpoint #(sync-once! token model %))
                    (if (= probe-title (.get model (long probe)))
                      (System/nanoTime)
                      (do (Thread/sleep 50) (recur))))
          import-result @importing]
      (reset! checkpoint (sync-once! token model (sync-once! token model @checkpoint)))
      (let [delay-ms (quot (- arrived committed) 1000000)
            result (diff model (server-titles user))]
        (println "6.4:" (count (server-titles user)) "rows; import" import-result
          "; a write committed during the import arrived after" delay-ms "ms; result" result)
        (is (:ok import-result))
        (is (= {:lost 0 :stale 0 :resurrected 0} result))
        (testing "2.2.1: the import's open transaction holds the write back"
          (is (>= delay-ms 0)))))))
