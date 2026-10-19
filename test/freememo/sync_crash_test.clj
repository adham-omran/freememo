(ns freememo.sync-crash-test
  "plans/electron-local-db-sync.md 2.3, 6.3: a client stopped at each step of a
   sync resumes with no lost and no duplicated row.

   Opt-in, because it needs two things the suite does not start:
   - a freememo server JVM on SYNC_E2E_ORIGIN (e.g. http://127.0.0.1:8091),
     on the database SERVER_DB_NAME, and
   - this JVM started as a desktop: FREEMEMO_PEER=desktop, with DB_NAME naming
     a fresh, empty database it may set up.

   A stop is an exception thrown at the step boundary. Like a killed process,
   it ends the sync where it stands, and the open transaction rolls back. The
   server keeps whatever it already committed."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]
            [freememo.desktop-auth :as auth]
            [freememo.desktop-link :as link]
            [freememo.peer :as peer]
            [freememo.sync-apply :as apply]
            [freememo.sync-client :as client]
            [freememo.sync-schema :as schema]))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})
(def ^:private origin (System/getenv "SYNC_E2E_ORIGIN"))

(def ^:private server-ds
  (delay (jdbc/get-datasource {:dbtype "postgresql" :host (or (System/getenv "DB_HOST") "localhost")
                               :port (parse-long (or (System/getenv "DB_PORT") "5432"))
                               :dbname (System/getenv "SERVER_DB_NAME")
                               :user "cardmaker" :password (or (System/getenv "DB_PASSWORD") "dev")})))

(defn- one [ds sql & ps] (jdbc/execute-one! ds (into [sql] ps) opts))
(defn- all [ds sql & ps] (jdbc/execute! ds (into [sql] ps) opts))

(defn- bind-desktop!
  "A server account with a token and a block, and this desktop bound to it,
   as sign-in and `desktop/start!` leave it."
  []
  (let [s @server-ds
        u (:id (one s "INSERT INTO users (username) VALUES (?) RETURNING id" (str "crash-" (System/nanoTime))))
        token (str "crash-token-" u)
        tok (:id (one s "INSERT INTO desktop_tokens (user_id, token_hash) VALUES (?, ?) RETURNING id"
                   u (auth/sha256-hex token)))
        block (auth/allocate-block! s u tok)]
    (one s "INSERT INTO desktop_accounts (user_id, scm) VALUES (?, md5(random()::text)) RETURNING user_id" u)
    (db/setup-schema)
    (one db/ds "INSERT INTO users (id, username) VALUES (?, 'crash') RETURNING id" u)
    (one db/ds "INSERT INTO sync_state (singleton, user_id, server_origin) VALUES (true, ?, ?) RETURNING user_id" u origin)
    (one db/ds "INSERT INTO local_id_blocks VALUES (?, ?, ?) RETURNING block_start" (:start block) (:end block) (:start block))
    (link/configure! {:origin origin :token token :user-id u})
    u))

(defn- snapshot
  "The account's content on one peer: id → title, and id → question."
  [ds u]
  {:topics (into {} (map (juxt :id :title)) (all ds "SELECT id, title FROM topics WHERE user_id = ?" u))
   :cards (into {} (map (juxt :id :question))
            (all ds "SELECT f.id, f.question FROM flashcards f JOIN topics t ON t.id = f.root_topic_id
                     WHERE t.user_id = ?" u))})

(defn- work!
  "Changes on both peers, so every step of the next sync has something to do."
  [u round]
  (let [d db/ds s @server-ds
        mine (fn [ds] (mapv :id (all ds "SELECT id FROM topics WHERE user_id = ? ORDER BY id" u)))]
    (dotimes [i 3]
      (let [t (:id (one d "INSERT INTO topics (user_id, title) VALUES (?, ?) RETURNING id" u (str "desk " round "/" i)))]
        (one d "INSERT INTO flashcards (topic_id, root_topic_id, question, answer, kind)
                VALUES (?, ?, ?, 'a', 'basic') RETURNING id" t t (str "desk card " round "/" i))))
    (dotimes [i 2]
      (one s "INSERT INTO topics (user_id, title) VALUES (?, ?) RETURNING id" u (str "web " round "/" i)))
    (let [synced (filter #(< % schema/client-id-floor) (mine d))]
      (when-let [t (first synced)] (one d "UPDATE topics SET title = ? WHERE id = ? RETURNING id" (str "desk edit " round) t))
      (when-let [t (second synced)] (one d "DELETE FROM topics WHERE id = ? RETURNING id" t)))
    (let [desk-made (filter #(>= % schema/client-id-floor) (mine s))]
      (when-let [t (first desk-made)] (one s "UPDATE topics SET title = ? WHERE id = ? RETURNING id" (str "web edit " round) t))
      (when-let [t (second desk-made)] (one s "DELETE FROM topics WHERE id = ? RETURNING id" t)))))

(defn- stop-on-call
  "A replacement for `f` that throws on call number `n`, and runs `f` on
   every other call."
  [f n]
  (let [calls (atom 0)]
    (fn [& args]
      (if (= n (swap! calls inc))
        (throw (ex-info "stopped by the test" {:type ::stopped}))
        (apply f args)))))

(def ^:private stops
  "Each step boundary of `sync-client/run-sync!`, as the var to break."
  [[:after-meta #'client/graves-step! 1]
   [:inside-graves-apply #'apply/apply-graves! 1]
   [:after-upload-commit #'client/settle-upload! 1]
   [:inside-settle #'apply/apply-rows! 1]
   [:inside-download #'apply/apply-rows! 2]
   [:before-checkpoint #'client/record! 2]
   [:inside-sanity #'client/sane? 1]])

(deftest a-sync-stopped-at-each-step-resumes-with-no-lost-or-duplicated-row
  (when (and origin (peer/desktop?))
    (let [u (bind-desktop!)]
      (is (= :idle (:state (client/sync-now!))) "the first sync")
      (doseq [[[step v n] round] (map vector stops (range))]
        (testing (name step)
          (work! u round)
          (let [stopped (with-redefs-fn {v (stop-on-call @v n)} client/sync-now!)
                resumed (client/sync-now!)
                desk (snapshot db/ds u)
                serv (snapshot @server-ds u)]
            (is (= :error (:state stopped)) "the stop ended that sync")
            (is (= :idle (:state resumed)) (str "the next sync completes: " (:error resumed)))
            (is (= (:topics serv) (:topics desk)) "same topics on both peers")
            (is (= (:cards serv) (:cards desk)) "same cards on both peers")
            (is (zero? (:n (one db/ds "SELECT count(*) AS n FROM topics WHERE usn = -1"))) "nothing left pending")
            (is (zero? (:n (one db/ds "SELECT count(*) AS n FROM sync_conflicts"))) "no row was refused")
            (is (= (count (:cards serv)) (count (set (vals (:cards serv))))) "no card duplicated")))))))
