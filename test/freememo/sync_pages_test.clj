(ns freememo.sync-pages-test
  "A parent and its child in different pages or batches
   (plans/electron-local-db-sync.md 2.4, 2.6).

   A download page and an upload batch each retry a missing parent only among
   their own rows. These tests put the parent in a later page:
   - a forked chat, whose fork message downloads after every chat;
   - an offline subtree of 600 topics whose root is read after its children.

   Opt-in, with the setup of `sync-crash-test`: a server JVM on
   SYNC_E2E_ORIGIN over SERVER_DB_NAME, and this JVM as a desktop
   (FREEMEMO_PEER=desktop) on a fresh DB_NAME."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]
            [freememo.desktop-auth :as auth]
            [freememo.desktop-link :as link]
            [freememo.peer :as peer]
            [freememo.sync-client :as client]))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})
(def ^:private origin (System/getenv "SYNC_E2E_ORIGIN"))

(def ^:private server-ds
  (delay (jdbc/get-datasource {:dbtype "postgresql" :host (or (System/getenv "DB_HOST") "localhost")
                               :port (parse-long (or (System/getenv "DB_PORT") "5432"))
                               :dbname (System/getenv "SERVER_DB_NAME")
                               :user "cardmaker" :password (or (System/getenv "DB_PASSWORD") "dev")})))

(defn- one [ds sql & ps] (jdbc/execute-one! ds (into [sql] ps) opts))
(defn- n [ds sql & ps] (:n (apply one ds sql ps)))

(defn- bind-desktop!
  "A server account with a root topic, and this desktop bound to it."
  []
  (let [s @server-ds
        u (:id (one s "INSERT INTO users (username) VALUES (?) RETURNING id" (str "pages-" (System/nanoTime))))
        token (str "pages-token-" u)
        tok (:id (one s "INSERT INTO desktop_tokens (user_id, token_hash) VALUES (?, ?) RETURNING id"
                   u (auth/sha256-hex token)))
        block (auth/allocate-block! s u tok)]
    (one s "INSERT INTO desktop_accounts (user_id, scm) VALUES (?, md5(random()::text)) RETURNING user_id" u)
    (db/setup-schema)
    (one db/ds "INSERT INTO users (id, username) VALUES (?, ?) RETURNING id" u (str "pages-" u))
    (one db/ds "INSERT INTO sync_state (singleton, user_id, server_origin) VALUES (true, ?, ?) RETURNING user_id" u origin)
    (one db/ds "INSERT INTO local_id_blocks VALUES (?, ?, ?) RETURNING block_start" (:start block) (:end block) (:start block))
    (link/configure! {:origin origin :token token :user-id u})
    u))

(deftest a-fork-whose-message-is-pages-later-downloads
  (when (and origin (peer/desktop?))
    (let [s @server-ds
          u (bind-desktop!)
          root (:id (one s "INSERT INTO topics (user_id, title) VALUES (?, 'root') RETURNING id" u))
          parent (:id (one s "INSERT INTO assistant_chats (user_id, root_topic_id, provider_session_id)
                              VALUES (?, ?, md5(random()::text)) RETURNING id" u root))
          _ (jdbc/execute! s ["INSERT INTO assistant_messages (chat_id, role, content)
                               SELECT ?, 'user', 'm' || g FROM generate_series(1, 600) g" parent])
          last-message (:id (one s "SELECT max(id) AS id FROM assistant_messages WHERE chat_id = ?" parent))
          fork (:id (one s "INSERT INTO assistant_chats (user_id, root_topic_id, provider_session_id,
                                                         parent_chat_id, fork_message_id)
                            VALUES (?, ?, md5(random()::text), ?, ?) RETURNING id" u root parent last-message))
          _ (jdbc/execute! s ["INSERT INTO assistant_messages (chat_id, role, content)
                               VALUES (?, 'user', 'in the fork'), (?, 'assistant', 'answer')" fork fork])
          status (client/sync-now!)]
      (is (= :idle (:state status)) (str "the first sync completes: " (:error status)))
      (is (some? (one db/ds "SELECT id FROM assistant_chats WHERE id = ?" fork)) "the forked chat arrived")
      (is (= 602 (n db/ds "SELECT count(*) AS n FROM assistant_messages")) "every message arrived")
      (is (not (:full-sync-required status)) "the sanity check agrees"))))

(deftest a-subtree-whose-root-is-read-last-uploads
  (when (and origin (peer/desktop?))
    (let [u (bind-desktop!)
          _ (client/sync-now!)
          root (:id (one db/ds "INSERT INTO topics (user_id, title) VALUES (?, 'offline root') RETURNING id" u))]
      (jdbc/execute! db/ds ["INSERT INTO topics (user_id, parent_id, title)
                             SELECT ?, ?, 'child ' || g FROM generate_series(1, 600) g" u root])
      ;; An update writes a new row version at the end of the heap, so the
      ;; pending read returns the root after its 600 children.
      (one db/ds "UPDATE topics SET title = 'offline root, renamed' WHERE id = ? RETURNING id" root)
      (let [status (client/sync-now!)]
        (is (= :idle (:state status)) (str "the sync completes: " (:error status)))
        (is (= 601 (n @server-ds "SELECT count(*) AS n FROM topics WHERE user_id = ?" u)) "all 601 topics reached the server")
        (is (= 601 (n db/ds "SELECT count(*) AS n FROM topics")) "none was removed locally")
        (is (zero? (n db/ds "SELECT count(*) AS n FROM sync_conflicts")) "none went to the conflict report")
        (is (zero? (n db/ds "SELECT count(*) AS n FROM topics WHERE usn = -1")) "nothing left pending")))))
