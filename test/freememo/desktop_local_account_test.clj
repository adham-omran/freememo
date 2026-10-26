(ns freememo.desktop-local-account-test
  "plans/electron-local-db-sync.md 15.2, 15.3.2, 15.5.3-15.5.5, 15.5.9,
   15.6.2: the desktop half of a local account.

   Desktop peer. Run with FREEMEMO_PEER=desktop and DB_NAME set to a desktop
   test database whose name starts with fm_desktop, set up by `db/setup-schema`
   under the same variables. Every test deletes that database's accounts, so
   the tests skip on any other database."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.account-transfer-format :as fmt]
            [freememo.db :as db]
            [freememo.desktop :as desktop]
            [freememo.desktop-link :as link]
            [freememo.peer :as peer]
            [freememo.sync-apply :as apply]
            [freememo.sync-client :as sync-client]
            [freememo.sync-schema :as schema]))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(defn- one [sql & params] (jdbc/execute-one! db/ds (into [sql] params) opts))
(defn- all [sql & params] (jdbc/execute! db/ds (into [sql] params) opts))

(defn- desktop-test-db? []
  (try (and (peer/desktop?)
            (str/starts-with? (:d (one "SELECT current_database() AS d")) "fm_desktop"))
       (catch Exception _ false)))

(defn- wipe!
  "Delete every account, block and sync record, as on a new install."
  []
  (apply/with-apply-tx db/ds
    (fn [tx]
      ;; Children first, because not every foreign key cascades.
      (doseq [table (reverse fmt/synced-tables)]
        (jdbc/execute! tx [(str "DELETE FROM " (name table))]))
      (doseq [sql ["DELETE FROM users" "DELETE FROM sync_conflicts" "DELETE FROM sync_graves"
                   "DELETE FROM local_id_blocks" "DELETE FROM sync_state"]]
        (jdbc/execute! tx [sql])))))

(use-fixtures :each
  (fn [t]
    (when (desktop-test-db?)
      (wipe!)
      (link/configure! {:origin nil :token nil :user-id desktop/local-user-id})
      (try (t) (finally (link/configure! {:origin nil :token nil :user-id nil}))))))

(def ^:private bind! #'desktop/bind-account!)

(def ^:private local-config {"user_id" 0 "local" true "launch_secret" "s"})

(defn- start-local!
  "A local account as its first launch leaves it: row 0 and one offline block."
  []
  (bind! local-config)
  (sync-client/ensure-id-blocks!))

(defn- topic! [user-id title & {:keys [parent]}]
  (:id (one "INSERT INTO topics (user_id, parent_id, title, content) VALUES (?, ?, ?, '<p>c</p>') RETURNING id"
         user-id parent title)))

(def ^:private server-block
  {"start" schema/client-id-floor "end" (+ schema/client-id-floor schema/block-size)})

(defn- link-config [user-id]
  {"user_id" user-id "username" (str "acct-" user-id) "origin" "https://freememo.test"
   "token" "t" "launch_secret" "s" "link_local" true "id_block" server-block})

(deftest a-local-account-starts-with-row-0-and-an-offline-block
  (when (desktop-test-db?)
    (start-local!)
    (testing "15.2.4: row 0 with a random username, and no origin"
      (is (str/starts-with? (:username (one "SELECT username FROM users WHERE id = 0")) "local-"))
      (is (= {:user_id 0 :server_origin nil} (one "SELECT user_id, server_origin FROM sync_state"))))
    (testing "15.3.1: one offline block, and new rows take ids from it"
      (let [[b :as blocks] (all "SELECT block_start, block_end, origin FROM local_id_blocks")
            id (topic! 0 "T")]
        (is (= 1 (count blocks)))
        (is (= "offline" (:origin b)))
        (is (<= schema/offline-id-floor (:block_start b)))
        (is (<= (:block_start b) id) "a topic id in the block")
        (is (< id (:block_end b)))))
    (testing "a second launch keeps the same account"
      (let [username (:username (one "SELECT username FROM users WHERE id = 0"))]
        (bind! local-config)
        (is (= username (:username (one "SELECT username FROM users WHERE id = 0"))))))
    (testing "15.3.2: below 20 % left, a second offline block, with no network"
      (jdbc/execute! db/ds ["UPDATE local_id_blocks SET next_id = block_start + 850000"])
      (sync-client/ensure-id-blocks!)
      (is (= ["offline" "offline"] (mapv :origin (all "SELECT origin FROM local_id_blocks")))))
    (testing "15.2.2: the status says local, and Sync now does nothing"
      (is (= :local (:state (sync-client/sync-now!)))))))

(deftest the-bound-account-decides-what-a-start-does
  (when (desktop-test-db?)
    (testing "15.5.5: a local database refuses an account without link_local"
      (start-local!)
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"another FreeMemo account"
            (bind! (dissoc (link-config 42) "link_local")))))
    (testing "an account's database starts with that account and refuses another"
      (wipe!)
      (bind! (dissoc (link-config 42) "link_local"))
      (bind! (dissoc (link-config 42) "link_local"))
      (is (= 42 (:user_id (one "SELECT user_id FROM sync_state"))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"another FreeMemo account"
            (bind! (dissoc (link-config 43) "link_local")))))
    (testing "an account's database refuses to start as a local account"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"another FreeMemo account"
            (bind! local-config))))))

(deftest a-re-key-moves-the-library-and-keeps-each-row-pending
  (when (desktop-test-db?)
    (start-local!)
    (let [t (topic! 0 "Kept")
          _ (jdbc/execute! db/ds ["INSERT INTO settings (user_id, key, value) VALUES (0, 'last_document', ?)" (str t)])
          before (one "SELECT usn, mtime FROM topics WHERE id = ?" t)
          setting-before (one "SELECT mtime FROM settings WHERE user_id = 0 AND key = 'last_document'")]
      (bind! (link-config 4242))
      (testing "15.5.3: no column holds user id 0, and row 0 is gone"
        (is (= [] (for [[tbl col] (desktop/user-columns db/ds)
                        :when (pos? (:n (one (format "SELECT count(*) AS n FROM %s WHERE %s = 0" tbl col))))]
                    [tbl col])))
        (is (nil? (one "SELECT 1 FROM users WHERE id = 0")))
        (is (= 4242 (:user_id (one "SELECT user_id FROM topics WHERE id = ?" t)))))
      (testing "every row keeps its usn and mtime, so the merge uploads it and judges it by its own time"
        (is (= before (one "SELECT usn, mtime FROM topics WHERE id = ?" t)))
        (is (= -1 (:usn before)))
        (is (= setting-before (one "SELECT mtime FROM settings WHERE user_id = 4242 AND key = 'last_document'"))))
      (testing "the merge is pending, the offline blocks are closed, and new rows take server ids"
        (is (= {:user_id 4242 :server_origin "https://freememo.test" :merge_pending true :checkpoint 0}
              (one "SELECT user_id, server_origin, merge_pending, checkpoint FROM sync_state")))
        (is (every? #(= (:next_id %) (:block_end %))
              (all "SELECT next_id, block_end FROM local_id_blocks WHERE origin = 'offline'")))
        (let [id (topic! 4242 "New")]
          (is (<= (get server-block "start") id))
          (is (< id (get server-block "end"))))))))

(deftest a-re-key-that-misses-a-column-changes-nothing
  (when (desktop-test-db?)
    (start-local!)
    (let [t (topic! 0 "Kept")
          all-columns desktop/user-columns]
      (with-redefs [desktop/user-columns (fn [conn] (remove #{["topics" "user_id"]} (all-columns conn)))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"reference the local account"
              (bind! (link-config 4242)))))
      (testing "15.5.3 step 3, 15.5.6: the count stopped the delete, and the transaction rolled back"
        (is (= 0 (:user_id (one "SELECT user_id FROM topics WHERE id = ?" t))))
        (is (some? (one "SELECT 1 FROM users WHERE id = 0")))
        (is (nil? (one "SELECT 1 FROM users WHERE id = 4242")))
        (is (= {:user_id 0 :merge_pending false} (one "SELECT user_id, merge_pending FROM sync_state"))))
      (testing "the local account still starts"
        (bind! local-config)
        (is (= 0 (:user_id (one "SELECT user_id FROM sync_state"))))))))

(deftest the-re-key-covers-every-user-id-column
  (when (desktop-test-db?)
    (testing "15.5.4"
      (let [named (set (map (juxt :table_name :column_name)
                         (all "SELECT table_name, column_name FROM information_schema.columns
                               WHERE table_schema = 'public' AND column_name = 'user_id'
                                 AND table_name IN (SELECT tablename FROM pg_tables WHERE schemaname = 'public')")))]
        (is (= #{} (set/difference named (set (desktop/user-columns db/ds)))))))))

(deftest a-refused-block-is-renumbered-with-its-references
  (when (desktop-test-db?)
    (start-local!)
    (let [{:keys [block_start block_end]} (one "SELECT block_start, block_end FROM local_id_blocks")
          parent (topic! 0 "Parent")
          child (topic! 0 "Child" :parent parent)
          _ (jdbc/execute! db/ds ["INSERT INTO settings (user_id, key, value) VALUES (0, ?, '3')" (str "last_page_" parent)])
          _ (jdbc/execute! db/ds ["UPDATE local_id_blocks SET next_id = block_end"])
          _ (jdbc/execute! db/ds ["INSERT INTO local_id_blocks (block_start, block_end, next_id, origin)
                                   VALUES (?, ?, ?, 'server')"
                                  (get server-block "start") (get server-block "end") (get server-block "start")])
          moved (apply/with-apply-tx db/ds #(apply/renumber-block! % {:start block_start :end block_end}))
          new-parent (:id (one "SELECT id FROM topics WHERE title = 'Parent'"))
          new-child (one "SELECT id, parent_id, usn FROM topics WHERE title = 'Child'")]
      (testing "15.5.9: both rows left the refused block"
        (is (= 2 moved))
        (is (not= parent new-parent))
        (is (not= child (:id new-child)))
        (is (every? #(< % block_start) [new-parent (:id new-child)])))
      (testing "the reference moved with its row, and the row is still pending"
        (is (= new-parent (:parent_id new-child)))
        (is (= -1 (:usn new-child))))
      (testing "the per-topic settings key names the new id"
        (is (some? (one "SELECT 1 FROM settings WHERE key = ?" (str "last_page_" new-parent))))
        (is (nil? (one "SELECT 1 FROM settings WHERE key = ?" (str "last_page_" parent)))))
      (testing "replica mode ended with the transaction"
        (is (= "origin" (:r (one "SELECT current_setting('session_replication_role') AS r"))))))))

;; ── 15.6.2 Unique keys under a merge ───────────────────────────────

(def ^:private natural-keys
  "Merged into the existing row's id by the apply (`sync-apply/natural-keys`)."
  {"media" ["user_id" "sha256"] "kg_predicates" ["user_id" "slug"]})

(def ^:private random-keys
  "Random per row (`random-uuid`, or md5 of random()), so two libraries do not
   repeat one."
  {"assistant_chats" ["provider_session_id"] "assistant_runs" ["run_uuid"]})

(defn- foreign-key-columns
  "Every [table column] with a foreign key, from the catalog."
  []
  (set (map (juxt :tbl :col)
         (all "SELECT c.relname AS tbl, a.attname AS col
               FROM pg_constraint k
               JOIN pg_class c ON c.oid = k.conrelid
               JOIN pg_attribute a ON a.attrelid = k.conrelid AND a.attnum = ANY (k.conkey)
               WHERE k.contype = 'f'"))))

(defn- merge-rule
  "How a merge keeps the unique index `cols` on `table` from colliding, or nil.
   `:parent-id`: the first column is a foreign key, so it holds an id, and the
   ids of two libraries never meet, because every id comes from a block of one
   account."
  [fk-columns table cols]
  (cond
    (= (natural-keys table) cols) :natural-key
    (= (random-keys table) cols) :random
    (fk-columns [table (first cols)]) :parent-id
    :else nil))

(deftest every-unique-key-on-a-synced-table-has-a-merge-rule
  (when (desktop-test-db?)
    (testing "15.6.2"
      (let [synced (set (map name fmt/synced-tables))
            fk-columns (foreign-key-columns)
            indexes (all "SELECT c.relname AS tbl, i.relname AS idx,
                                 array_agg(a.attname ORDER BY k.ord) AS cols
                          FROM pg_index x
                          JOIN pg_class c ON c.oid = x.indrelid
                          JOIN pg_class i ON i.oid = x.indexrelid
                          JOIN pg_namespace n ON n.oid = c.relnamespace
                          CROSS JOIN LATERAL unnest(x.indkey::int2[]) WITH ORDINALITY AS k(attnum, ord)
                          LEFT JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum = k.attnum
                          WHERE n.nspname = 'public' AND x.indisunique AND NOT x.indisprimary
                          GROUP BY c.relname, i.relname")]
        (is (= [] (for [{:keys [tbl idx cols]} indexes
                        :when (synced tbl)
                        :let [cols (vec (remove nil? (.getArray ^java.sql.Array cols)))]
                        :when (nil? (merge-rule fk-columns tbl cols))]
                    [tbl idx cols])))))))
