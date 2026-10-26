(ns freememo.desktop
  "Boot and HTTP gate of the JVM that the desktop app runs
   (plans/electron-local-db-sync.md §3.4, 3.5, 4).

   Electron starts this JVM with FREEMEMO_PEER=desktop and writes one JSON
   line on its stdin (3.4):

     {\"origin\": \"https://freememo.net\", \"token\": \"…\", \"user_id\": 7,
      \"username\": \"…\", \"launch_secret\": \"…\",
      \"id_block\": {\"start\": …, \"end\": …}}   ; first launch only

   A local account (§15) passes `{\"user_id\": 0, \"local\": true,
   \"launch_secret\": \"…\"}`, with no origin and no token. The first launch
   after its sign-in adds `\"link_local\": true`, which re-keys the library.

   The token never enters the environment, so `/proc/<pid>/environ` does not
   hold it. The launch secret is new on every launch. Electron sets it as a
   cookie on the window's session, and `wrap-launch-gate` answers 401 to any
   request without it (3.5), so another local program cannot use this server.

   The gate also signs every request in as the one local account. There is no
   login page on a desktop, because Electron already authenticated the user."
  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [taoensso.telemere :as tel]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]
            [freememo.desktop-link :as link]
            [freememo.sync-apply :as apply]
            [freememo.sync-client :as sync-client])
  (:import [java.io BufferedReader InputStreamReader]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(defonce ^:private !launch (atom nil))

(defn read-launch-config!
  "Read Electron's one stdin line.
   Pre:  Electron wrote the line before this JVM's -main ran (it writes it at
         spawn). A missing or malformed line is Electron's bug: this throws,
         and the JVM exits before serving anything."
  []
  (let [line (.readLine (BufferedReader. (InputStreamReader. System/in StandardCharsets/UTF_8)))
        config (some-> line json/parse-string)]
    (when-not (and (map? config) (get config "launch_secret") (get config "user_id"))
      (throw (ex-info "The desktop launch configuration on stdin is missing or malformed"
               {:type ::bad-launch-config})))
    (reset! !launch config)
    config))

(def local-user-id
  "15.1.2 — the user id of a local account. `users.id` is SERIAL, so no
   server account has it."
  0)

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(defn local-config?
  "True for the launch configuration of a local account (15.2.1)."
  [config]
  (= local-user-id (get config "user_id")))

(defn- insert-block! [conn {:keys [start end]} origin]
  (jdbc/execute! conn ["INSERT INTO local_id_blocks (block_start, block_end, next_id, origin)
                        VALUES (?, ?, ?, ?) ON CONFLICT (block_start) DO NOTHING" start end start origin]))

(defn- seed-server-account!
  "2.12 — the local users row carries the server's id and nothing secret."
  [conn {:strs [user_id username origin id_block]}]
  (jdbc/execute! conn ["INSERT INTO users (id, username) VALUES (?, ?)
                        ON CONFLICT (id) DO UPDATE SET username = EXCLUDED.username"
                       user_id username])
  (jdbc/execute! conn ["INSERT INTO sync_state (singleton, user_id, server_origin)
                        VALUES (true, ?, ?)
                        ON CONFLICT (singleton) DO UPDATE SET user_id = EXCLUDED.user_id,
                                                              server_origin = EXCLUDED.server_origin"
                       user_id origin])
  (when-let [{:strs [start end]} id_block]
    (insert-block! conn {:start start :end end} "server")))

(defn- seed-local-account!
  "15.1.2, 15.2.4 — the users row 0 of a local account. Its username is
   random, so it cannot equal the server username that a re-key inserts
   beside it. The UI names the account from the state `local`, never from it.
   Post: the row and the sync_state binding exist, with no origin. The first
         offline block comes from `sync-client/ensure-id-blocks!`."
  [conn]
  (jdbc/execute! conn ["INSERT INTO users (id, username) VALUES (?, ?) ON CONFLICT (id) DO NOTHING"
                       local-user-id (str "local-" (random-uuid))])
  (jdbc/execute! conn ["INSERT INTO sync_state (singleton, user_id, server_origin)
                        VALUES (true, ?, NULL) ON CONFLICT (singleton) DO NOTHING"
                       local-user-id]))

(defn- foreign-keys-to-users
  "Every column with a foreign key to `users(id)`, as [table column], from
   the catalog."
  [conn]
  (mapv (juxt :tbl :col)
    (jdbc/execute! conn ["SELECT c.relname AS tbl, a.attname AS col
                          FROM pg_constraint k
                          JOIN pg_class c ON c.oid = k.conrelid
                          JOIN pg_namespace n ON n.oid = c.relnamespace
                          JOIN pg_attribute a ON a.attrelid = k.conrelid AND a.attnum = ANY (k.conkey)
                          WHERE n.nspname = 'public' AND k.contype = 'f'
                            AND k.confrelid = 'users'::regclass"]
      opts)))

(defn user-columns
  "15.5.4 — every column the re-key changes, as [table column]: each column
   with a foreign key to `users(id)`, and each column named `user_id`, which
   finds the ones with no foreign key, such as `sync_state.user_id`. The list
   comes from the catalog, so a new table needs no entry here."
  [conn]
  (->> (jdbc/execute! conn ["SELECT table_name AS tbl, column_name AS col FROM information_schema.columns
                             WHERE table_schema = 'public' AND column_name = 'user_id'
                               AND table_name IN (SELECT tablename FROM pg_tables WHERE schemaname = 'public')"]
         opts)
    (map (juxt :tbl :col))
    (into (set (foreign-keys-to-users conn)))
    sort vec))

(defn- references-to-local-user
  "15.5.3 step 3 — the rows that still reference users row 0 through a
   foreign key, per [table column], with only the non-zero counts. It reads
   the foreign keys itself, not `user-columns`, so it finds a column that the
   change missed."
  [conn]
  (into {}
    (for [[t c] (foreign-keys-to-users conn)
          :let [n (:n (jdbc/execute-one! conn [(format "SELECT count(*) AS n FROM %s WHERE %s = ?" t c)
                                               local-user-id] opts))]
          :when (pos? n)]
      [[t c] n])))

(defn rekey-local-account!
  "15.5.3 — make a local account's library the account's, in one transaction.
   The sync bypass is on, so every row keeps its usn and mtime: a local
   settings key must not win against the account's newer one only because the
   re-key touched it.
   Pre:  `sync_state.user_id` is 0. `config` is a signed-in configuration.
   Post: no column holds user id 0, users row 0 is gone, `merge_pending` is
         set, every offline block is closed, so new rows take server ids, and
         the token's first block is in `local_id_blocks`. On any failure,
         nothing changed.
   Inv:  the delete of row 0 runs only after a count shows no row references
         it. 38 foreign keys reference users(id) ON DELETE CASCADE, so a
         column the change missed would otherwise lose its rows silently."
  [{:strs [user_id username origin id_block]}]
  (apply/with-apply-tx db/ds
    (fn [tx]
      (jdbc/execute! tx ["INSERT INTO users (id, username) VALUES (?, ?)
                          ON CONFLICT (id) DO UPDATE SET username = EXCLUDED.username" user_id username])
      (doseq [[t c] (user-columns tx)]
        (jdbc/execute! tx [(format "UPDATE %s SET %s = ? WHERE %s = ?" t c c) user_id local-user-id]))
      (let [left (references-to-local-user tx)]
        (when (seq left)
          (throw (ex-info "The re-key left rows that reference the local account"
                   {:type ::rekey-incomplete :left left}))))
      (jdbc/execute! tx ["DELETE FROM users WHERE id = ?" local-user-id])
      (jdbc/execute! tx ["UPDATE sync_state SET user_id = ?, server_origin = ?, merge_pending = true,
                                               checkpoint = 0"
                         user_id origin])
      (jdbc/execute! tx ["UPDATE local_id_blocks SET next_id = block_end WHERE origin = 'offline'"])
      (when-let [{:strs [start end]} id_block]
        (insert-block! tx {:start start :end end} "server"))))
  (tel/log! {:level :info :id ::rekeyed :data {:user-id user_id}}
    "The local account now belongs to a FreeMemo account"))

(defn- bind-account!
  "15.5.5 — seed, re-key, start or refuse, by the bound user id and the
   configuration:

   | Bound      | Configuration                 | Result           |
   |------------|-------------------------------|------------------|
   | NULL       | any                           | seed             |
   | 0          | account id, `link_local` true | re-key (15.5.3)  |
   | 0          | user id 0                     | local account    |
   | equal      | —                             | start            |
   | any other  | —                             | refuse (9.3)     |"
  [config]
  (let [bound (:user_id (jdbc/execute-one! db/ds ["SELECT user_id FROM sync_state"] opts))
        user-id (long (get config "user_id"))
        local? (local-config? config)]
    (cond
      (and (nil? bound) local?) (seed-local-account! db/ds)
      (nil? bound) (seed-server-account! db/ds config)
      (and (= local-user-id bound) local?) (seed-local-account! db/ds)
      (and (= local-user-id bound) (true? (get config "link_local"))) (rekey-local-account! config)
      (= (long bound) user-id) (seed-server-account! db/ds config)
      :else (throw (ex-info "This desktop database belongs to another FreeMemo account"
                     {:type ::other-account :bound bound :signing-in user-id})))))

(defn start!
  "Desktop boot, after `db/setup-schema`.
   Pre:  `read-launch-config!` has run.
   Post: the account is bound (`bind-account!`) and the link holds the token.
         A signed-in desktop syncs at open, every five minutes and on
         reconnect (2.11). A local account syncs nothing: it only tops up its
         offline id blocks (15.2.2, 15.3.2)."
  []
  (let [config @!launch
        local? (local-config? config)]
    (bind-account! config)
    (link/configure! {:origin (when-not local? (get config "origin"))
                      :token (when-not local? (get config "token"))
                      :user-id (get config "user_id")})
    (if local?
      (sync-client/start-local-block-timer!)
      (sync-client/start-scheduler!))
    (tel/log! {:level :info :id ::started :data {:user-id (get config "user_id") :local? local?}}
      "Desktop runtime started")))

;; ── 3.5 The launch gate ────────────────────────────────────────────

(def launch-cookie "fm_launch")

(defn- same-secret? [^String a ^String b]
  (and a b (MessageDigest/isEqual (.getBytes a StandardCharsets/UTF_8)
             (.getBytes b StandardCharsets/UTF_8))))

(defn- launch-cookie-value [request]
  (some->> (get-in request [:headers "cookie"])
    (re-find (re-pattern (str "(?:^|;\\s*)" launch-cookie "=([^;]+)")))
    second))

(defn- json-response [status body]
  {:status status :headers {"Content-Type" "application/json"} :body (json/generate-string body)})

(defn- local-route
  "The endpoints only Electron calls. They live here, not in `api/routes`,
   because freememo.net has no use for them."
  [request]
  (case [(:request-method request) (:uri request)]
    [:post "/api/desktop/connectivity"]
    (let [online? (get (some-> (:body request) slurp json/parse-string) "online")]
      (link/set-online! online?)
      (json-response 200 {:online (link/online?)}))

    [:post "/api/desktop/sync-now"]
    (json-response 200 (sync-client/sync-now!))

    [:get "/api/desktop/status"]
    (json-response 200 (sync-client/status))

    nil))

(defn wrap-launch-gate
  "3.5 — only Electron's window may use this server.
   Pre:  sits inside `wrap-session`, so the request carries :session.
   Post: a request without the launch cookie gets 401 and reaches nothing.
         A request with it is signed in as the local account."
  [handler]
  (fn [request]
    (let [{:strs [launch_secret user_id]} @!launch]
      (if-not (same-secret? (launch-cookie-value request) launch_secret)
        {:status 401 :headers {"Content-Type" "text/plain"} :body "Unauthorized"}
        (let [request (assoc-in request [:session :user-id] user_id)]
          (or (local-route request) (handler request)))))))
