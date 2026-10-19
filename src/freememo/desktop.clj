(ns freememo.desktop
  "Boot and HTTP gate of the JVM that the desktop app runs
   (plans/electron-local-db-sync.md §3.4, 3.5, 4).

   Electron starts this JVM with FREEMEMO_PEER=desktop and writes one JSON
   line on its stdin (3.4):

     {\"origin\": \"https://freememo.net\", \"token\": \"…\", \"user_id\": 7,
      \"username\": \"…\", \"launch_secret\": \"…\",
      \"id_block\": {\"start\": …, \"end\": …}}   ; first launch only

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
            [freememo.db :as db]
            [freememo.desktop-link :as link]
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

(defn- seed-account!
  "2.12 — the local users row carries the server's id and nothing secret.
   A database already bound to another account is refused: one install holds
   one account (9.3)."
  [{:strs [user_id username origin id_block]}]
  (let [bound (:sync_state/user_id (jdbc/execute-one! db/ds ["SELECT user_id FROM sync_state"]))]
    (when (and bound (not= (long bound) (long user_id)))
      (throw (ex-info "This desktop database belongs to another FreeMemo account"
               {:type ::other-account :bound bound :signing-in user_id}))))
  (jdbc/execute! db/ds ["INSERT INTO users (id, username) VALUES (?, ?)
                         ON CONFLICT (id) DO UPDATE SET username = EXCLUDED.username"
                        user_id username])
  (jdbc/execute! db/ds ["INSERT INTO sync_state (singleton, user_id, server_origin)
                         VALUES (true, ?, ?)
                         ON CONFLICT (singleton) DO UPDATE SET user_id = EXCLUDED.user_id,
                                                               server_origin = EXCLUDED.server_origin"
                        user_id origin])
  (when-let [{:strs [start end]} id_block]
    (jdbc/execute! db/ds ["INSERT INTO local_id_blocks (block_start, block_end, next_id)
                           VALUES (?, ?, ?) ON CONFLICT (block_start) DO NOTHING" start end start])))

(defn start!
  "Desktop boot, after `db/setup-schema`.
   Pre:  `read-launch-config!` has run.
   Post: the account is seeded, the link holds the token, and syncs run at
         open, every five minutes and on reconnect (2.11)."
  []
  (let [config @!launch]
    (seed-account! config)
    (link/configure! {:origin (get config "origin") :token (get config "token")
                      :user-id (get config "user_id")})
    (sync-client/start-scheduler!)
    (tel/log! {:level :info :id ::started :data {:user-id (get config "user_id")}}
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
