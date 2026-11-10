(ns freememo.local-install-network-test
  "Seam 1 for ticket 06 of plans/local-install.md: the Local Install finds out
   by itself that freememo.net is back, and syncs.

   A real token is seeded on the test server, so sync works. The test edits
   the local database while the server is down. It reads the local database
   through its own PostgreSQL port, because the web app has no HTTP call that
   makes a topic. Everything else goes through HTTP."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.local-install-harness :as h]
            [freememo.sync-schema :as schema])
  (:import [java.io File]
           [java.security MessageDigest]))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(defn- sha256-hex [^String s]
  (apply str (map #(format "%02x" %)
               (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))

(defn- server-ds [{:keys [db-name]}]
  (jdbc/get-datasource {:dbtype "postgresql" :host "localhost" :port 5432 :dbname db-name
                        :user "cardmaker" :password "dev"}))

(defn- seed-server-account!
  "Post: an account.json map whose token the test server accepts."
  [server]
  (let [ds (server-ds server)
        one (fn [sql & ps] (jdbc/execute-one! ds (into [sql] ps) opts))
        user-id (:id (one "INSERT INTO users (username) VALUES (?) RETURNING id"
                       (str "net-" (System/nanoTime))))
        token (str "net-token-" user-id)
        token-id (:id (one "INSERT INTO desktop_tokens (user_id, token_hash) VALUES (?, ?) RETURNING id"
                        user-id (sha256-hex token)))
        n (:n (one "SELECT nextval('id_block_seq') AS n"))
        start (+ schema/client-id-floor (* n schema/block-size))
        end (+ start schema/block-size)]
    (one "INSERT INTO desktop_accounts (user_id, scm) VALUES (?, md5(random()::text)) RETURNING user_id" user-id)
    (one "INSERT INTO id_blocks (block_start, block_end, user_id, token_id) VALUES (?, ?, ?, ?) RETURNING user_id"
      start end user-id token-id)
    {"origin" (:origin server) "token" token "user_id" user-id "username" "net-test"
     "id_block" {"start" start "end" end}}))

(defn- local-ds [li]
  (let [install (json/parse-string (slurp (io/file (:data-dir li) "install.json")))]
    (jdbc/get-datasource {:dbtype "postgresql" :host "127.0.0.1" :port (:pg-port li) :dbname "postgres"
                          :user "postgres" :password (get install "pg_password")})))

(defn- status [li cookie]
  (json/parse-string (:body (h/http (h/url li "/api/desktop/status") {:cookie cookie}))))

(defn- wait-for
  "Post: the first truthy value of `f`, polled every 500 ms, or nil after `ms`."
  [ms f]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (or (try (f) (catch Exception _ nil))
          (when (< (System/currentTimeMillis) deadline)
            (Thread/sleep 500)
            (recur))))))

(defn- restart-test-server!
  "Post: a new server JVM on the old port and the old database."
  [{:keys [origin db-name]}]
  (let [port (parse-long (re-find #"\d+$" origin))
        handle (#'h/spawn! "prod" []
                 (merge @#'h/pg-env {"DB_NAME" db-name "PORT" port "COOKIE_SECURE" "false"
                                     "AUTH_MODE" "password"
                                     "ENC_KEY_SECRET" "li-test-server-secret"})
                 (File/createTempFile "li-test-server2" ".log"))]
    (#'h/await-http handle (str origin "/"))
    handle))

(deftest an-edit-made-offline-reaches-the-server-when-it-returns
  (let [server (h/start-test-server!)
        account (seed-server-account! server)
        li (h/start-local-install! {:account account})
        server2 (atom nil)]
    (try
      (let [{:keys [cookie]} (h/sign-in! li)
            title (str "offline edit " (System/nanoTime))]
        (testing "the first sync finishes while the server is up"
          (is (wait-for 120000 #(let [s (status li cookie)]
                                  (and (= "idle" (name (get s "state"))) (get s "last-sync-at"))))))
        (.destroy ^Process (:process server))
        (.waitFor ^Process (:process server))
        (jdbc/execute! (local-ds li)
          ["INSERT INTO topics (user_id, title) VALUES (?, ?)" (get account "user_id") title])
        (testing "the web app shows the offline state, with no Electron report"
          (is (wait-for 60000 #(let [s (status li cookie)]
                                 (and (false? (get s "online")) (= "offline" (name (get s "state"))))))))
        (reset! server2 (restart-test-server! server))
        (let [back-at (System/currentTimeMillis)]
          (testing "the edit is on the server within 20 seconds of its return"
            (is (wait-for 20000 #(jdbc/execute-one! (server-ds server)
                                   ["SELECT id FROM topics WHERE title = ?" title])))
            (is (< (- (System/currentTimeMillis) back-at) 21000)))))
      (finally
        (when (h/alive? li) (h/stop! li))
        (when-let [p (:process @server2)] (.destroy ^Process p) (.waitFor ^Process p))
        (h/stop-test-server! server)
        (h/delete-tree! (:data-dir li))))))
