(ns freememo.local-install-account-test
  "Seam 1 for ticket 05 of plans/local-install.md: one library, one account.
   One Local Install lives through the whole story, because a start takes
   minutes. The test plays the student's browser, as in
   `freememo.local-install-signin-test`. It reads and edits the local database
   through its own PostgreSQL port, because the app has no HTTP route that
   reads or makes a topic."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.local-install-harness :as h]
            [freememo.local-install-signin-test :as s]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(defn- wait-until [pred ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (or (try (pred) (catch Exception _ nil))
        (when (< (System/currentTimeMillis) deadline) (Thread/sleep 1000) (recur))))))

(defn- local-ds [li]
  (let [install (json/parse-string (slurp (io/file (:data-dir li) "install.json")))]
    (jdbc/get-datasource {:dbtype "postgresql" :host "127.0.0.1" :port (:pg-port li) :dbname "postgres"
                          :user "postgres" :password (get install "pg_password")})))

(defn- local-titles [li]
  (set (map :title (jdbc/execute! (local-ds li) ["SELECT title FROM topics"] opts))))

(defn- status [li cookie]
  (json/parse-string (:body (h/http (h/url li "/api/desktop/status") {:cookie cookie}))))

(defn- sign-in-as!
  "The student signs in as `username` on the sign-in page of `li`.
   Post: the answer to the callback."
  [li cookie server username]
  (h/http (#'s/allow-on-server! server username (#'s/begin-sign-in! li cookie)) {}))

(defn- account-user-id [li]
  (get (json/parse-string (slurp (io/file (:data-dir li) "account.json"))) "user_id"))

(deftest one-library-one-account
  (let [server (h/start-test-server!)
        stamp (System/nanoTime)
        a (str "ann" stamp)
        b (str "bob" stamp)
        a-id (#'s/seed-student! server a)
        b-id (#'s/seed-student! server b)
        server-ds (#'s/server-ds server)
        li (h/start-local-install! {:account :none :origin (:origin server)})]
    (try
      (let [{:keys [cookie]} (h/sign-in! li)
            post (fn [path body]
                   (h/http (h/url li path)
                     {:method :post :cookie cookie :body body
                      :headers {"Origin" (str "http://127.0.0.1:" (:port li))
                                "Content-Type" "application/x-www-form-urlencoded"}}))
            page (fn [path] (:body (h/http (h/url li path) {:cookie cookie})))]
        (is (= 200 (:status (sign-in-as! li cookie server a))))
        (let [li (h/await-ready! li)]
          (is (wait-until #(and (get (status li cookie) "library-ready")
                             (= "idle" (get (status li cookie) "state")))
                240000))
          (testing "two changes made while the token is alive but never synced"
            (jdbc/execute! server-ds ["UPDATE desktop_tokens SET revoked_at = now() WHERE user_id = ?" a-id])
            (doseq [t ["offline one" "offline two"]]
              (jdbc/execute! (local-ds li) ["INSERT INTO topics (user_id, title) VALUES (?, ?)" a-id t])))
          (testing "a revoked token shows the sign-in page, and the library stays"
            (post "/api/desktop/sync-now" "")
            (is (wait-until #(= "signed-out" (name (get (status li cookie) "state"))) 60000))
            (is (str/includes? (page "/") "Sign in again"))
            (is (contains? (local-titles li) "offline two")))
          (testing "account B is refused, and the library of A is unchanged"
            (let [answer (sign-in-as! li cookie server b)]
              (is (= 200 (:status answer)))
              (is (str/includes? (:body answer) "/remove-library")))
            (is (= a-id (account-user-id li)))
            (is (= #{"Held on the server" "offline one" "offline two"} (local-titles li))))
          (testing "the remove page shows the changes that did not sync"
            (is (str/includes? (page "/remove-library") "<strong>2</strong> changes")))
          (testing "the same account signs in again, keeps the library and syncs"
            (is (= 200 (:status (sign-in-as! li cookie server a))))
            (is (wait-until #(jdbc/execute-one! server-ds ["SELECT id FROM topics WHERE title = 'offline two'"]) 120000))
            (is (wait-until #(not (str/includes? (page "/") "Sign in again")) 30000))
            (is (= a-id (account-user-id li))))
          (testing "nothing is removed without the confirmation"
            (is (= 400 (:status (post "/remove-library" ""))))
            (is (.exists (io/file (:data-dir li) "account.json"))))
          (testing "after a confirmed removal, account B downloads its own library"
            (is (= 303 (:status (post "/remove-library" "confirm=yes"))))
            (is (not (.exists (io/file (:data-dir li) "account.json"))))
            (is (str/includes? (page "/") "Sign in"))
            (is (= #{} (local-titles li)))
            (is (= 200 (:status (sign-in-as! li cookie server b))))
            (is (wait-until #(and (get (status li cookie) "library-ready")
                               (= #{"Held on the server"} (local-titles li)))
                  240000))
            (is (= b-id (account-user-id li)))
            (is (zero? (:n (jdbc/execute-one! (local-ds li) ["SELECT count(*) AS n FROM users WHERE id = ?" a-id] opts)))))))
      (finally
        (when (h/alive? li) (h/stop! li))
        (h/stop-test-server! server)
        (h/delete-tree! (:data-dir li))))))
