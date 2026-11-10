(ns freememo.local-install-signin-test
  "Seam 1 for ticket 04 of plans/local-install.md: the first-launch sign-in.
   The test plays the student's browser. It signs in on the test server that
   plays freememo.net, and carries the redirect to the Local Install. The Local
   Install is seen through HTTP only, except that the downloaded topic is read
   from its PostgreSQL (the app has no HTTP route that reads a topic).
   Needs the dev PostgreSQL on 5432 and a client build. A first-launch start is
   fast. A start with the app takes minutes."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [freememo.local-install-harness :as h]
            [next.jdbc :as jdbc])
  (:import [java.net URI URLEncoder]
           [java.nio.file Files LinkOption]
           [java.nio.file.attribute PosixFilePermissions]))

(def ^:dynamic *server* nil)

(use-fixtures :once
  (fn [f]
    (let [server (h/start-test-server!)]
      (try (binding [*server* server] (f))
           (finally (h/stop-test-server! server))))))

(defn- server-ds [{:keys [db-name]}]
  (jdbc/get-datasource {:dbtype "postgresql" :host "localhost" :port 5432 :dbname db-name
                        :user "cardmaker" :password "dev"}))

(defn- seed-student!
  "Post: a user with a password and one topic on the test server."
  [server username]
  (let [ds (server-ds server)
        id (:users/id (jdbc/execute-one! ds ["INSERT INTO users (username, password_hash)
                                              VALUES (?, crypt('pw', gen_salt('bf'))) RETURNING id" username]))]
    (jdbc/execute! ds ["INSERT INTO topics (user_id, title, content) VALUES (?, 'Held on the server', 'x')" id])
    id))

(defn- form [m]
  (str/join "&" (for [[k v] m] (str (name k) "=" (URLEncoder/encode (str v) "UTF-8")))))

(defn- local-origin [li] (str "http://127.0.0.1:" (:port li)))

(defn- begin-sign-in!
  "The first-launch page's Sign in button. Post: the freememo.net authorize URL."
  [li cookie]
  (let [response (h/http (h/url li "/first-launch/sign-in")
                   {:method :post :cookie cookie :body "" :headers {"Origin" (local-origin li)}})]
    (is (= 303 (:status response)))
    (get-in response [:headers "location"])))

(defn- allow-on-server!
  "The student signs in on the server and presses Allow.
   Post: the callback URL on the Local Install, with its code and state."
  [server username authorize-url]
  (let [session (h/cookie-pair (h/http (str (:origin server) "/login")
                                 {:method :post :body (form {:user username :password "pw"})
                                  :headers {"Content-Type" "application/x-www-form-urlencoded"
                                            "Origin" (:origin server)}}))
        query (.getRawQuery (URI. authorize-url))
        consent (h/http authorize-url {:cookie session})
        allow (h/http (str (:origin server) "/desktop/authorize")
                {:method :post :cookie session :body query
                 :headers {"Content-Type" "application/x-www-form-urlencoded" "Origin" (:origin server)}})]
    (is (= 200 (:status consent)))
    (is (= 302 (:status allow)))
    (get-in allow [:headers "location"])))

(defn- account-file [li] (io/file (:data-dir li) "account.json"))

(defn- mode [f]
  (PosixFilePermissions/toString
    (Files/getPosixFilePermissions (.toPath f) (make-array LinkOption 0))))

(defn- wait-until [pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (or (pred)
        (when (< (System/currentTimeMillis) deadline) (Thread/sleep 1000) (recur))))))

(deftest sign-in-downloads-the-library-and-a-restart-needs-no-sign-in
  (let [username (str "student" (System/nanoTime))
        _ (seed-student! *server* username)
        li (h/start-local-install! {:account :none :origin (:origin *server*)})]
    (try
      (let [{:keys [cookie]} (h/sign-in! li)]
        (testing "the first-launch page offers Sign in, and nothing is saved yet"
          (let [page (h/http (h/url li "/") {:cookie cookie})]
            (is (str/includes? (:body page) "Sign in"))
            (is (str/includes? (:body page) "<title>FreeMemo Local")))
          (is (not (.exists (account-file li))))
          (is (nil? (:pg-port li)) "PostgreSQL has not started"))
        (let [callback (allow-on-server! *server* username (begin-sign-in! li cookie))
              ;; freememo.net's redirect carries no Local Install cookie.
              answer (h/http callback {})
              li (h/await-ready! li)]
          (is (= 200 (:status answer)) "a page, not a redirect")
          (testing "the token file has mode 0600 and the token is not in the environment"
            (is (.exists (account-file li)))
            (is (= "rw-------" (mode (account-file li))))
            (let [account (json/parse-string (slurp (account-file li)))
                  environ (io/file "/proc" (str (.pid ^Process (:process li))) "environ")]
              (is (seq (get account "token")))
              (when (.canRead environ)
                (is (not (str/includes? (slurp environ) (get account "token")))))))
          (testing "the page shows the first download, then the app"
            (is (wait-until #(true? (get (json/parse-string
                                          (:body (h/http (h/url li "/api/desktop/status") {:cookie cookie})))
                                    "library-ready"))
                  240000))
            (let [page (h/http (h/url li "/") {:cookie cookie})]
              (is (not (str/includes? (:body page) "Getting your library")))))
          (testing "the topic that the server holds is in the local library"
            (let [install (json/parse-string (slurp (io/file (:data-dir li) "install.json")))
                  ds (jdbc/get-datasource {:dbtype "postgresql" :host "127.0.0.1" :port (:pg-port li)
                                           :dbname "postgres" :user "postgres"
                                           :password (get install "pg_password")})]
              (is (= ["Held on the server"]
                    (mapv :topics/title (jdbc/execute! ds ["SELECT title FROM topics"])))))))
        (testing "a restart opens the library with no new sign-in"
          (is (contains? #{0 143} (h/stop! li)))
          (let [again (h/start-local-install! {:data-dir (:data-dir li)})]
            (try
              (let [page (h/http (h/url again "/") {:cookie cookie})]
                (is (= 200 (:status page)))
                (is (not (str/includes? (:body page) "Sign in")))
                (is (not (str/includes? (:body page) "Getting your library"))))
              (finally (h/stop! again))))))
      (finally
        (when (h/alive? li) (h/stop! li))
        (h/delete-tree! (:data-dir li))))))

(deftest a-wrong-state-is-refused-and-writes-no-token-file
  (let [username (str "student" (System/nanoTime))
        _ (seed-student! *server* username)
        li (h/start-local-install! {:account :none :origin (:origin *server*)})]
    (try
      (let [{:keys [cookie]} (h/sign-in! li)
            callback (allow-on-server! *server* username (begin-sign-in! li cookie))
            forged (str/replace callback #"state=[^&]+" "state=forged")
            answer (h/http forged {})]
        (is (= 400 (:status answer)))
        (is (not (.exists (account-file li))))
        (is (not (.exists (io/file (:data-dir li) "pgdata")))))
      (finally
        (h/stop! li)
        (h/delete-tree! (:data-dir li))))))

(deftest with-no-connection-the-page-asks-for-the-internet-and-leaves-no-account
  (let [dead (str "http://127.0.0.1:" (h/free-port))
        li (h/start-local-install! {:account :none :origin dead})]
    (try
      (let [{:keys [cookie]} (h/sign-in! li)
            page (h/http (h/url li "/") {:cookie cookie})]
        (is (str/includes? (:body page) "Connect to the internet for the first start"))
        (is (str/includes? (:body page) "Try again"))
        (is (not (str/includes? (:body page) "Sign in")))
        (is (not (.exists (account-file li))))
        (is (not (.exists (io/file (:data-dir li) "pgdata"))))
        (is (not (.exists (io/file (:data-dir li) "account.json.tmp")))))
      (finally
        (h/stop! li)
        (h/delete-tree! (:data-dir li))))))
