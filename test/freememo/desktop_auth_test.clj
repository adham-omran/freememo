(ns freememo.desktop-auth-test
  "plans/electron-local-db-sync.md 3.1, 3.2. Needs PostgreSQL with the server
   schema; the database tests skip when it is down."
  (:require [clojure.test :refer [deftest is testing]]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]
            [freememo.desktop-auth :as auth]
            [freememo.desktop-link :as link]
            [freememo.peer :as peer])
  (:import [java.security MessageDigest]
           [java.util Base64]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1 FROM desktop_auth_codes LIMIT 1"]) true (catch Exception _ false)))

(defn- challenge [verifier]
  (.encodeToString (.withoutPadding (Base64/getUrlEncoder))
    (.digest (MessageDigest/getInstance "SHA-256") (.getBytes ^String verifier "UTF-8"))))

(def verifier "a-verifier-of-at-least-forty-three-characters-xyz")
(def redirect "http://127.0.0.1:51234/callback")

(defn- user! []
  (:id (jdbc/execute-one! db/ds ["INSERT INTO users (username) VALUES (?) RETURNING id" (str "auth-" (System/nanoTime))]
         {:builder-fn rs/as-unqualified-maps})))

(defn- code-for! [user-id]
  (let [r (auth/authorize-post-handler
            {:session {:user-id user-id}
             :params {"response_type" "code" "code_challenge" (challenge verifier)
                      "code_challenge_method" "S256" "redirect_uri" redirect "state" "s"}})]
    (second (re-find #"code=([^&]+)" (get-in r [:headers "Location"])))))

(defn- exchange [code v]
  (auth/token-handler {:headers {"content-type" "application/x-www-form-urlencoded"}
                       :params {"code" code "code_verifier" v "redirect_uri" redirect}}))

(deftest a-code-works-once-and-only-with-its-verifier
  (when (db-up?)
    (let [u (user!)]
      (testing "a wrong verifier is refused"
        (is (= 400 (:status (exchange (code-for! u) "a-different-verifier-of-forty-three-characters-xx")))))
      (testing "the right verifier gets a token and a block"
        (let [code (code-for! u)
              r (exchange code verifier)
              body (json/parse-string (:body r))]
          (is (= 200 (:status r)))
          (is (= u (get body "user_id")))
          (is (<= 1099511627776 (get-in body ["id_block" "start"])))
          (testing "a replayed code is refused"
            (is (= 400 (:status (exchange code verifier)))))
          (testing "3.2: a revoked token stops working"
            (let [holder (auth/bearer-token {:headers {"authorization" (str "Bearer " (get body "access_token"))}})]
              (is (= u (:user-id holder)))
              (auth/revoke-token! u (:token-id holder))
              (is (nil? (auth/bearer-token {:headers {"authorization" (str "Bearer " (get body "access_token"))}}))))))))))

(deftest only-a-loopback-redirect-is-accepted
  (is (= 400 (:status (auth/authorize-handler
                        {:session {:user-id 1}
                         :params {"response_type" "code" "code_challenge" (challenge verifier)
                                  "code_challenge_method" "S256" "state" "s"
                                  "redirect_uri" "https://evil.example/callback"}})))))

(deftest a-signed-out-browser-is-sent-to-sign-in-and-back
  (let [r (auth/authorize-handler {:session {} :query-string "a=1"
                                   :params {"response_type" "code" "code_challenge" (challenge verifier)
                                            "code_challenge_method" "S256" "state" "s"
                                            "redirect_uri" redirect}})]
    (is (= "/" (get-in r [:headers "Location"])))
    (is (= "/desktop/authorize?a=1" (auth/return-path (:session r))))
    (is (= "/" (auth/return-path {:desktop-return "//evil.example"})) "no open redirect")))

(deftest offline-features-get-a-message-only-on-a-desktop
  (try
    (link/set-online! false)
    (is (nil? (link/offline-message "X")) "the server peer is never offline")
    ;; The guard reads the peer, so a desktop is stubbed here.
    (with-redefs [peer/desktop? (constantly true)]
      (testing "a signed-in desktop"
        (link/configure! {:origin "https://freememo.net" :token "t" :user-id 1})
        (is (= "X needs an internet connection. FreeMemo is offline." (link/offline-message "X")))
        (is (thrown? clojure.lang.ExceptionInfo (link/require-online! "X")))))
    (finally
      (link/set-online! true)
      (link/configure! {:origin nil :token nil :user-id nil}))))
