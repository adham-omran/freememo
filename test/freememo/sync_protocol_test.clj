(ns freememo.sync-protocol-test
  "The Sync Protocol (CONTEXT.md). freememo.net refuses a different number on
   sync, sign-in and the AI proxy with one 409 answer, and answers the same
   number normally. Needs PostgreSQL with the server schema; the database
   tests skip when it is down."
  (:require [clojure.test :refer [deftest is testing]]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.ai-proxy :as ai-proxy]
            [freememo.db :as db]
            [freememo.desktop-auth :as auth]
            [freememo.desktop-version :as desktop-version]
            [freememo.sync-server :as server])
  (:import [java.io ByteArrayInputStream]))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})
(def ^:private header "x-freememo-sync-protocol")

(deftest the-protocol-is-compared-as-one-plain-number
  (is (nil? (desktop-version/protocol-error desktop-version/sync-protocol)))
  (is (= "update-required" (desktop-version/protocol-error (dec desktop-version/sync-protocol))))
  (is (= "server-outdated" (desktop-version/protocol-error (inc desktop-version/sync-protocol))))
  (is (= "server-outdated" (desktop-version/protocol-error nil))))

(deftest a-request-without-a-number-passes-unless-it-sends-the-retired-major
  (is (nil? (auth/protocol-refusal {:headers {}} nil)))
  (is (= 409 (:status (auth/protocol-refusal {:headers {"x-freememo-major" "1"}} nil)))
    "an old Local Install sends the major header and no Sync Protocol"))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1 FROM desktop_tokens LIMIT 1"]) true (catch Exception _ false)))

(defn- account! []
  (let [u (:id (jdbc/execute-one! db/ds ["INSERT INTO users (username) VALUES (?) RETURNING id"
                                         (str "protocol-" (System/nanoTime))] opts))
        token (str "protocol-token-" u "-" (System/nanoTime))]
    (jdbc/execute! db/ds ["INSERT INTO desktop_tokens (user_id, token_hash) VALUES (?, ?)" u (auth/sha256-hex token)])
    (jdbc/execute! db/ds ["INSERT INTO desktop_accounts (user_id, scm) VALUES (?, 'protocol')" u])
    token))

(defn- json-body [body]
  (ByteArrayInputStream. (.getBytes (json/generate-string body) "UTF-8")))

(defn- request [token protocol body]
  {:request-method :post
   :uri "/api/desktop/ai/chat/completions"
   :headers (cond-> {"authorization" (str "Bearer " token)}
              protocol (assoc header (str protocol)))
   :body (some-> body json-body)})

(deftest sync-and-the-ai-proxy-refuse-a-different-protocol
  (when (db-up?)
    (let [token (account!)
          mine desktop-version/sync-protocol]
      (doseq [other [(dec mine) (inc mine)]]
        (testing (str "protocol " other)
          (let [meta (server/meta-handler (request token other {}))
                download (server/download-handler (request token other {:from 0}))
                ai (ai-proxy/post-handler (request token other {:model "x"}))]
            (is (= [409 409 409] (map :status [meta download ai])))
            (is (= (:body meta) (:body download) (:body ai))
              "one answer on every path"))))
      (testing "the same protocol works"
        (let [meta (server/meta-handler (request token mine {}))
              ai (ai-proxy/post-handler (request token mine nil))]
          (is (= 200 (:status meta)))
          (is (= mine (get (json/parse-string (:body meta)) "protocol")))
          (is (= 400 (:status ai)) "past the protocol gate, then the empty body is rejected"))))))

(deftest sign-in-refuses-a-different-protocol
  (when (db-up?)
    (let [sign-in (fn [protocol]
                    (auth/token-handler
                      {:headers {"content-type" "application/json" header (str protocol)}
                       :body (json-body {"code" "c" "code_verifier" "v" "redirect_uri" "r"})}))
          refused (sign-in 0)]
      (is (= 409 (:status refused)))
      (is (= "update-required" (get (json/parse-string (:body refused)) "error")))
      (testing "the same protocol reaches the code check"
        (is (= 400 (:status (sign-in desktop-version/sync-protocol))))))))
