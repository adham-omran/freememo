(ns freememo.protocol-refusal-paths-test
  "Review fix 2: every path that sees the protocol 409 of
   `desktop-auth/protocol-refusal` sets the sync state `:update-required`, so
   \"Update to keep syncing\" shows. Another 409 does not. No database."
  (:require [clojure.test :refer [deftest is testing]]
            [cheshire.core :as json]
            [clj-http.client :as http]
            [freememo.desktop-auth :as auth]
            [freememo.desktop-link :as link]
            [freememo.openrouter :as openrouter]
            [freememo.peer :as peer]
            [freememo.sync-client :as sc]))

(def ^:private refusal-body
  (:body (auth/protocol-refusal {:headers {"x-freememo-sync-protocol" "0"}} nil)))

(deftest the-refusal-body-is-told-from-other-409s
  (is (string? refusal-body))
  (is (sc/protocol-refusal? refusal-body))
  (is (sc/protocol-refusal? (json/parse-string refusal-body)))
  (is (not (sc/protocol-refusal? "{\"error\":\"conflict\"}")))
  (is (not (sc/protocol-refusal? {"error" "update-required"})) "no protocol number")
  (is (not (sc/protocol-refusal? nil)))
  (is (not (sc/protocol-refusal? "<html>409</html>"))))

(defn- call-with [resp]
  (with-redefs [http/request (fn [_] resp)
                link/origin (constantly "http://x")
                link/token (constantly "t")
                link/set-online! (fn [_])]
    (try (#'sc/call! :post "/api/sync/graves")
      (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))

(deftest a-sync-step-throws-update-required-on-the-refusal
  (testing "the refusal, from any step"
    (is (= ::sc/update-required
          (call-with {:status 409 :body refusal-body
                      :headers {"content-type" "application/json"}}))))
  (testing "another 409 is an ordinary answer"
    (is (= {:status 409 :body {"error" "conflict"}}
          (call-with {:status 409 :body "{\"error\":\"conflict\"}"
                      :headers {"content-type" "application/json"}})))))

(deftest the-ai-proxy-409-sets-the-state
  (with-redefs [peer/desktop? (constantly true)
                link/require-online! (fn [_])
                link/origin (constantly "http://x")
                link/token (constantly "t")
                sc/local-user-id (constantly nil)]
    (testing "another 409 leaves the state"
      (with-redefs [http/post (fn [& _] {:status 409 :body "{\"error\":\"x\"}"})]
        (openrouter/post! nil "/chat/completions" {})
        (is (not= :update-required (sc/sync-state)))))
    (testing "protocol refusal"
      (with-redefs [http/post (fn [& _] {:status 409 :body refusal-body})]
        (openrouter/post! nil "/chat/completions" {})
        (is (= :update-required (sc/sync-state)))))))
