(ns freememo.wayl-test
  "The Wayl client against stubbed HTTP (plans/wayl-link-lifecycle.md §2).
   No call leaves the JVM."
  (:require
   [cheshire.core :as json]
   [clj-http.client :as http]
   [clojure.test :refer [deftest is testing]]
   [freememo.config :as config]
   [freememo.wayl :as wayl]))

(defmacro ^:private with-config
  "Run `body` with a merchant token and webhook secret configured."
  [& body]
  `(with-redefs [config/wayl-merchant-token (constantly "token")
                 config/wayl-webhook-secret (constantly "secret")
                 config/wayl-link-env (constantly "test")
                 config/wayl-host (constantly nil)]
     ~@body))

(deftest create-link-states-the-link-lifetime
  (let [sent (atom nil)]
    (with-config
      (with-redefs [http/post (fn [url opts]
                                (reset! sent {:url url :body (json/parse-string (:body opts) true)
                                              :headers (:headers opts)})
                                {:status 201 :body {:data {:url "https://link.thewayl.com/x"
                                                           :code "c" :id "l"}}})]
        (is (= {:ok true :url "https://link.thewayl.com/x" :code "c" :link-id "l"}
               (wayl/create-link! {:reference-id "fm-1" :amount-iqd 5000
                                   :webhook-url "https://h/api/wayl/webhook"
                                   :redirection-url "https://h/credits/return"})))))
    (testing "2.1: the body sends linkExpiresIn"
      (is (= "1h" (get-in @sent [:body :linkExpiresIn]))))
    (testing "the merchant header carries the token"
      (is (= "token" (get-in @sent [:headers "X-WAYL-AUTHENTICATION"]))))))

(deftest invalidate-link-reads-the-status-or-the-refusal
  (with-config
    (testing "2.2: a 201 returns the status and total"
      (with-redefs [http/post (fn [url _]
                                (is (re-find #"/api/v1/links/fm-1/invalidate$" url))
                                {:status 201 :body {:message "Link invalidated successfully"
                                                    :data {:status "Cancelled" :total "5000"}}})]
        (is (= {:ok true :status "Cancelled" :total "5000"} (wayl/invalidate-link! "fm-1")))))
    (testing "2.2: a 400 returns the HTTP status and Wayl's message"
      (with-redefs [http/post (constantly
                                {:status 400
                                 :body {:message "We can't invalidate link that is already [Cancelled]"
                                        :data {}}})]
        (is (= {:ok false :http-status 400
                :error "We can't invalidate link that is already [Cancelled]"}
               (wayl/invalidate-link! "fm-1")))))
    (testing "an exception reports an unreachable provider"
      (with-redefs [http/post (fn [& _] (throw (java.net.SocketTimeoutException. "timeout")))]
        (is (= {:ok false :error "Could not reach payment provider"}
               (wayl/invalidate-link! "fm-1")))))))

(deftest get-link-status-returns-the-total
  (with-config
    (with-redefs [http/get (constantly {:status 200 :body {:data {:status "Complete" :total "5000"}}})]
      (is (= {:ok true :status "Complete" :total "5000"} (wayl/get-link-status "fm-1"))))))

(deftest verify-auth-key-answers-without-logging
  (testing "2.3: a 200 is ok"
    (with-config
      (with-redefs [http/get (fn [url _]
                               (is (re-find #"/api/v1/verify-auth-key$" url))
                               {:status 200 :body {:message "Authentication key is valid"}})]
        (is (= {:ok true} (wayl/verify-auth-key))))))
  (testing "2.3: a 401 is not ok"
    (with-config
      (with-redefs [http/get (constantly {:status 401 :body {:message "Unauthorized"}})]
        (is (= {:ok false :error "Unauthorized"} (wayl/verify-auth-key))))))
  (testing "no token is not ok, and makes no call"
    (with-redefs [config/wayl-merchant-token (constantly nil)
                  http/get (fn [& _] (throw (ex-info "must not call" {})))]
      (is (false? (:ok (wayl/verify-auth-key)))))))
