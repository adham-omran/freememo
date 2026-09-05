(ns freememo.client-reactor-failure-test
  "HTTP contracts for fatal browser reports."
  (:require
   [cheshire.core :as json]
   [clojure.test :refer [deftest is testing]]
   [freememo.client-failure-http :as client-failure-http]))

(defn- report-request [session-id overrides]
  {:session {:user-id 91}
   :headers {"user-agent" "test-browser"}
   :remote-addr "127.0.0.1"
   :params
   (merge
     {"session-id" session-id
      "termination" "failure"
      "message" "boom"
      "stack" "stack"
      "route" "/viewer/topic/89913"
      "client-version" "test-version"
      "chat-id" "17"
      "run-uuid" "run-17"
      "interactions" "[{\"kind\":\"assistant\",\"detail\":{\"action\":\"submit\"}}]"}
     overrides)})

(deftest fatal-report-requires-authentication
  (is (= 401 (:status (client-failure-http/handler
                        {:params {} :session {}})))))

(deftest fatal-report-is-bounded-and-deduplicated
  (testing "the first bounded report is accepted"
    (let [session-id (str (random-uuid))
          first-response (client-failure-http/handler
                           (report-request session-id {}))
          duplicate-response (client-failure-http/handler
                               (report-request session-id {}))]
      (is (= 202 (:status first-response)))
      (is (true? (:accepted (json/parse-string (:body first-response) true))))
      (is (false? (:accepted
                    (json/parse-string (:body duplicate-response) true))))))
  (testing "an oversized report is rejected"
    (is (= 413
          (:status
            (client-failure-http/handler
              (report-request (str (random-uuid))
                {"stack" (apply str (repeat 17000 "x"))})))))))

(deftest recent-interactions-are-sanitized
  (is (= [{:kind "assistant" :detail {:action "submit"}}]
        (#'client-failure-http/sanitized-interactions
          "[{\"kind\":\"assistant\",\"detail\":{\"action\":\"submit\"}}]"))))
