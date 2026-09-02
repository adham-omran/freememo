(ns freememo.credits-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.config :as config]
   [freememo.cost-history :as cost-history]
   [freememo.credits :as credits]
   [freememo.db :as db]
   [freememo.user-state :as us]))

(deftest completed-cost-charge-refreshes-ai-costs
  (let [user-id 987654327
        refresh (us/get-atom user-id :credits-refresh)]
    (reset! refresh 0)
    (with-redefs [config/credits-enabled? (constantly true)
                  credits/charge-iqd-from-usd (fn [_user-id _usd] 12)
                  db/debit-credits! (fn [& _args] 88)]
      (is (= 12
            (credits/record-cost-charge!
              user-id :curriculum.exam-form "test-model" 0.25)))
      (is (= 1 @refresh)))))

(deftest ai-costs-labels-exam-charges
  (testing "exam form generation"
    (is (= "Exam form generation"
          (cost-history/endpoint-label "curriculum.exam-form"))))
  (testing "exam grading"
    (is (= "Exam grading"
          (cost-history/endpoint-label "curriculum.exam-grade")))))
