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
              user-id :curriculum.exam-form "test-model" 0.25 nil)))
      (is (= 1 @refresh)))))

(deftest cost-charge-ledger-carries-token-counts
  (let [user-id 987654328
        ledgers (atom [])
        tokens [{:input-tokens 1000 :cached-tokens 512
                 :output-tokens 80 :reasoning-tokens 40}
                {:input-tokens 1010 :output-tokens 90}]]
    (with-redefs [config/credits-enabled? (constantly true)
                  credits/charge-iqd-from-usd (fn [_user-id _usd] 7)
                  db/debit-credits! (fn [_user-id _cost ledger]
                                      (swap! ledgers conj ledger)
                                      100)]
      (testing "every billed completion reaches the annotation"
        (is (= 7 (credits/record-cost-charge!
                   user-id :cards.basic "gemini-3-flash" 0.01 tokens)))
        (is (= {:endpoint "cards.basic" :model "gemini-3-flash" :attempts tokens}
              (last @ledgers))))
      (testing "a caller with no counts records none"
        (credits/record-cost-charge! user-id :transcribe "whisper" 0.01 [])
        (is (nil? (:attempts (last @ledgers))))))))

(deftest debit-row-sums-tokens-and-keeps-unknown-null
  (testing "counts sum across attempts"
    (is (= {:input_tokens 2010 :cached_tokens 512
            :output_tokens 170 :reasoning_tokens 40 :attempts 2}
          (db/debit-token-columns
            [{:input-tokens 1000 :cached-tokens 512
              :output-tokens 80 :reasoning-tokens 40}
             {:input-tokens 1010 :output-tokens 90}]))))
  (testing "a count no attempt reported is NULL, and a reported zero is 0"
    (is (= {:input_tokens 5 :cached_tokens nil
            :output_tokens 0 :reasoning_tokens nil :attempts 1}
          (db/debit-token-columns [{:input-tokens 5 :output-tokens 0}]))))
  (testing "no known attempts writes NULL throughout"
    (is (= {:input_tokens nil :cached_tokens nil
            :output_tokens nil :reasoning_tokens nil :attempts nil}
          (db/debit-token-columns nil)))))

(deftest ai-costs-labels-exam-charges
  (testing "exam form generation"
    (is (= "Exam form generation"
          (cost-history/endpoint-label "curriculum.exam-form"))))
  (testing "exam grading"
    (is (= "Exam grading"
          (cost-history/endpoint-label "curriculum.exam-grade")))))
