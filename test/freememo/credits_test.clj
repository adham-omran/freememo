(ns freememo.credits-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.config :as config]
   [freememo.cost-history :as cost-history]
   [freememo.credits :as credits]
   [freememo.db :as db]
   [freememo.user-state :as us]
   [freememo.commands :as commands]
   [freememo.wayl :as wayl]))

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

(deftest request-base-url-prefers-the-configured-origin
  (let [proxied {:scheme :http
                 :headers {"x-forwarded-proto" "http" "host" "freememo.net"}}]
    (testing "a configured origin wins over the proxy's http scheme"
      (with-redefs [config/env-or-config (fn [env-var _path]
                                           (when (= "APP_BASE_URL" env-var)
                                             "https://freememo.net/"))]
        (is (= "https://freememo.net" (credits/request-base-url proxied)))))
    (testing "with no configured origin, the request decides"
      (with-redefs [config/env-or-config (constantly nil)]
        (is (= "http://freememo.net" (credits/request-base-url proxied)))
        (is (= "http://localhost:8080"
               (credits/request-base-url {:scheme :http :server-name "localhost"
                                          :server-port 8080 :headers {}})))))
    (testing "a blank configured origin is ignored"
      (with-redefs [config/env-or-config (constantly "  ")]
        (is (= "http://freememo.net" (credits/request-base-url proxied)))))))

(deftest sweep-mails-once-per-abandoned-checkout
  (let [order (fn [ref] {:reference_id ref :user_id 50 :kind "credits"
                         :amount_iqd 50000 :created_at "2026-10-30T12:33"
                         :email "buyer@example.com" :display_name "Buyer"
                         :tries 2 :paid 0})
        statuses {"fm-unpaid" {:ok true :status "Created"}
                  "fm-paid" {:ok true :status "Complete"}
                  "fm-done" {:ok true :status "Complete"}
                  "fm-down" {:ok false :error "Could not reach payment provider"}}
        marked (atom [])
        bumps (atom [])]
    (with-redefs [config/credits-enabled? (constantly true)
                  db/unalerted-pending-orders
                  (fn [min-age max-age]
                    (is (= [credits/abandon-after-minutes credits/abandon-lookback-hours]
                           [min-age max-age]))
                    (map order ["fm-unpaid" "fm-paid" "fm-done" "fm-down"]))
                  wayl/get-link-status (fn [ref] (statuses ref))
                  db/complete-credit-order! (fn [ref]
                                              (if (= "fm-paid" ref)
                                                {:credited true :amount 50000 :user-id 50}
                                                {:credited false :reason :already}))
                  db/fail-credit-order! (fn [_] nil)
                  db/mark-order-abandon-alerted! (fn [ref] (swap! marked conj ref))
                  commands/bump! (fn [user-id ch] (swap! bumps conj [user-id ch]))]
      (is (= {"fm-unpaid" :abandoned
              "fm-paid" :credited
              "fm-done" :already-complete
              "fm-down" :lookup-failed}
             (credits/sweep-abandoned-checkouts!)))
      (testing "only the unpaid order is marked, so a failed lookup is retried"
        (is (= ["fm-unpaid"] @marked)))
      (testing "a paid order the buyer never returned from is credited"
        (is (= [[50 :credits-granted]] @bumps))))))

(deftest sweep-does-nothing-when-credits-are-off
  (with-redefs [config/credits-enabled? (constantly false)
                db/unalerted-pending-orders (fn [& _] (throw (ex-info "must not query" {})))]
    (is (nil? (credits/sweep-abandoned-checkouts!)))))

(deftest abandoned-checkout-ids-are-mailed
  (let [src (slurp "src/freememo/logging.cljc")]
    (is (re-find #":freememo\.credits/checkout-abandoned" src))
    (is (re-find #":freememo\.credits/sweep-credited" src))))
