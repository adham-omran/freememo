(ns freememo.credits-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.config :as config]
   [freememo.cost-history :as cost-history]
   [freememo.credits :as credits]
   [freememo.db :as db]
   [freememo.user-state :as us]
   [freememo.commands :as commands]
   [freememo.storage-meter :as storage-meter]
   [freememo.wayl :as wayl]
   [taoensso.telemere :as tel]))

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

;; ── Wayl link lifecycle (plans/wayl-link-lifecycle.md §3 to §6) ──
;;
;; `with-orders` stands in for credit_orders with an atom, so each test runs
;; without PostgreSQL. It also records every Wayl call and every db write in
;; `events`, in order.

(defn- order-row [user-id kind amount-iqd]
  {:user-id user-id :kind kind :amount amount-iqd :status "pending"})

(defn- stub-complete! [!orders ref]
  (let [o (get @!orders ref)]
    (cond
      (nil? o) {:credited false :reason :unknown}
      (= "complete" (:status o)) {:credited false :reason :already}
      :else (do (swap! !orders assoc-in [ref :status] "complete")
                (if (= "desktop_license" (:kind o))
                  {:credited false :licensed true :amount 0 :user-id (:user-id o)}
                  {:credited true :amount (:amount o) :user-id (:user-id o)})))))

(defn- stub-fail! [!orders ref]
  (if (= "pending" (get-in @!orders [ref :status]))
    (do (swap! !orders assoc-in [ref :status] "failed")
        [{:next.jdbc/update-count 1}])
    [{:next.jdbc/update-count 0}]))

(defn- with-orders*
  "Call `f` with the order store, the event log and the bumps, with every db
   fn the routing calls stubbed. `wayl` maps :invalidate, :get and :create to
   fns of the reference (or of the create-link! options)."
  [orders wayl f]
  (let [!orders (atom orders)
        !events (atom [])
        !bumps (atom [])
        event! (fn [& e] (swap! !events conj (vec e)))]
    (with-redefs [config/credits-enabled? (constantly true)
                  db/get-credit-order
                  (fn [ref]
                    (when-let [o (get @!orders ref)]
                      {:credit_orders/user_id (:user-id o) :credit_orders/kind (:kind o)
                       :credit_orders/amount_iqd (:amount o)}))
                  db/complete-credit-order! (fn [ref] (event! :complete ref) (stub-complete! !orders ref))
                  db/fail-credit-order! (fn [ref] (event! :fail ref) (stub-fail! !orders ref))
                  db/insert-credit-order!
                  (fn [user-id ref amount kind]
                    (event! :insert ref)
                    (swap! !orders assoc ref (order-row user-id kind amount)))
                  db/pending-order-references
                  (fn [user-id kind]
                    (vec (sort (for [[ref o] @!orders
                                     :when (and (= user-id (:user-id o)) (= kind (:kind o))
                                                (= "pending" (:status o)))]
                                 ref))))
                  db/set-order-wayl-fields! (fn [& _] nil)
                  wayl/invalidate-link! (fn [ref] (event! :invalidate ref) ((:invalidate wayl) ref))
                  wayl/get-link-status (fn [ref] (event! :get ref) ((:get wayl) ref))
                  wayl/create-link! (fn [opts] (event! :create (:reference-id opts)) ((:create wayl) opts))
                  storage-meter/top-up-landed! (fn [_] nil)
                  commands/bump! (fn [user-id ch] (swap! !bumps conj [user-id ch]))]
      (f {:orders !orders :events !events :bumps !bumps}))))

(defmacro ^:private with-orders [[binding orders wayl] & body]
  `(with-orders* ~orders ~wayl (fn [~binding] ~@body)))

(def ^:private refuse-invalidate
  (constantly {:ok false :http-status 400 :error "We can't invalidate link that is already [Complete]"}))

(deftest status-routes-to-one-action
  (testing "3.1: the eight Wayl statuses, nil and an unknown string"
    (is (= {"Created" :wait "Pending" :wait "Processing" :wait
            "Complete" :complete "Delivered" :complete
            "Cancelled" :fail "Rejected" :fail "Returned" :fail
            nil :wait "Teleported" :wait}
           (into {} (map (juxt identity credits/status->action))
             ["Created" "Pending" "Processing" "Complete" "Delivered"
              "Cancelled" "Rejected" "Returned" nil "Teleported"])))))

(deftest a-paid-status-credits-once-and-bumps
  (with-orders [{:keys [orders bumps]} {"fm-a" (order-row 50 "credits" 5000)} {}]
    (is (= :credited (:outcome (credits/apply-wayl-status! "fm-a" "Complete" 5000))))
    (is (= "complete" (get-in @orders ["fm-a" :status])))
    (testing "a redelivery credits nothing and bumps nothing"
      (is (= :already (:outcome (credits/apply-wayl-status! "fm-a" "Complete" 5000))))
      (is (= [[50 :credits-granted]] @bumps)))))

(deftest a-desktop-license-bumps-too
  (with-orders [{:keys [bumps]} {"fm-dl-a" (order-row 51 "desktop_license" 9000)} {}]
    (is (= :licensed (:outcome (credits/apply-wayl-status! "fm-dl-a" "Delivered" "9000"))))
    (is (= [[51 :credits-granted]] @bumps))))

(deftest an-amount-mismatch-warns-and-still-credits
  (with-orders [{:keys [orders]} {"fm-a" (order-row 50 "credits" 5000)} {}]
    (let [{:keys [value signals]}
          (tel/with-signals
            (credits/apply-wayl-status! "fm-a" "Complete" "4000"))
          mismatch (filter #(= ::credits/wayl-amount-mismatch (:id %)) signals)]
      (is (= :credited (:outcome value)))
      (is (= "complete" (get-in @orders ["fm-a" :status])))
      (is (= 1 (count mismatch)))
      (is (= :warn (:level (first mismatch))))
      (is (= {:expected-iqd 5000 :wayl-total 4000}
             (select-keys (:data (first mismatch)) [:expected-iqd :wayl-total])))))
  (testing "a matching total, or none, logs nothing"
    (with-orders [_ {"fm-a" (order-row 50 "credits" 5000)
                     "fm-b" (order-row 50 "credits" 5000)} {}]
      (let [{:keys [signals]} (tel/with-signals
                                (credits/apply-wayl-status! "fm-a" "Complete" "5000")
                                (credits/apply-wayl-status! "fm-b" "Complete" nil))]
        (is (empty? (filter #(= ::credits/wayl-amount-mismatch (:id %)) signals)))))))

(deftest a-closed-status-fails-and-a-waiting-one-does-nothing
  (with-orders [{:keys [orders events]} {"fm-a" (order-row 50 "credits" 5000)
                                         "fm-b" (order-row 50 "credits" 5000)} {}]
    (is (= :failed (:outcome (credits/apply-wayl-status! "fm-a" "Rejected" nil))))
    (is (= :not-pending (:outcome (credits/apply-wayl-status! "fm-a" "Cancelled" nil))))
    (is (= :waiting (:outcome (credits/apply-wayl-status! "fm-b" "Processing" nil))))
    (is (= "pending" (get-in @orders ["fm-b" :status])))
    (is (= [[:fail "fm-a"] [:fail "fm-a"]] @events))))

(deftest close-link-settles-the-order
  (testing "3.3: 201 with Cancelled fails the order and makes no GET"
    (with-orders [{:keys [orders events]} {"fm-a" (order-row 50 "credits" 5000)}
                  {:invalidate (constantly {:ok true :status "Cancelled" :total "5000"})}]
      (is (= :failed (:outcome (credits/close-link! "fm-a"))))
      (is (= "failed" (get-in @orders ["fm-a" :status])))
      (is (not-any? #(= :get (first %)) @events))))
  (testing "3.3: 201 with a status that is not paid still fails the order"
    (with-orders [{:keys [orders]} {"fm-a" (order-row 50 "credits" 5000)}
                  {:invalidate (constantly {:ok true :status "Created" :total "5000"})}]
      (is (= :failed (:outcome (credits/close-link! "fm-a"))))
      (is (= "failed" (get-in @orders ["fm-a" :status])))))
  (testing "3.3: 201 with Complete credits the order"
    (with-orders [{:keys [orders]} {"fm-a" (order-row 50 "credits" 5000)}
                  {:invalidate (constantly {:ok true :status "Complete" :total "5000"})}]
      (is (= :credited (:outcome (credits/close-link! "fm-a"))))
      (is (= "complete" (get-in @orders ["fm-a" :status])))))
  (testing "3.3: a 4xx then a GET with Complete credits the order"
    (with-orders [{:keys [orders]} {"fm-a" (order-row 50 "credits" 5000)}
                  {:invalidate refuse-invalidate
                   :get (constantly {:ok true :status "Complete" :total "5000"})}]
      (let [r (credits/close-link! "fm-a")]
        (is (= :credited (:outcome r)))
        (is (true? (:credited r)))
        (is (= "Complete" (:wayl-status r))))
      (is (= "complete" (get-in @orders ["fm-a" :status])))))
  (testing "3.3: a 4xx then a failed GET leaves the order pending"
    (with-orders [{:keys [orders]} {"fm-a" (order-row 50 "credits" 5000)}
                  {:invalidate refuse-invalidate
                   :get (constantly {:ok false :error "Could not reach payment provider"})}]
      (is (= {:ok false :outcome :lookup-failed :error "Could not reach payment provider"}
             (credits/close-link! "fm-a")))
      (is (= "pending" (get-in @orders ["fm-a" :status]))))))

(deftest reconcile-keeps-the-return-page-shape
  (with-orders [_ {"fm-a" (order-row 50 "credits" 5000)}
                {:get (constantly {:ok true :status "Processing" :total "5000"})}]
    (is (= {:ok true :outcome :waiting :wayl-status "Processing" :credited false :licensed false}
           (credits/reconcile-order! "fm-a")))))

(defn- link-for [opts]
  {:ok true :url (str "https://link.thewayl.com/" (:reference-id opts)) :code "c" :link-id "l"})

(deftest a-new-checkout-closes-the-same-kind-first
  (with-orders [{:keys [orders events]}
                {"fm-old" (order-row 50 "credits" 5000)
                 "fm-dl-old" (order-row 50 "desktop_license" 9000)
                 "fm-other" (order-row 77 "credits" 5000)}
                {:invalidate (constantly {:ok true :status "Cancelled" :total "5000"})
                 :create link-for}]
    (let [r (credits/open-order! 50 {:kind "credits" :reference-id "fm-new" :amount-iqd 5000
                                     :base-url "https://h" :currency-suffix? false})]
      (is (= {:ok true :url "https://link.thewayl.com/fm-new"} r)))
    (testing "3.4: close, then insert, then create"
      (is (= [[:invalidate "fm-old"] [:fail "fm-old"] [:insert "fm-new"] [:create "fm-new"]]
             @events)))
    (testing "the closed order failed, and the other kind and other user stay pending"
      (is (= "failed" (get-in @orders ["fm-old" :status])))
      (is (= "pending" (get-in @orders ["fm-dl-old" :status])))
      (is (= "pending" (get-in @orders ["fm-other" :status])))
      (is (= "pending" (get-in @orders ["fm-new" :status]))))))

(deftest a-provider-error-fails-the-new-order
  (with-orders [{:keys [orders]} {}
                {:create (constantly {:ok false :error "Payment provider error"})}]
    (is (= {:ok false :error "Payment provider error"}
           (credits/open-order! 50 {:kind "credits" :reference-id "fm-new" :amount-iqd 5000
                                    :base-url "https://h"})))
    (is (= "failed" (get-in @orders ["fm-new" :status])))))

(defn- set-auth-state! [state]
  (reset! @#'credits/!wayl-auth state))

(deftest a-refused-token-disables-checkout
  (try
    (testing "6.1: :failed refuses before any db or Wayl call"
      (set-auth-state! :failed)
      (with-orders [{:keys [events]} {"fm-old" (order-row 50 "credits" 5000)} {}]
        (is (= {:ok false :error "Payments are unavailable. Try again later."}
               (credits/open-order! 50 {:kind "credits" :reference-id "fm-new" :amount-iqd 5000
                                        :base-url "https://h"})))
        (is (= [] @events))))
    (testing ":unknown lets checkout pass"
      (set-auth-state! :unknown)
      (with-orders [_ {} {:create link-for}]
        (is (:ok (credits/open-order! 50 {:kind "credits" :reference-id "fm-new"
                                          :amount-iqd 5000 :base-url "https://h"})))))
    (finally (set-auth-state! :unknown))))

(deftest the-token-flag-logs-each-change-once
  (try
    (set-auth-state! :unknown)
    (let [answers (atom [{:ok true} {:ok false :error "Unauthorized"}
                         {:ok false :error "Unauthorized"} {:ok true}])
          {:keys [value signals]}
          (with-redefs [config/wayl-merchant-token (constantly "token")
                        wayl/verify-auth-key (fn [] (let [a (first @answers)]
                                                      (swap! answers rest)
                                                      a))]
            (tel/with-signals
              (vec (repeatedly 4 credits/check-wayl-auth!))))
          ids (map :id (filter #(#{::credits/wayl-auth-failed ::credits/wayl-auth-recovered} (:id %))
                         signals))]
      (testing "4.2: :unknown, :ok, :failed, :failed, :ok"
        (is (= [:ok :failed :failed :ok] value)))
      (testing "4.3: two failures in a row log once, and a recovery logs once"
        (is (= [::credits/wayl-auth-failed ::credits/wayl-auth-recovered] ids))
        (is (= :error (:level (first (filter #(= ::credits/wayl-auth-failed (:id %)) signals)))))))
    (finally (set-auth-state! :unknown))))

(deftest no-token-means-no-check
  (with-redefs [config/wayl-merchant-token (constantly nil)
                wayl/verify-auth-key (fn [] (throw (ex-info "must not call" {})))]
    (is (nil? (credits/check-wayl-auth!)))))

(deftest sweep-closes-each-abandoned-link
  (let [row (fn [ref] {:reference_id ref :user_id 50 :kind "credits"
                       :amount_iqd 5000 :created_at "2026-10-30T12:33"
                       :email "buyer@example.com" :display_name "Buyer"
                       :tries 2 :paid 0})
        refs ["fm-unpaid" "fm-paid" "fm-done" "fm-busy" "fm-down"]
        invalidate {"fm-unpaid" {:ok true :status "Cancelled" :total "5000"}}
        statuses {"fm-paid" {:ok true :status "Complete" :total "5000"}
                  "fm-done" {:ok true :status "Complete" :total "5000"}
                  "fm-busy" {:ok true :status "Processing" :total "5000"}
                  "fm-down" {:ok false :error "Could not reach payment provider"}}
        marked (atom [])]
    (with-orders [{:keys [orders bumps]}
                  (assoc (zipmap refs (repeat (order-row 50 "credits" 5000)))
                    "fm-done" (assoc (order-row 50 "credits" 5000) :status "complete"))
                  {:invalidate #(get invalidate % (refuse-invalidate))
                   :get statuses}]
      (with-redefs [db/unalerted-pending-orders
                    (fn [min-age max-age]
                      (is (= [credits/abandon-after-minutes credits/abandon-lookback-hours]
                             [min-age max-age]))
                      (map row refs))
                    db/mark-order-abandon-alerted! (fn [ref] (swap! marked conj ref))]
        (is (= {"fm-unpaid" :abandoned
                "fm-paid" :credited
                "fm-done" :already-complete
                "fm-busy" :still-pending
                "fm-down" :lookup-failed}
               (credits/sweep-abandoned-checkouts!)))
        (testing "4.1: only the closed unpaid order is mailed and marked"
          (is (= ["fm-unpaid"] @marked))
          (is (= "failed" (get-in @orders ["fm-unpaid" :status]))))
        (testing "4.1: a pending result stays pending for the next sweep"
          (is (= "pending" (get-in @orders ["fm-busy" :status])))
          (is (= "pending" (get-in @orders ["fm-down" :status]))))
        (testing "a paid order the buyer never returned from is credited once"
          (is (= [[50 :credits-granted]] @bumps)))))))

(deftest alert-routes-name-the-new-ids
  (let [src (slurp "src/freememo/logging.cljc")]
    (testing "7.1, 7.2: mailed"
      (is (re-find #":freememo\.credits/wayl-amount-mismatch" src))
      (is (re-find #":freememo\.credits/wayl-auth-failed" src)))
    (testing "7.3, 7.4: not mailed"
      (is (not (re-find #"wayl-webhook-received" src)))
      (is (not (re-find #"invalidate-link-refused" src))))))

(deftest sweep-does-nothing-when-credits-are-off
  (with-redefs [config/credits-enabled? (constantly false)
                db/unalerted-pending-orders (fn [& _] (throw (ex-info "must not query" {})))]
    (is (nil? (credits/sweep-abandoned-checkouts!)))))

(deftest abandoned-checkout-ids-are-mailed
  (let [src (slurp "src/freememo/logging.cljc")]
    (is (re-find #":freememo\.credits/checkout-abandoned" src))
    (is (re-find #":freememo\.credits/sweep-credited" src))))
