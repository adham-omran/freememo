(ns freememo.paying-customer-db-test
  "The Paying Customer rule (plans/paying-customer-commitments.md, Q16):
   a `complete` order of either kind in the last 12 months. Granted credits do
   not count. Runs against real rows. SKIPS when no database is reachable."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(use-fixtures :once
  (fn [run]
    (when (db-up?) (db/setup-schema))
    (run)))

(defn- new-user! [email]
  (let [tag (str "paying-" (System/nanoTime))]
    (:id (jdbc/execute-one! db/ds
           ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id" tag email]
           {:builder-fn rs/as-unqualified-maps}))))

(defn- order! [uid kind status age-sql]
  (jdbc/execute! db/ds
    [(str "INSERT INTO credit_orders (user_id, reference_id, amount_iqd, status, kind,
                                      created_at, completed_at)
           VALUES (?, ?, 1000, ?, ?, " age-sql ", " age-sql ")")
     uid (str "ref-" (System/nanoTime) "-" (rand-int 1000000)) status kind]))

(deftest the-rule-counts-complete-orders-of-either-kind-inside-12-months
  (if-not (db-up?)
    (println "SKIP: no database")
    (do
      (testing "a complete credits order inside 12 months counts"
        (let [u (new-user! nil)]
          (order! u "credits" "complete" "now() - interval '30 days'")
          (is (true? (db/paying-customer? u)))))
      (testing "a complete desktop-license order counts"
        (let [u (new-user! nil)]
          (order! u "desktop_license" "complete" "now() - interval '30 days'")
          (is (true? (db/paying-customer? u)))))
      (testing "an order just inside 12 months counts"
        (let [u (new-user! nil)]
          (order! u "credits" "complete" "now() - interval '12 months' + interval '1 hour'")
          (is (true? (db/paying-customer? u)))))
      (testing "an order of 12 months and one day does not count"
        (let [u (new-user! nil)]
          (order! u "credits" "complete" "now() - interval '12 months' - interval '1 day'")
          (is (false? (db/paying-customer? u)))))
      (testing "a pending or failed order does not count"
        (let [u (new-user! nil)]
          (order! u "credits" "pending" "now()")
          (order! u "desktop_license" "failed" "now()")
          (is (false? (db/paying-customer? u)))))
      (testing "granted credits alone do not count"
        (let [u (new-user! nil)]
          (jdbc/execute! db/ds
            ["INSERT INTO credit_transactions (user_id, kind, amount_iqd, balance_after)
              VALUES (?, 'grant', 5000, 5000)" u])
          (is (false? (db/paying-customer? u)))))
      (testing "another user's order does not count"
        (let [u (new-user! nil) other (new-user! nil)]
          (order! other "credits" "complete" "now()")
          (is (false? (db/paying-customer? u))))))))

(deftest support-contact-gives-the-boolean-and-the-account-label
  (if-not (db-up?)
    (println "SKIP: no database")
    (let [with-email (new-user! "ann@example.test")
          no-email (new-user! nil)]
      (order! with-email "credits" "complete" "now()")
      (is (= {:paying? true :account "ann@example.test"} (db/support-contact with-email)))
      (let [c (db/support-contact no-email)]
        (is (false? (:paying? c)))
        (is (re-find #"^paying-" (:account c)) "the username stands in for a missing email")))))
