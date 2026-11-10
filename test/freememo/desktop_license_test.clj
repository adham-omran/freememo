(ns freememo.desktop-license-test
  "plans/electron-local-db-sync.md §13. Needs PostgreSQL with the server
   schema; each test skips when the database is down."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]
            [freememo.desktop-license :as license])
  (:import [java.time Duration Instant]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- user! [& {:keys [trial-started balance]}]
  (let [id (:id (jdbc/execute-one! db/ds ["INSERT INTO users (username, credit_balance_iqd) VALUES (?, ?) RETURNING id"
                                          (str "license-test-" (System/nanoTime)) (or balance 0)]
                  {:builder-fn rs/as-unqualified-maps}))]
    (when trial-started
      (jdbc/execute! db/ds ["INSERT INTO desktop_accounts (user_id, scm, trial_started_at) VALUES (?, 'x', ?)"
                            id (java.sql.Timestamp/from trial-started)]))
    id))

(defn- license! [user-id]
  (jdbc/execute! db/ds ["INSERT INTO desktop_licenses (user_id, source) VALUES (?, 'credits')" user-id]))

(defn- days-ago ^Instant [now n] (.minus ^Instant now (Duration/ofDays n)))

(deftest a-license-covers-every-release
  (when (db-up?)
    (with-redefs [license/trial-open? false]
     (let [now (Instant/now)
          u (user! :trial-started (days-ago now 400))]
      (license! u)
      (is (= :licensed (:state (license/access db/ds u now))) "a license has no end date")
      (is (= :licensed (:state (license/access db/ds u (.plus now (Duration/ofDays 5000)))))
        "and no version range")))))

(deftest an-open-trial-has-no-end
  (when (db-up?)
    (let [now (Instant/now)]
      (with-redefs [license/trial-open? true]
        (let [u (user! :trial-started (days-ago now 400))
              a (license/access db/ds u now)]
          (is (= {:state :trial :sync :full :ai? true :trial-open? true}
                 (select-keys a [:state :sync :ai? :trial-open?])) "a trial 400 days old still syncs")
          (is (nil? (:trial-ends a)) "the Settings page shows no end date"))))))

(deftest access-moves-with-the-clock
  (when (db-up?)
    (with-redefs [license/trial-open? false]
     (let [now (Instant/now)]
      (testing "covered"
        (let [u (user! :trial-started (days-ago now 400))]
          (license! u)
          (is (= {:state :licensed :sync :full :ai? true}
                 (select-keys (license/access db/ds u now) [:state :sync :ai?])))))
      (testing "trial"
        (let [u (user! :trial-started (days-ago now 10))]
          (is (= {:state :trial :sync :full :ai? true}
                 (select-keys (license/access db/ds u now) [:state :sync :ai?])))))
      (testing "trial over, never bought: nothing syncs (O12)"
        (let [u (user! :trial-started (days-ago now 61))
              a (license/access db/ds u now)]
          (is (= :unlicensed (:state a)))
          (is (not (license/allows? a :download)))))))))

(deftest buying-with-credits-is-one-transaction
  (when (db-up?)
    (with-redefs [license/price-iqd (constantly 1000)]
      (testing "13.1.2: an insufficient balance writes nothing"
        (let [u (user! :balance 999)]
          (is (false? (:ok (license/buy-with-credits! u))))
          (is (zero? (:n (jdbc/execute-one! db/ds ["SELECT count(*) AS n FROM desktop_licenses WHERE user_id = ?" u]
                           {:builder-fn rs/as-unqualified-maps}))))
          (is (= 999 (:credit_balance_iqd (jdbc/execute-one! db/ds ["SELECT credit_balance_iqd FROM users WHERE id = ?" u]
                                            {:builder-fn rs/as-unqualified-maps}))))))
      (testing "13.1: a paid purchase creates exactly one row, with no version columns"
        (let [u (user! :balance 1500)]
          (is (:ok (license/buy-with-credits! u)))
          (is (= [{:source "credits"}]
                 (jdbc/execute! db/ds ["SELECT source FROM desktop_licenses WHERE user_id = ?" u]
                   {:builder-fn rs/as-unqualified-maps})))
          (is (= 500 (:credit_balance_iqd (jdbc/execute-one! db/ds ["SELECT credit_balance_iqd FROM users WHERE id = ?" u]
                                            {:builder-fn rs/as-unqualified-maps})))))))))

(deftest a-wayl-license-order-grants-once
  (when (db-up?)
    (let [u (user!)
          ref (str "fm-dl-test-" (System/nanoTime))]
      (db/insert-credit-order! u ref 52000 "desktop_license")
      (is (:licensed (db/complete-credit-order! ref)) "13.1.1: completion writes the license")
      (is (= :already (:reason (db/complete-credit-order! ref))) "a replayed webhook is a no-op")
      (is (= 1 (:n (jdbc/execute-one! db/ds ["SELECT count(*) AS n FROM desktop_licenses WHERE user_id = ?" u]
                     {:builder-fn rs/as-unqualified-maps}))))
      (is (zero? (:credit_balance_iqd (jdbc/execute-one! db/ds ["SELECT credit_balance_iqd FROM users WHERE id = ?" u]
                                        {:builder-fn rs/as-unqualified-maps})))
        "a license order credits nothing"))))
