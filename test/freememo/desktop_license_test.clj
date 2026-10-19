(ns freememo.desktop-license-test
  "plans/electron-local-db-sync.md §13. Needs PostgreSQL with the server
   schema; each test skips when the database is down."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]
            [freememo.desktop-license :as license]
            [freememo.desktop-version :as desktop-version])
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

(defn- license! [user-id from to]
  (jdbc/execute! db/ds ["INSERT INTO desktop_licenses (user_id, major_from, major_to, source) VALUES (?, ?, ?, 'credits')"
                        user-id from to]))

(defn- days-ago ^Instant [now n] (.minus ^Instant now (Duration/ofDays n)))

(deftest a-license-covers-its-major-and-the-next
  (when (db-up?)
    (with-redefs [license/trial-open? false]
     (let [now (Instant/now)
          u (user! :trial-started (days-ago now 400))]
      (license! u 1 2)
      (is (= :licensed (:state (license/access db/ds u 2 now))) "13.2: major_from 1 passes major 2")
      (is (not= :licensed (:state (license/access db/ds u 3 now))) "13.2: and fails major 3")))))

(deftest an-open-trial-has-no-end
  (when (db-up?)
    (let [now (Instant/now)]
      (with-redefs [license/trial-open? true]
        (let [u (user! :trial-started (days-ago now 400))
              a (license/access db/ds u 1 now)]
          (is (= {:state :trial :sync :full :ai? true :trial-open? true}
                 (select-keys a [:state :sync :ai? :trial-open?])) "a trial 400 days old still syncs")
          (is (nil? (:trial-ends a)) "the Settings page shows no end date"))
        (let [u (user! :trial-started (days-ago now 400))]
          (license! u 1 2)
          (is (= :trial (:state (license/access db/ds u 3 now))) "an uncovered major is not in grace"))))))

(deftest access-moves-with-the-clock
  (when (db-up?)
    (with-redefs [license/trial-open? false]
     (let [now (Instant/now)]
      (testing "covered"
        (let [u (user! :trial-started (days-ago now 400))]
          (license! u 1 2)
          (is (= {:state :licensed :sync :full :ai? true}
                 (select-keys (license/access db/ds u 1 now) [:state :sync :ai?])))))
      (testing "trial"
        (let [u (user! :trial-started (days-ago now 10))]
          (is (= {:state :trial :sync :full :ai? true}
                 (select-keys (license/access db/ds u 1 now) [:state :sync :ai?])))))
      (testing "in grace: download only, no AI (13.4.2, O13)"
        (let [u (user! :trial-started (days-ago now 400))]
          (license! u 1 2)
          (let [a (license/access db/ds u 3 now)]
            (is (= {:state :grace :sync :download :ai? false} (select-keys a [:state :sync :ai?])))
            (is (license/allows? a :download))
            (is (not (license/allows? a :upload)))
            (is (not (license/allows? a :ai))))))
      (testing "past grace: refused with a reason (13.4.3)"
        (let [u (user! :trial-started (days-ago now 400))]
          (license! u 1 2)
          (license/access db/ds u 3 (days-ago now 31))
          (let [a (license/access db/ds u 3 now)]
            (is (= :expired (:state a)))
            (is (= #{:meta} (set (filter #(license/allows? a %)
                                   [:meta :graves :upload :download :blob :finish :full-upload :full-download :ai]))))
            (is (= "license-expired" (license/reason a))))))
      (testing "trial over, never bought: nothing syncs (O12)"
        (let [u (user! :trial-started (days-ago now 61))
              a (license/access db/ds u 1 now)]
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
      (testing "13.1: a paid purchase creates exactly one row for this major and the next"
        (let [u (user! :balance 1500)]
          (is (:ok (license/buy-with-credits! u)))
          (is (= [{:major_from desktop-version/desktop-major :major_to (inc desktop-version/desktop-major)}]
                 (jdbc/execute! db/ds ["SELECT major_from, major_to FROM desktop_licenses WHERE user_id = ?" u]
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
