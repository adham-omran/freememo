(ns freememo.run-once-test
  "db/run-once! runs a data migration at most once per database, and a failed
   run leaves no marker. Needs PostgreSQL; skips when it is down."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [freememo.db :as db]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(deftest a-migration-runs-once-and-a-failed-one-runs-again
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [id (str "run-once-test-" (System/nanoTime))
          !runs (atom 0)]
      (try
        (testing "the first call runs it, the second does not"
          (is (true? (db/run-once! id (fn [_] (swap! !runs inc)))))
          (is (nil? (db/run-once! id (fn [_] (swap! !runs inc)))))
          (is (= 1 @!runs)))
        (testing "a failure rolls the marker back, so the next call runs it"
          (let [failing (str id "-failing")]
            (is (thrown? clojure.lang.ExceptionInfo
                  (db/run-once! failing (fn [_] (throw (ex-info "boom" {}))))))
            (is (true? (db/run-once! failing (fn [_] :ok))))
            (jdbc/execute! db/ds ["DELETE FROM app_migrations WHERE id = ?" failing])))
        (finally
          (jdbc/execute! db/ds ["DELETE FROM app_migrations WHERE id = ?" id]))))))
