(ns freememo.loading-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.loading :as loading]))

(deftest region-phase-contract
  (testing "cold state"
    (is (= :cold
          (loading/region-phase
            {:value ::loading/no-region-value
             :settled-key ::not-settled
             :error nil}
            :request-a))))
  (testing "ready state"
    (is (= :ready
          (loading/region-phase
            {:value {:count 2}
             :settled-key :request-a
             :error nil}
            :request-a))))
  (testing "refresh keeps the last value"
    (is (= :refreshing
          (loading/region-phase
            {:value {:count 2}
             :settled-key :request-a
             :error nil}
            :request-b))))
  (testing "cold failure is distinct from an empty result"
    (is (= :failed
          (loading/region-phase
            {:value ::loading/no-region-value
             :settled-key :request-a
             :error "database unavailable"}
            :request-a))))
  (testing "refresh failure keeps the last value"
    (is (= :stale-error
          (loading/region-phase
            {:value {:count 2}
             :settled-key :request-b
             :error "database unavailable"}
            :request-b))))
  (testing "server-sited collections use readiness metadata"
    (is (= :ready
          (loading/region-phase
            {:has-value? true :settled-key :request-a :error nil}
            :request-a)))
    (is (= :refreshing
          (loading/region-phase
            {:has-value? true :settled-key :request-a :error nil}
            :request-b)))))

(deftest keyed-region-results
  (is (= {:ok? true :request-key :request-a :value {:count 1}}
        (loading/region-success :request-a {:count 1})))
  (is (= {:ok? false :request-key :request-a :error "failed"}
        (loading/region-failure :request-a "failed"))))
