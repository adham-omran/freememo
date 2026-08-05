(ns freememo.priority-test
  "Contract tests for the priority write boundary.
   `normalize` must absorb every unusable input rather than throw, and
   `round-to` must not drift across the priority domain."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.priority :as priority]))

(deftest normalize-absorbs-unusable-input
  (testing "non-finite and nil collapse to the default"
    (is (= 50.0 (priority/normalize nil)))
    (is (= 50.0 (priority/normalize ##NaN)))
    (is (= 50.0 (priority/normalize ##Inf)))
    (is (= 50.0 (priority/normalize ##-Inf)))
    (is (= 50.0 (priority/normalize "not a number")))))

(deftest normalize-clamps-to-the-domain
  (is (= 0.0 (priority/normalize -5)))
  (is (= 100.0 (priority/normalize 105)))
  (is (= 0.0 (priority/normalize priority/priority-min)))
  (is (= 100.0 (priority/normalize priority/priority-max))))

(deftest normalize-rounds-to-three-places
  (is (= 12.346 (priority/normalize 12.3456)))
  (is (= 12.345 (priority/normalize 12.345)))
  (is (= 50.0 (priority/normalize 50))))

(deftest normalize-is-idempotent
  (testing "a normalized value survives a second pass unchanged"
    (doseq [v [0 12.3456 49.9995 50 99.9994 100 -3 130]]
      (let [once (priority/normalize v)]
        (is (= once (priority/normalize once))
          (str "not idempotent for " v))))))

(deftest round-to-is-exact-across-the-priority-domain
  (testing "every 0.001 step from 0 to 100 is a fixpoint"
    (let [drifted (->> (range 0 100001)
                    (map #(/ % 1000.0))
                    (remove #(= % (priority/round-to % priority/priority-decimals)))
                    (take 5)
                    vec)]
      (is (= [] drifted))))
  (testing "a 4th decimal place resolves to the adjacent 3-place step"
    ;; The fixpoint test above passes for any idempotent function, including a
    ;; wrong one. This feeds values that MUST move, and names where they land.
    (let [wrong (->> (range 0 100000)
                  (mapcat (fn [k]
                            (let [lo (/ k 1000.0)
                                  hi (/ (inc k) 1000.0)]
                              [[(+ lo 0.0004) lo] [(+ lo 0.0006) hi]])))
                  (remove (fn [[in expected]]
                            (= expected (priority/round-to in priority/priority-decimals))))
                  (take 5)
                  vec)]
      (is (= [] wrong)))))

(deftest round-to-honors-other-precisions
  (testing "the stepper's integer controls use decimals 0"
    (is (= 13.0 (priority/round-to 12.6 0)))
    (is (= 12.0 (priority/round-to 12.4 0)))
    (is (= 12.3 (priority/round-to 12.34 1)))))
