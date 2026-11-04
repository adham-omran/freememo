(ns freememo.exam-minutes-test
  "The lines beside the exam length slider that do not come from the grid."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.exam-minutes :as minutes]))

(deftest digits-are-grouped-in-threes
  (is (= "0" (minutes/group-digits 0)))
  (is (= "999" (minutes/group-digits 999)))
  (is (= "1,000" (minutes/group-digits 1000)))
  (is (= "1,200,000" (minutes/group-digits 1200000))))

(deftest the-omission-warning-shows-only-when-text-is-left-out
  (testing "nothing while the sheet loads, or when the source fits"
    (is (= "" (minutes/omission-text nil)))
    (is (= "" (minutes/omission-text {:source-characters 5000 :omitted-characters 0})))
    (is (= "" (minutes/omission-text {:none true :source-characters 5000 :omitted-characters 0}))))
  (testing "a sentence with the three figures when text is left out"
    (is (= (str "This source has 1,500,000 characters, and an exam reads at most 1,200,000. "
                "The last 300,000 characters will not be assessed.")
           (minutes/omission-text {:source-characters 1500000 :omitted-characters 300000}))))
  (testing "an unpriced model still warns"
    (is (seq (minutes/omission-text {:none true :source-characters 1500000 :omitted-characters 300000})))))
