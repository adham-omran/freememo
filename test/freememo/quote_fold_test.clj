(ns freememo.quote-fold-test
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.quote-anchor :as anchor]
            [freememo.quote-fold :as fold]))

(deftest guard-characters-fold-away
  (testing "3.1.2 — Quill's U+FEFF embed guards do not change the folded text"
    (is (= (fold/normalize "x \\(n+1\\) y")
          (fold/normalize "x \ufeff\\(n+1\\)\ufeff y"))))
  (testing "a no-break space folds as a space does"
    (is (= (fold/normalize "a b") (fold/normalize "a\u00a0b")))))

(deftest one-definition
  (testing "3.1.1 — quote-anchor normalizes through quote-fold"
    (let [s (str "\u201cA\u201d \ufeff b\u00a0c")]
      (is (= (fold/normalize s) (anchor/normalize s)))
      (is (= "\"A\"bc" (anchor/normalize s))))))

(deftest folding-never-expands
  (let [s (apply str (map char (range 0 0x3100)))]
    (is (<= (count (fold/normalize s)) (count s)))))
