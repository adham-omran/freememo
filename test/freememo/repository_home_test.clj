(ns freememo.repository-home-test
  "The pure parts of the repository home (plans/repository-home.md): which
   kinds are forge structure, and how a walkthrough is named."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.forge-kinds :as forge-kinds]
            [freememo.topic-kinds :as topic-kinds]
            [freememo.walkthrough-name :as wname]))

(deftest only-forge-structure-loses-the-editor
  (testing "§4.1.1: the repository, commit and directory kinds, and no other"
    (is (= #{"repo" "commit" "dir"} (set (filter forge-kinds/structure-kind? topic-kinds/all)))))
  (is (not (forge-kinds/structure-kind? "page")) "a PDF page holds content and cards"))

(def ^:private steps
  [{:label "`hold` op" :noise? false}
   {:label "rename cycle" :noise? false}
   {:label "Other changes" :noise? false}
   {:label "Noise" :noise? true}])

(deftest a-walkthrough-is-named-after-its-first-step-until-named
  (testing "§4.3.2: the first taught step, and how many more"
    (is (= "hold op and 1 more" (wname/derived-name steps "a" "b")))
    (is (= "hold op" (wname/derived-name (take 1 steps) "a" "b"))))
  (testing "only Other changes and Noise: the first of them"
    (is (= "Other changes" (wname/derived-name (drop 2 steps) "a" "b"))))
  (testing "no step yet: the range"
    (is (= "1234567 → abcdef0" (wname/derived-name [] "1234567890" "abcdef0123"))))
  (testing "a set name wins"
    (is (= "Mine" (wname/display-name "Mine" steps "a" "b")))
    (is (= "hold op and 1 more" (wname/display-name nil steps "a" "b")))))

(deftest a-typed-name-is-trimmed-cleared-or-refused
  (is (= {:name "Hold and loop"} (wname/normalize-name "  Hold and loop ")))
  (is (= {:name nil} (wname/normalize-name "   ")) "blank clears the name")
  (is (= {:name nil} (wname/normalize-name nil)))
  (is (= {:name (apply str (repeat 120 "x"))} (wname/normalize-name (apply str (repeat 120 "x")))))
  (is (:error (wname/normalize-name (apply str (repeat 121 "x"))))))
