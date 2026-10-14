(ns freememo.question-map-view-test
  "The rules of the question map tab (plans/question-map-tab.md §4.3). Pure,
   so no database."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.question-map-view :as view]))

(defn- at [s] (java.sql.Timestamp/valueOf s))

(deftest a-root-has-one-state-by-the-first-match
  (testing "§4.3.3: status beats a mark, and a mark of either source is known"
    (is (= "learned" (view/root-state {:status "learned" :known_sources ["tutor"]})))
    (is (= "next" (view/root-state {:status "next" :known_sources ["self"]})))
    (is (= "known" (view/root-state {:status "open" :known_sources ["self"]})))
    (is (= "known" (view/root-state {:status "open" :known_sources ["tutor"]}))
      "a tutor mark from another map makes the root done")
    (is (= "open" (view/root-state {:status "open" :known_sources []})))))

(deftest the-tab-counts-learned-and-known-roots
  (let [roots [{:status "learned" :known_sources ["tutor"]}
               {:status "open" :known_sources ["tutor"]}
               {:status "next" :known_sources []}
               {:status "open" :known_sources []}]]
    (is (= 2 (view/done-count roots)))
    (is (= "Map 2 of 4" (view/tab-label roots)))))

(deftest the-trail-is-done-then-next-then-open
  (let [roots [{:id 1 :status "open" :known_sources []}
               {:id 2 :status "next" :known_sources []}
               {:id 3 :status "learned" :known_sources ["tutor"]
                :learned_at (at "2026-10-12 10:00:00")}
               {:id 4 :status "open" :known_sources ["self"]
                :known_at (at "2026-10-11 09:00:00")}
               {:id 5 :status "learned" :known_sources []}
               {:id 6 :status "open" :known_sources []}]]
    (testing "§4.3.2: an unrecorded done time first, then oldest first; open roots in tutor order"
      (is (= [5 4 3 2 1 6] (mapv :id (view/trail roots)))))))

(deftest a-citation-shows-its-quote
  (testing "§4.3.5: whitespace collapses and a long quote is cut"
    (is (= "A port declares a signature" (view/quote-label "A port\n  declares a\tsignature")))
    (is (= (str (apply str (repeat 60 "x")) "…") (view/quote-label (apply str (repeat 80 "x"))))))
  (testing "each distinct quote shows once, in its first order"
    (is (= [{:quote "a" :anchor "h"} {:quote "b" :anchor "h"}]
          (view/distinct-citations [{:quote "a" :anchor "h"} {:quote "b" :anchor "h"}
                                    {:quote "a" :anchor "other"}])))))
