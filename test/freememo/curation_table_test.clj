(ns freememo.curation-table-test
  "The pure halves of the Knowledge curation tables
   (plans/knowledge-tables-selection-and-bulk.md §1.3, §1.4.3, §3.3): the server
   sort, the selection counts the batch bar shows, and the sort grammar of the URL."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.knowledge-scope :as scope]
   [freememo.tape-table :as tape]))

(def ^:private rows
  [{:id 1 :label "beta" :n 3}
   {:id 2 :label "Alpha" :n nil}
   {:id 3 :label "gamma" :n 1}
   {:id 4 :label "alpha" :n 3}])

(deftest sort-rows-orders-without-case-and-puts-nil-last
  (testing "strings compare without case, and equal keys keep input order"
    (is (= [2 4 1 3] (mapv :id (tape/sort-rows rows :label :asc))))
    (is (= [3 1 2 4] (mapv :id (tape/sort-rows rows :label :desc)))))
  (testing "a nil key sorts last in both directions"
    (is (= [3 1 4 2] (mapv :id (tape/sort-rows rows :n :asc))))
    (is (= [1 4 3 2] (mapv :id (tape/sort-rows rows :n :desc)))))
  (testing "every row is kept once"
    (is (= (set rows) (set (tape/sort-rows rows :n :desc))))
    (is (= [] (tape/sort-rows [] :n :asc)))))

(deftest selection-counts-split-shown-from-hidden
  (is (= {:shown 0 :hidden 0} (tape/selection-counts rows :id #{})))
  (is (= {:shown 2 :hidden 0} (tape/selection-counts rows :id #{1 3})))
  (testing "a selected id the result does not hold counts as hidden"
    (is (= {:shown 1 :hidden 2} (tape/selection-counts rows :id #{1 99 100}))))
  (testing "the id key is the caller's"
    (is (= {:shown 1 :hidden 0}
          (tape/selection-counts [{:entity_id 7}] :entity_id #{7})))))

(deftest sort-param-round-trips-and-rejects-bad-input
  (let [cols #{:facts :page}
        default [:page :asc]]
    (testing "a sort the URL wrote reads back"
      (is (= [:facts :desc]
            (scope/parse-sort (scope/sort-param [:facts :desc] default) cols default))))
    (testing "the default sort leaves the URL clean"
      (is (nil? (scope/sort-param [:page :asc] default)))
      (is (nil? (scope/sort-param [nil nil] [nil nil]))))
    (testing "a half-written pair writes nothing rather than throwing"
      (is (nil? (scope/sort-param [:facts nil] default))))
    (testing "an unknown column, an unknown direction or junk falls back"
      (is (= default (scope/parse-sort "title:asc" cols default)))
      (is (= default (scope/parse-sort "facts:up" cols default)))
      (is (= default (scope/parse-sort "::" cols default)))
      (is (= default (scope/parse-sort nil cols default))))))

(deftest flag-params-are-one-or-absent
  (is (= "1" (scope/flag-param true)))
  (is (nil? (scope/flag-param false)))
  (is (scope/flag-set? {"flagged" "1"} "flagged"))
  (is (not (scope/flag-set? {"flagged" "true"} "flagged")))
  (is (not (scope/flag-set? {} "flagged"))))
