(ns freememo.graph-filter-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.graph-filter :as filter]))

(def page-a {:predicates [[101 "A"] [102 "B"]] :docs [[10 "First"] [20 "Shared"]]})
(def page-b {:predicates [[102 "B"] [103 "C"]] :docs [[20 "Shared"] [30 "Third"]]})

(deftest unchecking-a-relation-does-not-retarget-its-index-on-the-next-page
  (let [selection (filter/set-checked nil 101 false)]
    (is (= {:mode :exclude :ids #{101}} selection))
    (is (= #{1} (filter/renderer-indexes page-a selection)))
    (is (= #{0 1} (filter/renderer-indexes page-b selection)))
    (is (= nil (filter/set-checked selection 101 true)))))

(deftest explicit-inclusion-keeps-the-same-relation-on-overlapping-pages
  (let [selection (filter/set-checked {:mode :include :ids #{}} 102 true)]
    (is (= #{1} (filter/renderer-indexes page-a selection)))
    (is (= #{0} (filter/renderer-indexes page-b selection)))
    (is (= 1 (filter/selected-count page-b selection)))))

(deftest all-and-none-keep-their-meaning-on-another-page
  (is (nil? (filter/renderer-indexes page-b nil)))
  (is (= #{} (filter/renderer-indexes page-b {:mode :include :ids #{}})))
  (is (= 2 (filter/selected-count page-b nil))))

(deftest unavailable-document-filters-reset-to-all
  (is (= :all (filter/document-source page-b 10)))
  (is (= 20 (filter/document-source page-a 20) (filter/document-source page-b 20)))
  (is (= :all (filter/document-source page-b :all))))

(deftest concept-navigation-starts-on-the-first-page
  (is (= {:entity-id 10 :page 2} (filter/graph-selection 10 {:entity-id 10 :page 2})))
  (is (= {:entity-id 20 :page 0} (filter/graph-selection 20 {:entity-id 10 :page 2})))
  (is (= {:entity-id 10 :page 0}
        (filter/graph-selection 10 (filter/graph-selection 20 {:entity-id 10 :page 2}))))
  (is (= {:entity-id nil :page 0} (filter/graph-selection nil {:entity-id 10 :page 2}))))

(deftest a-smaller-page-cannot-retain-an-out-of-range-connectedness
  (is (= 1 (filter/connectedness 8 1)))
  (is (= 2 (filter/connectedness 2 8))))
