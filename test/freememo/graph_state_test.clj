(ns freememo.graph-state-test
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.graph-state :as graph-state]))

(def edges
  [{:id "0" :source "a" :target "b" :pred 0 :topic 10}
   {:id "1" :source "b" :target "c" :pred 0 :topic 10}
   {:id "2" :source "c" :target "a" :pred 1 :topic 10}
   {:id "3" :source "c" :target "d" :pred 0 :topic 20}
   {:id "4" :source "d" :target "e" :pred 0 :topic 20}])

(deftest filters-before-computing-the-core
  (testing "A source filter removes edges before the k-core pass"
    (is (= {:node-ids #{"a" "b" "c"}
            :edge-ids #{"0" "1" "2"}}
          (graph-state/visible-subgraph
            edges {:source 10 :pred-set nil :min-degree 2 :view-mode :overview}))))
  (testing "A relation filter can remove the full two-core"
    (is (= {:node-ids #{} :edge-ids #{}}
          (graph-state/visible-subgraph
            edges {:source :all :pred-set #{0} :min-degree 2 :view-mode :overview})))))

(deftest limits-a-selected-neighborhood
  (is (= {:node-ids #{"a" "b" "c"}
          :edge-ids #{"0" "1" "2"}}
        (graph-state/visible-subgraph
          edges {:source :all :pred-set nil :min-degree 1
                 :view-mode :neighborhood :selected "a" :depth 1})))
  (is (= {:node-ids #{"a" "b" "c" "d"}
          :edge-ids #{"0" "1" "2" "3"}}
        (graph-state/visible-subgraph
          edges {:source :all :pred-set nil :min-degree 1
                 :view-mode :neighborhood :selected "a" :depth 2}))))

(deftest hides-a-selected-node-that-the-filters-remove
  (is (= {:node-ids #{} :edge-ids #{}}
        (graph-state/visible-subgraph
          edges {:source 20 :pred-set nil :min-degree 1
                 :view-mode :neighborhood :selected "a" :depth 1}))))

(deftest parallel-relations-do-not-inflate-connectedness
  (let [parallel-edges [{:id "0" :source "a" :target "b" :pred 0 :topic 10}
                        {:id "1" :source "a" :target "b" :pred 1 :topic 10}
                        {:id "2" :source "a" :target "b" :pred 2 :topic 20}]]
    (is (= {:node-ids #{} :edge-ids #{}}
          (graph-state/visible-subgraph
            parallel-edges
            {:source :all :pred-set nil :min-degree 2 :view-mode :overview})))))
