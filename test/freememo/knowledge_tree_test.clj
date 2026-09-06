(ns freememo.knowledge-tree-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.knowledge-tree :as tree]))

(def ^:private roots-and-child
  [{:topics/id 1 :topics/parent_id nil :topics/title "Alpha"
    :topics/kind "basic" :topics/created_at 1 :has_children true}
   {:topics/id 2 :topics/parent_id nil :topics/title "Beta"
    :topics/kind "basic" :topics/created_at 2 :has_children false}
   {:topics/id 3 :topics/parent_id 1 :topics/title "Child"
    :topics/kind "basic" :topics/created_at 3 :has_children false}])

(def ^:private progress
  {1 {:total-items 2 :done-items 1 :total-cards 2 :synced-cards 1}
   2 {:total-items 1 :done-items 1 :total-cards 1 :synced-cards 1}})

(deftest tree-read-model-contract
  (testing "collapsed roots keep undisclosed-child metadata"
    (let [rows (tree/build-tree-rows roots-and-child progress
                 "" "all" "all" :added :desc {:mode :set :ids #{}})]
      (is (= [2 1] (mapv #(get-in % [:topic :topics/id]) rows)))
      (is (true? (:has-children (second rows))))))
  (testing "expansion adds only the loaded branch"
    (let [rows (tree/build-tree-rows roots-and-child progress
                 "" "all" "all" :document :asc {:mode :set :ids #{1}})]
      (is (= [1 3 2] (mapv #(get-in % [:topic :topics/id]) rows)))))
  (testing "maintained progress drives an exact completion sort"
    (let [rows (tree/build-tree-rows roots-and-child progress
                 "" "all" "all" :done :desc {:mode :set :ids #{}})]
      (is (= [2 1] (mapv #(get-in % [:topic :topics/id]) rows)))))
  (testing "one row DTO excludes the nested source row"
    (let [row (first (tree/build-tree-rows roots-and-child progress
                       "" "all" "all" :document :asc {:mode :set :ids #{}}))
          dto (tree/tree-row-dto row)]
      (is (= 1 (:id dto)))
      (is (nil? (:topic dto)))
      (is (= 2 (:total-items dto))))))
