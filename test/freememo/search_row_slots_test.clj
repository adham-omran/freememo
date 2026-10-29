(ns freememo.search-row-slots-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.search-page :as search]
            [freememo.search-result-row :as row]))

(deftest empty-and-invalid-queries-retain-one-hidden-result
  (let [row {:id 1 :title "First" :kind "markdown" :snippet "<mark>First</mark>"}
        initial (search/result-slot-state nil 10 nil)
        visible (search/result-slot-state initial 10 row)
        empty (search/result-slot-state visible 10 nil)
        invalid (search/result-slot-state empty 10 nil)]
    (is (= {:user-id 10 :visible? false :row nil} initial))
    (is (:visible? visible))
    (is (= row (:row empty) (:row invalid)))
    (is (false? (:visible? empty)))
    (is (false? (:visible? invalid)))))

(deftest a-recycled-result-replaces-the-complete-action-target
  (let [old {:id 1 :parent-id nil :title "First" :kind "markdown" :snippet "First"}
        replacement {:id 20 :parent-id 19 :title "Second" :kind "page" :snippet "Second"}
        hidden (search/result-slot-state (search/result-slot-state nil 10 old) 10 nil)
        recycled (search/result-slot-state hidden 10 replacement)]
    (is (= replacement (:row recycled)))
    (is (:visible? recycled))
    (is (= {:id 20 :parent-id 19 :kind "page"}
          (select-keys (:row recycled) [:id :parent-id :kind])))))

(deftest a-new-account-discards-hidden-results
  (is (= {:user-id 11 :visible? false :row nil}
        (search/result-slot-state
          (search/result-slot-state nil 10 {:id 1 :title "Private"}) 11 nil))))

(deftest rendered-targets-use-the-current-result-and-kind
  (is (= {:kind :topic
          :subjects [{:topic-id 20 :title "Current" :topic-kind "page" :root? false :deletable? false}]
          :surface {:origin :search}}
        (row/target {:id 20 :parent-id 19 :title "Current" :kind "page"})))
  (is (= {:topic-id 1 :title "" :topic-kind "markdown" :root? true :deletable? true}
        (first (:subjects (row/target {:id 1 :kind "markdown"}))))))
