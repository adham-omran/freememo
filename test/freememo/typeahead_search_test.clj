(ns freememo.typeahead-search-test
  "Late search results cannot replace the current request or selected ID."
  (:require [clojure.test :refer [deftest is]]
            [freememo.typeahead-search :as search]))

(deftest an-old-edit-cannot-supply-current-options
  (let [current {:id "new" :query "quasar" :scope [1 nil]}
        old {:id "old" :query "star" :scope [1 nil]}]
    (is (= [] (search/matching-options current
                {:request old :options [{:id 7 :label "Star"}]})))
    (is (= [{:id 8 :label "Quasar"}]
          (search/matching-options current
            {:request current :options [{:id 8 :label "Quasar"}]})))))

(deftest repeated-text-and-changed-scopes-have-distinct-identities
  (let [old {:id "old" :query "quasar" :scope [1 10]}
        response {:request old :options [{:id 7 :label "Quasar"}]}]
    (is (= [] (search/matching-options (assoc old :id "retry") response)))
    (is (= [] (search/matching-options (assoc old :scope [2 11]) response)))
    (is (= [] (search/matching-options nil response)))))

(deftest missing-responses-have-no-options
  (is (= [] (search/matching-options {:id "new" :query "" :scope nil} nil))))

(deftest selected-labels-belong-to-the-current-id
  (is (= "Selected quasar" (search/matching-label 8 {:id 8 :label "Selected quasar"})))
  (is (nil? (search/matching-label 9 {:id 8 :label "Selected quasar"})))
  (is (nil? (search/matching-label nil {:id 8 :label "Selected quasar"}))))
