(ns freememo.assistant-reference-choice-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.assistant-reference-choice :as choice]
            [freememo.assistant-composer :as composer]))

(def selected {:id 7 :title "Chosen document" :mention-query "Chosen"})

(deftest a-rendered-choice-carries-identity-and-query
  (let [captured (choice/capture selected)]
    (is (= {:id 7 :title "Chosen document" :query "Chosen"}
          (select-keys captured [:id :title :query])))
    (is (= {:id 7 :title "Chosen document"} (choice/for-draft captured "Read @Chosen")))
    (is (nil? (choice/for-draft captured "Read @Changed")))
    (is (not= (:request-id captured) (:request-id (choice/capture selected))))))

(deftest a-newer-draft-is-not-replaced-by-an-older-pick
  (let [picked (choice/capture selected)
        refs (atom []) draft (atom "Read @Changed") index (atom 4) open (atom true)]
    (is (nil? (composer/apply-reference-choice! picked refs draft index open)))
    (is (= "Read @Changed" @draft))
    (is (= [] @refs))
    (is (= 4 @index))
    (is (true? @open))))

(deftest selection-uses-the-rendered-id-even-if-the-result-order-changes
  (let [picked (choice/capture selected)
        refs (atom []) draft (atom "Read @Chosen") index (atom 0) open (atom true)
        later-options [{:id 99 :title "Another match" :mention-query "Chosen"} selected]]
    (is (= 99 (:id (first later-options))))
    (is (true? (composer/apply-reference-choice! picked refs draft index open)))
    (is (= [{:id 7 :title "Chosen document"}] @refs))
    (is (= "Read @Chosen document " @draft))
    (is (false? @open))))
