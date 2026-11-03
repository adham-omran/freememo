(ns freememo.comprehension-page-test
  "Commands the comprehension page no longer sends."
  (:require [clojure.test :refer [deftest is]]
            [freememo.comprehension-page :as page]))

(deftest extracting-facts-is-not-a-cycle-action
  ;; plans/knowledge-facts-role.md section 4: a tab from before the removal can
  ;; still send [:extract id]. It must reach the case default, not the extractor.
  (is (= {:ok false :error "Unknown cycle action."}
         (page/run-command* 1 [:extract 1]))))
