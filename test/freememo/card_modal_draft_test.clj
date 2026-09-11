(ns freememo.card-modal-draft-test
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.card-modals :as modal]))

(deftest copy-identity-includes-card-type
  (let [fields {:question "{{c1::Question}}" :cloze "{{c1::Question}}"
                :answer "Answer"}]
    (is (not= (modal/composed-card "basic" fields)
              (modal/composed-card "cloze" fields)))))

(deftest freeze-retains-selected-fields
  (doseq [kind ["basic" "cloze" "overlapping"]
          side [:none :front :back :both]]
    (testing (str kind " " side)
      (let [primary (atom "Front") answer (atom "Back")
            question (atom "Front") items (atom "Back")
            before (atom "3") reveal (atom false)]
        (modal/clear-add-card-draft!
          side kind primary answer question items before reveal)
        (let [[front back] (if (= kind "overlapping")
                             [@question @items] [@primary @answer])]
          (is (= (if (#{:front :both} side) "Front" "") front))
          (is (= (if (#{:back :both} side) "Back" "") back)))
        (when (= side :both)
          (is (= "3" @before))
          (is (false? @reveal)))))))

(deftest type-conversion-retains-draft-content
  (let [kind (atom "basic") primary (atom "Question") answer (atom "Answer")
        question (atom "") items (atom "")]
    (doseq [next-kind ["overlapping" "cloze" "basic" "overlapping" "basic"]]
      (modal/switch-add-card-kind!
        kind primary answer question items next-kind)
      (is (= next-kind @kind))
      (if (= next-kind "overlapping")
        (is (= ["Question" "Answer"] [@question @items]))
        (is (= ["Question" "Answer"] [@primary @answer]))))))
