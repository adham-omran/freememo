(ns freememo.answer-sheet-test
  "The Typst answer sheet sees only what a learner may read before answering."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.answer-sheet :as sheet]))

(def ^:private detail
  {:id 5 :title "Mixed"
   :form {:questions [{:id "q1" :format "multiple-choice" :points 2 :prompt "Pick one."
                       :options [{:id "A" :text "Yes"} "Bare option"]
                       :correct_option_id "A" :grading_guide "guide" :evidence [{:quote "q"}]}
                      {:id "q2" :format "long-essay" :points 10
                       :prompt "ما الفرق بين المحلل اللفظي والمحلل النحوي؟"
                       :rubric {:reasoning "rubric"} :fact_ids [1]}
                      {:id "q3" :prompt "No format or points."}]}})

(deftest sheet-data-carries-no-answer-key
  (let [data (sheet/sheet-data detail)]
    (is (= {:title "Mixed" :cycle 5} (dissoc data :questions)))
    (is (every? #(= #{:number :format :points :prompt :rtl :options} (set (keys %)))
          (:questions data)))
    (is (= [1 2 3] (mapv :number (:questions data))))
    (testing "options keep their ids, and a bare string option has none"
      (is (= [{:id "A" :text "Yes"} {:id "" :text "Bare option"}]
            (:options (first (:questions data))))))
    (testing "an Arabic prompt reads right to left"
      (is (= [false true false] (mapv :rtl (:questions data)))))
    (testing "a question with no format or points still prints"
      (is (= ["long-essay" 0] ((juxt :format :points) (nth (:questions data) 2)))))))

(deftest the-template-never-evaluates-data
  ;; Prompts are model-written. Typst `eval` would run one as markup.
  (is (not (str/includes? (slurp (io/resource "typst/answer-sheet.typ")) "eval("))))
