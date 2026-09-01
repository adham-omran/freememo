(ns freememo.question-card-parser-test
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.question-card-parser :as parser]))

(deftest parses-one-basic-card-per-nonblank-line
  (is (= {:cards [{:question "What is a string of terminals?"
                   :answer "A sequence of zero or more terminals."}
                  {:question "What do we call a sequence of zero or more terminals?"
                   :answer "A string of terminals"}
                  {:question "How is the string of zero terminals written as?"
                   :answer "ε"}
                  {:question "What is the name for this character ε?"
                   :answer "Epsilon"}]
          :invalid-line-numbers []
          :record-count 4}
        (parser/parse-question-selection
          (str "What is a string of terminals? A sequence of zero or more terminals.\n"
            "What do we call a sequence of zero or more terminals? A string of terminals\n"
            "How is the string of zero terminals written as? ε\n"
            "What is the name for this character ε? Epsilon")))))

(deftest ignores-blank-lines-and-trims-field-edges
  (is (= {:cards [{:question "Question?" :answer "Answer"}]
          :invalid-line-numbers []
          :record-count 1}
        (parser/parse-question-selection "  \n  Question?    Answer  \n\t"))))

(deftest rejects-the-complete-batch-when-any-record-is-invalid
  (testing "zero and multiple question marks"
    (is (= {:cards [] :invalid-line-numbers [2 4] :record-count 4}
          (parser/parse-question-selection
            "Valid? Answer\nNo separator\nAlso valid? Answer\nQuestion? Answer?"))))
  (testing "blank fields"
    (is (= {:cards [] :invalid-line-numbers [1 2] :record-count 2}
          (parser/parse-question-selection "? Answer\nQuestion?")))))

(deftest an-empty-selection-has-no-records
  (is (= {:cards [] :invalid-line-numbers [] :record-count 0}
        (parser/parse-question-selection nil))))
