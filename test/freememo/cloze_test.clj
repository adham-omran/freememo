(ns freememo.cloze-test
  "Covers freememo.cloze/validate: nested clozes, hints, stray/literal braces,
   numbering rules. V1 is the regression that motivated the nesting-aware scan —
   a c3 wrapping c1 and c2 with a hint was wrongly flagged 'Unclosed'.

   Also covers ords / mask-ord / answer-for-ord, which the quiz's one-item-per-
   deletion draw depends on (plans/cards-in-quiz-queue.md D7)."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.cloze :as cloze]))

(defn- valid? [text] (nil? (cloze/validate text)))

(deftest v1-nested-with-hint
  (testing "c3 wrapping c1 and c2, with a hint on c3 — the motivating case"
    (is (valid? "Components of all types can be {{c3:: {{c1::generic}} or can be {{c2::variants of generic}}:: ... or can be ...}}"))))

(deftest v2-simple
  (is (valid? "{{c1::x}}")))

(deftest v3-two-adjacent
  (is (valid? "{{c1::x}} {{c2::y}}")))

(deftest v4-unclosed
  (is (= "Unclosed cloze: a {{cN::...}} is missing its closing }}"
        (cloze/validate "{{c1::x"))))

(deftest v5-trailing-stray-braces
  (testing "literal }} after all clozes (Java block close) is ignored, not miscounted"
    (is (valid? "{{c1::x}} java }}"))))

(deftest v6-no-cloze
  (is (= "No cloze deletion: select text and press the {+} button to add {{c1::...}}"
        (cloze/validate "plain text"))))

(deftest v7-non-sequential
  (is (= "Non-sequential cloze numbers: found (1 3), expected 1 to 3"
        (cloze/validate "{{c1::x}} {{c3::y}}"))))

(deftest v8-duplicate-numbers
  (testing "the same cloze number may appear more than once"
    (is (valid? "{{c1::x}} {{c1::y}} {{c2::z}}"))))

(deftest v9-deep-nesting
  (is (valid? "{{c1:: {{c2:: {{c3::x}} }} }}")))

(deftest v10-leading-stray-braces
  (testing "literal }} before any opener is ignored"
    (is (valid? "}} {{c1::x}}"))))

(deftest empty-and-nil
  (testing "nil and empty are treated as no-cloze, not crashes"
    (is (= "No cloze deletion: select text and press the {+} button to add {{c1::...}}"
          (cloze/validate nil)))
    (is (= "No cloze deletion: select text and press the {+} button to add {{c1::...}}"
          (cloze/validate "")))))

(deftest empty-cloze-content
  (testing "an empty deletion {{c1::}} (as insert-cloze! creates) is valid"
    (is (valid? "{{c1::}}"))))

;; --- Quiz item extraction --------------------------------------------------

(def ^:private nested
  "V1's shape: c3 wraps c1 and c2 and carries a hint."
  "Types can be {{c3:: {{c1::generic}} or {{c2::variants of generic}}:: a hint}}")

(deftest ords-are-sorted-and-distinct
  (is (= [1 2 3] (cloze/ords nested)))
  (is (= [1 2] (cloze/ords "{{c1::x}} and {{c1::y}} and {{c2::z}}")))
  (is (= [] (cloze/ords "plain text")))
  (testing "an unclosed opener still declares its ord"
    (is (= [1] (cloze/ords "{{c1::x")))))

(deftest mask-hides-one-ord-and-reveals-the-rest
  (is (= "Types can be  [...] or variants of generic" (cloze/mask-ord nested 1)))
  (is (= "Types can be [...]" (cloze/mask-ord nested 3)))
  (testing "every span bearing the ord is hidden"
    (is (= "[...] and [...] and z"
          (cloze/mask-ord "{{c1::x}} and {{c1::y}} and {{c2::z}}" 1))))
  (testing "surrounding markup is untouched"
    (is (= "<p>a <b>[...]</b> z</p>"
          (cloze/mask-ord "<p>a <b>{{c1::bold}}</b> z</p>" 1))))
  (testing "text with no resolvable span is returned verbatim"
    (is (= "plain text" (cloze/mask-ord "plain text" 1)))
    (is (= "{{c1::x" (cloze/mask-ord "{{c1::x" 1)))))

(deftest answer-is-the-hidden-text
  (is (= "generic" (cloze/answer-for-ord nested 1)))
  (testing "a wrapping deletion answers with its nested deletions revealed, no hint"
    (is (= "generic or variants of generic" (cloze/answer-for-ord nested 3))))
  (testing "repeated ords join"
    (is (= "x / y" (cloze/answer-for-ord "{{c1::x}} and {{c1::y}}" 1))))
  (testing "an absent ord answers empty"
    (is (= "" (cloze/answer-for-ord nested 9)))))

(deftest math-regions-survive
  (testing "TeX }} does not close a deletion, and the formula stays in the answer"
    (let [t "The value is {{c1::\\(x^{a^{b}}\\)}} exactly"]
      (is (= [1] (cloze/ords t)))
      (is (= "The value is [...] exactly" (cloze/mask-ord t 1)))
      (is (= "\\(x^{a^{b}}\\)" (cloze/answer-for-ord t 1))))))
