(ns freememo.cloze-test
  "Covers freememo.cloze/validate: nested clozes, hints, stray/literal braces,
   numbering rules. V1 is the regression that motivated the nesting-aware scan —
   a c3 wrapping c1 and c2 with a hint was wrongly flagged 'Unclosed'."
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
