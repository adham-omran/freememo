(ns freememo.assistant-capture-test
  "Guards what ✦ cards out of an assistant reply.

   The rule is keyed on the reply's TEXT, not on the chat's mode: a reply can be
   captured long after a chat's persona was switched, and the Socratic persona's
   two headings are a MUST the model can still ignore. These cases pin both
   halves — anchored replies lose their question, everything else passes whole."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.assistant]))

(def ^:private capture #'freememo.assistant/cardable-text)

(def ^:private socratic-reply
  (str "**Where you are**\n\n"
    "A symbol table maps a name to its attributes.\n\n"
    "**Consider next**\n\n"
    "What does the parser do when a name is absent from it?"))

(deftest anchored-reply-cards-feedback-only
  (testing "the feedback body survives"
    (is (= "A symbol table maps a name to its attributes."
          (capture socratic-reply))))
  (testing "the Socratic question is never carded"
    (is (not (clojure.string/includes? (capture socratic-reply) "Consider next")))
    (is (not (clojure.string/includes? (capture socratic-reply) "What does the parser")))))

(deftest anchored-reply-without-question-anchor-runs-to-end
  (is (= "Only feedback here."
        (capture "**Where you are**\n\nOnly feedback here."))))

(deftest anchorless-reply-cards-whole-text
  (testing "a General reply — direct answer with a citation"
    (let [reply "A symbol table maps each name to its attributes (page 12)."]
      (is (= reply (capture reply)))))
  (testing "a Tutor reply — unstructured prose, no headings"
    (let [reply "Start from what a name has to carry. What would you need to look up?"]
      (is (= reply (capture reply)))))
  (testing "surrounding whitespace is trimmed"
    (is (= "Answer." (capture "\n  Answer.  \n")))))

(deftest empty-feedback-section-yields-blank
  (testing "reply->cards! rejects blank, so an all-question Socratic reply saves nothing"
    (is (clojure.string/blank?
          (capture "**Where you are**\n\n**Consider next**\n\nWhat comes first?")))))

(deftest nil-and-blank-input-are-blank
  (is (= "" (capture nil)))
  (is (clojure.string/blank? (capture "   "))))
