(ns freememo.poem-cards-test
  "Tests for the poem word panel's card HTML builders.

   NEEDS NO DATABASE. `freememo.poem-cards` requires nothing but clojure.string,
   so loading this namespace never reaches `freememo.db`, whose HikariCP pool is
   built at namespace load and blocks with no output when PostgreSQL is down.

   Run:
     clj -M:test -e \"(require 'freememo.poem-cards-test) (clojure.test/run-tests 'freememo.poem-cards-test)\""
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [freememo.poem-cards :as pc]))

;; A verse in the shape `freememo.db/get-poem-package` returns (format §3 and
;; §8): keyword keys, string values, `:words` a vector of {:i :half :text}.
;; "قفا" appears TWICE — once in each half — which is the case row 6.2's
;; done-condition and §4 C4.2 are about.
(def verse
  {:n 1
   :sadr "قِفَا نَبْكِ مِنْ ذِكْرَى"
   :ajuz "قِفَا بَيْنَ الدَّخُولِ"
   :words [{:i 0 :half "sadr" :text "قِفَا"}
           {:i 1 :half "sadr" :text "نَبْكِ"}
           {:i 2 :half "sadr" :text "مِنْ"}
           {:i 3 :half "sadr" :text "ذِكْرَى"}
           {:i 4 :half "ajuz" :text "قِفَا"}
           {:i 5 :half "ajuz" :text "بَيْنَ"}
           {:i 6 :half "ajuz" :text "الدَّخُولِ"}]})

(def gloss-notes
  [{:html "<p dir=\"rtl\">قفا: أمر بالوقوف</p>" :label "الدرة، ص 23"}])

(def iraab-notes
  [{:html "<p dir=\"rtl\">فعل أمر مبني على حذف النون</p>" :label "الدرة، ص 24"}])

;; ---------------------------------------------------------------------------

(deftest escape-covers-the-three-markup-characters
  (is (= "&amp;&lt;&gt;" (pc/escape "&<>")))
  (is (= "" (pc/escape nil)))
  (testing "a word carrying no markup is unchanged"
    (is (= "قِفَا" (pc/escape "قِفَا")))))

(deftest word-is-read-by-its-index-field-not-its-position
  (testing "a gap in :i does not shift the lookup"
    (let [words [{:i 0 :half "sadr" :text "أ"} {:i 2 :half "sadr" :text "ج"}]]
      (is (= "ج" (pc/word-text words 2)))
      (is (nil? (pc/word-text words 1))))))

(deftest words-html-emphasises-only-the-tapped-index
  (let [html (pc/words-html (:words verse) 3)]
    (is (str/includes? html "<strong>ذِكْرَى</strong>"))
    (is (= 1 (count (re-seq #"<strong>" html)))))
  (testing "nil emphasises nothing"
    (is (not (str/includes? (pc/words-html (:words verse) nil) "<strong>")))))

(deftest words-html-separates-the-two-halves
  (is (str/includes? (pc/words-html (:words verse) nil) pc/half-gap))
  (testing "a one-half verse gets no trailing gap"
    (is (= "أ ب"
          (pc/words-html [{:i 0 :half "sadr" :text "أ"} {:i 1 :half "sadr" :text "ب"}] nil)))))

(deftest the-same-word-in-two-places-gives-two-different-questions
  ;; Spec row 6.2: "The same word tapped in two verses saves 2 rows". The same
  ;; word tapped twice in ONE verse is the harder case, and the index is what
  ;; separates them — without it both questions hash equal and the flashcard
  ;; uniqueness index drops the second row with no error.
  (let [a (pc/meaning-card verse 0 gloss-notes)
        b (pc/meaning-card verse 4 gloss-notes)]
    (is (not= (:q a) (:q b)))
    (is (str/includes? (:q a) "ما معنى «قِفَا»؟"))
    (is (str/includes? (:q b) "ما معنى «قِفَا»؟"))
    (testing "the emphasis, not the asking line, is what differs"
      (is (str/includes? (:q a) "<strong>قِفَا</strong> نَبْكِ"))
      (is (str/includes? (:q b) (str pc/half-gap "<strong>قِفَا</strong>"))))))

(deftest meaning-and-iraab-cards-on-one-word-differ
  ;; The second collision §4 C4.2 names: one word, two basic cards, one topic.
  (let [m (pc/meaning-card verse 0 gloss-notes)
        i (pc/iraab-card verse 0 iraab-notes)]
    (is (not= (:q m) (:q i)))
    (is (str/includes? (:q i) "أعرب «قِفَا»"))))

(deftest every-block-element-carries-dir-rtl
  ;; Spec row 6.5. Counted rather than sampled: one unmarked <p> renders
  ;; left to right in Anki, whose templates set no dir of their own.
  (doseq [[label html] [["verse :c" (:c (pc/verse-card verse))]
                        ["meaning :q" (:q (pc/meaning-card verse 0 gloss-notes))]
                        ["meaning :a" (:a (pc/meaning-card verse 0 gloss-notes))]
                        ["iraab :q" (:q (pc/iraab-card verse 0 iraab-notes))]
                        ["iraab :a" (:a (pc/iraab-card verse 0 iraab-notes))]
                        ["extract" (pc/verse-extract-html verse)]]]
    (testing label
      (is (= (count (re-seq #"<p\b" html))
            (count (re-seq #"<p dir=\"rtl\"" html)))
        (str label " has a <p> with no dir")))))

(deftest verse-card-hides-the-second-half
  (let [{:keys [c a]} (pc/verse-card verse)]
    (is (nil? a))
    (is (str/includes? c "{{c1::قِفَا بَيْنَ الدَّخُولِ}}"))
    (testing "صدر stays outside the cloze"
      (is (str/starts-with? c "<p dir=\"rtl\">قِفَا نَبْكِ مِنْ ذِكْرَى {{c1::")))
    (testing "the raw halves are used, not the tokenization"
      (is (not (str/includes? c pc/half-gap))))))

(deftest an-answer-names-its-source-after-every-note
  (let [a (:a (pc/meaning-card verse 0
               [{:html "<p dir=\"rtl\">أ</p>" :label "الدرة، ص 23"}
                {:html "<p dir=\"rtl\">ب</p>" :label "التبريزي، ص 4"}]))]
    (is (str/includes? a "— الدرة، ص 23"))
    (is (str/includes? a "— التبريزي، ص 4"))
    (testing "the note html is passed through, not escaped"
      (is (str/includes? a "<p dir=\"rtl\">أ</p>"))))
  (testing "a word with no note gets an empty answer, never invented text"
    (is (= "" (:a (pc/meaning-card verse 0 []))))))

(deftest source-label-tolerates-a-book-with-no-printed-page
  (is (= "الدرة، ص 23" (pc/source-label "الدرة" 23)))
  (is (= "الدرة" (pc/source-label "الدرة" nil))))

(deftest normalize-for-match-ignores-sanitizer-reserialization
  ;; `:add-card` sanitizes every field before the insert, so the stored question
  ;; is equal in meaning and unequal in bytes to the one built here.
  (is (= (pc/normalize-for-match "<p dir=\"rtl\">ما  معنى «قفا»؟</p>")
        (pc/normalize-for-match "<p dir='rtl'>\n ما معنى «قفا»؟\n</p>")))
  (testing "the half gap normalizes to a plain space"
    (is (= "<p>أ ب</p>" (pc/normalize-for-match (str "<p dir=\"rtl\">أ" pc/half-gap "ب</p>")))))
  (testing "emphasis survives, because it is what separates two cards"
    (is (not= (pc/normalize-for-match "<p dir=\"rtl\"><strong>أ</strong> ب</p>")
             (pc/normalize-for-match "<p dir=\"rtl\">أ <strong>ب</strong></p>"))))
  (testing "two different questions stay different"
    (let [a (pc/meaning-card verse 0 gloss-notes)
          b (pc/meaning-card verse 4 gloss-notes)]
      (is (not= (pc/normalize-for-match (:q a))
               (pc/normalize-for-match (:q b)))))))

(deftest extract-title-is-bounded
  (is (= "قِفَا نَبْكِ مِنْ ذِكْرَى" (pc/verse-extract-title verse)))
  (is (>= 80 (count (pc/verse-extract-title {:sadr (str/join (repeat 200 "ا"))})))))
