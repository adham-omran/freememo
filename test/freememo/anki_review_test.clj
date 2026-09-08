(ns freememo.anki-review-test
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.anki-review :as review]))

(deftest remarks-use-the-field-name
  (testing "field order does not affect remark detection"
    (is (review/note-remarked?
          {:Back {:order 4 :value "answer"}
           :Remarks {:order 1 :value "<b>check this</b>"}}))
    (is (review/note-remarked?
          {"Remarks" {"value" "a string-keyed reply"}}))))

(deftest remark-text-is-readable
  (is (= "Check this\n[Image]"
        (review/note-remark
          {:Remarks {:value "<b>Check this</b><br><img src=\"note.png\">"}}))))

(deftest empty-remarks-stay-empty
  (doseq [fields [nil
                  {}
                  {:Remarks {:value ""}}
                  {:Remarks {:value "<div><br></div>"}}
                  {:Remarks {:value "&nbsp;\u00a0&#160;"}}]]
    (is (false? (review/note-remarked? fields))))
  (is (review/note-remarked? {:Remarks {:value "<img src=\"note.png\">"}})
    "an image-only remark has content"))

(deftest pushed-manifest-includes-group-members
  (is (= [{:card-id 1 :note-id 101}
          {:card-id 3 :note-id 103}]
        (review/pushed-manifest
          [{:members [{:flashcards/id 1 :flashcards/anki_note_id 101}
                      {:flashcards/id 2 :flashcards/anki_note_id nil}]}
           {:members [{:flashcards/id 3 :flashcards/anki_note_id 103}]}]))))

(deftest overlay-builds-all-review-filter-sets
  (is (= {:anki-changed #{1}
          :marked #{2 5}
          :suspended #{3 5}
          :remarked #{4 5}}
        (review/overlay->filter-ids
          {1 {:anki-modified true}
           2 {:marked true}
           3 {:suspended {:total 1 :suspended 1}}
           4 {:remarked true}
           5 {:marked true :remarked true
              :suspended {:total 2 :suspended 1}}
           6 {:suspended {:total 1 :suspended 0}}}))))

(deftest sparse-overlay-keeps-anki-review-states
  (is (= {:anki-modified true
          :marked true
          :remarked true
          :remark "Check source"
          :suspended {:total 2 :suspended 1}}
        (review/overlay-flags true ["marked" "custom"] "Check source"
          {:total 2 :suspended 1})))
  (is (= {} (review/overlay-flags false ["custom"] false
              {:total 1 :suspended 0}))))

(deftest display-row-keeps-each-anki-state
  (let [flags (review/row-flags
                [1 2 3]
                {1 {:marked true}
                 2 {:remarked true}
                 3 {:suspended {:total 2 :suspended 1}}})]
    (is (= {:marked true
            :remarked true
            :suspended {:total 2 :suspended 1}}
          flags))
    (is (= " card-anki-marked card-anki-suspended card-anki-remarked card-anki-state"
          (review/row-class flags)))))

(deftest find-next-review-row-wraps
  (let [members [[1] [2 3] [4] [5]]
        overlay {2 {:marked true}
                 3 {:remark "Check source" :remarked true}
                 5 {:marked true :suspended {:total 1 :suspended 1}}}]
    (is (= [1 3] (review/matching-row-indexes members overlay :marked)))
    (is (= [1] (review/matching-row-indexes members overlay :remarked)))
    (is (= [3] (review/matching-row-indexes members overlay :suspended)))
    (is (= 3 (review/next-row-index [1 3] 1)))
    (is (= 1 (review/next-row-index [1 3] 3)))
    (is (nil? (review/next-row-index [] 0)))))
