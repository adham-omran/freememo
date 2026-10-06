(ns freememo.card-filter-test
  "plans/card-table-filter.md §1.1."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.card-filter :as cf]
   [freememo.card-rows :as card-rows]))

(defn- card [id kind & {:as fields}]
  (merge {:flashcards/id id :flashcards/kind kind
          ;; Pushed and settled: synced unless a test says otherwise.
          :flashcards/anki_note_id (+ 1000 id)
          :flashcards/local_current_digest "d" :flashcards/local_baseline_digest "d"}
    fields))

(def ^:private rows
  (card-rows/collapse-groups
    [(card 1 "basic" :flashcards/question "What is <code>-hfql-resolve</code>?"
       :flashcards/answer "A <b>multimethod</b>")
     (card 2 "cloze" :flashcards/cloze "{{c1::GDPR}} restricts identifiers"
       :flashcards/anki_note_id nil)
     ;; Two members of one occlusion group: only the SECOND holds the word.
     (card 3 "occlusion" :flashcards/occlusion_group_id 7 :flashcards/question "Mask one")
     (card 4 "occlusion" :flashcards/occlusion_group_id 7 :flashcards/question "Mask two Injective"
       :flashcards/local_current_digest "moved")]))

(defn- ids [filters]
  (mapv :flashcards/id (cf/filter-rows filters rows (mapv cf/row-search-text rows))))

(deftest blank-filters-keep-every-row
  (is (= [1 2 3] (ids {})))
  (is (= [1 2 3] (ids {:text "   " :kind nil :unsynced? false})))
  (is (not (cf/active? {:text " "}))))

(deftest text-is-case-insensitive-and-ignores-tags
  (is (= [1] (ids {:text "HFQL-Resolve"})))
  (is (= [1] (ids {:text "a multimethod"})) "the <b> tag does not split the match")
  (is (= [] (ids {:text "code"})) "tag names are not text"))

(deftest text-covers-question-answer-and-cloze
  (is (= [1] (ids {:text "multimethod"})))
  (is (= [2] (ids {:text "gdpr"}))))

(deftest a-group-matches-through-any-member
  (is (= [3] (ids {:text "injective"})))
  (testing "the unsynced test also reads every member"
    (is (= [2 3] (ids {:unsynced? true})))))

(deftest kind-filter
  (is (= [2] (ids {:kind "cloze"})))
  (is (= [3] (ids {:kind "occlusion"}))))

(deftest filters-combine-with-and
  (is (= [3] (ids {:text "mask" :kind "occlusion" :unsynced? true})))
  (is (= [] (ids {:text "gdpr" :kind "basic"})))
  (is (= [] (ids {:text "multimethod" :unsynced? true}))))
