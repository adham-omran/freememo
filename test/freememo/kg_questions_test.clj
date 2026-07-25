(ns freememo.kg-questions-test
  "Guards the atomic generator's clustering contracts (plans/quiz-iteration.md §2).

   These are the two places a silent logic error is invisible in the UI: a split
   cluster still produces plausible-looking questions, and a leaked fact id still
   persists a question — both while quietly reintroducing the 'name one of…' defect
   the rewrite exists to remove."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.kg-questions :as kgq]
            [freememo.question-curation :as curate]))

(def ^:private batch-clusters @#'kgq/batch-clusters)
(def ^:private cluster->prompt-row @#'kgq/cluster->prompt-row)

(defn- cluster
  "A cluster whose members are also all its targets (nothing covered yet)."
  [subject fact-ids]
  {:subject_label subject
   :predicate_label "defines"
   :members (mapv (fn [i] {:id i :object_label (str "O" i) :object_entity_id (+ 100 i)})
              fact-ids)
   :targets (set fact-ids)})

(deftest batching-keeps-clusters-whole
  (let [cs [(cluster "S1" [1 2 3]) (cluster "S2" [4])
            (cluster "S3" (range 10 30)) (cluster "S4" [50 51])]
        batches (batch-clusters 15 cs)]
    (testing "packs up to the target cap, splitting between clusters only"
      (is (= [[3 1] [20] [2]]
            (mapv (fn [b] (mapv (comp count :targets) b)) batches))))
    (testing "a cluster wider than the cap forms its own batch rather than splitting"
      (is (= 1 (count (nth batches 1)))))
    (testing "concatenation reproduces input order"
      (is (= (mapv :subject_label cs)
            (mapv :subject_label (apply concat batches)))))
    (testing "no empty batch"
      (is (every? seq batches)))))

(deftest batching-handles-degenerate-input
  (is (= [] (batch-clusters 15 [])))
  (is (= [[1]] (mapv (fn [b] (mapv (comp count :targets) b))
                 (batch-clusters 15 [(cluster "S" [1])])))))

(deftest own-cluster-facts-are-never-discriminators
  ;; Offering the cluster's own fact as a discriminator would put the answer in the
  ;; question stem.
  (let [discriminators {101 [{:fact_id 1 :s "O1" :p "covers" :o "roles"}
                            {:fact_id 999 :s "O1" :p "spans" :o "people"}]
                        102 [{:fact_id 2 :s "O2" :p "covers" :o "workflow"}]}
        row (cluster->prompt-row discriminators (cluster "S1" [1 2 3]))
        members (:members row)]
    (testing "a member's own cluster fact is dropped, outside facts kept"
      (is (= [{:s "O1" :p "spans" :o "people"}]
            (:discriminators (first members)))))
    (testing "a member left with nothing carries no :discriminators key at all —
              this is what the prompt reads as 'omit this target'"
      (is (nil? (:discriminators (second members))))
      (is (nil? (:discriminators (nth members 2)))))))

(deftest prompt-row-cannot-leak-a-discriminator-fact-id
  ;; The model can only return ids it was shown, so stripping :fact_id from
  ;; discriminator rows enforces "link the cluster fact only" structurally rather
  ;; than by instruction.
  (let [discriminators {101 [{:fact_id 777 :s "O1" :p "covers" :o "roles"}]}
        row (cluster->prompt-row discriminators (cluster "S1" [1 2]))]
    (is (every? (fn [m] (every? #(nil? (:fact_id %)) (:discriminators m)))
          (:members row)))
    (is (= [1 2] (:targets row)) "targets are sorted for run-to-run stability")))

(deftest prompt-row-carries-every-sibling-not-just-targets
  ;; A cluster with 3 objects where 2 already have questions must still show all 3,
  ;; or the model cannot tell the remaining one is ambiguous.
  (let [c (-> (cluster "S1" [1 2 3]) (assoc :targets #{3}))
        row (cluster->prompt-row {} c)]
    (is (= 3 (count (:members row))))
    (is (= [3] (:targets row)))))

;; ── Snapshot-vs-live comparison (§9) ────────────────────────────────────────
;; The other silent-failure site: treating an unrecorded wording as "edited" would
;; stamp the marker across all pre-migration history, and treating a real edit as
;; unchanged would hide exactly what the marker exists to show.

(deftest unrecorded-wording-is-never-reported-as-edited
  (testing "nil snapshot — the row predates the column"
    (is (false? (curate/question-edited? nil "anything"))))
  (testing "blank snapshot"
    (is (false? (curate/question-edited? "" "anything")))
    (is (false? (curate/question-edited? "   " "anything")))))

(deftest identical-wording-is-not-edited
  (is (false? (curate/question-edited? "What is X?" "What is X?"))))

(deftest whitespace-only-differences-are-not-edited
  (testing "leading/trailing space"
    (is (false? (curate/question-edited? "  What is X? " "What is X?"))))
  (testing "collapsed runs and newlines — reflowing is not an edit"
    (is (false? (curate/question-edited? "What  is\nX?" "What is X?")))))

(deftest a-real-reword-is-edited
  (is (true? (curate/question-edited? "Name one component." "Which component covers roles?")))
  (testing "a live question emptied out still counts as changed"
    (is (true? (curate/question-edited? "What is X?" "")))
    (is (true? (curate/question-edited? "What is X?" nil)))))
