(ns freememo.card-rows-test
  "Covers freememo.card-rows: the rule that turns flashcard rows into the
   display rows both card tables render. Pure, so it runs without a database —
   which is why the model lives outside db.clj and outside the two tables."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.card-rows :as rows]))

(defn- card
  "A flashcard row with the columns collapse-groups reads."
  [id & {:keys [occ score kind]}]
  (cond-> {:flashcards/id id :flashcards/kind (or kind "basic")}
    occ (assoc :flashcards/occlusion_group_id occ)
    score (assoc :flashcards/score_group_id score)))

(deftest r1-plain-cards-are-their-own-rows
  (testing "a card with no group keys on itself and stands for itself alone"
    (let [out (rows/collapse-groups [(card 1) (card 2)])]
      (is (= [[:card 1] [:card 2]] (mapv :row-key out)))
      (is (= [[1] [2]] (mapv :member-ids out)))
      (is (= 2 (rows/card-count out)))
      (testing "and is not a group row"
        (is (not-any? rows/group-row? out))))))

(deftest r2-occlusion-group-collapses-to-one-row
  (testing "five mask groups of one occlusion become ONE row holding five ids"
    (let [out (rows/collapse-groups
                (mapv #(card % :occ 12 :kind "occlusion") [88 89 90 91 92]))]
      (is (= 1 (count out)))
      (is (= [:occlusion-group 12] (:row-key (first out))))
      (is (= [88 89 90 91 92] (:member-ids (first out))))
      (testing "card-count still reports the Anki-facing number"
        (is (= 5 (rows/card-count out))))
      (testing "and the row is flagged as standing for more than it looks"
        (is (rows/group-row? (first out)))))))

(deftest r3-score-group-collapses-the-same-way
  (testing "both directions of a score group are one row"
    (let [out (rows/collapse-groups
                [(card 70 :score 4 :kind "score") (card 71 :score 4 :kind "score")])]
      (is (= [[:score-group 4]] (mapv :row-key out)))
      (is (= [[70 71]] (mapv :member-ids out))))))

(deftest r4-scope-keywords-separate-equal-ids
  (testing "an occlusion group 5 and a score group 5 never share a row"
    (let [out (rows/collapse-groups
                [(card 1 :occ 5 :kind "occlusion") (card 2 :score 5 :kind "score")])]
      (is (= [[:occlusion-group 5] [:score-group 5]] (mapv :row-key out))))))

(deftest r5-order-is-first-appearance-members-are-id-sorted
  (testing "group order follows the caller's sort; members are ordinal order"
    (let [out (rows/collapse-groups
                [(card 50)
                 (card 92 :occ 12 :kind "occlusion")
                 (card 7)
                 (card 88 :occ 12 :kind "occlusion")])]
      (testing "the group takes the position of its FIRST member"
        (is (= [[:card 50] [:occlusion-group 12] [:card 7]] (mapv :row-key out))))
      (testing "members sort by id — mask-ordinal order for occlusion"
        (is (= [88 92] (:member-ids (second out)))))
      (testing "the representative is the lowest-id member"
        (is (= 88 (:flashcards/id (second out))))))))

(deftest r6-collapse-loses-no-card
  (testing "member ids partition the input exactly"
    (let [in (into [(card 1) (card 2 :score 4 :kind "score") (card 3 :score 4 :kind "score")]
               (mapv #(card % :occ 12 :kind "occlusion") [10 11 12]))
          out (rows/collapse-groups in)]
      (is (= (count in) (rows/card-count out)))
      (is (= (sort (map :flashcards/id in))
            (sort (mapcat :member-ids out)))))))

(deftest r7-members-are-retained-for-server-side-derivation
  (testing ":members holds the full rows the projections derive from"
    (let [out (rows/collapse-groups [(card 88 :occ 12 :kind "occlusion")
                                     (card 89 :occ 12 :kind "occlusion")])]
      (is (= [88 89] (mapv :flashcards/id (:members (first out))))))))

(deftest r8-any-member-is-the-filter-rule
  (let [row (first (rows/collapse-groups
                     [(assoc (card 88 :occ 12 :kind "occlusion") :pushed? false)
                      (assoc (card 89 :occ 12 :kind "occlusion") :pushed? true)]))]
    (testing "one qualifying member shows the whole row"
      (is (rows/any-member? :pushed? row)))
    (testing "no qualifying member hides it"
      (is (not (rows/any-member? #(= :never (:pushed? %)) row))))))

(deftest r9-aggregate-state-folds-to-mixed-on-disagreement
  (let [group (fn [& states]
                (first (rows/collapse-groups
                         (map-indexed (fn [i s]
                                        (assoc (card (+ 80 i) :occ 12 :kind "occlusion")
                                          :st s))
                           states))))]
    (testing "agreement returns the shared state"
      (is (= :synced (rows/aggregate-state :st (group :synced :synced :synced)))))
    (testing "disagreement returns :mixed"
      (is (= :mixed (rows/aggregate-state :st (group :synced :unpushed :synced)))))
    (testing "a lone card returns its own state, never :mixed"
      (is (= :unpushed
            (rows/aggregate-state :st
              (first (rows/collapse-groups [(assoc (card 1) :st :unpushed)]))))))))
