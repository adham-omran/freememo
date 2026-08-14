(ns freememo.anki-reconcile-test
  "Covers the reconcile classifier and the content digests.

   Needs no database and no AnkiConnect — that is the point of both namespaces
   being pure. `freememo.anki-digest` requires `freememo.anki-sync-helpers` and
   `freememo.math`, neither of which reaches `freememo.db`, so this suite runs
   with PostgreSQL down."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.anki-digest :as digest]
            [freememo.anki-fields :as af]
            [freememo.anki-sync-helpers :as helpers]
            [freememo.anki-reconcile :as rec]))

;; ── classify: every outcome in the contract table ───────────────────────────

(def ^:private A "aaa")
(def ^:private B "bbb")

(defn- c [m] (rec/classify m))

(deftest t1-unchanged
  (is (= :unchanged (c {:anki-now A :anki-baseline A :local-now B :local-baseline B}))))

(deftest t2-push-when-only-local-moved
  (is (= :push (c {:anki-now A :anki-baseline A :local-now "new" :local-baseline B}))))

(deftest t3-pull-when-only-anki-moved
  (is (= :pull (c {:anki-now "new" :anki-baseline A :local-now B :local-baseline B}))))

(deftest t4-conflict-when-both-moved
  (is (= :conflict (c {:anki-now "x" :anki-baseline A :local-now "y" :local-baseline B}))))

(deftest t5-create-when-never-synced
  (testing "no note in Anki and no baseline is a card that has never been pushed"
    (is (= :create (c {:anki-now :absent :anki-baseline nil
                       :local-now B :local-baseline nil})))))

(deftest t6-delete-local-when-anki-note-gone-and-local-still
  (is (= :delete-local (c {:anki-now :absent :anki-baseline A
                           :local-now B :local-baseline B}))))

(deftest t7-delete-anki-when-local-row-gone-and-anki-still
  (is (= :delete-anki (c {:anki-now A :anki-baseline A
                          :local-now :absent :local-baseline B}))))

(deftest t8-drop-tombstone-when-both-gone
  (is (= :drop-tombstone (c {:anki-now :absent :anki-baseline A
                             :local-now :absent :local-baseline B}))))

(deftest t9-needs-baseline
  (testing "a card holding a note id but no baseline is unknown, never changed"
    (is (= :needs-baseline (c {:anki-now A :anki-baseline nil
                               :local-now B :local-baseline nil})))))

;; ── classify: the rows that must never destroy an edit ──────────────────────

(deftest t10-deleted-in-anki-but-edited-locally-is-a-conflict
  (is (= :conflict (c {:anki-now :absent :anki-baseline A
                       :local-now "edited" :local-baseline B}))))

(deftest t11-deleted-locally-but-edited-in-anki-is-a-conflict
  (is (= :conflict (c {:anki-now "edited" :anki-baseline A
                       :local-now :absent :local-baseline B}))))

(deftest t12-half-written-baseline-reads-as-unknown
  (testing "with both sides present, one missing baseline blocks both questions"
    (is (= :needs-baseline (c {:anki-now A :anki-baseline A
                               :local-now B :local-baseline nil})))
    (is (= :needs-baseline (c {:anki-now A :anki-baseline nil
                               :local-now B :local-baseline B})))))

(deftest t12b-a-tombstone-needs-only-ankis-baseline
  (testing "the local baseline describes a side that no longer exists"
    (is (= :delete-anki (c {:anki-now A :anki-baseline A
                            :local-now :absent :local-baseline nil}))))
  (testing "with no Anki baseline the note stays — dropping it orphaned 10 notes in production"
    (is (= :conflict (c {:anki-now A :anki-baseline nil
                         :local-now :absent :local-baseline B})))))

(deftest t12c-a-missing-note-needs-only-the-local-baseline
  (is (= :delete-local (c {:anki-now :absent :anki-baseline nil
                           :local-now B :local-baseline B})))
  (testing "local baseline absent but Anki baseline present is not a fresh card"
    (is (= :conflict (c {:anki-now :absent :anki-baseline A
                         :local-now B :local-baseline nil})))))

(deftest t12d-no-absent-baseline-yields-a-destructive-outcome
  (testing "every branch reachable with a nil baseline is non-destructive"
    (doseq [m [{:anki-now A :anki-baseline nil :local-now :absent :local-baseline B}
               {:anki-now :absent :anki-baseline A :local-now B :local-baseline nil}
               {:anki-now A :anki-baseline nil :local-now B :local-baseline B}
               {:anki-now A :anki-baseline A :local-now B :local-baseline nil}]]
      (is (contains? #{:conflict :needs-baseline} (c m)) (pr-str m)))))

(deftest t13-every-outcome-is-declared
  (testing "the closed set covers what classify can actually return"
    (let [produced #{(c {:anki-now A :anki-baseline A :local-now B :local-baseline B})
                     (c {:anki-now A :anki-baseline A :local-now "n" :local-baseline B})
                     (c {:anki-now "n" :anki-baseline A :local-now B :local-baseline B})
                     (c {:anki-now "x" :anki-baseline A :local-now "y" :local-baseline B})
                     (c {:anki-now :absent :anki-baseline nil :local-now B :local-baseline nil})
                     (c {:anki-now :absent :anki-baseline A :local-now B :local-baseline B})
                     (c {:anki-now A :anki-baseline A :local-now :absent :local-baseline B})
                     (c {:anki-now :absent :anki-baseline A :local-now :absent :local-baseline B})
                     (c {:anki-now A :anki-baseline nil :local-now B :local-baseline nil})}]
      (is (= 9 (count produced)))
      (is (every? rec/outcomes produced)))))

;; ── kind limits ─────────────────────────────────────────────────────────────

(deftest t14-pull-becomes-conflict-for-kinds-with-no-inverse
  (testing "score and overlapping have no Anki->FreeMemo inverse"
    (is (= :conflict (rec/apply-kind-limits :pull "score")))
    (is (= :conflict (rec/apply-kind-limits :pull "overlapping"))))
  (testing "occlusion pulls through io_fields, so it is untouched"
    (is (= :pull (rec/apply-kind-limits :pull "occlusion"))))
  (testing "every other outcome passes through for every kind"
    (doseq [k ["basic" "cloze" "occlusion" "score" "overlapping"]
            o [:push :conflict :create :delete-local :delete-anki :unchanged]]
      (is (= o (rec/apply-kind-limits o k))))))

;; ── bulk absence guard ──────────────────────────────────────────────────────

(deftest t15-bulk-absence-guard
  (testing "an empty or foreign Anki profile reports every note absent"
    (is (true? (rec/bulk-absence? 1477 1477))))
  (testing "just over half trips it, exactly half does not"
    (is (true? (rec/bulk-absence? 100 51)))
    (is (false? (rec/bulk-absence? 100 50))))
  (testing "a handful of real deletions must not stop the run"
    (is (false? (rec/bulk-absence? 1477 10))))
  (testing "no synced cards cannot be a mass deletion"
    (is (false? (rec/bulk-absence? 0 0)))))

(deftest t15b-guard-does-not-strand-a-tiny-collection
  (testing "deleting the only synced card in Anki must not block every future run"
    (is (false? (rec/bulk-absence? 1 1)))
    (is (false? (rec/bulk-absence? 2 2))))
  (testing "the floor is where a majority stops being one card's worth of coincidence"
    (is (true? (rec/bulk-absence? 3 3)))
    (is (true? (rec/bulk-absence? 5 3)))
    (is (false? (rec/bulk-absence? 6 3)))))

;; ── report ──────────────────────────────────────────────────────────────────

(deftest t16-report-line
  (is (= "nothing to do" (rec/report-line (rec/summarize [:unchanged :unchanged]))))
  (is (= "2 pushed, 1 conflicts"
        (rec/report-line (rec/summarize [:push :push :conflict :unchanged])))))

(deftest t16c-outcome-text-reports-what-happened-not-what-was-planned
  (testing "a run that wrote nothing says so, however much the plan intended"
    (is (= "already in sync" (rec/outcome-text {})))
    (is (= "already in sync" (rec/outcome-text {:writes 0 :pulls 0}))))
  (testing "the counts come from the recorder's steps, not from outcomes"
    (is (= "46 sent to Anki" (rec/outcome-text {:writes 46})))
    (is (= "2 sent to Anki, 1 pulled from Anki, 3 notes removed from Anki"
          (rec/outcome-text {:writes 2 :pulls 1 :tombstones 3}))))
  (testing "conflicts are the caller's to phrase — they ask for an action"
    (is (= "already in sync" (rec/outcome-text {:conflicts 5}))))
  (testing "the plan's own wording stays available and stays distinct"
    (is (= "46 created" (rec/report-line {:create 46})))
    (is (not= (rec/report-line {:create 46}) (rec/outcome-text {:writes 0})))))

(deftest t16b-every-outcome-except-unchanged-is-reportable
  (testing "an outcome missing from the report order never reaches the user"
    (is (= (disj rec/outcomes :unchanged) rec/reportable-outcomes)))
  (testing "each one renders"
    (doseq [o rec/reportable-outcomes]
      (is (not= "nothing to do" (rec/report-line (rec/summarize [o]))) (str o)))))

;; ── digest: stability, the property every baseline depends on ───────────────

(defn- basic-card [q a]
  {:flashcards/kind "basic" :flashcards/question q :flashcards/answer a})

(deftest t17-local-digest-is-stable-and-sensitive
  (testing "same input, same digest — a baseline that drifted would report a change every run"
    (is (= (digest/local-digest (basic-card "<p>q</p>" "<p>a</p>"))
          (digest/local-digest (basic-card "<p>q</p>" "<p>a</p>")))))
  (testing "a changed field changes the digest"
    (is (not= (digest/local-digest (basic-card "<p>q</p>" "<p>a</p>"))
          (digest/local-digest (basic-card "<p>q</p>" "<p>b</p>")))))
  (is (= 64 (count (digest/local-digest (basic-card "q" "a"))))))

(deftest t18-local-digest-covers-all-five-kinds
  (doseq [card [{:flashcards/kind "basic" :flashcards/question "q" :flashcards/answer "a"}
                {:flashcards/kind "cloze" :flashcards/cloze "{{c1::x}}"}
                {:flashcards/kind "occlusion"
                 :flashcards/io_fields {:header "h" :footer "f" :remarks "r"
                                        :sources "s" :extra1 "1" :extra2 "2"}}
                {:flashcards/kind "score" :flashcards/score_direction "sheet-front"
                 :flashcards/score_group_id 7}
                {:flashcards/kind "overlapping" :flashcards/question "q"
                 :flashcards/overlapping {:items ["a" "b"]}}]]
    (let [d (digest/local-digest card)]
      (is (= 64 (count d)) (str "no digest for " (:flashcards/kind card)))
      (is (= d (digest/local-digest card))))))

(deftest t19-kind-is-part-of-the-digest
  (testing "the same text under a different kind is a different card"
    (is (not= (digest/local-digest {:flashcards/kind "basic" :flashcards/question "x"})
          (digest/local-digest {:flashcards/kind "cloze" :flashcards/cloze "x"})))))

(deftest t20-field-boundary-cannot-be-forged
  (testing "moving text across the question/answer boundary changes the digest"
    (is (not= (digest/local-digest (basic-card "ab" ""))
          (digest/local-digest (basic-card "a" "b"))))))

(deftest t21-overlapping-digest-ignores-map-order
  (testing "JSONB key order must not move the digest, or every restart reports a change"
    (is (= (digest/local-digest {:flashcards/kind "overlapping" :flashcards/question "q"
                                 :flashcards/overlapping {:a 1 :b 2}})
          (digest/local-digest {:flashcards/kind "overlapping" :flashcards/question "q"
                                :flashcards/overlapping {:b 2 :a 1}})))))

;; Sensitivity. Stability alone is satisfied by a constant, which is exactly what
;; the first version of stable-str returned for every map — it sorted a copy of
;; the keys, then re-derived them with (keyword (str k)) and read nil for each.
;; Every overlapping card digested identically and a local edit to one classified
;; :unchanged. These are the assertions that fail on that bug.

(deftest t21b-overlapping-content-moves-the-digest
  (let [d #(digest/local-digest {:flashcards/kind "overlapping"
                                 :flashcards/question "q"
                                 :flashcards/overlapping %})]
    (testing "a changed value"
      (is (not= (d {:items ["a" "b"]}) (d {:items ["a" "CHANGED"]}))))
    (testing "a changed key"
      (is (not= (d {:items ["a"]}) (d {:other ["a"]}))))
    (testing "an added key"
      (is (not= (d {:items ["a"]}) (d {:items ["a"] :extra 1}))))
    (testing "a nested change"
      (is (not= (d {:a {:b 1}}) (d {:a {:b 2}}))))
    (testing "a value that only differs by type"
      (is (not= (d {:a 1}) (d {:a "1"}))))))

(deftest t21c-occlusion-field-content-moves-the-digest
  (let [base {:header "h" :footer "f" :remarks "r"
              :sources "s" :extra1 "1" :extra2 "2"}
        d #(digest/local-digest {:flashcards/kind "occlusion" :flashcards/io_fields %})]
    (doseq [k [:header :footer :remarks :sources :extra1 :extra2]]
      (is (not= (d base) (d (assoc base k "CHANGED"))) (str "blind to " k)))))

(deftest t21d-every-kind-is-sensitive-to-its-own-content
  (testing "no kind may digest a constant"
    (doseq [[before after]
            [[{:flashcards/kind "basic" :flashcards/question "q" :flashcards/answer "a"}
              {:flashcards/kind "basic" :flashcards/question "q" :flashcards/answer "CHANGED"}]
             [{:flashcards/kind "cloze" :flashcards/cloze "{{c1::x}}"}
              {:flashcards/kind "cloze" :flashcards/cloze "{{c1::y}}"}]
             [{:flashcards/kind "occlusion" :flashcards/io_fields {:header "h"}}
              {:flashcards/kind "occlusion" :flashcards/io_fields {:header "CHANGED"}}]
             [{:flashcards/kind "score" :flashcards/score_direction "sheet-front"
               :flashcards/score_group_id 7}
              {:flashcards/kind "score" :flashcards/score_direction "audio-front"
               :flashcards/score_group_id 7}]
             [{:flashcards/kind "overlapping" :flashcards/question "q"
               :flashcards/overlapping {:t "a"}}
              {:flashcards/kind "overlapping" :flashcards/question "q"
               :flashcards/overlapping {:t "b"}}]]]
      (is (not= (digest/local-digest before) (digest/local-digest after))
        (str "blind kind: " (:flashcards/kind before))))))

(deftest t21e-unknown-kind-reads-every-content-column
  (testing "a sixth kind must not silently digest by basic's two fields"
    (let [base {:flashcards/kind "newkind" :flashcards/question "q"
                :flashcards/answer "a" :flashcards/cloze "c"
                :flashcards/score_direction "d" :flashcards/score_group_id 1
                :flashcards/overlapping {:t "o"} :flashcards/io_fields {:header "h"}}]
      (doseq [[k v] [[:flashcards/question "X"] [:flashcards/answer "X"]
                     [:flashcards/cloze "X"] [:flashcards/score_direction "X"]
                     [:flashcards/overlapping {:t "X"}]
                     [:flashcards/io_fields {:header "X"}]]]
        (is (not= (digest/local-digest base) (digest/local-digest (assoc base k v)))
          (str "blind to " k))))))

(deftest t21f-unparsed-jsonb-fails-loudly
  (testing "a PGobject answers every get with nil, which would digest nothing"
    (is (thrown? clojure.lang.ExceptionInfo
          (digest/local-digest {:flashcards/kind "overlapping"
                                :flashcards/question "q"
                                :flashcards/overlapping "{\"raw\":\"json\"}"})))
    (is (thrown? clojure.lang.ExceptionInfo
          (digest/local-digest {:flashcards/kind "occlusion"
                                :flashcards/io_fields "{\"header\":\"h\"}"})))))

(deftest t22-local-digest-ignores-user-owned-and-timestamp-columns
  (testing "updated_at moving without a content change must not move the digest"
    (is (= (digest/local-digest (basic-card "q" "a"))
          (digest/local-digest (assoc (basic-card "q" "a")
                                 :flashcards/updated_at "2026-08-14T00:00:00"))))))

;; ── digest: the Anki side ───────────────────────────────────────────────────

(defn- fields [& name-value-pairs]
  (into {} (map-indexed (fn [i [n v]] [(keyword n) {:value v :order i}])
             (partition 2 name-value-pairs))))

(deftest t23-anki-digest-reads-content-fields-only
  (let [base (fields "Front" "<p>q</p>" "Back" "<p>a</p>"
               "Links" "<p>l</p>" "Remarks" "")]
    (testing "an edit to the user's Remarks space is not a content change"
      (is (= (digest/anki-digest base "basic")
            (digest/anki-digest (fields "Front" "<p>q</p>" "Back" "<p>a</p>"
                                  "Links" "<p>l</p>" "Remarks" "my note")
              "basic"))))
    (testing "a regenerated Links field is not a content change"
      (is (= (digest/anki-digest base "basic")
            (digest/anki-digest (fields "Front" "<p>q</p>" "Back" "<p>a</p>"
                                  "Links" "<p>different</p>" "Remarks" "")
              "basic"))))
    (testing "an edit to Front is"
      (is (not= (digest/anki-digest base "basic")
            (digest/anki-digest (fields "Front" "<p>edited</p>" "Back" "<p>a</p>"
                                  "Links" "<p>l</p>" "Remarks" "")
              "basic"))))))

(deftest t24-anki-digest-tolerates-a-short-field-list
  (testing "a foreign or half-migrated model must classify as changed, not throw"
    (is (= 64 (count (digest/anki-digest (fields "Front" "only one") "basic"))))))

(deftest t24b-unknown-kind-reads-every-anki-field
  (testing "a sixth kind must not silently digest by basic's indexes 0 and 1"
    (let [base (fields "A" "1" "B" "2" "C" "3" "D" "4")]
      (is (not= (digest/anki-digest base "newkind")
            (digest/anki-digest (fields "A" "1" "B" "2" "C" "CHANGED" "D" "4")
              "newkind")))
      (testing "and basic still ignores field 2"
        (is (= (digest/anki-digest base "basic")
              (digest/anki-digest (fields "A" "1" "B" "2" "C" "CHANGED" "D" "4")
                "basic")))))))

(defn- model-note
  "A notesInfo :fields map holding every field of `kind`'s model, each carrying
   its own name as the value so a mix-up is visible in the digest."
  [kind]
  (let [names {"basic" helpers/basic-field-names
               "cloze" helpers/cloze-field-names
               "occlusion" helpers/io-field-names
               "score" helpers/score-field-names
               "overlapping" helpers/overlapping-field-names}]
    (into {} (map-indexed (fn [i n] [(keyword n) {:value n :order i}]))
      (get names kind))))

(deftest t24c-anki-digest-is-sensitive-to-every-content-field
  (testing "changing any one content field moves that kind's digest"
    (doseq [kind ["basic" "cloze" "occlusion" "score" "overlapping"]
            field-name (get af/content-field-names kind)]
      (let [base (model-note kind)
            changed (assoc-in base [(keyword field-name) :value] "CHANGED")]
        (is (not= (digest/anki-digest base kind) (digest/anki-digest changed kind))
          (str kind " is blind to " field-name))))))

(deftest t24d-anki-digest-ignores-every-non-content-field
  (testing "a field the app does not own must not move the digest"
    (doseq [kind ["basic" "cloze" "occlusion" "score" "overlapping"]]
      (let [base (model-note kind)
            owned (set (get af/content-field-names kind))]
        (doseq [[fk _] base
                :when (not (contains? owned (name fk)))]
          (is (= (digest/anki-digest base kind)
                (digest/anki-digest (assoc-in base [fk :value] "USER EDIT") kind))
            (str kind " reads " (name fk) " as content")))))))

(deftest t25-anki-digest-covers-all-five-kinds
  (doseq [kind ["basic" "cloze" "occlusion" "score" "overlapping"]]
    (let [d (digest/anki-digest (model-note kind) kind)]
      (is (= 64 (count d)) (str "no digest for " kind))
      (is (= d (digest/anki-digest (model-note kind) kind))))))

(deftest t25b-unknown-kind-digests-every-field-it-carries
  (testing "a sixth kind must not silently digest by another kind's names"
    (let [base (fields "Alpha" "1" "Beta" "2" "Gamma" "3")]
      (doseq [n ["Alpha" "Beta" "Gamma"]]
        (is (not= (digest/anki-digest base "newkind")
              (digest/anki-digest (assoc-in base [(keyword n) :value] "X") "newkind"))
          (str "unknown kind is blind to " n))))))

(deftest t26-digest-field-names-exist-in-their-model
  (is (= #{"basic" "cloze" "occlusion" "score" "overlapping"}
        (set (keys af/content-field-names))))
  (testing "occlusion covers the same six fields the pull path diffs"
    (is (= (set (vals helpers/io-pull-field-names))
          (set (get af/content-field-names "occlusion")))))
  (testing "every digested field name is a real field of that kind's model"
    (doseq [[kind model-names]
            {"basic"       helpers/basic-field-names
             "cloze"       helpers/cloze-field-names
             "occlusion"   helpers/io-field-names
             "score"       helpers/score-field-names
             "overlapping" helpers/overlapping-field-names}]
      (doseq [n (get af/content-field-names kind)]
        (is (contains? (set model-names) n)
          (str kind " digests \"" n "\", which its model does not have"))))))

(deftest t26b-digest-reads-by-name-not-position
  (testing "a model with Links appended late digests the same as one without"
    (let [with-links (fields "Front" "q" "Back" "a" "Links" "L" "Remarks" "")
          links-last (fields "Front" "q" "Back" "a" "Remarks" "" "Links" "L")]
      (is (= (digest/anki-digest with-links "basic")
            (digest/anki-digest links-last "basic")))))
  (testing "occlusion reads its six by name, whatever order the model holds"
    (let [a (fields "ID (hidden)" "x" "Header" "H" "Image" "i" "Question Mask" "m"
              "Footer" "F" "Remarks" "R" "Sources" "S" "Links" "L"
              "Extra 1" "1" "Extra 2" "2" "Answer Mask" "am" "Original Mask" "om")
          b (fields "ID (hidden)" "x" "Header" "H" "Image" "i" "Question Mask" "m"
              "Footer" "F" "Remarks" "R" "Sources" "S" "Extra 1" "1"
              "Extra 2" "2" "Answer Mask" "am" "Original Mask" "om" "Links" "L")]
      (is (= (digest/anki-digest a "occlusion") (digest/anki-digest b "occlusion"))))))

;; ── plan-run: classify over a manifest ──────────────────────────────────────

(defn- card-row
  "A get-cards-for-reconcile row."
  [id note-id q a anki-base local-base]
  {:flashcards/id id :flashcards/kind "basic"
   :flashcards/question q :flashcards/answer a
   :flashcards/anki_note_id note-id
   :flashcards/anki_baseline_digest anki-base
   :flashcards/local_baseline_digest local-base})

(defn- local-of [q a]
  (digest/local-digest {:flashcards/kind "basic"
                        :flashcards/question q :flashcards/answer a}))

(deftest t28-plan-run-classifies-a-mixed-manifest
  (let [in-sync-local (local-of "q" "a")
        edited-local  (local-of "q" "EDITED")
        anki-d        "ankidigest"
        manifest {:cards [;; unchanged
                          (card-row 1 100 "q" "a" anki-d in-sync-local)
                          ;; local moved only
                          (card-row 2 200 "q" "EDITED" anki-d in-sync-local)
                          ;; never pushed
                          (card-row 3 nil "q" "a" nil nil)
                          ;; synced, no baseline yet
                          (card-row 4 400 "q" "a" nil nil)]
                  :tombstones [{:anki_note_tombstones/anki_note_id 900
                                :anki_note_tombstones/anki_baseline_digest anki-d}]}
        plan (rec/plan-run manifest
               {100 anki-d, 200 anki-d, 400 anki-d, 900 anki-d}
               #{})]
    (is (false? (:aborted? plan)))
    (is (= [:unchanged :push :create :needs-baseline]
          (mapv :outcome (:cards plan))))
    (is (= [:delete-anki] (mapv :outcome (:tombstones plan))))
    (is (= {:unchanged 1 :push 1 :create 1 :needs-baseline 1 :delete-anki 1}
          (:counts plan)))
    (testing "the local digest is carried out so the caller need not recompute it"
      (is (= edited-local (:local-digest (second (:cards plan))))))))

(deftest t29-plan-run-pulls-when-only-anki-moved
  (let [l (local-of "q" "a")
        manifest {:cards [(card-row 1 100 "q" "a" "old-anki" l)] :tombstones []}]
    (is (= [:pull] (mapv :outcome (:cards (rec/plan-run manifest {100 "new-anki"} #{})))))))

(deftest t29b-plan-run-conflicts-when-both-moved
  (let [manifest {:cards [(card-row 1 100 "q" "EDITED" "old-anki" (local-of "q" "a"))]
                  :tombstones []}]
    (is (= [:conflict] (mapv :outcome (:cards (rec/plan-run manifest {100 "new-anki"} #{})))))))

(deftest t29c-plan-run-guard-ignores-never-pushed-cards
  (testing "unsynced cards must not dilute the absent ratio"
    (let [l (local-of "q" "a")
          synced (mapv #(card-row % (+ 100 %) "q" "a" "ad" l) (range 1 11))
          unsynced (mapv #(card-row % nil "q" "a" nil nil) (range 100 300))
          manifest {:cards (into synced unsynced) :tombstones []}]
      (testing "every synced note absent trips the guard despite 200 unsynced cards"
        (is (true? (:aborted? (rec/plan-run manifest {} (set (map #(+ 100 %) (range 1 11))))))))
      (testing "none absent does not"
        (is (false? (:aborted? (rec/plan-run manifest
                                 (into {} (map #(vector (+ 100 %) "ad") (range 1 11)))
                                 #{}))))))))

(deftest t29d-plan-run-reports-decisions-even-when-aborted
  (testing "the caller needs the counts to explain why it applied nothing"
    (let [l (local-of "q" "a")
          manifest {:cards (mapv #(card-row % (+ 100 %) "q" "a" "ad" l) (range 1 6))
                    :tombstones []}
          plan (rec/plan-run manifest {} (set (map #(+ 100 %) (range 1 6))))]
      (is (true? (:aborted? plan)))
      (is (= 5 (count (:cards plan))))
      (is (= {:delete-local 5} (:counts plan))))))

;; ── the two sides are independent, which is the whole design ────────────────

(deftest t27-a-local-only-edit-does-not-move-the-anki-digest
  (testing "the defect this replaces: stripped= reports a local edit as Anki-modified"
    (let [anki (fields "Front" "<p>q</p>" "Back" "<p>a</p>" "Links" "" "Remarks" "")
          before (digest/anki-digest anki "basic")
          edited-locally (basic-card "<p>q</p>" "<p>EDITED</p>")]
      (is (= before (digest/anki-digest anki "basic")))
      (is (not= (digest/local-digest edited-locally)
            (digest/local-digest (basic-card "<p>q</p>" "<p>a</p>"))))
      (testing "so the card classifies as push, not conflict"
        (is (= :push (rec/classify
                       {:anki-now before :anki-baseline before
                        :local-now (digest/local-digest edited-locally)
                        :local-baseline (digest/local-digest
                                          (basic-card "<p>q</p>" "<p>a</p>"))})))))))
