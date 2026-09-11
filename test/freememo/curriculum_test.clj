(ns freememo.curriculum-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [freememo.curriculum :as curriculum]
   [freememo.curriculum-exam :as exam]
   [freememo.db :as db]
   [freememo.undo :as undo]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs])
  (:import [java.time Instant LocalDate]))

(def opts {:builder-fn rs/as-unqualified-maps})

(def rubric
  {:factual_accuracy "Facts match the evidence."
   :reasoning "The claim follows from the evidence."
   :source_evidence "The answer uses the supplied evidence."
   :integration "The answer connects relevant points."
   :completeness "The answer covers the full prompt."})

(defn question [index]
  {:role (nth ["explain" "compare" "apply" "evaluate" "synthesize"] index)
   :prompt (str "Question " index)
   :outcomes [(str "outcome-" index)]
   :grading_guide "Use the cited evidence."
   :evidence [{:material_id 1 :topic_id 2 :quote "frozen evidence"}]
   :rubric rubric})

(deftest generated-form-requires-five-complete-essay-questions
  (let [form {:blueprint {:assessed_outcomes ["a"] :unassessed_outcomes ["b"]}
              :questions (mapv question (range 5))}
        validated (exam/validate-generated-form! form)]
    (is (= 5 (count (:questions validated))))
    (is (every? :id (:questions validated))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"exactly five"
        (exam/validate-generated-form!
          {:blueprint {:assessed_outcomes [] :unassessed_outcomes []}
           :questions (mapv question (range 4))})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Add material"
        (exam/validate-generated-form! {:insufficient true :reason "Too little evidence."}))))

(deftest generated-form-recovers-a-missing-redundant-grading-guide
  (let [form {:blueprint {:assessed_outcomes ["a"] :unassessed_outcomes []}
              :questions (mapv #(dissoc (question %) :grading_guide) (range 5))}
        validated (exam/validate-generated-form! form)]
    (is (every? #(not (str/blank? (:grading_guide %)))
          (:questions validated)))))

(def full-score
  {:factual_accuracy 4 :reasoning 4 :source_evidence 4
   :integration 4 :completeness 4})

(def blueprint
  {:evidence_packets [{:sections [{:text "This is frozen evidence for grading."}]}]})

(deftest grade-is-the-sum-of-25-zero-to-four-criteria
  (let [grade {:question_grades
               (vec (repeat 5 {:criteria full-score :deductions []
                               :weaknesses [] :explanation "Complete."}))
               :explanation "All answers meet the frozen rubric."}]
    (is (= 100 (:raw-score (exam/validate-grade! grade blueprint))))))

(deftest every-lost-point-needs-a-frozen-evidence-citation
  (let [scores (assoc full-score :reasoning 3)
        base {:question_grades
              (vec (repeat 5 {:criteria full-score :deductions []
                              :weaknesses [] :explanation "Complete."}))
              :explanation "One answer needs work."}]
    (testing "a missing deduction is invalid"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"lost point"
            (exam/validate-grade!
              (assoc-in base [:question_grades 0 :criteria] scores) blueprint))))
    (testing "a citation outside the frozen evidence is invalid"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"frozen evidence"
            (exam/validate-grade!
              (-> base
                (assoc-in [:question_grades 0 :criteria] scores)
                (assoc-in [:question_grades 0 :deductions]
                  [{:criterion "reasoning" :reason "Missing link."
                    :citation "invented text"}]))
              blueprint))))
    (testing "an exact frozen citation is accepted"
      (is (= 99
            (:raw-score
              (exam/validate-grade!
                (-> base
                  (assoc-in [:question_grades 0 :criteria] scores)
                  (assoc-in [:question_grades 0 :deductions]
                    [{:criterion "reasoning" :reason "Missing link."
                      :citation "frozen evidence"}]))
                blueprint)))))))

(deftest work-allocation-preserves-order-days-and-capacity
  (let [tasks [{:title "A" :minutes 90} {:title "B" :minutes 30}]
        monday (LocalDate/parse "2026-08-31")
        result (curriculum/allocate-work tasks monday #{1 3} 60)]
    (is (= ["2026-08-31" "2026-09-02" "2026-09-02"] (mapv :date result)))
    (is (= [60 30 30] (mapv :minutes result)))
    (is (= ["A" "A" "B"] (mapv :title result)))))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(deftest moving-one-step-writes-positions-a-check-constraint-accepts
  ;; `curriculum_materials.position` carries CHECK (position >= 0) and
  ;; UNIQUE (block_id, position). `resequence!` parks rows before it writes the
  ;; new order, and the park used to be negative — so every reorder rolled back
  ;; and the arrows looked dead. Only a real database can catch that.
  ;;
  ;; The step also has to be idempotent at each end. The client sends a
  ;; direction, so an end-stop is an ordinary no-op, not an error.
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [tag (str "reorder-" (System/nanoTime))
          user-id (:id (jdbc/execute-one! db/ds
                         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                          tag (str tag "@example.test")]
                         {:builder-fn rs/as-unqualified-maps}))
          topic! (fn [title]
                   (:id (jdbc/execute-one! db/ds
                          ["INSERT INTO topics (user_id, title, kind, content_text)
                            VALUES (?, ?, 'markdown', 'body') RETURNING id" user-id title]
                          {:builder-fn rs/as-unqualified-maps})))]
      (try
        (let [curriculum-id (curriculum/create-curriculum!
                              user-id {:title "Reorder" :daily-minutes 60
                                       :study-weekdays []})
              block-id (curriculum/add-block! user-id curriculum-id "Only")
              second-block (curriculum/add-block! user-id curriculum-id "Next")
              a (curriculum/add-material! user-id block-id (topic! "A") 30)
              b (curriculum/add-material! user-id block-id (topic! "B") 30)
              c (curriculum/add-material! user-id block-id (topic! "C") 30)
              ordered (fn []
                        (mapv :id (jdbc/execute! db/ds
                                    ["SELECT id FROM curriculum_materials
                                      WHERE block_id = ? ORDER BY position" block-id]
                                    {:builder-fn rs/as-unqualified-maps})))]
          (curriculum/move-material! user-id a 1)
          (is (= [b a c] (ordered)) "a material swaps with its neighbour")
          (curriculum/move-material! user-id a 1)
          (is (= [b c a] (ordered)) "a second step keeps going")
          (curriculum/move-material! user-id a 1)
          (is (= [b c a] (ordered)) "a step off the end changes nothing")
          (curriculum/move-material! user-id b -1)
          (is (= [b c a] (ordered)) "a step off the start changes nothing")
          (is (empty? (jdbc/execute! db/ds
                        ["SELECT id FROM curriculum_materials
                          WHERE block_id = ? AND position < 0" block-id]))
            "no row is left at a negative position")
          (curriculum/move-block! user-id block-id 1)
          (is (= [second-block block-id]
                (mapv :id (jdbc/execute! db/ds
                            ["SELECT id FROM curriculum_blocks
                              WHERE curriculum_id = ? ORDER BY position" curriculum-id]
                            {:builder-fn rs/as-unqualified-maps})))
            "a block swaps with its neighbour"))
        (finally
          (jdbc/execute-one! db/ds ["DELETE FROM users WHERE id = ?" user-id]))))))

(deftest staged-curriculum-retains-the-whole-graph-and-rejects-late-results
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [tag (str "curriculum-recovery-" (System/nanoTime))
          user-id (:id (jdbc/execute-one! db/ds
                         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                          tag (str tag "@example.test")]
                         {:builder-fn rs/as-unqualified-maps}))
          topic-id (:id (jdbc/execute-one! db/ds
                          ["INSERT INTO topics (user_id, title, kind, content_text)
                            VALUES (?, 'Evidence', 'markdown', 'frozen evidence') RETURNING id"
                           user-id]
                          {:builder-fn rs/as-unqualified-maps}))]
      (try
        (let [curriculum-id (curriculum/create-curriculum!
                              user-id {:title "Recover me" :daily-minutes 60
                                       :study-weekdays []})
              first-block (curriculum/add-block! user-id curriculum-id "First")
              _second-block (curriculum/add-block! user-id curriculum-id "Second")
              _material (curriculum/add-material! user-id first-block topic-id 30)
              evidence (curriculum/subject-evidence db/ds user-id {:block-id first-block})
              form-id (curriculum/save-draft-form! user-id {:block-id first-block}
                        {:threshold 70
                         :blueprint {:assessed_outcomes ["a"] :unassessed_outcomes []
                                     :evidence_packets []}
                         :questions (mapv question (range 5))
                         :source-versions (:source-versions evidence)})
              _approved (curriculum/approve-form! user-id form-id)
              sitting-id (curriculum/start-sitting! user-id form-id)
              _drafted (curriculum/save-answer-draft! user-id sitting-id
                         (mapv (fn [index] {:index index :answer "Answer"
                                            :do-not-know false})
                           (range 5)))
              _submitted (curriculum/finalize-sitting! user-id sitting-id)
              criteria (vec (repeat 5 {:criteria {:factual_accuracy 3 :reasoning 3
                                                   :source_evidence 3 :integration 3
                                                   :completeness 2}}))
              _grade (curriculum/record-grade-revision! user-id sitting-id
                       {:raw-score 70 :criteria criteria :explanation "Pass."})
              _override (curriculum/unlock-next-block! user-id first-block)
              graph-counts (fn []
                             {:blocks (:count (jdbc/execute-one! db/ds
                                        ["SELECT count(*)::int AS count FROM curriculum_blocks
                                          WHERE curriculum_id = ?" curriculum-id] opts))
                              :materials (:count (jdbc/execute-one! db/ds
                                           ["SELECT count(*)::int AS count FROM curriculum_materials m
                                             JOIN curriculum_blocks b ON b.id = m.block_id
                                            WHERE b.curriculum_id = ?" curriculum-id] opts))
                              :forms (:count (jdbc/execute-one! db/ds
                                       ["SELECT count(*)::int AS count FROM curriculum_exam_forms f
                                         JOIN curriculum_blocks b ON b.id = f.block_id
                                        WHERE b.curriculum_id = ?" curriculum-id] opts))
                              :sittings (:count (jdbc/execute-one! db/ds
                                          ["SELECT count(*)::int AS count FROM curriculum_exam_sittings s
                                            JOIN curriculum_exam_forms f ON f.id = s.form_id
                                            JOIN curriculum_blocks b ON b.id = f.block_id
                                           WHERE b.curriculum_id = ?" curriculum-id] opts))
                              :revisions (:count (jdbc/execute-one! db/ds
                                           ["SELECT count(*)::int AS count FROM curriculum_grade_revisions g
                                             JOIN curriculum_exam_sittings s ON s.id = g.sitting_id
                                             JOIN curriculum_exam_forms f ON f.id = s.form_id
                                             JOIN curriculum_blocks b ON b.id = f.block_id
                                            WHERE b.curriculum_id = ?" curriculum-id] opts))
                              :overrides (:count (jdbc/execute-one! db/ds
                                           ["SELECT count(*)::int AS count FROM curriculum_unlock_overrides
                                            WHERE curriculum_id = ?" curriculum-id] opts))})
              before (graph-counts)
              operation-started-at (Instant/now)
              staged (curriculum/stage-curriculum-for-deletion! user-id curriculum-id)]
          (is (= {:blocks 2 :materials 1 :forms 1 :sittings 1 :revisions 1 :overrides 1}
                before))
          (is (= before (graph-counts)) "staging retains every owned child row")
          (is (nil? (curriculum/curriculum-detail 0 user-id curriculum-id)))
          (is (some? (jdbc/execute-one! db/ds ["SELECT id FROM topics WHERE id = ?" topic-id]
                opts))
            "staging does not delete the imported source topic")
          (is (:success (undo/undo-entry! user-id (:entry-id staged))))
          (is (= before (graph-counts)) "Undo clears only the root marker")
          (is (nil? (curriculum/save-draft-form! user-id {:block-id first-block}
                     {:threshold 70
                      :blueprint {:assessed_outcomes ["a"] :unassessed_outcomes []
                                  :evidence_packets []}
                      :questions (mapv question (range 5))
                      :source-versions (:source-versions evidence)
                      :operation-started-at operation-started-at}))
            "a form result that began before deletion cannot land after Undo")
          (is (nil? (curriculum/record-grade-revision! user-id sitting-id
                     {:raw-score 70 :criteria criteria :explanation "Late."
                      :operation-started-at operation-started-at}))
            "a grade result that began before deletion cannot land after Undo")
          (is (= before (graph-counts)) "discarded late results write no child row"))
        (finally
          (jdbc/execute-one! db/ds ["DELETE FROM topics WHERE id = ?" topic-id])
          (jdbc/execute-one! db/ds ["DELETE FROM users WHERE id = ?" user-id]))))))

(deftest staged-curriculum-survives-the-ordinary-undo-cap-and-purges-after-expiry
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [tag (str "curriculum-expiry-" (System/nanoTime))
          user-id (:id (jdbc/execute-one! db/ds
                         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                          tag (str tag "@example.test")]
                         {:builder-fn rs/as-unqualified-maps}))]
      (try
        (let [curriculum-id (curriculum/create-curriculum!
                              user-id {:title "Expire me" :daily-minutes 30
                                       :study-weekdays []})
              {:keys [entry-id]} (curriculum/stage-curriculum-for-deletion!
                                   user-id curriculum-id)]
          (dotimes [n 101]
            (db/insert-undo-entry! user-id "reset-prompt" "setting" [(str "setting-" n)]
              [{:key (str "setting-" n) :value "before"}]))
          (is (some #(= entry-id (:id %)) (db/get-undo-entries user-id))
            "the staged curriculum is not removed by the ordinary 100-entry cap")
          (jdbc/execute-one! db/ds
            ["UPDATE undo_log SET occurred_at = now() - interval '12 hours 1 minute'
              WHERE id = ?" entry-id])
          (is (= 1 (db/purge-staged-curricula!)))
          (is (nil? (jdbc/execute-one! db/ds
                     ["SELECT id FROM curricula WHERE id = ?" curriculum-id] opts)))
          (is (nil? (db/get-undo-entry entry-id))
            "purge deletes the log entry after it physically deletes the root"))
        (finally
          (jdbc/execute-one! db/ds ["DELETE FROM users WHERE id = ?" user-id]))))))

(deftest a-cancelled-sitting-is-closed-and-can-be-archived
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [tag (str "cancel-" (System/nanoTime))
          user-id (:id (jdbc/execute-one! db/ds
                         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                          tag (str tag "@example.test")]
                         {:builder-fn rs/as-unqualified-maps}))
          topic-id (:id (jdbc/execute-one! db/ds
                          ["INSERT INTO topics (user_id, title, kind, content_text)
                            VALUES (?, 'Evidence', 'markdown', 'frozen evidence') RETURNING id"
                           user-id]
                          {:builder-fn rs/as-unqualified-maps}))]
      (try
        (let [curriculum-id (curriculum/create-curriculum!
                              user-id {:title "Cancel test" :daily-minutes 60
                                       :study-weekdays []})
              block-id (curriculum/add-block! user-id curriculum-id "Only")
              _material (curriculum/add-material! user-id block-id topic-id 30)
              evidence (curriculum/subject-evidence db/ds user-id {:block-id block-id})
              form-id (curriculum/save-draft-form! user-id {:block-id block-id}
                        {:threshold 70
                         :blueprint {:assessed_outcomes ["a"] :unassessed_outcomes []
                                     :evidence_packets []}
                         :questions (mapv question (range 5))
                         :source-versions (:source-versions evidence)})
              _approved (curriculum/approve-form! user-id form-id)
              sitting-id (curriculum/start-sitting! user-id form-id)
              listed (fn [] (:sittings (curriculum/exam-page-model 0 user-id form-id nil)))
              open-id (fn [] (:id (:open-sitting
                                    (curriculum/exam-page-model 0 user-id form-id nil))))]
          (is (= sitting-id (open-id)) "a fresh sitting is the open one")
          (is (= :not-cancelled (curriculum/archive-sitting! user-id sitting-id))
            "an open sitting cannot be archived")
          (is (= :cancelled (curriculum/cancel-sitting! user-id sitting-id)))
          (is (nil? (open-id)) "a cancelled sitting is no longer open")
          (is (= :already-closed
                (curriculum/save-answer-draft! user-id sitting-id
                  [{:index 0 :answer "Late" :do-not-know false}]))
            "a cancelled sitting takes no answers")
          (is (= :already-closed (curriculum/finalize-sitting! user-id sitting-id))
            "a cancelled sitting can never be graded")
          (is (= :already-closed (curriculum/cancel-sitting! user-id sitting-id))
            "a second cancel changes nothing")
          (is (= [sitting-id] (mapv :id (listed))) "a cancelled sitting still lists")
          (is (= :archived (curriculum/archive-sitting! user-id sitting-id)))
          (is (empty? (listed)) "an archived sitting leaves the list")
          (is (= :already-archived (curriculum/archive-sitting! user-id sitting-id))
            "a second archive changes nothing")
          (let [submitted (curriculum/start-sitting! user-id form-id)]
            (curriculum/save-answer-draft! user-id submitted
              (mapv (fn [index] {:index index :answer "Answer" :do-not-know false})
                (range 5)))
            (is (= :submitted (curriculum/finalize-sitting! user-id submitted)))
            (is (= :already-closed (curriculum/cancel-sitting! user-id submitted))
              "a submitted sitting can never be cancelled")
            (is (= :not-cancelled (curriculum/archive-sitting! user-id submitted))
              "a submitted sitting stays visible")))
        (finally
          (jdbc/execute-one! db/ds ["DELETE FROM topics WHERE id = ?" topic-id])
          (jdbc/execute-one! db/ds ["DELETE FROM users WHERE id = ?" user-id]))))))

(defn- sql-smallint-array [values]
  (reify java.sql.Array
    (getBaseTypeName [_] "int2")
    (getBaseType [_] java.sql.Types/SMALLINT)
    (getArray [_] (into-array Short (map short values)))
    (getArray [_ _] (into-array Short (map short values)))
    (getArray [_ _ _] (into-array Short (map short values)))
    (getArray [_ _ _ _] (into-array Short (map short values)))
    (getResultSet [_] nil)
    (getResultSet [_ _] nil)
    (getResultSet [_ _ _] nil)
    (getResultSet [_ _ _ _] nil)
    (free [_])))

(deftest curriculum-list-returns-plain-schedule-controls
  (let [weekdays (sql-smallint-array [1 3])]
    (with-redefs [jdbc/execute! (fn [& _]
                                  [{:id 7
                                    :study_weekdays weekdays
                                    :target_date (LocalDate/parse "2026-09-30")}])]
      (let [listed (first (curriculum/list-curricula 0 11))]
        (is (= [1 3] (:study_weekdays listed)))
        (is (vector? (:study_weekdays listed))
          "curriculum rows must contain Electric-wire-safe weekdays")
        (is (= "2026-09-30" (:target_date listed)))))))

(deftest passing-block-exams-and-overrides-have-distinct-effects
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [tag (str "curriculum-" (System/nanoTime))
          user-id (:id (jdbc/execute-one! db/ds
                         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                          tag (str tag "@example.test")]
                         {:builder-fn rs/as-unqualified-maps}))
          topic-id (:id (jdbc/execute-one! db/ds
                          ["INSERT INTO topics (user_id, title, kind, content_text)
                            VALUES (?, 'Evidence', 'markdown', 'frozen evidence') RETURNING id"
                           user-id]
                          {:builder-fn rs/as-unqualified-maps}))]
      (try
        (let [curriculum-id (curriculum/create-curriculum!
                              user-id {:title "Contract test" :daily-minutes 60
                                       :study-weekdays []})
              first-block (curriculum/add-block! user-id curriculum-id "First")
              second-block (curriculum/add-block! user-id curriculum-id "Second")
              third-block (curriculum/add-block! user-id curriculum-id "Third")
              _material (curriculum/add-material! user-id first-block topic-id 30)
              evidence (curriculum/subject-evidence db/ds user-id {:block-id first-block})
              questions (mapv question (range 5))
              form-id (curriculum/save-draft-form! user-id {:block-id first-block}
                        {:threshold 70
                         :blueprint {:assessed_outcomes ["a"] :unassessed_outcomes []
                                     :evidence_packets []}
                         :questions questions
                         :source-versions (:source-versions evidence)})
              _approved (curriculum/approve-form! user-id form-id)
              sitting-id (curriculum/start-sitting! user-id form-id)
              _drafted (curriculum/save-answer-draft! user-id sitting-id
                         (mapv (fn [index] {:index index :answer "Answer"
                                            :do-not-know false})
                           (range 5)))
              submitted (curriculum/finalize-sitting! user-id sitting-id)
              late-draft (curriculum/save-answer-draft! user-id sitting-id
                           [{:index 0 :answer "Changed" :do-not-know false}])
              resubmitted (curriculum/finalize-sitting! user-id sitting-id)
              passing-criteria (vec (repeat 5 {:criteria
                                                {:factual_accuracy 3 :reasoning 3
                                                 :source_evidence 3 :integration 3
                                                 :completeness 2}}))
              zero-criteria (vec (repeat 5 {:criteria
                                             {:factual_accuracy 0 :reasoning 0
                                              :source_evidence 0 :integration 0
                                              :completeness 0}}))
              _pass (curriculum/record-grade-revision! user-id sitting-id
                      {:raw-score 70 :criteria passing-criteria :explanation "Pass."})]
          (is (= :submitted submitted) "the first finalize closes the sitting")
          (is (= :already-closed late-draft)
            "a blur that lands after the finalize is an outcome, not a failure")
          (is (= :already-closed resubmitted) "a second finalize changes nothing")
          (is (= "Answer" (-> (curriculum/sitting-for-grade user-id sitting-id)
                             :answers first :answer))
            "the first submitted answer set is frozen")
          (let [detail (curriculum/curriculum-detail 0 user-id curriculum-id)
                blocks (:blocks detail)]
            (is (some? (:completed_at (first blocks))))
            (is (some? (:unlocked_at (second blocks)))))
          (curriculum/record-grade-revision! user-id sitting-id
            {:raw-score 0 :criteria zero-criteria :explanation "Lower regrade."})
          (is (some? (:completed_at
                       (first (:blocks (curriculum/curriculum-detail
                                        0 user-id curriculum-id)))))
            "a later lower revision cannot reverse completion")
          (curriculum/unlock-next-block! user-id second-block)
          (let [blocks (:blocks (curriculum/curriculum-detail 0 user-id curriculum-id))]
            (is (nil? (:completed_at (second blocks))))
            (is (some? (:unlocked_at (nth blocks 2)))
              "an override unlocks the next block without completing this one")))
        (finally
          (jdbc/execute-one! db/ds ["DELETE FROM topics WHERE id = ?" topic-id])
          (jdbc/execute-one! db/ds ["DELETE FROM users WHERE id = ?" user-id]))))))

(deftest a-staged-curriculum-reads-as-nil-not-as-an-exception
  ;; The readers a MOUNTED page holds must answer nil for a curriculum that is
  ;; staged. `require-owned!` reported it as a server exception instead, which
  ;; Electric propagates as a reactor failure — so deleting a curriculum closed
  ;; the WebSocket before the client could navigate away, and the page froze on
  ;; the row it had just removed. Writers keep throwing; a writer that names a
  ;; row it does not own IS a caller bug.
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [tag (str "curriculum-del-" (System/nanoTime))
          user-id (:id (jdbc/execute-one! db/ds
                         ["INSERT INTO users (username, password_hash)
                           VALUES (?, 'x') RETURNING id" tag]
                         {:builder-fn rs/as-unqualified-maps}))
          other-user-id (:id (jdbc/execute-one! db/ds
                               ["INSERT INTO users (username, password_hash)
                                 VALUES (?, 'x') RETURNING id" (str tag "-other")]
                               {:builder-fn rs/as-unqualified-maps}))]
      (try
        (let [curriculum-id (curriculum/create-curriculum! user-id
                              {:title tag :daily-minutes 30 :study-weekdays [1 2 3 4 5]})]
          (curriculum/add-block! user-id curriculum-id "Block one")
          (is (some? (curriculum/curriculum-detail 0 user-id curriculum-id)))
          (is (some? (curriculum/build-schedule 0 user-id curriculum-id)))
          (is (thrown? clojure.lang.ExceptionInfo
                (curriculum/stage-curriculum-for-deletion! other-user-id curriculum-id))
            "another user cannot stage this curriculum")
          (let [{:keys [entry-id]} (curriculum/stage-curriculum-for-deletion!
                                   user-id curriculum-id)]
          (is (nil? (curriculum/curriculum-detail 0 user-id curriculum-id))
            "a reader answers nil for a staged curriculum")
          (is (nil? (curriculum/build-schedule 0 user-id curriculum-id))
            "the schedule panel re-queries on the delete's own bump")
          (is (empty? (curriculum/list-curricula 0 user-id))
            "the staged curriculum stays out of its list")
          (is (thrown? clojure.lang.ExceptionInfo
                (curriculum/stage-curriculum-for-deletion! user-id curriculum-id))
            "a WRITER still blames a caller that names a row it does not own")
          (is (:success (undo/undo-entry! user-id entry-id)))
          (is (some? (curriculum/curriculum-detail 0 user-id curriculum-id))
            "Undo restores the same root without rebuilding it")))
        (finally
          (jdbc/execute-one! db/ds ["DELETE FROM users WHERE id = ?" user-id])
          (jdbc/execute-one! db/ds ["DELETE FROM users WHERE id = ?" other-user-id]))))))
