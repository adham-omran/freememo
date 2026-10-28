(ns freememo.comprehension-test
  "Database checks for frozen diagnostic evidence, ownership, and answer history."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [freememo.assessment :as assessment]
            [freememo.comprehension :as cycle]
            [freememo.comprehension-ocr :as scan]
            [freememo.credits :as credits]
            [freememo.db :as db]
            [freememo.kg-llm :as llm]
            [freememo.settings :as settings]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [java.awt.image BufferedImage]
           [java.io ByteArrayOutputStream]
           [javax.imageio ImageIO]))

(def opts {:builder-fn rs/as-unqualified-maps})
(use-fixtures :once (fn [run] (cycle/ensure-schema!) (run)))

(defn- account! []
  (let [tag (str "comprehension-test-" (random-uuid))]
    (:id (jdbc/execute-one! db/ds
           ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
            tag (str tag "@example.test")] opts))))

(defn- source! [uid]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, title, kind, content_text)
           VALUES (?, 'Diagnostic source', 'markdown', 'A force changes motion.') RETURNING id"
          uid] opts)))

(defn- form [evidence _]
  (let [material (first (:materials evidence))
        topic-id (:topic-id (first (:evidence material)))
        citation {:material_id (:material-id material) :topic_id topic-id :quote "A force changes motion."}]
    {:blueprint {:assessed_outcomes [] :unassessed_outcomes []
                 :evidence_packets (:materials evidence)}
     :questions [{:id "q1" :format "short-essay" :prompt "Explain how force changes motion."
                  :points 10 :rubric {:reasoning "Connect force and motion."}
                  :evidence [citation] :fact_ids []}]}))

(defn- grade [assessment answers]
  {:raw-score 50 :criteria [{:question_id "q1" :points 5 :max_points 10}]
   :explanation "The explanation needs a connection."
   :gaps [{:question_id "q1" :criterion "reasoning" :category "reasoning"
           :reason "Connect the force to the motion."
           :evidence (get-in assessment [:questions 0 :evidence]) :fact_ids []}]})

(defn- with-accounts [f]
  (let [uid (account!) other (account!) tid (source! uid)]
    (try (f uid other tid)
         (finally
           (jdbc/execute-one! db/ds ["DELETE FROM users WHERE id IN (?, ?)" uid other])))))

(deftest frozen-source-and-ownership
  (with-accounts
    (fn [uid other tid]
      (with-redefs [assessment/generate! (fn [_ evidence options] (form evidence options))]
        (let [started (cycle/start-cycle! uid tid 30) cid (:cycle-id started)]
          (is (:ok started))
          (is (not (:ok (cycle/start-cycle! other tid 30))))
          (jdbc/execute-one! db/ds ["UPDATE topics SET content_text = 'Changed later.' WHERE id = ?" tid])
          (is (= "A force changes motion."
                (get-in (cycle/cycle-detail uid cid) [:materials 0 :evidence 0 :text])))
          (is (nil? (cycle/cycle-detail other cid)))
          (is (not (:ok (cycle/save-draft-answers! other cid {"q1" "forged"}))))
          (is (not (:ok (cycle/save-draft-answers! uid cid {"q2" "foreign question"}))))
          (is (:ok (cycle/save-draft-answers! uid cid {"q1" "Draft"}))))))))

(deftest save-before-grading-and-retry-one-attempt
  (with-accounts
    (fn [uid _ tid]
      (let [calls (atom 0)]
        (with-redefs [assessment/generate! (fn [_ evidence options] (form evidence options))
                      assessment/grade! (fn [_ assessment answers]
                                          (swap! calls inc)
                                          (if (= 1 @calls) (throw (ex-info "Provider unavailable." {}))
                                              (grade assessment answers)))]
          (let [cid (:cycle-id (cycle/start-cycle! uid tid 30))
                failed (cycle/submit-answers! uid cid {"q1" "A force pushes."})
                before (cycle/cycle-detail uid cid)
                retry (cycle/submit-answers! uid cid {"q1" "A force pushes."})
                repeated (cycle/submit-answers! uid cid {"q1" "A force pushes."})
                after (cycle/cycle-detail uid cid)]
            (is (not (:ok failed)))
            (is (= "A force pushes." (get-in before [:submissions 0 :answers "q1"])))
            (is (= "failed" (get-in before [:submissions 0 :status])))
            (is (:ok retry))
            (is (= (:submission-id retry) (:submission-id repeated)))
            (is (= 2 @calls))
            (is (= 1 (count (:submissions after))))
            (is (= 1 (count (:gaps after))))))))))

(deftest study-retains-gap-and-targeted-reassessment-history
  (with-accounts
    (fn [uid other tid]
      (let [generation-input (atom nil)]
        (with-redefs [assessment/generate! (fn [_ evidence options]
                                            (reset! generation-input [evidence options])
                                            (form evidence options))
                      assessment/grade! (fn [_ assessment answers] (grade assessment answers))]
          (let [cid (:cycle-id (cycle/start-cycle! uid tid 30))]
            (cycle/submit-answers! uid cid {"q1" "A force pushes."})
            (let [gap (first (:gaps (cycle/cycle-detail uid cid)))
                  selected (cycle/save-followup! uid (:id gap) "source-review")
                  followup-id (get-in selected [:followup :id])]
              (is (not (:ok (cycle/save-followup! other (:id gap) "source-review"))))
              (is (:ok (cycle/finish-followup! uid followup-id)))
              (is (= "studied" (:status (first (:gaps (cycle/cycle-detail uid cid))))))
              (jdbc/execute-one! db/ds ["UPDATE topics SET content_text = 'Changed later.' WHERE id = ?" tid])
              (let [retest (cycle/reassess! uid (:id gap) 15)
                    child (:cycle-id retest)]
                (is (:ok retest))
                (is (= (:id gap) (:parent_gap_id (cycle/cycle-detail uid child))))
                (is (= "A force changes motion." (get-in @generation-input [0 :materials 0 :evidence 0 :text])))
                (is (= "reasoning" (get-in @generation-input [1 :target-outcomes 0 :criterion])))
                (is (= "studied" (:status (first (:gaps (cycle/cycle-detail uid cid))))))
                (cycle/submit-answers! uid child {"q1" "A force changes motion."})
                (is (= "reassessed" (:status (first (:gaps (cycle/cycle-detail uid cid))))))))))))))

(defn- png []
  (let [out (ByteArrayOutputStream.)]
    (ImageIO/write (BufferedImage. 20 20 BufferedImage/TYPE_INT_RGB) "png" out)
    (.toByteArray out)))

(defn- set-review! [sid]
  (jdbc/execute-one! db/ds ["UPDATE comprehension_scans SET status = 'review' WHERE id = ?" sid]))

(defn- two-question-form [evidence options]
  (let [exam (form evidence options)]
    (update exam :questions conj (assoc (first (:questions exam)) :id "q2"))))

(deftest accepting-another-scan-preserves-saved-answer-improvements
  (with-accounts
    (fn [uid _ tid]
      (with-redefs [assessment/generate! (fn [_ evidence options] (two-question-form evidence options))
                    assessment/grade! (fn [_ assessment answers] (grade assessment answers))]
        (let [cid (:cycle-id (cycle/start-cycle! uid tid 30))
              first-scan (:scan-id (scan/stage-scan! uid cid (png) "image/png" "first.png"))
              second-scan (:scan-id (scan/stage-scan! uid cid (png) "image/png" "second.png"))]
          (set-review! first-scan)
          (set-review! second-scan)
          (is (:ok (scan/accept-scan! uid cid first-scan
                     [{:question_id "q1" :text "Original handwritten answer." :page_numbers [1]}])))
          (is (:ok (scan/accept-scan! uid cid second-scan
                     [{:question_id "q2" :text "Original second answer." :page_numbers [1]}])))
          (is (:ok (cycle/submit-answers! uid cid (:answers (cycle/cycle-detail uid cid)))))
          (is (:ok (cycle/save-draft-answers! uid cid
                     {"q1" "Improved saved argument." "q2" "Original second answer."})))
          (is (:ok (scan/accept-scan! uid cid second-scan
                     [{:question_id "q2" :text "Corrected second transcript." :page_numbers [1]}])))
          (is (= "Improved saved argument." (get-in (cycle/cycle-detail uid cid) [:answers "q1"])))
          (is (= "Corrected second transcript." (get-in (cycle/cycle-detail uid cid) [:answers "q2"])))
          (is (:ok (cycle/submit-answers! uid cid (:answers (cycle/cycle-detail uid cid)))))
          (is (= "answer-revision"
                (:revision_kind (last (:submissions (cycle/cycle-detail uid cid)))))))))))

(deftest unchanged-scanned-answers-retain-their-own-uncertainty
  (with-accounts
    (fn [uid _ tid]
      (with-redefs [assessment/generate! (fn [_ evidence options] (two-question-form evidence options))
                    assessment/grade! (fn [_ assessment answers] (grade assessment answers))]
        (let [cid (:cycle-id (cycle/start-cycle! uid tid 30))
              sid (:scan-id (scan/stage-scan! uid cid (png) "image/png" "uncertain.png"))]
          (set-review! sid)
          (is (:ok (scan/accept-scan! uid cid sid
                     [{:question_id "q1" :text "Ambiguous handwriting." :uncertain true :page_numbers [1]}])))
          (is (:ok (cycle/submit-answers! uid cid {"q1" "Ambiguous handwriting." "q2" "First answer."})))
          (is (:ok (cycle/submit-answers! uid cid {"q1" "Ambiguous handwriting." "q2" "Improved answer."})))
          (let [latest (last (:submissions (cycle/cycle-detail uid cid)))]
            (is (= "answer-revision" (:revision_kind latest)))
            (is (= ["q1"] (get-in latest [:grade :uncertain_questions])))
            (is (= "uncertain" (get-in latest [:grade :gaps 0 :category]))))
          (is (:ok (cycle/submit-answers! uid cid {"q1" "A new typed answer." "q2" "Improved answer."})))
          (let [latest (last (:submissions (cycle/cycle-detail uid cid)))]
            (is (empty? (get-in latest [:grade :uncertain_questions])))
            (is (= "reasoning" (get-in latest [:grade :gaps 0 :category])))))))))

(deftest direct-submit-distinguishes-improvements-from-transcript-corrections
  (with-accounts
    (fn [uid _ tid]
      (with-redefs [assessment/generate! (fn [_ evidence options] (form evidence options))
                    assessment/grade! (fn [_ assessment answers] (grade assessment answers))]
        (let [cid (:cycle-id (cycle/start-cycle! uid tid 30))
              sid (:scan-id (scan/stage-scan! uid cid (png) "image/png" "answer.png"))]
          (set-review! sid)
          (is (:ok (scan/accept-scan! uid cid sid
                     [{:question_id "q1" :text "Original answer." :page_numbers [1]}])))
          (is (:ok (cycle/submit-answers! uid cid {"q1" "Original answer."})))
          (is (:ok (scan/accept-scan! uid cid sid
                     [{:question_id "q1" :text "Corrected original answer." :page_numbers [1]}])))
          (is (:ok (cycle/submit-answers! uid cid {"q1" "An improved answer with new reasoning."})))
          (is (= "answer-revision"
                (:revision_kind (last (:submissions (cycle/cycle-detail uid cid))))))
          (is (:ok (scan/accept-scan! uid cid sid
                     [{:question_id "q1" :text "Another transcript correction." :page_numbers [1]}])))
          (is (:ok (cycle/submit-answers! uid cid {"q1" "An improved answer with new reasoning."})))
          (is (= "answer-revision"
                (:revision_kind (last (:submissions (cycle/cycle-detail uid cid)))))))))))

(deftest remapping-a-transcript-remains-a-transcription-correction
  (with-accounts
    (fn [uid _ tid]
      (with-redefs [assessment/generate! (fn [_ evidence options] (two-question-form evidence options))
                    assessment/grade! (fn [_ assessment answers] (grade assessment answers))]
        (let [cid (:cycle-id (cycle/start-cycle! uid tid 30))
              sid (:scan-id (scan/stage-scan! uid cid (png) "image/png" "answer.png"))]
          (set-review! sid)
          (is (:ok (scan/accept-scan! uid cid sid
                     [{:question_id "q1" :text "The handwritten answer." :page_numbers [1]}])))
          (is (:ok (cycle/submit-answers! uid cid (:answers (cycle/cycle-detail uid cid)))))
          (is (:ok (scan/accept-scan! uid cid sid
                     [{:question_id "q2" :text "The handwritten answer." :page_numbers [1]}])))
          (is (= "" (get-in (cycle/cycle-detail uid cid) [:answers "q1"])))
          (is (:ok (cycle/submit-answers! uid cid (:answers (cycle/cycle-detail uid cid)))))
          (is (= "transcription-correction"
                (:revision_kind (last (:submissions (cycle/cycle-detail uid cid)))))))))))

(deftest scanned-answer-originals-corrections-and-uncertainty
  (with-accounts
    (fn [uid other tid]
      (with-redefs [assessment/generate! (fn [_ evidence options] (form evidence options))
                    assessment/grade! (fn [_ assessment answers] (grade assessment answers))]
        (let [cid (:cycle-id (cycle/start-cycle! uid tid 30))
              bytes (png)
              sid (:scan-id (scan/stage-scan! uid cid bytes "image/png" "answers.png"))]
          (is sid)
          (is (nil? (scan/scan-original other sid)))
          (is (= (seq bytes) (seq (:bytes (scan/scan-original uid sid)))))
          (is (not (:ok (cycle/submit-answers! uid cid {"q1" "unreviewed"}))))
          (set-review! sid)
          (is (:ok (scan/accept-scan! uid cid sid
                     [{:question_id "q1" :text "[unclear] changes motion." :page_numbers [1]}])))
          (let [submitted (cycle/submit-answers! uid cid (:answers (cycle/cycle-detail uid cid)))
                original (first (:submissions (cycle/cycle-detail uid cid)))]
            (is (:ok submitted))
            (is (= "initial" (:revision_kind original)))
            (is (= "uncertain" (:category (first (:gaps (cycle/cycle-detail uid cid))))))
            (is (:ok (scan/accept-scan! uid cid sid
                       [{:question_id "q1" :text "A force changes motion." :page_numbers [1]}])))
            (cycle/submit-answers! uid cid (:answers (cycle/cycle-detail uid cid)))
            (is (= ["initial" "transcription-correction"]
                  (mapv :revision_kind (:submissions (cycle/cycle-detail uid cid)))))
            (cycle/save-draft-answers! uid cid {"q1" "An improved argument about force."})
            (cycle/submit-answers! uid cid {"q1" "An improved argument about force."})
            (is (= "answer-revision" (:revision_kind (last (:submissions (cycle/cycle-detail uid cid))))))
            (is (= "[unclear] changes motion." (get-in original [:answers "q1"])))))))))

(deftest multiple-photo-files-can-continue-one-answer
  (with-accounts
    (fn [uid _ tid]
      (with-redefs [assessment/generate! (fn [_ evidence options] (form evidence options))]
        (let [cid (:cycle-id (cycle/start-cycle! uid tid 30))
              a (:scan-id (scan/stage-scan! uid cid (png) "image/png" "one.png"))
              b (:scan-id (scan/stage-scan! uid cid (png) "image/png" "two.png"))]
          (doseq [[sid text] [[a "First page."] [b "Second page."]]]
            (set-review! sid)
            (is (:ok (scan/accept-scan! uid cid sid [{:question_id "q1" :text text :page_numbers [1]}]))))
          (is (= "First page.\n\nSecond page."
                (get-in (cycle/cycle-detail uid cid) [:answers "q1"]))))))))

(deftest assignment-and-upload-boundaries
  (is (thrown? clojure.lang.ExceptionInfo
        (scan/normalize-assignments! #{"q1"} 1 [{:question_id "q2" :text "answer" :page_numbers [1]}])))
  (is (thrown? clojure.lang.ExceptionInfo
        (scan/normalize-assignments! #{"q1"} 1 [{:question_id "q1" :text "answer" :page_numbers [2]}])))
  (is (thrown? clojure.lang.ExceptionInfo
        (scan/validate-suggestions! #{"q1"} 1 {:answers [{:question_id "q2" :text "guess" :uncertain false}]
                                                :uncertainties []})))
  (with-accounts
    (fn [uid _ tid]
      (with-redefs [assessment/generate! (fn [_ evidence options] (form evidence options))]
        (let [cid (:cycle-id (cycle/start-cycle! uid tid 30))]
          (is (not (:ok (scan/stage-scan! uid cid (png) "image/jpeg" "false.jpg"))))
          (is (empty? (:scans (cycle/cycle-detail uid cid)))))))))

(deftest new-draft-and-abandoned-upload
  (with-accounts
    (fn [uid other tid]
      (with-redefs [assessment/generate! (fn [_ evidence options] (form evidence options))]
        (let [cid (:cycle-id (cycle/start-cycle! uid tid 30))
              sid (:scan-id (scan/stage-scan! uid cid (png) "image/png" "mistake.png"))]
          (cycle/save-draft-answers! uid cid {"q1" "My first draft."})
          (is (= "initial" (:draft_kind (cycle/cycle-detail uid cid))))
          (is (not (:ok (scan/delete-scan! other cid sid))))
          (is (:ok (scan/delete-scan! uid cid sid)))
          (is (nil? (scan/scan-detail uid sid))))))))

(deftest concurrent-submit-has-one-grade-owner
  (with-accounts
    (fn [uid _ tid]
      (let [entered (promise) release (promise) calls (atom 0)]
        (with-redefs [assessment/generate! (fn [_ evidence options] (form evidence options))
                      assessment/grade! (fn [_ assessment answers]
                                          (swap! calls inc)
                                          (deliver entered true)
                                          (deref release 10000 nil)
                                          (grade assessment answers))]
          (let [cid (:cycle-id (cycle/start-cycle! uid tid 30))
                first-run (future (cycle/submit-answers! uid cid {"q1" "The first answer."}))]
            (try
              (is (deref entered 10000 nil))
              (is (not (:ok (cycle/submit-answers! uid cid {"q1" "The first answer."}))))
              (deliver release true)
              (is (:ok (deref first-run 10000 nil)))
              (is (= 1 @calls))
              (is (= 1 (count (:submissions (cycle/cycle-detail uid cid)))))
              (finally (deliver release true)))))))))

(deftest targeted-practice-uses-owned-live-facts-without-changing-review-state
  (with-accounts
    (fn [uid _ tid]
      (let [entity (:id (jdbc/execute-one! db/ds
                         ["INSERT INTO kg_entities (user_id, label) VALUES (?, 'Force') RETURNING id" uid] opts))
            predicate (:id (jdbc/execute-one! db/ds
                            ["INSERT INTO kg_predicates (user_id, slug, label, status)
                              VALUES (?, 'changes', 'changes', 'approved') RETURNING id" uid] opts))
            fact (:id (jdbc/execute-one! db/ds
                       ["INSERT INTO kg_facts (user_id, subject_entity_id, predicate_id, object_literal, graph_topic_id, status)
                         VALUES (?, ?, ?, 'motion', ?, 'approved') RETURNING id" uid entity predicate tid] opts))
            qid (db/create-kg-question! uid "atomic" "What does force change?" "Motion" [fact] nil)]
        (with-redefs [assessment/generate! (fn [_ evidence options]
                                            (assoc-in (form evidence options) [:questions 0 :fact_ids] [fact]))
                      assessment/grade! (fn [_ assessment answers]
                                          (assoc-in (grade assessment answers) [:gaps 0 :fact_ids] [fact]))]
          (let [cid (:cycle-id (cycle/start-cycle! uid tid 30))]
            (cycle/submit-answers! uid cid {"q1" "A force pushes."})
            (let [gap (first (:gaps (cycle/cycle-detail uid cid)))
                  selected (cycle/save-followup! uid (:id gap) "targeted-questions")
                  sid (get-in selected [:followup :session_id])
                  repeated (cycle/save-followup! uid (:id gap) "targeted-questions")]
              (is (:ok selected))
              (is sid)
              (is (= sid (get-in repeated [:followup :session_id])))
              (is (= [qid] (:question-ids (db/get-active-kg-session uid "quiz"))))
              (is (= 0 (:fsrs_reps (jdbc/execute-one! db/ds
                                    ["SELECT fsrs_reps FROM kg_questions WHERE id = ?" qid] opts)))))))))))

(deftest correcting-assignment-clears-old-answer-and-keeps-file-order
  (with-accounts
    (fn [uid _ tid]
      (with-redefs [assessment/generate!
                    (fn [_ evidence options]
                      (let [base (form evidence options) q (get-in base [:questions 0])]
                        (assoc base :questions [q (assoc q :id "q2") (assoc q :id "q3")])))]
        (let [cid (:cycle-id (cycle/start-cycle! uid tid 30))
              a (:scan-id (scan/stage-scan! uid cid (png) "image/png" "first.png"))
              b (:scan-id (scan/stage-scan! uid cid (png) "image/png" "second.png"))]
          (cycle/save-draft-answers! uid cid {"q3" "Unrelated typed draft."})
          (doseq [[sid text] [[b "Second page."] [a "First page."]]]
            (set-review! sid)
            (is (:ok (scan/accept-scan! uid cid sid [{:question_id "q1" :text text :page_numbers [1]}]))))
          (is (= "First page.\n\nSecond page." (get-in (cycle/cycle-detail uid cid) [:answers "q1"])))
          (is (:ok (scan/accept-scan! uid cid a [{:question_id "q2" :text "Corrected assignment." :page_numbers [1]}])))
          (is (= "Second page." (get-in (cycle/cycle-detail uid cid) [:answers "q1"])))
          (is (= "Corrected assignment." (get-in (cycle/cycle-detail uid cid) [:answers "q2"])))
          (is (= "Unrelated typed draft." (get-in (cycle/cycle-detail uid cid) [:answers "q3"]))))))))

(deftest expired-grading-lease-can-retry-without-a-new-attempt
  (with-accounts
    (fn [uid _ tid]
      (with-redefs [assessment/generate! (fn [_ evidence options] (form evidence options))
                    assessment/grade! (fn [& _] (throw (ex-info "Provider interrupted." {})))]
        (let [cid (:cycle-id (cycle/start-cycle! uid tid 30))]
          (cycle/submit-answers! uid cid {"q1" "Answer."})
          (let [sid (:id (first (:submissions (cycle/cycle-detail uid cid))))]
            (jdbc/execute-one! db/ds
              ["UPDATE comprehension_submissions SET status = 'grading',
                 grading_started_at = now() - interval '21 minutes', grading_token = ? WHERE id = ?"
               (random-uuid) sid])
            (with-redefs [assessment/grade! (fn [_ assessment answers] (grade assessment answers))]
              (is (:ok (cycle/submit-answers! uid cid {"q1" "Answer."}))))
            (is (= 1 (count (:submissions (cycle/cycle-detail uid cid)))))
            (is (= "graded" (:status (first (:submissions (cycle/cycle-detail uid cid))))))))))))

(deftest expired-ocr-lease-preserves-literal-transcript-and-original-scan
  (with-accounts
    (fn [uid _ tid]
      (with-redefs [assessment/generate! (fn [_ evidence options] (form evidence options))
                    settings/get-openrouter-api-key (fn [_] "test-key")
                    credits/check-cost-billed-balance! (fn [_] {:ok true})
                    credits/record-cost-charge! (fn [& _] nil)
                    llm/chat! (fn [& _]
                                {:parsed {:answers [{:question_id "q1" :text "Force never changes motion." :uncertain false}]
                                          :uncertainties []}
                                 :cost 0.0 :tokens []})]
        (let [cid (:cycle-id (cycle/start-cycle! uid tid 30))
              bytes (png)
              sid (:scan-id (scan/stage-scan! uid cid bytes "image/png" "answers.png"))]
          (jdbc/execute-one! db/ds
            ["UPDATE comprehension_scans SET status = 'transcribing',
               transcription_started_at = now() - interval '21 minutes', transcription_token = ? WHERE id = ?"
             (random-uuid) sid])
          (is (:ok (scan/transcribe-scan! uid cid sid)))
          (is (= "review" (:status (scan/scan-detail uid sid))))
          (is (= "Force never changes motion." (get-in (scan/scan-detail uid sid) [:suggestions 0 :text])))
          (is (empty? (:answers (cycle/cycle-detail uid cid))))
          (is (= (seq bytes) (seq (:bytes (scan/scan-original uid sid)))))
          (is (= "A force changes motion."
                (:content_text (jdbc/execute-one! db/ds ["SELECT content_text FROM topics WHERE id = ?" tid] opts)))))))))

(deftest source-recursion-excludes-another-users-child
  (with-accounts
    (fn [uid other tid]
      (jdbc/execute-one! db/ds
        ["INSERT INTO topics (parent_id, user_id, title, kind, content_text)
          VALUES (?, ?, 'Foreign child', 'markdown', 'Another learner private text.')" tid other])
      (jdbc/execute-one! db/ds
        ["INSERT INTO topics (parent_id, user_id, title, kind, content_text)
          VALUES (?, NULL, 'Inherited child', 'markdown', 'Owned through its parent.')" tid])
      (with-redefs [assessment/generate! (fn [_ evidence options] (form evidence options))]
        (let [cid (:cycle-id (cycle/start-cycle! uid tid 30))
              text (map :text (get-in (cycle/cycle-detail uid cid) [:materials 0 :evidence]))]
          (is (some #{"Owned through its parent."} text))
          (is (not-any? #{"Another learner private text."} text)))))))

(deftest explicit-practice-session-does-not-resume-another-active-session
  (with-accounts
    (fn [uid other tid]
      (let [qid (:id (jdbc/execute-one! db/ds
                       ["INSERT INTO kg_questions (user_id, kind, question, reference_answer)
                         VALUES (?, 'atomic', 'What changes motion?', 'Force') RETURNING id" uid] opts))
            selected (db/create-kg-session! uid "quiz" [tid] [qid] nil)
            later (db/create-kg-session! uid "quiz" [tid] [qid] nil)
            exam (db/create-kg-session! uid "exam" [tid] [qid] 600)]
        (is (= selected (:id (db/get-active-kg-session uid "quiz" selected))))
        (is (= [qid] (:question-ids (db/get-active-kg-session uid "quiz" selected))))
        (is (= later (:id (db/get-active-kg-session uid "quiz"))))
        (is (= exam (:id (db/get-active-kg-session uid "exam"))))
        (is (nil? (db/get-active-kg-session other "quiz" selected)))
        (is (nil? (db/get-active-kg-session uid "quiz" exam)))
        (jdbc/execute-one! db/ds ["UPDATE kg_sessions SET finished_at = now() WHERE id = ?" selected])
        (is (nil? (db/get-active-kg-session uid "quiz" selected)))
        (is (= later (:id (db/get-active-kg-session uid "quiz"))))))))
