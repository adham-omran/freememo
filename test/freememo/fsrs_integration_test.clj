(ns freememo.fsrs-integration-test
  "DB-backed checks for the FSRS Review server layer: verdict→rating mapping
   (no DB), plus apply-fsrs-review! and draw-review-queue against a real
   Postgres. Covers both item types — a question and a card item — since they share
   one draw, one scheduler step and one review log
   (plans/cards-in-quiz-queue.md §3, §4). The DB tests SKIP (not fail) when no
   database is reachable, so the suite stays green in DB-less environments; run them
   with the app's dev DB up."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]
            [freememo.kg-grade :as grade]))

;; --- pure: verdict → FSRS rating (binary) --------------------------------

(deftest verdict->rating-mapping
  (is (= 3 (grade/verdict->rating "correct")))
  (is (= 1 (grade/verdict->rating "partial")))
  (is (= 1 (grade/verdict->rating "incorrect"))))

;; --- DB-backed (skipped when no Postgres) --------------------------------

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(def ^:private sched (freememo.fsrs/make-scheduler {:enable-fuzzing false}))

(defn- new-user! []
  (let [tag (str "fsrs-it-" (System/nanoTime))]
    (:id (jdbc/execute-one! db/ds
           ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
            tag (str tag "@example.test")]
           {:builder-fn rs/as-unqualified-maps}))))

(defn- new-question! [uid n]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO kg_questions (user_id, kind, question, reference_answer)
           VALUES (?, 'atomic', ?, ?) RETURNING id"
          uid (str "Q" n) (str "A" n)]
         {:builder-fn rs/as-unqualified-maps})))

(defn- new-root-topic! [uid]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, title, kind) VALUES (?, 'FSRS IT doc', 'basic')
           RETURNING id" uid]
         {:builder-fn rs/as-unqualified-maps})))

(defn- new-card! [root-id kind fields]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO flashcards (topic_id, root_topic_id, kind, question, answer, cloze)
           VALUES (?, ?, ?, ?, ?, ?) RETURNING id"
          root-id root-id kind (:question fields) (:answer fields) (:cloze fields)]
         {:builder-fn rs/as-unqualified-maps})))

(defn- card-schedule-of [card-id ord]
  (jdbc/execute-one! db/ds
    ["SELECT state, step, reps, lapses, due FROM card_schedules
      WHERE flashcard_id = ? AND ord = ?" card-id ord]
    {:builder-fn rs/as-unqualified-maps}))

(deftest ^:db apply-review-and-queue
  (if-not (db-up?)
    (println "SKIP fsrs-integration: no Postgres reachable (start the dev DB to run).")
    (let [uid (new-user!)]
      (try
        (db/setup-schema) ; idempotent — ensures FSRS columns/kg_reviews exist
        (let [qid (new-question! uid 1)]
          (testing "cold-start: a never-reviewed question is drawn as new"
            (is (= [[:question qid]] (db/draw-review-queue uid 20 9999))))
          (testing "first Good → Learning, stays due today, one log row, counted new"
            (let [r (db/apply-fsrs-review! uid [:question qid] 3
                      {:verdict "correct" :grade-source "ai"} sched false)]
              (is (= 1 (:state r)))            ; Learning
              (is (true? (:due-today? r)))     ; +10min ⇒ still today
              (is (= 1 (:reps r)))
              (is (= 1 (:new-today (db/fsrs-daily-counts uid))))))
          (testing "second Good (same day) → graduates to Review, due in the future"
            (let [r (db/apply-fsrs-review! uid [:question qid] 3
                      {:verdict "correct" :grade-source "ai"} sched false)]
              (is (= 2 (:state r)))            ; Review
              (is (false? (:due-today? r)))
              (is (= 2 (:reps r)))))
          (testing "a Review item due in the future is not in today's queue"
            (is (= [] (db/draw-review-queue uid 20 9999))))
          (testing "new-per-day cap bounds the draw (fresh user, 0 used today)"
            (let [u2 (new-user!)]
              (try
                (dotimes [i 3] (new-question! u2 (+ 10 i)))
                (is (= 2 (count (db/draw-review-queue u2 2 9999))))
                (finally
                  (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" u2]))))))
        (finally
          (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" uid]))))))

(deftest ^:db card-items-in-the-queue
  (if-not (db-up?)
    (println "SKIP fsrs-integration/cards: no Postgres reachable.")
    (let [uid (new-user!)]
      (try
        (db/setup-schema)
        (let [root (new-root-topic! uid)
              basic (new-card! root "basic" {:question "What is 2+2?" :answer "4"})
              cz (new-card! root "cloze" {:cloze "{{c1::Paris}} is in {{c2::France}}"})
              occl (new-card! root "occlusion" {:question "hidden"})]
          (testing "a basic card is one item; a cloze card is one item per deletion"
            (let [drawn (set (db/draw-review-queue uid 20 9999))]
              (is (contains? drawn [:card basic 0]))
              (is (contains? drawn [:card cz 1]))
              (is (contains? drawn [:card cz 2]))
              (testing "an occlusion card is never drawn — no typed answer exists"
                (is (not-any? #(= occl (second %)) drawn)))))
          (testing "the prompt hides one deletion and reveals the others"
            (let [item (db/get-card-item uid cz 1)]
              (is (= "[...] is in France" (:prompt item)))
              (is (= "Paris" (:answer item))))
            (is (= "What is 2+2?" (:prompt (db/get-card-item uid basic 0)))))
          (testing "no schedule row exists until the first review (lazy §2.3)"
            (is (nil? (card-schedule-of cz 1))))
          (testing "a self-graded Good advances the item and logs grade_source"
            (let [r (db/apply-fsrs-review! uid [:card cz 1] 3
                      {:grade-source "self"} sched false)]
              (is (= 1 (:state r)))
              (is (= 1 (:reps r)))
              (is (= 1 (:reps (card-schedule-of cz 1))))
              (is (nil? (:state (card-schedule-of cz 2)))) ; sibling ord untouched
              (let [row (first (db/fsrs-review-log uid 10))]
                (is (= "self" (:grade_source row)))
                (is (= cz (:flashcard_id row)))
                (is (nil? (:question_id row)))
                (testing "the list re-masks the raw cloze snapshot"
                  (is (= "[...] is in France" (:prompt row)))))))
          (testing "a partly-reviewed cloze card still offers its unreviewed ords"
            ;; Exercises the second (array_agg) pass of fresh-card-items: cz now has a
            ;; schedule row, so ord 2 can only come from the partial scan.
            (let [drawn (set (db/draw-review-queue uid 20 9999))]
              (is (contains? drawn [:card cz 2]))
              (is (not (contains? drawn [:card cz 1])))))
          (testing "editing the deletion away drops the stale item from the draw"
            (jdbc/execute! db/ds
              ["UPDATE flashcards SET cloze = '{{c1::Paris}} is a city' WHERE id = ?" cz])
            (let [drawn (set (db/draw-review-queue uid 20 9999))]
              (is (not (contains? drawn [:card cz 2])))
              (is (nil? (db/get-card-item uid cz 2)))))
          (testing "the new tier alternates the two banks under one cap (D11)"
            ;; Cards outnumber questions here, which is the case the round-robin
            ;; exists for: a straight concat would fill the cap with cards alone.
            (let [u3 (new-user!)]
              (try
                (let [root3 (new-root-topic! u3)]
                  (dotimes [i 2] (new-question! u3 (+ 20 i)))
                  (dotimes [i 6]
                    (new-card! root3 "basic" {:question (str "Q" i) :answer (str "A" i)}))
                  (let [drawn (db/draw-review-queue u3 4 9999)
                        kinds (frequencies (map first drawn))]
                    (is (= 4 (count drawn)))
                    (is (= 2 (get kinds :question)))
                    (is (= 2 (get kinds :card)))))
                (finally
                  (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" u3])))))
          (testing "another user's card is neither drawn nor readable"
            (let [u2 (new-user!)]
              (try
                (is (nil? (db/get-card-item u2 basic 0)))
                (is (nil? (db/apply-fsrs-review! u2 [:card basic 0] 3 {} sched false)))
                (finally
                  (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" u2]))))))
        (finally
          (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" uid]))))))
