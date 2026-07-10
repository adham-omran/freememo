(ns freememo.fsrs-integration-test
  "DB-backed checks for the FSRS Review server layer: verdict→rating mapping
   (no DB), plus apply-fsrs-review! and draw-fsrs-due-queue against a real
   Postgres. The DB tests SKIP (not fail) when no database is reachable, so the
   suite stays green in DB-less environments; run them with the app's dev DB up."
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

(defn- state-of [qid]
  (jdbc/execute-one! db/ds
    ["SELECT fsrs_state, fsrs_step, fsrs_reps, fsrs_lapses, fsrs_due FROM kg_questions WHERE id = ?" qid]
    {:builder-fn rs/as-unqualified-maps}))

(deftest ^:db apply-review-and-queue
  (if-not (db-up?)
    (println "SKIP fsrs-integration: no Postgres reachable (start the dev DB to run).")
    (let [uid (new-user!)]
      (try
        (db/setup-schema) ; idempotent — ensures FSRS columns/kg_reviews exist
        (let [qid (new-question! uid 1)]
          (testing "cold-start: a never-reviewed question is drawn as new"
            (is (= [qid] (db/draw-fsrs-due-queue uid 20 9999))))
          (testing "first Good → Learning, stays due today, one log row, counted new"
            (let [r (db/apply-fsrs-review! uid qid 3 "correct" sched false)]
              (is (= 1 (:state r)))            ; Learning
              (is (true? (:due-today? r)))     ; +10min ⇒ still today
              (is (= 1 (:reps r)))
              (is (= 1 (:new-today (db/fsrs-daily-counts uid))))))
          (testing "second Good (same day) → graduates to Review, due in the future"
            (let [r (db/apply-fsrs-review! uid qid 3 "correct" sched false)]
              (is (= 2 (:state r)))            ; Review
              (is (false? (:due-today? r)))
              (is (= 2 (:reps r)))))
          (testing "a Review card due in the future is not in today's queue"
            (is (= [] (db/draw-fsrs-due-queue uid 20 9999))))
          (testing "new-per-day cap bounds the draw (fresh user, 0 used today)"
            (let [u2 (new-user!)]
              (try
                (dotimes [i 3] (new-question! u2 (+ 10 i)))
                (is (= 2 (count (db/draw-fsrs-due-queue u2 2 9999))))
                (finally
                  (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" u2]))))))
        (finally
          (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" uid]))))))
