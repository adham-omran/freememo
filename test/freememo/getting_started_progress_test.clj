(ns freememo.getting-started-progress-test
  "DB-backed checks for the getting-started queries
   (plans/getting-started-checklist.md 6.3). The properties under test are the
   SQL: which rows count as a done step, and which topic a \"Show me\" opens.

   SKIPs (not fails) when no database is reachable, like
   `anki-settings-snapshot-test`. Every test creates its own user."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- one [sql & params]
  (jdbc/execute-one! db/ds (into [sql] params) {:builder-fn rs/as-unqualified-maps}))

(defn- new-user! []
  (let [tag (str "getting-started-" (System/nanoTime))]
    (:id (one "INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
           tag (str tag "@example.test")))))

(defn- new-topic! [attrs]
  (:topics/id (db/create-topic! attrs)))

(defn- new-card! [topic-id root-id]
  (:id (one "INSERT INTO flashcards (topic_id, root_topic_id, kind, question, answer)
             VALUES (?, ?, 'basic', ?, 'a') RETURNING id"
         topic-id root-id (str "q-" (System/nanoTime)))))

(defn- stage-for-deletion! [uid topic-id]
  (let [undo-id (:id (one "INSERT INTO undo_log (user_id, action_type, entity_type, entity_refs, snapshot)
                           VALUES (?, 'delete-document', 'document', '[]'::jsonb, '{}'::jsonb)
                           RETURNING id" uid))]
    (one "UPDATE topics SET staged_delete_id = ? WHERE id = ?" undo-id topic-id)))

(deftest ^:db a-new-account-has-done-nothing
  (if-not (db-up?)
    (println "SKIP a-new-account-has-done-nothing — no database")
    (do (db/setup-schema)
        (let [uid (new-user!)]
          (is (= {:imported? false :read? false :extracted? false
                  :carded? false :reviewed? false :anki-pushed? false}
                 (db/get-getting-started-progress uid)))
          (is (= {:text-topic-id nil :extract-id nil :pdf-id nil :carded-root-id nil}
                 (db/get-getting-started-targets uid)))))))

(deftest ^:db each-step-reads-its-own-row
  (if-not (db-up?)
    (println "SKIP each-step-reads-its-own-row — no database")
    (do (db/setup-schema)
        (let [uid (new-user!)
              root (new-topic! {:user-id uid :kind "epub" :title "Book"})
              chapter (new-topic! {:user-id uid :kind "basic" :title "Chapter"
                                   :parent-id root :content "<p>Chapter text</p>"})]
          (testing "an import makes a root, and an EPUB chapter is not an extract"
            (let [p (db/get-getting-started-progress uid)]
              (is (:imported? p))
              (is (not (:extracted? p)))))
          (testing "Extract marks its topic, and Show me opens it"
            (let [extract (new-topic! {:user-id uid :kind "basic" :title "Extract"
                                       :parent-id chapter :content "<p>Kept</p>"
                                       :extract? true})
                  targets (db/get-getting-started-targets uid)]
              (is (:extracted? (db/get-getting-started-progress uid)))
              (is (= extract (:extract-id targets)))
              (is (= extract (:text-topic-id targets)))))
          (testing "a Learn Next counts as read"
            (one "INSERT INTO topic_repetitions
                    (topic_id, user_id, event_type, status_before,
                     interval_days_before, a_factor_before)
                  VALUES (?, ?, 'advance', 'active', 1, 2) RETURNING id" chapter uid)
            (is (:read? (db/get-getting-started-progress uid))))
          (testing "a card, its Quiz review and its Anki push each count"
            (let [card (new-card! chapter root)]
              (is (:carded? (db/get-getting-started-progress uid)))
              (is (= root (:carded-root-id (db/get-getting-started-targets uid))))
              (one "INSERT INTO kg_reviews
                      (user_id, flashcard_id, ord, rating, state_before, state_after,
                       reps_before, grade_source)
                    VALUES (?, ?, 0, 3, 0, 1, 0, 'self') RETURNING id" uid card)
              (is (:reviewed? (db/get-getting-started-progress uid)))
              (is (not (:anki-pushed? (db/get-getting-started-progress uid))))
              (one "UPDATE flashcards SET anki_note_id = 42 WHERE id = ? RETURNING id" card)
              (is (:anki-pushed? (db/get-getting-started-progress uid)))))))))

(deftest ^:db a-staged-document-counts-for-nothing
  (if-not (db-up?)
    (println "SKIP a-staged-document-counts-for-nothing — no database")
    (do (db/setup-schema)
        (let [uid (new-user!)
              root (new-topic! {:user-id uid :kind "pdf" :title "Paper"})]
          (new-card! root root)
          (is (= root (:pdf-id (db/get-getting-started-targets uid))))
          (stage-for-deletion! uid root)
          (let [p (db/get-getting-started-progress uid)
                targets (db/get-getting-started-targets uid)]
            (is (not (:imported? p)))
            (is (not (:carded? p)))
            (is (nil? (:pdf-id targets)))
            (is (nil? (:carded-root-id targets))))))))
