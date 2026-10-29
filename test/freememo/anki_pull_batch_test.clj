(ns freememo.anki-pull-batch-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [freememo.db :as db]
            [freememo.anki-digest :as digest]
            [freememo.anki-pull-race-test :as fixture]))

(deftest a-batch-updates-content-and-baselines-in-one-statement
  (fixture/with-card
    (fn [uid cid nid]
      (let [root (:flashcards/root_topic_id (jdbc/execute-one! db/ds ["SELECT root_topic_id FROM flashcards WHERE id=?" cid]))
            ids (mapv :flashcards/id
                  (jdbc/execute! db/ds
                    ["INSERT INTO flashcards(topic_id,root_topic_id,kind,question,answer,anki_note_id)
                      SELECT ?,?,'basic','<p>q ' || n || '</p>','<p>old</p>',900000000+n
                      FROM generate_series(1,499) n RETURNING id" root root]))
            all (vec (cons cid ids))
            cards (db/get-cards-for-reconcile uid all)
            entries (mapv (fn [card]
                            {:card-id (:flashcards/id card)
                             :expected {:note-id (:flashcards/anki_note_id card)
                                        :local-digest (digest/local-digest card)}
                             :update-map {:answer "<p>batch answer</p>"}
                             :anki-digest (digest/anki-digest (fixture/fields "<p>batch answer</p>") "basic")}) cards)
            statements (atom []) execute! jdbc/execute-one!
            result (with-redefs [jdbc/execute-one!
                                (fn [connection statement & options]
                                  (when (str/starts-with? (first statement) "UPDATE flashcards")
                                    (swap! statements conj (first statement)))
                                  (apply execute! connection statement options))]
                     (db/apply-reconcile-pulls! uid entries))
            after (db/get-cards-for-reconcile uid all)]
        (is (= 500 (:count result)))
        (is (= 1 (count @statements)))
        (is (every? #(= "<p>batch answer</p>" (:flashcards/answer %)) after))
        (is (every? #(= (digest/local-digest %) (:flashcards/local_baseline_digest %)
                       (:flashcards/local_current_digest (jdbc/execute-one! db/ds ["SELECT local_current_digest FROM flashcards WHERE id=?" (:flashcards/id %)]))) after))
        (is (thrown? clojure.lang.ExceptionInfo (db/apply-reconcile-pulls! uid (conj entries (first entries)))))))))

(deftest a-concurrent-edit-rejects-only-that-row
  (fixture/with-card
    (fn [uid cid nid]
      (let [before (first (db/get-cards-for-reconcile uid [cid]))
            entry {:card-id cid :expected {:note-id nid :local-digest (digest/local-digest before)}
                   :update-map {:answer "<p>overwrite</p>"} :anki-digest (apply str (repeat 64 "a"))}]
        (db/update-flashcard! uid cid {:answer "<p>new local edit</p>"})
        (let [result (db/apply-reconcile-pulls! uid [entry])]
          (is (= {:count 0 :conflict-card-ids [cid]} result))
          (is (= "<p>new local edit</p>" (:flashcards/answer (first (db/get-cards-for-reconcile uid [cid]))))))))))
