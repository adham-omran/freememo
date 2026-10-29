(ns freememo.anki-overlay-lookup-test
  "Exercise overlay lookup beyond the prepared-statement parameter limit."
  (:require [clojure.test :refer [deftest is]]
            [freememo.db :as db]
            [next.jdbc :as jdbc]))

(deftest large-overlay-lookup-preserves-ownership
  (let [tag (str "overlay-limit-" (random-uuid))
        user-id (:users/id (jdbc/execute-one! db/ds
                            ["INSERT INTO users(username) VALUES (?) RETURNING id" tag]))
        other-id (:users/id (jdbc/execute-one! db/ds
                             ["INSERT INTO users(username) VALUES (?) RETURNING id" (str tag "-other")]))
        root-id (:topics/id (jdbc/execute-one! db/ds
                              ["INSERT INTO topics(user_id,title) VALUES (?,?) RETURNING id" user-id tag]))
        other-root (:topics/id (jdbc/execute-one! db/ds
                                 ["INSERT INTO topics(user_id,title) VALUES (?,?) RETURNING id" other-id tag]))]
    (try
      (let [card-id (:flashcards/id
                      (jdbc/execute-one! db/ds
                        ["INSERT INTO flashcards(topic_id,root_topic_id,kind,question,answer,anki_note_id)
                          VALUES (?,?,'basic','owned','answer',1) RETURNING id" root-id root-id]))]
        (jdbc/execute-one! db/ds
          ["INSERT INTO flashcards(topic_id,root_topic_id,kind,question,answer,anki_note_id)
            VALUES (?,?,'basic','foreign','answer',2)" other-root other-root])
        (let [rows (db/get-flashcards-by-anki-note-ids user-id (range 1 65536))]
          (is (= [card-id] (mapv :flashcards/id rows)))
          (is (= ["owned"] (mapv :flashcards/question rows))))
        (is (= [] (db/get-flashcards-by-anki-note-ids user-id []))))
      (finally
        (jdbc/execute! db/ds ["DELETE FROM topics WHERE id = ANY(?)" (long-array [root-id other-root])])
        (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ANY(?)" (long-array [user-id other-id])])))))
