(ns freememo.onboarding-progress
  "Reviewable card progress for the Home checklist."
  (:require [freememo.db :as db]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn reviewable-cards?
  "Complete the card step only when an owned basic or cloze card exists."
  [user-id]
  (boolean
    (:present
      (jdbc/execute-one! db/ds
        ["SELECT EXISTS (
           SELECT 1 FROM flashcards f JOIN topics r ON r.id = f.root_topic_id
            WHERE r.user_id = ? AND r.staged_delete_id IS NULL
              AND f.kind IN ('basic', 'cloze')) AS present"
         user-id]
        {:builder-fn rs/as-unqualified-maps}))))
