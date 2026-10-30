(ns freememo.sync-conflict-window-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.db :as db]
            [freememo.sync-client :as sync]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def opts {:builder-fn rs/as-unqualified-maps})

(deftest conflict-ranges-bound-reads-and-survive-resolution
  (if-not (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false))
    (is true "PostgreSQL is unavailable. Integration assertions skipped.")
    (let [tag (str "conflict-window-" (random-uuid))
          before (sync/open-conflict-count)
          ids (atom [])]
      (try
        (dotimes [index 45]
          (swap! ids conj
            (:id (jdbc/execute-one! db/ds
                   ["INSERT INTO sync_conflicts (table_name, row_key, row_data, reason)
                       VALUES ('flashcards', jsonb_build_object('probe', ?::text),
                               jsonb_build_object('question', ?::text), 'parent-deleted') RETURNING id"
                    tag (str "<p><b>Question " index "</b></p>")] opts))))
        (let [first-window (sync/open-conflict-window 0 20)
              second-window (sync/open-conflict-window 20 20)]
          (is (= (+ before 45) (sync/open-conflict-count)))
          (is (= (vec (reverse (subvec @ids 25 45))) (mapv :id first-window)))
          (is (= (vec (reverse (subvec @ids 5 25))) (mapv :id second-window)))
          (is (= "Card: Question 44" (:label (first first-window))))
          (is (= [] (sync/open-conflict-window 0 0)))
          (sync/discard-conflict! (last @ids))
          (is (= (+ before 44) (sync/open-conflict-count)))
          (is (= (nth @ids 43) (:id (first (sync/open-conflict-window 0 20))))))
        (finally
          (jdbc/execute! db/ds ["DELETE FROM sync_conflicts WHERE row_key->>'probe' = ?" tag]))))))
