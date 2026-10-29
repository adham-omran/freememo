(ns freememo.anki-pull-race-test
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [freememo.db :as db]
            [freememo.anki-digest :as digest]
            [freememo.anki-reconcile-server :as recon]))

(defn fields [answer]
  {:Front {:value "<p>q</p>" :order 0}
   :Back {:value answer :order 1}})

(defn with-card [f]
  (let [uid (:users/id (jdbc/execute-one! db/ds
                        ["INSERT INTO users(username) VALUES (?) RETURNING id"
                         (str "pull-race-" (random-uuid))]))]
    (try
      (let [root (:topics/id (jdbc/execute-one! db/ds
                              ["INSERT INTO topics(user_id,title) VALUES (?, 'Pull race') RETURNING id" uid]))
            cid (:flashcards/id (jdbc/execute-one! db/ds
                                 ["INSERT INTO flashcards(topic_id,root_topic_id,kind,question,answer,anki_note_id)
                                   VALUES (?,?,'basic','<p>q</p>','<p>old</p>',?) RETURNING id"
                                  root root (+ 800000 uid)]))
            nid (+ 800000 uid)]
        (db/restamp-card-digests! db/ds [cid])
        (db/set-card-baselines! uid [{:card-id cid
                                     :anki-digest (digest/anki-digest (fields "<p>old</p>") "basic")
                                     :local-digest (digest/local-digest (first (db/get-cards-for-reconcile uid [cid])))}])
        (f uid cid nid))
      (finally
        (jdbc/execute-one! db/ds ["DELETE FROM topics WHERE user_id=?" uid])
        (jdbc/execute-one! db/ds ["DELETE FROM users WHERE id=?" uid])))))

(defn planned-outcome [uid nid]
  (let [digests {nid (digest/anki-digest (fields "<p>from Anki</p>") "basic")}]
    (merge (recon/plan uid digests #{})
      {:digests digests :pull-notes {nid {:fields (fields "<p>from Anki</p>")}}})))

(deftest a-local-edit-after-planning-survives-the-pull
  (with-card
    (fn [uid cid nid]
      (let [outcome (planned-outcome uid nid)]
        (is (= [cid] (:pull-card-ids outcome)))
        (db/update-flashcard! uid cid {:answer "<p>new local edit</p>"})
        (let [before (first (db/get-cards-for-reconcile uid [cid]))
              result (recon/finish-run! uid outcome)
              after (jdbc/execute-one! db/ds ["SELECT * FROM flashcards WHERE id=?" cid])]
          (is (:success result))
          (is (= 0 (get-in result [:counts :pulls])))
          (is (= "<p>new local edit</p>" (:flashcards/answer after)))
          (is (= (:flashcards/local_baseline_digest before) (:flashcards/local_baseline_digest after)))
          (is (some? (:flashcards/anki_conflict_at after))))))))

(deftest an-unchanged-card-pulls-and-stamps-the-result
  (with-card
    (fn [uid cid nid]
      (let [result (recon/finish-run! uid (planned-outcome uid nid))
            after (jdbc/execute-one! db/ds ["SELECT * FROM flashcards WHERE id=?" cid])]
        (is (:success result))
        (is (= 1 (get-in result [:counts :pulls])))
        (is (= "<p>from Anki</p>" (:flashcards/answer after)))
        (is (= (:flashcards/local_current_digest after) (:flashcards/local_baseline_digest after)))))))

(deftest an-edit-between-the-server-reread-and-the-write-also-survives
  (with-card
    (fn [uid cid nid]
      (let [outcome (planned-outcome uid nid)
            apply! db/apply-reconcile-pulls!]
        (with-redefs [db/apply-reconcile-pulls!
                      (fn [& args]
                        (db/update-flashcard! uid cid {:answer "<p>edit at the write boundary</p>"})
                        (apply apply! args))]
          (let [result (recon/finish-run! uid outcome)
                row (jdbc/execute-one! db/ds ["SELECT * FROM flashcards WHERE id=?" cid])]
            (is (= 0 (get-in result [:counts :pulls])))
            (is (= "<p>edit at the write boundary</p>" (:flashcards/answer row)))
            (is (some? (:flashcards/anki_conflict_at row)))))))))

(deftest content-and-baselines-roll-back-together
  (with-card
    (fn [uid cid nid]
      (let [outcome (planned-outcome uid nid)
            before (first (db/get-cards-for-reconcile uid [cid]))
            execute! jdbc/execute-one!]
        (with-redefs [jdbc/execute-one!
                      (fn [connectable statement & options]
                        (if (and (string? (first statement))
                                 (.startsWith (first statement) "UPDATE flashcards AS f SET question=v.question"))
                          (throw (ex-info "Simulated atomic write failure" {}))
                          (apply execute! connectable statement options)))]
          (let [result (recon/finish-run! uid outcome)
                after (first (db/get-cards-for-reconcile uid [cid]))]
            (is (false? (:success result)))
            (is (= (:flashcards/answer before) (:flashcards/answer after)))
            (is (= (:flashcards/local_baseline_digest before) (:flashcards/local_baseline_digest after)))))))))

(deftest a-relinked-note-becomes-a-conflict
  (with-card
    (fn [uid cid nid]
      (let [outcome (planned-outcome uid nid)]
        (db/set-anki-note-ids uid [[cid (inc nid)]])
        (let [result (recon/finish-run! uid outcome)
              after (jdbc/execute-one! db/ds ["SELECT * FROM flashcards WHERE id=?" cid])]
          (is (= 0 (get-in result [:counts :pulls])))
          (is (= "<p>old</p>" (:flashcards/answer after)))
          (is (some? (:flashcards/anki_conflict_at after))))))))

(deftest a-planned-pull-cannot-write-another-users-card
  (with-card
    (fn [uid cid nid]
      (let [outcome (planned-outcome uid nid)
            verdict (db/apply-reconcile-pull! -1 cid (get (:pull-versions outcome) cid)
                      {:answer "<p>foreign write</p>"} (get (:digests outcome) nid))
            after (jdbc/execute-one! db/ds ["SELECT * FROM flashcards WHERE id=?" cid])]
        (is (= :missing verdict))
        (is (= "<p>old</p>" (:flashcards/answer after)))))))

(deftest cloze-and-occlusion-pulls-keep-their-kind-specific-conversions
  (doseq [[kind local-fields note-fields column expected]
          [["cloze" {:cloze "{{c1::old}}"}
            {:Text {:value "{{c1::new}}" :order 0}}
            :flashcards/cloze "{{c1::new}}"]
           ["occlusion" {:io_fields {:header "<p>h</p>" :remarks "<p>old</p>" :image "local-image"}}
            {:Header {:value "<p>h</p>" :order 0}
             :Remarks {:value "<p><b>new</b></p>" :order 1}}
            :flashcards/io_fields {:header "<p>h</p>" :remarks "<p><b>new</b></p>" :image "local-image"}]]]
    (with-card
      (fn [uid cid nid]
        (db/update-flashcard! uid cid (assoc local-fields :kind kind))
        (let [local (first (db/get-cards-for-reconcile uid [cid]))]
          (db/set-card-baselines! uid [{:card-id cid :anki-digest (apply str (repeat 64 "a"))
                                       :local-digest (digest/local-digest local)}]))
        (let [digests {nid (digest/anki-digest note-fields kind)}
              outcome (merge (recon/plan uid digests #{})
                        {:digests digests :pull-notes {nid {:fields note-fields}}})
              result (recon/finish-run! uid outcome)
              after (first (db/get-cards-for-reconcile uid [cid]))]
          (is (= 1 (get-in result [:counts :pulls])) kind)
          (is (= expected (get after column)) kind)
          (is (= (digest/local-digest after) (:flashcards/local_baseline_digest after)) kind))))))
