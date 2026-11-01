(ns freememo.sql-id-boundaries-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.string :as str]
            [freememo.db :as db]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def datasource db/ds)

(def fake-ids (vec (range -100000 0)))
(defn many [& ids] (into (vec ids) fake-ids))
(defn q [sql & args]
  (jdbc/execute-one! db/ds (into [sql] args) {:builder-fn rs/as-unqualified-maps}))
(defn rows [sql & args]
  (jdbc/execute! db/ds (into [sql] args) {:builder-fn rs/as-unqualified-maps}))

(use-fixtures :each
  (fn [run]
    (jdbc/with-transaction [tx db/ds {:rollback-only true}]
      (with-redefs [db/ds tx] (run)))))

(defn fixture []
  (let [user! #(-> (q "INSERT INTO users(username) VALUES (?) RETURNING id"
                    (str "audit-boundary-" (random-uuid))) :id)
        u (user!) v (user!)
        topic! #(-> (q "INSERT INTO topics(user_id,title) VALUES (?,'Boundary') RETURNING id" %) :id)
        t (topic! u) ft (topic! v)
        card! #(-> (q "INSERT INTO flashcards(topic_id,root_topic_id,kind,question,answer,anki_note_id)
                        VALUES (?,?,'basic','Q','A',?) RETURNING id" % %2 %3) :id)
        c (card! t t 10001) fc (card! ft ft 20001)]
    {:u u :v v :t t :ft ft :c c :fc fc}))

(deftest large-address-and-title-lookups
  (let [{:keys [u t]} (fixture)]
    (is (= [u] (mapv :users/id (db/get-mail-addressees (many u)))))
    (is (= {t "Boundary"} (db/get-topic-titles (many t))))
    (doseq [empty-value [nil []]]
      (is (nil? (db/get-mail-addressees empty-value)))
      (is (= {} (db/get-topic-titles empty-value))))))

(deftest large-digest-and-snapshot-scopes
  (let [{:keys [u t ft c fc]} (fixture)]
    (is (= 1 (db/restamp-card-digests! db/ds (many c))))
    (is (= 1 (db/refresh-anki-settings-snapshot! u
                  (mapv #(hash-map :card-id % :header-text "Header") (many c fc)))))
    (is (= 1 (db/set-anki-bibliography-snapshot-for-topics! u (many t ft) "Citation")))
    (is (= "Header" (:anki_header_text (q "SELECT anki_header_text FROM flashcards WHERE id=?" c))))
    (is (= "Citation" (:anki_bibliography_html (q "SELECT anki_bibliography_html FROM flashcards WHERE id=?" c))))
    (is (nil? (:anki_header_text (q "SELECT anki_header_text FROM flashcards WHERE id=?" fc))))
    (is (nil? (:anki_bibliography_html (q "SELECT anki_bibliography_html FROM flashcards WHERE id=?" fc))))
    (doseq [empty-value [nil []]]
      (is (= 0 (db/restamp-card-digests! db/ds empty-value)))
      (is (= 0 (db/refresh-anki-settings-snapshot! u empty-value)))
      (is (= 0 (db/set-anki-bibliography-snapshot-for-topics! u empty-value "None"))))))

(defn groups! [{:keys [u t c] :as f}]
  (let [media (:id (q "INSERT INTO media(user_id,kind,bytes,mime_type,sha256,byte_size)
                       VALUES (?,'image',?,'image/png',?,1) RETURNING id"
                    u (byte-array [1]) (apply str (repeat 64 "a"))))
        og (:id (q "INSERT INTO occlusion_groups(anki_key,image_media_id,mode,geometry)
                    VALUES (?::uuid,?,'hide-all','{}') RETURNING id" (str (random-uuid)) media))
        sg (:id (q "INSERT INTO score_groups(anki_key,start_ms,end_ms,clip_media_id,geometry)
                    VALUES (?::uuid,0,1,?,'{}') RETURNING id" (str (random-uuid)) media))
        c2 (:id (q "INSERT INTO flashcards(topic_id,root_topic_id,kind,question,answer)
                     VALUES (?,?,'basic','Q2','A2') RETURNING id" t t))]
    (q "UPDATE flashcards SET occlusion_group_id=?,mask_ordinal=1 WHERE id=?" og c)
    (q "UPDATE flashcards SET occlusion_group_id=?,mask_ordinal=2,score_group_id=? WHERE id=?" og sg c2)
    (assoc f :og og :sg sg :c2 c2)))

(deftest large-group-reads-and-complete-delete-membership
  (let [{:keys [u c fc c2 og sg]} (groups! (fixture))]
    (is (= [og] (vec (keys (db/get-occlusion-groups-by-ids (many og))))))
    (is (= [sg] (vec (keys (db/get-score-groups-by-ids (many sg))))))
    (is (empty? (:occlusion-groups (db/fully-deleted-group-rows u (many c fc)))))
    (let [result (db/fully-deleted-group-rows u (many c c2 fc))]
      (is (= [og] (mapv :occlusion_groups/id (:occlusion-groups result))))
      (is (= [sg] (mapv :score_groups/id (:score-groups result)))))
    (doseq [empty-value [nil []]]
      (is (= {} (db/get-occlusion-groups-by-ids empty-value)))
      (is (= {} (db/get-score-groups-by-ids empty-value)))
      (is (= {:occlusion-groups [] :score-groups []} (db/fully-deleted-group-rows u empty-value))))))

(deftest large-manual-delete-is-one-owned-delete-with-durable-tombstone
  (let [{:keys [u v c fc]} (fixture)
        execute jdbc/execute! deletes (atom 0)]
    (with-redefs [jdbc/execute! (fn [conn statement & options]
                                    (when (str/starts-with? (first statement) "DELETE FROM flashcards")
                                      (swap! deletes inc))
                                    (apply execute conn statement options))]
      (is (= [c] (mapv :flashcards/id (db/delete-user-flashcards! u (many c fc))))))
    (is (= 1 @deletes))
    (is (= fc (:id (q "SELECT id FROM flashcards WHERE id=?" fc))))
    (is (= [10001] (mapv :anki_note_id (rows "SELECT anki_note_id FROM anki_note_tombstones WHERE user_id=?" u))))
    (db/record-anki-tombstones! db/ds v [{:anki_note_id 10001}])
    (is (= 1 (db/drop-anki-tombstones! u (many 10001))))
    (is (= 1 (:n (q "SELECT count(*) n FROM anki_note_tombstones WHERE user_id=?" v))))
    (doseq [empty-value [nil []]]
      (is (= [] (db/delete-user-flashcards! u empty-value)))
      (is (= 0 (db/drop-anki-tombstones! u empty-value))))))

(deftest large-tombstone-writes-remain-bounded-and-idempotent
  (let [{:keys [u]} (fixture)
        deleted (mapv #(hash-map :anki_note_id % :anki_baseline_digest "digest") fake-ids)
        execute jdbc/execute! statements (atom [])]
    (with-redefs [jdbc/execute! (fn [conn statement & options]
                                    (when (str/starts-with? (first statement) "INSERT INTO anki_note_tombstones")
                                      (swap! statements conj (dec (count statement))))
                                    (apply execute conn statement options))]
      (is (= 100000 (db/record-anki-tombstones! db/ds u deleted))))
    (is (> (count @statements) 1))
    (is (every? #(<= % 65535) @statements))
    (is (= 100000 (:n (q "SELECT count(*) n FROM anki_note_tombstones WHERE user_id=?" u))))
    (is (= 1 (db/record-anki-tombstones! db/ds u [(first deleted)])))
    (is (= 100000 (:n (q "SELECT count(*) n FROM anki_note_tombstones WHERE user_id=?" u))))
    (is (= 0 (db/record-anki-tombstones! db/ds u nil)))
    (is (= 0 (db/record-anki-tombstones! db/ds u [])))))

(deftest large-recovery-statements-bind-within-the-caller-transaction
  (let [{:keys [c og sg]} (groups! (fixture))
        card (q "SELECT * FROM flashcards WHERE id=?" c)
        group (assoc (q "SELECT * FROM occlusion_groups WHERE id=?" og) :geometry {})
        score (assoc (q "SELECT * FROM score_groups WHERE id=?" sg) :geometry {})
        execute jdbc/execute! parameters (atom []) connections (atom #{})]
    (with-redefs [jdbc/execute!
                  (fn [conn statement & options]
                    (swap! connections conj conn)
                    (swap! parameters conj (dec (count statement)))
                    (apply execute conn (update statement 0 #(str "EXPLAIN " %)) options))]
      (is (some? (db/restore-flashcards-tx! db/ds (repeat 100000 card))))
      (is (some? (#'db/restore-group-rows-tx! db/ds :occlusion_groups (repeat 100000 group))))
      (is (some? (#'db/restore-group-rows-tx! db/ds :score_groups (repeat 100000 score)))))
    (is (= #{db/ds} @connections))
    (is (> (count @parameters) 3))
    (is (every? #(<= % 65535) @parameters))
    (is (some? (db/restore-flashcards-tx! db/ds [card])))
    (is (some? (#'db/restore-group-rows-tx! db/ds :occlusion_groups [group])))
    (is (some? (#'db/restore-group-rows-tx! db/ds :score_groups [score])))
    (is (= 1 (:n (q "SELECT count(*) n FROM flashcards WHERE id=?" c))))
    (is (nil? (db/restore-flashcards-tx! db/ds nil)))
    (is (nil? (#'db/restore-group-rows-tx! db/ds :score_groups [])))))

(defn kg! [{:keys [u v t ft] :as f}]
  (let [entity! #(-> (q "INSERT INTO kg_entities(user_id,label) VALUES (?,'Subject') RETURNING id" %) :id)
        e (entity! u) fe (entity! v)
        predicate! #(-> (q "INSERT INTO kg_predicates(user_id,slug,label,status)
                            VALUES (?,'boundary','Boundary','approved') RETURNING id" %) :id)
        p (predicate! u) fp (predicate! v)
        fact! (fn [uid tid eid pid object]
                (:id (q "INSERT INTO kg_facts(user_id,graph_topic_id,subject_entity_id,predicate_id,object_literal,status)
                          VALUES (?,?,?,?,?,'approved') RETURNING id" uid tid eid pid object)))
        a (fact! u t e p "One") b (fact! u t e p "Two") foreign (fact! v ft fe fp "One")
        question! (fn [uid fact]
                    (let [id (:id (q "INSERT INTO kg_questions(user_id,kind,question,reference_answer,status)
                                       VALUES (?,'atomic','Question','Answer','approved') RETURNING id" uid))]
                      (q "INSERT INTO kg_question_facts(question_id,fact_id) VALUES (?,?)" id fact)
                      id))
        question (question! u a) fq (question! v foreign)]
    (assoc f :e e :fe fe :a a :b b :foreign-fact foreign :question question :fq fq)))

(deftest large-knowledge-reads-preserve-user-and-membership
  (let [{:keys [u t ft e fe a b foreign-fact question]} (kg! (fixture))]
    (is (= #{a} (set (keys (db/get-fact-mastery u (many a foreign-fact))))))
    (is (= #{e} (set (keys (db/kg-entity-discriminators u (many e fe) 2)))))
    (is (= [a] (mapv :id (db/kg-facts-labeled u (many a foreign-fact)))))
    (is (= [b] (mapv :id (db/kg-fact-alternates u (many a foreign-fact)))))
    (is (= [question] (db/draw-kg-questions u (many t ft) 10)))
    (is (= [[a b]] (mapv (juxt :keep_id :dup_id)
                         (db/kg-duplicate-candidate-pairs u t (many b) 100 ["approved"]))))
    (is (empty? (db/kg-duplicate-candidate-pairs u t [] 100 ["approved"])))
    (is (= 1 (count (db/kg-duplicate-candidate-pairs u t nil 100 ["approved"]))))
    (doseq [empty-value [nil []]]
      (is (= {} (db/get-fact-mastery u empty-value)))
      (is (= {} (db/kg-entity-discriminators u empty-value 2)))
      (is (= [] (db/kg-facts-labeled u empty-value)))
      (is (nil? (db/kg-fact-alternates u empty-value)))
      (is (= [] (db/draw-kg-questions u empty-value 10))))))

(deftest large-knowledge-withdraw-and-restore-preserve-ownership
  (let [{:keys [u a b foreign-fact question]} (kg! (fixture))
        result (db/reject-kg-facts! u (many a foreign-fact))]
    (is (= "rejected" (:status (q "SELECT status FROM kg_facts WHERE id=?" a))))
    (is (= "approved" (:status (q "SELECT status FROM kg_facts WHERE id=?" foreign-fact))))
    (is (= "retired" (:status (q "SELECT status FROM kg_questions WHERE id=?" question))))
    (db/restore-withdrawn-facts! [{:fact-id a :prev-status "approved" :retired-question-ids (many question)}])
    (is (= "approved" (:status (q "SELECT status FROM kg_questions WHERE id=?" question))))
    (db/mark-kg-facts-duplicate! u (many b foreign-fact))
    (is (= "duplicate" (:status (q "SELECT status FROM kg_facts WHERE id=?" b))))
    (is (= "approved" (:status (q "SELECT status FROM kg_facts WHERE id=?" foreign-fact))))
    (q "UPDATE kg_questions SET status='retired' WHERE id=?" question)
    (db/restore-rejected-fact! [{:fact-id a :prev-status "approved" :retired-question-ids (many question)}])
    (is (= "approved" (:status (q "SELECT status FROM kg_questions WHERE id=?" question))))
    (doseq [empty-value [nil []]]
      (is (= {:rejected 0 :undo-id nil} (db/reject-kg-facts! u empty-value)))
      (is (= {:marked 0 :undo-id nil} (db/mark-kg-facts-duplicate! u empty-value))))))

(deftest large-session-arrays-use-one-parameter-per-array
  (let [{:keys [u t question]} (kg! (fixture))
        id (db/create-kg-session! u "quiz" (many t) (many question) nil)
        row (q "SELECT cardinality(scope_topic_ids) scopes,cardinality(question_ids) questions FROM kg_sessions WHERE id=?" id)]
    (is (= 100001 (:scopes row)))
    (is (= 100001 (:questions row)))
    (is (integer? (db/create-kg-session! u "quiz" [] [question] nil)))))

(deftest large-schedule-state-read-preserves-user-and-fingerprint
  (let [{:keys [u v]} (fixture)]
    (doseq [[uid cid fingerprint] [[u 1 "active"] [u 2 "stale"] [v 3 "active"]]]
      (q "INSERT INTO anki_card_states(user_id,anki_card_id,anki_note_id,config_fingerprint)
          VALUES (?,?,10001,?)" uid cid fingerprint))
    (is (= #{1} (set (keys (db/get-anki-card-states u (many 10001) "active")))))
    (is (= {} (db/get-anki-card-states u nil "active")))
    (is (= {} (db/get-anki-card-states u [] "active")))))

(deftest derived-large-subtree-lists-preserve-owner-and-delete-once
  (let [{:keys [u v t c fc]} (fixture)
        subtree @#'db/subtree-topic-ids]
    (with-redefs-fn {#'db/subtree-topic-ids
                      (fn [tx root] (into (subtree tx root) fake-ids))}
      (fn []
        (is (= {:count 100001} (db/dismiss-topic! u t)))
        (is (db/topic-dismissed? t))
        (is (nil? (db/undismiss-topic! v t)))
        (is (= {:count 100001} (db/undismiss-topic! u t)))
        (is (false? (db/topic-dismissed? t)))
        (let [result (db/stage-topic-for-deletion! u t)]
          (is (= 1 (:card-count result)))
          (is (= [10001] (:note-ids result)))
          (is (some? (:staged_delete_id (q "SELECT staged_delete_id FROM topics WHERE id=?" t))))
          (is (nil? (q "SELECT id FROM flashcards WHERE id=?" c)))
          (is (= fc (:id (q "SELECT id FROM flashcards WHERE id=?" fc)))))))))

(deftest derived-large-question-retirement-list
  (let [{:keys [u a question]} (kg! (fixture))
        execute jdbc/execute!]
    (with-redefs [jdbc/execute!
                  (fn [conn statement & options]
                    (let [result (apply execute conn statement options)]
                      (if (str/starts-with? (first statement) "SELECT id FROM kg_questions")
                        (into result (map #(hash-map :id %) fake-ids))
                        result)))]
      (is (integer? (db/reject-kg-fact! u a))))
    (is (= "retired" (:status (q "SELECT status FROM kg_questions WHERE id=?" question))))))

(deftest derived-large-session-highlight-entity-list
  (let [{:keys [u question e]} (kg! (fixture))
        execute jdbc/execute!]
    (with-redefs [jdbc/execute!
                  (fn [conn statement & options]
                    (let [result (apply execute conn statement options)]
                      (if (str/starts-with? (first statement) "SELECT f.id, s.label AS subject_label")
                        (into result (map #(hash-map :subject_entity_id %) fake-ids))
                        result)))]
      (let [result (db/get-kg-question-for-session u question)]
        (is (= [e] (mapv :id (:entities result))))
        (is (= ["Subject"] (:keywords result)))))))

(deftest later-recovery-batch-failure-rolls-back-the-whole-operation
  (let [one #(jdbc/execute-one! datasource % {:builder-fn rs/as-unqualified-maps})
        user (:id (one ["INSERT INTO users(username) VALUES (?) RETURNING id"
                        (str "audit-atomic-" (random-uuid))]))
        topic (:id (one ["INSERT INTO topics(user_id,title) VALUES (?,'Atomic') RETURNING id" user]))
        card! (fn [note] (:id (one ["INSERT INTO flashcards(topic_id,root_topic_id,kind,question,answer,anki_note_id)
                                    VALUES (?,?,'basic',?,'A',?) RETURNING id" topic topic (str "Q" note) note])))
        a (card! 40001) b (card! 40002)
        snapshots (jdbc/execute! datasource ["SELECT * FROM flashcards WHERE id=ANY(?)" (long-array [a b])]
                    {:builder-fn rs/as-unqualified-maps})
        execute jdbc/execute!]
    (try
      (one ["DELETE FROM flashcards WHERE id=ANY(?)" (long-array [a b])])
      (let [inserts (atom 0)]
        (with-redefs-fn
          {#'db/ds datasource
           #'db/max-statement-params 1
           #'jdbc/execute!
           (fn [conn statement & options]
             (when (and (str/starts-with? (first statement) "INSERT INTO flashcards")
                        (= 2 (swap! inserts inc)))
               (throw (ex-info "Controlled recovery failure." {})))
             (apply execute conn statement options))}
          #(is (thrown-with-msg? clojure.lang.ExceptionInfo #"Controlled recovery"
                 (db/restore-flashcards! snapshots))))
        (is (= 2 @inserts))
        (is (= 0 (:n (one ["SELECT count(*) n FROM flashcards WHERE id=ANY(?)" (long-array [a b])])))))
      (with-redefs [db/ds datasource]
        (db/restore-flashcards! snapshots))
      (one ["UPDATE flashcards SET anki_note_id=id WHERE id=ANY(?)" (long-array [a b])])
      (let [inserts (atom 0)]
        (with-redefs-fn
          {#'db/ds datasource
           #'db/max-statement-params 1
           #'jdbc/execute!
           (fn [conn statement & options]
             (when (and (str/starts-with? (first statement) "INSERT INTO anki_note_tombstones")
                        (= 2 (swap! inserts inc)))
               (throw (ex-info "Controlled tombstone failure." {})))
             (apply execute conn statement options))}
          #(is (thrown-with-msg? clojure.lang.ExceptionInfo #"Controlled tombstone"
                 (db/delete-user-flashcards! user (many a b)))))
        (is (= 2 @inserts))
        (is (= 2 (:n (one ["SELECT count(*) n FROM flashcards WHERE id=ANY(?)" (long-array [a b])]))))
        (is (= 0 (:n (one ["SELECT count(*) n FROM anki_note_tombstones WHERE user_id=?" user])))))
      (finally (one ["DELETE FROM users WHERE id=?" user])))))
