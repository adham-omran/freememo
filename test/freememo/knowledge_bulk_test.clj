(ns freememo.knowledge-bulk-test
  "The bulk operations of the Knowledge tables against PostgreSQL
   (plans/knowledge-tables-selection-and-bulk.md section 9.1): ownership, one undo
   entry, partial selections and an all-or-nothing merge. Every DB test skips when
   the database is down."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.db :as db]
   [freememo.knowledge-bulk :as bulk]
   [freememo.kg-extract :as kg]
   [freememo.kg-questions :as kgq]
   [freememo.credits :as credits]
   [freememo.settings :as settings]
   [freememo.undo :as undo]
   [freememo.user-state :as us]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})

(defonce ^:private schema-ready (delay (db/setup-schema) true))

(defn- q1 [sql & params]
  (jdbc/execute-one! db/ds (into [sql] params) opts))

(defn- create-user! []
  (let [tag (str "kg-bulk-" (random-uuid))]
    (:id (q1 "INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
           tag (str tag "@example.test")))))

(defn- create-doc! [user-id]
  (:id (q1 "INSERT INTO topics (user_id, kind, title) VALUES (?, 'basic', 'Doc') RETURNING id"
         user-id)))

(defn- create-predicate! [user-id]
  (:id (q1 "INSERT INTO kg_predicates (user_id, slug, label, status)
            VALUES (?, 'relates-to', 'relates to', 'approved') RETURNING id" user-id)))

(defn- create-entity! [user-id label]
  (:id (q1 "INSERT INTO kg_entities (user_id, label) VALUES (?, ?) RETURNING id"
         user-id label)))

(defn- create-fact!
  "A literal fact in `status` about `subject`."
  [user-id doc-id pred-id subject literal status]
  (:id (q1 "INSERT INTO kg_facts (user_id, subject_entity_id, predicate_id, object_literal,
                                  graph_topic_id, status)
            VALUES (?, ?, ?, ?, ?, ?) RETURNING id"
         user-id subject pred-id literal doc-id status)))

(defn- create-entity-fact!
  [user-id doc-id pred-id subject object]
  (:id (q1 "INSERT INTO kg_facts (user_id, subject_entity_id, predicate_id, object_entity_id,
                                  graph_topic_id, status)
            VALUES (?, ?, ?, ?, ?, 'approved') RETURNING id"
         user-id subject pred-id object doc-id)))

(defn- create-question! [user-id status]
  (:id (q1 "INSERT INTO kg_questions (user_id, kind, question, reference_answer, status)
            VALUES (?, 'atomic', ?, 'A', ?) RETURNING id"
         user-id (str "Q " (random-uuid)) status)))

(defn- question [id] (q1 "SELECT * FROM kg_questions WHERE id = ?" id))
(defn- fact-status [id] (:status (q1 "SELECT status FROM kg_facts WHERE id = ?" id)))
(defn- q-status [id] (:status (question id)))
(defn- entity-exists? [id] (some? (q1 "SELECT 1 AS x FROM kg_entities WHERE id = ?" id)))
(defn- undo-rows [user-id]
  (jdbc/execute! db/ds ["SELECT * FROM undo_log WHERE user_id = ? ORDER BY id" user-id] opts))

(defmacro ^:private with-users
  "Bind `syms` to fresh users and delete them afterwards, or skip with no DB."
  [syms & body]
  `(if-not (db-up?)
     (is true "PostgreSQL is unavailable; integration assertion skipped")
     (do @schema-ready
         (let [~@(mapcat (fn [s] [s `(create-user!)]) syms)]
           (try ~@body
             (finally
               (doseq [id# [~@syms]]
                 (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" id#]))))))))

;; ---------------------------------------------------------------------------
;; Questions
;; ---------------------------------------------------------------------------

(deftest reject-questions-writes-one-undo-entry-and-skips-the-rest
  (with-users [owner other]
    (let [approved (create-question! owner "approved")
          retired (create-question! owner "retired")
          already (create-question! owner "rejected")
          foreign (create-question! other "approved")
          r (bulk/reject-questions! owner [approved retired already foreign approved])]
      (testing "counts only owned rows that were not already rejected"
        (is (= {:success true :rejected 2} r)))
      (is (= "rejected" (q-status approved)))
      (is (= "rejected" (q-status retired)))
      (testing "a foreign question is untouched"
        (is (= "approved" (q-status foreign))))
      (testing "exactly one undo entry, naming the two rows"
        (let [rows (undo-rows owner)]
          (is (= 1 (count rows)))
          (is (= "bulk-reject-questions" (:action_type (first rows))))
          (is (= "question" (:entity_type (first rows))))
          (is (= #{approved retired} (set (map long (:entity_refs (db/get-undo-entry (:id (first rows))))))))))
      (testing "undo gives each question its OWN prior status"
        (let [entry-id (:id (first (undo-rows owner)))
              res (undo/undo-entry! owner entry-id)]
          (is (:success res))
          (is (= "approved" (q-status approved)))
          (is (= "retired" (q-status retired)))
          (is (= "rejected" (q-status already)))
          (is (= "approved" (q-status foreign)))))
      (testing "nothing left to reject is a success with no new undo entry"
        (let [before (count (undo-rows owner))]
          (is (= {:success true :rejected 0} (bulk/reject-questions! owner [already])))
          (is (= {:success true :rejected 0} (bulk/reject-questions! owner [])))
          (is (= before (count (undo-rows owner)))))))))

(deftest reject-questions-handles-a-selection-past-the-parameter-bound
  (with-users [owner]
    (let [q (create-question! owner "approved")
          ids (into [q] (range 1000000 1070000))]
      (is (= {:success true :rejected 1} (bulk/reject-questions! owner ids))))))

(deftest question-flags-change-only-rows-not-already-in-the-target-state
  (with-users [owner other]
    (let [a (create-question! owner "approved")
          b (create-question! owner "approved")
          rejected (create-question! owner "rejected")
          foreign (create-question! other "approved")
          _ (jdbc/execute! db/ds ["UPDATE kg_questions SET flagged = true WHERE id = ?" a])
          fsrs-before (select-keys (question b) [:fsrs_stability :fsrs_due :fsrs_state :fsrs_reps])]
      (testing "flag: the already flagged row is not counted"
        (is (= {:success true :changed 1}
               (bulk/set-questions-flags! owner [a b rejected foreign] {:flagged true})))
        (is (true? (:flagged (question a))))
        (is (true? (:flagged (question b))))
        (is (false? (:flagged (question rejected))))
        (is (false? (:flagged (question foreign)))))
      (testing "unflag"
        (is (= {:success true :changed 2}
               (bulk/set-questions-flags! owner [a b] {:flagged false}))))
      (testing "suspend and unsuspend write no FSRS column"
        (is (= {:success true :changed 2}
               (bulk/set-questions-flags! owner [a b] {:suspended true})))
        (is (true? (:suspended (question a))))
        (is (= {:success true :changed 2}
               (bulk/set-questions-flags! owner [a b] {:suspended false})))
        (is (= fsrs-before
               (select-keys (question b) [:fsrs_stability :fsrs_due :fsrs_state :fsrs_reps]))))
      (testing "no undo entry for a flag"
        (is (empty? (undo-rows owner))))
      (testing "a malformed flags map is refused"
        (is (false? (:success (bulk/set-questions-flags! owner [a] {}))))
        (is (false? (:success (bulk/set-questions-flags! owner [a] {:flagged true :suspended true}))))
        (is (false? (:success (bulk/set-questions-flags! owner [a] {:flagged "yes"}))))))))

;; ---------------------------------------------------------------------------
;; Facts
;; ---------------------------------------------------------------------------

(deftest unmark-duplicate-restores-only-duplicates-the-user-owns
  (with-users [owner other]
    (let [doc (create-doc! owner)
          other-doc (create-doc! other)
          pred (create-predicate! owner)
          other-pred (create-predicate! other)
          s (create-entity! owner "S")
          os (create-entity! other "S")
          dup1 (create-fact! owner doc pred s "a" "duplicate")
          dup2 (create-fact! owner doc pred s "b" "duplicate")
          approved (create-fact! owner doc pred s "c" "approved")
          rejected (create-fact! owner doc pred s "d" "rejected")
          foreign (create-fact! other other-doc other-pred os "a" "duplicate")]
      (is (= {:success true :restored 2}
             (bulk/unmark-duplicate-facts! owner [dup1 dup2 approved rejected foreign])))
      (is (= "approved" (fact-status dup1)))
      (is (= "approved" (fact-status dup2)))
      (testing "other statuses and foreign rows are untouched"
        (is (= "rejected" (fact-status rejected)))
        (is (= "duplicate" (fact-status foreign))))
      (testing "a second call changes nothing"
        (is (= {:success true :restored 0}
               (bulk/unmark-duplicate-facts! owner [dup1 dup2]))))
      (testing "no undo entry"
        (is (empty? (undo-rows owner)))))))

;; ---------------------------------------------------------------------------
;; Entities
;; ---------------------------------------------------------------------------

(deftest merge-entities-merges-all-or-none
  (with-users [owner other]
    (let [doc (create-doc! owner)
          pred (create-predicate! owner)
          keep (create-entity! owner "Keep")
          a (create-entity! owner "A")
          b (create-entity! owner "B")
          c (create-entity! owner "C")
          foreign (create-entity! other "Foreign")
          fa (create-fact! owner doc pred a "fa" "approved")
          fb (create-fact! owner doc pred b "fb" "approved")
          fc (create-entity-fact! owner doc pred c b)
          subject-of (fn [id] (:subject_entity_id (q1 "SELECT subject_entity_id FROM kg_facts WHERE id = ?" id)))]
      (testing "a foreign id in the list refuses the whole merge"
        (let [r (bulk/merge-entities! owner keep [a foreign])]
          (is (false? (:success r)))
          (is (entity-exists? a))
          (is (= a (subject-of fa)))))
      (testing "keep among the absorbed refuses the merge"
        (is (false? (:success (bulk/merge-entities! owner keep [a keep]))))
        (is (entity-exists? a)))
      (testing "an empty absorbed list refuses the merge"
        (is (false? (:success (bulk/merge-entities! owner keep [])))))
      (testing "a foreign keep is refused"
        (is (false? (:success (bulk/merge-entities! owner foreign [a])))))
      (testing "a failure on the SECOND merge rolls back the first"
        (let [real @#'db/merge-kg-entities-tx!
              calls (atom 0)]
          (with-redefs [db/merge-kg-entities-tx!
                        (fn [tx & args]
                          (when (= 2 (swap! calls inc)) (throw (ex-info "boom" {})))
                          (apply real tx args))]
            (let [r (bulk/merge-entities! owner keep [a b])]
              (is (false? (:success r)))
              (is (= 2 @calls))))
          (is (entity-exists? a))
          (is (entity-exists? b))
          (is (= a (subject-of fa)))))
      (testing "the merge itself"
        (is (= {:success true :merged 2} (bulk/merge-entities! owner keep [a b b])))
        (is (not (entity-exists? a)))
        (is (not (entity-exists? b)))
        (is (entity-exists? c))
        (is (entity-exists? foreign))
        (is (= keep (subject-of fa)))
        (is (= keep (subject-of fb)))
        (is (= keep (:object_entity_id (q1 "SELECT object_entity_id FROM kg_facts WHERE id = ?" fc))))
        (is (empty? (undo-rows owner)))))))

(deftest entity-choices-lists-selected-entities-by-fact-count
  (with-users [owner other]
    (let [doc (create-doc! owner)
          pred (create-predicate! owner)
          busy (create-entity! owner "Busy")
          quiet (create-entity! owner "Quiet")
          foreign (create-entity! other "Foreign")]
      (create-fact! owner doc pred busy "x" "approved")
      (create-fact! owner doc pred busy "y" "approved")
      (create-fact! owner doc pred busy "z" "duplicate")
      (is (= [{:id busy :label "Busy" :fact_count 2}
              {:id quiet :label "Quiet" :fact_count 0}]
             (bulk/entity-choices owner [quiet busy foreign])))
      (is (= [] (bulk/entity-choices owner []))))))

;; ---------------------------------------------------------------------------
;; Runs
;; ---------------------------------------------------------------------------

(deftest start-distills-skips-foreign-and-running-and-refuses-without-credits
  (with-users [owner other]
    (let [d1 (create-doc! owner)
          d2 (create-doc! owner)
          d3 (create-doc! owner)
          child (:id (q1 "INSERT INTO topics (user_id, parent_id, kind, title)
                          VALUES (?, ?, 'basic', 'c') RETURNING id" owner d1))
          foreign (create-doc! other)
          started (atom [])]
      (swap! (us/get-atom owner :distilling-docs) conj d3)
      (with-redefs [settings/get-openrouter-api-key (constantly "key")
                    credits/check-cost-billed-balance! (constantly {:ok true})
                    kg/start-distill! (fn [uid id] (swap! started conj [uid id]) nil)]
        (testing "only owned root documents start, and a running one is counted"
          (is (= {:success true :started 2 :running 1}
                 (bulk/start-distills! owner [d1 d2 d3 child foreign])))
          (is (= #{[owner d1] [owner d2]} (set @started))))
        (testing "ids that are all foreign are refused"
          (is (false? (:success (bulk/start-distills! owner [foreign child])))))
        (testing "an empty selection starts nothing"
          (is (= {:success true :started 0 :running 0} (bulk/start-distills! owner [])))))
      (testing "an exhausted balance refuses the whole start"
        (reset! started [])
        (with-redefs [settings/get-openrouter-api-key (constantly "key")
                      credits/check-cost-billed-balance!
                      (constantly {:ok false :error "Out of credits."})
                      kg/start-distill! (fn [uid id] (swap! started conj [uid id]) nil)]
          (let [r (bulk/start-distills! owner [d1 d2])]
            (is (false? (:success r)))
            (is (= "Out of credits." (:error r)))
            (is (= :insufficient-credits (:error-type r)))
            (is (empty? @started)))))
      (testing "a missing API key refuses the whole start"
        (with-redefs [settings/get-openrouter-api-key (constantly nil)
                      kg/start-distill! (fn [uid id] (swap! started conj [uid id]) nil)]
          (is (false? (:success (bulk/start-distills! owner [d1]))))
          (is (empty? @started)))))))

(deftest start-question-generations-skips-documents-without-approved-facts
  (with-users [owner other]
    (let [with-facts (create-doc! owner)
          only-proposed (create-doc! owner)
          empty-doc (create-doc! owner)
          foreign (create-doc! other)
          pred (create-predicate! owner)
          s (create-entity! owner "S")
          started (atom [])]
      (create-fact! owner with-facts pred s "a" "approved")
      (create-fact! owner only-proposed pred s "b" "proposed")
      (with-redefs [settings/get-openrouter-api-key (constantly "key")
                    credits/check-cost-billed-balance! (constantly {:ok true})
                    kgq/start-atomic-generation!
                    (fn [uid id] (swap! started conj [uid id]) nil)]
        (is (= {:success true :started 1 :skipped 2 :running 0}
               (bulk/start-question-generations!
                 owner [with-facts only-proposed empty-doc foreign])))
        (is (= [[owner with-facts]] @started))
        (testing "foreign ids alone are refused"
          (is (false? (:success (bulk/start-question-generations! owner [foreign]))))))
      (testing "a refused gate starts nothing"
        (reset! started [])
        (with-redefs [settings/get-openrouter-api-key (constantly "key")
                      credits/check-cost-billed-balance! (constantly {:ok false :error "No."})
                      kgq/start-atomic-generation!
                      (fn [uid id] (swap! started conj [uid id]) nil)]
          (is (false? (:success (bulk/start-question-generations! owner [with-facts]))))
          (is (empty? @started)))))))
