(ns freememo.kg-predicate-test
  "Predicate persistence and staged extraction against PostgreSQL. No model calls."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [freememo.credits :as credits]
            [freememo.db :as db]
            [freememo.kg-extract :as kg]
            [freememo.kg-llm :as llm]
            [freememo.kg-page-digest :as digest]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})

(defn- q1 [sql & params]
  (jdbc/execute-one! db/ds (into [sql] params) opts))

(defn- query [sql & params]
  (jdbc/execute! db/ds (into [sql] params) opts))

(use-fixtures :once
  (fn [run-tests]
    (db/setup-schema)
    (run-tests)))

(defmacro ^:private with-user [binding & body]
  `(let [tag# (str "kg-predicate-" (random-uuid))
         ~binding (:id (q1 "INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                          tag# (str tag# "@example.test")))]
     (try ~@body
       (finally
         (q1 "DELETE FROM users WHERE id = ?" ~binding)))))

(defn- create-document! [user-id]
  (:id (q1 "INSERT INTO topics (user_id, kind, title, content_text)
            VALUES (?, 'basic', 'Arabic predicates', 'Arabic extraction fixture') RETURNING id"
         user-id)))

(defn- predicate! [user-id label]
  (db/get-or-create-kg-predicate! user-id (kg/slugify label) label))

(defn- stage! [user-id document-id triples]
  (db/record-page-extraction!
    {:user-id user-id
     :graph-topic-id document-id
     :page-topic-id document-id
     :content-digest (digest/digest-of "Arabic extraction fixture")
     :digest-version digest/digest-version
     :prompt-version "predicate-test"
     :extract-model-id "predicate-test"
     :triples triples}))

(defn- deterministic-link! [_resolved user-id labels]
  {:by-label (into {}
               (map (fn [[label id]] [(str/lower-case (str/trim label)) id]))
               (db/get-or-create-kg-entities! user-id labels))
   :cost 0.0
   :tokens []})

(deftest staged-arabic-facts-survive-distillation-and-reuse-predicates
  (with-user user-id
    (let [document-id (create-document! user-id)
          labels ["يسبب" "علاقة 1" "سبب 1" "يُسَبِّبُ"]
          triples (into []
                    (mapcat (fn [label]
                              [{:s "مرض" :p label :o "حمى"}
                               {:s "مرض" :p label :lit "وصف"}]))
                    labels)]
      (with-redefs [llm/resolve-model+gate! (constantly {:api-key "unused"
                                                        :entry {:id "predicate-test"}
                                                        :model-fields {}})
                    llm/chat! (fn [& _] (throw (ex-info "Unexpected model call" {})))
                    kg/link-entities! deterministic-link!
                    credits/record-cost-charge! (fn [& _] nil)]
        (stage! user-id document-id triples)
        (let [result (kg/distill-document! user-id document-id)
              predicates (query "SELECT id, slug, label FROM kg_predicates WHERE user_id = ?" user-id)
              predicate-ids (set (map :id predicates))]
          (testing "resumed extraction keeps every Arabic predicate and both object types"
            (is (:success result) (pr-str result))
            (is (= 8 (:facts result)))
            (is (= (set labels) (set (map :label predicates))))
            (is (= 4 (count predicate-ids)))
            (is (every? #(re-matches #"[a-z0-9]+(?:-[a-z0-9]+)*" (:slug %)) predicates))
            (is (= {:entity_count 4 :literal_count 4}
                   (q1 "SELECT count(object_entity_id) AS entity_count,
                               count(object_literal) AS literal_count
                          FROM kg_facts WHERE user_id = ?" user-id)))
            (is (empty? (db/staged-triples-for-document user-id document-id))))
          (testing "the same staged facts reuse predicate IDs and existing facts"
            (stage! user-id document-id triples)
            (let [repeated (kg/distill-document! user-id document-id)]
              (is (:success repeated) (pr-str repeated))
              (is (zero? (:facts repeated))))
            (is (= predicate-ids
                   (set (map :id (query "SELECT id FROM kg_predicates WHERE user_id = ?" user-id)))))
            (is (= 8 (:n (q1 "SELECT count(*) AS n FROM kg_facts WHERE user_id = ?" user-id)))))
          (testing "completed unchanged pages require no new extraction"
            (let [repeated (kg/distill-document! user-id document-id)]
              (is (:success repeated) (pr-str repeated))
              (is (= 1 (:skipped repeated)))
              (is (zero? (:facts repeated))))))))))

(deftest literal-opaque-slug-labels-do-not-share-arabic-predicate-identity
  (doseq [arabic-first? [true false]]
    (testing (str "Arabic predicate inserted first: " arabic-first?)
      (with-user user-id
        (let [arabic "يسبب"
              opaque (kg/slugify arabic)
              labels (if arabic-first? [arabic opaque] [opaque arabic])
              inserted (into {} (map (fn [label] [label (:id (predicate! user-id label))])) labels)]
          (is (string? opaque))
          (is (not= (get inserted arabic) (get inserted opaque)))
          (doseq [label labels]
            (is (= (get inserted label) (:id (predicate! user-id label)))))
          (is (= #{arabic opaque}
                 (set (map :label
                        (query "SELECT label FROM kg_predicates WHERE user_id = ?" user-id)))))
          (is (= 2 (count (query "SELECT DISTINCT slug FROM kg_predicates WHERE user_id = ?" user-id)))))))))

(deftest existing-ascii-predicates-keep-their-id-slug-and-status
  (with-user user-id
    (let [existing (q1 "INSERT INTO kg_predicates (user_id, slug, label, status)
                       VALUES (?, 'causes', 'Causes', 'rejected') RETURNING id" user-id)
          reused (predicate! user-id "CAUSES")]
      (is (= (:id existing) (:id reused)))
      (is (= "rejected" (:status reused)))
      (is (= {:slug "causes" :label "Causes" :status "rejected"}
             (q1 "SELECT slug, label, status FROM kg_predicates WHERE id = ?" (:id existing)))))))

(deftest legacy-mixed-predicates-reuse-their-original-row
  (with-user user-id
    (let [existing (q1 "INSERT INTO kg_predicates (user_id, slug, label, status)
                       VALUES (?, '1', 'علاقة 1', 'rejected') RETURNING id" user-id)
          reused (predicate! user-id "علاقة 1")
          distinct-predicate (predicate! user-id "سبب 1")]
      (is (= (:id existing) (:id reused)))
      (is (= "rejected" (:status reused)))
      (is (= {:slug "1" :label "علاقة 1" :status "rejected"}
             (q1 "SELECT slug, label, status FROM kg_predicates WHERE id = ?" (:id existing))))
      (is (not= (:id existing) (:id distinct-predicate)))
      (is (= (:id distinct-predicate) (:id (predicate! user-id "سبب 1")))))))

(deftest legacy-unicode-predicates-reuse-canonically-equivalent-labels
  (doseq [[stored incoming legacy-slug]
          [["définit" "de\u0301finit" "d-finit"]
           ["de\u0301finit" "définit" "de-finit"]
           ["cafe\u0301 définit" "café définit" "cafe-d-finit"]]]
    (testing (str "Legacy slug " legacy-slug)
      (with-user user-id
        (let [existing (q1 "INSERT INTO kg_predicates (user_id, slug, label, status)
                           VALUES (?, ?, ?, 'rejected') RETURNING id"
                         user-id legacy-slug stored)
              reused (predicate! user-id incoming)]
          (is (= (:id existing) (:id reused)))
          (is (= "rejected" (:status reused)))
          (is (= {:slug legacy-slug :label stored :status "rejected"}
                 (q1 "SELECT slug, label, status FROM kg_predicates WHERE id = ?" (:id existing))))
          (is (= 1 (:n (q1 "SELECT count(*) AS n FROM kg_predicates WHERE user_id = ?" user-id)))))))))

(deftest legacy-opaque-slug-collisions-keep-both-predicate-identities
  (with-user user-id
    (let [arabic "يسبب"
          opaque (kg/slugify arabic)
          existing (q1 "INSERT INTO kg_predicates (user_id, slug, label, status)
                       VALUES (?, ?, ?, 'approved') RETURNING id"
                     user-id opaque opaque)
          arabic-predicate (predicate! user-id arabic)]
      (is (not= (:id existing) (:id arabic-predicate)))
      (is (= (:id existing) (:id (predicate! user-id opaque))))
      (is (= (:id arabic-predicate) (:id (predicate! user-id arabic))))
      (is (= {:slug opaque :label opaque}
             (q1 "SELECT slug, label FROM kg_predicates WHERE id = ?" (:id existing))))
      (is (= arabic
             (:label (q1 "SELECT label FROM kg_predicates WHERE id = ?" (:id arabic-predicate)))))
      (is (= 2 (count (query "SELECT DISTINCT slug FROM kg_predicates WHERE user_id = ?" user-id)))))))

(deftest duplicate-candidates-use-the-arabic-predicate-label
  (with-user user-id
    (let [document-id (create-document! user-id)
          label "يسبب"
          predicate-id (:id (predicate! user-id label))
          entities (db/get-or-create-kg-entities! user-id ["مرض" "حمى"])
          base {:user_id user-id :graph_topic_id document-id
                :subject_entity_id (get entities "مرض")
                :predicate_id predicate-id :status "approved" :page_number nil}
          inserted (db/insert-kg-facts!
                     [(assoc base :object_entity_id (get entities "حمى") :object_literal nil)
                      (assoc base :object_entity_id nil :object_literal "حمى")])
          pairs (db/kg-duplicate-candidate-pairs user-id document-id nil 100 db/sweep-statuses)]
      (is (= 2 (count inserted)))
      (is (= 1 (count pairs)))
      (is (= "مرض يسبب حمى" (:keep_flat (first pairs))))
      (is (= "مرض يسبب حمى" (:dup_flat (first pairs))))
      (is (not (str/includes? (or (:keep_flat (first pairs)) "") (kg/slugify label)))))))
