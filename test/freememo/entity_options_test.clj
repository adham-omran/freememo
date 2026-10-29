(ns freememo.entity-options-test
  "Database checks for bounded entity search and selected-ID ownership."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.db :as db]
            [freememo.entity-options :as options]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- one [sql & params]
  (jdbc/execute-one! db/ds (into [sql] params)
    {:builder-fn rs/as-unqualified-maps}))

(defn- user! []
  (:id (one "INSERT INTO users(username) VALUES (?) RETURNING id"
         (str "entity-options-" (random-uuid)))))

(deftest rare-exact-matches-remain-reachable-with-bounded-results
  (jdbc/with-transaction [tx db/ds {:rollback-only true}]
    (with-redefs [db/ds tx]
      (let [user-id (user!)
            _seed (jdbc/execute! tx
                    ["INSERT INTO kg_entities(user_id,label)
                      SELECT ?, 'A common entity ' || n FROM generate_series(1,1000) n"
                     user-id])
            rare (:id (one "INSERT INTO kg_entities(user_id,label)
                           VALUES (?, 'Z rare quasar nucleus') RETURNING id" user-id))]
        (is (not-any? #(= rare (:id %)) (db/list-kg-entities user-id nil 1000))
          "The rare entity is outside the former client-side search domain.")
        (is (= [{:id rare :label "Z rare quasar nucleus"}]
              (options/search-options user-id nil "Z rare quasar nucleus"))
          "An exact query reaches the rare entity through the real database search.")
        (is (= options/option-limit (count (options/search-options user-id nil ""))))
        (is (every? #(= #{:id :label} (set (keys %)))
              (options/search-options user-id nil "")))))))

(deftest selected-labels-remain-independent-and-owned
  (jdbc/with-transaction [tx db/ds {:rollback-only true}]
    (with-redefs [db/ds tx]
      (let [user-id (user!)
            other-user (user!)
            entity-id (:id (one "INSERT INTO kg_entities(user_id,label)
                                VALUES (?, 'Selected quasar') RETURNING id" user-id))]
        (is (empty? (options/search-options user-id nil "unrelated-zzzzz")))
        (is (= {:id entity-id :label "Selected quasar"}
              (options/selected-option user-id entity-id)))
        (is (nil? (options/selected-option other-user entity-id)))
        (is (empty? (options/search-options other-user nil "Selected quasar")))
        (is (nil? (options/selected-option user-id nil)))))))

(deftest source-search-includes-only-approved-entity-use
  (jdbc/with-transaction [tx db/ds {:rollback-only true}]
    (with-redefs [db/ds tx]
      (let [user-id (user!)
            source-id (:id (one "INSERT INTO topics(user_id,title) VALUES (?, 'Entity source') RETURNING id" user-id))
            other-source (:id (one "INSERT INTO topics(user_id,title) VALUES (?, 'Other source') RETURNING id" user-id))
            predicate-id (:id (one "INSERT INTO kg_predicates(user_id,slug,label,status)
                                   VALUES (?, 'describes', 'describes', 'approved') RETURNING id" user-id))
            entity-id (:id (one "INSERT INTO kg_entities(user_id,label)
                                VALUES (?, 'Source quasar') RETURNING id" user-id))]
        (one "INSERT INTO kg_facts(user_id,subject_entity_id,predicate_id,object_literal,graph_topic_id,status)
              VALUES (?, ?, ?, 'A supported statement', ?, 'approved')"
          user-id entity-id predicate-id source-id)
        (is (= [{:id entity-id :label "Source quasar"}]
              (options/search-options user-id source-id "Source quasar")))
        (is (empty? (options/search-options user-id other-source "Source quasar")))
        (one "UPDATE kg_facts SET status='rejected' WHERE user_id=?" user-id)
        (is (empty? (options/search-options user-id source-id "Source quasar")))))))
