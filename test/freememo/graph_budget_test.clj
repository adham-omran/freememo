(ns freememo.graph-budget-test
  "Bound graph reads before layout and retain access beyond the first page."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is use-fixtures]]
            [freememo.db :as db]
            [freememo.graph-queries :as queries]
            [freememo.kg-graph :as graph]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- one [sql & params]
  (jdbc/execute-one! db/ds (into [sql] params) {:builder-fn rs/as-unqualified-lower-maps}))

(use-fixtures :each
  (fn [run]
    (jdbc/with-transaction [tx db/ds {:rollback-only true}]
      (with-redefs [db/ds tx] (run)))))

(defn- seed-data! []
  (let [user-id (:id (one "INSERT INTO users(username) VALUES (?) RETURNING id" (str "audit-graph-" (random-uuid))))
        topic (:id (one "INSERT INTO topics(user_id,title) VALUES (?, 'Graph source') RETURNING id" user-id))
        predicate (:id (one "INSERT INTO kg_predicates(user_id,slug,label,status) VALUES (?, 'connects', 'connects', 'approved') RETURNING id" user-id))
        hub (:id (one "INSERT INTO kg_entities(user_id,label) VALUES (?, 'Hub') RETURNING id" user-id))]
    (one "INSERT INTO kg_entities(user_id,label) SELECT ?, 'Node ' || n FROM generate_series(1,1001) n" user-id)
    (one "INSERT INTO kg_facts(user_id,subject_entity_id,predicate_id,object_entity_id,graph_topic_id,status) SELECT ?, ?, ?, id, ?, 'approved' FROM kg_entities WHERE user_id = ? AND id <> ? ORDER BY id" user-id hub predicate topic user-id hub)
    {:user-id user-id :hub hub}))

(defn- seed! []
  (one "SET LOCAL session_replication_role = replica")
  (try (seed-data!)
       (finally (one "SET LOCAL session_replication_role = origin"))))

(deftest graph-pages-bound-edges-and-nodes
  (let [{:keys [user-id]} (seed!)
        first-page (queries/elements user-id {:page 0})
        last-page (queries/elements user-id {:page 2})]
    (is (= 500 (count (:edges first-page))))
    (is (= 501 (count (:nodes first-page))))
    (is (= 1001 (get-in first-page [:budget :total-edges])))
    (is (= 1 (count (:edges last-page))))
    (is (= 2 (count (:nodes last-page))))
    (is (empty? (set/intersection (set (map :t (:edges first-page))) (set (map :t (:edges last-page))))))))

(deftest a-concept-outside-the-first-page-keeps-its-connection
  (let [{:keys [user-id hub]} (seed!)
        rare (:id (one "SELECT max(id) AS id FROM kg_entities WHERE user_id = ?" user-id))
        elements (queries/elements user-id {:entity-id rare :page 0})]
    (is (= #{hub rare} (set (map :id (:nodes elements)))))
    (is (= 1 (count (:edges elements))))
    (is (= 1 (get-in elements [:budget :total-edges])))))

(deftest a-foreign-concept-does-not-expose-its-graph
  (let [{:keys [hub]} (seed!)
        other (:id (one "INSERT INTO users(username) VALUES (?) RETURNING id" (str "audit-other-" (random-uuid))))]
    (is (empty? (:edges (queries/elements other {:entity-id hub}))))
    (is (empty? (:nodes (queries/elements other {:entity-id hub}))))))

(deftest a-selected-isolated-concept-remains-visible
  (let [{:keys [user-id]} (seed!)
        isolated (:id (one "INSERT INTO kg_entities(user_id,label) VALUES (?, 'An isolated concept') RETURNING id" user-id))
        elements (queries/elements user-id {:entity-id isolated})]
    (is (= [isolated] (mapv :id (:nodes elements))))
    (is (empty? (:edges elements)))
    (is (= 0 (get-in elements [:budget :total-edges])))))

(deftest cache-selection-and-layout-input-remain-bounded
  (let [{:keys [user-id hub]} (seed!)
        cached (atom nil)
        writes (atom [])
        layout-counts (atom [])
        positions (ns-resolve 'freememo.kg-graph 'run-sfdp)]
    (with-redefs-fn
      {#'db/read-graph-layout (fn [_ scope] (is (= "bounded" scope)) @cached)
       #'db/write-graph-layout! (fn [_ scope version payload]
                                  (swap! writes conj scope)
                                  (reset! cached {:version version :payload payload}))
       positions (fn [dot]
                   (swap! layout-counts conj (count (re-seq #"--" dot)))
                   {})}
      (fn []
        (let [first-page (graph/get-bounded user-id 1 {:page 0})
              cached-page (graph/get-bounded user-id 1 {:page 0})
              next-page (graph/get-bounded user-id 1 {:page 1})]
          (is (= first-page cached-page))
          (is (= [500 500] @layout-counts))
          (is (= ["bounded" "bounded"] @writes))
          (is (not= (:selection first-page) (:selection next-page)))
          (is (<= (count (:nodes next-page)) queries/node-limit)))))))

(deftest continuous-relations-reach-every-fact-with-bounded-results
  (let [{:keys [user-id]} (seed!)
        total (queries/relation-count user-id {})
        windows (mapv #(queries/relation-window user-id {} % 100) (range 0 total 100))
        rows (vec (mapcat identity windows))]
    (is (= 1001 total (count rows)))
    (is (every? #(<= (count %) 100) windows))
    (is (= (count rows) (count (set (map :id rows)))))
    (is (apply < (map :id rows)))
    (is (= [] (queries/relation-window user-id {} total 100)))
    (is (= 100 (count (queries/relation-window user-id {} 0 10000))))))

(deftest continuous-relations-apply-source-and-predicate-scope
  (let [{:keys [user-id]} (seed!)
        row (first (queries/relation-window user-id {} 0 1))
        fact (one "SELECT predicate_id,graph_topic_id FROM kg_facts WHERE id = ?" (:id row))
        predicate (:predicate_id fact)
        source (:graph_topic_id fact)]
    (is (= 1001 (queries/relation-count user-id {:source source})))
    (is (= 0 (queries/relation-count user-id {:source -1})))
    (is (= 1001 (queries/relation-count user-id {:predicates {:mode :include :ids #{predicate}}})))
    (is (= 0 (queries/relation-count user-id {:predicates {:mode :include :ids #{}}})))
    (is (= [] (queries/relation-window user-id {:predicates {:mode :exclude :ids #{predicate}}} 0 20)))
    (is (= 1001 (queries/relation-count user-id {:predicates {:mode :exclude :ids #{}}})))))

(deftest relation-identity-preserves-parallel-facts
  (let [{:keys [user-id]} (seed!)
        row (first (queries/relation-window user-id {} 0 1))
        other-topic (:id (one "INSERT INTO topics(user_id,title) VALUES (?, 'Parallel source') RETURNING id" user-id))
        duplicate (one "INSERT INTO kg_facts(user_id,subject_entity_id,predicate_id,object_entity_id,graph_topic_id,status) SELECT user_id,subject_entity_id,predicate_id,object_entity_id,?,status FROM kg_facts WHERE id = ? RETURNING id" other-topic (:id row))
        payload (with-redefs-fn {(ns-resolve 'freememo.kg-graph 'run-sfdp) (constantly {})}
                  #(graph/elements->payload user-id
                     (queries/elements user-id {:entity-id (:object_id row)})))]
    (is (= 2 (count (:edges payload))))
    (is (= #{(:id row) (:id duplicate)} (set (map #(nth % 4) (:edges payload)))))))

(deftest predicate-catalog-keeps-excluded-choices-reachable
(let [{:keys [user-id]} (seed!)
      option (first (queries/predicate-window user-id :all "" 0 10))]
    (is (= 1 (queries/predicate-count user-id :all "")))
    (is (= "connects" (:label option)))
    (is (= 1001 (:count option)))
    (is (= [] (queries/predicate-window user-id :all "absent predicate" 0 10)))
    (is (= 0 (queries/predicate-count user-id -1 "")))
    (is (= 0 (queries/relation-count user-id {:predicates {:mode :exclude :ids #{(:id option)}}})))
    (is (= [option] (queries/predicate-window user-id :all "" 0 10)))))
