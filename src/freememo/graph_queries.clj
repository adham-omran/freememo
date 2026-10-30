(ns freememo.graph-queries
  "Bound graph transfer and layout before the renderer allocates its graph."
  (:require [freememo.db :as db]
            [freememo.bounded-page :as page]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def edge-limit 500)
(def node-limit (* 2 edge-limit))
(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(defn- relation-scope [user-id {:keys [entity-id source predicates]}]
  {:where (str "f.user_id = ? AND f.status = 'approved' AND f.object_entity_id IS NOT NULL"
               (when entity-id " AND (f.subject_entity_id = ? OR f.object_entity_id = ?)")
               (when (and source (not= :all source)) " AND f.graph_topic_id = ?")
               (when predicates
                 (if (= :exclude (:mode predicates))
                   " AND NOT (f.predicate_id = ANY(?))"
                   " AND f.predicate_id = ANY(?)")))
   :args (cond-> [user-id]
           entity-id (conj entity-id entity-id)
           (and source (not= :all source)) (conj source)
           predicates (conj (long-array (:ids predicates))))})

(defn relation-count [user-id selection]
  (let [{:keys [where args]} (relation-scope user-id selection)]
    (:n (jdbc/execute-one! db/ds
          (into [(str "SELECT count(*)::int AS n FROM kg_facts f WHERE " where)] args) opts))))

(defn relation-window
  "Return only the visible compact relation rows. Bodies stay in the Facts view."
  [user-id selection offset limit]
  (let [{:keys [where args]} (relation-scope user-id selection)]
    (jdbc/execute! db/ds
      (into [(str "SELECT f.id, f.subject_entity_id AS subject_id, f.object_entity_id AS object_id, "
                  "left(s.label,256) AS subject_label, left(o.label,256) AS object_label, "
                  "left(p.label,128) AS predicate_label FROM kg_facts f "
                  "JOIN kg_entities s ON s.id = f.subject_entity_id "
                  "JOIN kg_entities o ON o.id = f.object_entity_id "
                  "JOIN kg_predicates p ON p.id = f.predicate_id WHERE " where
                  " ORDER BY f.id LIMIT ? OFFSET ?")]
        (conj args (min 100 (max 0 (long limit))) (max 0 (long offset)))) opts)))

(defn predicate-count [user-id source query]
  (let [{:keys [where args]} (relation-scope user-id {:source source})]
    (:n (jdbc/execute-one! db/ds
          (into [(str "SELECT count(DISTINCT f.predicate_id)::int AS n FROM kg_facts f "
                      "JOIN kg_predicates p ON p.id = f.predicate_id WHERE " where
                      " AND position(lower(?) in lower(p.label)) > 0")]
            (conj args (or query ""))) opts))))

(defn predicate-window [user-id source query offset limit]
  (let [{:keys [where args]} (relation-scope user-id {:source source})]
    (jdbc/execute! db/ds
      (into [(str "SELECT p.id, left(p.label,256) AS label, count(*)::int AS count "
                  "FROM kg_facts f JOIN kg_predicates p ON p.id = f.predicate_id WHERE " where
                  " AND position(lower(?) in lower(p.label)) > 0 "
                  "GROUP BY p.id,p.label ORDER BY count(*) DESC,lower(p.label),p.id LIMIT ? OFFSET ?")]
        (conj args (or query "") (min 100 (max 0 (long limit))) (max 0 (long offset)))) opts)))

(defn elements [user-id {:keys [entity-id page] :or {page 0} :as selection}]
  (let [{:keys [where args]} (relation-scope user-id selection)
        total (:n (jdbc/execute-one! db/ds
                                     (into [(str "SELECT count(*)::int AS n FROM kg_facts f WHERE " where)] args) opts))
        page (page/page-index page total edge-limit)
        edges (jdbc/execute! db/ds
                             (into [(str "SELECT f.id, f.subject_entity_id AS s, f.object_entity_id AS t, f.predicate_id AS p, f.graph_topic_id AS topic FROM kg_facts f WHERE " where " ORDER BY f.id LIMIT ? OFFSET ?")]
                                   (conj args edge-limit (* page edge-limit))) opts)
        ids (long-array (distinct (cond-> (mapcat (juxt :s :t) edges)
                                   entity-id (concat [entity-id]))))
        predicates (long-array (distinct (map :p edges)))
        docs (long-array (distinct (map :topic edges)))]
    {:nodes (jdbc/execute! db/ds
                           ["SELECT id, left(label,256) AS label FROM kg_entities WHERE user_id = ? AND id = ANY(?) ORDER BY id" user-id ids] opts)
     :edges edges
     :predicates (jdbc/execute! db/ds
                                ["SELECT id, left(label,256) AS label FROM kg_predicates WHERE id = ANY(?) ORDER BY id" predicates] opts)
     :docs (jdbc/execute! db/ds
                          ["SELECT id, left(title,256) AS title FROM topics WHERE user_id = ? AND id = ANY(?) ORDER BY id" user-id docs] opts)
     :budget {:edges edge-limit :nodes node-limit :total-edges total :page page :entity-id entity-id}}))
