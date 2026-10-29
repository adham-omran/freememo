(ns freememo.graph-queries
  "Bound graph transfer and layout before the renderer allocates its graph."
  (:require [freememo.db :as db]
            [freememo.bounded-page :as page]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def edge-limit 500)
(def node-limit (* 2 edge-limit))
(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(defn elements [user-id {:keys [entity-id page] :or {page 0}}]
  (let [where (str "user_id = ? AND status = 'approved' AND object_entity_id IS NOT NULL"
                   (when entity-id " AND (subject_entity_id = ? OR object_entity_id = ?)"))
        args (cond-> [user-id] entity-id (conj entity-id entity-id))
        total (:n (jdbc/execute-one! db/ds
                                     (into [(str "SELECT count(*)::int AS n FROM kg_facts WHERE " where)] args) opts))
        page (page/page-index page total edge-limit)
        edges (jdbc/execute! db/ds
                             (into [(str "SELECT subject_entity_id AS s, object_entity_id AS t, predicate_id AS p, graph_topic_id AS topic FROM kg_facts WHERE " where " ORDER BY id LIMIT ? OFFSET ?")]
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
