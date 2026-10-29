(ns freememo.option-queries
  "Bounded, owned option queries for user-defined domains."
  (:require [clojure.string :as str]
            [freememo.db :as db]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def suggestion-limit 50)
(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(defn- conditions [domain kind]
  (str "t.user_id = ? AND t.parent_id IS NULL AND t.staged_delete_id IS NULL"
       (case domain
         :schedule " AND t.kind = 'pdf' AND EXISTS (SELECT 1 FROM topics p WHERE p.parent_id = t.id AND p.kind = 'page' AND p.staged_delete_id IS NULL)"
         :curriculum " AND t.kind IN ('pdf','markdown','web','wikipedia')"
         :playlist " AND t.kind = 'video-playlist'"
         "")
       (when kind " AND t.kind = ?")))

(defn- project [domain row]
  (when row
    (let [title (or (:title row) "Untitled")]
      {:id (:id row) :kind (:kind row)
       :label title
       :description (when (= domain :schedule) (str (:pages row) " pages"))})))

(defn search
  "Return at most 50 suggestions. Match literal text, including SQL wildcard characters."
  [user-id domain kind request]
  (let [q (-> (or (:query request) "") str str/trim str/lower-case)
        params (cond-> [user-id] kind (conj kind))
        rows (jdbc/execute! db/ds
                            (into [(str "SELECT t.id, t.title, t.kind"
                                        (when (= domain :schedule)
                                          ", (SELECT count(*) FROM topics p WHERE p.parent_id = t.id AND p.kind = 'page' AND p.staged_delete_id IS NULL) AS pages")
                                        " FROM topics t WHERE " (conditions domain kind)
                                        " AND strpos(lower(COALESCE(t.title,'')), ?) > 0"
                                        " ORDER BY lower(t.title), t.id LIMIT ?")]
                                  (conj params q suggestion-limit)) opts)]
    {:request request :options (mapv #(project domain %) rows)}))

(defn selected
  "Resolve one owned selection independently of the current suggestion page or kind filter."
  [user-id domain id]
  (when id
    (project domain
             (jdbc/execute-one! db/ds
                                [(str "SELECT t.id, t.title, t.kind FROM topics t WHERE "
                                      (conditions domain nil) " AND t.id = ?") user-id id] opts))))

(defn any-playlist? [user-id]
  (boolean (jdbc/execute-one! db/ds
                              ["SELECT id FROM topics WHERE user_id = ? AND parent_id IS NULL AND kind = 'video-playlist' AND staged_delete_id IS NULL LIMIT 1" user-id] opts)))

(defn model-options [user-id request]
  {:request request
   :options (into [{:id "" :label "All models"}]
                  (map (fn [row] {:id (:model row) :label (:model row)}))
                  (jdbc/execute! db/ds
                                 ["SELECT DISTINCT model FROM credit_transactions WHERE user_id = ? AND model IS NOT NULL AND model <> '' AND strpos(lower(model), lower(?)) > 0 ORDER BY model LIMIT ?"
                                  user-id (or (:query request) "") suggestion-limit] opts))})

(defn selected-model [user-id id]
  (if (str/blank? id)
    {:id "" :label "All models"}
    (when (jdbc/execute-one! db/ds
                             ["SELECT id FROM credit_transactions WHERE user_id = ? AND model = ? LIMIT 1" user-id id] opts)
      {:id id :label id})))
