(ns freememo.graph-filter
  "Keep relation choices stable when bounded graph pages change.")

(defn checked? [selection predicate-id]
  (if (nil? selection)
    true
    (let [present? (contains? (:ids selection) predicate-id)]
      (if (= :exclude (:mode selection)) (not present?) present?))))

(defn set-checked [selection predicate-id checked?]
  (let [{:keys [mode ids]} (or selection {:mode :exclude :ids #{}})
        add? (if (= mode :exclude) (not checked?) checked?)
        ids ((if add? conj disj) ids predicate-id)]
    (when-not (and (= mode :exclude) (empty? ids))
      {:mode mode :ids ids})))

(defn renderer-indexes [payload selection]
  (when selection
    (into #{}
      (keep-indexed (fn [index [id _]] (when (checked? selection id) index)))
      (:predicates payload))))

(defn selected-count [payload selection]
  (if selection (count (renderer-indexes payload selection)) (count (:predicates payload))))

(defn document-source [payload source]
  (if (or (= :all source) (some #(= source (first %)) (:docs payload))) source :all))

(defn connectedness [requested maximum]
  (max 1 (min (long (or requested 1)) (long (or maximum 1)))))

(defn graph-selection [entity-id stored]
  {:entity-id entity-id
   :page (if (= entity-id (:entity-id stored)) (long (or (:page stored) 0)) 0)})
