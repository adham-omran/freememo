(ns freememo.graph-state)

(defn- edge-in-scope?
  [{:keys [pred topic]} source pred-set]
  (and (or (nil? pred-set) (contains? pred-set pred))
       (or (= source :all) (= source topic))))

(defn- degrees
  [edges node-ids]
  (into {}
    (map (fn [[node-id neighbors]] [node-id (count neighbors)]))
    (reduce (fn [m {:keys [source target]}]
              (if (and (contains? node-ids source)
                       (contains? node-ids target))
                (-> m
                  (update source (fnil conj #{}) target)
                  (update target (fnil conj #{}) source))
                m))
      {} edges)))

(defn- k-core-node-ids
  [edges min-degree]
  (let [candidate-ids (into #{} (mapcat (juxt :source :target)) edges)
        k (max 1 (or min-degree 1))]
    (loop [node-ids candidate-ids]
      (let [degree-by-id (degrees edges node-ids)
            next-ids (into #{}
                       (filter #(>= (get degree-by-id % 0) k))
                       node-ids)]
        (if (= node-ids next-ids)
          node-ids
          (recur next-ids))))))

(defn- induced-edges
  [edges node-ids]
  (filterv (fn [{:keys [source target]}]
             (and (contains? node-ids source)
                  (contains? node-ids target)))
    edges))

(defn- neighbor-ids
  [edges node-ids]
  (reduce (fn [result {:keys [source target]}]
            (cond
              (contains? node-ids source) (conj result target)
              (contains? node-ids target) (conj result source)
              :else result))
    #{} edges))

(defn- neighborhood-node-ids
  [edges selected depth]
  (loop [seen #{selected}
         frontier #{selected}
         remaining (max 1 (min 2 (or depth 1)))]
    (if (zero? remaining)
      seen
      (let [next-frontier (apply disj (neighbor-ids edges frontier) seen)]
        (recur (into seen next-frontier) next-frontier (dec remaining))))))

(defn visible-subgraph
  "Return the visible node and edge IDs after scope, k-core, and view filters."
  [edges {:keys [source pred-set min-degree view-mode selected depth]}]
  (let [scoped-edges (filterv #(edge-in-scope? % source pred-set) edges)
        core-node-ids (k-core-node-ids scoped-edges min-degree)
        core-edges (induced-edges scoped-edges core-node-ids)
        visible-node-ids
        (if (= view-mode :neighborhood)
          (if (contains? core-node-ids selected)
            (neighborhood-node-ids core-edges selected depth)
            #{})
          core-node-ids)
        visible-edges (induced-edges core-edges visible-node-ids)]
    {:node-ids visible-node-ids
     :edge-ids (into #{} (map :id) visible-edges)}))
