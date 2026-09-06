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

(def ^:private full-turn (* 2.0 #?(:clj Math/PI :cljs js/Math.PI)))
(def ^:private start-angle (- (/ #?(:clj Math/PI :cljs js/Math.PI) 2.0)))
(def ^:private inner-radius 0.27)
(def ^:private outer-radius 0.46)

(defn- cos [angle]
  (#?(:clj Math/cos :cljs js/Math.cos) angle))

(defn- sin [angle]
  (#?(:clj Math/sin :cljs js/Math.sin) angle))

(defn- point-on-ring
  [radius angle]
  {:x (+ 0.5 (* radius (cos angle)))
   :y (+ 0.5 (* radius (sin angle)))})

(defn- ring-positions
  [node-ids radius offset]
  (let [ids (vec node-ids)
        n (count ids)]
    (into {}
      (map-indexed
        (fn [index node-id]
          [node-id (point-on-ring radius
                     (+ start-angle
                        (* full-turn (/ (mod (+ index offset) n) n))))])
        ids))))

(defn- adjacency
  [edges]
  (reduce
    (fn [result {:keys [source target]}]
      (if (= source target)
        result
        (-> result
          (update source (fnil conj #{}) target)
          (update target (fnil conj #{}) source))))
    {} edges))

(defn- assign-second-hop-parents
  [neighbors first-hop second-hop]
  (reduce
    (fn [{:keys [counts parents] :as result} node-id]
      (let [candidates (sort-by (juxt #(get counts % 0) str)
                         (filter first-hop (get neighbors node-id)))
            parent (first candidates)]
        (if parent
          (-> result
            (assoc-in [:parents node-id] parent)
            (update-in [:counts parent] (fnil inc 0)))
          result)))
    {:counts {} :parents {}}
    (sort-by str second-hop)))

(defn- circular-distance
  [a b]
  (let [distance (#?(:clj Math/abs :cljs js/Math.abs) (- a b))]
    (min distance (- full-turn distance))))

(defn- best-ring-offset
  [desired-angles]
  (let [n (count desired-angles)]
    (if (zero? n)
      0
      (->> (range n)
        (map (fn [offset]
               [(reduce +
                  (map-indexed
                    (fn [index desired-angle]
                      (let [slot-angle (+ start-angle
                                         (* full-turn
                                           (/ (mod (+ index offset) n) n)))
                            distance (circular-distance desired-angle slot-angle)]
                        (* distance distance)))
                    desired-angles))
                offset]))
        (sort-by (juxt first second))
        first
        second))))

(defn neighborhood-layout
  "Place a selected node and its two-hop neighborhood on normalized rings."
  [edges selected]
  (let [neighbors (adjacency edges)
        first-hop (disj (get neighbors selected #{}) selected)
        second-hop (apply disj
                     (disj (neighbor-ids edges first-hop) selected)
                     first-hop)
        first-order (vec (sort-by str first-hop))
        first-index (zipmap first-order (range))
        first-angles (into {}
                       (map-indexed
                         (fn [index node-id]
                           [node-id (+ start-angle
                                      (* full-turn (/ index (count first-order))))])
                         first-order))
        {:keys [parents]} (assign-second-hop-parents neighbors first-hop second-hop)
        second-order (vec (sort-by (juxt #(get first-index (get parents %) 0) str)
                            second-hop))
        desired-angles (mapv #(get first-angles (get parents %) start-angle)
                         second-order)
        second-offset (best-ring-offset desired-angles)
        center {selected {:x 0.5 :y 0.5}}
        positions (merge center
                    (ring-positions first-order inner-radius 0)
                    (ring-positions second-order outer-radius second-offset))
        hop-by-id (merge {selected 0}
                    (zipmap first-order (repeat 1))
                    (zipmap second-order (repeat 2)))]
    {:positions positions
     :hop-by-id hop-by-id
     :node-ids-by-hop {0 #{selected} 1 first-hop 2 second-hop}
     :parent-by-id parents
     :radii {1 inner-radius 2 outer-radius}}))

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
