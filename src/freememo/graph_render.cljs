(ns freememo.graph-render
  "Imperative sigma.js renderer for the Graph tab. CLJS-only. freememo.graph-page
   owns the Electric lifecycle and calls init!/destroy!/apply-filters!/
   focus-node! here.

   Positions are server-computed (sfdp) and FIXED — no client layout runs.
   Visibility filters and hover/click highlight are pure sigma reducers reading
   one mutable ui-atom; mutate it, then (.refresh renderer) to repaint.

   sigma + graphology load as CDN UMD globals (window.Sigma / window.graphology,
   added to index.*.html), matching Konva/Quill — the prod Docker build has no
   npm step, so npm :require is not an option here. JS objects are ^js-hinted so
   :advanced does not rename their (extern-less) methods.

   ctx = {:renderer <Sigma> :graph <graphology.Graph> :ui <atom>}."
  (:require [clojure.string :as str]))

(defn- sigma-ctor [] (.-Sigma js/window))

(defn- graphology-ctor
  "The graphology Graph constructor from the UMD global, tolerating either
   shape: window.graphology being the class itself or exposing .Graph."
  []
  (when-let [^js g (.-graphology js/window)]
    (or (.-Graph g) g)))

(defn- merge-js
  "Clone JS object `o` and set each key of CLJS map `m` on the clone.
   Reducers MUST return a fresh object — sigma keeps the input immutable."
  [o m]
  (let [c (js/Object.assign #js {} o)]
    (doseq [[k v] m] (aset c (name k) v))
    c))

(defn- build-graphology
  "payload {:nodes [[id label degree x y]…] :edges [[s t pred-idx topic]…]} →
   an undirected multigraph with fixed positions and degree-scaled sizes.
   Edge key = the edge's index in payload, so predicate/source filters address
   edges by key. Endpoint-guarded so a malformed edge cannot throw."
  [Graph payload]
  (let [^js g (new Graph #js {:type "undirected" :multi true})]
    (doseq [[id label degree x y] (:nodes payload)]
      (.addNode g (str id)
        #js {:label (str label)
             :x x :y y
             :size (+ 3 (* 1.4 (js/Math.sqrt (max 1 (or degree 1)))))}))
    (doseq [[i [s t pidx topic]] (map-indexed vector (:edges payload))]
      (let [sk (str s) tk (str t)]
        (when (and (.hasNode g sk) (.hasNode g tk))
          (.addUndirectedEdgeWithKey g (str i) sk tk
            #js {:pred pidx :topic topic}))))
    g))

(defn- recompute-visibility!
  "From the ui-atom's filter state, compute the visible edge-key set and the
   visible node-key set (a node is visible iff its VISIBLE degree ≥ min-degree,
   min 1 — so a node whose every edge is filtered out disappears). Stored back
   on the atom for O(1) reducer lookups."
  [^js g !ui]
  (let [{:keys [source pred-set min-degree]} @!ui
        vedges (js/Set.)
        deg #js {}]
    (.forEachEdge g
      (fn [ekey ^js attrs s t _sa _ta _u]
        (let [pidx (.-pred attrs) topic (.-topic attrs)]
          (when (and (or (nil? pred-set) (contains? pred-set pidx))
                     (or (= source :all) (= source topic)))
            (.add vedges ekey)
            (aset deg s (inc (or (aget deg s) 0)))
            (aset deg t (inc (or (aget deg t) 0)))))))
    (let [vnodes (js/Set.)
          mind (max 1 (or min-degree 1))]
      (.forEachNode g (fn [n _] (when (>= (or (aget deg n) 0) mind) (.add vnodes n))))
      (swap! !ui assoc :vedges vedges :vnodes vnodes))))

(defn- node-reducer [^js g !ui]
  (fn [node data]
    (let [{:keys [hover selected vnodes]} @!ui
          focus (or selected hover)]
      (cond
        (and vnodes (not (.has vnodes node)))
        (merge-js data {:hidden true})
        (and focus (not= node focus) (not (.areNeighbors g focus node)))
        (merge-js data {:color "#c8ccd4" :label "" :zIndex 0})
        (and focus (= node focus))
        (merge-js data {:highlighted true :zIndex 2})
        :else data))))

(defn- edge-reducer [^js g !ui]
  (fn [edge data]
    (let [{:keys [hover selected vedges]} @!ui
          focus (or selected hover)]
      (cond
        (and vedges (not (.has vedges edge)))
        (merge-js data {:hidden true})
        (and focus (not (.hasExtremity g edge focus)))
        (merge-js data {:hidden true})
        :else data))))

(defn init!
  "Create a sigma renderer over `container` for `payload`. Wires clickNode →
   (on-select entity-id-int), clickStage → (on-select nil), and hover
   highlight. Returns ctx, or nil when a lib/container is missing or empty."
  [container payload on-select]
  (let [Sigma (sigma-ctor)
        Graph (graphology-ctor)]
    (when (and container Sigma Graph (seq (:nodes payload)))
      (let [^js g (build-graphology Graph payload)
            !ui (atom {:hover nil :selected nil
                       :source :all :pred-set nil :min-degree 1
                       :vnodes nil :vedges nil})
            ^js renderer (new Sigma g container
                           #js {:renderEdgeLabels false
                                :labelDensity 0.6
                                :labelRenderedSizeThreshold 12
                                :defaultNodeColor "#5b8def"
                                :defaultEdgeColor "#dbe0e8"
                                :nodeReducer (node-reducer g !ui)
                                :edgeReducer (edge-reducer g !ui)})]
        (recompute-visibility! g !ui)
        (.refresh renderer)
        (.on renderer "clickNode"
          (fn [^js e]
            (let [n (.-node e)]
              (swap! !ui assoc :selected n)
              (.refresh renderer)
              (when on-select (on-select (js/parseInt n 10))))))
        (.on renderer "clickStage"
          (fn [_]
            (swap! !ui assoc :selected nil)
            (.refresh renderer)
            (when on-select (on-select nil))))
        (.on renderer "enterNode"
          (fn [^js e] (swap! !ui assoc :hover (.-node e)) (.refresh renderer)))
        (.on renderer "leaveNode"
          (fn [_] (swap! !ui assoc :hover nil) (.refresh renderer)))
        {:renderer renderer :graph g :ui !ui}))))

(defn apply-filters!
  "Set the filter state (source topic-id or :all; pred-set = allowed pred idx
   set or nil for all; min-degree int), recompute visibility, repaint. Node
   positions never move."
  [ctx source pred-set min-degree]
  (when ctx
    (let [^js renderer (:renderer ctx)]
      (swap! (:ui ctx) assoc :source source :pred-set pred-set :min-degree min-degree)
      (recompute-visibility! (:graph ctx) (:ui ctx))
      (.refresh renderer))))

(defn find-node-id-by-label
  "First node id whose label contains `q` (case-insensitive), or nil."
  [payload q]
  (let [ql (some-> q str str/lower-case str/trim)]
    (when (seq ql)
      (some (fn [[id label]]
              (when (str/includes? (str/lower-case (str label)) ql) id))
        (:nodes payload)))))

(defn focus-node!
  "Select + highlight the node for `entity-id` and animate the camera to it."
  [ctx entity-id]
  (when (and ctx entity-id)
    (let [^js renderer (:renderer ctx)
          ^js graph (:graph ctx)
          ui (:ui ctx)
          n (str entity-id)]
      (when (.hasNode graph n)
        (swap! ui assoc :selected n)
        (.refresh renderer)
        (when-let [^js disp (.getNodeDisplayData renderer n)]
          (let [^js cam (.getCamera renderer)]
            (.animate cam
              #js {:x (.-x disp) :y (.-y disp) :ratio 0.35}
              #js {:duration 500})))))))

(defn destroy! [ctx]
  (when-let [^js r (:renderer ctx)] (.kill r)))
