(ns freememo.kg-graph
  "Graph-tab data layer: assemble the edges-only concept graph, lay it out with
   Graphviz sfdp, cache the positioned render payload per (user, scope), and
   serve it to freememo.graph-page.

   Node = a kg_entity in ≥1 approved entity→entity fact. Edge = such a fact,
   labeled by predicate. Literal-object facts are attributes, surfaced only in
   the side panel (db/kg-entity-card) — never graph edges.

   Layout: sfdp uses Prism scaling without iterative overlap removal. Graphviz
   receives each node's rendered size and reserves space for large hubs. It also
   packs components and distributes leaf fans. Positions are raw Graphviz points.
   The client camera fits them, so the server does no normalization."
  (:require [freememo.db :as db]
            [clojure.string :as str]
            [taoensso.telemere :as tel]))

(def ^:private sfdp-bin "sfdp")
(def ^:private layout-revision 2)

;; prism0 runs only Prism's scaling phase. It does not run the costly iterative
;; overlap pass that took about 90 seconds on a 5,000-node graph. The pack margin
;; keeps disconnected components distinct. sep adds space around collision boxes.
(def ^:private sfdp-args
  ["-Goverlap=prism0"
   "-Gsep=+24"
   "-Gpack=24"
   "-Gpackmode=node"
   "-Gbeautify=true"
   "-Gstart=42"
   "-Tplain"])

(def ^:private graphviz-dpi 72.0)

(defn- degree-by-node [edge-pairs]
  (reduce (fn [m [s t]]
            (-> m
              (update s (fnil inc 0))
              (update t (fnil inc 0))))
    {} edge-pairs))

(defn- rendered-node-radius
  "Return the Sigma node radius in pixels for a degree. Keep this formula equal
   to graph-render/build-graphology."
  [degree]
  (+ 3.0 (* 1.4 (Math/sqrt (double (max 1 (or degree 1)))))))

(defn- graphviz-node-width
  "Return a circle diameter in Graphviz inches for the rendered Sigma radius."
  [degree]
  (/ (* 2.0 (rendered-node-radius degree)) graphviz-dpi))

(defn ->dot
  "DOT for an undirected graph over integer node-ids and [s t] edge pairs.
   Every node is emitted explicitly so a node whose edges are later all
   filtered client-side still exists in the model. Node collision boxes match
   the degree-scaled circles that Sigma renders.
   Pre: ids are integers (DOT-safe as bare numerals). Post: a `graph{…}` string."
  [node-ids edge-pairs]
  (let [degree (degree-by-node edge-pairs)
        sb (StringBuilder. "graph G {\nnode [shape=circle,fixedsize=true,label=\"\",margin=0];\n")]
    (doseq [id node-ids]
      (let [width (Double/toString (graphviz-node-width (get degree id 0)))]
        (.append sb id)
        (.append sb "[width=")
        (.append sb width)
        (.append sb ",height=")
        (.append sb width)
        (.append sb "];\n")))
    (doseq [[s t] edge-pairs]
      (.append sb s) (.append sb "--") (.append sb t) (.append sb ";\n"))
    (.append sb "}\n")
    (.toString sb)))

(defn- parse-sfdp-positions [text]
  (into {}
    (keep (fn [line]
            (when (str/starts-with? line "node ")
              ;; A plain node row starts with: node name x y width height.
              (let [parts (str/split line #"\s+" 5)]
                [(parse-long (nth parts 1))
                 [(parse-double (nth parts 2)) (parse-double (nth parts 3))]]))))
    (str/split-lines text)))

(defn- current-cache? [cached version]
  (boolean
    (and cached
         (= (long (:version cached)) (long version))
         (= layout-revision (get-in cached [:payload :layout-revision])))))

(defn- run-sfdp
  "Pipe `dot` through sfdp with bounded Prism scaling. Parse node positions.
   Pre: the `sfdp` binary is on PATH (provisioned in the runtime image).
   Post: {node-id [x y]} for every `node` line. Throws if sfdp is absent or
   exits non-zero. This indicates a deployment image fault. Logs identify each
   start, finish, and failure."
  [^String dot]
  (let [t0 (System/currentTimeMillis)
        payload-bytes (.getBytes dot "UTF-8")]
    (tel/log! {:level :info :id ::sfdp.start :data {:dot-bytes (alength payload-bytes)}}
      "kg-graph sfdp: starting layout")
    (let [proc (.start (ProcessBuilder. (into [sfdp-bin] sfdp-args)))
          ;; Drain stdout on another thread while we write stdin, so neither pipe
          ;; buffer can fill and deadlock the subprocess.
          out (future (slurp (.getInputStream proc)))
          ;; Capture stderr. It contains the cause when sfdp closes stdin during
          ;; startup. Bind it outside the try so the catch can read it.
          err (future (slurp (.getErrorStream proc)))]
      (try
        (with-open [w (.getOutputStream proc)]
          (.write w payload-bytes))
        (let [text @out
              code (.waitFor proc)]
          (when-not (zero? code)
            (throw (ex-info (str "sfdp exited " code ": " (str/trim @err)) {:code code})))
          (let [pos (parse-sfdp-positions text)]
            (tel/log! {:level :info :id ::sfdp.done
                       :data {:exit code :positions (count pos)
                              :out-bytes (count text)
                              :ms (- (System/currentTimeMillis) t0)}}
              "kg-graph sfdp: layout complete")
            pos))
        (catch Throwable t
          (tel/log! {:level :error :id ::sfdp.error
                     :data {:ms (- (System/currentTimeMillis) t0)
                            :error (.getMessage t)
                            :class (.getName (class t))
                            :stderr (try (str/trim @err) (catch Throwable _ nil))}}
            "kg-graph sfdp: failed. See :stderr for the cause")
          (throw t))))))

(defn compute-payload
  "Assemble + lay out the edges-only graph for `user-id`.
   Post: wire-ready
     {:nodes      [[id label degree x y] …]
      :edges      [[s t pred-idx topic-id] …]
      :predicates [[pred-id label] …]     ; index = pred-idx above
      :docs       [[topic-id title] …]
      :layout-revision <integer>}
   Isolated nodes cannot occur (edges-only). Empty graph → empty vectors."
  [user-id]
  (let [t0 (System/currentTimeMillis)
        {:keys [nodes edges predicates docs]} (db/kg-graph-elements user-id)
        _ (tel/log! {:level :info :id ::compute.elements
                     :data {:user-id user-id :nodes (count nodes) :edges (count edges)
                            :predicates (count predicates) :docs (count docs)
                            :ms (- (System/currentTimeMillis) t0)}}
            "kg-graph compute: elements queried")
        node-ids (mapv :id nodes)
        pos (if (seq node-ids)
              (run-sfdp (->dot node-ids (mapv (juxt :s :t) edges)))
              {})
        degree (degree-by-node (map (juxt :s :t) edges))
        pred-idx (into {} (map-indexed (fn [i {:keys [id]}] [id i]) predicates))
        payload {:nodes (mapv (fn [{:keys [id label]}]
                                (let [[x y] (get pos id [0.0 0.0])]
                                  [id label (get degree id 0) x y]))
                          nodes)
                 :edges (mapv (fn [{:keys [s t p topic]}] [s t (get pred-idx p) topic]) edges)
                 :predicates (mapv (fn [{:keys [id label]}] [id label]) predicates)
                 :docs (mapv (fn [{:keys [id title]}] [id title]) docs)
                 :layout-revision layout-revision}]
    (tel/log! {:level :info :id ::compute.done
               :data {:user-id user-id :nodes (count (:nodes payload))
                      :edges (count (:edges payload)) :positioned (count pos)
                      :total-ms (- (System/currentTimeMillis) t0)}}
      "kg-graph compute: payload assembled")
    payload))

(defn get-or-compute
  "Serve the cached payload when its data version and layout revision match.
   Otherwise, recompute the payload and replace the cache.
   `version` is that counter, threaded from the client's reactive watch, so any
   KG mutation invalidates the cache on the next open. A layout revision also
   invalidates positions after a layout algorithm change.
   Pre: called inside e/server (JDBC + subprocess). Post: a render payload map;
   a cold or stale cache triggers exactly one sfdp run. Inv: on return the
   cached row's version = `version`."
  [user-id scope version]
  (let [cached (db/read-graph-layout user-id scope)]
    (if (current-cache? cached version)
      (do (tel/log! {:level :info :id ::cache.hit
                     :data {:user-id user-id :scope scope :version version
                            :nodes (count (:nodes (:payload cached)))}}
            "kg-graph: layout cache hit")
        (:payload cached))
      (do (tel/log! {:level :info :id ::cache.miss
                     :data {:user-id user-id :scope scope :requested version
                            :cached-version (:version cached)}}
            "kg-graph: layout cache miss — recomputing")
        (let [payload (compute-payload user-id)]
          (db/write-graph-layout! user-id scope version payload)
          (tel/log! {:level :info :id ::cache.written
                     :data {:user-id user-id :scope scope :version version
                            :nodes (count (:nodes payload)) :edges (count (:edges payload))}}
            "kg-graph: layout computed and cached")
          payload)))))
