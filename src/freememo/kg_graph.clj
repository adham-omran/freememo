(ns freememo.kg-graph
  "Graph-tab data layer: assemble the edges-only concept graph, lay it out with
   Graphviz sfdp, cache the positioned render payload per (user, scope), and
   serve it to freememo.graph-page.

   Node = a kg_entity in ≥1 approved entity→entity fact. Edge = such a fact,
   labeled by predicate. Literal-object facts are attributes, surfaced only in
   the side panel (db/kg-entity-card) — never graph edges.

   Layout: sfdp with overlap removal OFF (-Goverlap=true). The edges-only graph
   has no isolated nodes, so overlap removal — sfdp's one expensive step — is
   unneeded; a 5k-node graph then lays out in <1s (measured), vs ~90s with the
   default overlap pass. Positions are raw graphviz points; the client camera
   fits them, so the server does no normalization."
  (:require [freememo.db :as db]
            [clojure.string :as str]
            [taoensso.telemere :as tel]))

(def ^:private sfdp-bin "sfdp")

(defn ->dot
  "DOT for an undirected graph over integer node-ids and [s t] edge pairs.
   Every node is emitted explicitly so a node whose edges are later all
   filtered client-side still exists in the model.
   Pre: ids are integers (DOT-safe as bare numerals). Post: a `graph{…}` string."
  [node-ids edge-pairs]
  (let [sb (StringBuilder. "graph G {\n")]
    (doseq [id node-ids] (.append sb id) (.append sb ";\n"))
    (doseq [[s t] edge-pairs]
      (.append sb s) (.append sb "--") (.append sb t) (.append sb ";\n"))
    (.append sb "}\n")
    (.toString sb)))

(defn- run-sfdp
  "Pipe `dot` through `sfdp -Goverlap=true -Tplain`; parse node positions.
   Pre: the `sfdp` binary is on PATH (provisioned in the runtime image).
   Post: {node-id [x y]} for every `node` line. Throws if sfdp is absent or
   exits non-zero — blame there is a deployment (image) bug, not a caller bug.
   Every start/finish/failure is logged so a prod hang or crash is locatable."
  [^String dot]
  (let [t0 (System/currentTimeMillis)
        payload-bytes (.getBytes dot "UTF-8")]
    (tel/log! {:level :info :id ::sfdp.start :data {:dot-bytes (alength payload-bytes)}}
      "kg-graph sfdp: starting layout")
    (let [proc (.start (ProcessBuilder. [sfdp-bin "-Goverlap=true" "-Tplain"]))
          ;; Drain stdout on another thread while we write stdin, so neither pipe
          ;; buffer can fill and deadlock the subprocess.
          out (future (slurp (.getInputStream proc)))
          ;; Capture — never discard — stderr. When sfdp aborts at startup (e.g.
          ;; a missing neato/sfdp layout plugin) it prints the reason here and
          ;; closes stdin, so our write fails with a bare "Broken pipe"; stderr
          ;; is the only place the true cause is visible. Bound outside the try
          ;; so the catch can read it on the broken-pipe path.
          err (future (slurp (.getErrorStream proc)))]
      (try
        (with-open [w (.getOutputStream proc)]
          (.write w payload-bytes))
        (let [text @out
              code (.waitFor proc)]
          (when-not (zero? code)
            (throw (ex-info (str "sfdp exited " code ": " (str/trim @err)) {:code code})))
          (let [pos (into {}
                      (keep (fn [line]
                              (when (str/starts-with? line "node ")
                                ;; `node <name> <x> <y> <w> <h> <label> …` — names
                                ;; are our integer ids (no spaces), plain split safe.
                                (let [parts (str/split line #"\s+" 5)]
                                  [(parse-long (nth parts 1))
                                   [(parse-double (nth parts 2)) (parse-double (nth parts 3))]]))))
                      (str/split-lines text))]
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
            "kg-graph sfdp: FAILED — see :stderr for the real cause (missing layout plugin, etc.)")
          (throw t))))))

(defn compute-payload
  "Assemble + lay out the edges-only graph for `user-id`.
   Post: wire-ready
     {:nodes      [[id label degree x y] …]
      :edges      [[s t pred-idx topic-id] …]
      :predicates [[pred-id label] …]     ; index = pred-idx above
      :docs       [[topic-id title] …]}
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
        degree (reduce (fn [m {:keys [s t]}]
                         (-> m (update s (fnil inc 0)) (update t (fnil inc 0))))
                 {} edges)
        pred-idx (into {} (map-indexed (fn [i {:keys [id]}] [id i]) predicates))
        payload {:nodes (mapv (fn [{:keys [id label]}]
                                (let [[x y] (get pos id [0.0 0.0])]
                                  [id label (get degree id 0) x y]))
                          nodes)
                 :edges (mapv (fn [{:keys [s t p topic]}] [s t (get pred-idx p) topic]) edges)
                 :predicates (mapv (fn [{:keys [id label]}] [id label]) predicates)
                 :docs (mapv (fn [{:keys [id title]}] [id title]) docs)}]
    (tel/log! {:level :info :id ::compute.done
               :data {:user-id user-id :nodes (count (:nodes payload))
                      :edges (count (:edges payload)) :positioned (count pos)
                      :total-ms (- (System/currentTimeMillis) t0)}}
      "kg-graph compute: payload assembled")
    payload))

(defn get-or-compute
  "Serve the cached payload for (user, scope) when its version matches the
   current :kg-mutations counter; else recompute, cache, and return it.
   `version` is that counter, threaded from the client's reactive watch, so any
   KG mutation invalidates on the next open.
   Pre: called inside e/server (JDBC + subprocess). Post: a render payload map;
   a cold or stale cache triggers exactly one sfdp run. Inv: on return the
   cached row's version = `version`."
  [user-id scope version]
  (let [cached (db/read-graph-layout user-id scope)]
    (if (and cached (= (long (:version cached)) (long version)))
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
