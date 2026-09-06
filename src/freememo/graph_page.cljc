(ns freememo.graph-page
  "Search-first exploration of the approved concept graph. The server computes
   stable positions. The client applies scope, relation, k-core, and local-view
   filters without a database round trip."
  (:require
   [clojure.string :as str]
   [freememo.icons :as icons]
   [freememo.navigation :as nav]
   [freememo.quiz-feedback :as fb]
   [freememo.segmented :as seg]
   [freememo.typeahead :as typeahead]
   [freememo.viewport :as viewport]
   [hyperfiddle.electric-forms5 :as forms]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.kg-graph :as kgg])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [taoensso.telemere :as tel])
   #?(:cljs [freememo.graph-render :as render])
   #?(:cljs [freememo.vendor-libs :as vendor])))

;; ---------------------------------------------------------------------------
;; Pure projections
;; ---------------------------------------------------------------------------

(defn- item-count-label [n singular]
  (str n " " singular (when (not= 1 n) "s")))

(defn predicate-counts [edges]
  (frequencies (map #(nth % 2) edges)))

(defn document-counts [edges]
  (frequencies (map #(nth % 3) edges)))

(defn node-document-counts [edges]
  (into {}
    (map (fn [[node-id doc-ids]] [node-id (count doc-ids)]))
    (reduce (fn [acc [source target _predicate topic]]
              (-> acc
                (update source (fnil conj #{}) topic)
                (update target (fnil conj #{}) topic)))
      {} edges)))

(defn concept-options [{:keys [nodes edges]}]
  (let [doc-counts (node-document-counts edges)]
    (->> nodes
      (map (fn [[id label degree _x _y]]
             (let [docs (get doc-counts id 0)]
               {:id id
                :label (str label)
                :description (str (item-count-label degree "connection") " · "
                               (item-count-label docs "document"))})))
      (sort-by (comp str/lower-case :label))
      vec)))

(defn document-options [{:keys [docs edges]}]
  (let [counts (document-counts edges)]
    (into [{:id :all :label "All documents"
            :description (item-count-label (count edges) "relation")}]
      (->> docs
        (map (fn [[id title]]
               {:id id :label (str title)
                :description (item-count-label (get counts id 0) "relation")}))
        (sort-by (comp str/lower-case :label))))))

(defn relation-rows [{:keys [predicates edges]} source query]
  (let [scoped-edges (if (= source :all)
                       edges
                       (filterv #(= source (nth % 3)) edges))
        counts (predicate-counts scoped-edges)
        q (str/lower-case (str/trim (or query "")))]
    (->> predicates
      (map-indexed (fn [idx [_id label]]
                     {:idx idx :label (str label) :count (get counts idx 0)}))
      (filter #(or (str/blank? q)
                 (str/includes? (str/lower-case (:label %)) q)))
      (sort-by (juxt (comp - :count) (comp str/lower-case :label)))
      vec)))

(defn- pred-checked? [pred-set idx]
  (or (nil? pred-set) (contains? pred-set idx)))

(defn- set-pred-checked [pred-set idx n-preds checked?]
  (let [full (set (range n-preds))
        current (or pred-set full)
        next (if checked? (conj current idx) (disj current idx))]
    (when-not (= next full) next)))

(defn- selected-predicate-count [pred-set n-preds]
  (if (nil? pred-set) n-preds (count pred-set)))

(defn- source-title [payload source]
  (if (= source :all)
    "All documents"
    (or (some (fn [[id title]] (when (= id source) title)) (:docs payload))
      "Document")))

;; ---------------------------------------------------------------------------
;; Server queries
;; ---------------------------------------------------------------------------

(defn graph-payload*
  "Return the cached positioned graph for one user and mutation version."
  [version user-id]
  #?(:clj
     (do
       (tel/log! {:level :info :id ::payload.enter
                  :data {:user-id user-id :version version}}
         "graph-page: graph payload requested")
       (let [started-at (System/currentTimeMillis)]
         (try
           (let [payload (kgg/get-or-compute user-id "all" version)]
             (tel/log! {:level :info :id ::payload.exit
                        :data {:user-id user-id
                               :nodes (count (:nodes payload))
                               :edges (count (:edges payload))
                               :ms (- (System/currentTimeMillis) started-at)}}
               "graph-page: graph payload ready")
             payload)
           (catch Throwable error
             (tel/log! {:level :error :id ::payload.error
                        :data {:user-id user-id
                               :ms (- (System/currentTimeMillis) started-at)
                               :error (.getMessage error)
                               :class (.getName (class error))}}
               "graph-page: graph payload failed")
             (throw error)))))
     :cljs nil))

(defn graph-entity-card* [_version user-id entity-id]
  #?(:clj (db/kg-entity-card user-id entity-id)
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Sigma bridge
;; ---------------------------------------------------------------------------

(defn schedule-graph-init! [!ctx container payload on-select opts]
  #?(:cljs
     (js/setTimeout
       (fn []
         (vendor/with! :graph
           (fn [] (reset! !ctx (render/init! container payload on-select opts)))))
       0)
     :clj nil))

(defn apply-filters! [ctx source pred-set connectedness]
  #?(:cljs (render/apply-filters! ctx source pred-set connectedness) :clj nil))

(defn set-view! [ctx mode selected-id depth]
  #?(:cljs (render/set-view! ctx mode selected-id depth) :clj nil))

(defn zoom-in! [ctx]
  #?(:cljs (render/zoom-in! ctx) :clj nil))

(defn zoom-out! [ctx]
  #?(:cljs (render/zoom-out! ctx) :clj nil))

(defn fit-visible! [ctx]
  #?(:cljs (render/fit! ctx) :clj nil))

(defn reset-camera! [ctx]
  #?(:cljs (render/reset-camera! ctx) :clj nil))

(defn destroy-graph! [ctx]
  #?(:cljs (render/destroy! ctx) :clj nil))

;; ---------------------------------------------------------------------------
;; Filter controls
;; ---------------------------------------------------------------------------

(e/defn ConceptPicker [payload !selected-id !view-mode]
  (e/client
    (let [!committed (atom nil)
          committed (e/watch !committed)
          selected-id (e/watch !selected-id)
          view-mode (e/watch !view-mode)]
      (when committed
        (case (reset! !view-mode :neighborhood)
          (reset! !committed nil)))
      (when (and (nil? selected-id) (= view-mode :neighborhood))
        (reset! !view-mode :overview))
      (dom/section
        (dom/props {:class "graph-filter-section"})
        (dom/div
          (dom/props {:class "graph-filter-heading"})
          (dom/label (dom/text "Find a concept")))
        (typeahead/Typeahead !selected-id (concept-options payload)
          "Search concepts…" !committed nil)))))

(e/defn ViewPicker [!view-mode view-mode selected-id]
  (e/client
    (let [SaveView (e/fn [t next-view]
                     (e/client
                       (if (or (= next-view :overview) selected-id)
                         (case (reset! !view-mode next-view) (t))
                         (t))))]
      (dom/section
        (dom/props {:class "graph-filter-section"})
        (dom/div (dom/props {:class "graph-filter-heading"})
          (dom/span (dom/text "View")))
        (dom/div
          (dom/props {:class "segmented graph-view-picker"
                      :role "radiogroup" :aria-label "Graph view"})
          (seg/Segment view-mode :overview "Overview" SaveView)
          (when selected-id
            (seg/Segment view-mode :neighborhood "Neighborhood" SaveView)))))))

(e/defn DocumentPicker [payload !source]
  (e/client
    (let [source (e/watch !source)]
      (when (nil? source) (reset! !source :all))
      (dom/section
        (dom/props {:class "graph-filter-section"})
        (dom/div (dom/props {:class "graph-filter-heading"})
          (dom/label (dom/text "Documents")))
        (typeahead/Typeahead !source (document-options payload)
          "Search documents…" nil nil)))))

(e/defn ConnectednessControl [!connectedness connectedness max-connectedness]
  (e/client
    (dom/section
      (dom/props {:class "graph-filter-section"})
      (dom/div
        (dom/props {:class "graph-filter-heading"})
        (dom/label (dom/text "Connectedness"))
        (dom/strong (dom/text (str connectedness "+"))))
      (let [value (forms/Input (str connectedness)
                    :type "range" :min "1" :max (str max-connectedness)
                    :class "graph-connectedness" :aria-label "Minimum connectedness")]
        (when (not= value (str connectedness))
          (reset! !connectedness (or (parse-long value) 1))))
      (dom/div
        (dom/props {:class "graph-range-labels"})
        (dom/span (dom/text "Include small pairs"))
        (dom/span (dom/text "Core concepts"))))))

(e/defn RelationFilter [payload source !preds preds]
  (e/client
    (let [n-preds (count (:predicates payload))]
      (dom/section
        (dom/props {:class "graph-filter-section graph-relations-section"})
        (dom/div
          (dom/props {:class "graph-filter-heading"})
          (dom/label (dom/text "Relations"))
          (dom/span
            (dom/props {:class "graph-filter-meta"})
            (dom/text (str (selected-predicate-count preds n-preds) " of " n-preds))))
        (let [query (forms/Input* "" :type "search" :class "input"
                      :placeholder "Filter relations…" :aria-label "Filter relations")
              rows (relation-rows payload source query)]
        (dom/div
          (dom/props {:class "graph-relation-actions"})
          (dom/button
            (dom/props {:type "button" :class "graph-text-action"})
            (dom/text "Select all")
            (dom/On "click" (fn [_] (reset! !preds nil)) nil))
          (dom/button
            (dom/props {:type "button" :class "graph-text-action"})
            (dom/text "Select none")
            (dom/On "click" (fn [_] (reset! !preds #{})) nil)))
        (dom/div
          (dom/props {:class "graph-relation-list"})
          (if (empty? rows)
            (dom/p (dom/props {:class "graph-filter-empty"})
              (dom/text "No matching relations"))
            (e/for [{:keys [idx label count]} (e/diff-by :idx rows)]
              (let [checked? (pred-checked? preds idx)]
                (dom/label
                  (dom/props {:class "graph-relation-row"})
                  (let [next-checked? (forms/Checkbox checked?
                                        :aria-label (str "Show " label))]
                    (when (not= checked? next-checked?)
                      (reset! !preds
                        (set-pred-checked @!preds idx n-preds next-checked?))))
                  (dom/span (dom/props {:class "graph-relation-label" :dir "auto"})
                    (dom/text label))
                  (dom/span (dom/props {:class "graph-filter-meta"})
                    (dom/text count))))))))))))

(e/defn FiltersPanel
  [payload !selected-id selected-id !source !preds preds
   !connectedness connectedness max-connectedness !view-mode view-mode open?]
  (e/client
    (dom/aside
      (dom/props {:class (str "graph-filters" (when-not open? " is-closed"))
                  :aria-label "Graph filters" :aria-hidden (str (not open?))})
      (ConceptPicker payload !selected-id !view-mode)
      (ViewPicker !view-mode view-mode selected-id)
      (DocumentPicker payload !source)
      (ConnectednessControl !connectedness connectedness max-connectedness)
      (RelationFilter payload (e/watch !source) !preds preds))))

;; ---------------------------------------------------------------------------
;; Graph scene and inspector
;; ---------------------------------------------------------------------------

(e/defn GraphToolButton [label glyph on-click]
  (e/client
    (dom/button
      (dom/props {:type "button" :class "graph-tool-button"
                  :aria-label label :data-tooltip label})
      (if (keyword? glyph)
        (icons/Icon glyph :size 16)
        (dom/span (dom/props {:aria-hidden "true"}) (dom/text glyph)))
      (dom/On "click" (fn [_] (on-click)) nil))))

(e/defn GraphToolbar
  [ctx stats view-mode depth source-title selected-pred-count n-preds
   connectedness filters-open? !filters-open? !source !preds !connectedness]
  (e/client
    (dom/div
      (dom/props {:class "graph-scene-toolbar"})
      (dom/div
        (dom/props {:class "graph-toolbar-summary" :role "status" :aria-live "polite"})
        (dom/span (dom/props {:class "graph-stat-chip"})
          (dom/strong (dom/text (or (:nodes stats) 0))) (dom/text " concepts"))
        (dom/span (dom/props {:class "graph-stat-chip"})
          (dom/strong (dom/text (or (:edges stats) 0))) (dom/text " relations"))
        (when (= view-mode :neighborhood)
          (dom/span (dom/props {:class "graph-stat-chip"})
            (dom/text (str depth " hop" (when (not= depth 1) "s")))))
        (when (not= source-title "All documents")
          (dom/button
            (dom/props {:type "button"
                        :class "filter-chip filter-chip-active graph-active-filter"
                        :aria-label (str "Remove document filter " source-title)})
            (dom/text (str source-title " ×"))
            (dom/On "click" (fn [_] (reset! !source :all)) nil)))
        (when (not= selected-pred-count n-preds)
          (dom/button
            (dom/props {:type "button"
                        :class "filter-chip filter-chip-active graph-active-filter"
                        :aria-label "Remove relation filters"})
            (dom/text (str selected-pred-count " relations ×"))
            (dom/On "click" (fn [_] (reset! !preds nil)) nil)))
        (when (> connectedness 1)
          (dom/button
            (dom/props {:type "button"
                        :class "filter-chip filter-chip-active graph-active-filter"
                        :aria-label "Remove connectedness filter"})
            (dom/text (str "Connectedness " connectedness "+ ×"))
            (dom/On "click" (fn [_] (reset! !connectedness 1)) nil)))
        (when (or (not= source-title "All documents")
                  (not= selected-pred-count n-preds)
                  (> connectedness 1))
          (dom/button
            (dom/props {:type "button" :class "graph-clear-filters"})
            (dom/text "Clear filters")
            (dom/On "click"
              (fn [_]
                (reset! !source :all)
                (reset! !preds nil)
                (reset! !connectedness 1))
              nil))))
      (dom/div
        (dom/props {:class "graph-toolbar-actions"})
        (GraphToolButton (if filters-open? "Hide filters" "Show filters")
          "☷" #(swap! !filters-open? not))
        (GraphToolButton "Zoom out" "−" #(zoom-out! ctx))
        (GraphToolButton "Zoom in" "+" #(zoom-in! ctx))
        (GraphToolButton "Fit visible concepts" "⌗" #(fit-visible! ctx))
        (GraphToolButton "Reset view" :rotate-ccw #(reset-camera! ctx))))))

(e/defn DepthPicker [!depth depth]
  (e/client
    (let [SaveDepth (e/fn [t next-depth]
                      (e/client
                        (case (reset! !depth next-depth) (t))))]
      (dom/div
        (dom/props {:class "graph-depth-picker"})
        (dom/span (dom/text "Depth"))
        (dom/div
          (dom/props {:class "segmented" :role "radiogroup"
                      :aria-label "Neighborhood depth"})
          (seg/Segment depth 1 "1 hop" SaveDepth)
          (seg/Segment depth 2 "2 hops" SaveDepth))))))

(e/defn GraphScene
  [payload kg-bump !ctx ctx !selected-id selected-id !view-mode view-mode
   !depth depth !source source !preds preds !connectedness connectedness
   stats !stats filters-open? !filters-open?]
  (e/client
    (let [node-count (count (:nodes payload))
          n-preds (count (:predicates payload))
          !minimap-node (atom nil)]
      (dom/main
        (dom/props {:class "graph-scene" :aria-label "Knowledge graph"})
        (dom/div
          (dom/props {:class "graph-minimap" :role "img"
                      :aria-label "Graph overview"})
          (case (when-not (identical? @!minimap-node dom/node)
                  (reset! !minimap-node dom/node))
            nil))
        (dom/div
          (dom/props {:class "graph-render-surface"})
          (e/for-by identity [_key [[kg-bump node-count]]]
            (dom/div
              (dom/props {:class "graph-render-canvas"})
              (schedule-graph-init! !ctx dom/node payload
                (fn [id]
                  (reset! !selected-id id)
                  (reset! !view-mode (if id :neighborhood :overview)))
                {:on-counts (fn [counts]
                              (when (not= counts @!stats)
                                (reset! !stats counts)))
                 :minimap-container @!minimap-node})
              (e/on-unmount
                (fn []
                  (destroy-graph! @!ctx)
                  (reset! !ctx nil))))))
        (case (apply-filters! ctx source preds connectedness) nil)
        (case (set-view! ctx view-mode selected-id depth) nil)
        (GraphToolbar ctx stats view-mode depth (source-title payload source)
          (selected-predicate-count preds n-preds) n-preds
          connectedness filters-open? !filters-open?
          !source !preds !connectedness)
        (when (= view-mode :neighborhood)
          (DepthPicker !depth depth))
        (dom/div
          (dom/props {:class "graph-scene-hint"})
          (dom/text "Drag to pan · Scroll to zoom · Select a concept to inspect it"))))))

(e/defn GraphInspector
  [kg-bump user-id navigate! !selected-id selected-id !view-mode]
  (e/client
    (when selected-id
      (let [card (e/server
                   (e/Offload #(graph-entity-card* kg-bump user-id selected-id)))
            facts (:facts card)
            document-count (count (into #{} (keep :graph_topic_id) facts))
            !linked-entity (atom nil)
            linked-entity (e/watch !linked-entity)]
        (when linked-entity
          (case (reset! !selected-id (:id linked-entity))
            (reset! !linked-entity nil)))
        (dom/aside
          (dom/props {:class "graph-inspector" :aria-label "Selected concept details"})
          (dom/div
            (dom/props {:class "graph-inspector-header"})
            (dom/div
              (dom/div (dom/props {:class "graph-inspector-kicker"})
                (dom/text "Selected concept"))
              (dom/h2 (dom/props {:dir "auto"}) (dom/text (:label card))))
            (dom/button
              (dom/props {:type "button" :class "icon-btn"
                          :aria-label "Close concept details" :data-tooltip "Close"})
              (icons/Icon :x :size 17)
              (dom/On "click"
                (fn [_]
                  (reset! !selected-id nil)
                  (reset! !view-mode :overview))
                nil)))
          (dom/p
            (dom/props {:class "graph-inspector-summary"})
            (dom/text (str (item-count-label (count facts) "fact") " across "
                        (item-count-label document-count "document"))))
          (when (seq (:aliases card))
            (dom/p
              (dom/props {:class "graph-inspector-aliases" :dir "auto"})
              (dom/text (str "Also: " (str/join ", " (:aliases card))))))
          (dom/button
            (dom/props {:type "button" :class "btn btn-primary graph-open-facts"})
            (dom/text "Open Facts")
            (dom/On "click" (fn [_] (navigate! :knowledge)) nil))
          (dom/div
            (dom/props {:class "graph-inspector-section-heading"})
            (dom/strong (dom/text "Connected facts"))
            (dom/span (dom/text (count facts))))
          (if (empty? facts)
            (dom/p (dom/props {:class "graph-filter-empty"})
              (dom/text "No facts reference this concept"))
            (dom/ul
              (dom/props {:class "graph-inspector-facts"})
              (e/for [fact (e/diff-by :id facts)]
                (fb/FactLine fact !linked-entity
                  (fn [topic-id page]
                    (navigate! :viewer (nav/nav-topic topic-id :graph page))))))))))))

(e/defn GraphEmptyState [navigate!]
  (e/client
    (dom/div
      (dom/props {:class "graph-empty-state"})
      (icons/Icon :share-2 :size 40)
      (dom/h2 (dom/text "No concept connections yet"))
      (dom/p
        (dom/text "Distill documents in the Facts tab. Connected concepts will appear here."))
      (dom/button
        (dom/props {:type "button" :class "btn btn-secondary"})
        (dom/text "Go to Facts")
        (dom/On "click" (fn [_] (navigate! :knowledge)) nil)))))

;; ---------------------------------------------------------------------------
;; Page
;; ---------------------------------------------------------------------------

(e/defn GraphPage [user-id navigate!]
  (e/client
    (let [kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
          payload (e/server (e/Offload #(graph-payload* kg-bump user-id)))
          node-count (count (:nodes payload))
          max-degree (reduce (fn [m node] (max m (nth node 2 0))) 1 (:nodes payload))
          max-connectedness (max 1 (min 10 max-degree))
          compact? (e/watch viewport/!compact?)
          !ctx (atom nil) ctx (e/watch !ctx)
          !selected-id (atom nil) selected-id (e/watch !selected-id)
          !source (atom :all) source (e/watch !source)
          !preds (atom nil) preds (e/watch !preds)
          !connectedness (atom 1) connectedness (e/watch !connectedness)
          !view-mode (atom :overview) view-mode (e/watch !view-mode)
          !depth (atom 2) depth (e/watch !depth)
          !stats (atom {:nodes node-count :edges (count (:edges payload))})
          stats (e/watch !stats)
          !filters-open? (atom (not compact?)) filters-open? (e/watch !filters-open?)]
      (if (zero? node-count)
        (GraphEmptyState navigate!)
        (dom/div
          (dom/props {:class "graph-page"})
          (FiltersPanel payload !selected-id selected-id !source !preds preds
            !connectedness connectedness max-connectedness
            !view-mode view-mode filters-open?)
          (GraphScene payload kg-bump !ctx ctx !selected-id selected-id
            !view-mode view-mode !depth depth !source source !preds preds
            !connectedness connectedness stats !stats filters-open? !filters-open?)
          (GraphInspector kg-bump user-id navigate! !selected-id selected-id
            !view-mode))))))
