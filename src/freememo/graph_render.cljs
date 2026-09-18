(ns freememo.graph-render
  "Imperative sigma.js renderer for the Graph tab. CLJS-only. freememo.graph-page
   owns the Electric lifecycle and calls init!/destroy!/apply-filters! and the
   view and camera functions here.

   The overview uses server-computed sfdp positions. A selected neighborhood
   uses a deterministic radial layout so its hop structure stays legible.
   Visibility filters and hover/click highlight are sigma reducers that read one
   mutable ui-atom; mutate it, then call (.refresh renderer) to repaint.

   sigma + graphology are vendored UMD globals (window.Sigma / window.graphology)
   with NO <script> tag: freememo.vendor-libs injects them the first time the
   Graph tab opens (:graph group, graphology first — sigma's Graph model comes
   from it). npm :require is still not an option: the prod Docker build has no
   npm step. JS objects are ^js-hinted so :advanced does not rename their
   (extern-less) methods.

   ctx = {:renderer <Sigma> :graph <graphology.Graph> :ui <atom>
          :stop-theme-colours! <0-arg fn> and optional minimap functions}.
   :stop-theme-colours! detaches the theme listeners that keep canvas-drawn
   labels and hover chrome readable — destroy! owns calling it; nothing else may."
  (:require [clojure.string :as str]
            [freememo.graph-state :as graph-state]))

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

(defn- payload-model
  [payload]
  (mapv (fn [[i [s t pidx topic]]]
          {:id (str i) :source (str s) :target (str t)
           :pred pidx :topic topic})
    (map-indexed vector (:edges payload))))

(defn- build-graphology
  "payload {:nodes [[id label degree x y]…] :edges [[s t pred-idx topic]…]} →
   a directed multigraph with fixed positions and degree-scaled sizes.
   Edge key = the edge's index in payload, so predicate/source filters address
   edges by key. Endpoint-guarded so a malformed edge cannot throw."
  [Graph payload]
  (let [^js g (new Graph #js {:type "directed" :multi true})
        predicate-labels (mapv second (:predicates payload))]
    (doseq [[id label degree x y] (:nodes payload)]
      (.addNode g (str id)
        #js {:label (str label)
             :x x :y y
             :overviewX x :overviewY y
             :size (+ 3 (* 1.4 (js/Math.sqrt (max 1 (or degree 1)))))}))
    (doseq [[i [s t pidx topic]] (map-indexed vector (:edges payload))]
      (let [sk (str s) tk (str t)]
        (when (and (.hasNode g sk) (.hasNode g tk))
          (.addDirectedEdgeWithKey g (str i) sk tk
            #js {:pred pidx :topic topic
                 :type "arrow"
                 :label (str (get predicate-labels pidx ""))}))))
    g))

(defn- recompute-visibility!
  "Apply scope filters, then the k-core and selected view. Store JS sets for
   constant-time reducer lookups. Return the new visible counts."
  [!ui]
  (let [{:keys [model source pred-set min-degree view-mode selected depth]} @!ui
        {:keys [node-ids edge-ids]} (graph-state/visible-subgraph
                                     model {:source source :pred-set pred-set
                                            :min-degree min-degree
                                            :view-mode view-mode
                                            :selected selected :depth depth})
        vnodes (js/Set.)
        vedges (js/Set.)
        counts {:nodes (count node-ids) :edges (count edge-ids)}]
    (doseq [node node-ids] (.add vnodes node))
    (doseq [edge edge-ids] (.add vedges edge))
    (swap! !ui assoc :vnodes vnodes :vedges vedges :counts counts)
    counts))

(defn- notify-counts!
  [!ui counts]
  (when-let [on-counts (:on-counts @!ui)]
    (on-counts counts)))

(def ^:private neighborhood-label-limit 16)

(defn- visible-edges
  [model ^js vedges]
  (filterv #(.has vedges (:id %)) model))

(defn- evenly-spaced
  [node-ids positions limit]
  (let [ordered (vec (sort-by (fn [node]
                                (let [{:keys [x y]} (get positions node)]
                                  (js/Math.atan2 (- y 0.5) (- x 0.5))))
                       node-ids))
        n (count ordered)
        take-count (min n limit)]
    (mapv #(nth ordered (int (* % (/ n take-count))))
      (range take-count))))

(defn- neighborhood-labels
  "Label a small, evenly spaced sample from both hop rings."
  [selected {:keys [positions node-ids-by-hop]}]
  (let [first-labels (evenly-spaced (get node-ids-by-hop 1) positions 10)
        second-labels (evenly-spaced (get node-ids-by-hop 2) positions
                        (- neighborhood-label-limit 1 (count first-labels)))
        label-ids (js/Set.)]
    (.add label-ids selected)
    (doseq [node (concat first-labels second-labels)]
      (.add label-ids node))
    label-ids))

(defn- restore-overview-layout!
  [^js graph]
  (.forEachNode graph
    (fn [node _]
      (.setNodeAttribute graph node "x" (.getNodeAttribute graph node "overviewX"))
      (.setNodeAttribute graph node "y" (.getNodeAttribute graph node "overviewY")))))

(defn- apply-current-layout!
  "Apply the overview or radial positions once for each distinct view state."
  [ctx]
  (let [!ui (:ui ctx)
        {:keys [view-mode selected depth source pred-set min-degree model vedges]} @!ui
        layout-key [view-mode selected depth source pred-set min-degree]
        ^js graph (:graph ctx)
        ^js renderer (:renderer ctx)]
    (when (not= layout-key (:layout-key @!ui))
      (if (= view-mode :neighborhood)
        (let [edges (visible-edges model vedges)
              layout (graph-state/neighborhood-layout edges selected)]
          (doseq [[node {:keys [x y]}] (:positions layout)]
            (.setNodeAttribute graph node "x" x)
            (.setNodeAttribute graph node "y" y))
          (.setCustomBBox renderer #js {:x #js [0 1] :y #js [0 1]})
          (swap! !ui assoc
            :layout-key layout-key
            :hop-by-id (:hop-by-id layout)
            :hop-radii (:radii layout)
            :label-node-ids (neighborhood-labels selected layout)))
        (do
          (restore-overview-layout! graph)
          (.setCustomBBox renderer nil)
          (swap! !ui assoc
            :layout-key layout-key
            :hop-by-id nil
            :hop-radii nil
            :label-node-ids nil)))
      true)))

(defn- refresh-visibility!
  [ctx]
  (let [!ui (:ui ctx)
        counts (recompute-visibility! !ui)
        ^js renderer (:renderer ctx)
        layout-changed? (apply-current-layout! ctx)]
    (.refresh renderer)
    (when layout-changed?
      (js/requestAnimationFrame
        (fn []
          (.animatedReset (.getCamera renderer) #js {:duration 350}))))
    (when-let [draw! (:draw-minimap! ctx)] (draw!))
    (notify-counts! !ui counts)
    counts))

(defn- set-view-state!
  [ctx mode selected-id depth]
  (when ctx
    (let [^js graph (:graph ctx)
          selected (some-> selected-id str)
          neighborhood? (and (= mode :neighborhood)
                             selected
                             (.hasNode graph selected))]
      (swap! (:ui ctx) assoc
        :view-mode (if neighborhood? :neighborhood :overview)
        :selected (when neighborhood? selected)
        :depth (max 1 (min 2 (or depth 1))))
      (refresh-visibility! ctx))))

(defn- node-reducer [^js g !ui]
  (fn [node data]
    (let [{:keys [hover selected vnodes colours view-mode hop-by-id
                  label-node-ids]} @!ui
          hop (get hop-by-id node)]
      (cond
        (and vnodes (not (.has vnodes node)))
        (merge-js data {:hidden true})
        (and hover (not= node hover) (not (.areNeighbors g hover node)))
        (merge-js data {:color (:dim colours) :label "" :zIndex 0})
        (= node hover)
        (merge-js data {:forceLabel true :zIndex 3})
        (= node selected)
        (merge-js data {:highlighted true :forceLabel true :size 13 :zIndex 2})
        (= view-mode :neighborhood)
        (merge-js data {:label (if (and label-node-ids (.has label-node-ids node))
                                 (.-label data) "")
                        :forceLabel (boolean (and label-node-ids
                                              (.has label-node-ids node)))
                        :size (if (= hop 1) (min 6 (.-size data))
                                (min 4 (.-size data)))
                        :zIndex (if (= hop 1) 1 0)})
        :else data))))

(defn- edge-reducer
  "Visibility, plus the theme's edge colour — which has to be applied HERE and
   cannot be a tracked setting. sigma bakes :defaultEdgeColor into each edge's
   display data during INDEXATION (`ds` in sigma.min.js, when the edge carries
   no own color attribute), and handleSettingsUpdate re-indexes nothing — so a
   setSetting on a theme change would repaint the old colour. Reducers run on
   every refresh, so this one tracks."
  [^js g !ui]
  (fn [edge data]
    (let [{:keys [hover vedges colours view-mode]} @!ui]
      (cond
        (and vedges (not (.has vedges edge)))
        (merge-js data {:hidden true})
        (and hover (not (.hasExtremity g edge hover)))
        (merge-js data {:hidden true})
        (= view-mode :neighborhood)
        (merge-js data {:color (:edge colours) :label ""})
        :else (merge-js data {:color (:edge colours)})))))

(defn- inherited-text-colour
  "The container's computed CSS `color`. index.css sets `color` from
   --color-text-primary on body, so this resolves to whatever the ACTIVE theme
   computes — an explicit body[data-theme], or the OS preference under \"auto\".
   Pre: `container` is attached to the document (else the cascade has no root to
   inherit through). Post: an rgb() string, never nil."
  [^js container]
  (.-color (js/getComputedStyle container)))

(defn- parse-rgb
  "[r g b] from a computed colour string (`rgb(r, g, b)`, `rgba(r, g, b, a)`,
   or the space/slash notation), or nil when it is fully transparent or in any
   other notation — neither conveys a colour to mix with."
  [s]
  (when-let [m (some->> s (re-find #"rgba?\(\s*([\d.]+)[,\s]+([\d.]+)[,\s]+([\d.]+)(?:\s*[,/]\s*([\d.%]+))?"))]
    (let [[_ r g b a] m]
      (when-not (contains? #{"0" "0%" "0.0"} a)
        (mapv #(js/parseFloat %) [r g b])))))

(defn- surface-colour
  "The computed `background-color` of the nearest element at or above `el` that
   parse-rgb can read — the surface the canvas is painted over. Canvas chrome
   cannot inherit a background, so the colour has to be fetched as a value.

   The walk is what keeps this correct. graph-page leaves the render container
   transparent so the scene's dot grid shows through, and a read of the
   container alone resolved to rgba(0,0,0,0). Every canvas colour then fell to
   fallback-colours, which are light-theme constants, so dark mode drew
   #e5e7eb labels on a #FFF hover pill.

   An element in a notation parse-rgb rejects is skipped like a transparent
   one: its ancestor's colour is a closer guess than fallback-colours.
   Post: a colour string, or nil when `el` is detached and no ancestor paints."
  [^js el]
  (loop [^js node el]
    (when node
      (let [bg (.-backgroundColor (js/getComputedStyle node))]
        (if (parse-rgb bg)
          bg
          (recur (.-parentElement node)))))))

(defn- blend-colour
  "Opaque mix of `fg` into `bg` — t 0 → bg, 1 → fg. Both are computed colour
   strings. Returns an rgb() string, or nil when either side is unparseable."
  [fg bg t]
  (let [f (parse-rgb fg) b (parse-rgb bg)]
    (when (and f b)
      (str "rgb(" (str/join ", " (map #(js/Math.round (+ %2 (* t (- %1 %2)))) f b)) ")"))))

;; Mix ratios against the page background. Nodes outside the focus recede at
;; dim-mix; edges and the hover pill's 1px outline need more, as thin strokes
;; read weaker than fills at equal contrast. Browser-measured against
;; --color-bg-page, both themes: edges land at 2.82:1 dark / 2.27:1 light,
;; under the nodes' 5.49:1 / 3.09:1 — present as structure, never competing
;; with the nodes they connect.
(def ^:private dim-mix 0.3)
(def ^:private edge-mix 0.35)
(def ^:private hover-stroke-mix 0.4)

(def ^:private fallback-colours
  "Answer when no element at or above the container paints a readable
   background. index.css paints html and body, so an attached container never
   reaches this. Degrade to sigma's own light-theme constants rather than to a
   colour derived from a wrong surface."
  {:dim "#c8ccd4" :edge "#dbe0e8" :hover-fill "#FFF" :hover-stroke "rgba(0, 0, 0, 0.25)"})

(defn- theme-colours
  "Every canvas colour the active theme implies, all derived from the
   container's computed `color` and the surface under it, so this file holds no
   second palette. Post: every value is a colour string, never nil."
  [^js container]
  (let [text (inherited-text-colour container)
        surface (surface-colour container)]
    (merge {:label text}
      (if-let [dim (blend-colour text surface dim-mix)]
        {:dim dim
         :edge (blend-colour text surface edge-mix)
         :hover-fill surface
         :hover-stroke (blend-colour text surface hover-stroke-mix)}
        fallback-colours))))

(defn- draw-node-hover
  "Replacement for sigma's defaultDrawNodeHover, reading the pill's colours from
   the ui-atom instead of hardcoding them.

   sigma 3.0.2 fills that pill with `#FFF` and drops a `#000` shadow behind it
   (`kn` in sigma.min.js). In dark mode the label is then --color-text-primary
   on white — about 1.1:1, i.e. hovering a node HID its label. Same geometry
   here, but filled with the page background and outlined instead of shadowed,
   so the label keeps body text's own contrast in both themes.

   Pre: (:colours @!ui) is seeded — init! does that when it builds the atom,
   and track-theme-colours! keeps it current. Label drawing is delegated to
   sigma's own drawer, which reads the labelColor that same fn keeps in step."
  [!ui]
  (fn [^js ctx ^js data ^js settings]
    (let [{:keys [hover-fill hover-stroke]} (:colours @!ui)
          label (.-label data)
          font-size (.-labelSize settings)
          pad 2
          x (.-x data) y (.-y data)]
      (set! (.-font ctx)
        (str (.-labelWeight settings) " " font-size "px " (.-labelFont settings)))
      (set! (.-fillStyle ctx) hover-fill)
      (set! (.-strokeStyle ctx) hover-stroke)
      (set! (.-lineWidth ctx) 1)
      (.beginPath ctx)
      (if (string? label)
        ;; A circle around the node, extended right into a box for the label.
        (let [box-w (js/Math.round (+ (.-width (.measureText ctx label)) 5))
              half-h (/ (js/Math.round (+ font-size (* 2 pad))) 2)
              radius (+ (max (.-size data) (/ font-size 2)) pad)
              cap-angle (js/Math.asin (/ half-h radius))
              cap-x (js/Math.sqrt (js/Math.abs (- (* radius radius) (* half-h half-h))))]
          (.moveTo ctx (+ x cap-x) (+ y half-h))
          (.lineTo ctx (+ x radius box-w) (+ y half-h))
          (.lineTo ctx (+ x radius box-w) (- y half-h))
          (.lineTo ctx (+ x cap-x) (- y half-h))
          (.arc ctx x y radius cap-angle (- cap-angle)))
        (.arc ctx x y (+ (.-size data) pad) 0 (* 2 js/Math.PI)))
      (.closePath ctx)
      (.fill ctx)
      (.stroke ctx)
      ((.-defaultDrawNodeLabel settings) ctx data settings))))

(defn- track-theme-colours!
  "Seed the ui-atom's :colours + sigma's labelColor from the active theme, keep
   both in step with it, and return a 0-arg stop fn.

   Canvas drawing never sees the CSS cascade, so every colour sigma paints has
   to be pushed in as a value. Two of them were theme-blind constants: sigma's
   default labelColor {color: \"#000\"} is 1.43:1 on the dark page background
   (#111827), and its hover pill is a hardcoded #FFF that the dark label then
   vanished into (see draw-node-hover). Deriving all of them from the
   container's computed colour and background keeps one source of truth for the
   palette — index.css — instead of a second one here.

   Two triggers, and both are needed: main.cljc writes body[data-theme] for an
   explicit choice, and under \"auto\" it writes no attribute at all, so the OS
   preference moves with no DOM mutation to observe.

   Post: (:colours @!ui) and labelColor both match (theme-colours container)
   until the returned stop fn runs. Invariant: one observer + one media listener
   per live ctx — destroy! MUST call the stop fn or both outlive the killed
   renderer."
  [^js renderer ^js container !ui]
  (let [sync! (fn [_]
                (let [colours (theme-colours container)]
                  (swap! !ui assoc :colours colours)
                  ;; setSetting schedules its own refresh — do not add one. It
                  ;; also repaints the reducers, which read :colours above.
                  (.setSetting renderer "labelColor" #js {:color (:label colours)})
                  (.setSetting renderer "edgeLabelColor" #js {:color (:label colours)})))
        ^js obs (js/MutationObserver. sync!)
        ^js mq (.matchMedia js/window "(prefers-color-scheme: dark)")]
    (sync! nil)
    (.observe obs js/document.body #js {:attributes true
                                        :attributeFilter #js ["data-theme"]})
    (.addEventListener mq "change" sync!)
    (fn [] (.disconnect obs) (.removeEventListener mq "change" sync!))))

(defn- start-hop-rings!
  "Draw hop guides on a Sigma-owned canvas below the graph edges."
  [^js renderer ^js graph !ui]
  (let [_ (.createCanvasContext renderer "hopRings"
            #js {:beforeLayer "edges"})
        ^js canvas (aget (.getCanvases renderer) "hopRings")
        ;; Sigma returns the renderer from createCanvasContext. Read the real
        ;; context from the canvas that Sigma registered.
        ^js ctx2d (.getContext canvas "2d")
        draw!
        (fn []
          (let [^js dims (.getDimensions renderer)
                width (.-width dims)
                height (.-height dims)
                {:keys [view-mode selected depth hop-radii colours]} @!ui]
            (.clearRect ctx2d 0 0 width height)
            (when (and (= view-mode :neighborhood) selected hop-radii)
              (let [x (.getNodeAttribute graph selected "x")
                    y (.getNodeAttribute graph selected "y")
                    ^js center (.graphToViewport renderer #js {:x x :y y})]
                (.save ctx2d)
                (set! (.-strokeStyle ctx2d) (:edge colours))
                (set! (.-lineWidth ctx2d) 1.25)
                (set! (.-lineCap ctx2d) "round")
                (.setLineDash ctx2d #js [2 7])
                (doseq [hop (range 1 (inc depth))]
                  (when-let [radius (get hop-radii hop)]
                    (let [^js rim (.graphToViewport renderer
                                    #js {:x (+ x radius) :y y})
                          screen-radius (js/Math.abs (- (.-x rim) (.-x center)))]
                      (.beginPath ctx2d)
                      (.arc ctx2d (.-x center) (.-y center) screen-radius
                        0 (* 2 js/Math.PI))
                      (.stroke ctx2d))))
                (.restore ctx2d)))))]
    (.setAttribute canvas "aria-hidden" "true")
    (set! (.. canvas -style -pointerEvents) "none")
    (.resize renderer true)
    (.on renderer "afterRender" draw!)
    {:draw! draw!
     :stop! (fn [] (.off renderer "afterRender" draw!))}))

(defn- start-minimap!
  "Draw a non-interactive graph overview and the current camera viewport."
  [ctx container]
  (when container
    (let [^js renderer (:renderer ctx)
          ^js graph (:graph ctx)
          ^js camera (.getCamera renderer)
          ^js canvas (.createElement js/document "canvas")
          !draw (atom nil)
          ^js resize-observer (when (exists? js/ResizeObserver)
                                (js/ResizeObserver.
                                  (fn [_] (when-let [draw! @!draw] (draw!)))))
          bounds (atom nil)
          graph-bounds
          (fn []
            (or @bounds
                (let [xs (array) ys (array)]
                  (.forEachNode graph
                    (fn [node _]
                      (.push xs (.getNodeAttribute graph node "overviewX"))
                      (.push ys (.getNodeAttribute graph node "overviewY"))))
                  (let [b {:min-x (apply min (array-seq xs))
                           :max-x (apply max (array-seq xs))
                           :min-y (apply min (array-seq ys))
                           :max-y (apply max (array-seq ys))}]
                    (reset! bounds b)
                    b))))
          draw!
          (fn []
            (let [width (max 1 (.-clientWidth container))
                  height (max 1 (.-clientHeight container))
                  dpr (or (.-devicePixelRatio js/window) 1)
                  {:keys [min-x max-x min-y max-y]} (graph-bounds)
                  span-x (max 1.0e-9 (- max-x min-x))
                  span-y (max 1.0e-9 (- max-y min-y))
                  pad 6
                  plot-w (- width (* 2 pad))
                  plot-h (- height (* 2 pad))
                  ->x (fn [x] (+ pad (* plot-w (/ (- x min-x) span-x))))
                  ->y (fn [y] (+ pad (* plot-h (- 1 (/ (- y min-y) span-y)))))
                  ^js ctx2d (.getContext canvas "2d")
                  vnodes (:vnodes @(:ui ctx))
                  colours (:colours @(:ui ctx))]
              (when (or (not= (.-width canvas) (* width dpr))
                        (not= (.-height canvas) (* height dpr)))
                (set! (.-width canvas) (* width dpr))
                (set! (.-height canvas) (* height dpr)))
              (.setTransform ctx2d dpr 0 0 dpr 0 0)
              (.clearRect ctx2d 0 0 width height)
              (.forEachNode graph
                (fn [node _]
                  (set! (.-fillStyle ctx2d)
                    (if (and vnodes (.has vnodes node)) "#5b8def" (:dim colours)))
                  (.beginPath ctx2d)
                  (.arc ctx2d
                    (->x (.getNodeAttribute graph node "overviewX"))
                    (->y (.getNodeAttribute graph node "overviewY"))
                    1.25 0 (* 2 js/Math.PI))
                  (.fill ctx2d)))
              (let [dims (.getDimensions renderer)
                    top-left (.viewportToGraph renderer #js {:x 0 :y 0})
                    bottom-right (.viewportToGraph renderer
                                   #js {:x (.-width dims) :y (.-height dims)})
                    x1 (->x (min (.-x top-left) (.-x bottom-right)))
                    x2 (->x (max (.-x top-left) (.-x bottom-right)))
                    y1 (->y (max (.-y top-left) (.-y bottom-right)))
                    y2 (->y (min (.-y top-left) (.-y bottom-right)))]
                (set! (.-strokeStyle ctx2d) (:label colours))
                (set! (.-lineWidth ctx2d) 1)
                (.strokeRect ctx2d x1 y1 (- x2 x1) (- y2 y1)))))]
      (reset! !draw draw!)
      (set! (.. canvas -style -width) "100%")
      (set! (.. canvas -style -height) "100%")
      (set! (.. canvas -style -display) "block")
      (.appendChild container canvas)
      (.on camera "updated" draw!)
      (when resize-observer (.observe resize-observer container))
      (draw!)
      {:draw! draw!
       :stop! (fn []
                (.off camera "updated" draw!)
                (when resize-observer (.disconnect resize-observer))
                (when (= (.-parentNode canvas) container)
                  (.removeChild container canvas)))})))

(defn init!
  "Create a sigma renderer over `container` for `payload`. Wires clickNode →
   (on-select entity-id-int), clickStage → (on-select nil), and hover
   highlight. Optional opts support :on-counts and :minimap-container. Returns
   ctx, or nil when a library, the container, or the graph is missing."
  [container payload on-select & [opts]]
  (let [Sigma (sigma-ctor)
        Graph (graphology-ctor)]
    (when (and container Sigma Graph (seq (:nodes payload)))
      (let [^js g (build-graphology Graph payload)
            ;; Read once, and BEFORE the renderer: the Sigma constructor
            ;; indexes and refreshes, so both reducers read this atom before
            ;; track-theme-colours! has run. A nil :dim or :edge would reach
            ;; sigma's colour parser.
            colours (theme-colours container)
            !ui (atom {:hover nil :selected nil
                       :source :all :pred-set nil :min-degree 1
                       :view-mode :overview :depth 1
                       :vnodes nil :vedges nil
                       :model (payload-model payload)
                       :counts {:nodes 0 :edges 0}
                       :on-select on-select
                       :on-counts (:on-counts opts)
                       :colours colours})
            ^js renderer (new Sigma g container
                           #js {:renderEdgeLabels false
                                :labelDensity 0.6
                                :labelRenderedSizeThreshold 12
                                :edgeLabelSize 10
                                :defaultNodeColor "#5b8def"
                                ;; Matches what edge-reducer paints, so
                                ;; indexation and the first frame agree; the
                                ;; reducer owns it from then on.
                                :defaultEdgeColor (:edge colours)
                                :labelColor #js {:color (:label colours)}
                                :edgeLabelColor #js {:color (:label colours)}
                                :defaultDrawNodeHover (draw-node-hover !ui)
                                :nodeReducer (node-reducer g !ui)
                                :edgeReducer (edge-reducer g !ui)})
            stop-theme-colours! (track-theme-colours! renderer container !ui)
            base-ctx {:renderer renderer :graph g :ui !ui
                      :stop-theme-colours! stop-theme-colours!}
            hop-rings (start-hop-rings! renderer g !ui)
            minimap (when-let [minimap-container (:minimap-container opts)]
                      (start-minimap! base-ctx minimap-container))
            ctx (cond-> (assoc base-ctx
                          :draw-hop-rings! (:draw! hop-rings)
                          :stop-hop-rings! (:stop! hop-rings))
                  minimap (assoc :draw-minimap! (:draw! minimap)
                                 :stop-minimap! (:stop! minimap)))]
        (refresh-visibility! ctx)
        (.on renderer "clickNode"
          (fn [^js e]
            (let [n (.-node e)]
              (set-view-state! ctx :neighborhood n (:depth @!ui))
              (when on-select (on-select (js/parseInt n 10))))))
        (.on renderer "clickStage"
          (fn [_]
            (set-view-state! ctx :overview nil (:depth @!ui))
            (when on-select (on-select nil))))
        (.on renderer "enterNode"
          (fn [^js e] (swap! !ui assoc :hover (.-node e)) (.refresh renderer)))
        (.on renderer "leaveNode"
          (fn [_] (swap! !ui assoc :hover nil) (.refresh renderer)))
        ctx))))

(defn apply-filters!
  "Set the filter state. Source is a topic ID or :all. Pred-set is the allowed
   predicate index set or nil. Min-degree is the k-core threshold. Recompute,
   lay out the visible view, and repaint."
  [ctx source pred-set min-degree]
  (when ctx
    (swap! (:ui ctx) assoc :source source :pred-set pred-set :min-degree min-degree)
    (refresh-visibility! ctx)))

(defn set-view!
  "Show the filtered overview or a one-hop or two-hop selected neighborhood."
  [ctx mode selected-id depth]
  (set-view-state! ctx mode selected-id depth))

(defn select-node!
  "Select a node through a keyboard or another accessible control."
  [ctx entity-id]
  (when ctx
    (if entity-id
      (let [node (str entity-id)
            ^js graph (:graph ctx)]
        (when (.hasNode graph node)
          (set-view-state! ctx :neighborhood node (:depth @(:ui ctx)))
          (when-let [on-select (:on-select @(:ui ctx))]
            (on-select (js/parseInt node 10)))
          true))
      (do
        (set-view-state! ctx :overview nil (:depth @(:ui ctx)))
        (when-let [on-select (:on-select @(:ui ctx))] (on-select nil))
        true))))

(defn visible-counts
  "Return {:nodes n :edges n} for the current filtered view."
  [ctx]
  (when ctx (:counts @(:ui ctx))))

(defn zoom-in! [ctx]
  (when-let [^js renderer (:renderer ctx)]
    (.animatedZoom (.getCamera renderer) #js {:duration 250})))

(defn zoom-out! [ctx]
  (when-let [^js renderer (:renderer ctx)]
    (.animatedUnzoom (.getCamera renderer) #js {:duration 250})))

(defn reset-camera! [ctx]
  (when-let [^js renderer (:renderer ctx)]
    (.animatedReset (.getCamera renderer) #js {:duration 350})))

(defn fit!
  "Fit the current visible nodes in the camera with a small margin."
  [ctx]
  (when ctx
    (let [^js renderer (:renderer ctx)]
      (if (= :neighborhood (:view-mode @(:ui ctx)))
        (reset-camera! ctx)
        (let [nodes (array)]
          (.forEach (:vnodes @(:ui ctx))
            (fn [node]
              (when-let [data (.getNodeDisplayData renderer node)]
                (.push nodes data))))
          (if (zero? (alength nodes))
            (reset-camera! ctx)
            (let [data (array-seq nodes)
                  xs (map #(.-x ^js %) data)
                  ys (map #(.-y ^js %) data)
                  min-x (apply min xs) max-x (apply max xs)
                  min-y (apply min ys) max-y (apply max ys)
                  ^js dims (.getDimensions renderer)
                  aspect (/ (max 1 (.-width dims)) (max 1 (.-height dims)))
                  ratio (max 0.08 (* 1.2 (max (- max-x min-x)
                                               (* aspect (- max-y min-y)))))
                  ^js camera (.getCamera renderer)]
              (.animate camera
                #js {:x (/ (+ min-x max-x) 2)
                     :y (/ (+ min-y max-y) 2)
                     :ratio ratio}
                #js {:duration 350}))))))))

(defn find-node-id-by-label
  "First node id whose label contains `q` (case-insensitive), or nil."
  [payload q]
  (let [ql (some-> q str str/lower-case str/trim)]
    (when (seq ql)
      (some (fn [[id label]]
              (when (str/includes? (str/lower-case (str label)) ql) id))
        (:nodes payload)))))

(defn destroy!
  "Stop custom drawing and theme listeners, then kill the renderer."
  [ctx]
  (when-let [stop! (:stop-minimap! ctx)] (stop!))
  (when-let [stop! (:stop-hop-rings! ctx)] (stop!))
  (when-let [stop! (:stop-theme-colours! ctx)] (stop!))
  (when-let [^js r (:renderer ctx)] (.kill r)))
