(ns freememo.graph-render
  "Imperative sigma.js renderer for the Graph tab. CLJS-only. freememo.graph-page
   owns the Electric lifecycle and calls init!/destroy!/apply-filters!/
   focus-node! here.

   Positions are server-computed (sfdp) and FIXED — no client layout runs.
   Visibility filters and hover/click highlight are pure sigma reducers reading
   one mutable ui-atom; mutate it, then (.refresh renderer) to repaint.

   sigma + graphology are vendored UMD globals (window.Sigma / window.graphology)
   with NO <script> tag: freememo.vendor-libs injects them the first time the
   Graph tab opens (:graph group, graphology first — sigma's Graph model comes
   from it). npm :require is still not an option: the prod Docker build has no
   npm step. JS objects are ^js-hinted so :advanced does not rename their
   (extern-less) methods.

   ctx = {:renderer <Sigma> :graph <graphology.Graph> :ui <atom>
          :stop-theme-colours! <0-arg fn>}.
   :stop-theme-colours! detaches the theme listeners that keep canvas-drawn
   labels and hover chrome readable — destroy! owns calling it; nothing else may."
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
    (let [{:keys [hover selected vnodes colours]} @!ui
          focus (or selected hover)]
      (cond
        (and vnodes (not (.has vnodes node)))
        (merge-js data {:hidden true})
        (and focus (not= node focus) (not (.areNeighbors g focus node)))
        (merge-js data {:color (:dim colours) :label "" :zIndex 0})
        (and focus (= node focus))
        (merge-js data {:highlighted true :zIndex 2})
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
    (let [{:keys [hover selected vedges colours]} @!ui
          focus (or selected hover)]
      (cond
        (and vedges (not (.has vedges edge)))
        (merge-js data {:hidden true})
        (and focus (not (.hasExtremity g edge focus)))
        (merge-js data {:hidden true})
        :else (merge-js data {:color (:edge colours)})))))

(defn- inherited-text-colour
  "The container's computed CSS `color`. index.css sets `color` from
   --color-text-primary on body, so this resolves to whatever the ACTIVE theme
   computes — an explicit body[data-theme], or the OS preference under \"auto\".
   Pre: `container` is attached to the document (else the cascade has no root to
   inherit through). Post: an rgb() string, never nil."
  [^js container]
  (.-color (js/getComputedStyle container)))

(defn- container-surface-colour
  "The container's computed CSS `background-color`. graph-page paints
   --color-bg-page there for exactly this read — canvas chrome cannot inherit a
   background, so the colour has to be fetched as a value.
   Pre: `container` carries an OPAQUE background-color; a transparent one
   resolves to rgba(0,0,0,0), which parse-rgb rejects and the caller answers
   with fallback-colours. Post: a colour string, never nil."
  [^js container]
  (.-backgroundColor (js/getComputedStyle container)))

(defn- parse-rgb
  "[r g b] from a computed colour string (`rgb(r, g, b)`, `rgba(r, g, b, a)`,
   or the space/slash notation), or nil when it is fully transparent or in any
   other notation — neither conveys a colour to mix with."
  [s]
  (when-let [m (some->> s (re-find #"rgba?\(\s*([\d.]+)[,\s]+([\d.]+)[,\s]+([\d.]+)(?:\s*[,/]\s*([\d.%]+))?"))]
    (let [[_ r g b a] m]
      (when-not (contains? #{"0" "0%" "0.0"} a)
        (mapv #(js/parseFloat %) [r g b])))))

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
  "Answer when the container's background is not an opaque colour — its
   precondition is violated, so degrade to sigma's own light-theme constants
   rather than to a colour derived from a wrong surface. Caller's bug: whoever
   dropped the container background."
  {:dim "#c8ccd4" :edge "#dbe0e8" :hover-fill "#FFF" :hover-stroke "rgba(0, 0, 0, 0.25)"})

(defn- theme-colours
  "Every canvas colour the active theme implies, all derived from the
   container's own computed `color` and `background-color` so this file holds no
   second palette. Post: every value is a colour string, never nil."
  [^js container]
  (let [text (inherited-text-colour container)
        surface (container-surface-colour container)]
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
                  (.setSetting renderer "labelColor" #js {:color (:label colours)})))
        ^js obs (js/MutationObserver. sync!)
        ^js mq (.matchMedia js/window "(prefers-color-scheme: dark)")]
    (sync! nil)
    (.observe obs js/document.body #js {:attributes true
                                        :attributeFilter #js ["data-theme"]})
    (.addEventListener mq "change" sync!)
    (fn [] (.disconnect obs) (.removeEventListener mq "change" sync!))))

(defn init!
  "Create a sigma renderer over `container` for `payload`. Wires clickNode →
   (on-select entity-id-int), clickStage → (on-select nil), and hover
   highlight. Returns ctx, or nil when a lib/container is missing or empty."
  [container payload on-select]
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
                       :vnodes nil :vedges nil
                       :colours colours})
            ^js renderer (new Sigma g container
                           #js {:renderEdgeLabels false
                                :labelDensity 0.6
                                :labelRenderedSizeThreshold 12
                                :defaultNodeColor "#5b8def"
                                ;; Matches what edge-reducer paints, so
                                ;; indexation and the first frame agree; the
                                ;; reducer owns it from then on.
                                :defaultEdgeColor (:edge colours)
                                :labelColor #js {:color (:label colours)}
                                :defaultDrawNodeHover (draw-node-hover !ui)
                                :nodeReducer (node-reducer g !ui)
                                :edgeReducer (edge-reducer g !ui)})
            stop-theme-colours! (track-theme-colours! renderer container !ui)]
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
        {:renderer renderer :graph g :ui !ui
         :stop-theme-colours! stop-theme-colours!}))))

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

(defn destroy!
  "Kill the renderer and detach the theme listeners track-theme-colours!
   registered. Both MUST go: they close over the renderer and would call
   setSetting on a killed instance on the next theme change."
  [ctx]
  (when-let [stop! (:stop-theme-colours! ctx)] (stop!))
  (when-let [^js r (:renderer ctx)] (.kill r)))
