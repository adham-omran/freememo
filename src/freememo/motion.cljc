(ns freememo.motion
  "Short motions that answer the user's own action: an answer that opens, the
   next Learn topic that comes in, a verdict, a count that the user just
   changed.

   Every entry point is a plain fn, called with the node it moves. Never start
   one from an `e/defn` body that re-runs. Call it with the value it follows,
   as the LAST form of the element's body, and it decides itself whether the
   value is new. It keeps its own state per node, in a WeakMap, so Electric's
   re-runs and remounts cannot reset it.

   Durations are 120 to 260 ms: `ease-out` for a thing that arrives, `ease` for
   a thing that moves. Nothing moves when the reader prefers reduced motion.
   A flash is the one exception: it still shows, as a still outline, so the
   change is not lost.

   Rows of a table move by key in freememo.row-motion, not here.")

#?(:cljs
   (defn prefers-reduced-motion?
     "True when the reader asked for reduced motion."
     []
     (.-matches (js/matchMedia "(prefers-reduced-motion: reduce)"))))

#?(:cljs
   (defn- play!
     "Run `keyframes` on `node`. Pre: node is an Element or nil."
     [^js node keyframes opts]
     (when (and node (not (prefers-reduced-motion?)))
       (.animate node (clj->js keyframes) (clj->js opts)))))

;; ---------------------------------------------------------------------------
;; One motion per new key. The first key a node shows makes no motion, so a
;; page that opens does not move.

#?(:cljs (defonce ^:private !last-key (js/WeakMap.)))

(defn on-new-key!
  "Run `(motion! node)` when `node` shows a key it did not show last time.
   Post: the first call for a node runs nothing. Returns nil."
  [node key motion!]
  #?(:cljs
     (when node
       (let [had? (.has !last-key node)
             prev (.get !last-key node)]
         (.set !last-key node key)
         (when (and had? (not= prev key))
           (motion! node))
         nil))
     :clj nil))

(defn arrive-forward!
  "The next item of a queue comes in from the reading direction's end."
  [node]
  #?(:cljs
     (let [rtl? (= "rtl" (.-direction (js/getComputedStyle node)))]
       (play! node
         [{:transform (str "translateX(" (if rtl? "-" "") "24px)") :opacity 0}
          {:transform "translateX(0)" :opacity 1}]
         {:duration 150 :easing "ease-out"}))
     :clj nil))

(defn open-down!
  "A section that a click revealed opens downward from what is above it."
  [node]
  #?(:cljs
     (play! node
       [{:transform "translateY(-6px)" :opacity 0}
        {:transform "translateY(0)" :opacity 1}]
       {:duration 160 :easing "ease-out"})
     :clj nil))

(defn rise-in!
  "A result banner rises into its place under the answer."
  [node]
  #?(:cljs
     (play! node
       [{:transform "translateY(8px)" :opacity 0}
        {:transform "translateY(0)" :opacity 1}]
       {:duration 200 :easing "ease-out"})
     :clj nil))

;; ---------------------------------------------------------------------------
;; A count flashes once when it changed soon after this tab's own input. No
;; server channel records who bumped it, and every tab of a user shares the
;; channels. So "this tab's user acted just before" stands in for "this tab's
;; action changed it". A sync in another tab while this one is idle flashes
;; nothing.

(def own-change-window-ms
  "How long after a click or key in this tab a count change still counts as
   the user's own."
  5000)

#?(:cljs
   (defonce ^:private !last-input-ms
     (let [!ms (atom 0)
           note! (fn [_] (reset! !ms (js/Date.now)))]
       (.addEventListener js/document "pointerdown" note! #js {:capture true :passive true})
       (.addEventListener js/document "keydown" note! #js {:capture true :passive true})
       !ms)))

#?(:cljs
   (defn- flash-color []
     (let [v (.trim (.getPropertyValue (js/getComputedStyle js/document.documentElement)
                      "--color-primary-bg"))]
       (if (seq v) v "rgba(255, 214, 102, 0.55)"))))

#?(:cljs
   (defn- flash! [^js node]
     (if (prefers-reduced-motion?)
       (.animate node
         #js [#js {:outline "2px solid var(--color-primary)" :offset 0}
              #js {:outline "2px solid var(--color-primary)" :offset 0.99}
              #js {:outline "none"}]
         #js {:duration 1200})
       (.animate node
         #js [#js {:backgroundColor (flash-color) :offset 0}]
         #js {:duration 900 :easing "ease-out"}))))

#?(:cljs (defonce ^:private !last-count (js/WeakMap.)))

(defn flash-own-change!
  "Flash `node` once when `value` changed inside the same `scope`, and this
   tab had a click or key in the last `own-change-window-ms`.
   `scope` is what the count is OF: a topic, a user. A new scope shows a new
   count, it does not change one, so it flashes nothing.
   Post: the first value a node shows never flashes. Returns nil."
  [node scope value]
  #?(:cljs
     (when node
       (let [[prev-scope prev-value :as prev] (.get !last-count node)]
         (.set !last-count node [scope value])
         (when (and prev (= prev-scope scope) (not= prev-value value)
                 (< (- (js/Date.now) @!last-input-ms) own-change-window-ms))
           (flash! node))
         nil))
     :clj nil))

(defn flash-box!
  "Flash a box of the viewport once, over whatever is there.
   Pre:  `box` is {:left :top :width :height} in viewport px.
   Post: a fixed overlay on document.body fades out and removes itself.
         Under reduced motion it is a still outline for the same time.
         Returns nil."
  [{:keys [left top width height]}]
  #?(:cljs
     ;; document.body is the one parent Electric adds to and removes from by
     ;; identity, so a foreign node there cannot shift Electric's children.
     (let [el (js/document.createElement "div")
           s (.-style el)]
       (set! (.-position s) "fixed")
       (set! (.-left s) (str left "px"))
       (set! (.-top s) (str top "px"))
       (set! (.-width s) (str width "px"))
       (set! (.-height s) (str height "px"))
       (set! (.-pointerEvents s) "none")
       (set! (.-zIndex s) "2")
       (set! (.-borderRadius s) "var(--radius-sm)")
       (.setAttribute el "aria-hidden" "true")
       (.appendChild js/document.body el)
       (let [^js anim (flash! el)]
         (set! (.-onfinish anim) #(.remove el))
         (set! (.-oncancel anim) #(.remove el)))
       nil)
     :clj nil))

(defn held?
  "True while `press-then!` holds `button`."
  [button]
  #?(:cljs (boolean (some-> ^js button (.hasAttribute "data-press-hold")))
     :clj false))

(def press-hold-ms
  "How long a pressed button shows its done look before the queue moves on."
  180)

(defn press-then!
  "Show `button` pressed for `press-hold-ms`, then run `then!`.
   Pre:  `button` is a `.done-toggle` button that Electric renders with
         aria-pressed \"false\", or nil.
   Post: `then!` runs once. While the button is held, `held?` is true, and
         its click handler must ignore clicks. aria-pressed is \"false\"
         again before `then!` runs, so a button that stays mounted for the
         next item is not left pressed. With no button, or under reduced
         motion, `then!` runs at once. Returns nil."
  [button then!]
  #?(:cljs
     (if (or (nil? button) (prefers-reduced-motion?))
       (then!)
       (do (.setAttribute ^js button "aria-pressed" "true")
           (.setAttribute ^js button "data-press-hold" "1")
           (play! button [{:transform "scale(1)"} {:transform "scale(1.06)"} {:transform "scale(1)"}]
             {:duration press-hold-ms :easing "ease-out"})
           (js/setTimeout (fn []
                            (.setAttribute ^js button "aria-pressed" "false")
                            (.removeAttribute ^js button "data-press-hold")
                            (then!))
             press-hold-ms)))
     :clj (then!))
  nil)
