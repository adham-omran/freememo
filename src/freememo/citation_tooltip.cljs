(ns freememo.citation-tooltip
  "The bubble that shows a citation pill's full label on hover and on focus.

   Plain DOM on `document.body` with `position: fixed`, like
   freememo.answer-selection-menu, because the assistant panel's ancestors clip
   their overflow. A `[data-tooltip]` bubble is an `::after` inside its element,
   so one on the last visible line of the transcript would be cut off
   (plans/assistant-citation-pills.md §5.3).

   One bubble serves every pill. It reads the label from the pill's
   `aria-label`, which `assistant-evidence/decorate-citations!` sets, so the
   bubble needs no evidence lookup of its own. It is `aria-hidden`: the
   aria-label already names the pill for assistive tech.")

(def ^:private gap
  "Pixels between the pill and the bubble, as `[data-tooltip]` uses."
  4)

(def ^:private edge
  "Pixels the bubble keeps clear of the viewport edges."
  8)

(defonce ^:private !bubble (atom nil))

(defn hide!
  "Hide the bubble. Post: no bubble is visible. Returns nil."
  []
  (some-> @!bubble (.setAttribute "hidden" ""))
  nil)

(defn- bubble!
  "The bubble element, created on first use with a listener that hides it on
   any scroll: a fixed bubble would otherwise stay behind while its pill moves."
  []
  (or @!bubble
    (let [el (.createElement js/document "div")]
      (set! (.-className el) "citation-tooltip")
      (.setAttribute el "aria-hidden" "true")
      (.setAttribute el "hidden" "")
      (.appendChild (.-body js/document) el)
      (.addEventListener js/document "scroll" hide! #js {:capture true :passive true})
      (reset! !bubble el))))

(defn- hover-device?
  "True when the primary pointer can hover. A touch tap latches `:hover` and
   opens the evidence dialog, which already names the source."
  []
  (not (.-matches (js/matchMedia "(hover: none)"))))

(defn show!
  "Show the label of `pill` below it, or above it when the viewport has no room
   below, clamped to the viewport edges.
   Pre:  `pill` is a decorated citation link in the document.
   Post: the bubble shows the pill's aria-label next to it, or no bubble is
         visible when the pill has no label or the device cannot hover.
         Returns nil."
  [pill]
  (let [label (.getAttribute pill "aria-label")]
    (if (or (empty? label) (not (hover-device?)))
      (hide!)
      (let [el (bubble!)
            rect (.getBoundingClientRect pill)]
        (set! (.-textContent el) label)
        ;; Measure at the left edge: a fixed box's width shrinks to the room
        ;; right of its last position, which would skew the clamp below.
        (set! (.. el -style -left) "0px")
        (.removeAttribute el "hidden")
        (let [w (.-offsetWidth el)
              h (.-offsetHeight el)
              vw (.-innerWidth js/window)
              vh (.-innerHeight js/window)
              below (+ (.-bottom rect) gap)
              top (if (<= (+ below h) (- vh edge))
                    below
                    (max edge (- (.-top rect) gap h)))
              centered (- (+ (.-left rect) (/ (.-width rect) 2)) (/ w 2))
              left (-> centered (min (- vw w edge)) (max edge))]
          (set! (.. el -style -top) (str top "px"))
          (set! (.. el -style -left) (str left "px"))
          nil)))))
