(ns freememo.question-pin-dom
  "The plain-DOM half of question pins on rendered Markdown and on code
   (plans/anchored-questions.md §6.2, §6.5; plans/commit-range-walkthrough.md
   §6.3.7).

   Electric owns the rendered pane's `div` but none of its children: the code
   pane writes them with `innerHTML`, and a block's index among them is half of
   every location the server reports (`quote-anchor/locate`). So nothing here
   ever adds a node inside the pane. Tints are CSS Custom Highlights, and the
   label badges, the virtual finger and the Ask button live in one layer
   appended to `document.body`, positioned from range rects.

   Two text models, one per surface:
     :blocks  rendered Markdown. A location is {:start [block offset]
              :end [block offset]}. `offset` counts the characters of the
              block's text nodes, as the server's Jsoup parse of the same HTML
              did.
     :source  code. Every text node under the container, in order, is the
              file's source exactly, so a location is {:offsets [start end]},
              character offsets into the whole source. A folded run keeps its
              text nodes, hidden, so folding moves no offset. KaTeX rewrites a formula after the HTML lands, so a typeset
   `.katex` or `.katex-display` counts as its TeX plus its two delimiters — the
   text the server saw — and a range never starts or ends inside one.

   `install!` is the only entry point. It returns a handle; every other fn
   takes that handle. A plain defn, never an Electric body, owns the state
   (CLAUDE.md, \"JS Library Init Side Effects in e/defn\")."
  (:require [clojure.string :as str]
            [freememo.line-bands :as line-bands]
            [freememo.selection-placement :as sel-place]))

;; ── Text model ─────────────────────────────────────────────────────

(defn- tex-of [^js el]
  (or (some-> (.querySelector el "annotation[encoding=\"application/x-tex\"]") .-textContent)
    ""))

(defn- formula?
  "A typeset formula counted as one unit. A `.katex` inside a `.katex-display`
   is reached as part of the display, never on its own."
  [^js el]
  (let [cl (.-classList el)]
    (or (.contains cl "katex-display")
      (and (.contains cl "katex")
        (not (some-> (.-parentElement el) (.closest ".katex-display")))))))

(defn- segments
  "The text units of `block` in document order: {:text node :len n} for a text
   node, {:formula el :len n} for a typeset formula."
  [^js block]
  (let [out (array)]
    (letfn [(walk [^js n]
              (cond
                (= 3 (.-nodeType n)) (.push out {:text n :len (.-length n)})
                (and (= 1 (.-nodeType n)) (formula? n))
                (.push out {:formula n :len (+ 4 (count (tex-of n)))})
                :else (doseq [c (array-seq (.-childNodes n))] (walk c))))]
      (walk block))
    (vec out)))

(defn- set-boundary!
  "Put one end of `rng` at character `offset` of `block`. A point inside a
   formula moves to the formula's edge: before it for a start, after it for an
   end. Post: true when the block was long enough."
  [^js rng ^js block offset start?]
  (loop [[seg & more] (segments block) acc 0]
    (if-not seg
      false
      (let [end (+ acc (:len seg))]
        (if (or (< offset end) (and (= offset end) (not start?)))
          (do (if-let [t (:text seg)]
                (if start? (.setStart rng t (- offset acc)) (.setEnd rng t (- offset acc)))
                (if start? (.setStartBefore rng (:formula seg)) (.setEndAfter rng (:formula seg))))
              true)
          (recur more end))))))

(declare block-range)

(defn- text-nodes
  "Every text node under `container`, in document order."
  [^js container]
  (let [walker (.createTreeWalker js/document container 4)
        out (array)]
    (loop []
      (when-let [n (.nextNode walker)]
        (.push out n)
        (recur)))
    (vec out)))

(defn- source-point
  "[node offset-in-node] for character `offset` of `container`'s source. An
   end exactly at a node boundary stays in the earlier node."
  [^js container offset end?]
  (loop [[n & more] (text-nodes container) acc 0]
    (when n
      (let [len (.-length n)]
        (if (or (< offset (+ acc len)) (and end? (= offset (+ acc len))))
          [n (- offset acc)]
          (recur more (+ acc len)))))))

(defn- source-range [^js container [s e]]
  (let [a (source-point container s false)
        b (source-point container e true)]
    (when (and a b)
      (doto (.createRange js/document)
        (.setStart (first a) (second a))
        (.setEnd (first b) (second b))))))

(defn range-for
  "A DOM Range over `location` in `container`, or nil when the content it
   names is not there (the pane re-rendered with other content)."
  [^js container location]
  (if-let [offsets (:offsets location)]
    (source-range container offsets)
    (block-range container location)))

(defn- block-range
  [^js container {[sb so] :start [eb eo] :end}]
  (let [blocks (.-children container)
        b1 (aget blocks sb)
        b2 (aget blocks eb)]
    (when (and b1 b2)
      (let [rng (.createRange js/document)]
        (when (and (set-boundary! rng b1 so true) (set-boundary! rng b2 eo false))
          rng)))))

;; ── Capturing a selection ──────────────────────────────────────────

(defn- block-of [^js container ^js node]
  (loop [n (if (= 1 (.-nodeType node)) node (.-parentElement node))]
    (cond (nil? n) nil
          (identical? (.-parentElement n) container) n
          :else (recur (.-parentElement n)))))

(defn- heading-anchor
  "The id of the nearest heading at or above `block`, or nil."
  [^js block]
  (loop [el block]
    (cond (nil? el) nil
          (and (re-matches #"H[1-6]" (.-tagName el)) (seq (.-id el))) (.-id el)
          :else (recur (.-previousElementSibling el)))))

(defn- formulas->tex!
  "Each typeset formula under detached `holder` becomes its TeX text, with the
   delimiters the stored Markdown uses: `\\[…\\]` for display math, `\\(…\\)`
   inline. The server matches a quote against that stored text, so the
   delimiters have to agree. A formula cut by the selection edge has no
   annotation and is dropped."
  [^js holder]
  (doseq [el (js/Array.from (.querySelectorAll holder ".katex-display, .katex"))]
    (when (.contains holder el)
      (let [tex (some-> (.querySelector el "annotation[encoding=\"application/x-tex\"]")
                  .-textContent)
            display? (.contains (.-classList el) "katex-display")]
        (if tex
          (.replaceWith el (.createTextNode js/document
                             (if display? (str "\\[" tex "\\]") (str "\\(" tex "\\)"))))
          (.remove el))))))

(defn- live-range
  "The selection's Range when both ends are inside `container`, else nil."
  [^js container]
  (let [sel (.getSelection js/window)]
    (when (and sel (pos? (.-rangeCount sel)) (not (.-isCollapsed sel)))
      (let [rng (.getRangeAt sel 0)]
        (when (and (.contains container (.-startContainer rng))
                (.contains container (.-endContainer rng)))
          rng)))))

(defn- source-offsets [^js container ^js rng]
  (let [nodes (text-nodes container)
        index-of (fn [^js node offset]
                   (if (= 3 (.-nodeType node))
                     (loop [[n & more] nodes acc 0]
                       (cond (nil? n) nil
                             (identical? n node) (+ acc offset)
                             :else (recur more (+ acc (.-length n)))))
                     ;; An element point: count the text before its offset-th child.
                     (let [child (aget (.-childNodes node) offset)
                           before (.createRange js/document)]
                       (.setStart before container 0)
                       (if child (.setEndBefore before child) (.setEndAfter before node))
                       (count (.toString before)))))]
    [(index-of (.-startContainer rng) (.-startOffset rng))
     (index-of (.-endContainer rng) (.-endOffset rng))]))

(defn- capture-source
  "The passage selected in code `container`: its exact source text and its
   start offset, the hint that picks among repeated quotes (§6.3.7)."
  [^js container ^js rng]
  (let [[s e] (source-offsets container rng)
        text (apply str (map #(.-data %) (text-nodes container)))
        box (.getBoundingClientRect rng)]
    (when (and s e (< s e))
      {:quote (subs text s e) :offset s
       :rect {:top (.-top box) :bottom (.-bottom box) :left (.-left box)
              :right (.-right box)}})))

(declare capture-blocks)

(defn capture
  "The passage the reader selected in `container`, or nil.
   Post, :blocks: {:quote :anchor :rect}. `:quote` is the selected text with
         formulas as TeX and whitespace squeezed. `:anchor` is the id of the
         heading above the selection's start.
   Post, :source: {:quote :offset :rect}. `:quote` is the exact source text.
   `:rect` is the selection's bounding box."
  [^js container model]
  (when-let [rng (live-range container)]
    (if (= :source model)
      (capture-source container rng)
      (capture-blocks container rng))))

(defn- capture-blocks
  [^js container ^js rng]
  (do
    (let [holder (.createElement js/document "div")]
      (.appendChild holder (.cloneContents rng))
      (formulas->tex! holder)
      (let [quote (str/trim (str/replace (.-textContent holder) #"\s+" " "))
            box (.getBoundingClientRect rng)]
        (when (seq quote)
          {:quote quote
           :anchor (some-> (block-of container (.-startContainer rng)) heading-anchor)
           :rect {:top (.-top box) :bottom (.-bottom box) :left (.-left box)
                  :right (.-right box)}})))))

;; ── The layer ──────────────────────────────────────────────────────

(def ^:private highlight-name "question-pin")

(defn- el [tag cls]
  (doto (.createElement js/document tag) (.setAttribute "class" cls)))

(defn- clear! [^js node] (set! (.-innerHTML node) ""))

(defn- visible-rects
  "The client rects of `rng` that fall inside `container`'s box."
  [^js rng ^js container]
  (let [^js c (.getBoundingClientRect container)]
    (filterv (fn [^js r] (and (pos? (.-width r)) (> (.-bottom r) (.-top c))
                           (< (.-top r) (.-bottom c))))
      (array-seq (.getClientRects rng)))))

(defn- paint-pins!
  "Tint every anchored open pin and put its label at the start of its range."
  [{:keys [^js container ^js badges !state]}]
  (let [{:keys [pins on-open-pin]} @!state
        ranges (keep (fn [pin] (when-let [r (some->> (:range pin) (range-for container))]
                                 [pin r]))
                 (filter #(= "open" (:status %)) pins))]
    (clear! badges)
    (when (exists? js/Highlight)
      (if (seq ranges)
        (.set (.-highlights js/CSS) highlight-name
          (let [h (js/Highlight.)] (doseq [[_ r] ranges] (.add h r)) h))
        (.delete (.-highlights js/CSS) highlight-name)))
    ;; Two pins can start at one point, and a badge drawn over another hides
    ;; it and takes its clicks. Each badge at a taken point moves one badge
    ;; width to the right.
    (let [taken (volatile! #{})]
     (doseq [[pin rng] ranges]
      (when-let [^js r (first (visible-rects rng container))]
        (let [b (el "button" "question-pin-badge")
              x (loop [x (js/Math.round (- (.-left r) 10))]
                  (if (contains? @taken [x (js/Math.round (.-top r))]) (recur (+ x 20)) x))]
          (vswap! taken conj [x (js/Math.round (.-top r))])
          (set! (.-type b) "button")
          (set! (.-textContent b) (str (:label pin)))
          (.setAttribute b "aria-label" (str "Pin " (:label pin)
                                          (when (seq (:question pin)) (str ": " (:question pin)))))
          (set! (.. b -style -left) (str x "px"))
          (set! (.. b -style -top) (str (js/Math.round (- (.-top r) 10)) "px"))
          (.addEventListener b "mousedown" (fn [e] (.preventDefault e)))
          (.addEventListener b "click"
            (fn [_] (when on-open-pin
                      (on-open-pin pin {:top (.-top r) :bottom (.-bottom r)
                                        :left (.-left r) :right (.-right r)}))))
          (.appendChild badges b)))))))

(defn- finger-bands
  "One rect per visual line of `rects`, as maps. Outlining the raw rects draws
   a box around every inline fragment, and two nested ones around a `<code>`
   run (`freememo.line-bands`)."
  [rects]
  (line-bands/bands
    (map (fn [^js r] {:left (.-left r) :top (.-top r)
                      :right (.-right r) :bottom (.-bottom r)})
      rects)))

(defn- paint-finger!
  "Outline the finger's range where it is visible inside the pane, and point
   at it from the pane's edge when it is not (§6.5.1, §6.5.2)."
  [{:keys [^js container ^js finger !state]}]
  (clear! finger)
  (when-let [rng (some->> (:finger @!state) (range-for container))]
    (let [^js c (.getBoundingClientRect container)
          ^js box (.getBoundingClientRect rng)
          rects (visible-rects rng container)]
      (if (seq rects)
        (doseq [{:keys [left top right bottom]} (finger-bands rects)]
          (let [o (el "div" "question-finger")]
            (set! (.. o -style -left) (str (- left 2) "px"))
            (set! (.. o -style -top) (str (- top 2) "px"))
            (set! (.. o -style -width) (str (+ (- right left) 4) "px"))
            (set! (.. o -style -height) (str (+ (- bottom top) 4) "px"))
            (.appendChild finger o)))
        (let [below? (>= (.-top box) (.-bottom c))
              m (el "div" (str "question-finger-edge question-finger-edge--"
                            (if below? "below" "above")))]
          (set! (.-textContent m) (if below? "↓" "↑"))
          (set! (.. m -style -left) (str (+ (.-left c) (/ (.-width c) 2) -12) "px"))
          (set! (.. m -style -top) (str (if below? (- (.-bottom c) 28) (+ (.-top c) 4)) "px"))
          (.appendChild finger m))))))

(defn- place-ask!
  "Show the Ask button beside the live selection, or hide it."
  [{:keys [^js container ^js ask]}]
  (if-let [rng (live-range container)]
    (let [^js box (.getBoundingClientRect rng)
          ^js con (.getBoundingClientRect container)]
      (set! (.. ask -style -display) "block")
      (if-let [p (sel-place/place
                   {:selection {:top (.-top box) :bottom (.-bottom box)}
                    :focus-x (.-right box) :rtl? false
                    :container {:top (.-top con) :bottom (.-bottom con)}
                    :viewport {:width (.-innerWidth js/window) :height (.-innerHeight js/window)}
                    :menu {:width (.-offsetWidth ask) :height (.-offsetHeight ask)}})]
        (do (set! (.. ask -style -left) (str (js/Math.round (:left p)) "px"))
            (set! (.. ask -style -top) (str (js/Math.round (:top p)) "px")))
        (set! (.. ask -style -display) "none")))
    (set! (.. ask -style -display) "none")))

(defn- repaint! [handle]
  (paint-pins! handle)
  (paint-finger! handle))

(defn ask-current!
  "Open the popover for the live selection, as the Ask button does. Post: true
   when there was a selection to ask about."
  [{:keys [container ask !state]}]
  (if-let [c (capture container (:model @!state))]
    (do (set! (.. ask -style -display) "none")
        (when-let [f (:on-ask @!state)] (f c))
        true)
    false))

(defn install!
  "Attach pins, the finger and the Ask button to `container`.
   `callbacks`: {:on-ask (fn [capture]) :on-open-pin (fn [pin rect])
   :model :blocks|:source}, `:model` being the container's text model.
   Post: a handle for `set-pins!`, `set-finger!`, `ask-current!` and
         `teardown!`. The layer repaints itself on scroll, resize, and any
         change to the pane's DOM — a new document, or KaTeX typesetting."
  [^js container callbacks]
  (let [layer (el "div" "question-pin-layer")
        badges (el "div" "question-pin-layer__badges")
        finger (el "div" "question-pin-layer__finger")
        ask (el "button" "btn btn-sm btn-primary question-pin-ask")
        !state (atom (merge callbacks {:pins [] :finger nil}))
        handle {:container container :layer layer :badges badges :finger finger
                :ask ask :!state !state}
        !frame (atom nil)
        schedule! (fn []
                    (when-not @!frame
                      (reset! !frame (js/requestAnimationFrame
                                       (fn [] (reset! !frame nil)
                                         (repaint! handle) (place-ask! handle))))))
        on-up (fn [_] (js/setTimeout #(place-ask! handle) 0))
        on-selection (fn [_] (when-not (live-range container)
                               (set! (.. ask -style -display) "none")))
        observer (js/MutationObserver. (fn [_ _] (schedule!)))]
    (set! (.-type ask) "button")
    (set! (.-textContent ask) "Ask")
    (.setAttribute ask "aria-label" "Ask about the selected passage")
    (set! (.. ask -style -display) "none")
    ;; The mousedown default would move focus and collapse the selection before
    ;; the click reads it (§6.2.2, CLAUDE.md "Modal Focus Management").
    (.addEventListener ask "mousedown" (fn [e] (.preventDefault e)))
    (.addEventListener ask "click" (fn [_] (ask-current! handle)))
    (.appendChild layer badges)
    (.appendChild layer finger)
    (.appendChild layer ask)
    (.appendChild (.-body js/document) layer)
    (.addEventListener container "mouseup" on-up)
    (.addEventListener container "keyup" on-up)
    (.addEventListener container "scroll" schedule!)
    (.addEventListener js/window "resize" schedule!)
    (.addEventListener js/document "selectionchange" on-selection)
    (.observe observer container #js {:childList true :subtree true :characterData true})
    (assoc handle
      :teardown! (fn []
                   (.disconnect observer)
                   (.removeEventListener container "mouseup" on-up)
                   (.removeEventListener container "keyup" on-up)
                   (.removeEventListener container "scroll" schedule!)
                   (.removeEventListener js/window "resize" schedule!)
                   (.removeEventListener js/document "selectionchange" on-selection)
                   (when (exists? js/Highlight) (.delete (.-highlights js/CSS) highlight-name))
                   (.remove layer))
      :schedule! schedule!)))

(defn set-pins!
  "Show `pins` ([{:id :label :question :status :range}]) in the pane."
  [{:keys [!state schedule!]} pins]
  (swap! !state assoc :pins (vec pins))
  (schedule!)
  nil)

(defn set-finger!
  "Point at `location` in the pane, or stop pointing when it is nil. With
   `scroll?`, also scroll the pane so the passage is in view (§6.5.4)."
  [{:keys [container !state schedule!]} location scroll?]
  (swap! !state assoc :finger location)
  (when-let [^js rng (when (and scroll? location) (range-for container location))]
    (let [^js node (.-startContainer rng)
          ^js target (if (= 1 (.-nodeType node)) node (.-parentElement node))]
      (.scrollIntoView target #js {:behavior "smooth" :block "center"})))
  (schedule!)
  nil)

(defn teardown! [{:keys [teardown!]}] (teardown!))
