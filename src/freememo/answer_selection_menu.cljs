(ns freememo.answer-selection-menu
  "The menu a selection inside a settled assistant answer opens, and the
   highlights of the panel's annotations.

   The menu offers Generate cards, Ask aside and Annotate. Annotate turns the
   menu into a note field with ✓. Like freememo.format-menu it is plain DOM on
   `document.body` with `position: fixed`: the panel's ancestors clip their
   overflow, and Electric owns no node here. freememo.selection-placement
   places it so it never covers the selection, with the transcript as the
   container.

   Three facts about the answer DOM decide the shape, all measured on
   2026-10-02 (plans/assistant-answer-selection-actions.md §2.1):
   - A streaming answer's leaf is rewritten about every 100 ms, so a range in
     it collapses. The menu therefore opens only on a settled answer, which the
     answer wrapper's `data-run-status` says.
   - A settled leaf is not rewritten by later sends. An annotation can
     therefore keep text OFFSETS into its leaf, and the highlight finds the same
     text again whenever the leaf is rendered.
   - `Selection.toString()` across a formula is KaTeX's MathML and glyphs, one
     character per line. The quote is read from a clone through
     `math/katex->tex!` instead.

   Highlights use the CSS Custom Highlight API, so they change no node of a leaf
   that `math/set-html!` owns. A browser without the API shows none."
  (:require [clojure.string :as str]
            [freememo.math :as math]
            [freememo.selection-placement :as sel-place]))

(def ^:private highlight-name
  "The CSS highlight name; index.css styles `::highlight(assistant-annotation)`."
  "assistant-annotation")

(def ^:private unsettled-statuses
  "Run statuses whose answer leaf is still being rewritten."
  #{"queued" "streaming" "stop-requested"})

(def ^:private leaf-selector ".assistant-msg__rich-text")
(def ^:private answer-selector ".assistant-msg--assistant")

(defn- element-of [^js node]
  (when node (if (= 1 (.-nodeType node)) node (.-parentElement node))))

(defn- text-offset
  "The number of text characters in `leaf` before the point (node, offset).
   It counts what `Range.toString` counts: every Text node, KaTeX's hidden
   MathML included, so `range-at` can invert it exactly."
  [^js leaf ^js node offset]
  (let [^js r (.createRange js/document)]
    (.setStart r leaf 0)
    (.setEnd r node offset)
    (count (.toString r))))

(defn- range-at
  "A Range over the text characters [start, end) of `leaf`, or nil when the leaf
   holds fewer characters than `end`."
  [^js leaf start end]
  (let [^js walker (.createTreeWalker js/document leaf js/NodeFilter.SHOW_TEXT)
        ^js r (.createRange js/document)]
    (loop [seen 0 started? false]
      (when-let [^js node (.nextNode walker)]
        (let [next-seen (+ seen (.-length node))
              started? (or started?
                         (when (< start next-seen)
                           (.setStart r node (- start seen))
                           true))]
          (if (and started? (<= end next-seen))
            (do (.setEnd r node (- end seen)) r)
            (recur next-seen started?)))))))

(defn- capture
  "The live selection, when both its ends lie in ONE settled answer of
   `transcript`, else nil.
   Post: {:message-id :html :quote :start :end :range}. :html and :quote hold
         formulas as `\\(TeX\\)`. :start and :end are `text-offset`s into the
         leaf. :range is a copy of the live range, for placement."
  [^js transcript]
  (let [^js sel (.getSelection js/window)]
    (when (and sel (pos? (.-rangeCount sel)) (not (.-isCollapsed sel)))
      (let [leaf (some-> (element-of (.-anchorNode sel)) (.closest leaf-selector))
            focus-leaf (some-> (element-of (.-focusNode sel)) (.closest leaf-selector))
            ^js answer (some-> leaf (.closest answer-selector))]
        (when (and leaf (identical? leaf focus-leaf) answer (.contains transcript leaf)
                (not (unsettled-statuses (.getAttribute answer "data-run-status"))))
          (let [^js rng (.getRangeAt sel 0)
                holder (.createElement js/document "div")]
            (.appendChild holder (.cloneContents rng))
            (math/katex->tex! holder)
            (let [quoted (-> (.-textContent holder) (str/replace #"\s+" " ") str/trim)
                  start (text-offset leaf (.-startContainer rng) (.-startOffset rng))]
              (when-not (str/blank? quoted)
                {:message-id (js/parseInt (.getAttribute answer "data-message-id") 10)
                 :html (.-innerHTML holder)
                 :quote quoted
                 :start start
                 :end (+ start (count (.toString rng)))
                 :range (.cloneRange rng)}))))))))

(defn- el [tag class-name]
  (let [node (.createElement js/document tag)]
    (set! (.-className node) class-name)
    node))

(defn- action-button [label title]
  (let [b (el "button" "btn btn-sm btn-secondary answer-selection-menu__action")]
    (set! (.-type b) "button")
    (set! (.-textContent b) label)
    (.setAttribute b "title" title)
    ;; Keep the selection: a mousedown on a button would otherwise collapse it
    ;; before the click reads it.
    (.addEventListener b "mousedown" #(.preventDefault %))
    b))

(defn- build-menu []
  (let [root (el "div" "answer-selection-menu")
        actions (el "div" "answer-selection-menu__actions")
        generate (action-button "Generate cards" "Make cards from the selected text")
        ask (action-button "Ask aside" "Ask a side question about the selected text")
        annotate (action-button "Annotate" "Mark this text and add a note to your next message")
        note (el "div" "answer-selection-menu__note")
        input (el "textarea" "input answer-selection-menu__input")
        save (el "button" "btn btn-sm btn-primary")
        cancel (el "button" "btn btn-sm btn-secondary")]
    (.setAttribute root "role" "toolbar")
    (.setAttribute root "aria-label" "Selection actions")
    (set! (.-rows input) 2)
    (.setAttribute input "placeholder" "Your question about this text (optional)")
    (.setAttribute input "aria-label" "Annotation note")
    (set! (.-type save) "button")
    (set! (.-textContent save) "✓")
    (.setAttribute save "title" "Save annotation")
    (.setAttribute save "aria-label" "Save annotation")
    (set! (.-type cancel) "button")
    (set! (.-textContent cancel) "✕")
    (.setAttribute cancel "title" "Cancel")
    (.setAttribute cancel "aria-label" "Cancel")
    (doseq [c [generate ask annotate]] (.appendChild actions c))
    (doseq [c [input save cancel]] (.appendChild note c))
    (.appendChild root actions)
    (.appendChild root note)
    {:root root :actions actions :note note :input input :save save :cancel cancel
     :generate generate :ask ask :annotate annotate}))

(defn- place!
  "Position `root` where it covers no part of the captured range, or hide it
   when no tier fits. Inv: the applied rect never intersects the range's
   visible rows inside `transcript`."
  [^js root ^js transcript ^js rng]
  (set! (.. root -style -display) "block")
  (let [^js box (.getBoundingClientRect rng)
        ^js con (.getBoundingClientRect transcript)
        leaf (some-> (element-of (.-startContainer rng)) (.closest leaf-selector))
        placement (when (pos? (.-height box))
                    (sel-place/place
                      {:selection {:top (.-top box) :bottom (.-bottom box)}
                       :focus-x (.-left box)
                       :rtl? (boolean (and leaf (= "rtl" (.-direction (js/getComputedStyle leaf)))))
                       :container {:top (.-top con) :bottom (.-bottom con)}
                       :viewport {:width (.-innerWidth js/window)
                                  :height (.-innerHeight js/window)}
                       :menu {:width (.-offsetWidth root) :height (.-offsetHeight root)}}))]
    (if placement
      (do (set! (.. root -style -left) (str (js/Math.round (:left placement)) "px"))
          (set! (.. root -style -top) (str (js/Math.round (:top placement)) "px")))
      (set! (.. root -style -display) "none"))))

(defonce ^:private painted
  ;; transcript node → the annotations last painted in it, so a leaf that
  ;; renders again can be highlighted again.
  (js/WeakMap.))

(defn paint!
  "Highlight `annotations` in `transcript`, replacing the previous highlight.
   Pre:  each annotation has :message-id, :start and :end from `capture`.
   Post: the highlight covers each annotation whose answer is in the
         transcript. Returns nil."
  [^js transcript annotations]
  (.set painted transcript annotations)
  (when-let [^js registry (some-> js/CSS .-highlights)]
    (let [ranges (keep (fn [{:keys [message-id start end]}]
                         (when-let [leaf (.querySelector transcript
                                           (str answer-selector "[data-message-id=\""
                                             message-id "\"] " leaf-selector))]
                           (range-at leaf start end)))
                   annotations)]
      (if (seq ranges)
        (let [^js highlight (js/Highlight.)]
          (doseq [r ranges] (.add highlight r))
          (.set registry highlight-name highlight))
        (.delete registry highlight-name))))
  nil)

(defn install!
  "Open the selection menu for settled answers in `transcript`.

   `handlers` = {:on-generate (fn [capture]) :on-ask (fn [capture])
                 :on-annotate (fn [capture note])}, where `capture` is
   {:message-id :html :quote :start :end} and `note` is a string, possibly blank.

   Post: a mouse or keyboard selection inside one settled answer shows the
         menu. A collapsed selection, an outside mousedown, or Escape hides it.
         A scroll or resize re-places it. Returns a teardown fn that removes
         every listener, the menu node and the highlights."
  [^js transcript {:keys [on-generate on-ask on-annotate]}]
  (let [{:keys [root actions note input save cancel generate ask annotate]} (build-menu)
        !capture (atom nil)
        ;; nil while hidden, :actions for the three buttons, :note for the field.
        !mode (atom nil)
        hide! (fn []
                (reset! !mode nil)
                (reset! !capture nil)
                (set! (.-value input) "")
                (set! (.. root -style -display) "none"))
        place-now! #(when-let [c @!capture] (place! root transcript (:range c)))
        show-actions! (fn [c]
                        (reset! !capture c)
                        (reset! !mode :actions)
                        (set! (.. actions -style -display) "flex")
                        (set! (.. note -style -display) "none")
                        (place-now!))
        take! (fn [] (let [c (dissoc @!capture :range)]
                       (hide!)
                       (some-> (.getSelection js/window) .removeAllRanges)
                       c))
        save-note! (fn [] (when @!capture
                            (let [text (.-value input)
                                  c (take!)]
                              (on-annotate c text))))
        on-up (fn [_]
                ;; After the browser finishes the selection.
                (js/setTimeout
                  (fn []
                    (when-not (= :note @!mode)
                      (if-let [c (capture transcript)]
                        (show-actions! c)
                        (hide!))))
                  0))
        on-selection-change (fn [_]
                              (when (= :actions @!mode)
                                (let [^js sel (.getSelection js/window)]
                                  (when (or (nil? sel) (.-isCollapsed sel))
                                    (hide!)))))
        on-doc-down (fn [^js e]
                      (when (and @!mode (not (.contains root (.-target e))))
                        (hide!)))
        on-key (fn [^js e]
                 (when (and @!mode (= "Escape" (.-key e)))
                   (hide!)))
        reposition (fn [_] (when @!mode (place-now!)))
        repaint (fn [_] (when-let [items (.get painted transcript)]
                          (paint! transcript items)))]
    (.addEventListener generate "click" (fn [_] (when @!capture (on-generate (take!)))))
    (.addEventListener ask "click" (fn [_] (when @!capture (on-ask (take!)))))
    (.addEventListener annotate "click"
      (fn [_]
        (when @!capture
          (reset! !mode :note)
          (set! (.. actions -style -display) "none")
          (set! (.. note -style -display) "flex")
          (place-now!)
          (.focus input))))
    (.addEventListener save "click" (fn [_] (save-note!)))
    (.addEventListener cancel "click" (fn [_] (hide!)))
    (.addEventListener input "keydown"
      (fn [^js e]
        (when (and (= "Enter" (.-key e)) (not (.-shiftKey e)))
          (.preventDefault e)
          (save-note!))))
    (set! (.. root -style -display) "none")
    (.appendChild js/document.body root)
    (.addEventListener transcript "mouseup" on-up)
    (.addEventListener transcript "keyup" on-up)
    (.addEventListener transcript "scroll" reposition)
    (.addEventListener transcript "assistant-answer-rendered" repaint)
    (.addEventListener js/document "selectionchange" on-selection-change)
    (.addEventListener js/document "mousedown" on-doc-down true)
    (.addEventListener js/document "keydown" on-key)
    (.addEventListener js/window "resize" reposition)
    (fn teardown []
      (.removeEventListener transcript "mouseup" on-up)
      (.removeEventListener transcript "keyup" on-up)
      (.removeEventListener transcript "scroll" reposition)
      (.removeEventListener transcript "assistant-answer-rendered" repaint)
      (.removeEventListener js/document "selectionchange" on-selection-change)
      (.removeEventListener js/document "mousedown" on-doc-down true)
      (.removeEventListener js/document "keydown" on-key)
      (.removeEventListener js/window "resize" reposition)
      (.delete painted transcript)
      (some-> js/CSS .-highlights (.delete highlight-name))
      (when (.-parentNode root) (.remove root)))))
