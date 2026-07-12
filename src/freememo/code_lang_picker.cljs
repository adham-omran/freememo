(ns freememo.code-lang-picker
  "Custom language picker for Quill code blocks.

   Quill's syntax module renders a native `<select class=\"ql-ui\">` per code
   block; its open options list is browser chrome that renders detached from the
   control. We hide that select (CSS) and present our own trigger + popup, which
   drive the hidden select (set `value` + dispatch `change`) so Quill still
   applies and re-highlights the language — we own only the UI, not the logic.

   The trigger and popup live on `document.body`, NOT inside the code block.
   Injecting a node into `.ql-code-block-container` fights Quill's DOM
   reconciliation: Quill strips the foreign node, and any observer that
   re-injects it loops forever, freezing the page. Instead a single shared
   trigger is driven by `selection-change` — it appears anchored to the block
   the caret sits in, and hides when the caret leaves. Nothing is ever added to
   the editor's own subtree, so no mutation-feedback loop can form.

   install! wires the listeners and returns a teardown fn removing everything.")

(defn- el [tag class]
  (let [n (js/document.createElement tag)]
    (set! (.-className n) class)
    n))

(defn- ->vec
  "Materialize a NodeList/HTMLCollection into a Clojure vector."
  [coll]
  (vec (js/Array.from coll)))

(defn- native-select [^js container]
  (.querySelector container "select.ql-ui"))

(defn- languages
  "[[value label] …] from the native select's options — kept in sync with
   Quill's syntax config rather than hard-coded."
  [^js select]
  (mapv (fn [^js o] [(.-value o) (.-text o)]) (->vec (.-options select))))

(defn- current-label [^js select]
  (let [v (.-value select)]
    (or (some (fn [[val lbl]] (when (= val v) lbl)) (languages select)) v)))

(defn- container-of
  "The `.ql-code-block-container` enclosing the line at `index`, or nil when
   that line is not code. Resolves via the line blot's DOM node."
  [^js editor index]
  (let [line (aget (.getLine editor index) 0)]
    (when line
      (.closest (.-domNode line) ".ql-code-block-container"))))

(defn install!
  "Wire the custom code-language picker for `editor`. Returns a teardown fn.

   Pre:  `editor` is an initialized Quill instance whose native `.ql-ui`
         select is hidden by CSS.
   Post: a caret/selection inside a code block shows one shared trigger at the
         block's top-right; clicking it opens a viewport-clamped popup; picking
         sets the block's hidden select + dispatches `change` (Quill applies +
         re-highlights); leaving the block hides the trigger. Teardown removes
         the trigger, popup, and all listeners.
   Invariant: nothing is appended to the editor root subtree, so no mutation
         feedback loop can form."
  [^js editor]
  (let [root    (.-root editor)
        trigger (el "button" "ql-lang-trigger")
        popup   (el "div" "ql-lang-popup")
        !active (atom nil)                                ; {:container :select} the caret's code block
        popup-open? #(= "block" (.. popup -style -display))
        close-popup! (fn [] (set! (.. popup -style -display) "none"))
        place-trigger! (fn [^js container]
                         (let [r  (.getBoundingClientRect container)
                               m  8
                               vw (.-innerWidth js/window)
                               tw (.-offsetWidth trigger)
                               top  (max m (+ (.-top r) 6))
                               left (max m (min (- (.-right r) tw 8) (- vw tw m)))]
                           (set! (.. trigger -style -top) (str (js/Math.round top) "px"))
                           (set! (.. trigger -style -left) (str (js/Math.round left) "px"))))
        place-popup! (fn []
                       (let [r  (.getBoundingClientRect trigger)
                             m  8
                             vh (.-innerHeight js/window)
                             vw (.-innerWidth js/window)
                             _  (set! (.. popup -style -display) "block")
                             pw (.-offsetWidth popup)
                             ph (.-offsetHeight popup)
                             top  (if (<= (+ (.-bottom r) 4 ph) (- vh m))
                                    (+ (.-bottom r) 4)
                                    (max m (- (.-top r) 4 ph)))
                             left (max m (min (.-left r) (- vw pw m)))]
                         (set! (.. popup -style -top) (str (js/Math.round top) "px"))
                         (set! (.. popup -style -left) (str (js/Math.round left) "px"))))
        open-popup! (fn [^js select]
                      (set! (.-innerHTML popup) "")
                      (doseq [[value lbl] (languages select)]
                        (let [item (el "button" "ql-lang-item")]
                          (set! (.-type item) "button")
                          (set! (.-textContent item) lbl)
                          (when (= value (.-value select)) (.add (.-classList item) "active"))
                          (.addEventListener item "mousedown" (fn [^js e] (.preventDefault e)))
                          (.addEventListener item "click"
                            (fn [^js e]
                              (.stopPropagation e)
                              (set! (.-value select) value)
                              (.dispatchEvent select (js/Event. "change" #js {:bubbles true}))
                              (set! (.-textContent trigger) lbl)
                              (close-popup!)))
                          (.appendChild popup item)))
                      (place-popup!))
        show! (fn [^js container ^js select]
                (reset! !active {:container container :select select})
                (set! (.-textContent trigger) (current-label select))
                (set! (.. trigger -style -display) "block")
                (place-trigger! container))
        hide! (fn []
                (close-popup!)
                (set! (.. trigger -style -display) "none")
                (reset! !active nil))
        on-selection (fn [^js range _old _source]
                       ;; A real caret move stales any open popup; drop it, then
                       ;; re-anchor to the new block (or hide when leaving code).
                       (close-popup!)
                       (let [c (when range (container-of editor (.-index range)))
                             sel (when c (native-select c))]
                         (if sel (show! c sel) (hide!))))
        on-trigger-click (fn [^js e]
                           (.stopPropagation e)
                           (if (popup-open?)
                             (close-popup!)
                             (when-let [sel (:select @!active)] (open-popup! sel))))
        reposition (fn [_]
                     (when-let [container (:container @!active)]
                       (place-trigger! container)
                       (when (popup-open?) (place-popup!))))
        on-doc-down (fn [^js e]
                      (when (and (popup-open?)
                              (not (.contains popup (.-target e)))
                              (not (.contains trigger (.-target e))))
                        (close-popup!)))
        on-key (fn [^js e] (when (and (popup-open?) (= "Escape" (.-key e))) (close-popup!)))]
    (set! (.-type trigger) "button")
    (.setAttribute trigger "title" "Code language")
    (.setAttribute trigger "aria-label" "Code block language")
    ;; Keep the editor selection on click so selection-change doesn't hide us.
    (.addEventListener trigger "mousedown" (fn [^js e] (.preventDefault e)))
    (.addEventListener trigger "click" on-trigger-click)
    (set! (.. trigger -style -display) "none")
    (set! (.. popup -style -display) "none")
    (.appendChild js/document.body trigger)
    (.appendChild js/document.body popup)
    (.on editor "selection-change" on-selection)
    (.addEventListener root "scroll" reposition false)
    (.addEventListener js/document "scroll" reposition true)  ; capture: outer-ancestor scroll
    (.addEventListener js/window "resize" reposition)
    (.addEventListener js/document "mousedown" on-doc-down true)
    (.addEventListener js/document "keydown" on-key)
    (fn teardown []
      (.off editor "selection-change" on-selection)
      (.removeEventListener root "scroll" reposition false)
      (.removeEventListener js/document "scroll" reposition true)
      (.removeEventListener js/window "resize" reposition)
      (.removeEventListener js/document "mousedown" on-doc-down true)
      (.removeEventListener js/document "keydown" on-key)
      (when (.-parentNode trigger) (.remove trigger))
      (when (.-parentNode popup) (.remove popup)))))
