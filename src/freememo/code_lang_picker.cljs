(ns freememo.code-lang-picker
  "Custom language picker for Quill code blocks.

   Quill's syntax module renders a native `<select class=\"ql-ui\">` per code
   block; its open options list is browser chrome that renders detached from the
   control. We hide that select (CSS) and overlay our own trigger + popup, which
   drive the hidden select (set `value` + dispatch `change`) so Quill still
   applies and re-highlights the language — we own only the UI, not the logic.

   install! observes the editor for `.ql-code-block-container`s, gives each a
   trigger, and returns a teardown fn removing everything.")

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

(defn install!
  "Wire the custom code-language picker for `editor`. Returns a teardown fn.

   Pre:  the native `.ql-ui` select is hidden by CSS.
   Post: each code block shows a custom language trigger; clicking opens a
         viewport-clamped popup; picking sets the hidden select + dispatches
         `change` (Quill applies + re-highlights). Teardown disconnects the
         observer and removes the popup, triggers, and listeners."
  [^js editor]
  (let [root  (.-root editor)
        popup (el "div" "ql-lang-popup")
        !open (atom nil)                                  ; {:select :trigger} currently open
        close! (fn []
                 (set! (.. popup -style -display) "none")
                 (reset! !open nil))
        place! (fn [^js trigger]
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
        open! (fn [^js select ^js trigger]
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
                        (close!)))
                    (.appendChild popup item)))
                (reset! !open {:select select :trigger trigger})
                (place! trigger))
        inject! (fn [^js container]
                  ;; re-inject if a rebuild wiped our trigger (guard on the child,
                  ;; not a marker attr, so it survives Quill's re-highlighting).
                  (when-let [select (native-select container)]
                    (when-not (.querySelector container ":scope > .ql-lang-trigger")
                      (let [trigger (el "button" "ql-lang-trigger")]
                        (set! (.-type trigger) "button")
                        (set! (.-textContent trigger) (current-label select))
                        (.setAttribute trigger "title" "Code language")
                        (.setAttribute trigger "aria-label" "Code language")
                        (.addEventListener trigger "mousedown" (fn [^js e] (.preventDefault e)))
                        (.addEventListener trigger "click"
                          (fn [^js e]
                            (.stopPropagation e)
                            (if (= (:trigger @!open) trigger) (close!) (open! select trigger))))
                        (.appendChild container trigger)))))
        scan! (fn [] (doseq [c (->vec (.querySelectorAll root ".ql-code-block-container"))]
                       (inject! c)))
        observer (js/MutationObserver. (fn [_ _] (scan!)))
        on-doc-down (fn [^js e]
                      (when (and @!open
                              (not (.contains popup (.-target e)))
                              (not (= (:trigger @!open) (.-target e))))
                        (close!)))
        on-scroll (fn [_] (when-let [{:keys [trigger]} @!open] (place! trigger)))
        on-key (fn [^js e] (when (and @!open (= "Escape" (.-key e))) (close!)))]
    (set! (.. popup -style -display) "none")
    (.appendChild js/document.body popup)
    (.observe observer root #js {:childList true :subtree true})
    (scan!)
    (.addEventListener js/document "mousedown" on-doc-down true)
    (.addEventListener js/document "scroll" on-scroll true)
    (.addEventListener js/window "resize" on-scroll)
    (.addEventListener js/document "keydown" on-key)
    (fn teardown []
      (.disconnect observer)
      (.removeEventListener js/document "mousedown" on-doc-down true)
      (.removeEventListener js/document "scroll" on-scroll true)
      (.removeEventListener js/window "resize" on-scroll)
      (.removeEventListener js/document "keydown" on-key)
      (doseq [t (->vec (.querySelectorAll root ".ql-lang-trigger"))] (.remove t))
      (when (.-parentNode popup) (.remove popup)))))
