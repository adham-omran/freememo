(ns freememo.tooltip-layer
  "The one hover bubble for every `[data-tooltip]` element
   (plans/tooltip-top-layer.md §1).

   `freememo.tooltip/Tooltip!` and `set-tooltip!` write the attribute. This ns
   reads it. One `<div popover=\"manual\">` on `document.body` shows the text
   of the trigger under the pointer, placed by Floating UI.

   Why the top layer and not a CSS `::after`: a tape row carries a
   `transform`, which makes the row the containing block of a fixed bubble.
   The bubble then fits inside a 36px row, covers its own button and is
   clipped by the scroll box (measured, Chrome 156). The top layer escapes
   every transform, overflow clip and stacking context, so no container
   needs an override.

   The trigger is found by hit-testing the last pointer position, never by
   `pointerover` alone, for two reasons:
   - A scroll moves content under a still pointer, and no pointer event
     fires. A capture-phase `scroll` re-runs the hit test.
   - Whether a disabled `<button>` receives pointer events differs by
     browser. `elementFromPoint` sees it either way.

   Plain JS DOM, not Electric: the bubble outlives every frame, and an
   Electric body re-runs unpredictably (docs/electric-patterns.md, \"JS Library Init Side
   Effects in e/defn\")."
  (:require [goog.object :as gobj]
            [freememo.vendor-libs :as vendor]))

;; nil, or the open bubble:
;; {:trigger <element> :stop-tracking (fn []) :observer MutationObserver}
(defonce ^:private !open (atom nil))

;; A trigger the user pressed or typed into. Its bubble stays hidden until the
;; pointer reaches a different trigger, the way a native tooltip behaves.
(defonce ^:private !dismissed (atom nil))

;; {:x :y :touch?} of the last pointer event, or nil before the first one.
(defonce ^:private !pointer (atom nil))

(defonce ^:private !hit-test-queued? (atom false))

(defn- hover-device? []
  (.-matches (.matchMedia js/window "(hover: hover)")))

(defn- bubble-text
  "Post: the trigger's tooltip text, or nil when it has none to show."
  [^js trigger]
  (let [text (.getAttribute trigger "data-tooltip")]
    (when (and (seq text)
               (not= "true" (.getAttribute trigger "aria-expanded")))
      text)))

(defonce ^:private bubble
  (let [^js el (.createElement js/document "div")]
    (.setAttribute el "popover" "manual")
    (.setAttribute el "role" "tooltip")
    (set! (.-className el) "tooltip-bubble")
    (.appendChild (.-body js/document) el)
    el))

(defn- show-bubble! []
  (when (and (fn? (.-showPopover bubble)) (not (.matches bubble ":popover-open")))
    (.showPopover bubble)))

(defn- hide-bubble! []
  (when (and (fn? (.-hidePopover bubble)) (.matches bubble ":popover-open"))
    (.hidePopover bubble))
  (.removeAttribute bubble "data-placed"))

(defn hide!
  "Close the bubble and stop following its trigger. Idempotent.
   Post: no bubble is visible and nothing tracks a trigger."
  []
  (when-let [{:keys [stop-tracking ^js observer]} @!open]
    (reset! !open nil)
    (.disconnect observer)
    (when stop-tracking (stop-tracking)))
  (hide-bubble!))

(defn- place!
  "Put the bubble below `trigger`, flipped or shifted to stay on screen.
   Pre : Floating UI is loaded.
   Post: the bubble is positioned and marked `data-placed`, or hidden when the
         trigger has left the document."
  [^js F ^js trigger]
  (if-not (.-isConnected trigger)
    (hide!)
    (-> (.computePosition F trigger bubble
          #js {:strategy "fixed"
               :placement (if (.contains (.-classList trigger) "tooltip-right")
                            "bottom-end"
                            "bottom")
               :middleware #js [(.offset F 4)
                                (.flip F)
                                (.shift F #js {:padding 8})]})
      (.then (fn [^js p]
               (when (identical? trigger (:trigger @!open))
                 (set! (.. bubble -style -left) (str (.-x p) "px"))
                 (set! (.. bubble -style -top) (str (.-y p) "px"))
                 (.setAttribute bubble "data-placed" "true")))))))

(defn- follow-trigger-text!
  "Keep the bubble's text equal to the trigger's attribute. Electric keeps a
   row's node and writes the next row's content into it, so the text can
   change while the pointer is still.
   Post: returns the observer; it hides the bubble when the text goes away."
  [^js trigger]
  (doto (js/MutationObserver.
          (fn [_ _]
            (if-let [text (bubble-text trigger)]
              (set! (.-textContent bubble) text)
              (hide!))))
    (.observe trigger #js {:attributes true
                           :attributeFilter #js ["data-tooltip" "aria-expanded"]})))

(defn- open-for!
  "Show `trigger`'s text in the bubble.
   Pre : `(bubble-text trigger)` is non-nil.
   Post: `(:trigger @!open)` is `trigger`. The bubble is visible once Floating
         UI has loaded and placed it, and never before."
  [^js trigger]
  (hide!)
  (set! (.-textContent bubble) (bubble-text trigger))
  (reset! !open {:trigger trigger :observer (follow-trigger-text! trigger)})
  (show-bubble!)
  (-> (vendor/ensure! :floating-ui)
    (.then (fn [_]
             (when (identical? trigger (:trigger @!open))
               (let [^js F (gobj/get js/window "FloatingUIDOM")
                     stop (.autoUpdate F trigger bubble #(place! F trigger)
                            #js {:animationFrame true})]
                 (swap! !open assoc :stop-tracking stop)))))
    ;; ensure! has logged the failure. No placement means no bubble.
    (.catch (fn [_] (hide!)))))

(defn- trigger-under-pointer
  "Post: the nearest `[data-tooltip]` element at the last pointer position, or
         nil. The bubble has `pointer-events: none`, so it is never the hit."
  []
  (when-let [{:keys [x y]} @!pointer]
    (some-> (.elementFromPoint js/document x y)
      (.closest "[data-tooltip]"))))

(defn- hit-test!
  "Open, keep or close the bubble for what is under the pointer now."
  []
  (reset! !hit-test-queued? false)
  (let [{:keys [touch?]} @!pointer
        trigger (when-not touch? (trigger-under-pointer))]
    (when-not (identical? trigger @!dismissed)
      (reset! !dismissed nil))
    (cond
      (or (nil? trigger) (nil? (bubble-text trigger))
          (identical? trigger @!dismissed))
      (hide!)

      (not (identical? trigger (:trigger @!open)))
      (open-for! trigger))))

(defn- queue-hit-test!
  "Run `hit-test!` once in the next frame, however many events ask for it."
  []
  (when (compare-and-set! !hit-test-queued? false true)
    (js/requestAnimationFrame hit-test!)))

(defn- on-pointer-move [^js e]
  (reset! !pointer {:x (.-clientX e) :y (.-clientY e)
                    :touch? (= "touch" (.-pointerType e))})
  (queue-hit-test!))

(defn- dismiss! [_]
  (when-let [{:keys [trigger]} @!open]
    (reset! !dismissed trigger))
  (hide!))

(defn- on-pointer-left-page [^js e]
  (when (nil? (.-relatedTarget e))
    (reset! !pointer nil)
    (hide!)))

(defonce ^{:doc "Install the listeners once per page load. A hot reload keeps
   the first set, because each listener resolves its fn through this ns."}
  installed?
  (when (hover-device?)
    (let [opts #js {:capture true :passive true}]
      (.addEventListener js/document "pointermove" #(on-pointer-move %) opts)
      (.addEventListener js/document "pointerover" #(on-pointer-move %) opts)
      (.addEventListener js/document "scroll" #(queue-hit-test!) opts)
      (.addEventListener js/document "pointerout" #(on-pointer-left-page %) opts)
      (doseq [event ["pointerdown" "contextmenu" "keydown" "dragstart"]]
        (.addEventListener js/document event #(dismiss! %) opts))
      (vendor/prefetch! :floating-ui)
      true)))
