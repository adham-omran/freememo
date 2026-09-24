(ns freememo.spotlight
  "One card that points at one control, with the rest of the page dimmed
   (plans/getting-started-checklist.md §6). The getting-started checklist is
   the first caller. Any feature may call `show!`.

   The card is Electric DOM. Everything that measures or listens is a plain
   fn, because it is imperative and an Electric body re-runs unpredictably.

   Four rules hold it together:
   - The anchor is found by `[data-tour=…]` at every measurement and never
     kept. A remount replaces the node (3.5).
   - The ring has `pointer-events: none`, so the target stays clickable
     through the highlight (3.8).
   - An outside `pointerdown` closes the card and still reaches the page, so
     a click in the editor both closes it and places the caret (3.9).
   - Floating UI loads on demand. If it fails, or no anchor appears within
     `anchor-wait-ms`, the card shows centered with no ring (3.6, 3.7)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.spotlight-anchors :as catalogue]
   #?(:cljs [goog.object :as gobj])
   #?(:cljs [freememo.vendor-libs :as vendor])))

(def anchor-wait-ms
  "How long to wait after opening for an anchor to appear on screen (3.6)."
  5000)

(defonce ^{:doc "nil, or the open request:
   {:id uuid :anchor-values [data-tour …] :title s :body s :route key :from key}.
   `:route` is the route key the card belongs to, or ::pending until the
   navigation its caller started lands."}
  !spotlight
  (atom nil))

(defonce ^{:doc "The route key the shell reported last, via `observe-route!`."}
  !route-key
  (atom nil))

(defn route-key
  "The part of a route a spotlight belongs to: the tab, and the topic id on a
   topic route. A PDF page change keeps the key, so it does not close a card."
  [tab topic-id]
  [tab topic-id])

(defn close!
  "Post: no spotlight is open."
  []
  (reset! !spotlight nil)
  nil)

(defn show!
  "Open a spotlight, replacing any open one (3.4).

   Pre : every key in `anchors` is a key of `spotlight-anchors/anchors`
         (violation = caller bug). An empty `anchors` asks for a centered
         card. `navigates?` is true when the caller has just called
         `navigate!`, so the card belongs to the next route.
   Post: `!spotlight` holds the new request. Returns nil."
  [{:keys [anchors title body navigates?]}]
  {:pre [(every? catalogue/anchors anchors)]}
  (let [here @!route-key]
    (reset! !spotlight {:id (random-uuid)
                        :anchor-values (mapv catalogue/anchors anchors)
                        :title title
                        :body body
                        :route (if navigates? ::pending here)
                        :from here})
    nil))

(defn observe-route!
  "Close the spotlight when the route leaves the one it belongs to (3.9).
   A request opened with `:navigates?` adopts the first route that differs
   from the one it opened on.
   Post: `!route-key` holds `key`. Returns nil."
  [key]
  (reset! !route-key key)
  (when-let [{:keys [route from]} @!spotlight]
    (cond
      (= route ::pending) (when (not= key from) (swap! !spotlight assoc :route key))
      (not= route key) (close!)))
  nil)

;; ── DOM ────────────────────────────────────────────────────────────────────

#?(:cljs
   (defn- on-screen-anchor
     "The element of the first value in `anchor-values` that is on screen, or
      nil. A closed drawer or a hidden settings tab is not on screen."
     [anchor-values]
     (some (fn [value]
             (when-let [^js el (.querySelector js/document (str "[data-tour=\"" value "\"]"))]
               (let [r (.getBoundingClientRect el)]
                 (when (and (pos? (.-width r)) (pos? (.-height r))
                            (< (.-left r) (.-innerWidth js/window)) (pos? (.-right r))
                            (< (.-top r) (.-innerHeight js/window)) (pos? (.-bottom r)))
                   el))))
       anchor-values)))

#?(:cljs
   (defn close-and-refocus!
     "Close the open spotlight and give focus to its anchor, if the anchor is
      on screen (3.10). For the closes that start inside the card: Esc and
      Got it. A click elsewhere keeps the focus it gave."
     []
     (let [el (some-> @!spotlight :anchor-values on-screen-anchor)]
       (close!)
       (some-> ^js el (.focus #js {:preventScroll true}))
       nil)))
#?(:clj (defn close-and-refocus! [] (close!)))

#?(:cljs
   (defn- place-ring!
     "Draw the ring 4px outside `rect`."
     [^js ring ^js rect]
     (let [pad 4
           style (.-style ring)]
       (set! (.-left style) (str (- (.-left rect) pad) "px"))
       (set! (.-top style) (str (- (.-top rect) pad) "px"))
       (set! (.-width style) (str (+ (.-width rect) (* 2 pad)) "px"))
       (set! (.-height style) (str (+ (.-height rect) (* 2 pad)) "px")))))

#?(:cljs
   (defn- track-anchor!
     "Keep the ring on the anchor and the card beside it with Floating UI.
      The reference is a virtual element that finds the anchor on every
      frame, so a replaced node is picked up (3.5).
      Post: returns Floating UI's cleanup fn."
     [^js layer anchor-values ^js first-rect]
     (let [^js F (gobj/get js/window "FloatingUIDOM")
           ^js ring (.querySelector layer ".spotlight-ring")
           ^js card (.querySelector layer ".spotlight-card")
           !rect (atom first-rect)
           reference #js {:getBoundingClientRect
                          (fn []
                            (when-let [^js el (on-screen-anchor anchor-values)]
                              (reset! !rect (.getBoundingClientRect el)))
                            @!rect)}
           reposition!
           (fn []
             (place-ring! ring @!rect)
             (-> (.computePosition F reference card
                   #js {:strategy "fixed"
                        :placement "bottom"
                        :middleware #js [(.offset F 12)
                                         (.flip F)
                                         (.shift F #js {:padding 8})]})
               (.then (fn [^js p]
                        (set! (.. card -style -left) (str (.-x p) "px"))
                        (set! (.. card -style -top) (str (.-y p) "px"))))))]
       (.autoUpdate F reference card reposition! #js {:animationFrame true}))))

#?(:cljs
   (defn attach!
     "Place the card for one request and wire its close paths.

      Pre : `layer` is the request's `.spotlight-layer`, which holds a
            `.spotlight-ring` and a `.spotlight-card` by the time the first
            animation frame runs.
      Post: the layer's `data-placement` becomes \"anchor\" once an anchor is
            tracked, or \"center\" after `anchor-wait-ms` or a Floating UI
            load failure. Focus moves to the card when it first shows.
            Returns a fn that removes every listener and stops tracking."
     [^js layer {:keys [anchor-values]}]
     (let [!alive? (atom true)
           !stop-tracking (atom nil)
           started (js/Date.now)
           card (fn [] (.querySelector layer ".spotlight-card"))
           show-card! (fn [placement]
                        (.setAttribute layer "data-placement" placement)
                        (some-> ^js (card) (.focus #js {:preventScroll true})))
           on-key (fn [^js e]
                    (when (= "Escape" (.-key e))
                      (close-and-refocus!)))
           on-pointer-down (fn [^js e]
                             (when-not (some-> ^js (card) (.contains (.-target e)))
                               (close!)))
           anchor! (fn [^js el]
                     (-> (vendor/ensure! :floating-ui)
                       (.then (fn [_]
                                (when @!alive?
                                  (reset! !stop-tracking
                                    (track-anchor! layer anchor-values (.getBoundingClientRect el)))
                                  (show-card! "anchor"))))
                       (.catch (fn [_]
                                 (when @!alive? (show-card! "center"))))))
           wait! (fn wait! []
                   (when @!alive?
                     (if-let [el (on-screen-anchor anchor-values)]
                       (anchor! el)
                       (if (> (- (js/Date.now) started) anchor-wait-ms)
                         (show-card! "center")
                         (js/requestAnimationFrame wait!)))))]
       (.addEventListener js/document "keydown" on-key true)
       (.addEventListener js/document "pointerdown" on-pointer-down true)
       (if (seq anchor-values)
         (js/requestAnimationFrame wait!)
         (js/requestAnimationFrame #(when @!alive? (show-card! "center"))))
       (fn detach! []
         (reset! !alive? false)
         (.removeEventListener js/document "keydown" on-key true)
         (.removeEventListener js/document "pointerdown" on-pointer-down true)
         (when-let [stop-tracking! @!stop-tracking] (stop-tracking!))
         nil))))
#?(:clj (defn attach! [_layer _request] (fn [] nil)))

;; ── Components ─────────────────────────────────────────────────────────────

(e/defn SpotlightCard
  "The layer for one request: the ring, and the card as a non-modal dialog
   (3.10). `attach!` positions both, so neither carries a position prop."
  [{:keys [title body] :as request}]
  (e/client
    (dom/div
      (dom/props {:class "spotlight-layer"})
      (dom/div (dom/props {:class "spotlight-ring" :aria-hidden "true"}))
      (dom/div
        (dom/props {:class "spotlight-card"
                    :role "dialog"
                    :tabindex "-1"
                    :aria-labelledby "spotlight-title"
                    :aria-describedby "spotlight-body"})
        (dom/h3 (dom/props {:id "spotlight-title" :class "spotlight-card__title"})
          (dom/text title))
        (dom/p (dom/props {:id "spotlight-body" :class "spotlight-card__body"})
          (dom/text body))
        (dom/div
          (dom/props {:class "spotlight-card__actions"})
          (dom/button
            (dom/props {:class "btn btn-primary btn-sm"})
            (dom/text "Got it")
            (dom/On "click" (fn [_] (close-and-refocus!)) nil))))
      (e/on-unmount (attach! dom/node request)))))

(e/defn Spotlight
  "Mount once at the Main root, as the last form, so the card paints over the
   page and under the toasts (3.3). `key` is `(route-key tab topic-id)`."
  [key]
  (e/client
    (let [request (e/watch !spotlight)]
      (e/for-by :id [r (if request [request] [])]
        (SpotlightCard r))
      (observe-route! key))))
