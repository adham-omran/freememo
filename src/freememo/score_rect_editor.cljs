(ns freememo.score-rect-editor
  "CLJS-only Konva wrapper for drawing Score notation rectangles over a
   rendered PDF-page canvas — freememo.occlusion-editor retargeted from an
   <img> onto a canvas source (Konva.Image accepts a canvas), with the same
   scaling contract: every Konva node lives in SNAPSHOT-pixel coordinates
   (the rendered canvas's own pixel space, the space score geometry stores
   and score-pdf/crop-blob reads) while rendering fit-to-container.

   Unlike occlusion masks, score rects are outlines (the region crops to an
   image; it doesn't hide anything), so the fill stays translucent.

   API:
     (init! {:container el :canvas c :rects [{:x :y :w :h}]
             :on-change (fn [rects])}) -> handle atom, or nil without Konva
     (read-rects handle) -> current rects (save-time authoritative read)
     (destroy! handle)"
  (:require [freememo.logging :as log]))

(def ^:private min-rect-px 8)
(def ^:private rect-fill "rgba(99,102,241,0.15)")
(def ^:private rect-stroke "#6366f1")

(defn- round2 [v]
  (/ (js/Math.round (* v 100)) 100))

(defn- node->rect [^js node]
  {:x (round2 (.x node))
   :y (round2 (.y node))
   :w (round2 (.width node))
   :h (round2 (.height node))})

(defn read-rects
  [handle]
  (when handle
    (let [{:keys [rect-layer]} @handle]
      (when rect-layer
        (->> (.find rect-layer "Rect")
          (mapv node->rect))))))

(defn- emit! [handle]
  (let [{:keys [on-change]} @handle]
    (when on-change
      (on-change (read-rects handle)))))

(defn- select! [handle node]
  (let [^js tr (:tr @handle)]
    (.nodes tr (if node (array node) (array)))))

(defn- normalize-transform! [^js node]
  (let [w (* (.width node) (.scaleX node))
        h (* (.height node) (.scaleY node))]
    (.width node (js/Math.max min-rect-px w))
    (.height node (js/Math.max min-rect-px h))
    (.scaleX node 1)
    (.scaleY node 1)))

(defn- attach-rect-handlers! [handle ^js node]
  (.on node "mousedown touchstart"
    (fn [e]
      (set! (.-cancelBubble e) true)
      (select! handle node)))
  (.on node "dragend" (fn [_] (emit! handle)))
  (.on node "transformend"
    (fn [_]
      (normalize-transform! node)
      (emit! handle))))

(defn- make-rect [handle {:keys [x y w h]}]
  (let [Konva (.-Konva js/window)
        ^js node (new (.-Rect Konva)
                   #js {:x x :y y :width w :height h
                        :fill rect-fill :stroke rect-stroke :strokeWidth 2
                        :strokeScaleEnabled false
                        :draggable true})]
    (attach-rect-handlers! handle node)
    node))

(defn- natural-pointer [^js stage scale]
  (when-let [pos (.getPointerPosition stage)]
    {:x (/ (.-x pos) scale) :y (/ (.-y pos) scale)}))

(defn- wire-draw-handlers! [handle]
  (let [{:keys [rect-layer scale]} @handle
        ^js stage (:stage @handle)
        !draft (atom nil)]
    (.on stage "mousedown touchstart"
      (fn [e]
        (when (or (= (.-target e) stage)
                (= "Image" (.getClassName ^js (.-target e))))
          (select! handle nil)
          (when-let [{:keys [x y]} (natural-pointer stage scale)]
            (let [node (make-rect handle {:x x :y y :w 0 :h 0})]
              (.add rect-layer node)
              (reset! !draft {:node node :ox x :oy y}))))))
    (.on stage "mousemove touchmove"
      (fn [_]
        (when-let [{:keys [node ox oy]} @!draft]
          (when-let [{:keys [x y]} (natural-pointer stage scale)]
            (.x node (js/Math.min x ox))
            (.y node (js/Math.min y oy))
            (.width node (js/Math.abs (- x ox)))
            (.height node (js/Math.abs (- y oy)))))))
    (.on stage "mouseup touchend"
      (fn [_]
        (when-let [^js node (:node @!draft)]
          (reset! !draft nil)
          (if (or (< (.width node) min-rect-px) (< (.height node) min-rect-px))
            (.destroy node)
            (do (select! handle node)
              (emit! handle))))))))

(defn- wire-delete-key! [handle]
  (let [{:keys [container]} @handle
        key-handler
        (fn [e]
          (when (contains? #{"Delete" "Backspace"} (.-key e))
            (let [^js tr (:tr @handle)
                  ^js selected (first (.nodes tr))]
              (when selected
                (.preventDefault e)
                (select! handle nil)
                (.destroy selected)
                (emit! handle)))))]
    (.setAttribute container "tabindex" "0")
    (set! (.-outline (.-style container)) "none")
    (.addEventListener container "pointerdown" (fn [_] (.focus container)))
    (.addEventListener container "keydown" key-handler)
    (swap! handle assoc :key-handler key-handler)))

(defn init!
  "Create the editor inside `container` over `canvas` (a rendered PDF page).
   Synchronous — the canvas is already rasterized. `zoom` scales the DISPLAY
   only (factor of fit-to-container; rect coordinates stay in snapshot px —
   the host scrolls when the stage outgrows it). Returns a handle atom, or
   nil when Konva is not loaded."
  [{:keys [container ^js canvas rects on-change zoom]}]
  (when (and container canvas (.-Konva js/window))
    (let [handle (atom {:container container :on-change on-change})
          Konva (.-Konva js/window)
          nw (.-width canvas)
          nh (.-height canvas)
          cw (js/Math.max 200 (.-clientWidth container))
          max-h (* 0.55 (.-innerHeight js/window))
          scale (* (or zoom 1) (js/Math.min (/ cw nw) (/ max-h nh)))
          stage (new (.-Stage Konva)
                  #js {:container container
                       :width (* nw scale)
                       :height (* nh scale)})
          image-layer (new (.-Layer Konva) #js {:listening false})
          rect-layer (new (.-Layer Konva))
          tr-layer (new (.-Layer Konva))
          tr (new (.-Transformer Konva)
               #js {:rotateEnabled false
                    :flipEnabled false
                    :ignoreStroke true})]
      (.scale stage #js {:x scale :y scale})
      (.add image-layer (new (.-Image Konva)
                          #js {:image canvas :x 0 :y 0 :width nw :height nh}))
      (.add tr-layer tr)
      (.add stage image-layer)
      (.add stage rect-layer)
      (.add stage tr-layer)
      (swap! handle assoc
        :stage stage :rect-layer rect-layer :tr tr
        :scale scale :natural {:width nw :height nh})
      (doseq [r rects]
        (.add rect-layer (make-rect handle r)))
      (wire-draw-handlers! handle)
      (wire-delete-key! handle)
      handle)))

(defn destroy! [handle]
  (when handle
    (let [{:keys [container key-handler]} @handle
          ^js stage (:stage @handle)]
      (when (and container key-handler)
        (.removeEventListener container "keydown" key-handler))
      (when stage (.destroy stage))
      (reset! handle nil))))
