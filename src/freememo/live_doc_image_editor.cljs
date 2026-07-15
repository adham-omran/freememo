(ns freememo.live-doc-image-editor
  "CLJS-only helpers for the Live-Document photo wizard: load a staged image,
   raster it at its chosen orientation, draw cropped thumbnails, and run a
   single-crop-rectangle Konva editor over the oriented raster.

   Coordinate contract: the editor rasters the photo to a canvas at its
   POST-ROTATION orientation (rotate is baked into the raster, clockwise, to
   match the server's live-doc/rotate-image), then emits the crop rectangle as
   normalized fractions ∈ [0,1] of that oriented canvas. The server crops the
   rotated image with the identical fractions, so preview == saved page.

   Adapted from freememo.score-rect-editor (same Konva idioms), narrowed to one
   axis-aligned rectangle whose coordinates are normalized rather than raw
   snapshot pixels.

   API:
     (load-image url)              -> Promise<HTMLImageElement>
     (oriented-canvas img deg)     -> canvas rotated clockwise by deg
     (draw-thumb! canvas img deg crop max-px)  -> fills `canvas` with the
         rotated+cropped photo, letterboxed to max-px (used by staging thumbs)
     (init! {:container el :canvas oriented-canvas :crop {norm}|nil
             :on-change (fn [{norm}|nil])})     -> handle, or nil without Konva
     (destroy! handle)"
  (:require [freememo.logging :as log]))

(def ^:private min-rect-px 8)
(def ^:private rect-fill "rgba(45,108,223,0.15)")
(def ^:private rect-stroke "#2d6cdf")

;; ---------------------------------------------------------------------------
;; Image loading + orientation raster
;; ---------------------------------------------------------------------------

(defn load-image
  "Resolve to a decoded HTMLImageElement for `url` (an object URL). Rejects on
   decode error so callers can surface a per-photo failure."
  [url]
  (js/Promise.
    (fn [resolve reject]
      (let [img (js/Image.)]
        (set! (.-onload img) (fn [] (resolve img)))
        (set! (.-onerror img) (fn [e] (reject e)))
        (set! (.-src img) url)))))

(defn oriented-canvas
  "Return a canvas holding `img` rotated CLOCKWISE by `deg` ∈ {0,90,180,270}.
   Dimensions swap on quarter turns, mirroring live-doc/rotate-image so the
   normalized crop rect means the same thing on client and server."
  [^js img deg]
  (let [d (mod (long deg) 360)
        iw (.-naturalWidth img) ih (.-naturalHeight img)
        swap? (or (= d 90) (= d 270))
        cw (if swap? ih iw) ch (if swap? iw ih)
        canvas (js/document.createElement "canvas")
        ctx (.getContext canvas "2d")]
    (set! (.-width canvas) cw)
    (set! (.-height canvas) ch)
    (.translate ctx (/ cw 2) (/ ch 2))
    (.rotate ctx (* d (/ js/Math.PI 180)))
    (.drawImage ctx img (- (/ iw 2)) (- (/ ih 2)))
    canvas))

(defn draw-thumb!
  "Draw `img` rotated by `deg` and cropped to normalized `crop` (nil = whole
   frame) into `target` canvas, scaled to fit within `max-px` and letterboxed.
   Pre: `target` is an attached <canvas>; `img` is decoded."
  [^js target ^js img deg crop max-px]
  (let [oriented (oriented-canvas img deg)
        ow (.-width oriented) oh (.-height oriented)
        sx (if crop (* (:x crop) ow) 0)
        sy (if crop (* (:y crop) oh) 0)
        sw (if crop (max 1 (* (:w crop) ow)) ow)
        sh (if crop (max 1 (* (:h crop) oh)) oh)
        scale (min (/ max-px sw) (/ max-px sh))
        dw (* sw scale) dh (* sh scale)
        ctx (.getContext target "2d")]
    (set! (.-width target) max-px)
    (set! (.-height target) max-px)
    (.clearRect ctx 0 0 max-px max-px)
    (.drawImage ctx oriented sx sy sw sh
      (/ (- max-px dw) 2) (/ (- max-px dh) 2) dw dh)))

;; ---------------------------------------------------------------------------
;; Konva single-rect crop editor
;; ---------------------------------------------------------------------------

(defn- clamp01 [v] (max 0.0 (min 1.0 v)))

(defn- current-rect
  "The sole crop Rect node's geometry in oriented-canvas pixels, or nil."
  [handle]
  (let [{:keys [rect-layer]} @handle]
    (when-let [^js node (first (.find rect-layer "Rect"))]
      {:x (.x node) :y (.y node) :w (.width node) :h (.height node)})))

(defn- emit! [handle]
  (let [{:keys [on-change nat]} @handle
        {nw :w nh :h} nat
        r (current-rect handle)]
    (when on-change
      (on-change (when r
                   {:x (clamp01 (/ (:x r) nw)) :y (clamp01 (/ (:y r) nh))
                    :w (clamp01 (/ (:w r) nw)) :h (clamp01 (/ (:h r) nh))})))))

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
    (fn [e] (set! (.-cancelBubble e) true) (select! handle node)))
  (.on node "dragend" (fn [_] (emit! handle)))
  (.on node "transformend" (fn [_] (normalize-transform! node) (emit! handle))))

(defn- make-rect [handle {:keys [x y w h]}]
  (let [Konva (.-Konva js/window)
        ^js node (new (.-Rect Konva)
                   #js {:x x :y y :width w :height h
                        :fill rect-fill :stroke rect-stroke :strokeWidth 2
                        :strokeScaleEnabled false :draggable true})]
    (attach-rect-handlers! handle node)
    node))

(defn- clear-rects! [handle]
  (let [{:keys [rect-layer]} @handle]
    (doseq [^js n (.find rect-layer "Rect")] (.destroy n))))

(defn- natural-pointer [^js stage scale]
  (when-let [pos (.getPointerPosition stage)]
    {:x (/ (.-x pos) scale) :y (/ (.-y pos) scale)}))

(defn- wire-draw-handlers!
  "Drag on the image draws THE crop rect, replacing any previous one (single
   selection, unlike score's multi-rect editor)."
  [handle]
  (let [{:keys [rect-layer scale]} @handle
        ^js stage (:stage @handle)
        !draft (atom nil)]
    (.on stage "mousedown touchstart"
      (fn [e]
        (when (or (= (.-target e) stage)
                (= "Image" (.getClassName ^js (.-target e))))
          (select! handle nil)
          (clear-rects! handle)
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
            (do (.destroy node) (emit! handle))     ; tap/tiny → clear crop
            (do (select! handle node) (emit! handle))))))))

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
  "Mount the crop editor inside `container` over `canvas` (an oriented raster
   from oriented-canvas). `crop` (normalized, or nil) seeds the initial
   rectangle. `on-change` fires with the normalized crop (or nil when cleared).
   Returns a handle atom, or nil when Konva is not loaded."
  [{:keys [container ^js canvas crop on-change]}]
  (if-not (and container canvas (.-Konva js/window))
    (do (when-not (.-Konva js/window)
          (log/log-debug "[live-doc-image-editor] Konva not loaded — crop disabled"))
        nil)
    (let [handle (atom {:container container :on-change on-change})
          Konva (.-Konva js/window)
          nw (.-width canvas) nh (.-height canvas)
          cw (js/Math.max 200 (.-clientWidth container))
          max-h (* 0.5 (.-innerHeight js/window))
          scale (js/Math.min (/ cw nw) (/ max-h nh))
          stage (new (.-Stage Konva)
                  #js {:container container :width (* nw scale) :height (* nh scale)})
          image-layer (new (.-Layer Konva) #js {:listening false})
          rect-layer (new (.-Layer Konva))
          tr-layer (new (.-Layer Konva))
          tr (new (.-Transformer Konva)
               #js {:rotateEnabled false :flipEnabled false :ignoreStroke true})]
      (.scale stage #js {:x scale :y scale})
      (.add image-layer (new (.-Image Konva)
                          #js {:image canvas :x 0 :y 0 :width nw :height nh}))
      (.add tr-layer tr)
      (.add stage image-layer)
      (.add stage rect-layer)
      (.add stage tr-layer)
      (swap! handle assoc
        :stage stage :rect-layer rect-layer :tr tr
        :scale scale :nat {:w nw :h nh})
      (when crop
        (let [node (make-rect handle {:x (* (:x crop) nw) :y (* (:y crop) nh)
                                      :w (* (:w crop) nw) :h (* (:h crop) nh)})]
          (.add rect-layer node)
          (select! handle node)))
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
