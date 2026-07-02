(ns freememo.occlusion-editor
  "CLJS-only Konva wrapper for the image-occlusion mask editor.

   The stage is scaled so every Konva node lives in NATURAL-IMAGE pixel
   coordinates — the same space the stored geometry and the generated mask
   SVGs use — while rendering fit-to-container. Pointer positions are divided
   by the scale on the way in; node attrs are read back verbatim on the way
   out, so there is no coordinate translation anywhere else.

   Operations: drag-on-empty draws a rect (minimum size enforced), click
   selects (Transformer move/resize, rotation disabled), Delete/Backspace
   removes the selection.

   API (mirrors quill_field's init/destroy lifecycle split):
     (init! {:container el :image-url s :rects [{:x :y :w :h :ordinal}]
             :on-change (fn [rects]) :on-ready (fn [{:keys [width height]}])})
       -> handle (atom), or nil when window.Konva is absent
     (read-rects handle)  -> current rects vector (save-time authoritative read)
     (destroy! handle)

   Rects carry :ordinal when they existed before this session; rects drawn
   here have :ordinal nil — the server assigns fresh ordinals on save."
  (:require [freememo.logging :as log]))

(def ^:private min-rect-px 8)     ; natural px — smaller drags are discarded
(def ^:private rect-fill "#FFEBA2")
(def ^:private rect-stroke "#2D2D2D")

(defn- round2 [v]
  (/ (js/Math.round (* v 100)) 100))

(defn- node->rect [^js node]
  {:x (round2 (.x node))
   :y (round2 (.y node))
   :w (round2 (.width node))
   :h (round2 (.height node))
   :ordinal (.getAttr node "fmOrdinal")})

(defn read-rects
  "Current rects (natural px) straight from the Konva nodes — the
   authoritative save-time read."
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

(defn- normalize-transform!
  "Konva resizes by scaling; fold scaleX/scaleY back into width/height so the
   stored rect stays a plain axis-aligned box."
  [^js node]
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

(defn- make-rect [handle {:keys [x y w h ordinal]}]
  (let [Konva (.-Konva js/window)
        ^js node (new (.-Rect Konva)
               #js {:x x :y y :width w :height h
                    :fill rect-fill :stroke rect-stroke :strokeWidth 1
                    :strokeScaleEnabled false
                    :draggable true})]
    (.setAttr node "fmOrdinal" (or ordinal nil))
    (attach-rect-handlers! handle node)
    node))

(defn- natural-pointer [^js stage scale]
  (when-let [pos (.getPointerPosition stage)]
    {:x (/ (.-x pos) scale) :y (/ (.-y pos) scale)}))

(defn- wire-draw-handlers!
  "Drag on empty canvas creates a new rect; click on empty deselects."
  [handle]
  (let [{:keys [rect-layer scale]} @handle
        ^js stage (:stage @handle)
        !draft (atom nil)]                          ; {:node :ox :oy} while drawing
    (.on stage "mousedown touchstart"
      (fn [e]
        ;; Only start drawing from the stage/image itself — rect mousedown
        ;; cancels bubbling above.
        (when (or (= (.-target e) stage)
                (= "Image" (.getClassName ^js (.-target e))))
          (select! handle nil)
          (when-let [{:keys [x y]} (natural-pointer stage scale)]
            (let [node (make-rect handle {:x x :y y :w 0 :h 0 :ordinal nil})]
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
            (.destroy node)                          ; too small — discard
            (do (select! handle node)
              (emit! handle))))))))

(defn- wire-delete-key!
  "Delete/Backspace removes the selected rect. Listens on the editor
   container (focused on pointerdown) so modal Quill fields keep their own
   Backspace."
  [handle]
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

(defn- build-stage!
  "Build the stage once the image element has loaded."
  [handle img]
  (let [{:keys [container rects on-ready]} @handle
        Konva (.-Konva js/window)
        nw (.-naturalWidth img)
        nh (.-naturalHeight img)
        cw (js/Math.max 200 (.-clientWidth container))
        max-h (* 0.55 (.-innerHeight js/window))
        scale (js/Math.min (/ cw nw) (/ max-h nh))
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
                        #js {:image img :x 0 :y 0 :width nw :height nh}))
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
    (when on-ready (on-ready {:width nw :height nh}))))

(defn init!
  "Create the editor inside `container`. Returns a handle atom, or nil when
   Konva is not loaded. The stage is built async once the image loads."
  [{:keys [container image-url rects on-change on-ready] :as opts}]
  (when (and container (.-Konva js/window))
    (let [handle (atom {:container container
                        :rects (vec rects)
                        :on-change on-change
                        :on-ready on-ready})
          img (js/Image.)]
      (set! (.-onload img)
        (fn []
          ;; Container may have unmounted while the image was in flight.
          (when (and @handle (.-isConnected container))
            (build-stage! handle img))))
      (set! (.-onerror img)
        (fn [_]
          (log/log-error (str "[occlusion-editor] image load failed: " image-url))))
      (set! (.-src img) image-url)
      handle)))

(defn destroy!
  "Tear down the stage and container listeners. Idempotent."
  [handle]
  (when handle
    (let [{:keys [container key-handler]} @handle
          ^js stage (:stage @handle)]
      (when (and container key-handler)
        (.removeEventListener container "keydown" key-handler))
      (when stage (.destroy stage))
      (reset! handle nil))))
