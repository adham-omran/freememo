(ns freememo.occlusion-editor
  "CLJS-only Konva wrapper for the image-occlusion mask editor.

   The stage is scaled so the geometry this editor reads and writes is in
   NATURAL-IMAGE pixels — the same space the stored geometry and the generated
   mask SVGs use — while rendering fit-to-container. Pointer positions are
   divided by the scale on the way in. On the way out, `node->rect` resolves
   each rect through its ancestors (`getAbsolutePosition`/`getAbsoluteScale`)
   and divides the stage scale back out, so a rect inside a Konva group — whose
   own attrs are group-relative — still reads as natural pixels.

   Mask groups: rects sharing a mask ordinal are ONE card, and grouping is how
   the user says so (freememo.occlusion-ordinals owns the server-side algebra).
   A group of rects lives in a Konva group node, and every member carries the
   same fmOrdinal (an existing mask group) or the same fmGid (a new one).

   Tools: :draw (drag on empty canvas draws a rect) and :select (click selects,
   Shift-click adds, drag on empty canvas marquee-selects). The caller picks the
   starting tool — draw when creating, select when editing an existing group.

   Operations: move/resize the selection via Transformer (rotation disabled),
   group (G) / ungroup (U) / select-all (A) / delete (Delete or Backspace), and
   S / R switch tool.

   API (mirrors quill_field's init/destroy lifecycle split):
     (init! {:container el :image-url s :rects [{:x :y :w :h :ordinal}]
             :tool :draw|:select
             :on-change (fn [rects]) :on-ready (fn [{:keys [width height]}])
             :on-tool-change (fn [tool])})
       -> handle (atom), or nil when window.Konva is absent
     (read-rects handle)      -> current rects vector (save-time authoritative read)
     (set-tool! handle tool)  (group-selection! handle)  (ungroup-selection! handle)
     (destroy! handle)

   Rects carry :ordinal when their mask group existed before this session, and
   :gid when this session grouped them into a new mask group; a rect drawn here
   and left alone carries neither — the server assigns fresh ordinals on save."
  (:require [freememo.logging :as log]))

(def ^:private min-rect-px 8)     ; natural px — smaller drags are discarded
(def ^:private rect-fill "#FFEBA2")
(def ^:private rect-stroke "#2D2D2D")
(def ^:private grouped-dash #js [6 3])   ; persistent cue: this rect is grouped

(defn- round2 [v]
  (/ (js/Math.round (* v 100)) 100))

(defn- konva [] (.-Konva js/window))

(defn- group-node? [^js node]
  (and node (= "Group" (.getClassName node))))

;; ---------------------------------------------------------------------------
;; Reading geometry out
;; ---------------------------------------------------------------------------

(defn- node->rect
  "One rect in natural-image pixels.
   Pre:  node is a Konva Rect on the rect layer, possibly inside a group.
   Post: :x/:y/:w/:h are absolute natural pixels regardless of ancestor
         transforms, and AT MOST ONE membership key is present — :ordinal for a
         saved mask group, :gid for one this session made, neither for a lone
         new mask. Absent rather than nil, so a rect read back unchanged is `=`
         to the geometry it was loaded from (the modal's dirty check)."
  [^js node stage-scale]
  (let [pos (.getAbsolutePosition node)
        sc (.getAbsoluteScale node)
        ordinal (.getAttr node "fmOrdinal")
        gid (.getAttr node "fmGid")]
    (cond-> {:x (round2 (/ (.-x pos) stage-scale))
             :y (round2 (/ (.-y pos) stage-scale))
             :w (round2 (* (.width node) (/ (.-x sc) stage-scale)))
             :h (round2 (* (.height node) (/ (.-y sc) stage-scale)))}
      (some? ordinal) (assoc :ordinal ordinal)
      (and (nil? ordinal) (some? gid)) (assoc :gid gid))))

(defn read-rects
  "Current rects (natural px) straight from the Konva nodes — the
   authoritative save-time read. Groups are traversed (Konva `find` is
   recursive), so a grouped rect appears exactly once, like any other."
  [handle]
  (when handle
    (let [{:keys [rect-layer scale]} @handle]
      (when rect-layer
        (->> (.find rect-layer "Rect")
          (mapv (fn [node] (node->rect node scale))))))))

(defn- emit! [handle]
  (let [{:keys [on-change]} @handle]
    (when on-change
      (on-change (read-rects handle)))))

;; ---------------------------------------------------------------------------
;; Selection
;; ---------------------------------------------------------------------------

(defn- selection-target
  "What clicking `node` selects: its mask group when it has one, else itself.
   Grouped rects move and resize together, so the group is the unit."
  [^js node]
  (let [parent (.getParent node)]
    (if (group-node? parent) parent node)))

(defn- selected-nodes [handle]
  (vec (.nodes ^js (:tr @handle))))

(defn- select-nodes! [handle nodes]
  (.nodes ^js (:tr @handle) (into-array nodes)))

(defn- toggle-selected! [handle ^js node]
  (let [current (selected-nodes handle)]
    (select-nodes! handle
      (if (some #(= % node) current)
        (remove #(= % node) current)
        (conj current node)))))

(defn- member-rects
  "The Rect nodes `node` stands for — itself, or a group's children."
  [^js node]
  (if (group-node? node) (vec (.getChildren node)) [node]))

;; ---------------------------------------------------------------------------
;; Transform normalization — Konva resizes by scaling; fold it back into
;; width/height so stored rects stay plain axis-aligned boxes.
;; ---------------------------------------------------------------------------

(defn- normalize-rect-transform! [^js node]
  (let [w (* (.width node) (.scaleX node))
        h (* (.height node) (.scaleY node))]
    (.width node (js/Math.max min-rect-px w))
    (.height node (js/Math.max min-rect-px h))
    (.scaleX node 1)
    (.scaleY node 1)))

(defn- normalize-group-transform!
  "Fold a group's position and scale into its children, leaving the group at the
   layer origin with scale 1.
   Post: absolute layout unchanged; every mask-group node has an identity
         transform, so a child's local coordinates ARE its natural coordinates
         and reparenting a child never moves it; every child is at least
         min-rect-px."
  [^js group]
  (let [sx (.scaleX group)
        sy (.scaleY group)
        gx (.x group)
        gy (.y group)]
    (when (or (not= 1 sx) (not= 1 sy) (not= 0 gx) (not= 0 gy))
      (doseq [^js child (.getChildren group)]
        (.x child (+ gx (* (.x child) sx)))
        (.y child (+ gy (* (.y child) sy)))
        (.width child (js/Math.max min-rect-px (* (.width child) sx)))
        (.height child (js/Math.max min-rect-px (* (.height child) sy))))
      (.x group 0)
      (.y group 0)
      (.scaleX group 1)
      (.scaleY group 1))))

(defn- normalize-transform! [^js node]
  (if (group-node? node)
    (normalize-group-transform! node)
    (normalize-rect-transform! node)))

(defn- make-mask-group!
  "A Konva group for one mask group: draggable as a unit, identity transform at
   the layer origin, and self-normalizing after every drag or resize so the
   identity invariant holds whenever geometry is read or a child is reparented."
  [handle]
  ;; `Konva` MUST be a local, never `(new (.-Group (konva)) …)` inline: CLJS
  ;; emits `new konva.call(null).Group(args)` for that, which JS parses as
  ;; `(new konva.call(null)).Group(args)` — so Konva's Group CLASS is called
  ;; without `new` ("Class constructor … cannot be invoked without 'new'", the
  ;; whole grouping feature dead in dev and prod alike). `new Konva.Group(args)`
  ;; parses correctly. Same reason at wire-marquee! and make-rect.
  (let [Konva (konva)
        {:keys [rect-layer]} @handle
        ^js group (new (.-Group Konva) #js {:draggable true})]
    (.on group "dragend"
      (fn [_] (normalize-group-transform! group) (emit! handle)))
    (.on group "transformend"
      (fn [_] (normalize-group-transform! group) (emit! handle)))
    (.add rect-layer group)
    group))

;; ---------------------------------------------------------------------------
;; Mask-group membership
;; ---------------------------------------------------------------------------

(defn- stamp-membership!
  "Write one mask group's membership onto its rects.
   Post: every rect carries `ordinal` as fmOrdinal and `gid` as fmGid (either
         may be nil), and shows the grouped dash iff `grouped?`."
  [rects ordinal gid grouped?]
  (doseq [^js rect rects]
    (.setAttr rect "fmOrdinal" ordinal)
    (.setAttr rect "fmGid" gid)
    (.dash rect (if grouped? grouped-dash #js []))))

(defn- lowest-ordinal
  "The mask ordinal a merge keeps: the lowest one any member already owns, or
   nil when no member has been saved yet. Preserving it keeps that card — and
   its Anki note and scheduling — instead of retiring the ordinal."
  [rects]
  (->> rects
    (keep (fn [^js r] (.getAttr r "fmOrdinal")))
    sort
    first))

(defn group-selection!
  "Merge the selection into one mask group — one card.
   Pre:  two or more nodes selected (a no-op otherwise).
   Post: all member rects live in one Konva group with identity transform and
         share a membership: the lowest existing ordinal, or a fresh gid when
         none of them has been saved. Nothing moves on screen."
  [handle]
  (when handle
    (let [selection (selected-nodes handle)]
      (when (< 1 (count selection))
        ;; Normalize first: a member group that has been dragged holds its
        ;; children in ITS frame, and reparenting would shift them.
        (doseq [^js node selection]
          (when (group-node? node) (normalize-group-transform! node)))
        (let [rects (vec (mapcat member-rects selection))
              ordinal (lowest-ordinal rects)
              gid (when (nil? ordinal) (str (random-uuid)))
              ^js group (make-mask-group! handle)]
          (doseq [^js node selection]
            (if (group-node? node)
              (do (doseq [^js child (vec (.getChildren node))]
                    (.moveTo child group))
                (.destroy node))
              (.moveTo node group)))
          (doseq [^js rect rects]
            (.draggable rect false))
          (stamp-membership! rects ordinal gid true)
          (select-nodes! handle [group])
          (emit! handle))))))

(defn ungroup-selection!
  "Split the selected mask group back into independent masks.
   Pre:  a group node is selected (a no-op otherwise).
   Post: children are back on the rect layer at unchanged absolute positions;
         the topmost-then-leftmost child keeps the group's ordinal — so its card
         and scheduling survive — and every other child loses its membership,
         earning a fresh ordinal on save."
  [handle]
  (when handle
    (let [{:keys [rect-layer scale]} @handle
          groups (filterv group-node? (selected-nodes handle))]
      (when (seq groups)
        (doseq [^js group groups]
          (let [children (vec (.getChildren group))
                ordinal (lowest-ordinal children)
                ;; Absolute natural coordinates, captured before the move.
                placed (mapv (fn [^js child]
                               (let [pos (.getAbsolutePosition child)
                                     sc (.getAbsoluteScale child)]
                                 {:node child
                                  :x (/ (.-x pos) scale)
                                  :y (/ (.-y pos) scale)
                                  :w (* (.width child) (/ (.-x sc) scale))
                                  :h (* (.height child) (/ (.-y sc) scale))}))
                         children)
                keeper (:node (first (sort-by (juxt :y :x) placed)))]
            (doseq [{:keys [node x y w h]} placed]
              (.moveTo ^js node rect-layer)
              (.x ^js node x)
              (.y ^js node y)
              (.width ^js node w)
              (.height ^js node h)
              (.scaleX ^js node 1)
              (.scaleY ^js node 1)
              (.draggable ^js node true))
            (stamp-membership! (mapv :node placed) nil nil false)
            (when (and keeper ordinal)
              (stamp-membership! [keeper] ordinal nil false))
            (.destroy group)))
        (select-nodes! handle [])
        (emit! handle)))))

;; ---------------------------------------------------------------------------
;; Nodes and stage wiring
;; ---------------------------------------------------------------------------

(defn- attach-rect-handlers! [handle ^js node]
  (.on node "mousedown touchstart"
    (fn [^js e]
      (set! (.-cancelBubble e) true)
      (let [target (selection-target node)]
        (if (.-shiftKey ^js (.-evt e))
          (toggle-selected! handle target)
          (select-nodes! handle [target])))))
  (.on node "dragend" (fn [_] (emit! handle)))
  (.on node "transformend"
    (fn [_]
      (normalize-transform! node)
      (emit! handle))))

(defn- make-rect [handle {:keys [x y w h ordinal gid]}]
  (let [Konva (konva)
        ^js node (new (.-Rect Konva)
               #js {:x x :y y :width w :height h
                    :fill rect-fill :stroke rect-stroke :strokeWidth 1
                    :strokeScaleEnabled false
                    :draggable true})]
    (.setAttr node "fmOrdinal" (or ordinal nil))
    (.setAttr node "fmGid" (or gid nil))
    (attach-rect-handlers! handle node)
    node))

(defn- rebuild-mask-groups!
  "Re-form the Konva groups the loaded geometry implies: rects sharing an
   ordinal are one mask group, exactly as on the Anki side.
   Pre:  every node is a direct child of the rect layer.
   Post: each shared-ordinal set lives in one identity-transform group with the
         grouped cue; singletons are untouched."
  [handle nodes]
  (doseq [[ordinal members] (group-by (fn [^js n] (.getAttr n "fmOrdinal")) nodes)]
    (when (and (some? ordinal) (< 1 (count members)))
      (let [^js group (make-mask-group! handle)]
        (doseq [^js node members]
          (.moveTo node group)
          (.draggable node false))
        (stamp-membership! members ordinal nil true)))))

(defn set-tool!
  "Switch tool. Post: @handle :tool = tool, the selection is cleared, and
   on-tool-change has been told (so a toolbar can follow keyboard switches)."
  [handle tool]
  (when handle
    (let [{:keys [on-tool-change]} @handle]
      (when (not= tool (:tool @handle))
        (swap! handle assoc :tool tool)
        (select-nodes! handle [])
        (when on-tool-change (on-tool-change tool))))))

(defn- natural-pointer [^js stage scale]
  (when-let [pos (.getPointerPosition stage)]
    {:x (/ (.-x pos) scale) :y (/ (.-y pos) scale)}))

(defn- empty-canvas-target?
  "True when the event landed on the stage or the image, i.e. not on a mask.
   Rect mousedown cancels bubbling, so this is the 'nothing there' test."
  [^js stage e]
  (or (= (.-target e) stage)
    (= "Image" (.getClassName ^js (.-target e)))))

(defn- wire-draw!
  "Drag on empty canvas creates a rect (:draw tool only)."
  [handle]
  (let [{:keys [rect-layer scale]} @handle
        ^js stage (:stage @handle)
        !draft (atom nil)]                          ; {:node :ox :oy} while drawing
    (.on stage "mousedown touchstart"
      (fn [e]
        (when (and (= :draw (:tool @handle)) (empty-canvas-target? stage e))
          (select-nodes! handle [])
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
            (.destroy node)                          ; too small — discard
            (do (select-nodes! handle [node])
              (emit! handle))))))))

(defn- wire-marquee!
  "Drag on empty canvas rubber-band selects (:select tool only)."
  [handle]
  ;; Konva read once at wire time, not per pointerdown, and MUST be a local for
  ;; the `new` to compile correctly — see make-mask-group!. init! has already
  ;; verified window.Konva exists, so capturing it here is safe.
  (let [Konva (konva)
        {:keys [tr-layer scale]} @handle
        ^js stage (:stage @handle)
        ^js rect-layer (:rect-layer @handle)
        !band (atom nil)]                            ; {:node :ox :oy}
    (.on stage "mousedown touchstart"
      (fn [e]
        (when (and (= :select (:tool @handle)) (empty-canvas-target? stage e))
          (select-nodes! handle [])
          (when-let [{:keys [x y]} (natural-pointer stage scale)]
            (let [^js node (new (.-Rect Konva)
                            #js {:x x :y y :width 0 :height 0
                                 :stroke "#2D6BFF" :strokeWidth 1
                                 :strokeScaleEnabled false
                                 :dash #js [4 3] :listening false})]
              (.add tr-layer node)
              (reset! !band {:node node :ox x :oy y}))))))
    (.on stage "mousemove touchmove"
      (fn [_]
        (when-let [{:keys [^js node ox oy]} @!band]
          (when-let [{:keys [x y]} (natural-pointer stage scale)]
            (.x node (js/Math.min x ox))
            (.y node (js/Math.min y oy))
            (.width node (js/Math.abs (- x ox)))
            (.height node (js/Math.abs (- y oy)))))))
    (.on stage "mouseup touchend"
      (fn [_]
        (when-let [^js node (:node @!band)]
          (reset! !band nil)
          (let [box (.getClientRect node)
                hit (filterv (fn [^js child]
                               (.haveIntersection (.-Util Konva) box (.getClientRect child)))
                      (vec (.getChildren rect-layer)))]
            (.destroy node)
            (select-nodes! handle hit)))))))

(defn- wire-keys!
  "Editor shortcuts, scoped to the editor container (focused on pointerdown) so
   the modal's Quill fields keep every key to themselves — the app's global
   handler treats plain letters as global, which is why these must not be
   registered there. Modified keys are left to the app."
  [handle]
  (let [{:keys [container]} @handle
        delete-selection!
        (fn []
          (let [selection (selected-nodes handle)]
            (when (seq selection)
              (select-nodes! handle [])
              (doseq [^js node selection] (.destroy node))
              (emit! handle))))
        key-handler
        (fn [e]
          (when-not (or (.-metaKey e) (.-ctrlKey e) (.-altKey e))
            (case (.toLowerCase (.-key e))
              ("delete" "backspace") (do (.preventDefault e) (delete-selection!))
              "s" (do (.preventDefault e) (set-tool! handle :select))
              "r" (do (.preventDefault e) (set-tool! handle :draw))
              "g" (do (.preventDefault e) (group-selection! handle))
              "u" (do (.preventDefault e) (ungroup-selection! handle))
              "a" (do (.preventDefault e)
                    (select-nodes! handle (vec (.getChildren ^js (:rect-layer @handle)))))
              nil)))]
    (.setAttribute container "tabindex" "0")
    (set! (.-outline (.-style container)) "none")
    (.addEventListener container "pointerdown" (fn [_] (.focus container)))
    (.addEventListener container "keydown" key-handler)
    (swap! handle assoc :key-handler key-handler)))

(defn- build-stage!
  "Build the stage once the image element has loaded."
  [handle img]
  (let [{:keys [container rects on-ready]} @handle
        Konva (konva)
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
      :stage stage :rect-layer rect-layer :tr-layer tr-layer :tr tr
      :scale scale :natural {:width nw :height nh})
    (let [nodes (mapv (fn [r]
                        (let [node (make-rect handle r)]
                          (.add rect-layer node)
                          node))
                  rects)]
      (rebuild-mask-groups! handle nodes))
    (wire-draw! handle)
    (wire-marquee! handle)
    (wire-keys! handle)
    (when on-ready (on-ready {:width nw :height nh}))))

(defn init!
  "Create the editor inside `container`. Returns a handle atom, or nil when
   Konva is not loaded. The stage is built async once the image loads."
  [{:keys [container image-url rects tool on-change on-ready on-tool-change]}]
  (when (and container (.-Konva js/window))
    (let [handle (atom {:container container
                        :rects (vec rects)
                        :tool (or tool :draw)
                        :on-change on-change
                        :on-ready on-ready
                        :on-tool-change on-tool-change})
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
