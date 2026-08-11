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

   LABELS are user-authored text, so an unlabelled diagram can be occluded at
   all: place a label, type into it, then drag a mask over it. They live on
   their OWN layer, mounted below the rect layer, which does three things at
   once — it matches the paint order of the generated svg (a mask covers a
   label), it keeps `read-rects`, marquee selection and select-all reading only
   the rect layer, and it lets the whole label layer stop listening while the
   :draw tool is active. That last one is load-bearing: a label that swallowed
   mousedown would block drawing a mask over it, which IS the flow.

   A label is never a card. Cards come from mask ordinals and only rects carry
   one, so `group-selection!` refuses to take a label into a mask group.

   Tools: :draw (drag on empty canvas draws a rect), :select (click selects,
   Shift-click adds, drag on empty canvas marquee-selects) and :text (click
   places a label and opens its editor). The caller picks the starting tool —
   draw when creating, select when editing an existing group.

   Operations: move/resize the selection via Transformer (rotation disabled),
   group (G) / ungroup (U) / select-all (A) / delete (Delete or Backspace), and
   S / R / T switch tool.

   API (mirrors quill_field's init/destroy lifecycle split):
     (init! {:container el :image-url s :rects [{:x :y :w :h :ordinal}]
             :elements [{:type \"text\" :x :y :size :text}]
             :tool :draw|:select|:text
             :on-change (fn [rects]) :on-ready (fn [{:keys [width height]}])
             :on-tool-change (fn [tool]) :on-selection-change (fn [n])
             :on-label-selected (fn [size-or-nil])})
       -> handle (atom), or nil when window.Konva is absent
     (read-rects handle)      -> current rects vector (save-time authoritative read)
     (read-elements handle)   -> current labels vector (same)
     (set-tool! handle tool)  (group-selection! handle)  (ungroup-selection! handle)
     (delete-selection! handle)  (set-label-size! handle size)
     (destroy! handle)

   on-selection-change reports how many nodes are selected, where a mask group
   counts as ONE. It fires from select-nodes!, the single funnel every
   selection write goes through, so a toolbar can follow rubber-band, click,
   select-all, tool switch and delete alike.

   on-label-selected reports the font size of the selected label, or nil, and is
   the AUTHORITATIVE value a size control binds to. It fires from
   report-label-size! rather than from select-nodes! alone, because a font size
   changes without the selection changing — a drag-resize does exactly that, and
   reporting only on selection left a bound control displaying a stale number.

   Rects carry :ordinal when their mask group existed before this session, and
   :gid when this session grouped them into a new mask group; a rect drawn here
   and left alone carries neither — the server assigns fresh ordinals on save."
  (:require [clojure.string :as str]
            [freememo.logging :as log]
            [freememo.occlusion-svg :as osvg]))

(def ^:private min-rect-px 8)     ; natural px — smaller drags are discarded
(def ^:private min-placed-label-size 10)  ; floor for a NEWLY placed label, which
                                          ; must be legible on a small image
(def ^:private rect-fill "#FFEBA2")
(def ^:private rect-stroke "#2D2D2D")
(def ^:private grouped-dash #js [6 3])   ; persistent cue: this rect is grouped

;; A label resizes uniformly — one fontSize cannot honour two axes — so its
;; Transformer offers corner anchors only and keeps the ratio. Masks keep the
;; full set and resize freely. Held as vectors and copied into a fresh array per
;; call: one shared #js array handed to Konva on every selection write would be
;; corrupted for every later call if Konva ever retained or mutated it.
(def ^:private label-anchors
  ["top-left" "top-right" "bottom-left" "bottom-right"])
(def ^:private mask-anchors
  ["top-left" "top-center" "top-right" "middle-right" "middle-left"
   "bottom-left" "bottom-center" "bottom-right"])

(defn- round2 [v]
  (/ (js/Math.round (* v 100)) 100))

(defn- konva [] (.-Konva js/window))

(defn- group-node? [^js node]
  (and node (= "Group" (.getClassName node))))

(defn- text-node? [^js node]
  (and node (= "Text" (.getClassName node))))

(defn- blank-label?
  "True when a label node holds no text.
   The ONE definition of the placeholder state, and both readers of it depend on
   this being the only one: close-label-editor! destroys such a node when its
   editor goes away, and read-elements skips one whose editor is still open. A
   blank label is invisible, unselectable, and rejected by the server."
  [^js node]
  (str/blank? (.text node)))

(defn- clamp-label-size
  "A font size a label may actually hold, in natural px.
   Post: a whole number inside [min-label-size, max-label-size]. Integral by
         design — the size control parses with parse-long, and the stored :size
         round-trips through the svg's font-size attribute.
   The single clamp: make-label!, normalize-text-transform! and set-label-size!
   are the three writers of fontSize and all three go through it."
  [size]
  (-> (or size osvg/min-label-size)
    (js/Math.round)
    (js/Math.max osvg/min-label-size)
    (js/Math.min osvg/max-label-size)))

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

(defn- node->element
  "One label in natural-image pixels.
   Pre:  node is a Konva Text on the label layer with an identity scale —
         normalize-text-transform! restores that after every resize.
   Post: :x/:y is the TOP-LEFT corner, which is both Konva.Text's origin and
         what freememo.occlusion-svg writes as the text element's (x, y), so
         nothing is measured or converted between the two. Keys match exactly
         what occlusion/checked-element stores, so a label read back unchanged
         is `=` to the geometry it was loaded from (the dirty check)."
  [^js node stage-scale]
  (let [pos (.getAbsolutePosition node)]
    {:type "text"
     :x (round2 (/ (.-x pos) stage-scale))
     :y (round2 (/ (.-y pos) stage-scale))
     :size (round2 (.fontSize node))
     :text (.text node)}))

(defn read-elements
  "Current labels (natural px) straight from the Konva nodes — the authoritative
   save-time read, and what the modal's dirty check compares.
   Post: a node whose text is blank is EXCLUDED. A freshly placed label is blank
         until its first keystroke, and the server rejects a blank label, so an
         in-flight placeholder must reach neither the dirty check nor the save."
  [handle]
  (when handle
    (let [{:keys [label-layer scale]} @handle]
      (when label-layer
        (->> (.find label-layer "Text")
          (remove blank-label?)
          (mapv (fn [^js n] (node->element n scale))))))))

(defn- emit! [handle]
  (let [{:keys [on-change]} @handle]
    (when on-change
      (on-change (read-rects handle)))))

;; ---------------------------------------------------------------------------
;; Selection
;; ---------------------------------------------------------------------------

(defn- selection-target
  "What clicking `node` selects: its mask group when it has one, else itself.
   Grouped rects move and resize together, so the group is the unit. A label is
   never in a group, so it always selects itself."
  [^js node]
  (let [parent (.getParent node)]
    (if (group-node? parent) parent node)))

(defn- selected-nodes [handle]
  (vec (.nodes ^js (:tr @handle))))

(defn- lone-selected-label
  "The selected label, when a label is the WHOLE selection — otherwise nil.
   The single definition of that condition. The Transformer configuration, the
   size report and set-label-size! all ask it, so a size control can never be
   enabled over a state the setter refuses, or the reverse."
  [handle]
  (let [nodes (selected-nodes handle)]
    (when (and (= 1 (count nodes)) (text-node? (first nodes)))
      (first nodes))))

(defn- report-label-size!
  "Tell the caller the selected label's font size, or nil.
   The single funnel for that signal. Selection writes, drag-resizes and the size
   control itself all route through it, so the number a size control displays can
   never disagree with the label — a resize used to leave it stale."
  [handle]
  (when-let [f (:on-label-selected @handle)]
    (let [^js lone (lone-selected-label handle)]
      (f (when lone (.fontSize lone))))))

(defn- select-nodes!
  "The one place the selection is written. Every caller — click, shift-click,
   rubber band, select-all, tool switch, group, ungroup, delete — comes through
   here, which is why on-selection-change only needs hooking once. It is also
   where the Transformer is configured for what is selected: a lone label
   resizes uniformly from its corners, everything else resizes freely."
  [handle nodes]
  (let [^js tr (:tr @handle)]
    (.nodes tr (into-array nodes))
    ;; Configured AFTER the write, so lone-selected-label reads the new
    ;; selection rather than a second copy of the same test.
    (let [lone (lone-selected-label handle)]
      (.keepRatio tr (some? lone))
      (.enabledAnchors tr (into-array (if lone label-anchors mask-anchors))))
    (when-let [f (:on-selection-change @handle)]
      (f (count nodes)))
    (report-label-size! handle)))

(defn- toggle-selected! [handle ^js node]
  (let [current (selected-nodes handle)]
    (select-nodes! handle
      (if (some #(= % node) current)
        (remove #(= % node) current)
        (conj current node)))))

(defn delete-selection!
  "Destroy the selected nodes and re-emit.
   Pre:  handle is live; an empty selection is a no-op.
   Post: a selected mask GROUP takes its members with it — the group node is
   the selection unit, so deleting it deletes the whole card. A selected label
   is destroyed the same way. Nothing reaches the database until the modal
   saves; reconcile then removes the matching flashcard rows and their Anki
   notes."
  [handle]
  (when handle
    (let [selection (selected-nodes handle)]
      (when (seq selection)
        (select-nodes! handle [])
        (doseq [^js node selection] (.destroy node))
        (emit! handle)))))

(defn- member-rects
  "The Rect nodes `node` stands for — itself, or a group's children."
  [^js node]
  (if (group-node? node) (vec (.getChildren node)) [node]))

;; ---------------------------------------------------------------------------
;; Transform normalization — Konva resizes by scaling; fold it back into
;; width/height (rects) or fontSize (labels) so stored geometry stays plain.
;; ---------------------------------------------------------------------------

(defn- normalize-rect-transform! [^js node]
  (let [w (* (.width node) (.scaleX node))
        h (* (.height node) (.scaleY node))]
    (.width node (js/Math.max min-rect-px w))
    (.height node (js/Math.max min-rect-px h))
    (.scaleX node 1)
    (.scaleY node 1)))

(defn- normalize-text-transform!
  "Fold a resize back into the font size.
   Pre:  scaleX = scaleY, which select-nodes! guarantees by giving a lone label
         a keepRatio Transformer with corner anchors only.
   Post: scale is identity and fontSize carries the resize, so node->element
         reads a plain size and the halo matches it. Reading scaleY is what
         makes the axis unambiguous — a font size is a vertical measure."
  [^js node]
  (let [s (.scaleY node)]
    (when (not= 1 s)
      (let [sz (clamp-label-size (* (.fontSize node) s))]
        (.fontSize node sz)
        (.strokeWidth node (osvg/halo-width sz))))
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
  (cond
    (group-node? node) (normalize-group-transform! node)
    (text-node? node) (normalize-text-transform! node)
    :else (normalize-rect-transform! node)))

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
  ;; parses correctly. Same reason at wire-marquee!, make-rect and make-label!.
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
   Pre:  two or more MASKS selected (a no-op otherwise).
   Post: all member rects live in one Konva group with identity transform and
         share a membership: the lowest existing ordinal, or a fresh gid when
         none of them has been saved. Nothing moves on screen.
   Labels are dropped from the selection first. Shift-click can mix a label
   with masks, and taking one in would move it off the label layer — out of
   read-elements entirely — and stamp a mask ordinal on it."
  [handle]
  (when handle
    (let [selection (filterv (complement text-node?) (selected-nodes handle))]
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
;; Label text editor — a textarea over the canvas
;; ---------------------------------------------------------------------------

(defn- close-label-editor!
  "Remove the live label editor, if any. Idempotent and re-entrant safe:
   removing a focused textarea can fire blur again, and the handle key is
   dissoc'd first so the second call finds nothing.
   Post: the node is visible again, and a node still holding no text is
         destroyed — a label with no text is not a label, and leaving it would
         put an invisible unselectable node on the layer."
  [handle]
  (when-let [{:keys [textarea node]} (:label-editor @handle)]
    (swap! handle dissoc :label-editor)
    (.remove ^js textarea)
    (.visible ^js node true)
    (when (blank-label? node)
      (select-nodes! handle [])
      (.destroy ^js node))
    (emit! handle)))

(defn- open-label-editor!
  "Mount a textarea over `node` and focus it.
   Pre:  the stage is built, so :scale and :container are known, and node is a
         Konva Text on the label layer.
   Post: exactly one editor is open, the node is hidden while it is, and every
         keystroke the node's text follows the textarea immediately — the save
         and the dirty check both read the NODE, so nothing depends on a commit
         happening. Escape and blur only tear the textarea down.
   Blame: calling this before build-stage! is a caller bug (scale is nil)."
  [handle ^js node]
  (close-label-editor! handle)
  (let [{:keys [container scale]} @handle
        pos (.absolutePosition node)
        fs (* (.fontSize node) scale)
        textarea (js/document.createElement "textarea")
        st (.-style textarea)]
    (set! (.-value textarea) (.text node))
    (set! (.-position st) "absolute")
    (set! (.-left st) (str (.-x pos) "px"))
    (set! (.-top st) (str (.-y pos) "px"))
    (set! (.-width st) (str (js/Math.max 140 (* (.width node) scale)) "px"))
    (set! (.-height st) (str (js/Math.max (* fs 1.8) (* (.height node) scale)) "px"))
    (set! (.-fontSize st) (str fs "px"))
    (set! (.-fontFamily st) osvg/label-font-family)
    (set! (.-lineHeight st) (str osvg/label-line-height))
    (set! (.-color st) osvg/label-fill)
    (set! (.-background st) "rgba(255,255,255,0.96)")
    (set! (.-border st) "1px solid #2D6BFF")
    (set! (.-margin st) "0")
    (set! (.-padding st) "1px")
    (set! (.-outline st) "none")
    (set! (.-resize st) "none")
    (set! (.-overflow st) "hidden")
    (set! (.-zIndex st) "2")
    (.addEventListener textarea "input"
      (fn [_] (.text node (.-value textarea))))
    (.addEventListener textarea "keydown"
      (fn [e]
        ;; Two ancestors listen for keys and neither may see a keystroke meant
        ;; for this textarea: wire-keys! on the container (where a plain letter
        ;; switches tool) and the modal's ModalEscape (where Escape closes the
        ;; whole modal, losing the label and every other edit).
        ;;
        ;; Modified Enter is the ONE exception, and it has to be: the modal binds
        ;; Cmd/Ctrl+Enter to submit and it works from every Quill field on the
        ;; same dialog. Swallowing it here would make this the one text surface
        ;; where that chord silently does nothing. Safe to let through — the save
        ;; reads the node, whose text this handler's sibling keeps current, and
        ;; wire-keys! ignores modified keys.
        (let [mod-enter? (and (= "Enter" (.-key e))
                           (or (.-metaKey e) (.-ctrlKey e)))]
          (when-not mod-enter?
            (.stopPropagation e))
          (when (= "Escape" (.-key e))
            (.preventDefault e)
            (close-label-editor! handle)))))
    ;; The container focuses ITSELF on pointerdown (wire-keys!, so the editor
    ;; owns the keyboard). Left to bubble, a click inside this textarea would
    ;; move focus to the container, blur the textarea, and tear the editor down
    ;; mid-edit — so clicking to reposition the caret closed the box.
    (.addEventListener textarea "pointerdown" (fn [e] (.stopPropagation e)))
    (.addEventListener textarea "mousedown" (fn [e] (.stopPropagation e)))
    (.addEventListener textarea "blur" (fn [_] (close-label-editor! handle)))
    (.visible node false)
    (.appendChild container textarea)
    (swap! handle assoc :label-editor {:textarea textarea :node node})
    ;; Imperative focus, deliberately, and synchronous rather than through a
    ;; requestAnimationFrame. HTML :autofocus does not fire on a dynamically
    ;; inserted element — Forms5 says so in its own source and does the same
    ;; thing (electric_forms5.cljc, Input!: ":autofocus only works on page load,
    ;; not dynamic insertion"). rAF would be worse than useless here: it would
    ;; land after the pointerdown that created this node has finished, i.e. after
    ;; the container has taken focus for itself.
    (.focus textarea)
    (.select textarea)))

;; ---------------------------------------------------------------------------
;; Nodes and stage wiring
;; ---------------------------------------------------------------------------

(defn- attach-node-handlers!
  "Selection, drag and resize handling shared by masks and labels."
  [handle ^js node]
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
      ;; A resize changes a label's fontSize without changing the selection, so
      ;; the size signal has to be re-reported here or a size control keeps
      ;; showing the size the label had when it was selected.
      (report-label-size! handle)
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
    (attach-node-handlers! handle node)
    node))

(defn- make-label!
  "One label node, drawn exactly as freememo.occlusion-svg will emit it.
   The halo deliberately does NOT set strokeScaleEnabled false: its width is in
   natural px and must shrink with the stage, just as it scales with the pushed
   svg. fillAfterStrokeEnabled is Konva's paint-order=\"stroke fill\"."
  [handle {:keys [x y size text]}]
  (let [Konva (konva)
        sz (clamp-label-size size)
        ^js node (new (.-Text Konva)
                   #js {:x x :y y
                        :text (or text "")
                        :fontSize sz
                        :fontFamily osvg/label-font-family
                        :lineHeight osvg/label-line-height
                        :fill osvg/label-fill
                        :stroke osvg/label-halo
                        :strokeWidth (osvg/halo-width sz)
                        :fillAfterStrokeEnabled true
                        :draggable true})]
    (attach-node-handlers! handle node)
    (.on node "dblclick dbltap"
      (fn [^js e]
        (set! (.-cancelBubble e) true)
        (open-label-editor! handle node)))
    node))

(defn- default-label-size
  "Font size for a newly placed label, in natural px.
   Post: proportional to image width, so one label reads the same relative to
         the image at any resolution, and never below min-placed-label-size.
         The divisor reproduces the add-on's 24 on a 742px-wide image."
  [handle]
  (let [w (or (:width (:natural @handle)) 0)]
    (js/Math.max min-placed-label-size (js/Math.round (/ w 30)))))

(defn set-label-size!
  "Set the selected label's font size, in natural px.
   Pre:  a label is the whole selection (a no-op otherwise) and size is a
         positive number.
   Post: fontSize is size clamped by clamp-label-size and the halo is rescaled
         with it — freememo.occlusion-svg derives the same width from the same
         size, so the editor and the pushed svg stay one picture. The clamped
         size is reported back, so a control bound to that signal shows what the
         label actually took rather than what was asked for."
  [handle size]
  (when (and handle (number? size) (pos? size))
    (when-let [^js node (lone-selected-label handle)]
      (let [sz (clamp-label-size size)]
        (.fontSize node sz)
        (.strokeWidth node (osvg/halo-width sz))
        (report-label-size! handle)))))

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
  "Switch tool. Post: @handle :tool = tool, any open label editor is torn down,
   the label layer listens iff the tool is not :draw, the selection is cleared,
   and on-tool-change has been told (so a toolbar can follow keyboard switches)."
  [handle tool]
  (when handle
    (let [{:keys [on-tool-change label-layer]} @handle]
      (when (not= tool (:tool @handle))
        (close-label-editor! handle)
        (swap! handle assoc :tool tool)
        ;; A listening label would swallow the mousedown that starts a mask
        ;; drawn OVER it — which is the whole point of placing one.
        (when label-layer (.listening ^js label-layer (not= :draw tool)))
        (select-nodes! handle [])
        (when on-tool-change (on-tool-change tool))))))

(defn- natural-pointer [^js stage scale]
  (when-let [pos (.getPointerPosition stage)]
    {:x (/ (.-x pos) scale) :y (/ (.-y pos) scale)}))

(defn- empty-canvas-target?
  "True when the event landed on the stage or the image, i.e. not on a mask or a
   label. Rect and label mousedown both cancel bubbling, so this is the
   'nothing there' test."
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

(defn- wire-text!
  "Click on empty canvas places a label and opens its editor (:text tool only).
   The stage handler is enough because a click on an existing label cancels
   bubbling in attach-node-handlers!, so it never reaches here."
  [handle]
  (let [{:keys [scale]} @handle
        ^js stage (:stage @handle)]
    (.on stage "mousedown touchstart"
      (fn [e]
        (when (and (= :text (:tool @handle)) (empty-canvas-target? stage e))
          (close-label-editor! handle)
          (select-nodes! handle [])
          (when-let [{:keys [x y]} (natural-pointer stage scale)]
            (let [^js label-layer (:label-layer @handle)
                  node (make-label! handle {:x x :y y
                                            :size (default-label-size handle)
                                            :text ""})]
              (.add label-layer node)
              (open-label-editor! handle node))))))))

(defn- wire-marquee!
  "Drag on empty canvas rubber-band selects (:select tool only). Masks only —
   the hit test reads the rect layer's children, so a label is never caught by
   a band and can never be dragged into a mask group by one."
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

(defn- prepare-container!
  "Every property the editor requires of its host element, in one place.
   Post: the container is focusable with no focus ring (wire-keys! scopes the
         editor's shortcuts to it) and is a positioned ancestor (a label's
         textarea is absolutely placed inside it).
   These are imperative on purpose. The caller declares the container's LOOK in
   dom/props; these three are load-bearing behaviour the editor cannot borrow
   from a stylesheet, and keeping them here means one owner rather than a
   requirement split across two namespaces with nothing enforcing it."
  [^js container]
  (.setAttribute container "tabindex" "0")
  (set! (.-outline (.-style container)) "none")
  (set! (.-position (.-style container)) "relative"))

(defn- wire-keys!
  "Editor shortcuts, scoped to the editor container (focused on pointerdown) so
   the modal's Quill fields keep every key to themselves — the app's global
   handler treats plain letters as global, which is why these must not be
   registered there. Modified keys are left to the app. A keystroke typed into
   a label's textarea never arrives: that handler stops propagation."
  [handle]
  (let [{:keys [container]} @handle
        key-handler
        (fn [e]
          (when-not (or (.-metaKey e) (.-ctrlKey e) (.-altKey e))
            (case (.toLowerCase (.-key e))
              ("delete" "backspace") (do (.preventDefault e) (delete-selection! handle))
              "s" (do (.preventDefault e) (set-tool! handle :select))
              "r" (do (.preventDefault e) (set-tool! handle :draw))
              "t" (do (.preventDefault e) (set-tool! handle :text))
              "g" (do (.preventDefault e) (group-selection! handle))
              "u" (do (.preventDefault e) (ungroup-selection! handle))
              ;; Masks only, deliberately: select-all feeds Group and Delete,
              ;; and a label must never reach Group.
              "a" (do (.preventDefault e)
                    (select-nodes! handle (vec (.getChildren ^js (:rect-layer @handle)))))
              nil)))]
    (.addEventListener container "pointerdown" (fn [_] (.focus container)))
    (.addEventListener container "keydown" key-handler)
    (swap! handle assoc :key-handler key-handler)))

(defn- build-stage!
  "Build the stage once the image element has loaded.
   Layer order is image, label, rect, transformer — the label layer sits under
   the rect layer so a mask covers a label, matching the paint order of the
   generated svg (Labels group before Masks group)."
  [handle img]
  (let [{:keys [container rects elements on-ready tool]} @handle
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
        label-layer (new (.-Layer Konva))
        rect-layer (new (.-Layer Konva))
        tr-layer (new (.-Layer Konva))
        tr (new (.-Transformer Konva)
             #js {:rotateEnabled false
                  :flipEnabled false
                  :ignoreStroke true})]
    (prepare-container! container)
    (.scale stage #js {:x scale :y scale})
    (.add image-layer (new (.-Image Konva)
                        #js {:image img :x 0 :y 0 :width nw :height nh}))
    (.add tr-layer tr)
    (.add stage image-layer)
    (.add stage label-layer)
    (.add stage rect-layer)
    (.add stage tr-layer)
    (swap! handle assoc
      :stage stage :label-layer label-layer :rect-layer rect-layer
      :tr-layer tr-layer :tr tr
      :scale scale :natural {:width nw :height nh})
    (doseq [element elements]
      (.add label-layer (make-label! handle element)))
    (.listening ^js label-layer (not= :draw tool))
    (let [nodes (mapv (fn [r]
                        (let [node (make-rect handle r)]
                          (.add rect-layer node)
                          node))
                  rects)]
      (rebuild-mask-groups! handle nodes))
    (wire-draw! handle)
    (wire-text! handle)
    (wire-marquee! handle)
    (wire-keys! handle)
    (when on-ready (on-ready {:width nw :height nh}))))

(defn init!
  "Create the editor inside `container`. Returns a handle atom, or nil when
   Konva is not loaded. The stage is built async once the image loads."
  [{:keys [container image-url rects elements tool on-change on-ready
           on-tool-change on-selection-change on-label-selected]}]
  (when (and container (.-Konva js/window))
    (let [handle (atom {:container container
                        :rects (vec rects)
                        :elements (vec elements)
                        :tool (or tool :draw)
                        :on-change on-change
                        :on-ready on-ready
                        :on-tool-change on-tool-change
                        :on-selection-change on-selection-change
                        :on-label-selected on-label-selected})
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
  "Tear down the stage and container listeners. Idempotent.
   The label editor is torn down FIRST: its textarea is a child of the
   container, not of the stage, so destroying the stage would leave it behind."
  [handle]
  (when handle
    (when (:label-editor @handle)
      (let [{:keys [textarea]} (:label-editor @handle)]
        (swap! handle dissoc :label-editor)
        (.remove ^js textarea)))
    (let [{:keys [container key-handler]} @handle
          ^js stage (:stage @handle)]
      (when (and container key-handler)
        (.removeEventListener container "keydown" key-handler))
      (when stage (.destroy stage))
      (reset! handle nil))))
