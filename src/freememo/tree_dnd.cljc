(ns freememo.tree-dnd
  "Shared native HTML5 drag-and-drop wiring for the two topic trees — the
   library documents view (knowledge_tree) and the viewer hierarchy panel.
   Re-parenting a topic = dragging its row onto another topic's row. The actual
   move + undo lives in freememo.topic-move / freememo.db; this only wires the
   DOM events and the valid/invalid drop affordance.

   Contract — call DragDropRow! inside the row's first boxed cell (a `dom/td`),
   NOT the `dom/tr`: these grid-tables give the tr `display:contents`, so the tr
   has no layout box. dom/node must be that td. DragDropRow! wires the drop-TARGET
   side onto that td (dragover/drop/data-drop) and renders a `.drag-grip` handle
   as the cell's first child carrying the drag-SOURCE side (draggable +
   dragstart/dragend). The grip — not the cell text — is the only draggable
   element, so the title stays selectable and a click-drag over it never turns
   into a drag.
     my-id      this row's topic-id
     draggable? false for structural rows that must not move (e.g. PDF pages);
                such rows render no grip (they stay drop targets only)
     !drag-src  client atom holding the dragged id (nil when idle); dragstart
                writes it, dragend clears it
     drag-src   (e/watch !drag-src)
     forbidden  set of ids that are invalid targets — the dragged node's subtree;
                computed server-side from !drag-src on drag start (one round-trip)
     !drop-cmd  client atom the drop writes {:src :dst} into; the parent's
                e/Token fires the server move and clears it

   Visual block: invalid targets never preventDefault on dragover, so the browser
   shows its native no-drop cursor; valid targets get [data-drop=valid] for a CSS
   highlight. Until `forbidden` arrives (brief gap after dragstart) a descendant
   may look valid — move-topic! re-checks the cycle server-side and rejects, so
   the gap is a cosmetic best-effort, never a correctness hole."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(e/defn DragDropRow! [my-id draggable? !drag-src drag-src forbidden !drop-cmd]
  (e/client
    (let [active? (some? drag-src)
          source? (= drag-src my-id)
          valid?  (and active? (not source?) (not (contains? forbidden my-id)))]
      ;; Drop-TARGET side — stays on the cell (dom/node = the td). The cell text
      ;; is NOT draggable, so a click-drag over the title selects text as usual.
      (dom/props {:data-drop (when active? (if valid? "valid" "invalid"))})
      (dom/On "dragover"
        (fn [e]
          (when valid?
            (.preventDefault e)
            (set! (.. e -dataTransfer -dropEffect) "move")))
        nil)
      (dom/On "drop"
        (fn [e]
          (when valid?
            (.preventDefault e)
            (reset! !drop-cmd {:src drag-src :dst my-id})))
        nil)
      ;; Drag-SOURCE side — a dedicated grip handle so the title text stays
      ;; selectable. Structural rows (draggable? false) render no grip.
      ;; `draggable` is an enumerated attribute: it needs the literal "true"
      ;; (a boolean true renders as draggable="" → "auto" → not draggable).
      (when draggable?
        (dom/span
          (dom/props {:class "drag-grip" :draggable "true"
                      :aria-label "Drag to re-nest"})
          (dom/text "⠿")
          ;; Grabbing the grip must not bubble to the row's navigate click.
          (dom/On "mousedown" (fn [e] (.stopPropagation e)) nil)
          (dom/On "click" (fn [e] (.stopPropagation e)) nil)
          (dom/On "dragstart"
            (fn [e]
              (.setData (.-dataTransfer e) "text/plain" (str my-id))
              (set! (.. e -dataTransfer -effectAllowed) "move")
              ;; Custom drag image = the whole row cell, not the bare glyph, so
              ;; the ghost shows what is being moved.
              (let [cell (.closest (.-currentTarget e) "td")]
                (when cell (.setDragImage (.-dataTransfer e) cell 0 0)))
              (reset! !drag-src my-id))
            nil)
          (dom/On "dragend" (fn [_] (reset! !drag-src nil)) nil))))))
