(ns freememo.selection-placement
  "Pure placement arithmetic for the selection popover.
   Prefer a position clear of selected text. The editor menu permits overlap
   when no clear position fits. Other callers retain the clearance rule.")

(def gap
  "Pixels between the selection edge and the popover."
  8)

(def margin
  "Pixels kept clear of the viewport edges."
  8)

(def touch-reserve
  "Pixels kept clear above AND below the selection on a touch device, on top of
   `gap`. iOS Safari and Android Chrome draw their own Cut/Copy/Paste bar beside
   a selection — above it when there is room, else below — and a page cannot
   move it, read its rect, or suppress it for a contenteditable. Measured on an
   iPhone: the bar ends about 60px below the selection's last row, handle
   included. Reserving the band on both sides clears it whichever side the
   browser picks."
  60)

(defn- clamp [lo hi v]
  (max lo (min v hi)))

(defn place
  "Return a menu position, or nil when the selection is outside the view.
   If no clear position fits, return nil unless :allow-overlap? is true.

   `opts` (all lengths in viewport pixels):
     :selection {:top :bottom :left :right} the selection's whole bounding box
     :focus-x                    x of the end the user dragged to
     :rtl?                       the focus line resolves right-to-left
     :container {:top :bottom :left :right} the Quill container
     :viewport  {:width :height} window inner size
     :menu      {:width :height} measured popover size
     :reserve                    extra clearance on each side of the selection,
                                 for the browser's own selection bar (default 0;
                                 the caller passes `touch-reserve` on touch)
     :allow-overlap?             keep the menu visible if no clear position fits

   Five tiers, first fit wins:
     1. below the selection, inside the container
     2. above the selection, inside the container
     3. below the VISIBLE selection, inside the viewport — overlaps whatever
        sits under the editor, which the contract permits
     4. beside the visible selection, inside the viewport
     5. over the editor, clamped to the viewport

   The optional final tier keeps Select All usable when the selection fills the pane.
   Only that tier may cover selected text. Missing horizontal bounds mean the
   selection spans the viewport.

   Pre: menu fits the viewport. Container bounds are ordered.
   Post: :side is :below, :above, :left, :right, or :overlap."
  [{:keys [selection focus-x rtl? container viewport menu reserve allow-overlap?]}]
  (let [clearance (+ gap (or reserve 0))
        {sel-top :top sel-bottom :bottom sel-left :left sel-right :right} selection
        {con-top :top con-bottom :bottom con-left :left con-right :right} container
        {view-w :width view-h :height} viewport
        {menu-w :width menu-h :height} menu
        viewport-bottom (- view-h margin)
        top-limit (max con-top margin)
        bottom-limit (min con-bottom viewport-bottom)
        visible-left (max (or sel-left 0) (or con-left 0) margin)
        visible-right (min (or sel-right view-w) (or con-right view-w) (- view-w margin))]
    ;; Out of view: no selected row is both inside the container and on screen,
    ;; so there is nothing to anchor to and nothing the user can read.
    (when (and (< top-limit bottom-limit)
               (< visible-left visible-right)
               (> sel-bottom top-limit) (< sel-top bottom-limit))
      (let [visible-top (max sel-top top-limit)
            visible-bottom (min sel-bottom bottom-limit)
            anchor-left (clamp margin (- view-w menu-w margin)
                          (if rtl? (- focus-x menu-w) focus-x))
            fallback-top (clamp (min top-limit (- viewport-bottom menu-h))
                           (- viewport-bottom menu-h) visible-top)
            below-in-container (+ sel-bottom clearance)
            above-in-container (- sel-top clearance menu-h)
            below-in-viewport (+ visible-bottom clearance)
            beside-left (- visible-left clearance menu-w)
            beside-right (+ visible-right clearance)
            [top left side] (cond
                         (<= (+ below-in-container menu-h) bottom-limit)
                         [below-in-container anchor-left :below]

                         (>= above-in-container top-limit)
                         [above-in-container anchor-left :above]

                         (<= (+ below-in-viewport menu-h) viewport-bottom)
                         [below-in-viewport anchor-left :below]

                         (>= beside-left margin)
                         [fallback-top beside-left :left]

                         (<= (+ beside-right menu-w) (- view-w margin))
                         [fallback-top beside-right :right]

                         allow-overlap? [fallback-top anchor-left :overlap]

                         :else nil)]
        (when top
          {:top (max margin top) :left left :side side})))))
