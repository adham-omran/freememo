(ns freememo.selection-placement
  "Where a selection-anchored popover goes so it never covers the selection.

   Pure rectangle arithmetic, no DOM. `.cljc` with no requires so `test/` can
   reach it — the project has a Clojure test harness only, and Playwright's
   synthetic scroll/selection reproduce none of this geometry, so this file is
   the only part of the placement that a test can check at all.

   `freememo.format-menu` is the one caller: it measures the rects, calls
   `place`, and applies the result.

   Inv : the returned rect never intersects the rows of the selection that are
         visible inside the container. The vertical tiers carry that alone, so a
         degraded `focus-x` costs alignment and never the invariant.")

(def gap
  "Pixels between the selection edge and the popover."
  8)

(def margin
  "Pixels kept clear of the viewport edges."
  8)

(defn- clamp [lo hi v]
  (max lo (min v hi)))

(defn place
  "Position for a popover anchored to a selection, or nil for \"show nothing\".

   `opts` (all lengths in viewport pixels):
     :selection {:top :bottom}   the selection's whole bounding box
     :focus-x                    x of the end the user dragged to
     :rtl?                       the focus line resolves right-to-left
     :container {:top :bottom}   the editor element the selection lives in
     :viewport  {:width :height} window inner size
     :menu      {:width :height} measured popover size

   Four tiers, first fit wins:
     1. below the selection, inside the container
     2. above the selection, inside the container
     3. below the VISIBLE selection, inside the viewport — overlaps whatever
        sits under the editor, which the contract permits
     4. none — return nil, and the caller hides the popover

   There is deliberately no above-the-container tier: the container top is the
   floor that keeps the popover clear of the app command bar.

   The container is clipped to the viewport before anything else reads it, and
   every tier inherits that one clip. A container whose own rect leaves the
   viewport — an ancestor scrolled it, a modal inside a scrolling page — bounds
   nothing on its far side, so an unclipped `con-top` lets the above-tier return
   a position off the top of the screen while the user can see selected text.
   Clipping also makes the visible band honest: a selected row inside the
   container but outside the viewport is not visible either.

   Pre  : container top <= container bottom; menu width/height > 0.
   Post : nil, or {:top :left :side} with :side in #{:below :above}, and
          :top >= margin.
   Inv  : the returned [top, top+height] band never overlaps the visible
          selection band [max(sel-top, top-limit), min(sel-bottom, bottom-limit)]."
  [{:keys [selection focus-x rtl? container viewport menu]}]
  (let [{sel-top :top sel-bottom :bottom} selection
        {con-top :top con-bottom :bottom} container
        {view-w :width view-h :height} viewport
        {menu-w :width menu-h :height} menu
        viewport-bottom (- view-h margin)
        top-limit (max con-top margin)
        bottom-limit (min con-bottom viewport-bottom)]
    ;; Out of view: no selected row is both inside the container and on screen,
    ;; so there is nothing to anchor to and nothing the user can read.
    (when (and (> sel-bottom top-limit) (< sel-top bottom-limit))
      (let [visible-bottom (min sel-bottom bottom-limit)
            below-in-container (+ sel-bottom gap)
            above-in-container (- sel-top gap menu-h)
            below-in-viewport (+ visible-bottom gap)
            [top side] (cond
                         (<= (+ below-in-container menu-h) bottom-limit)
                         [below-in-container :below]

                         (>= above-in-container top-limit)
                         [above-in-container :above]

                         (<= (+ below-in-viewport menu-h) viewport-bottom)
                         [below-in-viewport :below]

                         :else [nil nil])]
        (when top
          {:top top
           :left (clamp margin (- view-w menu-w margin)
                   (if rtl? (- focus-x menu-w) focus-x))
           :side side})))))
