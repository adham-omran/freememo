(ns freememo.tape-table
  "The virtualized zebra table the Knowledge views share (Tape idiom).

   Fixed header table + virtual-scrolled body, the shape `library_cards`
   established. Callers own the columns, the rows and the empty message; this
   namespace owns the header, the scroll container, the row window and the
   empty-state overlay.

   Scroll-window comes from the app-local vendored copy, not the jar's: only it
   takes :reset-key, and without one it falls back to record-count, so a filter
   change between two results of equal count would leave the scroll position
   mid-list over an entirely different list. Tape is unchanged upstream and still
   comes from the jar."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.scroll :refer [Scroll-window]]
   [hyperfiddle.electric-scroll0 :refer [Tape]]))

(def row-height
  "Row height in px, matching library_cards: 12px padding + 28px line + 1px border."
  41)

(def ^:private visible-rows
  "Rows the scroll container shows before it scrolls.

   The cap is on the CONTAINER, never on the window: a window smaller than the
   container paints a blank band under the last row, because rows are absolutely
   positioned and the table keeps its full --count × --row-height height.
   Uncapped, the window grew with the viewport (~32 rows on a full-height desktop
   window) and every filter change re-mounted the lot."
  10)

(def ^:private overquery-factor
  "Buffer rows as a multiple of the visible window; 10 × 1.5 = 15 mounted rows,
   fixed, whatever the viewport height. Matches library_cards."
  1.5)

(def ^:private empty-rows
  "Minimum height of the scroll container, in rows, while the count is 0.

   The container takes its height from its content, and at count 0 that content
   is a table of height 0 and the empty overlay. The overlay is absolutely
   positioned, so it adds no height. With no minimum, the container shrinks to
   its 1px bottom border, and `overflow-y: auto` clips the message. Measured on
   2026-10-01 on the Settings AI costs card with an empty ledger: the body was
   1px tall, the overlay 48px tall, and no text showed.

   Two rows is 82px, so the overlay is 81px tall inside the 1px border. A line of
   the message is 18px, and the overlay's 24px top and bottom padding is inside
   that box, so up to 4 lines fit. At 360px width the Quiz messages wrap to 2
   lines and show in full (measured 2026-10-01).

   A parent that stretches the container needs none of this. The Knowledge views
   stretch it to its 410px cap, so their message always showed."
  2)

(e/defn TapeTable
  "Fixed header + virtual-scrolled body over `item-count` rows.

   The header is a sibling table ABOVE the scroll box, not a sticky child of it —
   the arrangement `library_cards` uses. That costs one alignment hazard and pays
   for it in CSS: the body reserves `scrollbar-gutter: stable`, so the header is
   wider than the body it labels by exactly the gutter, and every column boundary
   drifts. `.tape-table-header` compensates with `padding-right: 10px`, which is
   the gutter `.tape-scroll::-webkit-scrollbar` fixes. That width is a Blink and
   WebKit fact; Firefox picks its own, so alignment there is approximate by a few
   px. An earlier revision pinned the header INSIDE the scroll box to dodge this
   entirely — correct, but it made these tables the odd ones out.

   The body table is mounted at every count, INCLUDING 0, and `empty-message`
   paints as an absolutely positioned sibling overlay. A branch that swapped the
   table out would tear down Scroll-window, Tape and every per-row server flow,
   which races a cancelled e/Offload teardown and can crash the WS with
   :diff-corruption (observed in search_page). At count 0 the Tape still emits its
   window, every read past the end returns nil, and the rows render nothing.

   `reset-key` is what scrolls the body back to the top. Pass the view's
   filter/sort state, NOT the row count: a count changes for reasons that must not
   move the user (a background sweep marking rows, a delete), while a changed
   filter genuinely means a different list. nil keeps the upstream record-count
   fallback, which is right for a table with no filter of its own.

   Pre:  `Row` is an e/fn of [i] rendering one dom/tr, nil-safe past the end, and
         carrying `:style {:--order i}` for its own C1c positioning.
         `Row` is called from CLIENT scope, because it renders DOM — so it owns
         the siting of its own data access. A Row closing over a server-side
         collection MUST read it as `(e/server (nth coll i nil))`; a bare
         `(nth coll i nil)` pulls the whole collection to the client on mount and
         the window then buys nothing.
         The caller's container is a flex column with `min-height: 0`.
         `headers` and `grid-cols` describe the same columns, in the same order.
   Post: 15 rows are mounted whatever the viewport height; the container is
         `visible-rows × row-height` tall, less the 1px `.table-frame-body` bottom
         border, so it shows 10 rows with the last one clipped by that 1px;
         total scroll height equals item-count rows; the body table is mounted at
         every count; at count 0 the container is at least `empty-rows` rows
         tall, so `empty-message` shows; header and body column boundaries agree
         in Blink and WebKit."
  [headers grid-cols item-count reset-key empty-message Row]
  (e/client
    (dom/table
      (dom/props {:class "tape-table-header table-frame-head"
                  :style {:width "100%" :--grid-cols grid-cols}})
      (dom/thead
        (dom/props {:style {:display "contents"}})
        (dom/tr
          (dom/props {:style {:display "contents"}})
          (e/for [h (e/diff-by identity headers)]
            (dom/th (dom/text h))))))
    (dom/div
      ;; max-height caps the table at `visible-rows`, and the window follows it.
      ;; max-height, not height: a viewport shorter than 10 rows still shrinks the
      ;; table normally, and the window shrinks with it.
      ;; position:relative is inline, not on the shared .tape-scroll class — the
      ;; overlay below needs it and ten other sites share that class.
      ;; min-height is NOT here: it depends on item-count, so the props map
      ;; below owns it. Two maps that both set it race at mount.
      (dom/props {:style {:flex "1" :overflow-y "auto"
                          :max-height (str (* visible-rows row-height) "px")
                          :scrollbar-gutter "stable" :position "relative"}})
      (let [[offset limit] (Scroll-window row-height item-count dom/node
                             {:overquery-factor overquery-factor
                              :reset-key reset-key})]
        (dom/props {:class "tape-scroll table-frame-body"
                    ;; C1c per-row transform positioning (see .tape-scroll in
                    ;; index.css): --count → table height (the scroll range),
                    ;; --grid-cols → the row grid.
                    ;; min-height keeps the empty overlay visible at count 0
                    ;; (see `empty-rows`). At any other count it is 0, as it was.
                    :style {:--count item-count :--row-height (str row-height "px")
                            :--grid-cols grid-cols
                            :min-height (if (zero? item-count)
                                          (str (* empty-rows row-height) "px")
                                          "0")}})
        (dom/table
          (dom/props {:class "tape-table-body" :style {:width "100%"}})
          (e/for [i (Tape offset limit)]
            (Row i)))
        ;; Sibling overlay — never a branch that unmounts the table above.
        (when (zero? item-count)
          (dom/div
            (dom/props {:class "tape-table-empty"})
            (dom/text empty-message)))))))
