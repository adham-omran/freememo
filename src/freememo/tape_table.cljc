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
   comes from the jar.

   Curation views compose their own header row from `SelectAllHeader`,
   `SortHeader` and `PlainHeader`, put `SelectCell` first in each row, and open
   a `BatchBar` above the table while rows are selected
   (plans/knowledge-tables-selection-and-bulk.md §1). The view owns the selection
   atom and the sort atoms; these components only read and write them."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.a11y :as a11y]
   [freememo.scroll :refer [Scroll-window]]
   [hyperfiddle.electric-scroll0 :refer [Tape]]))

(defn sort-rows
  "Rows ordered by `key-fn`, rows whose key is nil last in either direction.
   Strings compare without case. Runs on the server, over the whole result, so a
   sort never reorders only the window the client holds.
   Pre:  `dir` is :asc or :desc; key-fn returns values of one comparable type.
   Post: a vector holding every row once; equal keys keep their input order."
  [rows key-fn dir]
  (let [k (fn [row] (let [v (key-fn row)] (if (string? v) (str/lower-case v) v)))
        present (filterv #(some? (key-fn %)) rows)
        absent (filterv #(nil? (key-fn %)) rows)]
    (into (vec (sort-by k (if (= dir :desc) #(compare %2 %1) compare) present))
      absent)))

(defn selection-counts
  "How many of `selected` the result holds, and how many it does not.
   Pre:  `rows` is the view's whole server-side result; `selected` a set of ids.
   Post: {:shown n :hidden m}, n + m = (count selected)."
  [rows id-key selected]
  (let [shown (if (seq selected)
                (count (filter #(contains? selected (get % id-key)) rows))
                0)]
    {:shown shown :hidden (- (count selected) shown)}))

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

(e/defn HeadedTapeTable
  "`TapeTable` with a header row the caller renders: `Head` is an e/fn of no
   arguments that mounts one `dom/th` per column of `grid-cols`, in order.
   `HeadOverlay` is nil or an e/fn of no arguments whose content is laid over
   the header row — the slot a `BatchBar` goes in, so opening it moves nothing.
   Everything else — Pre, Post, the window, the empty overlay — is TapeTable's."
  [Head HeadOverlay grid-cols item-count reset-key empty-message Row]
  (e/client
    (dom/div
      (dom/props {:class "tape-table-head-frame"})
      (dom/table
        (dom/props {:class "tape-table-header table-frame-head"
                    :style {:width "100%" :--grid-cols grid-cols}})
        (dom/thead
          (dom/props {:style {:display "contents"}})
          (dom/tr
            (dom/props {:style {:display "contents"}})
            (Head))))
      (when HeadOverlay (HeadOverlay)))
    (dom/div
      ;; max-height caps the table at `visible-rows`, and the window follows it.
      ;; max-height, not height: a viewport shorter than 10 rows still shrinks the
      ;; table normally, and the window shrinks with it.
      ;; position:relative is inline, not on the shared .tape-scroll class — the
      ;; overlay below needs it and ten other sites share that class.
      ;; min-height is NOT here: it depends on item-count, so the props map
      ;; below owns it. Two maps that both set it race at mount.
      ;; `tape-table-scroll` carries the loading placeholder (index.css). It is
      ;; not `.tape-scroll`, because that class is shared with virtual-scroll
      ;; sites that are not TapeTable. Electric counts class tokens per node, so
      ;; two maps can both set :class.
      (dom/props {:class "tape-table-scroll"
                  :style {:flex "1" :overflow-y "auto"
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
         in Blink and WebKit; while the body holds no row and no empty message —
         the count or the first row reads are still pending — CSS paints
         skeleton rows on `.tape-table-scroll` (index.css), with no DOM node."
  [headers grid-cols item-count reset-key empty-message Row]
  (e/client
    (HeadedTapeTable
      (e/fn [] (e/for [h (e/diff-by identity headers)]
                 (dom/th (dom/text h))))
      nil grid-cols item-count reset-key empty-message Row)))

;; ── Curation pieces: header cells, the select cell, the batch bar ──────────

(e/defn PlainHeader [label]
  (e/client (dom/th (dom/text label))))

(e/defn SortHeader
  "A header that sorts by `col`. The first click sorts by `default-dir`; each
   later click on the same column flips the direction. Only the sorted column
   shows an arrow.
   Pre:  `!order` is the view's one sort atom, holding [col dir] or [nil nil];
         `order` is its watched value. One atom, not a column atom and a direction
         atom: two atoms reach a reader in separate propagations, and a reader
         that saw a new column with no direction yet crashed the client.
   Post: a click writes [col dir] in one swap; the view's query reads it."
  [label col default-dir order !order]
  (e/client
    (let [[sort-col sort-dir] order
          sort! (fn [_]
                  (swap! !order (fn [[c d]]
                                 (if (= c col)
                                   [c (if (= d :asc) :desc :asc)]
                                   [col default-dir]))))]
      (dom/th
        (dom/props {:class "tape-th-sort"
                    :aria-sort (if (= sort-col col)
                                 (if (= sort-dir :asc) "ascending" "descending")
                                 "none")})
        (a11y/KeyActivate {:label (str "Sort by " label)} sort!)
        (dom/text (str label (when (= sort-col col) (if (= sort-dir :asc) " \u25B2" " \u25BC"))))
        (dom/On "click" sort! nil)))))

#?(:cljs
   (defn- set-check-state!
     "Write a checkbox's checked and mixed states. Imperative: a reactive
      :checked prop fights the browser's own toggle on click."
     [^js node checked? mixed?]
     (set! (.-checked node) (boolean checked?))
     (set! (.-indeterminate node) (boolean mixed?))
     nil)
   :clj (defn- set-check-state! [_node _checked? _mixed?] nil))

(e/defn SelectAllHeader
  "The header checkbox. It selects every row the filter matches — the whole
   server-side result, not the window — and a second click clears exactly those.
   It shows the mixed state while some, not all, of them are selected.

   The id vector crosses only inside the click token, so a keystroke in search
   never sends it to the client (library_cards/SelectAllCell). `e/snapshot` is
   load-bearing: the swap changes `selected-shown`, which feeds this frame's own
   test, and a live read would re-run the other branch and undo the click.
   Pre:  `rows` is the server-sited result, `id-key` the key of its ids,
         `selected-shown` the server count of selected ids among `rows`.
   Post: after a click, `!selected` holds every id of `rows`, or none of them."
  [rows id-key item-count selected-shown !selected]
  (e/client
    (dom/th
      (dom/props {:class "tape-th-check"})
      (dom/input
        (dom/props {:type "checkbox" :disabled (zero? item-count)
                    :aria-label "Select every row that matches the filter"})
        (let [all? (and (pos? item-count) (= item-count selected-shown))]
          (set-check-state! dom/node all? (and (pos? selected-shown) (not all?)))
          (let [[t _] (e/Token (dom/On "click" identity nil))]
            (when t
              (let [select-all? (not (e/snapshot all?))
                    ids (e/server (mapv id-key rows))]
                (case (if select-all?
                        (swap! !selected into ids)
                        (swap! !selected #(reduce disj % ids)))
                  (t))))))))))

(e/defn SelectCell
  "The first cell of a selectable row. stopPropagation, so a tick never also
   activates the row. `checked` is set imperatively: rows are recycled as the
   window moves, so the cell must follow `selected`, never its own last click."
  [id label !selected selected]
  (e/client
    (dom/td
      (dom/props {:class "tape-cell-check"})
      (dom/input
        (dom/props {:type "checkbox" :aria-label (str "Select " label)})
        (set-check-state! dom/node (contains? selected id) false)
        (dom/On "click"
          (fn [e]
            (.stopPropagation e)
            (swap! !selected #(if (contains? % id) (disj % id) (conj % id))))
          nil)))))

(e/defn BatchBar
  "The bar a selection opens over the table's header row: the count, the
   view's bulk actions, and Cancel. It goes in `HeadedTapeTable`'s
   `HeadOverlay` slot and leaves the select-all checkbox uncovered, so ticking
   the first row moves nothing — a bar above the table pushed every row down.
   `hidden` counts selected rows the current filter does not list — a filter
   change keeps the selection, so the bar must say what the actions reach
   (plans/knowledge-tables-selection-and-bulk.md O1).
   Pre:  `Actions` is an e/fn of no arguments mounting the bulk buttons.
   Post: Cancel empties `!selected`, which unmounts the bar."
  [n hidden !selected Actions]
  (e/client
    (when (pos? n)
      (dom/div
        (dom/props {:class "tape-batch-bar" :role "region" :aria-label "Selected rows"})
        (dom/span
          (dom/props {:class "tape-batch-count"})
          (dom/text (str n " selected"
                      (when (pos? hidden) (str ", " hidden " not in this list")))))
        (Actions)
        (dom/button
          (dom/props {:type "button" :class "btn btn-sm tape-batch-cancel"})
          (dom/text "Cancel")
          (dom/On "click" (fn [_] (reset! !selected #{})) nil))))))

(e/defn FilterLabel
  "One active filter, shown as a label that removes the filter on click
   (PatternFly filter labels). `clear!` is a plain fn."
  [text clear!]
  (e/client
    (dom/button
      (dom/props {:type "button" :class "filter-label" :aria-label (str "Remove filter " text)})
      (dom/text text)
      (dom/span (dom/props {:class "filter-label-x" :aria-hidden "true"}) (dom/text "×"))
      (dom/On "click" (fn [_] (clear!)) nil))))

(e/defn ClearFilters [clear-all!]
  (e/client
    (dom/button
      (dom/props {:type "button" :class "filter-clear"})
      (dom/text "Clear filters")
      (dom/On "click" (fn [_] (clear-all!)) nil))))

(e/defn BatchAction
  "One bulk action in a `BatchBar`. `pick!` is a plain fn that reads the
   selection when it runs, never a value captured at render."
  [label danger? disabled? pick!]
  (e/client
    (dom/button
      (dom/props {:type "button" :disabled disabled?
                  :class (if danger? "btn btn-sm btn-danger-fill" "btn btn-sm btn-secondary")})
      (dom/text label)
      (dom/On "click" (fn [_] (pick!)) nil))))
