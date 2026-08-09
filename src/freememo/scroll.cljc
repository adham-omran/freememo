(ns freememo.scroll
  "App-local virtual-scroll primitive, vendored from hyperfiddle.electric-scroll0.

   Stateless Tape windowing with four deltas from upstream:

   1. `:reset-key` — scrollTop resets to 0 only when the caller's key changes
      (search / sort / navigate), NOT on every `record-count` change. Absent
      `:reset-key`, matches upstream (resets on count).

   2. D′ — `scroll-state` drops the upstream 16 ms throttle; the observer already
      coalesces to one emit per animation frame.

   3. Offset is a pure per-frame function of scrollTop (no cross-frame state), so
      it survives Electric frame re-mounts (WS reconnect / hot reload).

   4. `:max-visible-rows` — caps the page-size, so mounted rows stop growing with
      viewport height. Mount/unmount of a row is the dominant cost in the Library
      tables (measured ~50 ms per row), and it scales linearly with the window.
      Absent the key, matches upstream (window = viewport).

   Flicker-free rendering depends on CSS companions in index.css. Always:
   `overflow-anchor: none` on `.tape-scroll` (browser scroll-anchoring otherwise
   fights the row repositioning into a ±row oscillation). Positioning is PER ROW
   (C1c) for EVERY `.tape-scroll` table — all Scroll-window callers, vendored and
   jar alike, since the positioning lives in the shared CSS, not here: each row
   sets `--order` (0-based) and gets its own `transform: translateY(var(--order) *
   var(--row-height))`; the table sets `height: calc(var(--count) *
   var(--row-height))` for the scroll range and columns via `var(--grid-cols)`.
   Row position and content live on the same element → one Electric commit → no
   skew. The older container-level `--offset` transform put position and content
   on different elements; they committed a frame apart and snapped one row at
   each boundary.

   `Tape` / `IndexRing` are unchanged and still referred from electric-scroll0."
  (:require [clojure.math :as math]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [missionary.core :as m]))

(defn- clamp [x lo hi] (min (max x lo) hi))

#?(:cljs
   (defn scroll-state
     "Continuous flow of [scrollTop scrollHeight clientHeight scrollLeft scrollWidth
      clientWidth] for `scrollable`, sampled once per animation frame.

      Reads are deferred into a single rAF (never synchronously in the scroll/resize
      handler — a sync scrollTop read can force an expensive layout pass with sticky
      children or iOS Safari grid containers). The upstream 16 ms throttle is dropped
      (D′); the observer already coalesces to one emit per frame."
     [scrollable]
     (->> (m/observe
            (fn [!]
              (let [!state (object-array [(.-scrollTop scrollable)
                                          (.-scrollHeight scrollable)
                                          (.-clientHeight scrollable)
                                          (.-scrollLeft scrollable)
                                          (.-scrollWidth scrollable)
                                          (.-clientWidth scrollable)])
                    in-raf? (atom false)
                    emit (fn [!] (! (into [] (array-seq !state))))
                    schedule (fn [callback]
                               (when (compare-and-set! in-raf? false true)
                                 (.requestAnimationFrame js/window
                                   (fn [_]
                                     (reset! in-raf? false)
                                     (callback)))))
                    sizes (js/ResizeObserver. ; fires after layout flush
                            (fn [entries _]
                              (let [r (.-contentRect (aget entries 0))]
                                (doto !state
                                  (aset 5 (.-width r))
                                  (aset 2 (.-height r))))
                              (schedule #(emit !))))
                    on-scroll (fn [^js event] ; do not read scrollTop synchronously — layout pass risk
                                (let [t (.-target event)]
                                  (schedule
                                    (fn []
                                      (doto !state
                                        (aset 0 (.-scrollTop t))
                                        (aset 3 (.-scrollLeft t)))
                                      (emit !)))))]
                (.observe sizes scrollable)
                (.addEventListener scrollable "scroll" on-scroll #js{:passive true})
                (emit !)
                #(do (.disconnect sizes)
                     (.removeEventListener scrollable "scroll" on-scroll)))))
       (m/relieve {})))) ; discrete → continuous for e/input; keeps latest

#?(:cljs
   (defn compute-overquery [overquery-factor record-count offset limit]
     (let [q-limit (int (math/ceil (* limit overquery-factor))) ; ceil — limit is an item count, must be integer
           occluded (clamp (- q-limit limit) 0 record-count)
           q-offset (clamp (- offset (math/floor (/ occluded overquery-factor)))
                      0 (max 0 (- record-count q-limit)))] ; offset + limit ≤ record-count — no out-of-grid indices at far end
       [q-offset q-limit])))

#?(:cljs
   (defn compute-scroll-window
     "Stateless window for one axis: floor(scrollTop / row-height) → offset, plus the
      overquery buffer. Pure per-frame function of scrollTop (no cross-frame state).

      `max-visible-rows` (nil = uncapped) caps the page-size BEFORE the overquery
      call, so `compute-overquery` still derives `occluded`, the buffer shift and
      the end-of-list clamp from ONE consistent `limit`. Capping its returned
      q-limit instead would size the buffer shift and that clamp for a window
      wider than the one rendered, and the last rows of the list would come up
      blank at maximum scrollTop.

      Post: q-limit ≤ max-visible-rows × overquery-factor."
     [row-height record-count clientHeight scrollTop overquery-factor max-visible-rows]
     (let [visible (int (math/ceil (/ clientHeight (max row-height 1)))) ; page-size the viewport asks for
           limit   (if max-visible-rows (min visible max-visible-rows) visible)
           offset  (int (/ (clamp scrollTop 0 (* record-count row-height)) ; clamp — no overscroll past the end
                          (max row-height 1)))]
       (compute-overquery overquery-factor record-count offset limit))))

(e/defn Scroll-window ; returns [offsetV, limitV, offsetH, limitH]
  [row-height record-count node
   ;; `:max-visible-rows` caps the PRE-overquery page-size, so the caller gets
   ;; `max-visible-rows × overquery-factor` mounted rows — not `max-visible-rows`.
   #_& {:keys [column-width column-record-count overquery-factor reset-key max-visible-rows]
        :or {column-width row-height
             overquery-factor 1}}]
  (e/client
    ;; Reset to top when the caller's reset-key changes (search / sort / navigate).
    ;; Falls back to record-count so callers that don't distinguish in-place row
    ;; growth from navigation keep the upstream behaviour.
    ((fn [_] (set! (.-scrollTop dom/node) 0)) (if (some? reset-key) reset-key record-count))
    (let [[scrollTop _ clientHeight scrollLeft _ clientWidth] (e/input (scroll-state node))]
      (concat
        (compute-scroll-window row-height record-count clientHeight scrollTop overquery-factor max-visible-rows)
        ;; Horizontal axis may window a different item set (transposed tables);
        ;; column-record-count lets the caller specify it independently.
        ;; nil, never max-visible-rows: that key caps ROWS, and this axis windows
        ;; columns — the same number would silently mean something else here.
        (compute-scroll-window column-width (or column-record-count record-count) clientWidth scrollLeft (max 1 overquery-factor) nil)))))
