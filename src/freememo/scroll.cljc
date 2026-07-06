(ns freememo.scroll
  "App-local virtual-scroll primitive, vendored from hyperfiddle.electric-scroll0.

   Two deltas from upstream, both fixing observed Library bugs:

   1. `scroll-state` drops the upstream 16 ms throttle. The observer is already
      rAF-coalesced (one emit per frame), so the throttle only added latency: it
      let `--offset` lag native scroll by up to an extra frame, leaving a blank
      strip at the viewport's trailing edge during fast scroll. Removing it keeps
      positioning within one frame of the scroll.

   2. `Scroll-window` takes an optional `:reset-key`. scrollTop is forced to 0
      only when that key changes (search / sort / navigate), NOT on every
      `record-count` change. Callers that grow the list in place (expand a topic,
      delete a row) pass a key excluding that growth, so the view no longer jumps
      to the top. Absent `:reset-key`, behaviour is identical to upstream
      (resets on record-count change).

   `Tape` / `IndexRing` are unchanged and still referred from electric-scroll0."
  (:require [clojure.math :as math]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [missionary.core :as m]))

(defn- clamp [x lo hi] (min (max x lo) hi))

#?(:cljs
   (defn scroll-state
     "Continuous flow of [scrollTop scrollHeight clientHeight scrollLeft scrollWidth
      clientWidth] for `scrollable`.

      Reads are deferred into a single rAF (never synchronously in the scroll/resize
      handler — a sync scrollTop read can force an expensive layout pass with sticky
      children or iOS Safari grid containers). No throttle beyond that rAF, so the
      window position trails native scroll by at most one frame."
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
                    on-scroll (fn [^js event]
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
       (m/relieve {})))) ; discrete → continuous for e/input; keeps latest, no rate cap

#?(:cljs
   (defn compute-overquery [overquery-factor record-count offset limit]
     (let [q-limit (int (math/ceil (* limit overquery-factor))) ; ceil — limit is an item count, must be integer
           occluded (clamp (- q-limit limit) 0 record-count)
           q-offset (clamp (- offset (math/floor (/ occluded overquery-factor)))
                      0 (max 0 (- record-count q-limit)))] ; offset + limit ≤ record-count — no out-of-grid indices at far end
       [q-offset q-limit])))

#?(:cljs
   (defn compute-scroll-window [row-height record-count clientHeight scrollTop overquery-factor]
     (let [padding-top 0 ; e.g. sticky header row
           limit (math/ceil (/ (- clientHeight padding-top) (max row-height 1))) ; aka page-size
           offset (int (/ (clamp scrollTop 0 (* record-count row-height)) ; prevent overscroll past the end
                         (max row-height 1)))]
       (compute-overquery overquery-factor record-count offset limit))))

(e/defn Scroll-window ; returns [offsetV, limitV, offsetH, limitH]
  [row-height record-count node
   #_& {:keys [column-width column-record-count overquery-factor reset-key]
        :or {column-width row-height
             overquery-factor 1}}]
  (e/client
    ;; Reset to top when the caller's reset-key changes (search / sort / navigate).
    ;; Falls back to record-count so callers that don't distinguish in-place row
    ;; growth from navigation keep the upstream behaviour.
    ((fn [_] (set! (.-scrollTop dom/node) 0)) (if (some? reset-key) reset-key record-count))
    (let [[scrollTop scrollHeight clientHeight scrollLeft scrollWidth clientWidth] (e/input (scroll-state node))] ; smooth scroll has already happened, cannot quantize
      (concat
        (compute-scroll-window row-height record-count clientHeight scrollTop overquery-factor)
        ;; Horizontal axis may window a different item set (transposed tables);
        ;; column-record-count lets the caller specify it independently.
        (compute-scroll-window column-width (or column-record-count record-count) clientWidth scrollLeft (max 1 overquery-factor))))))
