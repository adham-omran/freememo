(ns freememo.measured-list
  "Measured scrolling for variable-height rows. Only row heights remain offscreen."
  (:require [hyperfiddle.electric3 :as e]))

(def ^:private tree-size 1073741824)

(defn height-model [record-count estimate]
  {:count (max 0 record-count) :estimate (max 1 estimate) :heights {} :tree {}})

(defn prefix-height
  "Return the estimated height before an absolute row index."
  [{:keys [count estimate tree]} index]
  (let [index (min count (max 0 index))]
    (loop [i index total (* index estimate)]
      (if (pos? i)
        (recur (- i (bit-and i (- i))) (+ total (get tree i 0)))
        total))))

(defn measure-height [model index height]
  (if (and (<= 0 index) (< index (:count model)) (pos? height))
    (let [height (max 1 height)
          previous (get-in model [:heights index] (:estimate model))
          delta (- height previous)]
      (if (< (abs delta) 0.25)
        model
        (loop [i (inc index) model (assoc-in model [:heights index] height)]
          (if (<= i tree-size)
            (recur (+ i (bit-and i (- i))) (update-in model [:tree i] (fnil + 0) delta))
            model))))
    model))

(defn index-at [model position]
  (loop [lo 0 hi (:count model)]
    (if (< lo hi)
      (let [mid (quot (+ lo hi) 2)]
        (if (<= (prefix-height model (inc mid)) position)
          (recur (inc mid) hi)
          (recur lo mid)))
      (min lo (max 0 (dec (:count model)))))))

(defn follow-target [before model scroll-height viewport-height]
  (max 0 (- (+ scroll-height
               (- (prefix-height model (:count model))
                  (prefix-height before (:count before))))
            viewport-height)))

(defn visible-window
  "Include one viewport of overscan and retain explicit interaction pins."
  [model scroll-top viewport-height pins max-mounted]
  (let [record-count (:count model)
        viewport-height (max 1 viewport-height)
        visible-start (if (pos? record-count) (index-at model scroll-top) 0)
        visible-end (if (pos? record-count)
                      (min record-count (inc (index-at model (+ scroll-top viewport-height)))) 0)
        capacity (max 1 max-mounted (- visible-end visible-start))
        start (if (pos? record-count)
                (max (index-at model (max 0 (- scroll-top viewport-height))) (- visible-end capacity)) 0)
        end (if (pos? record-count)
              (min record-count (+ start capacity)
                (inc (index-at model (+ scroll-top (* 2 viewport-height))))) 0)
        indices (sort (into (set (range start end))
                       (filter #(<= 0 % (dec record-count)) pins)))]
    {:rows (mapv (fn [index] {:index index :top (prefix-height model index)
                             :height (get-in model [:heights index] (:estimate model))}) indices)
     :height (prefix-height model record-count)
     :offset start :limit (- end start)}))

#?(:cljs (defonce ^:private owners (js/WeakMap.)))

#?(:cljs
   (defn- row-index [scroller node]
     (loop [element (some-> node (cond-> (= 3 (.-nodeType node)) .-parentElement)
                      (.closest "[data-measured-index]"))]
       (when element
         (if (identical? scroller (.closest element "[data-measured-root]"))
           (js/parseInt (.getAttribute element "data-measured-index") 10)
           (recur (some-> (.-parentElement element) (.closest "[data-measured-index]"))))))))

#?(:cljs
   (defn- interaction-pins [scroller rows explicit]
     (let [selection (.getSelection js/window)
           active (row-index scroller (.-activeElement js/document))
           selected (when (and selection (not (.-isCollapsed selection)))
                      (keep #(row-index scroller %) [(.-anchorNode selection) (.-focusNode selection)]))]
       (into (cond-> (set explicit) (some? active) (conj active))
         (concat (when (seq selected) (range (apply min selected) (inc (apply max selected))))
           (keep (fn [[index node]]
                   (when (or (.matches node "[data-window-pin='true'],[aria-busy='true'],[aria-invalid='true']")
                           (.querySelector node "[data-window-pin='true'],[aria-busy='true'],[aria-invalid='true']"))
                     index)) rows))))))

#?(:cljs
   (defn install! [scroller !view]
     (let [!model (atom (height-model 0 100))
           !config (atom nil)
           !rows (atom {})
           !sizes (atom {})
           !pins (atom #{})
           !frame (atom nil)
           !adjust (atom nil)
           !alive (atom true)
           !anchored (atom false)
           !anchor (atom nil)
           !scrolled (atom false)
           schedule-ref (atom nil)
           schedule! (fn [] (when-let [schedule @schedule-ref] (schedule)))
           sizes (js/ResizeObserver.
                   (fn [entries]
                     (doseq [entry (array-seq entries)]
                       (let [node (.-target entry)]
                         (when-not (identical? node scroller)
                           (let [index (js/parseInt (.getAttribute node "data-measured-index") 10)
                                 height (.-height (.getBoundingClientRect node))]
                             (when (and (js/isFinite index) (pos? height))
                               (swap! !sizes assoc index height))))))
                     (schedule!)))
           mutations (js/MutationObserver. (fn [_] (schedule!)))
           owner {:model !model :config !config :rows !rows :sizes !sizes :pins !pins
                  :schedule schedule! :observer sizes :adjust !adjust :anchored !anchored}
           cancel-adjust! (fn [_] (reset! !adjust nil))
           scroll! (fn [event]
                     (when (= "scroll" (.-type event)) (reset! !scrolled true))
                     (schedule!))
           render! (fn []
                     (reset! !frame nil)
                     (when @!alive
                       (when-let [target @!adjust]
                         (reset! !adjust nil)
                         (set! (.-scrollTop scroller) target))
                       (let [before @!model
                             top (.-scrollTop scroller)
                             height (.-clientHeight scroller)
                             old-anchor @!anchor
                             moved-index (when (and old-anchor (not @!scrolled))
                                           (some (fn [[index node]]
                                                   (when (identical? node (:node old-anchor)) index)) @!rows))
                             anchor (if (some? moved-index) moved-index (index-at before top))
                             within (if (some? moved-index) (:within old-anchor)
                                      (- top (prefix-height before anchor)))
                             {:keys [record-count estimate-height reset-key initial-anchor max-mounted follow-end?]} @!config
                             scope-change? (or (not= reset-key (:reset-key before))
                                             (not= estimate-height (:estimate before)))
                             model (if scope-change?
                                     (assoc (height-model record-count estimate-height) :reset-key reset-key)
                                     (assoc before :count record-count))
                             model (reduce-kv measure-height model @!sizes)
                             initial? (and (pos? record-count) (or scope-change? (not @!anchored)))
                             target (cond
                                      initial? (if (= :end initial-anchor)
                                                 (max 0 (- (prefix-height model record-count) height)) 0)
                                      (and follow-end? (not @!scrolled))
                                      (follow-target before model (.-scrollHeight scroller) height)
                                      (or (not= before model)
                                          (and (some? moved-index) (not= moved-index (:index old-anchor))))
                                      (+ (prefix-height model anchor) within)
                                      :else top)
                             pins (interaction-pins scroller @!rows @!pins)
                             view (visible-window model target height pins max-mounted)]
                         (reset! !sizes {})
                         (reset! !scrolled false)
                         (reset! !anchor
                           (when-let [node (get @!rows anchor)]
                             {:node node :index anchor :within within}))
                         (when scope-change? (reset! !anchor nil))
                         (cond
                           initial? (reset! !anchored true)
                           scope-change? (reset! !anchored false))
                         (reset! !model model)
                         (when (not= view @!view) (reset! !view view))
                         (when (> (abs (- target top)) 0.25)
                           (reset! !adjust target)
                           (schedule!)))))]
       (.set owners scroller owner)
       (.setAttribute scroller "data-measured-root" "true")
       (reset! schedule-ref
         (fn [] (when (and @!alive (nil? @!frame))
                  (reset! !frame (js/requestAnimationFrame render!)))))
       (.observe sizes scroller)
       (.observe mutations scroller #js {:subtree true :attributes true
                                        :attributeFilter #js ["data-window-pin" "aria-busy" "aria-invalid"]})
       (doseq [event ["scroll" "focusin" "focusout"]]
         (.addEventListener scroller event scroll! #js {:passive true}))
       (doseq [event ["wheel" "touchstart" "pointerdown"]]
         (.addEventListener scroller event cancel-adjust! #js {:passive true}))
       (.addEventListener js/document "selectionchange" scroll!)
       (fn []
         (reset! !alive false)
         (when-let [frame @!frame] (js/cancelAnimationFrame frame))
         (.disconnect sizes)
         (.disconnect mutations)
         (doseq [event ["scroll" "focusin" "focusout"]] (.removeEventListener scroller event scroll!))
         (doseq [event ["wheel" "touchstart" "pointerdown"]] (.removeEventListener scroller event cancel-adjust!))
         (.removeEventListener js/document "selectionchange" scroll!)
         (when (identical? owner (.get owners scroller))
           (.delete owners scroller)
           (.removeAttribute scroller "data-measured-root"))))))

(defn configure! [scroller record-count options]
  #?(:cljs
     (when-let [{:keys [config schedule]} (.get owners scroller)]
       (let [next (merge {:estimate-height 100 :max-mounted 150} options
                    {:record-count (max 0 record-count)})]
         (when (not= next @config)
           (reset! config next)
           (schedule))))
     :clj nil)
  nil)

(defn pin! [scroller index pinned?]
  #?(:cljs
     (when-let [{:keys [pins schedule]} (.get owners scroller)]
       (swap! pins (if pinned? conj disj) index)
       (schedule)) :clj nil)
  nil)

(defn jump! [scroller index alignment]
  #?(:cljs
     (when-let [{:keys [model adjust schedule]} (.get owners scroller)]
       (reset! adjust
         (max 0 (if (= :end alignment)
                  (- (prefix-height @model (inc index)) (.-clientHeight scroller))
                  (prefix-height @model index))))
       (schedule)) :clj nil)
  nil)

(defn observe! [scroller index node]
  #?(:cljs
     (if-let [{:keys [rows observer sizes schedule]} (.get owners scroller)]
       (do
         (.setAttribute node "data-measured-index" (str index))
         (swap! rows assoc index node)
         (swap! sizes assoc index (.-height (.getBoundingClientRect node)))
         (.observe observer node)
         (schedule)
         (fn []
           (when (identical? node (get @rows index))
             (swap! rows dissoc index)
             (swap! sizes dissoc index))
           (when-not (some #(identical? node %) (vals @rows))
             (.unobserve observer node))
           (schedule)))
       (fn []))
     :clj (fn [])))

(e/defn Window [scroller record-count options]
  (e/client
    (let [!view (atom {:rows [] :height 0 :offset 0 :limit 0})
          cleanup (install! scroller !view)]
      ;; Registration must finish before configuration reads the owner.
      (case cleanup
        (do
          (e/on-unmount cleanup)
          (case (configure! scroller record-count options)
            (e/watch !view)))))))

(e/defn Observe [scroller index node]
  (e/client (e/on-unmount (observe! scroller index node))))
