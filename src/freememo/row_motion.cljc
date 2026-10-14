(ns freememo.row-motion
  "Slide the rows of a virtual-scroll table (C1c) when a row is added, removed
   or reordered, and slide a new row in.

   The motion follows the ROW KEY, never the node. Electric does not move a
   row's node when a row is inserted above it. It keeps the nodes in their
   slots and writes each one the next row's content. Measured on one card add
   in the content card table: 8 of 10 nodes changed card in place. A CSS
   transition on `--order` therefore slides a node that already shows another
   card, so the table seems to move up by one row while nothing moved.

   So this is FLIP over keys. Once per frame, before paint, it reads each row's
   `data-row-key` and `--order`. A key whose order changed is animated from the
   place it was painted last frame to its new place, on whichever node shows
   it now. A key that appears with `data-enter=\"1\"` slides down from the row
   above. A key that appears by scrolling has no `data-enter`, so it does not
   move. A key that appears where another key left in the same frame takes
   over that key's slide, so a pending row that is replaced mid-slide by its
   saved row does not snap.

   Reading once per frame, not per mutation, is load-bearing. A card that
   lands applies the pending row's removal and the new row as separate DOM
   writes, and a reader in between would see every row move up and back.")

#?(:cljs
   (defn- row-height [^js table]
     (let [h (js/parseFloat (.getPropertyValue (js/getComputedStyle table) "--row-height"))]
       (if (js/isNaN h) 0 h))))

#?(:cljs
   (defn- rows-by-key
     "{key {:node tr :order n}} for every keyed row of `table`."
     [^js table]
     (persistent!
       (reduce (fn [m ^js tr]
                 (if-let [k (.. tr -dataset -rowKey)]
                   (assoc! m k {:node tr
                                :order (js/parseFloat (.getPropertyValue (.-style tr) "--order"))})
                   m))
         (transient {})
         (array-seq (.-children table))))))

#?(:cljs
   (defn- residual
     "How far from its place the running slide `a` paints its row now, in px."
     [{:keys [^js anim from]}]
     (let [p (some-> anim .-effect .getComputedTiming .-progress)]
       (if (number? p) (* from (- 1 p)) 0))))

#?(:cljs
   (defn- slide! [^js node from-px]
     (.animate node
       #js [#js {:translate (str "0 " from-px "px")} #js {:translate "0 0"}]
       #js {:duration 220 :easing "ease"})))

#?(:cljs
   (defn- enter! [^js node]
     (.animate node
       #js [#js {:translate "0 -100%" :opacity 0} #js {:translate "0 0" :opacity 1}]
       #js {:duration 260 :easing "ease-out"})))

(defn install!
  "Animate the rows of `table`, a `.tape-scroll` table whose rows carry
   `data-row-key` and `--order`.
   Pre:  every row of one key carries the same `data-row-key` for its life.
   Post: a row that changes order slides from its old place to its new one,
         and a row that appears with `data-enter=\"1\"` slides in. Nothing
         moves when the reader prefers reduced motion. Returns a teardown fn."
  [table]
  #?(:cljs
     (let [!last (atom nil)                ; key -> order, as last painted
           !moving (atom {})               ; key -> {:node :anim :from}
           !frame (atom nil)
           reduced (js/matchMedia "(prefers-reduced-motion: reduce)")
           step!
           (fn []
             (reset! !frame nil)
             (let [now (rows-by-key table)
                   prev @!last
                   h (row-height table)
                   ;; A slide longer than the view is a filter or a jump, not
                   ;; a neighbour making room, and it is drawn as a jump.
                   limit (max h (or (some-> (.-parentElement table) .-clientHeight) 0))
                   moving @!moving
                   ;; Where each running slide paints its row now, read before
                   ;; any slide is cancelled.
                   offsets (update-vals moving residual)
                   ;; order -> offset of a key that left this frame mid-slide.
                   left (into {} (keep (fn [[k o]]
                                         (when (and prev (not (contains? now k)) (contains? prev k))
                                           [(get prev k) o])))
                          offsets)]
               ;; A node that now paints another key, or no key, must not keep
               ;; sliding the content it showed before.
               (doseq [[k {:keys [node anim]}] moving]
                 (when-not (identical? node (get-in now [k :node]))
                   (.cancel ^js anim)))
               (reset! !moving
                 (if (or (nil? prev) (.-matches reduced))
                   {}
                   (reduce
                     (fn [acc [k {:keys [^js node order]}]]
                       (let [running (get moving k)
                             same-node? (and running (identical? node (:node running)))]
                         (cond
                           (not (contains? prev k))
                           (let [from (get left order 0)]
                             (cond
                               (= "1" (.. node -dataset -enter))
                               (assoc acc k {:node node :from (- h) :anim (enter! node)})

                               (< 0.5 (js/Math.abs from) limit)
                               (assoc acc k {:node node :from from :anim (slide! node from)})

                               :else acc))

                           (and (= order (get prev k)) (or (nil? running) same-node?))
                           (if (and running (not= "finished" (.-playState ^js (:anim running))))
                             (assoc acc k running)
                             acc)

                           :else
                           (let [from (+ (* h (- (get prev k) order)) (get offsets k 0))]
                             (when same-node? (.cancel ^js (:anim running)))
                             (if (< 0.5 (js/Math.abs from) limit)
                               (assoc acc k {:node node :from from :anim (slide! node from)})
                               acc)))))
                     {}
                     now)))
               (reset! !last (update-vals now :order))))
           schedule! (fn [_ _]
                       (when-not @!frame
                         (reset! !frame (js/requestAnimationFrame step!))))
           observer (js/MutationObserver. schedule!)]
       (.observe observer table #js {:childList true :subtree true
                                     :attributes true
                                     :attributeFilter #js ["style" "data-row-key"]})
       (schedule! nil nil)
       (fn []
         (.disconnect observer)
         (some-> @!frame js/cancelAnimationFrame)))
     :clj nil))
