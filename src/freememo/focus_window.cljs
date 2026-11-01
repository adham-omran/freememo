(ns freememo.focus-window
  "Patch overlapping editor ranges without replacing retained page content."
  (:require [clojure.set :as set]
            [freememo.editor-actions :as actions]
            [freememo.math :as math]
            [freememo.focus-window-model :as model]
            [freememo.merged-window :as window]
            [freememo.page-merge :as page-merge]))

(defn overlapping? [old-scope new-scope]
  (and (:merged? old-scope) (:merged? new-scope)
       (= (:pdf-root-id old-scope) (:pdf-root-id new-scope))
       (seq (set/intersection (set (:page-numbers old-scope))
                             (set (:page-numbers new-scope))))))

(defn- page-spans [^js delta lowest-page]
  (model/page-spans (js->clj (.-ops delta) :keywordize-keys true) lowest-page))

(defn- window-change [^js editor ^js incoming old-scope new-scope]
  (let [live (.getContents editor)
        old-spans (page-spans live (first (:page-numbers old-scope)))
        new-spans (page-spans incoming (first (:page-numbers new-scope)))
        {:keys [keep-start keep-end prefix-pages suffix-pages]}
        (model/patch-plan old-spans (:page-numbers old-scope) (:page-numbers new-scope) (.length live))
        Delta (.import (.-Quill js/window) "delta")
        append-pages (fn [^js delta pages]
                       (reduce (fn [^js result page]
                                 (if-let [[start end] (get new-spans page)]
                                   (.concat result (.slice incoming start end))
                                   result)) delta pages))]
    ;; Explicit page boundaries prevent repeated text from matching another page.
    ;; Missing live markers remain missing under the merged-page save contract.
    (let [^js change (append-pages (new Delta) prefix-pages)]
      (.delete change keep-start)
      (.retain change (- keep-end keep-start))
      (.delete change (- (.length live) keep-end))
      (append-pages change suffix-pages))))

(defn- visible-anchor [^js editor]
  (let [^js root (.-root editor)
        top (.-top (.getBoundingClientRect root))
        ^js line (some #(when (> (.-bottom (.getBoundingClientRect %)) top) %)
                       (array-seq (.-children root)))]
    (when line
      (let [blot (.find (.-Quill js/window) line)]
        {:node line :index (.getIndex editor blot) :top (.-top (.getBoundingClientRect line))}))))

(defn- selection-retained? [^js editor old-scope new-scope !last-selection]
  (let [^js selected (.getSelection editor)
        ^js native (js/window.getSelection)
        ^js root (.-root editor)
        range (cond
                (and selected (pos? (.-length selected)))
                {:index (.-index selected) :length (.-length selected)}
                (and native (not (.-isCollapsed native))
                     (.contains root (.-anchorNode native))
                     (.contains root (.-focusNode native))) @!last-selection)
        overlap (set/intersection (set (:page-numbers old-scope))
                                  (set (:page-numbers new-scope)))
        spans (vals (select-keys (page-spans (.getContents editor) (first (:page-numbers old-scope))) overlap))]
    (or (nil? range)
        (and (seq spans)
             (<= (apply min (map first spans)) (:index range))
             (<= (+ (:index range) (:length range)) (apply max (map second spans)))))))

(defn- ambiguous-prefix? [^js editor old-scope new-scope]
  (let [lowest (first (:page-numbers old-scope))
        span (get (page-spans (.getContents editor) lowest) lowest)
        marker (.querySelector (.-root editor) (str "#" (page-merge/marker-id lowest)))
        marker-index (when marker (.getIndex editor (.find (.-Quill js/window) marker)))]
    (model/prefix-reload? (:page-numbers old-scope) (:page-numbers new-scope) marker-index span)))

(defn- caret-retained? [^js editor old-scope new-scope ^js selected]
  (let [live (.getContents editor)
        spans (page-spans live (first (:page-numbers old-scope)))
        {:keys [keep-start keep-end]} (model/patch-plan spans (:page-numbers old-scope)
                                       (:page-numbers new-scope) (.length live))]
    (or (nil? selected) (pos? (.-length selected))
        (<= keep-start (.-index selected) (dec keep-end)))))

(defn- apply-page-change! [^js editor ^js change]
  (let [^js history (.-history editor)
        ignored? (.-ignoreChange history)
        ^js range (.-currentRange history)]
    ;; Quill can emit a repeated-text diff instead of the supplied page delta.
    ;; Transform undo and redo with page identity, then restore the history range.
    (set! (.-ignoreChange history) true)
    (try
      (.updateContents editor change "api")
      (finally (set! (.-ignoreChange history) ignored?)))
    (.transform history change)
    (set! (.-currentRange history)
      (when range
        (let [start (.transformPosition change (.-index range) false)
              end (.transformPosition change (+ (.-index range) (.-length range)) true)]
          #js {:index start :length (max 0 (- end start))})))))

(defn patch!
  "Keep the visible text and transform selection across a bounded range patch."
  [^js editor math? html old-scope new-scope !last-selection]
  (let [^js selection (.-selection editor)
        prefix-blocked? (ambiguous-prefix? editor old-scope new-scope)]
    (when prefix-blocked?
      (reset! window/!blocked-range {:root-id (:pdf-root-id old-scope)
                                    :range [(first (:page-numbers old-scope)) (last (:page-numbers old-scope))]}))
    (when-not (or (.-composing selection) (actions/pending-editor-upload? editor)
                  prefix-blocked?
                  (not (selection-retained? editor old-scope new-scope !last-selection)))
      (let [^js root (.-root editor)
            anchor (visible-anchor editor)
            ^js clipboard (.-clipboard editor)
            html (if math? (math/stored->editor-html html) html)
            incoming (.convert clipboard #js {:html html})
            ^js change (window-change editor incoming old-scope new-scope)
            current-selection (.getSelection editor)
            selected (if (caret-retained? editor old-scope new-scope current-selection)
                       current-selection
                       (do (.setSelection editor nil "silent")
                           (reset! !last-selection nil)
                           nil))
            saved-selection @!last-selection
            transform (fn [{:keys [index length]}]
                        (let [start (.transformPosition change index false)
                              end (.transformPosition change (+ index length) true)]
                          {:index start :length (max 0 (- end start))}))]
        (.cutoff (.-history editor))
        (apply-page-change! editor change)
        (.cutoff (.-history editor))
        (when selected
          (let [{:keys [index length]} (transform {:index (.-index selected) :length (.-length selected)})]
            (.setSelection editor index length "silent")))
        (when saved-selection
          (let [next-selection (transform saved-selection)]
            (reset! !last-selection (when (pos? (:length next-selection)) next-selection))))
        (actions/invalidate-marker-offsets!)
        (when anchor
          (let [index (.transformPosition change (:index anchor) false)
                ^js line (aget (.getLine editor index) 0)]
            (when line
              (set! (.-scrollTop root)
                (+ (.-scrollTop root)
                   (- (.-top (.getBoundingClientRect (.-domNode line))) (:top anchor))))
              (actions/settle-scroll-origin! editor))))
        true))))

(defn- resize-anchor [^js editor]
  (let [^js root (.-root editor)
        rect (.getBoundingClientRect root)
        style (js/getComputedStyle root)
        x (+ (.-left rect) (js/parseFloat (.-paddingLeft style)) 12)
        y (+ (.-top rect) 8)
        ^js range (if (.-caretRangeFromPoint js/document)
                    (.caretRangeFromPoint js/document x y)
                    (when-let [position (when (.-caretPositionFromPoint js/document)
                                          (.caretPositionFromPoint js/document x y))]
                      (doto (.createRange js/document)
                        (.setStart (.-offsetNode position) (.-offset position))
                        (.collapse true))))]
    (if (and range (= 3 (.-nodeType (.-startContainer range)))
             (.contains root (.-startContainer range))
             (pos? (.-height (.getBoundingClientRect range))))
      {:range range :node (.-startContainer range)
       :top (.-top (.getBoundingClientRect range))}
      (visible-anchor editor))))

(defn install-resize-anchor!
  "Preserve the visible block when loaded media changes height."
  [^js editor active?]
  (let [^js root (.-root editor)
        !anchor (atom nil)
        !observed (atom #{})
        !sizes (atom {})
        !frame (atom nil)
        capture! (fn []
                   (reset! !anchor
                     (when (and (active?) (.-isConnected root))
                       (when-let [anchor (resize-anchor editor)]
                         (assoc anchor :scroll-top (.-scrollTop root))))))
        compensate! (fn []
                      (when (active?)
                        (when-let [{:keys [node range top scroll-top]} @!anchor]
                          (when (and (.-isConnected node) (= scroll-top (.-scrollTop root)))
                            (let [rect (if range (.getBoundingClientRect range) (.getBoundingClientRect node))
                                  delta (- (.-top rect) top)]
                              (when (> (js/Math.abs delta) 0.5)
                                (set! (.-scrollTop root) (+ scroll-top delta))
                                (actions/settle-scroll-origin! editor))))))
                      (actions/invalidate-marker-offsets!)
                      (capture!))
        resize (js/ResizeObserver.
                 (fn [entries]
                   (let [sizes (into {} (map (fn [^js entry]
                                              [(.-target entry) [(.-width (.-contentRect entry))
                                                                 (.-height (.-contentRect entry))]])
                                         (array-seq entries)))
                         changed? (some (fn [[node size]]
                                          (when-let [old (get @!sizes node)] (not= old size))) sizes)]
                     (swap! !sizes merge sizes)
                     (if changed? (compensate!) (capture!)))))
        refresh! (fn []
                   (let [nodes (if (active?) (set (array-seq (.-children root))) #{})]
                     (doseq [node (set/difference @!observed nodes)] (.unobserve resize node))
                     (doseq [node (set/difference nodes @!observed)] (.observe resize node))
                     (reset! !observed nodes)
                     (swap! !sizes select-keys nodes)))
        mutation (js/MutationObserver. (fn [_] (refresh!)))
        scroll! (fn [_]
                  (when-let [frame @!frame] (js/cancelAnimationFrame frame))
                  (reset! !frame (js/requestAnimationFrame (fn [] (capture!))))) ]
    (.observe mutation root #js {:childList true})
    (.addEventListener root "scroll" scroll! #js {:passive true})
    (reset! !frame (js/requestAnimationFrame (fn [] (refresh!) (capture!))))
    (fn []
      (when-let [frame @!frame] (js/cancelAnimationFrame frame))
      (.disconnect resize)
      (.disconnect mutation)
      (reset! !anchor nil)
      (reset! !sizes {})
      (.removeEventListener root "scroll" scroll!))))
