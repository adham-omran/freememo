(ns freememo.transcript-scroll
  "Reader-controlled following for assistant transcripts.")

#?(:cljs (defonce ^:private owners (js/WeakMap.)))

(defn near-bottom?
  "Allow a small gap for rounding and delayed layout."
  [scroll-top client-height scroll-height]
  (<= (- scroll-height client-height scroll-top) 48))

(defn following-after-scroll?
  "Keep following through layout changes. Pause when the reader leaves the latest region."
  [following? navigation? scroll-top client-height scroll-height]
  (cond
    (near-bottom? scroll-top client-height scroll-height) true
    navigation? false
    :else following?))

(defn install!
  "Own the scroll listener and queued frame until the transcript unmounts."
  [node notify!]
  #?(:cljs
     (let [!following (atom true)
           !frame (atom nil)
           !touch-y (atom nil)
           !pointer-until (atom 0)
           !last-top (atom (.-scrollTop node))
           pause-reader! (fn []
                           (when (and @!following (> (.-scrollHeight node) (.-clientHeight node)))
                             (reset! !following false)
                             (notify! false)))
           changed! (fn []
                      (let [top (.-scrollTop node)
                            navigation? (and (< (.now js/Date) @!pointer-until)
                                             (< top (- @!last-top 0.5)))
                            following? (following-after-scroll? @!following navigation?
                                         top (.-clientHeight node) (.-scrollHeight node))]
                        (reset! !last-top top)
                        (when (not= following? @!following)
                          (reset! !following following?)
                          (notify! following?))))
           wheel! (fn [event]
                    (when (neg? (.-deltaY event)) (pause-reader!)))
           touch-start! (fn [event]
                          (reset! !touch-y (some-> (aget (.-touches event) 0) .-clientY)))
           touch-move! (fn [event]
                         (when-let [y (some-> (aget (.-touches event) 0) .-clientY)]
                           (when (and @!touch-y (> y @!touch-y)) (pause-reader!))
                           (reset! !touch-y y)))
           key! (fn [event]
                  (when (or (#{"ArrowUp" "PageUp" "Home"} (.-key event))
                            (and (= " " (.-key event)) (.-shiftKey event)))
                    (pause-reader!)))
           pointer! (fn [event]
                      (let [rect (.getBoundingClientRect node)
                            gutter (max 12 (- (.-offsetWidth node) (.-clientWidth node)))]
                        (when (and (identical? node (.-target event))
                                   (>= (.-clientX event) (- (.-right rect) gutter)))
                          (reset! !pointer-until (+ (.now js/Date) 600000)))))
           pointer-up! (fn [_]
                         (when (> @!pointer-until (.now js/Date))
                           (reset! !pointer-until (+ (.now js/Date) 250))))
           pointer-cancel! (fn [_] (reset! !pointer-until 0))
           listeners [["scroll" changed!] ["wheel" wheel!] ["touchstart" touch-start!]
                      ["touchmove" touch-move!] ["keydown" key!] ["pointerdown" pointer!]]
           owner {:following !following :frame !frame :notify! notify!}]
       (.set owners node owner)
       (notify! true)
       (doseq [[event listener] listeners]
         (.addEventListener node event listener #js {:passive true}))
       (.addEventListener js/document "pointerup" pointer-up! #js {:passive true})
       (.addEventListener js/document "pointercancel" pointer-cancel! #js {:passive true})
       (.addEventListener js/window "blur" pointer-cancel!)
       (fn []
         (doseq [[event listener] listeners] (.removeEventListener node event listener))
         (.removeEventListener js/document "pointerup" pointer-up!)
         (.removeEventListener js/document "pointercancel" pointer-cancel!)
         (.removeEventListener js/window "blur" pointer-cancel!)
         (when-let [frame @!frame] (js/cancelAnimationFrame frame))
         (when (identical? owner (.get owners node)) (.delete owners node))))
     :clj (fn [])))

(defn follow!
  "Follow new content only while the reader remains at the latest message."
  [node]
  #?(:cljs
     (when-let [{:keys [following frame] :as owner} (.get owners node)]
       (when (and @following (nil? @frame))
         (reset! frame
           (js/requestAnimationFrame
             (fn []
               (reset! frame nil)
               (when (and @following (identical? owner (.get owners node)))
                 (set! (.-scrollTop node) (.-scrollHeight node))))))))
     :clj nil)
  nil)

(defn resume!
  "Return to the latest message and resume following new output."
  [node]
  #?(:cljs
     (when-let [{:keys [following notify!]} (.get owners node)]
       (reset! following true)
       (notify! true)
       (follow! node))
     :clj nil)
  nil)

(defn pause!
  "Pause automatic following while the reader reviews earlier messages."
  [node]
  #?(:cljs
     (when-let [{:keys [following notify!]} (.get owners node)]
       (reset! following false)
       (notify! false))
     :clj nil)
  nil)
