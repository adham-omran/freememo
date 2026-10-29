(ns freememo.transcript-scroll
  "Reader-controlled following for assistant transcripts.")

#?(:cljs (defonce ^:private owners (js/WeakMap.)))

(defn near-bottom?
  "Allow a small gap for rounding and delayed layout."
  [scroll-top client-height scroll-height]
  (<= (- scroll-height client-height scroll-top) 48))

(defn install!
  "Own the scroll listener and queued frame until the transcript unmounts."
  [node notify!]
  #?(:cljs
     (let [!following (atom true)
           !frame (atom nil)
           changed! (fn []
                      (let [following? (near-bottom? (.-scrollTop node)
                                         (.-clientHeight node) (.-scrollHeight node))]
                        (when (not= following? @!following)
                          (reset! !following following?)
                          (notify! following?))))
           owner {:following !following :frame !frame :notify! notify!}]
       (.set owners node owner)
       (notify! true)
       (.addEventListener node "scroll" changed! #js {:passive true})
       (fn []
         (.removeEventListener node "scroll" changed!)
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
