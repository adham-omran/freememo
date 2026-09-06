(ns freememo.route-timing
  "Client timing marks for route shell and first useful content.")

#?(:cljs (defonce ^:private !next-run-id (atom 0)))
#?(:cljs (defonce ^:private !active-run (atom nil)))

#?(:cljs
   (defn- stop-observer! [run]
     (when-let [observer (:observer run)]
       (.disconnect observer))))

#?(:cljs
   (defn- long-task-observer [!long-tasks]
     (try
       (when (and (exists? js/PerformanceObserver)
               (some #{"longtask"}
                 (js->clj (.-supportedEntryTypes js/PerformanceObserver))))
         (let [observer
               (js/PerformanceObserver.
                 (fn [entry-list _observer]
                   (doseq [entry (array-seq (.getEntries entry-list))]
                     (swap! !long-tasks conj
                       {:start-time (.-startTime entry)
                        :duration (.-duration entry)}))))]
           (.observe observer #js {:type "longtask" :buffered true})
           observer))
       (catch :default _ nil))))

(defn start-navigation!
  "Start one client route measurement.

   Pre : tab names the requested primary route.
   Post: later shell and useful marks for this tab share one run id."
  [tab]
  #?(:cljs
     (when (exists? js/performance)
       (stop-observer! @!active-run)
       (let [run-id (swap! !next-run-id inc)
             start-mark (str "freememo:route:" run-id ":start")
             !long-tasks (atom [])]
         (.mark js/performance start-mark)
         (reset! !active-run
           {:id run-id
            :tab tab
            :start-mark start-mark
            :start-time (.now js/performance)
            :shell-marked? false
            :useful-marked? false
            :long-tasks !long-tasks
            :observer (long-task-observer !long-tasks)})))
     :clj nil)
  :started)

(defn mark-shell!
  "Mark the first destination-shell paint opportunity for the active route."
  [tab]
  #?(:cljs
     (when-let [run @!active-run]
       (when (and (= tab (:tab run)) (not (:shell-marked? run)))
         (let [mark-name (str "freememo:route:" (:id run) ":shell")]
           (swap! !active-run assoc :shell-marked? true)
           (js/requestAnimationFrame
             (fn [_timestamp]
               (.mark js/performance mark-name)
               (.measure js/performance "freememo:route-shell"
                 #js {:start (:start-mark run)
                      :end mark-name
                      :detail #js {:route (name tab) :runId (:id run)}}))))))
     :clj nil)
  :marked)

(defn mark-first-useful!
  "Mark the first useful data region and attach bounded client costs.

   Resource transfer bytes cover resource timing entries that start after the
   navigation mark. Electric WebSocket frame bytes need a browser trace."
  [tab region]
  #?(:cljs
     (when-let [run @!active-run]
       (when (and (= tab (:tab run)) (not (:useful-marked? run)))
         (let [mark-name (str "freememo:route:" (:id run) ":useful")]
           (swap! !active-run assoc :useful-marked? true)
           (js/requestAnimationFrame
             (fn [_timestamp]
               (let [resources (array-seq (.getEntriesByType js/performance "resource"))
                     transfer-bytes (reduce + 0
                                      (map #(or (.-transferSize %) 0)
                                        (filter #(>= (.-startTime %) (:start-time run)) resources)))
                     long-tasks @(:long-tasks run)
                     long-task-ms (reduce + 0 (map :duration long-tasks))]
                 (.mark js/performance mark-name)
                 (.measure js/performance "freememo:first-useful"
                   #js {:start (:start-mark run)
                        :end mark-name
                        :detail #js {:route (name tab)
                                     :region (name region)
                                     :runId (:id run)
                                     :resourceTransferBytes transfer-bytes
                                     :longTaskCount (count long-tasks)
                                     :longTaskDurationMs long-task-ms}})
                 (stop-observer! run)
                 (when (= (:id run) (:id @!active-run))
                   (swap! !active-run assoc :observer nil))))))))
     :clj nil)
  :marked)
