(ns freememo.graph-lifecycle
  "Own delayed graph initialization and its renderer through one frame.")

(defn- with-owner-lock [owner f]
  #?(:clj (locking owner (f)) :cljs (f)))

(defn start! [!ctx schedule! initialize! destroy!]
  (let [owner (atom {:phase :waiting :renderer nil})
        cancel-schedule!
        (schedule!
          (fn []
            (with-owner-lock owner
              (fn []
                (when (= :waiting (:phase @owner))
                  (swap! owner assoc :phase :initializing)
                  (let [renderer (initialize!)]
                    (if (= :initializing (:phase @owner))
                      (do (swap! owner assoc :phase :mounted :renderer renderer)
                          (reset! !ctx renderer))
                      (when renderer (destroy! renderer)))))))))]
    (fn []
      (with-owner-lock owner
        (fn []
          (when (not= :disposed (:phase @owner))
            (let [renderer (:renderer @owner)]
              (reset! owner {:phase :disposed :renderer nil})
              (when cancel-schedule! (cancel-schedule!))
              (when renderer
                (destroy! renderer)
                (swap! !ctx #(when-not (identical? renderer %) %))))))))))
