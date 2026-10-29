(ns freememo.work-run
  "An immutable attempt identity with a cooperative commit boundary.")

(defn start
  ([] (start nil))
  ([timeout-ms]
   {:id (str (random-uuid))
    :state (atom :active)
    :deadline (when timeout-ms (+ (System/nanoTime) (* 1000000 timeout-ms)))}))

(defn state [run]
  (locking (:state run)
    (when (and (= :active @(:state run)) (:deadline run)
               (>= (System/nanoTime) (:deadline run)))
      (reset! (:state run) :timed-out))
    @(:state run)))

(defn cancel! [run]
  (locking (:state run)
    (when (= :active @(:state run))
      (reset! (:state run) :cancelled))))

(defn commit!
  "Serialize cancellation and the final write for this attempt."
  [run write!]
  (locking (:state run)
    (case (state run)
      :active (let [result (write!)]
                (reset! (:state run) :committed)
                result)
      :timed-out {:success false :timed-out? true :error "Scan timed out after 30 seconds"}
      {:success false :cancelled? true :error "Work canceled"})))
