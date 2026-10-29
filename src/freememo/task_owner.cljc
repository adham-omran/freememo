(ns freememo.task-owner
  "A task consumer that rejects callbacks after settlement or teardown.")

(defn start!
  "Start `task` and return an idempotent canceller.
   Teardown invalidates callbacks before it asks the task to cancel."
  [task success failure]
  (let [!state (atom :active)
        settle (fn [callback value]
                 (when (compare-and-set! !state :active :settled)
                   (callback value)))
        cancel! (task #(settle success %) #(settle failure %))]
    (fn []
      (when (compare-and-set! !state :active :canceled)
        (cancel!)))))
