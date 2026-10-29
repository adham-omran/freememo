(ns freememo.task-request
  "Await one acknowledged request without retaining a result queue.")

(defn request!
  "Return a task that publishes one request and waits for its owner."
  [!request data]
  (fn [success failure]
    (let [id (random-uuid)
          !active (atom true)]
      (reset! !request
        {:id id :data data
         :resolve (fn [result]
                    (when (compare-and-set! !active true false)
                      (success result)))})
      (fn []
        (when (compare-and-set! !active true false)
          (swap! !request #(when-not (= id (:id %)) %))
          (failure (ex-info "The request was canceled." {:cancelled? true})))))))

(defn resolve! [request result]
  ((:resolve request) result))
