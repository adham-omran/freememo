(ns freememo.generation-queue
  "Bounded retained work with FIFO order per user and fair user turns."
  (:require [freememo.work-run :as run])
  (:import [missionary Cancelled]))

(def default-limits
  {:items 256 :user-items 32 :bytes (* 64 1024 1024)
   :user-bytes (* 8 1024 1024) :item-bytes (* 2 1024 1024)})

(defn create [limits]
  {:limits (merge default-limits limits)
   :state (atom {:queues {} :order [] :items {} :waiters []})})

(defn payload-bytes
  "Charge UTF-16 text storage and a fixed metadata allowance before admission."
  [item]
  (+ 2048 (reduce + (map #(* 2 (count (str (get item % ""))))
                         [:content :context :pre-prompt]))))

(defn- reserve-error [scheduler item bytes]
  (let [limits (:limits scheduler)
        entries (vals (:items @(:state scheduler)))
        user-entries (filter #(= (:user-id item) (:user-id (:item %))) entries)]
    (cond
      (> bytes (:item-bytes limits)) "This generation is too large. Select a shorter passage."
      (>= (count user-entries) (:user-items limits)) "Your generation queue is full. Wait for a job to finish."
      (> (+ bytes (reduce + 0 (map :bytes user-entries))) (:user-bytes limits)) "Your generation queue is full. Wait for a job to finish."
      (>= (count entries) (:items limits)) "The generation queue is full. Try again after a job finishes."
      (> (+ bytes (reduce + 0 (map :bytes entries))) (:bytes limits)) "The generation queue is full. Try again after a job finishes.")))

(defn- drain! [scheduler]
  (let [state (:state scheduler)]
    (loop [callbacks []]
      (let [{:keys [queues order items waiters]} @state]
        (if (and (seq order) (seq waiters))
          (let [uid (first order)
                queue (get queues uid)
                id (peek queue)
                remaining (pop queue)
                waiter (first waiters)
                entry (get items id)]
            (swap! state assoc
              :queues (if (seq remaining) (assoc queues uid remaining) (dissoc queues uid))
              :order (cond-> (vec (rest order)) (seq remaining) (conj uid))
              :waiters (vec (rest waiters))
              :items (assoc items id (assoc entry :phase :running)))
            (recur (conj callbacks [(:success waiter) (:item entry)])))
          callbacks)))))

(defn- deliver! [callbacks]
  (doseq [[callback item] callbacks] (callback item)))

(defn admit!
  "Reserve capacity before publishing membership or waking a worker."
  [scheduler item publish!]
  (let [state (:state scheduler)
        result
        (locking state
          (let [bytes (payload-bytes item)]
            (if-let [error (reserve-error scheduler item bytes)]
              [{:ok false :error error :error-type :queue-full} []]
              (let [uid (:user-id item) id (:id item)
                    before @state
                    queue (get-in before [:queues uid] clojure.lang.PersistentQueue/EMPTY)]
                (when (contains? (:items before) id)
                  (throw (ex-info "Generation identity already exists" {:id id})))
                (swap! state
                  (fn [s]
                    (-> s
                        (assoc-in [:items id] {:item item :bytes bytes :phase :queued})
                        (assoc-in [:queues uid] (conj queue id))
                        (cond-> (empty? queue) (update :order conj uid)))))
                (try
                  (publish!)
                  [{:ok true :id id} (drain! scheduler)]
                  (catch Throwable e (reset! state before) (throw e)))))))]
    (deliver! (second result))
    (first result)))

(defn take-task [scheduler]
  (fn [success failure]
    (let [state (:state scheduler) id (str (random-uuid))
          callbacks (locking state
                      (swap! state update :waiters conj {:id id :success success})
                      (drain! scheduler))]
      (deliver! callbacks)
      (fn []
        (let [removed? (locking state
                         (let [present? (some #(= id (:id %)) (:waiters @state))]
                           (swap! state update :waiters
                             #(vec (remove (fn [w] (= id (:id w))) %)))
                           present?))]
          (when removed? (failure (Cancelled. "Generation worker canceled"))))))))

(defn finish! [scheduler id]
  (locking (:state scheduler)
    (swap! (:state scheduler) update :items dissoc id)))

(defn cancel-topic! [scheduler uid tid]
  (let [state (:state scheduler)]
    (locking state
      (let [matching (filter (fn [[_ entry]]
                               (and (= uid (:user-id (:item entry)))
                                    (= tid (:topic-id (:item entry)))))
                            (:items @state))
            queued (set (map first (filter #(= :queued (:phase (second %))) matching)))]
        (doseq [[_ entry] matching] (run/cancel! (:run (:item entry))))
        (swap! state
          (fn [s]
            (let [queue (into clojure.lang.PersistentQueue/EMPTY
                          (remove queued (get-in s [:queues uid])))
                  order (vec (remove #(= uid %) (:order s)))]
              (-> s
                  (update :items #(apply dissoc % queued))
                  (assoc :order (cond-> order (seq queue) (conj uid)))
                  (update :queues #(if (seq queue) (assoc % uid queue) (dissoc % uid)))))))
        (count matching)))))

(defn topic-state [scheduler uid tid]
  (locking (:state scheduler)
    (let [entries (filter #(and (= uid (:user-id (:item %)))
                                (= tid (:topic-id (:item %)))
                                (not (#{:cancelled :timed-out} (run/state (:run (:item %))))))
                          (vals (:items @(:state scheduler))))]
      {:pending (count entries)
       :active-count (count (filter #(= :running (:phase %)) entries))
       :active-id (:id (:item (first (filter #(= :running (:phase %)) entries))))})))

(defn snapshot [scheduler]
  (locking (:state scheduler)
    (let [entries (vals (:items @(:state scheduler)))]
      {:items (count entries) :bytes (reduce + 0 (map :bytes entries))
       :queued (count (filter #(= :queued (:phase %)) entries))
       :running (count (filter #(= :running (:phase %)) entries))})))
