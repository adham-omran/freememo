(ns freememo.schedule-history
  "Transport complete review histories through acknowledged bounded chunks."
  (:require [missionary.core :as m]))

(def cards-per-read 25)
(def reviews-per-upload 1000)

(defn read-history!
  "Release each Anki response after the server replays its card histories."
  [call! consume! base infos]
  (m/sp
    (loop [batches (seq (partition-all cards-per-read infos))
           total 0]
      (if-let [batch (first batches)]
        (let [ids (mapv :cardId batch)
              logs (m/? (call! "getReviewsOfCards" {:cards ids}))
              consumed
              (loop [cards (seq batch) n total]
                (if-let [info (first cards)]
                  (let [cid (:cardId info)
                        rows (vec (sort-by :id (or (get logs cid) (get logs (keyword (str cid))) [])))
                        done
                        (loop [chunks (seq (or (seq (partition-all reviews-per-upload rows)) [[]]))
                               state nil]
                          (if-let [chunk (first chunks)]
                            (let [reply (m/? (consume! (assoc base :info info :reviews (vec chunk)
                                                        :initial state :final? (nil? (next chunks)))))]
                              (when-not (:ok reply)
                                (throw (ex-info "Schedule history could not be replayed." reply)))
                              (recur (next chunks) (:state reply)))
                            true))]
                    (when-not done (throw (ex-info "Schedule replay did not finish." {})))
                    (recur (next cards) (+ n (count rows))))
                  n))]
          (m/? (m/sleep 0))
          (recur (next batches) consumed))
        {:review-count total :streamed? true}))))
