(ns freememo.anki-note-deletion
  (:require [missionary.core :as m]
            [freememo.anki-reads :as reads]
            [freememo.task-owner :as task-owner])
  (:import [missionary Cancelled]))

(defn delete-notes!
  "Attempt each bounded note deletion in order. A failed batch does not stop
   later batches. The caller retains its durable tombstones for recovery."
  ([call! note-ids] (delete-notes! call! note-ids (constantly true)))
  ([call! note-ids continue?]
   (m/sp
     (loop [chunks (seq (partition-all reads/batch-size note-ids))]
       (when (and (continue?) (seq chunks))
         (try
           (m/? (call! "deleteNotes" {:notes (vec (first chunks))}))
           (catch Cancelled cancelled (throw cancelled))
           (catch #?(:clj Exception :cljs :default) _ nil))
         (recur (next chunks)))))))

(defn close-owner! [!current owner]
  (reset! (:active? owner) false)
  (let [[tasks _] (reset-vals! (:cancellers owner) {})]
    (doseq [cancel! (vals tasks)] (cancel!)))
  (compare-and-set! !current owner nil)
  nil)

(defn mount-owner! [!current user-id]
  (let [owner {:id (random-uuid) :user-id user-id
               :active? (atom true) :cancellers (atom {})}
        [previous _] (reset-vals! !current owner)]
    (when previous (close-owner! !current previous))
    owner))

(defn start-owned!
  "Start one captured deletion under the current user root. Its lifetime spans
   route changes and ends when that root releases its owner."
  [!current call! note-ids]
  (when-let [owner (when (seq note-ids) @!current)]
    (when @(:active? owner)
      (let [id (random-uuid)
            settled? (atom false)
            current? #(and @(:active? owner) (identical? owner @!current))
            settle! (fn [_]
                      (reset! settled? true)
                      (swap! (:cancellers owner) dissoc id))
            cancel! (task-owner/start! (delete-notes! call! note-ids current?) settle! settle!)]
        (when-not @settled?
          (swap! (:cancellers owner) assoc id cancel!)
          (when-not (current?)
            (swap! (:cancellers owner) dissoc id)
            (cancel!))))))
  nil)
