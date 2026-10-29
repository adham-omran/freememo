(ns freememo.anki-reads
  "Bound sequential Anki reads while preserving positional missing-note results."
  (:require [missionary.core :as m]))

(def batch-size 500)

(defn read-notes!
  "Return a task that reads notes in bounded requests through `call!`.
   The result keeps request order, including missing-note entries."
  [call! note-ids]
  (m/sp
    (loop [chunks (seq (partition-all batch-size note-ids))
           result []]
      (if-let [chunk (first chunks)]
        (let [ids (vec chunk)
              notes (m/? (call! "notesInfo" {:notes ids}))]
          (when-not (= (count ids) (count notes))
            (throw (ex-info "Anki returned an incomplete note batch."
                     {:requested (count ids) :received (count notes)})))
          (recur (next chunks) (into result notes)))
        result))))
