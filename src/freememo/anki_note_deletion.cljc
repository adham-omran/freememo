(ns freememo.anki-note-deletion
  (:require [missionary.core :as m]
            [freememo.anki-reads :as reads])
  (:import [missionary Cancelled]))

(defn delete-notes!
  "Attempt each bounded note deletion in order. A failed batch does not stop
   later batches. The caller retains its durable tombstones for recovery."
  [call! note-ids]
  (m/sp
    (loop [chunks (seq (partition-all reads/batch-size note-ids))]
      (when-let [chunk (first chunks)]
        (try
          (m/? (call! "deleteNotes" {:notes (vec chunk)}))
          (catch Cancelled cancelled (throw cancelled))
          (catch #?(:clj Exception :cljs :default) _ nil))
        (recur (next chunks))))))
