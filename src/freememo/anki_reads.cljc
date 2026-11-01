(ns freememo.anki-reads
  "Bound sequential Anki reads while preserving positional missing-note results."
  (:require [missionary.core :as m]))

(def batch-size 500)

(defn- validate-notes! [ids notes]
  (when-not (= (count ids) (count notes))
    (throw (ex-info "Anki returned an incomplete note batch."
             {:requested (count ids) :received (count notes)})))
  (doseq [[id note] (map vector ids notes)]
    (when (and (:noteId note) (not= id (:noteId note)))
      (throw (ex-info "Anki returned a note in the wrong position."
               {:requested id :received (:noteId note)})))))

(defn consume-notes!
  "Await each batch consumer before requesting the next batch."
  [call! step init note-ids]
  (m/sp
    (loop [chunks (seq (partition-all batch-size note-ids))
           result init]
      (if-let [chunk (first chunks)]
        (let [ids (vec chunk)
              notes (m/? (call! "notesInfo" {:notes ids}))]
          (validate-notes! ids notes)
          (let [next-result (m/? (step result ids notes))]
            (m/? (m/sleep 0))
            (recur (next chunks) next-result)))
        result))))

(defn reduce-notes!
  "Reduce each validated batch before requesting the next batch."
  [call! step init note-ids]
  (consume-notes! call! (fn [result ids notes] (m/sp (step result ids notes))) init note-ids))

(defn read-suspension!
  "Read suspension flags through bounded positional requests."
  [call! card-ids]
  (m/sp
    (loop [chunks (seq (partition-all batch-size card-ids)) result {}]
      (if-let [chunk (first chunks)]
        (let [ids (vec chunk)
              flags (m/? (call! "areSuspended" {:cards ids}))]
          (when-not (= (count ids) (count flags))
            (throw (ex-info "Anki returned an incomplete suspension batch."
                     {:requested (count ids) :received (count flags)})))
          (recur (next chunks) (into result (map vector ids flags))))
        result))))

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
          (validate-notes! ids notes)
          (recur (next chunks) (into result notes)))
        result))))
