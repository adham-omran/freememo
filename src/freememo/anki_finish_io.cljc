(ns freememo.anki-finish-io
  "Bound pull transport and acknowledge each completed deletion."
  (:require [missionary.core :as m]
            [freememo.anki-reads :as reads]
            [freememo.anki-digest :as digest]))

(defn finish!
  "Check ownership before each external chunk and persist completed chunks."
  [call! rpc! plan scan]
  (m/sp
    (let [pulls
          (loop [chunks (seq (partition-all reads/batch-size (:pull-card-ids plan)))
                 result {:count 0 :conflict-card-ids []}]
            (if-let [chunk (first chunks)]
              (let [verdict (:verdict (m/? (rpc! {:op :checkpoint})))]
                (if (not= :continue verdict)
                  (assoc result :verdict verdict)
                  (let [versions (select-keys (:pull-versions plan) chunk)
                        ids (mapv #(get-in versions [% :note-id]) chunk)
                        notes (m/? (reads/read-notes! call! ids))
                        kinds (:pull-kind-by-card plan)
                        changed (into #{}
                                  (keep (fn [[cid note]]
                                          (let [nid (get-in versions [cid :note-id])]
                                            (when (or (nil? (:noteId note))
                                                      (not= (get-in scan [:digests nid])
                                                        (digest/anki-digest (:fields note) (get kinds cid))))
                                              cid))))
                                  (map vector chunk notes))
                        accepted (vec (remove changed chunk))
                        reply (m/? (rpc! {:op :pull :card-ids accepted
                                          :versions (select-keys versions accepted)
                                          :digests (select-keys (:digests scan) ids)
                                          :notes (into {} (keep (fn [[cid note]]
                                                                 (when-not (changed cid)
                                                                   [(:noteId note) note])))
                                                   (map vector chunk notes))}))]
                    (cond
                      (:verdict reply) (assoc result :verdict (:verdict reply))
                      (false? (:success reply)) (throw (ex-info "An Anki pull batch failed." reply))
                      :else (recur (next chunks)
                              (-> result
                                (update :count + (:count reply))
                                (update :conflict-card-ids into (concat changed (:conflict-card-ids reply)))))))))
              result))]
      (if (:verdict pulls)
        pulls
        (loop [chunks (seq (partition-all reads/batch-size (:note-ids-to-delete plan)))
               deleted 0]
          (if-let [chunk (first chunks)]
            (let [verdict (:verdict (m/? (rpc! {:op :checkpoint})))]
              (if (not= :continue verdict)
                {:verdict verdict}
                (do
                  (m/? (call! "deleteNotes" {:notes (vec chunk)}))
                  (let [recorded (m/? (rpc! {:op :deleted :note-ids (vec chunk)}))]
                    (when-not (:success recorded)
                      (throw (ex-info "Deleted Anki notes could not be recorded." recorded)))
                    (recur (next chunks) (+ deleted (count chunk)))))))
            {:pulls pulls :deleted deleted :finished-note-ids (vec (:note-ids-to-drop plan))}))))))
