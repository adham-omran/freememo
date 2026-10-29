(ns freememo.anki-overlay-reads
  "Read compact overlay state without retaining account-wide field bodies."
  (:require [missionary.core :as m]
            [freememo.anki-reads :as reads]
            [freememo.anki-digest :as digest]
            [freememo.anki-fields :as fields]
            [freememo.anki-review :as review]
            [freememo.math :as math]))

(defn field-fingerprints [note]
  (mapv (comp digest/text-fingerprint fields/strip-html math/anki->stored-html)
    (take 2 (fields/ordered-field-values (:fields note)))))

(defn note-fingerprint [note kind]
  (digest/text-tuple-fingerprint
    (map (comp fields/strip-html math/anki->stored-html)
      (take (if (= kind "basic") 2 1)
        (concat (fields/ordered-field-values (:fields note)) (repeat "")))))))

(defn read-overlay!
  ([call! note-ids] (read-overlay! call! note-ids {}))
  ([call! note-ids kind-by-note]
   (reads/consume-notes! call!
     (fn [result ids notes]
       (m/sp
         (let [card-ids (into [] (mapcat :cards) notes)
               suspended (m/? (reads/read-suspension! call! card-ids))]
           (reduce (fn [acc [nid note]]
                     (if (:noteId note)
                       (let [cards (:cards note)
                             kind (get kind-by-note nid)
                             count-suspended (count (filter #(true? (get suspended %)) cards))
                             state (cond-> {:note-id nid}
                                     (nil? kind) (assoc :field-fingerprints (field-fingerprints note))
                                     (and kind (not (#{"occlusion" "score" "overlapping"} kind)))
                                     (assoc :fingerprint (note-fingerprint note kind))
                                     (some #{"marked"} (:tags note)) (assoc :tags ["marked"])
                                     (review/note-remarked? (:fields note)) (assoc :remark true)
                                     (pos? count-suspended)
                                     (assoc :suspended {:total (count cards) :suspended count-suspended}))]
                         (update acc :present conj state))
                       (update acc :absent conj nid)))
             result (map vector ids notes)))))
     {:present [] :absent []} note-ids)))
