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

(defn read-overlay! [call! note-ids]
  (reads/consume-notes! call!
    (fn [result ids notes]
      (m/sp
        (let [card-ids (into [] (mapcat :cards) notes)
              suspended (m/? (reads/read-suspension! call! card-ids))]
          (reduce (fn [acc [nid note]]
                    (if (:noteId note)
                      (let [cards (:cards note)]
                        (update acc :present conj
                          {:note-id nid
                           :field-fingerprints (field-fingerprints note)
                           :tags (if (some #{"marked"} (:tags note)) ["marked"] [])
                           :remark (boolean (review/note-remarked? (:fields note)))
                           :suspended {:total (count cards)
                                       :suspended (count (filter #(true? (get suspended %)) cards))}}))
                      (update acc :absent conj nid)))
            result (map vector ids notes)))))
    {:present [] :absent []} note-ids))
