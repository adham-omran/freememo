(ns freememo.anki-review
  "Pure rules for Anki review states."
  (:require [clojure.string :as str]
            [freememo.anki-fields :as fields]))

(defn note-remark
  "Return readable text from Anki's named Remarks field, or nil when empty."
  [fields-map]
  (let [field (or (get fields-map :Remarks) (get fields-map "Remarks"))
        html (str (or (when (map? field)
                        (or (:value field) (get field "value")))
                      (when (string? field) field)
                      ""))
        text (-> html
               (str/replace #"(?i)<img\b[^>]*>" "[Image]")
               fields/strip-html
               (str/replace #"(?i)(?:&nbsp;|&#160;)" " ")
               (str/replace "\u00a0" " ")
               str/trim)]
    (not-empty text)))

(defn note-remarked?
  "True when the named Anki Remarks field has text or an image."
  [fields-map]
  (boolean (note-remark fields-map)))

(defn pushed-manifest
  "Return the card and Anki note IDs for pushed members in display rows."
  [display-rows]
  (into []
    (comp
      (mapcat :members)
      (keep (fn [card]
              (when-some [note-id (:flashcards/anki_note_id card)]
                {:card-id (:flashcards/id card) :note-id note-id}))))
    display-rows))

(defn overlay->filter-ids
  "Group the sparse Anki overlay into filter ID sets."
  [anki-overlay]
  {:anki-changed (set (keep (fn [[card-id flags]]
                              (when (:anki-modified flags) card-id))
                       anki-overlay))
   :marked (set (keep (fn [[card-id flags]]
                        (when (:marked flags) card-id))
                 anki-overlay))
   :suspended (set (keep (fn [[card-id flags]]
                           (when (pos? (get-in flags [:suspended :suspended] 0))
                             card-id))
                    anki-overlay))
   :remarked (set (keep (fn [[card-id flags]]
                          (when (:remarked flags) card-id))
                   anki-overlay))})

(defn overlay-flags
  "Build one card's sparse Anki state map."
  [anki-modified? tags remark suspended]
  (let [marked? (boolean (some #{"marked"} tags))
        remarked? (boolean remark)
        suspended-count (get suspended :suspended 0)]
    (cond-> {}
      anki-modified? (assoc :anki-modified true)
      marked? (assoc :marked true)
      remarked? (assoc :remarked true)
      (string? remark) (assoc :remark remark)
      (pos? suspended-count) (assoc :suspended suspended))))

(defn row-flags
  "Fold Anki flags across all cards in one display row."
  [member-ids anki-overlay]
  (let [flags (keep #(get anki-overlay %) member-ids)
        remarks (into [] (keep :remark) flags)
        total (transduce (keep (comp :total :suspended)) + 0 flags)
        suspended (transduce (keep (comp :suspended :suspended)) + 0 flags)]
    (cond-> {:marked (boolean (some :marked flags))
             :remarked (boolean (some :remarked flags))
             :suspended (when (pos? total) {:total total :suspended suspended})}
      (seq remarks) (assoc :remarks remarks))))

(defn row-class
  "Return the CSS classes for the Anki state of a display row."
  [flags]
  (str (when (:marked flags) " card-anki-marked")
    (when (pos? (get-in flags [:suspended :suspended] 0)) " card-anki-suspended")
    (when (:remarked flags) " card-anki-remarked")
    (when (or (:marked flags)
            (pos? (get-in flags [:suspended :suspended] 0))
            (:remarked flags))
      " card-anki-state")))

(defn matching-row-indexes
  "Return the display-row indexes that have the requested Anki state."
  [row-member-ids anki-overlay state]
  (into []
    (keep-indexed
      (fn [index member-ids]
        (let [flags (row-flags member-ids anki-overlay)]
          (when (case state
                  :marked (:marked flags)
                  :suspended (pos? (get-in flags [:suspended :suspended] 0))
                  :remarked (:remarked flags)
                  false)
            index))))
    row-member-ids))

(defn next-row-index
  "Return the next matching index after current, and wrap to the first match."
  [matching-indexes current]
  (or (first (drop-while #(<= % current) matching-indexes))
    (first matching-indexes)))
