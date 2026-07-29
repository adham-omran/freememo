(ns freememo.occlusion-ordinals
  "Mask-ordinal algebra for image-occlusion geometry: the rule that turns rect
   membership keys into card-bearing ordinals. Pure and platform-free — shared
   by freememo.db's geometry writes, the authoring modal's counts, and the
   pending-card overlay, and testable without a database.

   Vocabulary: an occlusion_groups row is a 'group' everywhere else in this
   codebase, so a set of rects sharing one ordinal is a MASK GROUP, and one
   mask group is one flashcards row is one Anki note is one card.

   Membership contract for an incoming rect (plans/occlusion-mask-grouping.md
   §4.1.1) — at most one key:
     :ordinal int     belongs to a mask group that already owns a row; several
                      rects MAY share it, and sharing it IS the mask group
     :gid     string  belongs to a NEW mask group; rects sharing a :gid become
                      one ordinal, hence one card
     neither          a new singleton mask group
     both             invalid — freememo.occlusion/checked-geometry rejects it")

(defn assign-ordinals
  "Stamp a card-bearing :ordinal on every rect, one ordinal per mask group.
   Pre:  rects satisfy the membership contract; next-ordinal is the group's
         unused-ordinal watermark (occlusion_groups.next_ordinal).
   Post: every rect carries an :ordinal and no :gid; rects that shared a :gid
         share one freshly minted ordinal; a rect that arrived with an :ordinal
         keeps it. Returns [rects next-ordinal'], and every minted ordinal is
         >= the incoming next-ordinal — that is the caller's test for 'this
         ordinal needs a new row'.
   Blame: a rect carrying both keys is a caller bug; :ordinal wins here."
  [rects next-ordinal]
  (let [{:keys [out next]}
        (reduce
          (fn [{:keys [out next gid->ordinal] :as st} rect]
            (let [gid (:gid rect)]
              (if-let [known (or (:ordinal rect) (get gid->ordinal gid))]
                (assoc st :out (conj out (-> rect (assoc :ordinal known) (dissoc :gid))))
                (cond-> (assoc st
                          :out (conj out (-> rect (assoc :ordinal next) (dissoc :gid)))
                          :next (inc next))
                  gid (assoc-in [:gid->ordinal gid] next)))))
          {:out [] :next next-ordinal :gid->ordinal {}}
          rects)]
    [out next]))

(defn ordinals-in-order
  "Distinct ordinals of `rects` in first-appearance order — one per card.
   Pre:  every rect carries an :ordinal (i.e. post-assign-ordinals).
   Post: (count result) = the number of mask groups."
  [rects]
  (into [] (distinct) (map :ordinal rects)))

(defn card-count
  "How many cards `rects` produce, before ordinals are assigned.
   Pre:  rects satisfy the membership contract (ordinals need not be present).
   Post: rects sharing an :ordinal or a :gid count once; a rect with neither
         counts on its own. Keys are namespaced so an :ordinal and a :gid of
         the same value cannot collide."
  [rects]
  (count
    (into #{}
      (map-indexed (fn [i {:keys [ordinal gid]}]
                     (cond
                       (some? ordinal) [:ordinal ordinal]
                       (some? gid) [:gid gid]
                       :else [:solo i])))
      rects)))
