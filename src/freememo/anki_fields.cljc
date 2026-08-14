(ns freememo.anki-fields
  "Anki note field text — stripping and ordering. Two pure functions, no deps.

   Extracted from `freememo.anki-sync-helpers` so `freememo.anki-digest` can
   reach them without pulling in missionary, the CLJS push pipeline and
   `occlusion-svg`. The digest is what every other reconcile namespace depends
   on, so it must sit BELOW the sync machinery, not above it.

   `anki-sync-helpers` re-exports both names, so its existing callers are
   unaffected."
  (:require [clojure.string :as str]))

(defn strip-html
  "Remove all HTML tags and push-time decorations from Anki field text.
   Strips: fm-header tagged headers, source/bibliography suffix (<hr>...),
   then all remaining HTML tags."
  [text]
  (if (str/blank? text)
    text
    (-> text
      (str/replace #"<p[^>]*class=\"fm-header\"[^>]*>.*?</p>" "") ;; strip tagged header
      (str/replace #"\n?<hr[^>]*class=\"fm-(?:source|bibliography)\"[^>]*>[\s\S]*$" "") ;; strip tagged source/bib block
      (str/replace #"<br\s*/?>" "\n") ;; preserve line breaks
      (str/replace #"<[^>]*>" "") ;; strip all remaining tags
      str/trim)))

(def no-inverse-kinds
  "Card kinds whose Anki fields are derived from app state with no
   Anki→FreeMemo inverse.

   Score fields are media references generated from the score group. Overlapping
   fields are the expanded per-cloze list. Reading either back would have to
   guess the state it came from, so nothing is pulled for them.

   THE single definition. `note->pull-update` returns nil for these, and
   `anki-reconcile/apply-kind-limits` turns their pulls into conflicts. Two
   copies would let one path silently drop what the other reports.
   Occlusion is NOT here — its six text fields do pull, through `io_fields`."
  #{"score" "overlapping"})

(defn pullable-kind?
  "True iff Anki's version of a `kind` card can be written back locally."
  [kind]
  (not (contains? no-inverse-kinds kind)))

(def content-field-names
  "Per card kind, the Anki fields whose text the app is responsible for.

   Read BY NAME, never by position. `modelFieldAdd` appends, so a model that
   existed before `Links` was introduced holds it at a different index from one
   created after, and no single index map serves both — `io-pull-field-names`
   records the same lesson for the pull path, where the failure was writing
   `Extra 2`'s text into `:extra1`. A digest taken over the wrong positions
   fails more quietly still: the card simply reports a change forever.

   The omissions are deliberate. Each is a field the user owns in Anki, or one
   the app regenerates from other state:
     - `Remarks` / `Back Extra` are the user's Anki-side space (see
       `migrate-carry-fields`). An edit there must not read as a change.
     - `Links` is regenerated from the topic and its bibliography on every push.
     - occlusion `Image`, the three masks and `ID (hidden)` are app-owned media,
       tracked by `anki_key`, not by text.
   Occlusion keeps its own `Remarks`, because `io-pull-field-names` pulls it —
   the digest and the pull must cover the same fields, or a pulled edit would
   not clear the flag it raised.

   Guarded by `anki-reconcile-test/t26-digest-field-names-exist-in-their-model`."
  {"basic"       ["Front" "Back"]
   "cloze"       ["Text"]
   "occlusion"   ["Header" "Footer" "Remarks" "Sources" "Extra 1" "Extra 2"]
   "score"       ["Front" "Back"]
   "overlapping" (into ["Question"]
                   (concat (map #(str "Text" %) (range 1 21))
                     ["Full" "Original"]))})

(defn ordered-field-values
  "Field values of an Anki notesInfo :fields map, sorted by Anki's :order.
   Shared by pull (field-update detection), the library cards Anki overlay
   (anki-modified diff) and the reconcile digest — one definition of 'Anki's
   field order'."
  [fields-map]
  (->> fields-map
    (sort-by (fn [[_ v]] (or (:order v) 0)))
    (mapv (fn [[_ v]] (:value v)))))
