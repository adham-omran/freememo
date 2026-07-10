(ns freememo.overlapping
  "Pure derivation of Cloze-Overlapper-style fields from an ordered list.

   One overlapping-cloze card is authored as an ordered list of items plus
   window settings. Anki's Cloze-Overlapper model needs one cloze field per
   item (Text1..TextN), where item k is the active deletion {{ck::…}} and the
   items around it form plain-text context, plus a Full field that clozes every
   item together under c21 (the 'reveal all' card). This ns is the single
   source of that expansion — shared by the server save path and any
   client-side preview. It is pure: no I/O, no platform deps."
  (:require [clojure.string :as str]))

(def max-items
  "Anki's model exposes Text1..Text20 and Full uses cloze number 21, so a note
   holds at most 20 list items."
  20)

(def full-cloze-number
  "Fixed cloze index of the Full 'reveal all' card. Fixed (not N+1) so the model
   CSS can target the last card regardless of list length; safe because list
   items only ever use c1..c20 (max-items) and never reach 21."
  21)

(def default-settings
  "Window defaults reproducing the observed Cloze-Overlapper output: one line of
   prior context shown as text, none after, a reveal-all card, left-to-right."
  {:before 1 :after 0 :reveal-all? true :direction "ltr"})

(defn- div [s] (str "<div>" s "</div>"))

(defn- in-context-window?
  "Is 1-based position i within the visible plain-text window of active
   position k? before/after are line counts of surrounding context."
  [i k before after]
  (and (not= i k)
    (>= i (- k before))
    (<= i (+ k after))))

(defn text-field
  "The Text_k field value: item k as the active cloze {{ck::…}}, items inside
   the [before,after] window as plain text, all others collapsed to '...'.
   pre:  1 <= k <= (count items); items are plain HTML without cloze markers.
   post: a string of (count items) <div> lines, exactly one carrying {{ck::}}."
  [items k {:keys [before after]}]
  (->> (map-indexed
         (fn [idx item]
           (let [i (inc idx)]
             (cond
               (= i k)                                (str "{{c" k "::" item "}}")
               (in-context-window? i k before after)  item
               :else                                  "...")))
         items)
    (map div)
    (str/join "")))

(defn full-field
  "The Full field: every item clozed together under c21 (the reveal-all card).
   Empty string when reveal-all? is false, so no Full card is generated."
  [items reveal-all?]
  (if reveal-all?
    (->> items
      (map #(div (str "{{c" full-cloze-number "::" % "}}")))
      (str/join ""))
    ""))

(defn original-field
  "The Original field: the plain list, one <div> line per item. Shown on the
   back as the fully-revealed reference."
  [items]
  (->> items (map div) (str/join "")))

(defn expand
  "Derive the Anki cloze fields from an ordered item list + window settings.
   pre:  1 <= (count items) <= max-items; items are plain HTML, no clozes.
   post: {:Original s :Full s :Text1 s … :TextN s}, N = (count items); Text keys
         beyond N are absent (the field builder fills the blanks).
   Blame: pre violated → caller (save/authoring) bug — throws ex-info rather
   than silently truncating."
  [items settings]
  (let [items (vec items)
        n (count items)
        {:keys [reveal-all?] :as s} (merge default-settings settings)]
    (when (zero? n)
      (throw (ex-info "overlapping card needs at least one item" {:count n})))
    (when (> n max-items)
      (throw (ex-info (str "overlapping card exceeds " max-items " items")
               {:count n :max max-items})))
    (into {:Original (original-field items)
           :Full (full-field items reveal-all?)}
      (map (fn [k] [(keyword (str "Text" k)) (text-field items k s)]))
      (range 1 (inc n)))))
