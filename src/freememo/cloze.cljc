(ns freememo.cloze
  "Cloze-deletion syntax validation, shared by the client editors and the server
   save path.

   Anki cloze markers are {{cN::answer}} or {{cN::answer::hint}}, and may nest —
   {{c3:: ... {{c1::x}} ...}}. Validation is a single left-to-right scan that
   balances openers against closers with a depth counter, so nested clozes match
   correctly and stray braces from code content (a trailing }} that closes two
   Java blocks) are ignored rather than miscounted. Pure: no I/O, no platform
   deps beyond integer parsing.")

(defn- parse-cloze-int [s]
  #?(:cljs (js/parseInt s 10)
     :clj  (Integer/parseInt ^String s)))

(def ^:private token-re
  "Matches one cloze opener {{cN:: (capturing N) or one closer }}."
  #"\{\{c(\d+)::|\}\}")

(defn validate
  "Return an error string if `text` is not a well-formed cloze field, else nil.

   Checks, in order: at least one cloze deletion exists; every opener {{cN:: is
   balanced by a later }} (nesting-aware — an unmatched }} is treated as literal
   text and ignored); cloze numbers run 1..max with no gaps (duplicates allowed).

   pre : `text` is a string or nil.
   post: nil ⟺ text has ≥1 opener, every opener is closed, and numbering is a
         gap-free 1..max; otherwise a human-readable reason string."
  [text]
  (let [{:keys [nums depth]}
        (reduce (fn [acc tok]
                  (if-let [n (second tok)]              ; opener: {{cN::
                    (-> acc (update :nums conj (parse-cloze-int n))
                            (update :depth inc))
                    (cond-> acc                          ; closer: }} (pop if open)
                      (pos? (:depth acc)) (update :depth dec))))
          {:nums #{} :depth 0}
          (re-seq token-re (or text "")))
        max-n    (if (seq nums) (apply max nums) 0)
        expected (set (range 1 (inc max-n)))]
    (cond
      (empty? nums)
      "No cloze deletion: select text and press the {+} button to add {{c1::...}}"
      (pos? depth)
      "Unclosed cloze: a {{cN::...}} is missing its closing }}"
      (not= nums expected)
      (str "Non-sequential cloze numbers: found " (sort nums) ", expected 1 to " max-n)
      :else nil)))
