(ns freememo.cloze
  "Cloze-deletion syntax: one index-aware parse, shared by validation, the quiz
   review surface, and card grading.

   Anki cloze markers are {{cN::answer}} or {{cN::answer::hint}}, and may nest —
   {{c3:: ... {{c1::x}} ...}}. `parse` is a single left-to-right scan that balances
   openers against closers with a stack, so nested clozes match correctly and stray
   braces from code content (a trailing }} that closes two Java blocks) are ignored
   rather than miscounted. Pure: no I/O, no platform deps beyond regex indices.

   `parse` is the ONE definition of \"which deletions does this text have\". The
   quiz draws one item per deletion (see plans/cards-in-quiz-queue.md), so a second
   scanner anywhere would let the drawn items drift from the validated ones.

   Math regions are invisible to the scan: TeX brace nesting (`x^{a^{b}}`) emits
   `}}` that would otherwise be counted as a cloze closer. A cloze may wrap a whole
   formula; it may not cut into one."
  (:require [freememo.math :as math]
            [clojure.string :as str]))

(defn- parse-cloze-int [s]
  #?(:cljs (js/parseInt s 10)
     :clj  (Integer/parseInt ^String s)))

(def ^:private token-re
  "Matches one cloze opener {{cN:: (capturing N) or one closer }}."
  #"\{\{c(\d+)::|\}\}")

(defn- matches
  "Every match of `re` in `s` as {:g1 <group 1 or nil> :start i :end i}, in document
   order. Group 1 stays a string — only the caller knows what it means.
   Pre:  `re` never matches the empty string (else the CLJS loop would not
         advance — caller bug).
   Post: ordered by :start; :end is exclusive."
  [re s]
  #?(:clj
     (let [m (re-matcher re s)]
       (loop [acc []]
         (if (.find m)
           (recur (conj acc {:g1 (.group m 1) :start (.start m) :end (.end m)}))
           acc)))
     :cljs
     (let [g (js/RegExp. (.-source re) "g")]
       (loop [acc []]
         (if-some [m (.exec g s)]
           (recur (conj acc {:g1 (aget m 1)
                             :start (.-index m)
                             :end (+ (.-index m) (.-length (aget m 0)))}))
           acc)))))

(defn- inside?
  "Whether index `i` falls inside any [start end) range in `ranges`."
  [ranges i]
  (boolean (some (fn [{:keys [start end]}] (and (<= start i) (< i end))) ranges)))

(defn parse
  "Parse `text` into its cloze structure. The single scan every other fn here uses.

   pre : `text` is a string or nil.
   post: {:spans   nested spans, outermost first, in document order
          :ords    sorted distinct N over EVERY opener, closed or not
          :unclosed count of openers with no closer}
         A span is {:n int :outer-start i :outer-end i :inner-start i :inner-end i
                    :children [span]}. Indices address `text` itself, so a caller
         can splice. Openers and closers inside `\\(…\\)` math regions are ignored.
         An unclosed opener contributes to :ords and :unclosed but yields no span."
  [text]
  (let [s (or text "")
        maths (matches math/tex-region-re s)]
    (loop [ts (matches token-re s), stack [], roots [], ords #{}]
      (if-some [t (first ts)]
        (cond
          (inside? maths (:start t))
          (recur (rest ts) stack roots ords)

          (:g1 t)                                       ; opener: {{cN::
          (let [n (parse-cloze-int (:g1 t))]
            (recur (rest ts)
              (conj stack {:n n :outer-start (:start t)
                           :inner-start (:end t) :children []})
              roots
              (conj ords n)))

          (seq stack)                                   ; closer with an open span
          (let [done (assoc (peek stack) :inner-end (:start t) :outer-end (:end t))
                rest-stack (pop stack)]
            (if (seq rest-stack)
              (recur (rest ts)
                (conj (pop rest-stack) (update (peek rest-stack) :children conj done))
                roots ords)
              (recur (rest ts) rest-stack (conj roots done) ords)))

          :else                                         ; stray }} — literal text
          (recur (rest ts) stack roots ords))
        {:spans roots
         :ords (vec (sort ords))
         :unclosed (count stack)}))))

(defn validate
  "Return an error string if `text` is not a well-formed cloze field, else nil.

   Checks, in order: at least one cloze deletion exists; every opener {{cN:: is
   balanced by a later }} (nesting-aware — an unmatched }} is treated as literal
   text and ignored); cloze numbers run 1..max with no gaps (duplicates allowed).

   pre : `text` is a string or nil.
   post: nil ⟺ text has ≥1 opener, every opener is closed, and numbering is a
         gap-free 1..max; otherwise a human-readable reason string. Braces inside
         `\\(…\\)` math regions are invisible to the scan."
  [text]
  (let [{:keys [ords unclosed]} (parse text)
        max-n (if (seq ords) (apply max ords) 0)]
    (cond
      (empty? ords)
      "No cloze deletion: select text and press the {+} button to add {{c1::...}}"
      (pos? unclosed)
      "Unclosed cloze: a {{cN::...}} is missing its closing }}"
      (not= (set ords) (set (range 1 (inc max-n))))
      (str "Non-sequential cloze numbers: found " (sort ords) ", expected 1 to " max-n)
      :else nil)))

(defn ords
  "Sorted distinct deletion numbers in `text` — the quiz items this card yields.
   post: a vector, ascending, possibly empty."
  [text]
  (:ords (parse text)))

;; ---------------------------------------------------------------------------
;; Rendering one deletion — the quiz item's prompt and answer
;; ---------------------------------------------------------------------------

(def blank
  "What a hidden deletion renders as, in the prompt and in Anki alike."
  "[...]")

(defn- strip-hint
  "Drop a trailing `::hint` from one span's revealed inner text.

   Splits at the FIRST `::`, which is Anki's own rule. A deletion whose answer
   legitimately contains `::` (`std::vector`) therefore loses everything after it —
   the same way it does in Anki. Nested spans are already substituted by the time
   this runs, so no `::` here belongs to an opener."
  [s]
  (let [i (str/index-of s "::")]
    (if i (subs s 0 i) s)))

(declare ^:private render-children)

(defn- render-span
  "One span: `blank` when it is the hidden ord, else its revealed answer text."
  [text span target]
  (if (= target (:n span))
    blank
    (strip-hint (render-children text span target))))

(defn- splice
  "Replace each span in `spans` with `(render span)`, keeping the text between them.
   Post: the region [from to) of `text` with every span substituted."
  [text spans from to render]
  (let [[out cursor]
        (reduce
          (fn [[out cursor] span]
            [(str out (subs text cursor (:outer-start span)) (render span))
             (:outer-end span)])
          ["" from]
          spans)]
    (str out (subs text cursor to))))

(defn- render-children
  "A span's inner text with every nested span rendered. `target` stays hidden at
   any depth, so a nested deletion never leaks the answer of the item being asked."
  [text {:keys [inner-start inner-end children]} target]
  (splice text children inner-start inner-end #(render-span text % target)))

(defn mask-ord
  "`text` with deletion `n` hidden and every other deletion revealed — Anki's
   default rendering, and the prompt the learner sees.

   pre : `text` is a string or nil; `n` is an int.
   post: contains `blank` once per span bearing `n`; contains no `{{cN::` marker
         for any span the scan resolved. Text outside every span is verbatim."
  [text n]
  (let [s (or text "")
        {:keys [spans]} (parse s)]
    (splice s spans 0 (count s) #(render-span s % n))))

(defn answer-for-ord
  "The text deletion `n` hides — the reference answer for that quiz item.

   Several spans may carry the same `n` (Anki allows it); their revealed texts are
   joined with \" / \". Nested deletions inside are revealed, since they are part of
   this answer's own text.
   pre : `text` is a string or nil; `n` is an int.
   post: a string, empty when no span carries `n`."
  [text n]
  (let [s (or text "")
        {:keys [spans]} (parse s)
        matching (filter #(= n (:n %))
                   (tree-seq :children :children {:children spans}))]
    (str/join " / "
      (map #(str/trim (strip-hint (render-children s % nil))) matching))))
