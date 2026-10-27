(ns freememo.quote-fold
  "The comparison space a quote is matched in, on both peers
   (plans/document-question-pins.md §3.1).

   The server matches a NeoReader quote or a pin on rendered Markdown
   (`freememo.quote-anchor`). The browser matches a pin in a live Quill editor
   (`freememo.question-pin-dom`). Both MUST fold one character the same way,
   or a quote that matches on one peer is stale on the other. So this is the
   ONE definition, and it has no requires: a `.cljc` with none is the part of
   the matcher a Clojure test can reach.

   Normalization is 1:1 or dropping, NEVER expanding. Every surviving
   character maps back to exactly one input character, which is what lets a
   normalized range be tinted in a DOM.")

(def ^:private quote-folds
  "Curly punctuation folded to its ASCII form. Every entry is one character to
   one character.

   Written as escapes on purpose: a bare curly quote in source is a reader
   hazard, and an editor that normalizes punctuation would silently empty this
   map."
  {\u2018 \' \u2019 \'      ; single quotation marks
   \u201c \" \u201d \"      ; double quotation marks
   \u2032 \' \u2033 \"})    ; prime, double prime

(defn- code-of [c]
  #?(:clj (int ^Character c)
     :cljs (.charCodeAt c 0)))

(defn whitespace?
  "True for a character the comparison space drops.

   The set is spelled out, because the two peers' own tests disagree. Java's
   `Character/isWhitespace` keeps U+00A0 and U+FEFF, and JavaScript's `\\s`
   drops both. Quill wraps every embed, a formula included, in two U+FEFF
   guard characters, so a quote captured in the editor and the same text in
   stored HTML differ by exactly those (§3.1.2)."
  [c]
  (let [n (code-of c)]
    (or (<= 0x09 n 0x0D)        ; tab, line feed, vertical tab, form feed, return
      (<= 0x1C n 0x20)          ; information separators, space
      (= n 0xA0)                ; no-break space
      (= n 0x1680)
      (<= 0x2000 n 0x200A)      ; en quad .. hair space
      (= n 0x2028) (= n 0x2029) ; line and paragraph separators
      (= n 0x202F) (= n 0x205F) (= n 0x3000)
      (= n 0xFEFF))))           ; zero-width no-break space: Quill's guard

(defn fold-char
  "Pre : `c` is a character (a one-character string on CLJS).
   Post: nil when `c` is dropped, otherwise the folded character."
  [c]
  (when-not (whitespace? c)
    (get quote-folds c c)))

(defn normalize
  "Fold `s` into the comparison space.

   Pre : `s` is a string or nil.
   Post: a string holding no whitespace and no curly quotation mark. nil yields
         \"\".
   Inv : the result's length is at most the input's. Each output character
         comes from exactly one input character."
  [s]
  (if (nil? s)
    ""
    #?(:clj (let [sb (StringBuilder.)]
              (dotimes [i (count s)]
                (when-let [c (fold-char (.charAt ^String s i))]
                  (.append sb c)))
              (.toString sb))
       :cljs (let [out (array)]
               (dotimes [i (count s)]
                 (when-let [c (fold-char (.charAt s i))]
                   (.push out c)))
               (.join out "")))))
