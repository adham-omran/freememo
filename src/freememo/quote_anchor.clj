(ns freememo.quote-anchor
  "Locate a NeoReader quote inside an EPUB chapter's stored HTML, and tint what
   it found. Two phases, both taking strings and returning data or strings:
   `place-highlights` decides where each quote goes, `mark-chapters` rewrites
   only the chapters that got one. No database, no HTTP, no Electric.

   No mutable DOM crosses either function's boundary. A Jsoup document lives
   inside one call and is serialized before it returns, so a caller can hold a
   result across syncs or hand it to another thread.

   Neither function may be named in an Electric reactive body: this is a `.clj`
   namespace, and `(e/server (quote-anchor/…))` compiles the symbol on the CLJS
   peer too. The caller is an HTTP route or a `.cljc` wrapper whose body is
   `#?(:clj … :cljs nil)`. `epub.clj`, reached from `api.clj`, is the precedent.

   The export carries no offset, so a quote is found by text alone. Comparison
   runs in a NORMALIZED space that removes every whitespace character and folds
   curly quotation marks. That is not a convenience — it is what makes the
   export's own damage match. Measured in the reference export
   (plans/boox-neoreader-google-drive-sync.md):

     `curse of knowledgeis our tendency …`   source has `<i>curse of knowledge</i> is …`
     `hindsight bias,or what is often …`     source has `<i>hindsight bias,</i> or …`

   NeoReader joins two inline elements with no separator, so the exported text
   is missing a space the source has. Removing all whitespace from both sides
   makes the two strings equal. The same property makes a quote that spans
   element boundaries match: `Generation<br/>In 2013, John McPhee …` crosses a
   heading and a paragraph, and the normalized forms agree however Jsoup
   renders the `<br/>`.

   Normalization is 1:1 or dropping, NEVER expanding. Every surviving character
   maps back to exactly one character of one Jsoup text node, which is what
   lets a normalized range be tinted in the DOM. A rule that expanded one
   character into two would break that mapping and must not be added.

   Ambiguity is real, not theoretical: `reflection` is a standalone highlight in
   the reference export and also sits inside longer highlights, and
   `Suggested Reading` is both a chapter heading and a highlight. A
   reading-order cursor resolves them WITHIN a chapter — each quote must be
   found at or after the end of the previous placement in that same chapter.
   The cursor resets at every chapter boundary, which is deliberate: see
   `place-highlights`."
  (:require [clojure.string :as str])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Document Element Node TextNode]))

(def highlight-color
  "The tint a synced NeoReader highlight carries.

   `#faebdd` is the `Orange` entry of the background palette in the Quill text
   popover (format_menu.cljs:174), so a synced mark is a colour the reader can
   already produce by hand.

   Two properties make it the only workable carrier. Quill's stock registry
   models `background-color` and keeps it through an edit round-trip, and this
   repository registers no blot of its own (quill_field.cljc:100) — an unknown
   class or `data-*` attribute would be dropped. `clean-html` independently
   allows `background-color` and strips both of the others
   (html_cleaner.clj:70, :121).

   It MUST differ from `#44C2FF`, which extractor.clj:40 documents as the
   manual extract highlight. The same colour would make a synced mark
   indistinguishable from an extract."
  "#faebdd")

;; ── Normalization ──────────────────────────────────────────────────

(def ^:private quote-folds
  "Curly punctuation folded to its ASCII form. Every entry is one character to
   one character, per the namespace docstring's 1:1 invariant.

   Written as escapes on purpose: a bare curly quote in source is a reader
   hazard, and an editor that normalizes punctuation would silently empty this
   map."
  {\u2018 \' \u2019 \'      ; single quotation marks
   \u201c \" \u201d \"      ; double quotation marks
   \u2032 \' \u2033 \"})    ; prime, double prime

(defn- fold-char
  "Pre : `c` is a character.
   Post: nil when `c` is whitespace, otherwise the folded character."
  [^Character c]
  (when-not (Character/isWhitespace c)
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
    (let [sb (StringBuilder.)]
      (dotimes [i (count s)]
        (when-let [c (fold-char (.charAt ^String s i))]
          (.append sb c)))
      (.toString sb))))

;; ── Chapter text ───────────────────────────────────────────────────
;;
;; `chapter-text` and `position-index` MUST agree character for character.
;; Phase 1 matches against the first and phase 2 tints using the second, so a
;; drift between them would mark text the placement never chose. They agree
;; because both derive from `text-nodes` and `fold-char` and from nothing else.
;; A test asserts the agreement.

(defn- parse-body
  "Pre : `html` is a chapter topic's stored content, or nil.
   Post: the body Element of a fragment parse. nil yields an empty body."
  ^Element [html]
  (let [^Document doc (Jsoup/parseBodyFragment (or html ""))]
    (.body doc)))

(defn- text-nodes
  "Every TextNode under `root`, in document order.

   Pre : `root` is a parsed node.
   Post: a vector of TextNode. Order is the order a reader meets the text.
         A plain recursive walk, not a Jsoup NodeVisitor: the visitor needs a
         `reify` whose method arities must track Jsoup's interface, and this
         needs no such coupling. Recursion depth is HTML nesting depth."
  [^Node root]
  (if (instance? TextNode root)
    [root]
    (into [] (mapcat text-nodes) (.childNodes root))))

(defn- fold-text-nodes
  "Fold every text node under `root`, calling `f` with each surviving character.

   Pre : `root` is a parsed element. `f` takes [text-node raw-offset character].
   Post: `f` has been called once per character of the chapter's normalized
         text, in normalized order. The single traversal both `chapter-text`
         and `position-index` are built on."
  [^Element root f]
  (doseq [^TextNode tn (text-nodes root)]
    (let [^String whole (.getWholeText tn)]
      (dotimes [i (count whole)]
        (when-let [c (fold-char (.charAt whole i))]
          (f tn i c))))))

(defn chapter-text
  "The normalized text of a chapter, with no position map.

   Pre : `html` is a chapter topic's stored content, or nil.
   Post: the same string `position-index` puts in its `:text`. Phase 1 uses
         this because it needs no position map, and the map is what costs
         memory — roughly 96 bytes per character, measured."
  [html]
  (let [sb (StringBuilder.)]
    (fold-text-nodes (parse-body html) (fn [_ _ c] (.append sb ^Character c)))
    (.toString sb)))

(defn- position-index
  "Parse a chapter and map every normalized character back into its DOM.

   Pre : `html` is a chapter topic's stored content, or nil.
   Post: {:root <body Element> :text <normalized string>
          :positions [[TextNode raw-offset] …]}, with one position per
         character of `:text`.
   Inv : (count positions) = (count text). Entry i names the text node and the
         offset within that node's whole text that produced character i, which
         is what `mark-anchors!` needs to tint a normalized range.
   Inv : `:root` is mutable and MUST NOT escape this namespace."
  [html]
  (let [root (parse-body html)
        sb (StringBuilder.)
        positions (volatile! [])]
    (fold-text-nodes root
      (fn [tn i c]
        (.append sb ^Character c)
        (vswap! positions conj [tn i])))
    {:root root :text (.toString sb) :positions @positions}))

(defn- find-from
  "First occurrence of `needle` in `haystack` at or after `from`.

   Pre : `needle` and `haystack` are normalized strings. `from` is a
         non-negative index, and may exceed the haystack's length.
   Post: [start end] as normalized indices, or nil. A blank needle yields nil —
         an empty quote would match everywhere and place nothing meaningful."
  [^String haystack ^String needle ^long from]
  (when (seq needle)
    (let [i (.indexOf haystack needle (int from))]
      (when (>= i 0)
        [i (+ i (count needle))]))))

;; ── Phase 1: placement ─────────────────────────────────────────────

(defn- chapter-key
  "Comparison key for a chapter title.

   Pre : `title` is a string or nil.
   Post: the normalized title, lower-cased, or \"\" for nil and blank.
         Exact-after-normalization is the whole rule: quote-level fuzziness is
         a separate, unresolved decision (spec item 4.7), and guessing here
         would hide it."
  [title]
  (str/lower-case (normalize title)))

(defn- positions-by-title
  "Every chapter position each title occupies.

   Pre : `chapters` is a seq of {:title :html} in reading order.
   Post: a map from `chapter-key` to a set of chapter positions. A title is
         mapped to EVERY chapter bearing it, not just the first — a book that
         repeats a title (several `Notes` sections, or `epub.clj`'s
         `extract-chapter-title` yielding the same first heading for
         consecutive spine items) must not send all of its highlights to the
         earliest one."
  [chapters]
  (reduce (fn [m [pos c]] (update m (chapter-key (:title c)) (fnil conj #{}) pos))
    {}
    (map-indexed vector chapters)))

(defn- eligibility
  "Attach the search plan to each highlight.

   Pre : `highlights` is `neoreader-export/parse-highlights` output, in
         document order. `by-title` is `positions-by-title` output.
   Post: a vector of {:slot :quoted :needle :at :outcome}, where `:slot` is the
         highlight's position in the input, `:at` is the set of chapter
         positions the highlight may be placed in or `:any`, and `:outcome` is
         the outcome to record on a hit.
   Inv : `:at` is `:any` exactly when the declared heading names no chapter, so
         `:outcome` is `:fallback` for exactly those highlights (spec 4.6)."
  [highlights by-title]
  (into []
    (map-indexed (fn [slot h]
                   (let [at (get by-title (chapter-key (:chapter h)))]
                     {:slot slot
                      :quoted h
                      :needle (normalize (:quote h))
                      :at (or at :any)
                      :outcome (if at :placed :fallback)})))
    highlights))

(defn place-highlights
  "Decide where every highlight goes, walking the book once.

   Pre : `chapters` is a seq of {:title :html} in reading order. The caller
         owes the conversion from topic rows — a chapter row arrives as
         `:topics/title` and `:topics/content`.
         `highlights` is `neoreader-export/parse-highlights` output, in
         document order.
   Post: a vector positionally matching `highlights`, each entry
         {:index :chapter-pos :start :end :outcome}. `:index` echoes the
         highlight's own `:index`. `:outcome` is :placed when the quote was
         found in a chapter its heading names, :fallback when the heading named
         no chapter and a forward search found it, and :unmatched otherwise.
         `:chapter-pos`, `:start` and `:end` are nil for :unmatched.
   Inv : one chapter's normalized text is live at a time, so peak memory tracks
         the LARGEST chapter and not the book. This is why the walk is over
         chapters and not over highlights.
   Inv : within a chapter, placements advance monotonically — each search
         starts at the end of the previous placement in that chapter, so the
         standalone `reflection` highlight lands after its predecessor rather
         than at the chapter's first occurrence.
   Inv : the cursor RESETS at every chapter boundary. A highlight is therefore
         never blocked by a placement in an earlier chapter. The stricter
         book-wide cursor was tried and rejected: it made every highlight of a
         duplicate-titled later chapter :unmatched, because the title resolved
         to the earlier chapter which lay behind the cursor.
   Note: a quote absent from every chapter its heading allows is :unmatched and
         is never placed approximately. Whether a similarity threshold should
         sit behind this is spec item 4.7 and is not decided, so this function
         does not guess."
  [chapters highlights]
  (let [chapters (vec chapters)
        prepared (eligibility highlights (positions-by-title chapters))]
    (loop [pos 0
           pending prepared
           placed {}]
      (if (or (>= pos (count chapters)) (empty? pending))
        (mapv (fn [{:keys [slot quoted]}]
                (or (get placed slot)
                  {:index (:index quoted) :chapter-pos nil
                   :start nil :end nil :outcome :unmatched}))
          prepared)
        (let [text (chapter-text (:html (nth chapters pos)))
              step (fn [[still found cursor] entry]
                     (if-let [[s e] (when (or (= :any (:at entry))
                                          (contains? (:at entry) pos))
                                      (find-from text (:needle entry) cursor))]
                       [still
                        (assoc found (:slot entry)
                          {:index (:index (:quoted entry)) :chapter-pos pos
                           :start s :end e :outcome (:outcome entry)})
                        e]
                       [(conj still entry) found cursor]))
              [still found _] (reduce step [[] placed 0] pending)]
          (recur (inc pos) still found))))))

;; ── Phase 2: tinting ───────────────────────────────────────────────

(defn- node-runs
  "Split a normalized range into per-text-node raw offsets.

   Pre : `positions` is a `position-index` `:positions` vector. 0 <= start <
         end <= (count positions).
   Post: a vector of [TextNode from to] in document order, where `from` and
         `to` are offsets into that node's whole text and `to` is exclusive.
         Consecutive characters of one node collapse into one run."
  [positions ^long start ^long end]
  (reduce (fn [runs i]
            (let [[node off] (nth positions i)
                  [prev-node prev-from prev-to] (peek runs)]
              (if (and (identical? node prev-node) (= prev-to off))
                (conj (pop runs) [node prev-from (inc off)])
                (conj runs [node off (inc off)]))))
    []
    (range start end)))

(defn- wrap-run!
  "Tint one run inside one text node. Mutates the node's document.

   Pre : `node` is attached to a document. 0 <= from < to <= length of the
         node's whole text.
   Post: the characters in [from, to) sit inside a span carrying
         `highlight-color`. The node object keeps holding the text BEFORE
         `from`, which is why a caller working in descending order keeps every
         earlier offset valid."
  [^TextNode node ^long from ^long to]
  (let [target (if (pos? from) (.splitText node (int from)) node)]
    (when (< (- to from) (count (.getWholeText target)))
      (.splitText target (int (- to from))))
    (.wrap target (str "<span style=\"background-color: " highlight-color ";\"></span>"))))

(defn- mark-anchors!
  "Tint every range in one indexed chapter. Mutates its DOM.

   Pre : `index` is `position-index` output. `ranges` is a non-empty seq of
         [start end] normalized indices, each non-empty and within bounds.
         Private because that precondition is not defensible against a caller
         holding raw placements: an :unmatched placement is [nil nil], which is
         not nil and would reach `node-runs`.
   Post: every range sits inside a `highlight-color` span. Returns `:root`.
   Inv : ranges are applied in DESCENDING start order, and runs within a range
         in descending offset order. Splitting a text node invalidates offsets
         for text AFTER the split point only, so descending order keeps every
         not-yet-applied offset correct. The caller does not pre-sort.
   Inv : `index` is STALE once this returns."
  [index ranges]
  (let [positions (:positions index)]
    (doseq [[s e] (sort-by (comp - first) ranges)
            [node from to] (reverse (node-runs positions s e))]
      (wrap-run! node from to))
    (:root index)))

(defn mark-chapters
  "Tint the placed highlights, returning HTML for the chapters that changed.

   Pre : `chapters` is the same seq, in the same order, that `place-highlights`
         was given. `placements` is its output.
   Post: a map from chapter position to that chapter's rewritten HTML,
         holding an entry ONLY for a chapter with at least one placement. A
         chapter with none is absent, so the caller writes nothing for it —
         re-serializing an untouched chapter would still change its HTML,
         because a Jsoup parse and serialize can alter attribute quoting and
         entity escaping, and that write would bump the refresh channel and
         re-initialize Quill for no change.
   Inv : one chapter's position index is live at a time, and it is discarded
         before the next chapter is parsed.
   Inv : an :unmatched placement contributes nothing, so no [nil nil] range can
         reach `mark-anchors!`."
  [chapters placements]
  (let [chapters (vec chapters)
        by-chapter (reduce (fn [m {:keys [chapter-pos start end]}]
                             (if (some? chapter-pos)
                               (update m chapter-pos (fnil conj []) [start end])
                               m))
                     {}
                     placements)]
    (reduce-kv (fn [out pos ranges]
                 (let [index (position-index (:html (nth chapters pos)))]
                   (mark-anchors! index ranges)
                   (assoc out pos (.html ^Element (:root index)))))
      {}
      by-chapter)))
