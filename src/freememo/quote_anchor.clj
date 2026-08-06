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
   `place-highlights`.

   A chapter gets THREE passes, each weaker than the last, and the first that
   finds a quote takes it. The cursor pass is the one above. The rescan pass
   searches the whole chapter for whatever lies before the cursor, because the
   cursor sits at the previous placement's END — so a highlight nested inside an
   earlier one, or an export not listed in reading order, would otherwise place
   nothing. The prefix pass retries the longest prefix of the quote. Every pass
   is EXACT matching in the normalized space — there is no similarity threshold
   anywhere, so no pass can ever mark text the quote does not literally contain.

   This namespace also owns the two sections a sync writes, `USER NOTES` on a
   chapter and `UNPLACED HIGHLIGHTS` on the document root. Both are found again
   by their heading TEXT, because nothing else survives a Quill edit round trip.
   `strip-sync-content` erases everything a previous sync wrote, and it lives
   here rather than beside the caller for one reason: the writer and the eraser
   must agree on the tint colour, the marker shape and the heading text, and one
   namespace is the only place they cannot drift."
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

(defn- tint-span-html
  "Pre : `inner` is HTML-safe text, possibly empty.
   Post: a span carrying `highlight-color` around it."
  [inner]
  (str "<span style=\"background-color: " highlight-color ";\">" inner "</span>"))

(def ^:private tint-selector
  "Jsoup selector for a span this namespace tinted. Keyed on the colour with no
   leading `#`, because that is how the value appears inside a `style` attribute
   however the serializer spaces it."
  (str "span[style*=" (subs highlight-color 1) "]"))

(defn- marker-html
  "Pre : `n` is a per-chapter note number, counted from 1.
   Post: the `[n]` footnote marker, as its own tinted span.
   Inv : the marker MUST be its own element and MUST carry `highlight-color`.
         Both make it identifiable on the next sync, which has to remove it
         before re-numbering. A bare text `[n]` would be indistinguishable from
         a citation the book itself carries, and stripping every bracketed
         number out of a chapter would destroy real content."
  [n]
  (tint-span-html (str "[" n "]")))

(def ^:private marker-pattern
  "Whole text of a tinted span that is a `[n]` marker rather than a quote."
  #"\s*\[\d+\]\s*")

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
   Note: an explicit stack, not a Jsoup NodeVisitor and not recursion. The
         visitor needs a `reify` whose method arities must track Jsoup's
         interface. Recursion would be bounded by HTML nesting depth, which an
         imported EPUB chooses and this code does not — a chapter of deeply
         nested `div`s would overflow the JVM stack."
  [^Node root]
  (loop [stack (list root) out []]
    (if (seq stack)
      (let [^Node n (first stack)]
        (if (instance? TextNode n)
          (recur (rest stack) (conj out n))
          (recur (concat (.childNodes n) (rest stack)) out)))
      out)))

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

(def ^:private min-prefix-chars
  "Shortest normalized prefix a partial placement may mark.

   Roughly one line of prose. A shorter prefix is not distinctive enough to be
   the passage the reader meant, so a placement on it would be a guess."
  40)

(defn- find-prefix
  "Longest prefix of `needle` that occurs in `haystack` at or after `from`.

   Pre : both strings are normalized. `from` is a non-negative index.
   Post: [start end] covering the longest prefix that both occurs and is at
         least `min-prefix-chars` long AND at least half of `needle`, or nil.
         The second bound is what keeps a long quote from placing on a small
         fragment of itself.
   Note: this settles spec item 4.7. It is EXACT matching on a shorter needle,
         NOT fuzzy matching, so it can never mark text the quote does not
         literally contain. A similarity threshold could, and a mark on the
         wrong sentence is worse than no mark — the reader has no way to tell
         the two apart once it is in the content.
   Inv : \"a prefix of length L occurs\" is monotonically decreasing in L, since
         a prefix of a prefix is a prefix. That is what makes the binary search
         valid and turns a 500-character quote into about 9 searches."
  [^String haystack ^String needle ^long from]
  (let [n (count needle)
        floor (max min-prefix-chars (quot n 2))]
    (when (>= n floor)
      (loop [lo floor hi n best nil]
        (if (> lo hi)
          best
          (let [mid (quot (+ lo hi) 2)]
            (if-let [hit (find-from haystack (subs needle 0 mid) from)]
              (recur (inc mid) hi hit)
              (recur lo (dec mid) best))))))))

(defn- merge-ranges
  "Pre : `ranges` is a seq of [start end] normalized index pairs, any order.
   Post: the same coverage as an ascending vector of DISJOINT [start end] pairs.
   Note: overlapping device highlights are real — a reader marks a phrase, then
         later marks the sentence holding it. Two nested spans of the SAME colour
         are invisible, and they break the rule `mark-anchors!` depends on:
         wrapping the inner range first shortens the text node the outer range's
         offsets were measured against, and the outer split then throws. Merging
         keeps every placement reported and every offset valid."
  [ranges]
  (reduce (fn [out [s e]]
            (let [[ps pe] (peek out)]
              (if (and ps (<= (long s) (long pe)))
                (conj (pop out) [ps (max (long pe) (long e))])
                (conj out [s e]))))
    []
    (sort ranges)))

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

(defn- place-in-chapter
  "Place every eligible pending highlight in one chapter.

   Pre : `text` is that chapter's normalized text. `pending` is `eligibility`
         output not yet placed, in document order. `pos` is the chapter's
         position.
   Post: [still found] — `still` is the entries this chapter did not take, in
         document order, and `found` maps a slot to its placement.
   Inv : three passes, each weaker than the last. A highlight is taken by the
         first that finds it, so the strongest evidence always wins.
   Inv : a pass only records an outcome. Two placements MAY share or overlap a
         range — `merge-ranges` is what makes that safe to tint."
  [pos text pending]
  (let [eligible? (fn [e] (or (= :any (:at e)) (contains? (:at e) pos)))
        pass (fn [acc outcome search]
               (reduce (fn [a e]
                         (if-let [[s end] (and (eligible? e) (search a e))]
                           (cond-> (assoc-in a [:found (:slot e)]
                                     {:index (:index (:quoted e))
                                      :notes (:notes (:quoted e))
                                      :chapter-pos pos :start s :end end
                                      :outcome (or outcome (:outcome e))})
                             ;; Only the cursor pass advances the cursor. The
                             ;; later passes exist BECAUSE it advanced too far.
                             (nil? outcome) (assoc :cursor end))
                           (update a :missed conj e)))
                 (assoc acc :missed [])
                 (:missed acc)))
        seed {:found {} :cursor 0 :missed (vec pending)}
        ;; Pass 1 — the reading-order cursor (spec 4.5). It advances to each
        ;; placement's END, which is what sends the standalone `reflection`
        ;; highlight to the occurrence after its predecessor.
        cursor-pass (pass seed nil (fn [a e] (find-from text (:needle e) (:cursor a))))
        ;; Pass 2 — the whole chapter, for whatever lies BEFORE the cursor. Two
        ;; causes, one fact: a highlight nested inside an earlier one, or an
        ;; export that does not list a chapter in reading order.
        rescan-pass (pass cursor-pass :out-of-order
                      (fn [_ e] (find-from text (:needle e) 0)))
        ;; Pass 3 — the longest prefix (spec 4.7).
        prefix-pass (pass rescan-pass :partial
                      (fn [_ e] (find-prefix text (:needle e) 0)))]
    [(:missed prefix-pass) (:found prefix-pass)]))

(defn place-highlights
  "Decide where every highlight goes, walking the book once.

   Pre : `chapters` is a seq of {:title :html} in reading order. The caller
         owes the conversion from topic rows — a chapter row arrives as
         `:topics/title` and `:topics/content`.
         `highlights` is `neoreader-export/parse-highlights` output, in
         document order.
   Post: a vector positionally matching `highlights`, each entry
         {:index :notes :chapter-pos :start :end :outcome}. `:index` and
         `:notes` echo the highlight's own. `:chapter-pos`, `:start` and `:end`
         are nil for :unmatched.
   Post: `:outcome` is one of five, strongest first —
           :placed      the quote was found in a chapter its heading names
           :fallback    the heading named no chapter, and a search found it
           :out-of-order it lies BEFORE where the reading-order cursor had
                        reached — a highlight nested inside an earlier one, or
                        an export not listed in reading order
           :partial     only its longest prefix was found, and that is what is
                        marked
           :unmatched   no pass found it, and nothing is marked
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
   Note: a quote no pass finds is :unmatched and nothing is marked for it. No
         pass matches approximately — `find-prefix` records why."
  [chapters highlights]
  (let [chapters (vec chapters)
        prepared (eligibility highlights (positions-by-title chapters))]
    (loop [pos 0
           pending prepared
           placed {}]
      (if (or (>= pos (count chapters)) (empty? pending))
        (mapv (fn [{:keys [slot quoted]}]
                (or (get placed slot)
                  {:index (:index quoted) :notes (:notes quoted)
                   :chapter-pos nil :start nil :end nil :outcome :unmatched}))
          prepared)
        (let [[still found] (place-in-chapter pos
                              (chapter-text (:html (nth chapters pos)))
                              pending)]
          (recur (inc pos) still (merge placed found)))))))

;; ── Tinting primitives ─────────────────────────────────────────────

(defn- node-runs
  "Split a normalized range into per-text-node raw offsets.

   Pre : `positions` is a `position-index` `:positions` vector. 0 <= start <
         end <= (count positions).
   Post: a vector of [TextNode from to] in document order, where `from` and
         `to` are offsets into that node's whole text and `to` is exclusive.
         One run per text node the range touches, covering it from its first
         character to its last.
   Inv : a run spans the GAPS between the characters it holds, so the whitespace
         normalization dropped is tinted with the words around it. Collapsing
         only ADJACENT characters instead gives one span per word, which paints
         as striped text rather than a highlighted passage.
   Inv : bridging a gap can never reach outside the range, because a text node
         appears once in document order — so the range's characters inside one
         node are one contiguous span of that node."
  [positions ^long start ^long end]
  (reduce (fn [runs i]
            (let [[node off] (nth positions i)
                  [prev-node prev-from] (peek runs)]
              (if (identical? node prev-node)
                (conj (pop runs) [node prev-from (inc (long off))])
                (conj runs [node off (inc (long off))]))))
    []
    (range start end)))

(defn- wrap-run!
  "Tint one run inside one text node. Mutates the node's document.

   Pre : `node` is attached to a document. 0 <= from < to <= length of the
         node's whole text.
   Post: the characters in [from, to) sit inside a span carrying
         `highlight-color`, and that span is returned. The node object keeps
         holding the text BEFORE `from`, which is why a caller working in
         descending order keeps every earlier offset valid."
  ^Element [^TextNode node ^long from ^long to]
  (let [target (if (pos? from) (.splitText node (int from)) node)]
    (when (< (- to from) (count (.getWholeText target)))
      (.splitText target (int (- to from))))
    (.wrap target (tint-span-html ""))
    (.parent target)))

(defn- mark-anchors!
  "Tint every range in one indexed chapter. Mutates its DOM.

   Pre : `index` is `position-index` output. `ranges` is a non-empty seq of
         [start end label] normalized indices, each range non-empty and within
         bounds. `label` is marker HTML or nil.
         Private because that precondition is not defensible against a caller
         holding raw placements: an :unmatched placement is [nil nil], which is
         not nil and would reach `node-runs`.
   Post: every range sits inside a `highlight-color` span, and a range with a
         label carries it immediately after that span's LAST run — which is
         where a footnote marker belongs. Returns `:root`.
   Inv : ranges are applied in DESCENDING start order, and runs within a range
         in descending offset order. Splitting a text node invalidates offsets
         for text AFTER the split point only, so descending order keeps every
         not-yet-applied offset correct. The caller does not pre-sort.
   Inv : inserting the label disturbs no offset. It becomes a SIBLING of the
         span, and `:positions` names text nodes by identity, never by their
         place among their siblings.
   Inv : `index` is STALE once this returns."
  [index ranges]
  (let [positions (:positions index)]
    (doseq [[s e label] (sort-by (comp - first) ranges)]
      ;; Reversed, so the first span built is the range's LAST run. That is the
      ;; one the label goes after.
      (let [spans (mapv (fn [[node from to]] (wrap-run! node from to))
                    (reverse (node-runs positions s e)))]
        (when (and label (seq spans))
          (.after ^Element (first spans) ^String label))))
    (:root index)))

;; ── Sections ───────────────────────────────────────────────────────
;;
;; A sync writes two sections, and finds them again by their heading TEXT. That
;; settles spec item 6.4. The alternative was a sentinel attribute, and it
;; cannot work here: Quill's clipboard drops every unknown class and `data-*`
;; attribute, so one user edit of the chapter would erase the sentinel and the
;; next sync would append a second section beside the one it could no longer
;; see. Heading text survives that round trip, because Quill's stock registry
;; models headings.
;;
;; The accepted cost is a rename. A user who retitles the heading gets a second
;; section on the next sync. That is visible immediately and one delete fixes
;; it, which is the mildest failure available under this design.

(def notes-heading
  "Heading opening a chapter's synced notes section (spec 6.2)."
  "USER NOTES")

(def unplaced-heading
  "Heading opening the list of highlights nothing could place.

   This settles spec item 7.2. The toast reports HOW MANY went unplaced, and a
   count alone is not actionable — the reader cannot tell which passage to look
   for. The other candidates were a server log, which no reader sees, and a
   report view, which the decision record rules out. A section needs no new view
   and is rebuilt on every sync by the same code that rebuilds the notes.

   WHICH topic carries it is the caller's decision, not this namespace's: the
   root for an EPUB, the first section for a PDF, because a PDF root's own
   content is never rendered (spec 10.4.1)."
  "UNPLACED HIGHLIGHTS")

(def ^:private sync-headings
  "Every heading a sync writes, as `chapter-key` comparison keys.

   ONE set, because `strip-sync-content` must erase all of them and a heading
   missing from here would survive into the next sync's match text. A second
   heading added to this namespace belongs here in the same commit."
  #{(chapter-key notes-heading) (chapter-key unplaced-heading)})

(def ^:private heading-tags #{"h1" "h2" "h3" "h4" "h5" "h6"})

(defn- escape-text
  "Pre : `s` is plain text from the export.
   Post: the same text with `&`, `<` and `>` escaped, so a quote holding a
         literal `<` becomes text rather than markup."
  [s]
  (-> (str s)
    (str/replace "&" "&amp;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")))

(defn- section-html
  "Pre : `heading` is one of this namespace's two headings. `lines` is a
         non-empty seq of plain-text lines.
   Post: an `<h3>` plus one `<p>` per line. `<h3>` because Quill's stock
         registry models it, which is what keeps the heading findable after a
         user edits the chapter."
  [heading lines]
  (apply str "<h3>" (escape-text heading) "</h3>"
    (map #(str "<p>" (escape-text %) "</p>") lines)))

(defn- sync-section-start
  "Where a topic's synced sections begin.

   Pre : `body` is a parsed body Element.
   Post: the index among `body`'s direct child elements of the EARLIEST heading
         in `sync-headings`, or nil when it holds none.
   Inv : earliest, not first-matching-one-heading. A topic can carry both
         sections, and removing the earlier one has to take the later one with
         it — see `remove-sync-sections!`.
   Note: direct children only. A sync appends its sections at that level, so a
         user who re-nests one gets a second section next time — the same
         accepted cost as renaming it."
  [^Element body]
  (first (keep-indexed (fn [i ^Element el]
                         (when (and (heading-tags (.tagName el))
                                 (sync-headings (chapter-key (.text el))))
                           i))
           (vec (.children body)))))

(defn- remove-sync-sections!
  "Pre : `body` is a parsed body Element.
   Post: every synced section is gone, along with anything between or after
         them. Mutates `body`.
   Inv : taking everything after the earliest sync heading is complete, because
         a sync always appends its sections LAST and never anything else.
   Inv : this is the ONLY removal in the sync's write path. `append-section`
         removes nothing, so a section appended after this ran cannot be taken
         back out by a later append — which is the defect that split the two
         (spec 10.2.3)."
  [^Element body]
  (when-let [at (sync-section-start body)]
    (doseq [^Element el (drop at (vec (.children body)))]
      (.remove el))))

(defn append-section
  "Append one synced section to a topic's content.

   Pre : `html` went through `strip-sync-content` THIS sync, so it carries no
         synced section. A caller that skips that owes a duplicate section.
   Pre : `heading` is one of this namespace's headings. `lines` is a seq of
         plain-text lines, possibly empty.
   Post: `html` with the section appended when `lines` is non-empty, and `html`
         returned UNCHANGED — not re-parsed — when it is empty. An empty `lines`
         therefore leaves no heading and no spurious diff, so a document that no
         longer needs the section does not keep an empty one and is not rewritten
         for nothing.
   Post: rebuilt in full every sync (spec 6.3), because the strip already took
         the previous one. A user's edits inside a section do not survive."
  [html heading lines]
  (if-not (seq lines)
    html
    (let [body (parse-body html)]
      (.append body (section-html heading lines))
      (.html body))))

(defn strip-sync-content
  "Remove everything a previous sync wrote into a topic.

   Pre : `html` is a topic's stored content, or nil.
   Post: every `highlight-color` span unwrapped, every `[n]` marker span
         removed, and every synced section dropped. nil and blank pass through
         unchanged.
   Inv : only spans carrying exactly `highlight-color` are touched. An extract
         highlight (`#44C2FF`) and every other inline colour survive. A tint the
         user applied by hand with the Quill popover's Orange does NOT — that is
         the cost of carrying no identity in the content.
   Note: this MUST run BEFORE placement, on every topic the sync will write.
         Both sections repeat the quotes' own words, so leaving one in gives a
         quote a second place to match. The `UNPLACED HIGHLIGHTS` list is the
         dangerous one: it lives on a topic the matcher READS when that topic is
         a PDF's first page, and its lines are quotes truncated at 300
         characters — exactly the shape `find-prefix` matches. An unplaced
         highlight would then place on its own copy and report success (spec
         10.2.5)."
  [html]
  (if (or (nil? html) (str/blank? html))
    html
    (let [body (parse-body html)]
      (doseq [^Element el (vec (.select body tint-selector))]
        (if (re-matches marker-pattern (.text el))
          (.remove el)
          (.unwrap el)))
      (remove-sync-sections! body)
      (.html body))))

(defn resolved-title-count
  "How many highlights name a section by its title.

   Pre : `sections` is a seq of {:title :html}. `highlights` is
         `neoreader-export/parse-highlights` output.
   Post: the number of highlights whose chapter label resolves to at least one
         section — the same test `eligibility` applies, so the two cannot
         disagree.
   Note: 0 means chapter scoping did nothing for this document, which is the
         permanent state of every PDF: `insert-page-stubs!` titles pages
         `Page N` and Boox emits the PDF outline's label. A caller reports
         differently in that case (spec 10.5.2).
   Note: this cannot be derived from the `:placed` count. A highlight whose title
         DID resolve but which the rescan or prefix pass took reports
         `:out-of-order` or `:partial`, not `:placed`."
  [sections highlights]
  (let [by-title (positions-by-title sections)]
    (count (filter #(contains? by-title (chapter-key (:chapter %))) highlights))))

;; ── Phase 2: the chapter rewrite ───────────────────────────────────

(defn- note-line
  "Pre : `notes` is a placement's `:notes` vector, possibly empty.
   Post: its notes as one line, or nil when it holds none."
  [notes]
  (let [joined (->> notes (map str/trim) (remove str/blank?) (str/join " "))]
    (when-not (str/blank? joined) joined)))

(defn- chapter-notes
  "Number the notes of one chapter's placements.

   Pre : `placements` all share one `:chapter-pos`, in any order.
   Post: {:labels {index → marker-html} :lines [\"[1] …\" …]}, numbered from 1 in
         READING order — the order the marks appear in the chapter, not the order
         the export lists them.
   Inv : one function yields both the markers and the section lines, so a marker
         can never name a line the section does not hold."
  [placements]
  (->> placements
    (filter #(some? (note-line (:notes %))))
    (sort-by :start)
    (map-indexed (fn [i p] [(inc i) p]))
    (reduce (fn [acc [n p]]
              (-> acc
                (assoc-in [:labels (:index p)] (marker-html n))
                (update :lines conj (str "[" n "] " (note-line (:notes p))))))
      {:labels {} :lines []})))

(defn- labeled-ranges
  "The tint ranges of one chapter, each carrying the markers that end inside it.

   Pre : `placements` all share one `:chapter-pos`. `labels` maps a highlight
         index to marker HTML.
   Post: a vector of [start end label] over MERGED ranges. `label` concatenates
         every marker whose placement ends inside that range, in reading order,
         or is nil.
   Note: a marker for a placement swallowed by an overlap lands at the merged
         range's end rather than at its own. Precision inside an overlap is not
         worth a nested span, and the markers still read in order."
  [placements labels]
  (let [merged (merge-ranges (map (juxt :start :end) placements))
        containing (fn [^long end]
                     (first (keep-indexed (fn [i [s e]]
                                            (when (and (<= (long s) end) (<= end (long e))) i))
                              merged)))
        by-range (reduce (fn [m p]
                           (if-let [label (get labels (:index p))]
                             (update m (containing (:end p)) (fnil conj []) [(:start p) label])
                             m))
                   {} placements)]
    (into []
      (map-indexed (fn [i [s e]]
                     [s e (when-let [ls (get by-range i)]
                            (apply str (map second (sort-by first ls))))]))
      merged)))

(defn mark-chapters
  "Rewrite the chapters a sync touched: tints, note markers, notes section.

   Pre : `chapters` is the same seq, in the same order, that `place-highlights`
         was given, and each `:html` has already been through
         `strip-sync-content`. `placements` is `place-highlights` output.
   Post: a map from chapter position to that chapter's rewritten HTML,
         holding an entry ONLY for a chapter with at least one placement. A
         chapter with none is absent, so the caller writes nothing for it —
         re-serializing an untouched chapter would still change its HTML,
         because a Jsoup parse and serialize can alter attribute quoting and
         entity escaping, and that write would bump the refresh channel and
         re-initialize Quill for no change.
   Post: a chapter whose placements carry notes ends with a `USER NOTES` section
         (spec 6.2), and each noted passage carries the matching `[n]`.
   Inv : one chapter's position index is live at a time, and it is discarded
         before the next chapter is parsed.
   Inv : an :unmatched placement contributes nothing, so no [nil nil] range can
         reach `mark-anchors!`. A highlight nothing placed has no chapter to hold
         its note either, which is why the root's `UNPLACED HIGHLIGHTS` section
         exists."
  [chapters placements]
  (let [chapters (vec chapters)
        by-chapter (group-by :chapter-pos (filter #(some? (:chapter-pos %)) placements))]
    (reduce-kv
      (fn [out pos ps]
        (let [{:keys [labels lines]} (chapter-notes ps)
              index (position-index (:html (nth chapters pos)))
              ^Element root (:root index)]
          (mark-anchors! index (labeled-ranges ps labels))
          (when (seq lines)
            (.append root (section-html notes-heading lines)))
          (assoc out pos (.html root))))
      {}
      by-chapter)))
