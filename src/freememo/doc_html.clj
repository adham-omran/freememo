(ns freememo.doc-html
  "Rewrite imported documentation markup into the constructs Quill can hold.

   **Quill's Delta is a flat op list, so it drops every tag it has no blot
   for.** Quill 2.0.3 registers no blot for `dl`, `dt`, `dd`, `figure` or
   `div`. `clipboard.convert` keeps the text of such an element and discards
   the element. A Sphinx page therefore renders as a wall of paragraphs with
   no term hierarchy, although `topics.content` still holds the whole tree.

   Worse, the loss is not read-only. `quill-field/editor-html` reads the
   FLATTENED DOM back out, and `editor_pane`'s auto-save writes it to the
   database, so one keystroke rewrites a whole page permanently.

   This namespace maps each construct onto a form Quill DOES hold, so the
   stored form and the rendered form agree and no edit can flatten anything:

     <dt>                  -> <p><strong>
     <dd>                  -> each block inside it gains `ql-indent-1`
     <kbd>                 -> <code>
     <div class=admonition> -> <blockquote>, title line bolded
     <figure>              -> <p>, any <figcaption> becomes <em>
     <div> (layout)        -> unwrapped, unless it is a Quill code block
     <cite> <q>            -> <em>
     <small>               -> unwrapped
     <caption>             -> <p><strong> above its table
     <th>                  -> <td> holding a <strong>
     <thead> <tfoot>       -> rows folded into <tbody>
     <col> <colgroup>      -> removed

   The last five exist for browser DOM, which the Chrome capture extension
   posts verbatim. See `plans/chrome-capture-extension.md`.

   **It MUST run before `clean-html`**, for the reason
   `sphinx-import/strip-headerlinks!` records: the sanitizer strips the class
   that identifies a construct, and afterwards nothing can recognize it.
   `div.admonition` is the clearest case — after sanitizing it is a bare
   `<div>`, indistinguishable from the 10391 layout wrappers SuperMemo emits.

   **No construct needs a wider allow-list.** Every target tag and the
   `ql-indent-1` class already survive `clean-html`, so
   `freememo.html-cleaner` is untouched by this namespace. That is the same
   invariant `supermemo-import/supermemo-html->quill-html` states, and this
   namespace is the second instance of that pattern.

   `hr` is the one construct this seam cannot reach. Quill drops it whatever
   the database holds, so it needs a blot instead — see the `divider`
   registration in `freememo.quill-field`.

   See `plans/sphinx-quill-fidelity.md`."
  (:require [clojure.string :as str])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Element TextNode]))

;; ---------------------------------------------------------------------------
;; Vocabulary
;; ---------------------------------------------------------------------------

(def ^:private body-indent-class
  "The indent a definition body carries. Quill's `indent` format spans 1-8 and
   `html-cleaner/quill-class-allow-list` admits exactly `ql-indent-1` through
   `ql-indent-8`, so this token needs no allow-list change.

   One level for every depth, deliberately. The corpus nests `dl` inside `dd`
   two deep, and a per-depth scheme reproduces Sphinx exactly, but it must
   INCREMENT an existing indent rather than add a second class — the naive form
   emits `class=\"ql-indent-2 ql-indent-1\"` and leaves which one wins up to
   Quill's attributor order. Flat costs one of three indent steps on the 16 of
   62 term pairs that sit at depth 2. A depth-2 `dt` still nests correctly: it
   inherits its parent body's indent, because `flatten-definition-lists!`
   rewrites the deepest list first. See `plans/sphinx-quill-fidelity.md`."
  "ql-indent-1")

(def ^:private block-tags
  "Tags that must never end up inside a `<p>`. `collect-definition-blocks`
   groups everything else into synthetic paragraphs, so this set is what keeps
   that grouping from nesting a list or a table in a paragraph."
  #{"p" "div" "ul" "ol" "li" "dl" "dt" "dd" "blockquote" "pre" "table"
    "thead" "tbody" "tr" "td" "th" "h1" "h2" "h3" "h4" "h5" "h6"
    "hr" "figure" "figcaption" "section" "article" "aside" "main" "nav"})

(def ^:private preformatted-selector
  "Where whitespace carries meaning and `squeeze-whitespace!` must not reach.
   Mirrors the exclusion `freememo.math`'s editor-direction fns apply."
  "pre, code, .ql-code-block-container")

;; ---------------------------------------------------------------------------
;; Steps
;; ---------------------------------------------------------------------------

(defn- bold-in-place!
  "Wrap `el`'s children in a `<strong>`, keeping `el` itself.
   Pre : `el` is an Element.
   Post: `el` has exactly one child, a `<strong>` holding its former children
         in order.
   Inv : no text is added or removed."
  [^Element el]
  (let [strong (Element. "strong")]
    (doseq [node (vec (.childNodes el))]
      (.appendChild strong node))
    (.appendChild el strong)))

(def ^:private boxed-directive-selector
  "Sphinx wrappers that render as a boxed aside AND whose label this ns can
   bold. Two shapes, two label mechanisms, both handled:

     admonition directives — note, warning, tip, seealso, custom title — all
       carry `admonition` and label themselves with `p.admonition-title`.
     version and deprecation directives carry NO `admonition` class; they emit
       a bare `div.versionadded` and label themselves inline with
       `span.versionmodified`.

   `topic` and `sidebar` are deliberately absent. They are boxes too, but they
   label themselves with `p.topic-title` / `p.sidebar-title`, which nothing here
   reads — admitting them would produce a quote whose heading is indistinguishable
   from its body. Add them WITH their title selector or not at all."
  (str "div.admonition, div.versionadded, div.versionchanged, "
    "div.versionremoved, div.deprecated"))

(defn- admonitions->blockquotes!
  "Turn every Sphinx boxed aside into a blockquote with a bolded label line.
   Pre : `root` still carries its source classes, i.e. `clean-html` has NOT run.
   Post: no element matching `boxed-directive-selector` remains. Returns how
         many were rewritten.
   Inv : a `div` carrying none of those classes is untouched — SuperMemo's
         10391 bare layout wrappers must survive, and after sanitizing nothing
         could tell them apart from a note.
   Note: order does not matter here. Retagging a box leaves its children in
         place, so a nested box is rewritten correctly whenever it is reached —
         unlike `flatten-definition-lists!`, where deepest-first is load-bearing."
  [^Element root]
  (let [els (vec (.select root boxed-directive-selector))]
    (doseq [^Element el els]
      (doseq [^Element title (vec (.select el "p.admonition-title"))]
        (bold-in-place! title))
      ;; The version directives label themselves inline instead of with a
      ;; title paragraph, so bold that span to give the box the same shape.
      (doseq [^Element label (vec (.select el "span.versionmodified"))]
        (bold-in-place! label))
      (.tagName el "blockquote"))
    (count els)))

(defn- keyboard-keys->code!
  "Turn every `<kbd>` into inline `<code>`.
   Pre : `root` has not been sanitized — `clean-html`'s safelist has no `kbd`,
         so afterwards the key boundaries are gone with no trace.
   Post: no `kbd` remains. Returns how many were rewritten.
   Inv : `code` is in the safelist, in Quill's registry, and already styled by
         `.ql-snow .ql-editor :not(pre) > code` in index.css."
  [^Element root]
  (let [els (vec (.select root "kbd"))]
    (doseq [^Element el els]
      (.tagName el "code"))
    (count els)))

;; Quill's `indent` is a per-LINE block format, so the question for every block
;; of a definition body is "which of my lines take the indent?". Three answers,
;; and they are exhaustive:
(def ^:private list-tags
  "Blocks whose LINES are their children. The indent goes on each `li`, never on
   the list — Quill drops an indent on a `ul` because a `ul` is not a line."
  #{"ul" "ol"})

(def ^:private unindentable-tags
  "Blocks that are not lines and hold no lines, so no indent can represent them.
   A rule spans its column whatever its offset, and a table's geometry is its
   own. `clean-html` independently proves the point by admitting `class` on
   neither tag, so an indent written here is stripped and the caller would never
   learn it had asked for something impossible."
  #{"hr" "table"})

(defn- indent-definition-block!
  "Give one block of a definition body its indent.
   Pre : `el` is a block-level Element, and `el` will still be `el` when
         `clean-html` runs — a step that replaces or unwraps `el` afterwards
         discards this class with it, so every such step MUST run first.
   Post: `el` carries `body-indent-class`, or each of its items does when `el`
         is a list, or nothing changes when `el` cannot carry an indent.
   Inv : no class is written that `clean-html` will strip."
  [^Element el]
  (let [tag (.tagName el)]
    (cond
      (unindentable-tags tag) nil
      (list-tags tag) (doseq [^Element item (vec (.children el))]
                        (.addClass item body-indent-class))
      :else (.addClass el body-indent-class))))

(defn- collect-definition-blocks
  "The blocks a `<dd>` contributes, in source order.
   Pre : `dd` is a `<dd>` Element.
   Post: a vector of block-level Elements. Loose text and inline elements are
         grouped into synthetic `<p>`s, so no block ever lands inside a `<p>`.
         The returned Elements are reparented out of `dd`.
   Inv : every non-blank child node of `dd` reaches exactly one returned block."
  [^Element dd]
  (let [!open-paragraph (volatile! nil)
        close! (fn [acc]
                 (if-let [p @!open-paragraph]
                   (do (vreset! !open-paragraph nil) (conj! acc p))
                   acc))]
    ;; `vec` is the snapshot and is NOT redundant: Jsoup's `childNodes` returns
    ;; an unmodifiable VIEW over the live list, and this loop reparents the very
    ;; nodes it walks. `(seq (.childNodes dd))` would iterate a list that
    ;; shrinks underneath it.
    ;; `seq` rather than `(first nodes)` so a falsy child cannot end the loop
    ;; early and drop every node after it — Jsoup produces none, and this makes
    ;; that structural rather than a property of the input.
    (loop [nodes (seq (vec (.childNodes dd))) acc (transient [])]
      (if-let [[node & more] nodes]
        (recur more
          (cond
            (and (instance? Element node) (block-tags (.tagName ^Element node)))
            (conj! (close! acc) node)

            (and (instance? TextNode node) (.isBlank ^TextNode node))
            acc

            :else
            (do (when-not @!open-paragraph (vreset! !open-paragraph (Element. "p")))
                (.appendChild ^Element @!open-paragraph node)
                acc)))
        (persistent! (close! acc))))))

(defn- dl-nesting-depth
  "How many `dl` ancestors `el` has. Read before any rewrite, so it describes
   the source tree rather than a half-rewritten one."
  [^Element el]
  (count (filter #(= "dl" (.tagName ^Element %)) (.parents el))))

(defn- flatten-one-definition-list!
  "Replace one `dl` with the flat blocks it becomes.
   Pre : every `dl` nested inside `dl` has already been flattened, so `dl`'s
         own children are `dt`, `dd` and nothing else that nests.
   Post: `dl` is removed. Its terms and bodies sit in its former position, in
         source order. Returns {:dt n :dd n}.
   Inv : a child that is neither `dt` nor `dd` is moved across unchanged — a
         theme may put anything there and losing it silently is not acceptable."
  [^Element dl]
  (let [!terms (volatile! 0)
        !bodies (volatile! 0)]
    (doseq [^Element child (vec (.children dl))]
      (case (.tagName child)
        "dt" (let [p (Element. "p")]
               (doseq [node (vec (.childNodes child))]
                 (.appendChild p node))
               (bold-in-place! p)
               (.before dl p)
               (vswap! !terms inc))
        "dd" (do (doseq [^Element block (collect-definition-blocks child)]
                   (indent-definition-block! block)
                   (.before dl block))
                 (vswap! !bodies inc))
        (.before dl child)))
    (.remove dl)
    {:dt @!terms :dd @!bodies}))

(defn- flatten-definition-lists!
  "Flatten every definition list, deepest first.
   Pre : `root` is a parsed document body.
   Post: no `dl`, `dt` or `dd` remains. Returns {:dt n :dd n} summed.
   Inv : deepest first is load-bearing. It means an inner list is already flat
         when its enclosing `dd` is read, so the terms it produced are ordinary
         blocks of that body and pick up the body's indent — which is what
         keeps a nested term from reading as a sibling of its grandparent."
  [^Element root]
  (->> (.select root "dl")
    vec
    (sort-by dl-nesting-depth >)
    (map flatten-one-definition-list!)
    (reduce (partial merge-with +) {:dt 0 :dd 0})))

(defn- flatten-figures!
  "Replace every `<figure>` with the block it should have been, caption kept.
   Pre : `root` is a parsed document body. MUST run before
         `flatten-definition-lists!` — see the ordering note in
         `rewrite-for-quill!`.
   Post: no `figure` or `figcaption` remains. Returns how many figures changed.
   Inv : no block element ends up inside a `<p>`. A figure holding only inline
         content is RETAGGED, which keeps the caption in the same paragraph as
         the image. One holding a block — a `table` or a list, which Sphinx's
         figure directive permits — is UNWRAPPED instead, because retagging
         would emit `<p><table>…</table></p>` and every HTML parser splits the
         paragraph back apart at that point.
   Note: Quill unwraps a figure on load regardless, so this only moves the
         change into the stored form, where it narrows the first-edit diff."
  [^Element root]
  (let [els (vec (.select root "figure"))]
    (doseq [^Element el els]
      ;; Caption first: `figcaption` is itself block-level, so converting it
      ;; decides the retag-or-unwrap question below rather than pre-empting it.
      (doseq [^Element caption (vec (.select el "figcaption"))]
        (.tagName caption "em"))
      (if (some #(block-tags (.tagName ^Element %)) (.children el))
        (.unwrap el)
        (.tagName el "p")))
    (count els)))

(def ^:private code-block-container-selector
  "Where a `div` is load-bearing and MUST survive `unwrap-layout-divs!`.
   Mirrors `html-cleaner/inside-code-block-container?`, which is the only
   other place that treats a `div` as meaningful rather than as layout."
  ".ql-code-block-container")

(defn- unwrap-layout-divs!
  "Unwrap every `div` that is not part of a Quill code block.

   Pre : `root` is a parsed document body. `admonitions->blockquotes!` MUST
         have run — it is the one step that reads a `div`'s class, and after
         this step that div is gone.
   Post: every remaining `div` is, or sits inside, a `.ql-code-block-container`.
         Returns how many were unwrapped.
   Inv : no text is added or removed. Children keep their order and their
         position relative to the div's former siblings.

   Quill 2.0.3 registers no `div` blot outside the code-block pair, so a layout
   div is dropped on load whatever the database holds. Unwrapping here moves
   that loss into the stored form, where the first edit cannot enlarge it.

   A browser DOM is mostly these. One measured Wikipedia article carried 179
   divs, all of which `clean-html` keeps and Quill then discards."
  [^Element root]
  (let [els (->> (.select root "div")
              vec
              (remove (fn [^Element el] (some? (.closest el code-block-container-selector)))))]
    ;; Deepest first: unwrapping an ancestor reparents a descendant, and Jsoup
    ;; keeps the descendant's own handle valid, but the depth order makes the
    ;; traversal independent of that guarantee.
    (doseq [^Element el (reverse els)]
      (.unwrap el))
    (count els)))

(def ^:private emphasis-tags
  "Inline tags Quill has no blot for whose meaning survives as emphasis.
   `cite` appears on real articles — one measured Wikipedia page carried 38."
  #{"cite" "q"})

(defn- inline-semantics->em!
  "Retag every `cite` and `q` as `em`.

   Pre : `root` is a parsed document body.
   Post: no `cite` or `q` remains. Returns how many were retagged.
   Inv : text is unchanged. `em` is in `clean-html`'s allow-list and has an
         `italic` blot, so nothing downstream drops the result.

   Both tags survive `clean-html` — they are in Jsoup's relaxed safelist — and
   both are absent from Quill's registry, so without this the emphasis is lost
   on the first edit while the text stays."
  [^Element root]
  (let [els (vec (.select root (str/join ", " emphasis-tags)))]
    (doseq [^Element el els]
      (.tagName el "em"))
    (count els)))

(defn- drop-presentational!
  "Unwrap `small` and remove `col` and `colgroup`.

   Pre : `root` is a parsed document body.
   Post: no `small`, `col` or `colgroup` remains. Returns how many elements
         were touched.
   Inv : `small` keeps its text, because it wraps content. `col` and
         `colgroup` carry none, so removing them loses nothing.

   All three pass `clean-html` through Jsoup's relaxed safelist and all three
   are absent from Quill's registry."
  [^Element root]
  (let [smalls (vec (.select root "small"))
        cols (vec (.select root "col, colgroup"))]
    (doseq [^Element el smalls]
      (.unwrap el))
    (doseq [^Element el cols]
      (.remove el))
    (+ (count smalls) (count cols))))

(defn- table-captions->paragraphs!
  "Lift every `<caption>` out of its table as a bolded paragraph above it.

   Pre : `root` is a parsed document body.
   Post: no `caption` remains. Each former caption is a `<p><strong>` placed
         immediately before the table that held it. Returns how many moved.
   Inv : the caption's text and inline markup are preserved in order.

   Quill's table blots are `table`, `table-row`, `table-body` and
   `table-container` — no caption among them — so a caption's text would
   otherwise reappear as a bare run inside the table on the first edit."
  [^Element root]
  (let [els (vec (.select root "caption"))]
    (doseq [^Element caption els]
      (when-let [^Element table (.parent caption)]
        (let [p (Element. "p")
              strong (Element. "strong")]
          (doseq [node (vec (.childNodes caption))]
            (.appendChild strong node))
          (.appendChild p strong)
          (.before table p)
          (.remove caption))))
    (count els)))

(defn- normalize-table-sections!
  "Fold `thead` and `tfoot` into `tbody`, and retag `th` as a bolded `td`.

   Pre : `root` is a parsed document body.
   Post: no `th`, `thead` or `tfoot` remains. Every `tr` sits in a `tbody`, in
         the order a browser renders them: head rows, body rows, foot rows.
         Returns how many elements changed.
   Inv : no row and no cell is lost, and a header cell keeps its emphasis as a
         `strong` that `clean-html` and Quill both hold.

   Measured against the vendored Quill 2.0.3 bundle: the table blots are
   `table` (tagName TD), `table-row` (TR), `table-body` (TBODY) and
   `table-container` (TABLE). No blot carries TH, THEAD or TFOOT, so all three
   are dropped on load while their text stays."
  [^Element root]
  (let [headers (vec (.select root "th"))
        sections (vec (.select root "thead, tfoot"))]
    (doseq [^Element th headers]
      (bold-in-place! th)
      (.tagName th "td"))
    (doseq [^Element table (vec (.select root "table"))]
      (let [head-rows (vec (.select table "thead > tr"))
            foot-rows (vec (.select table "tfoot > tr"))
            body (or (.first (.select table "tbody"))
                     (let [tb (Element. "tbody")]
                       (.appendChild table tb)
                       tb))]
        (doseq [^Element tr (reverse head-rows)]
          (.prependChild body tr))
        (doseq [^Element tr foot-rows]
          (.appendChild body tr))))
    (doseq [^Element el sections]
      (.remove el))
    (+ (count headers) (count sections))))

(defn- squeeze-whitespace!
  "Drop whitespace-only text nodes and collapse the rest.

   Pre : `root` is a parsed document body.
   Post: no text node outside `preformatted-selector` is blank. Every other
         such text node has its whitespace runs collapsed to one space, and is
         trimmed at whichever end touches its parent's boundary. Returns how
         many blank nodes were dropped.
   Inv : content inside `pre`, `code` or `.ql-code-block-container` is
         untouched — whitespace is significant there, which is exactly why
         `clean-html` turns Jsoup pretty-print off.

   Two reasons this is not cosmetic. `.ql-editor` sets `white-space: pre-wrap`,
   so a Sphinx build's own source indentation renders as real blank space in any
   view that does not go through Quill — measured at 30180px against 9292px for
   one page. And a `<dt>` written across lines yields
   `<strong> Splash Screen </strong>`, whose flanking spaces show inside the
   bold run. Dropping blank nodes alone does not fix the second one, because
   those spaces live inside the term's own text node."
  [^Element root]
  (let [!dropped (volatile! 0)]
    (doseq [^Element el (vec (.select root "*"))
            :when (nil? (.closest el preformatted-selector))
            ^TextNode t (vec (.textNodes el))]
      (if (.isBlank t)
        (do (.remove t) (vswap! !dropped inc))
        (let [raw (.getWholeText t)
              siblings (count (.childNodes el))
              index (.siblingIndex t)
              collapsed (str/replace raw #"\s+" " ")
              trimmed (cond-> collapsed
                        (zero? index) str/triml
                        (= index (dec siblings)) str/trimr)]
          (when (not= raw trimmed)
            (.text t trimmed)))))
    @!dropped))

;; ---------------------------------------------------------------------------
;; Entry points
;; ---------------------------------------------------------------------------

(defn rewrite-for-quill!
  "Rewrite `root` in place into constructs Quill's Delta can hold.

   Pre : `root` is a parsed element holding imported documentation HTML, with
         its SOURCE classes intact — `clean-html` must not have run yet.
   Post: `root` holds no `dl`, `dt`, `dd`, `kbd`, `div.admonition`, `figure` or
         `figcaption`. Returns a count per step, for the caller's import report.
   Inv : every rewrite lands on a tag and class `clean-html` already keeps, so
         no caller needs a wider allow-list.

   Step order is load-bearing at three points.

   Keys become `code` BEFORE terms are flattened, so a term's bold run wraps
   them.

   Figures are flattened BEFORE definition lists, because
   `flatten-definition-lists!` writes the body indent onto the blocks it finds
   and `flatten-figures!` may UNWRAP the element it wrote it on — which
   discards the class with the element and drops a figure's body to the left
   margin. Running it first means the indent only ever lands on a block that
   survives to `clean-html`, which is `indent-definition-block!`'s stated
   precondition.

   Whitespace is squeezed LAST, so no earlier step can put blank nodes back.

   `:browser-dom?` adds the five steps a captured page needs and no importer
   wants. It is opt-in because two of them contradict invariants the other
   corpora rely on: SuperMemo's 10391 bare layout divs and Sphinx's
   `div.sidebar` are both asserted to SURVIVE, and `unwrap-layout-divs!`
   removes them. A capture has no such construct and no such expectation.

   When enabled, layout divs are unwrapped AFTER admonitions, because that step
   is the only one that reads a div's class, and BEFORE definition lists and
   figures, because `indent-definition-block!` requires that nothing later
   unwraps the element it wrote the indent onto."
  ([^Element root] (rewrite-for-quill! root {}))
  ([^Element root {:keys [browser-dom?]}]
   (let [admonitions (admonitions->blockquotes! root)
         keys-rewritten (keyboard-keys->code! root)
         browser (when browser-dom?
                   {:captions-lifted (table-captions->paragraphs! root)
                    :table-sections-normalized (normalize-table-sections! root)
                    :inline-semantics-to-em (inline-semantics->em! root)
                    :presentational-dropped (drop-presentational! root)
                    :layout-divs-unwrapped (unwrap-layout-divs! root)})
         figures (flatten-figures! root)
         {:keys [dt dd]} (flatten-definition-lists! root)
         blank-dropped (squeeze-whitespace! root)]
     (merge
       {:admonitions-to-blockquotes admonitions
        :keys-to-code keys-rewritten
        :terms-flattened dt
        :definition-bodies-flattened dd
        :figures-flattened figures
        :whitespace-nodes-dropped blank-dropped}
       browser))))

(defn doc-html->quill-html
  "`rewrite-for-quill!` for a caller holding a string rather than an Element.

   Pre : `html` is imported documentation HTML, or nil.
   Post: nil iff `html` is nil. Otherwise the rewritten HTML. The counts are
         not returned — a caller that needs them holds an Element and should
         call `rewrite-for-quill!` directly.
   Inv : does NOT sanitize. The caller still owes `clean-html`, which is what
         keeps this namespace out of the two-sanitize trap."
  [html]
  (when html
    (let [doc (Jsoup/parseBodyFragment html)]
      (.prettyPrint (.outputSettings doc) false)
      (rewrite-for-quill! (.body doc))
      (.html (.body doc)))))

(defn browser-dom->quill-html
  "`doc-html->quill-html` for a page serialized out of a live browser.

   Pre : `html` is browser DOM, or nil. `clean-html` has NOT run — the layout
         step reads a div's class to spare Quill code blocks.
   Post: nil iff `html` is nil. Otherwise the rewritten HTML, holding no
         layout `div`, `cite`, `q`, `small`, `caption`, `col` or `colgroup`.
   Inv : does NOT sanitize. The caller still owes `clean-html`.

   The importers keep calling `doc-html->quill-html`, whose output this does
   not change. See the `:browser-dom?` note on `rewrite-for-quill!`."
  [html]
  (when html
    (let [doc (Jsoup/parseBodyFragment html)]
      (.prettyPrint (.outputSettings doc) false)
      (rewrite-for-quill! (.body doc) {:browser-dom? true})
      (.html (.body doc)))))

(defn drop-bare-spans
  "Unwrap every `<span>` left carrying no attribute.

   Pre : `html` has ALREADY been sanitized. Before `clean-html` runs, a Sphinx
         `<span class=\"std std-ref\">` still has its class and would be kept,
         so calling this earlier is a no-op on the spans it exists for.
   Post: no attribute-less `span` remains. Text and every other element are
         unchanged. nil iff `html` is nil.
   Inv : reads only whether attributes remain, never WHICH classes survive, so
         `html-cleaner`'s allow-list stays the single place that decides that.

   Quill discards such a span on load regardless. Removing it here moves the
   change into the stored form, which shrinks the one-time normalization the
   first edit of an imported page performs."
  [html]
  (when html
    (let [doc (Jsoup/parseBodyFragment html)]
      (.prettyPrint (.outputSettings doc) false)
      (doseq [^Element el (vec (.select doc "span"))]
        (when (.isEmpty (.attributes el))
          (.unwrap el)))
      (.html (.body doc)))))
