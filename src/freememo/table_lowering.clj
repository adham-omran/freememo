(ns freememo.table-lowering
  "Rewrite imported tables into the shape Quill 2.0.3 can hold on load.

   `freememo.doc-html` owns every other imported construct. Tables live here
   because their failures come from five facts about Quill that no other
   construct touches, and because the grid arithmetic needs its own test file.

   **Quill numbers a table's rows by their index WITHIN THAT TABLE.** The
   clipboard matcher is

     [\"tr\", … Array.from(table.querySelectorAll(\"tr\")).indexOf(tr)+1 …]

   so two tables both emit the sequence 1, 2, 3.

   **`TableContainer` and `TableBody` never override `checkMerge`**, and the
   inherited test is `next != null && next.blotName === this.blotName`. Two
   adjacent `TABLE` blots therefore merge with no further condition, and inside
   the merged body row 1 of one table meets row 1 of the other with an equal id,
   so `TableRow.checkMerge` merges those too. One Wikipedia article rendered its
   maintenance banner and its infobox as a single grid for this reason.

   **Adjacency here means Delta adjacency, not DOM sibling adjacency.** Only two
   whitespace-filled empty `<span>` elements separated that article's two
   tables. Both are inline, so neither produces a line in the Delta and the two
   tables become neighbors in the blot tree. A `nextElementSibling` test reports
   no adjacent pair on that article, which is wrong. `line-producing?` is the
   test that is right, and it is public so the suite asserts against THIS
   definition rather than a copy of it.

   **A cell can hold no block child and no line break.** `TableCell` extends
   `Block`, whose `allowedChildren` holds no list container, and Parchment's
   `enforceAllowedChildren` responds to a block child by splitting the row and
   unwrapping the cell — which is how a `<ul>` inside a cell escaped its table
   and rendered as a bullet below it. Separately, the `tr` matcher applies the
   `table` format to EVERY insert op in the row delta, so any newline inside a
   cell becomes a cell boundary.

   **`colspan` and `rowspan` have no Quill representation at all.** Cell merge
   and split are out of scope for the editor (`plans/quill-v2-upgrade-with-tables.md`),
   so a spanning cell is expanded into filler cells instead.

   **It MUST run before `clean-html`**, for the reason
   `sphinx-import/strip-headerlinks!` records: the sanitizer strips the class
   and the `role` that identify a layout table, and it strips the `display:none`
   that identifies a hidden row. Afterwards nothing can recognize either.

   **No step needs a wider allow-list.** Every target tag already survives
   `clean-html`, which is the same invariant `freememo.doc-html` states.

   See `plans/wiki-table-lowering.md`."
  (:require [clojure.string :as str])
  (:import [org.jsoup.nodes Element Node TextNode]))

;; ---------------------------------------------------------------------------
;; Vocabulary
;; ---------------------------------------------------------------------------

(def presentation-selector
  "Layout tables any HTML source may emit. A table marked `presentation` claims
   it carries no data relationship, which is exactly the claim that lets this
   namespace discard its grid and keep its content.

   Wikipedia's maintenance banners also carry `role=presentation`, so this rule
   already matches every one of them — measured across four articles,
   `table.ambox:not([role=presentation]), table.metadata:not([role=presentation])`
   is 0. `freememo.wikipedia` still runs its own class-based pass, and its
   docstring states what that buys."
  "table[role=presentation]")

(def ^:private cell-selector "td, th")

(def ^:private cell-tags #{"td" "th"})

(def ^:private block-tags
  "Tags that occupy their own line. `line-producing?` reads this to decide
   whether two tables are Delta-adjacent, and `inline-cell-blocks!` reads it to
   decide what may not stay inside a cell."
  #{"p" "div" "ul" "ol" "li" "dl" "dt" "dd" "blockquote" "pre"
    "table" "thead" "tbody" "tr" "td" "th"
    "h1" "h2" "h3" "h4" "h5" "h6"
    "hr" "figure" "figcaption" "section" "article" "aside" "main" "nav"})

(def ^:private cell-block-tags
  "What `inline-cell-blocks!` unwraps. `table` and its structural tags are
   absent because `unnest-tables!` owns a table inside a cell, and unwrapping
   one here would spill its `tr` and `td` into the outer row."
  (apply disj block-tags "table" "thead" "tbody" "tr" cell-tags))

(def ^:private cell-block-selector (str/join ", " cell-block-tags))

(def ^:private hidden-style-re
  "Matches a `display:none` declaration in a `style` attribute. Wikipedia hides
   an infobox row this way, and `clean-html` keeps only `color` and
   `background-color`, so after sanitizing the row is visible and empty.

   This is the GENERIC signal, and it is the only one this namespace reads. A
   source that hides a row by class name alone — Wikipedia also marks these
   `infobox-hiddenrow`, though it always sets the style too — must drop it in
   its own preprocess step. A source-specific class here would put that
   vocabulary in the shared layer, which is the cost `presentation-selector`
   exists to avoid."
  #"(?i)display\s*:\s*none")

(def inline-block-separator
  "What joins two former block siblings inside one cell. A cell can hold no
   newline, so the separator has to be text. The constructs reaching this step
   are lists, and a semicolon separates list items that can themselves hold
   commas."
  "; ")

(def ^:private max-colspan
  "The largest `colspan` worth expanding into real cells.

   A larger value is not describing a grid a reader can use, and expanding it
   would manufacture more nodes than the document has content — measured,
   `colspan=\"5000\"` produced 5000 cells before this bound existed. Such a value
   is treated as 1, which costs nothing: `rectangularize-rows!` pads every row to
   the width the REAL rows establish, so the table comes out rectangular either
   way."
  100)

;; ---------------------------------------------------------------------------
;; Predicates
;; ---------------------------------------------------------------------------

(defn- element-tag
  "`n`'s tag name, or nil when `n` is not an Element."
  [^Node n]
  (when (instance? Element n) (.tagName ^Element n)))

(defn- block-element?
  [^Node n]
  (contains? block-tags (element-tag n)))

(defn- cell-element?
  [^Node n]
  (contains? cell-tags (element-tag n)))

(defn- table-element?
  [^Node n]
  (= "table" (element-tag n)))

(defn content-bearing?
  "True when `n` puts something a reader can see into its parent.

   Pre : `n` is a Jsoup node.
   Post: true for a non-blank text node, an `img`, and any element holding
         either. False for a blank text node and for an empty inline element.
   Inv : an element with no text but with an image IS content — an infobox logo
         row is exactly that shape, and treating it as empty deletes the logo.

   Public because the suite asserts the separator rules against this definition."
  [^Node n]
  (cond
    (instance? TextNode n) (not (.isBlank ^TextNode n))
    (instance? Element n) (let [^Element el n]
                            (or (= "img" (.tagName el))
                                (some? (.selectFirst el "img"))
                                (not (str/blank? (.text el)))))
    :else false))

(defn line-producing?
  "True when `n` occupies a line of its own in Quill's document.

   Pre : `n` is a Jsoup node.
   Post: true for a block element, a `br`, an `hr`, an `img`, and any node
         holding non-blank text. False for a blank text node and for an empty
         inline element.
   Inv : a false answer means the node cannot separate two tables in the blot
         tree, so `delta-adjacent-followers` may look past it.

   Erring toward true costs a blank line that nobody asked for. Erring toward
   false leaves the merge in place. The set below is the measured case — empty
   spans holding category links and templatestyles — plus the three inline
   embeds that do take a line.

   Public so `test/freememo/table_lowering_test.clj` asserts against this
   definition. A copy of it in the suite could not fail for a bug in this one,
   and the two had already drifted when they were separate."
  [^Node n]
  (cond
    (instance? TextNode n) (not (.isBlank ^TextNode n))
    (instance? Element n) (let [^Element el n
                                tag (.tagName el)]
                            (or (contains? block-tags tag)
                                (contains? #{"br" "hr" "img"} tag)
                                (some? (.selectFirst el "br, hr, img"))
                                (not (str/blank? (.text el)))))
    :else false))

(defn- separator-text?
  "True when `n` is a text node holding exactly the separator."
  [^Node n]
  (and (instance? TextNode n)
       (= inline-block-separator (.getWholeText ^TextNode n))))

(defn- leaf-content?
  "True when `n` is a LEAF that renders something — a non-blank text node or an
   `img`.

   `content-bearing?` answers the same question for a subtree, which is what a
   sibling test wants. This one exists for a document-order test, where an
   ancestor of the node under test would otherwise answer true for holding that
   node's own siblings. Restricting to leaves removes the ancestor from the
   question entirely."
  [^Node n]
  (or (and (instance? TextNode n) (not (.isBlank ^TextNode n)))
      (= "img" (element-tag n))))

(defn- descendants-in-document-order
  "`el`'s descendant nodes, pre-order, excluding `el` itself."
  [^Element el]
  (rest (tree-seq #(instance? Element %) #(.childNodes ^Node %) el)))

;; ---------------------------------------------------------------------------
;; Structure readers
;; ---------------------------------------------------------------------------

(defn- row-cells
  "The cells `tr` owns, as a vector.

   Pre : `tr` is a `tr` element.
   Post: every element is a direct child of `tr` whose tag is `td` or `th`.
   Inv : a cell belonging to a table nested inside this row is NEVER returned.

   A `.select` scan would return those too, and counting them would widen the
   outer row by the inner table's cells. `unnest-tables!` removes that case, but
   the width arithmetic must not depend on another step having run."
  [^Element tr]
  (vec (filter cell-element? (.children tr))))

(defn- nearest-table-ancestor
  [^Element el]
  (first (filter table-element? (.parents el))))

(defn- table-rows
  "The rows `table` owns, as a vector.

   Pre : `table` is a `table` element.
   Post: every element is a `tr` whose nearest table ancestor is `table`.
   Inv : a row belonging to a nested table is NEVER returned, for the same
         reason `row-cells` excludes a nested table's cells — the width
         arithmetic must not depend on `unnest-tables!` having run."
  [^Element table]
  (vec (filter (fn [^Element tr] (identical? table (nearest-table-ancestor tr)))
         (.select table "tr"))))

(defn delta-adjacent-followers
  "Every table that FOLLOWS another with nothing line-producing between them.

   Pre : `root` is a parsed element.
   Post: each returned element is a `table` reachable from an earlier `table` by
         walking `nextSibling` past nodes for which `line-producing?` is false.
   Inv : a table appears at most once, because only its immediate predecessor
         can reach it.

   Public because `separate-delta-adjacent-tables!` fixes what this reports and
   the suite asserts that nothing remains. One definition, two readers."
  [^Element root]
  (vec (keep (fn [^Element t]
               (loop [n (.nextSibling t)]
                 (cond
                   (nil? n) nil
                   (table-element? n) n
                   (line-producing? n) nil
                   :else (recur (.nextSibling n)))))
         (.select root "table"))))

;; ---------------------------------------------------------------------------
;; Steps
;; ---------------------------------------------------------------------------

(defn- unnest-tables!
  "Lift every nested table out to follow its outermost table ancestor.

   Pre : `root` is a parsed document body.
   Post: `root` holds no `table` inside another `table`. Returns how many moved.
   Inv : every grid keeps its own rows and cells, and the relative order of the
         lifted tables is unchanged.

   Quill has no representation for a nested table. `TableContainer`'s only
   allowed child is `TableBody`, so an inner table's rows are pulled into the
   outer row and both grids are destroyed. A sibling keeps both.

   ONE pass always suffices, so the postcondition is unconditional. `table
   table` selects every table having a table ancestor, and each one is moved to
   directly after its OUTERMOST such ancestor, which sits at depth 0 — so after
   the pass every formerly nested table is at depth 0. Recomputing the ancestor
   per element is what makes that hold when an ancestor moved earlier in the
   same pass. Measured: twelve levels of nesting resolve in one pass. The loop
   and its bound are a backstop against a Jsoup surprise, not the argument."
  [^Element root]
  (loop [moved 0
         pass 0]
    (let [nested (vec (.select root "table table"))]
      (if (or (empty? nested) (>= pass 4))
        moved
        (do
          ;; Reverse document order: each table is placed immediately after its
          ;; outermost ancestor, so walking backwards leaves them in order.
          (doseq [^Element t (reverse nested)]
            (when-let [^Element outer (last (filter table-element? (.parents t)))]
              (.after outer t)))
          (recur (+ moved (count nested)) (inc pass)))))))

(defn- reparent-stray-cell-content!
  "Wrap every content-bearing non-cell child of a `tr` in a `td`.

   Pre : `root` is a parsed document body.
   Post: no `tr` holds a content-bearing child that is not a `td` or a `th`.
         Returns how many cells were created.
   Inv : no content moves out of its row, and no content changes order.

   A `tr` is not a container Quill can put content in — `TableRow`'s only
   allowed child is `TableCell` — so a stray run is hoisted out of the table
   entirely. Blank text between cells is left alone, because
   `doc-html/squeeze-whitespace!` removes it later and wrapping it would
   manufacture an empty column.

   Each stray node gets its own cell rather than being grouped into runs. The
   markup that reaches this step is malformed, so the simpler rule is the one
   whose result can be predicted."
  [^Element root]
  (let [!count (volatile! 0)]
    (doseq [^Element tr (vec (.select root "tr"))
            ^Node n (vec (.childNodes tr))]
      (when (and (content-bearing? n) (not (cell-element? n)))
        (.wrap n "<td></td>")
        (vswap! !count inc)))
    @!count))

(defn- captions->paragraphs!
  "Lift every `<caption>` out of its table as a bolded paragraph above it.

   Pre : `root` is a parsed document body.
   Post: no `caption` remains. Each former caption is a `<p><strong>` placed
         immediately before the table that held it. Returns how many moved.
   Inv : the caption's text and inline markup are preserved in order.

   Quill's table blots are `table`, `table-row`, `table-body` and
   `table-container` — no caption among them — so a caption's text would
   otherwise reappear as a bare run inside the table on the first edit.

   Relocated from `freememo.doc-html`, where it ran for the Chrome capture
   extension only."
  [^Element root]
  (let [els (vec (.select root "caption"))]
    (doseq [^Element caption els]
      (when-let [^Element table (.parent caption)]
        (let [p (Element. "p")
              strong (Element. "strong")]
          (doseq [^Node node (vec (.childNodes caption))]
            (.appendChild strong node))
          (.appendChild p strong)
          (.before table p)
          (.remove caption))))
    (count els)))

(defn tables->blockquotes!
  "Replace every table matching `selector` with a blockquote holding its rows'
   content in document order.

   Pre : `root` still carries its source classes and attributes, i.e.
         `clean-html` has NOT run. `unnest-tables!` SHOULD have run, so no
         matched table holds another table.
   Post: no element matching `selector` remains. Each one is a `blockquote`
         standing where the table stood, or is REMOVED when it carried no
         content-bearing node. Returns how many were replaced or removed.
   Inv : nothing a reader could see is lost. Every row child crosses, whether it
         sits in a cell or strayed outside one — this is also called BEFORE
         `reparent-stray-cell-content!`, from `freememo.wikipedia`, so it cannot
         assume every row child is a cell.

   The content moves across untouched rather than being wrapped in paragraphs.
   Quill drops a `div` and keeps its text, so a wrapper would only add a
   construct with no blot. A `blockquote` is the one boxed-aside form Quill
   holds, which is the same target `doc-html/admonitions->blockquotes!` picked
   for Sphinx.

   Public because `freememo.wikipedia` calls it with the wiki banner classes.
   Keeping the arithmetic here and the vocabulary there is what stops a
   Wikipedia class from reaching the shared layer."
  [^Element root selector]
  (let [tables (vec (.select root selector))]
    (doseq [^Element table tables]
      (let [quote-el (Element. "blockquote")]
        (doseq [^Element tr (vec (.select table "tr"))
                ^Node child (vec (.childNodes tr))]
          (if (cell-element? child)
            (doseq [^Node node (vec (.childNodes child))]
              (.appendChild quote-el node))
            (when (content-bearing? child)
              (.appendChild quote-el child))))
        ;; An empty blockquote is a box with nothing in it. Removing the table
        ;; outright is what the reader would expect of a layout wrapper that
        ;; carried nothing.
        (if (some content-bearing? (.childNodes quote-el))
          (.replaceWith table quote-el)
          (.remove table))))
    (count tables)))

(defn- header-cells->data-cells!
  "Retag every `th` as a `td` whose content sits in a `strong`.

   Pre : `root` is a parsed document body.
   Post: no `th` remains. Each former `th` is a `td` with exactly one child, a
         `strong` holding its former children in order. Returns how many changed.
   Inv : no cell and no text is lost.

   Quill's table blots carry TD and no TH, and a `th` is lost OUTRIGHT rather
   than degraded — measured against the pre-change stored HTML, the merged
   table's rows came out 1 cell wide, not 2. So this step is structural, not
   cosmetic. The `strong` keeps the emphasis the browser gave the header for
   free, in a tag `clean-html` and Quill both hold.

   The four lines below repeat `doc-html/bold-in-place!` deliberately.
   `freememo.doc-html` requires THIS namespace, so the dependency runs the wrong
   way for reuse, and a shared namespace holding one four-line helper would cost
   more than it saves.

   Relocated from `freememo.doc-html/normalize-table-sections!`, which did this
   and the `thead` fold together. They are unrelated jobs."
  [^Element root]
  (let [headers (vec (.select root "th"))]
    (doseq [^Element th headers]
      (let [strong (Element. "strong")]
        (doseq [^Node node (vec (.childNodes th))]
          (.appendChild strong node))
        (.appendChild th strong)
        (.tagName th "td")))
    (count headers)))

(defn- fold-sections-into-tbody!
  "Move every `thead` and `tfoot` row into the table's `tbody` and drop the
   section wrappers.

   Pre : `root` is a parsed document body.
   Post: no `thead` and no `tfoot` remains. Every `tr` sits in a `tbody`, in the
         order a browser renders them: head rows, body rows, foot rows. Returns
         how many wrappers were removed.
   Inv : no row is lost and no row changes its rendered position.

   Quill registers `table-body` for TBODY and nothing for THEAD or TFOOT, so
   both are dropped on load and their rows would leave the table with them.

   Relocated from `freememo.doc-html/normalize-table-sections!`."
  [^Element root]
  (let [sections (vec (.select root "thead, tfoot"))]
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
    (count sections)))

(defn- drop-hidden-rows!
  "Remove every row whose `style` declares `display:none`.

   Pre : `root` still carries its source `style` attributes, i.e. `clean-html`
         has NOT run.
   Post: no `tr` carries a `display:none` declaration. Returns how many were
         removed.
   Inv : a row that is merely empty is untouched. An infobox logo row holds an
         `img` and no text, and it must survive.

   `clean-html` keeps only `color` and `background-color`, so after sanitizing a
   hidden row is an ordinary empty row that nothing can tell from a real one.
   One Wikipedia article carried two, which rendered as blank rows between the
   real ones.

   This runs BEFORE `rectangularize-rows!` so a row nobody will see cannot widen
   the table it was removed from. `hidden-style-re` records why the class-based
   spelling is not read here."
  [^Element root]
  (let [rows (vec (filter (fn [^Element tr]
                            (some? (re-find hidden-style-re (.attr tr "style"))))
                    (.select root "tr[style]")))]
    (doseq [^Element tr rows]
      (.remove tr))
    (count rows)))

(defn- expand-row-spans!
  "Replace one row's `colspan` attributes with real cells.

   Pre : `tr` is a `tr` element.
   Post: no cell in `tr` carries `colspan` or `rowspan`. A cell that declared
         `colspan=\"n\"`, for n within 1 and `max-colspan`, is followed by n-1
         empty `td` siblings. Returns how many cells were added.
   Inv : the original cells keep their content, their order, and their position
         relative to the cells around them. At most
         `max-colspan - 1` cells are added per cell, so a hostile attribute
         cannot inflate the document.

   `rowspan` is removed without being expanded. It needs an occupancy grid
   across rows, and it does not occur in the corpus this was built against —
   zero occurrences across four Wikipedia articles after navbox removal. See
   `plans/wiki-table-lowering.md`."
  [^Element tr]
  (reduce
    (fn [added ^Element cell]
      (let [declared (try (Integer/parseInt (.attr cell "colspan"))
                          (catch Exception _ 1))
            span (if (<= 1 declared max-colspan) declared 1)
            extra (dec span)]
        (.removeAttr cell "colspan")
        (.removeAttr cell "rowspan")
        ;; Each filler lands directly after `cell`. They are identical and empty,
        ;; so insertion order among them does not matter.
        (dotimes [_ extra]
          (.after cell (Element. "td")))
        (+ added extra)))
    0
    (row-cells tr)))

(defn- rectangularize-rows!
  "Give every row of every table the same cell count.

   Pre : `root` is a parsed document body. `header-cells->data-cells!` MUST have
         run, or a `th` is counted as a cell and then retagged afterwards.
         `reparent-stray-cell-content!` MUST have run, or content outside a cell
         is not counted and the row comes out one column short.
   Post: within each table, every `tr` holds an equal number of cells, and no
         cell carries `colspan` or `rowspan`. Returns
         `{:span-cells n :padding-cells n}`.
   Inv : no existing cell is removed, reordered, or emptied.

   A ragged table is what Quill's `balanceCells()` would fix, but that runs on
   `text-change` and not on load, so a freshly opened document would stay ragged
   until the user typed and then change shape under them.

   `table-rows` and `row-cells` both read direct structure only, so a nested
   table that survived cannot widen the table holding it."
  [^Element root]
  (reduce
    (fn [acc ^Element table]
      (let [rows (table-rows table)
            spans (reduce + 0 (map expand-row-spans! rows))
            widths (map (comp count row-cells) rows)
            target (if (seq widths) (apply max widths) 0)
            padding (reduce
                      (fn [n ^Element tr]
                        (let [short (- target (count (row-cells tr)))]
                          (dotimes [_ (max 0 short)]
                            (.appendChild tr (Element. "td")))
                          (+ n (max 0 short))))
                      0
                      rows)]
        (-> acc
          (update :span-cells + spans)
          (update :padding-cells + padding))))
    {:span-cells 0 :padding-cells 0}
    (vec (.select root "table"))))

(defn- separate-cell-blocks!
  "Put the separator between two content-bearing block siblings inside `cell`.

   Pre : `cell` is a `td` or `th` whose block children are still nested.
   Post: every block child with a content-bearing block element next to it ends
         with a separator text node. Returns how many were added.
   Inv : a block with no content, or with no content-bearing block after it,
         gets nothing — a separator with nothing on one side of it is what
         leaves a dangling mark when `doc-html/squeeze-whitespace!` later trims
         a boundary text node.

   This runs BEFORE the unwrap, because unwrapping destroys the sibling
   relationships that say where one former block ended and the next began."
  [^Element cell]
  (let [els (vec (.select cell cell-block-selector))]
    (count (filter identity
             (for [^Element el els]
               (let [nxt (.nextElementSibling el)]
                 (when (and (content-bearing? el)
                            (block-element? nxt)
                            (content-bearing? nxt))
                   (.appendChild el (TextNode. inline-block-separator))
                   true)))))))

(defn- resolve-cell-breaks!
  "Turn each `br` in a flattened `cell` into a separator, or remove it.

   Pre : `cell` holds no element in `cell-block-tags` — the unwrap has already
         run. Inline elements MAY still nest, so a `br` is not necessarily a
         direct child.
   Post: no `br` remains. A `br` with rendering leaves on both sides in document
         order, whose nearest preceding leaf is not already a separator, becomes
         one. Every other `br` is removed. Returns how many were handled.
   Inv : no run of `br` yields two adjacent separators, and a leading or
         trailing `br` yields none.

   The trailing case is why this is not a plain replace. A `<td>a<br></td>` gave
   `<td>a;</td>`, because the separator landed as the cell's last child and
   `doc-html/squeeze-whitespace!` then trimmed its trailing space — a mark the
   reader sees, separating nothing.

   The test is over LEAVES in document order, not over the `br`'s siblings. A
   `br` inside an inline element — `<span>a<br>b</span>`, which the unwrap does
   not touch — has a sibling index into its span and not into the cell, and
   reading it as the latter threw `IndexOutOfBoundsException` on real PostgreSQL
   content. `leaf-content?` also keeps the `br`'s own ancestors out of the
   answer, which a subtree test could not."
  [^Element cell]
  (let [brs (vec (.select cell "br"))]
    (doseq [^Element br brs]
      (let [nodes (vec (descendants-in-document-order cell))
            i (first (keep-indexed (fn [i n] (when (identical? n br) i)) nodes))
            before (when i (filter leaf-content? (subvec nodes 0 i)))
            after? (boolean (when i (some leaf-content? (subvec nodes (inc i)))))]
        (if (and (seq before) after? (not (separator-text? (last before))))
          (.replaceWith br (TextNode. inline-block-separator))
          (.remove br))))
    (count brs)))

(defn- inline-cell-blocks!
  "Turn every block child of a cell into inline content.

   Pre : `root` is a parsed document body. `unnest-tables!` MUST have run, so no
         cell holds a table.
   Post: no cell holds an element in `cell-block-tags`, and no cell holds a
         `br`. Returns how many separators, unwraps and `br` removals it made.
   Inv : all text survives. Two content-bearing runs that were separate blocks
         are separated by `inline-block-separator`, and no separator is left
         with nothing on one side of it.

   Two Quill facts make this necessary. `TableCell` extends `Block`, whose
   `allowedChildren` holds no list container, and Parchment answers a block
   child by splitting the row and unwrapping the cell — which put a plainlist's
   URL outside its table. And the `tr` matcher formats every op in the row
   delta, so a newline inside a cell becomes a cell boundary. A `br` is
   therefore resolved to text or removed, never kept.

   Three phases, in this order: separators while the nesting still says where
   the boundaries are, then the unwrap, then the `br` pass over the flat result.
   `resolve-cell-breaks!` states why it cannot run first."
  [^Element root]
  (let [!count (volatile! 0)]
    (doseq [^Element cell (vec (.select root cell-selector))]
      (vswap! !count + (separate-cell-blocks! cell))
      ;; Reverse document order puts a child before its parent, so a handle is
      ;; never used after its holder was unwrapped. Same idiom as
      ;; `doc-html/unwrap-layout-divs!`.
      (doseq [^Element el (reverse (vec (.select cell cell-block-selector)))]
        (.unwrap el)
        (vswap! !count inc))
      (vswap! !count + (resolve-cell-breaks! cell)))
    @!count))

(defn- separate-delta-adjacent-tables!
  "Put a block between two tables that nothing separates in the Delta.

   Pre : `root` is a parsed document body. Every step that adds, removes or
         moves a table MUST have run, because each one changes which tables are
         adjacent.
   Post: `delta-adjacent-followers` returns empty for `root`. Returns how many
         separators were inserted.
   Inv : no existing node is removed or reordered.

   Without this, `TableContainer.checkMerge` merges the two tables into one grid
   and `TableRow.checkMerge` merges their same-numbered rows, because Quill
   numbers rows per table. The separator is `<p><br></p>`, which is Quill's own
   representation of an empty line, so it survives `clean-html` and produces a
   line the two containers cannot see past.

   `delta-adjacent-followers` holds the adjacency rule, and a
   `nextElementSibling` test is NOT it — the measured case had two
   whitespace-filled empty spans between the tables."
  [^Element root]
  (let [followers (delta-adjacent-followers root)]
    (doseq [^Element following followers]
      (.before following (doto (Element. "p") (.appendChild (Element. "br")))))
    (count followers)))

;; ---------------------------------------------------------------------------
;; Entry point
;; ---------------------------------------------------------------------------

(defn lower-tables!
  "Rewrite every table under `root` into a shape Quill loads unchanged.

   Pre : `root` is a parsed element holding imported HTML with its SOURCE
         classes, attributes and styles intact. `clean-html` MUST NOT have run.
   Post: `root` holds no `th`, `thead`, `tfoot`, `caption`, nested table,
         `table[role=presentation]`, `colspan`, `rowspan`, hidden row, block
         child of a cell, `br` inside a cell, content-bearing non-cell child of
         a `tr`, or Delta-adjacent table pair. Every table's rows hold an equal
         number of cells. Returns a count per step.
   Inv : nothing a reader could see is lost, and every target tag already
         survives `clean-html`.

   Step order carries the correctness at six points.

   Nested tables are lifted FIRST, so `tables->blockquotes!` never carries a
   data table into a blockquote and the width arithmetic never meets one.

   Stray row content is reparented BEFORE the width arithmetic, or a row holding
   content outside a cell comes out one column short of what it renders.

   Captions are lifted BEFORE layout tables are replaced. A caption is not a row
   child, so `tables->blockquotes!` would discard one still attached.

   Header cells become data cells BEFORE rows are rectangularized, because a
   `th` is a cell and retagging it afterwards would leave the count stale.

   Hidden rows are dropped BEFORE rectangularization, so a row nobody will see
   cannot set the width every other row is padded to.

   Table separation runs LAST, because lifting a nested table and replacing a
   layout table both change which tables are adjacent."
  [^Element root]
  (let [unnested   (unnest-tables! root)
        strays     (reparent-stray-cell-content! root)
        captions   (captions->paragraphs! root)
        quoted     (tables->blockquotes! root presentation-selector)
        headers    (header-cells->data-cells! root)
        sections   (fold-sections-into-tbody! root)
        hidden     (drop-hidden-rows! root)
        {:keys [span-cells padding-cells]} (rectangularize-rows! root)
        inlined    (inline-cell-blocks! root)
        separators (separate-delta-adjacent-tables! root)]
    {:tables-unnested unnested
     :stray-cells-created strays
     :captions-lifted captions
     :layout-tables-quoted quoted
     :header-cells-retagged headers
     :table-sections-folded sections
     :hidden-rows-dropped hidden
     :span-cells-added span-cells
     :padding-cells-added padding-cells
     :cell-blocks-inlined inlined
     :table-separators-inserted separators}))
