(ns freememo.table-lowering-test
  "Asserts `freememo.table-lowering` rewrites every table construct Quill 2.0.3
   cannot hold, and that the result satisfies the whole-document invariant its
   entry point states.

   The browser half lives in `playwright/probe-wiki-tables.mjs`, which loads the
   stored HTML this namespace's pipeline produces into a real Quill and asserts
   the rendered shape. Neither half is sufficient alone. The corruption this
   fixes happens during Quill's Delta conversion, so a passing suite here proves
   only that the input Quill receives is the shape the fix intends.

   Needs no database."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.doc-html :as dh]
            [freememo.html-cleaner :as cleaner]
            [freememo.table-lowering :as tl])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Element]))

;; ---------------------------------------------------------------------------
;; Harness
;; ---------------------------------------------------------------------------

(defn- body
  "Parse `html` and return its body element, with pretty-print off.
   Jsoup's default formatting injects whitespace text nodes between siblings,
   which would make `line-producing?` answer differently than it does in the
   real pipeline."
  ^Element [html]
  (let [doc (Jsoup/parseBodyFragment html)]
    (.prettyPrint (.outputSettings doc) false)
    (.body doc)))

(defn- lower
  "Run the whole entry point over `html` and return the rewritten HTML."
  [html]
  (let [el (body html)]
    (tl/lower-tables! el)
    (.html el)))

(defn- counts
  "Run the whole entry point over `html` and return only its count map."
  [html]
  (tl/lower-tables! (body html)))

(defn- tag-count [html tag]
  (.size (.select (body html) tag)))

(defn- row-cells [^Element tr]
  (filter (fn [^Element e] (contains? #{"td" "th"} (.tagName e))) (.children tr)))

(defn- row-widths
  "Cell count per row, per table, as a vector of vectors."
  [html]
  (mapv (fn [^Element t] (mapv (comp count row-cells) (.select t "tr")))
        (.select (body html) "table")))

(defn- row-texts
  "Whitespace-collapsed text per row of the first table."
  [html]
  (mapv (fn [^Element tr] (str/replace (.text tr) #"\s+" " "))
        (.select (body html) "table tr")))

;; ---------------------------------------------------------------------------
;; The invariant the entry point promises
;; ---------------------------------------------------------------------------

(defn- violations
  "Every way `html` still breaks `lower-tables!`'s postcondition.
   Returns a map of reason to count, holding only the non-zero reasons, so a
   failure names what is wrong instead of only that something is.

   Adjacency is read through `tl/delta-adjacent-followers` and stray content
   through `tl/content-bearing?`, NOT through a copy of either. A copy could not
   fail for a bug in the definition it copied, and when this suite did carry one
   the two had already drifted — the copy's block-tag set was missing `thead`,
   `tbody`, `tr`, `td`, `th`, `hr`, `figcaption`, `article`, `aside`, `main` and
   `nav`."
  [html]
  (let [b (body html)
        ragged (count (filter (fn [^Element t]
                                (let [ws (map (comp count row-cells) (.select t "tr"))]
                                  (and (seq ws) (not= 1 (count (set ws))))))
                        (.select b "table")))
        strays (count (for [^Element tr (.select b "tr")
                            child (.childNodes tr)
                            :when (and (tl/content-bearing? child)
                                       (not (contains? #{"td" "th"}
                                              (when (instance? Element child)
                                                (.tagName ^Element child)))))]
                        child))]
    (into {}
      (remove (comp zero? val))
      {:header-cells (.size (.select b "th"))
       :sections (.size (.select b "thead, tfoot"))
       :captions (.size (.select b "caption"))
       :colspan (.size (.select b "[colspan]"))
       :rowspan (.size (.select b "[rowspan]"))
       :nested-tables (.size (.select b "table table"))
       :presentation-tables (.size (.select b tl/presentation-selector))
       :hidden-rows (.size (.select b "tr[style*=none]"))
       :cell-blocks (.size (.select b "td ul, td ol, td div, td p, td dl, td blockquote"))
       :cell-breaks (.size (.select b "td br"))
       :stray-row-content strays
       :ragged-tables ragged
       :delta-adjacent-tables (count (tl/delta-adjacent-followers b))})))

;; ---------------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------------

(def ^:private infobox-html
  "The shape Wikipedia's Parsoid HTML gives an infobox — a caption row and an
   image row that both span the grid, two rows hidden with `display:none`, and a
   plainlist inside the last cell."
  (str "<table class=\"infobox vevent\">"
       "<tbody>"
       "<tr><th colspan=\"2\">Hazelcast</th></tr>"
       "<tr><td colspan=\"2\"><img src=\"/api/media/9\" alt=\"logo\"></td></tr>"
       "<tr><th>Developer</th><td>Hazelcast</td></tr>"
       "<tr class=\"infobox-hiddenrow\" style=\"display: none;\"><td colspan=\"2\"></td></tr>"
       "<tr><th>Written in</th><td>Java</td></tr>"
       "<tr style=\"display:none\"><td colspan=\"2\"> </td></tr>"
       "<tr><th>Repository</th>"
       "<td><div class=\"plainlist\"><ul><li>github.com/x</li><li>gitlab.com/y</li></ul></div></td></tr>"
       "</tbody></table>"))

(def ^:private banner-then-infobox-html
  "Two tables separated only by whitespace-filled empty spans. This is the
   measured shape of the defect — the spans are inline, so neither produces a
   line in the Delta and Quill merges the two tables."
  (str "<table role=\"presentation\" class=\"ambox\"><tbody><tr>"
       "<td><img src=\"/api/media/1\" alt=\"icon\"></td>"
       "<td>This article needs more citations.</td>"
       "</tr></tbody></table>"
       "<span>\n  \n  </span> <span>\n  </span>\n "
       "<table><tbody><tr><th colspan=\"2\">Hazelcast</th></tr>"
       "<tr><th>Developer</th><td>Hazelcast</td></tr></tbody></table>"))

;; ---------------------------------------------------------------------------
;; Per-step tests
;; ---------------------------------------------------------------------------

(deftest a-presentation-table-becomes-a-blockquote
  (let [out (lower "<table role=\"presentation\"><tbody><tr><td>note</td><td>body</td></tr></tbody></table>")]
    (is (zero? (tag-count out "table")))
    (is (= 1 (tag-count out "blockquote")))
    (is (str/includes? out "note"))
    (is (str/includes? out "body")
      "no cell content may be lost when the grid is discarded")))

(deftest the-selector-is-a-parameter-so-wiki-classes-stay-in-the-wiki-path
  (testing "freememo.wikipedia passes its own banner classes"
    (let [el (body "<table class=\"ambox\"><tbody><tr><td>notice</td></tr></tbody></table>")
          n (tl/tables->blockquotes! el "table.ambox, table.metadata")
          out (.html el)]
      (is (= 1 n))
      (is (zero? (tag-count out "table")))
      (is (str/includes? out "notice"))))
  (testing "the shared rule does not read a Wikipedia class"
    (is (not (str/includes? tl/presentation-selector "ambox")))))

(deftest a-caption-becomes-a-bold-paragraph-above-its-table
  (let [out (lower "<table><caption>Releases</caption><tbody><tr><td>x</td></tr></tbody></table>")]
    (is (zero? (tag-count out "caption")))
    (is (re-find #"<p><strong>Releases</strong></p>\s*<table" out)
      "the paragraph must sit before the table, not inside it")))

(deftest a-caption-on-a-layout-table-survives-the-blockquote-replacement
  (testing "captions lift BEFORE layout tables are replaced, or the caption goes
            with the table that held it"
    (let [out (lower (str "<table role=\"presentation\"><caption>Notice</caption>"
                          "<tbody><tr><td>body</td></tr></tbody></table>"))]
      (is (str/includes? out "Notice"))
      (is (str/includes? out "body")))))

(deftest a-nested-table-becomes-a-sibling
  (let [out (lower (str "<table><tbody><tr><td>outer"
                        "<table><tbody><tr><td>inner</td></tr></tbody></table>"
                        "</td></tr></tbody></table>"))]
    (is (zero? (tag-count out "table table")))
    (is (= 2 (tag-count out "table")))
    (is (str/includes? out "outer"))
    (is (str/includes? out "inner"))
    (is (< (str/index-of out "outer") (str/index-of out "inner"))
      "the inner table follows the outer one, so reading order is preserved")))

(deftest a-doubly-nested-table-unnests-in-one-call
  (testing "the loop repeats, because lifting one table exposes the next"
    (let [out (lower (str "<table><tbody><tr><td>a"
                          "<table><tbody><tr><td>b"
                          "<table><tbody><tr><td>c</td></tr></tbody></table>"
                          "</td></tr></tbody></table>"
                          "</td></tr></tbody></table>"))]
      (is (zero? (tag-count out "table table")))
      (is (= 3 (tag-count out "table"))))))

(deftest a-header-cell-becomes-a-bold-data-cell
  (let [out (lower "<table><tbody><tr><th>Developer</th><td>x</td></tr></tbody></table>")]
    (is (zero? (tag-count out "th")))
    (is (= 2 (tag-count out "td")))
    (is (str/includes? out "<td><strong>Developer</strong></td>")
      "Quill carries TD and no TH, so the emphasis has to move into the content")))

(deftest thead-and-tfoot-rows-fold-into-tbody-in-render-order
  (let [out (lower (str "<table>"
                        "<tfoot><tr><td>foot</td></tr></tfoot>"
                        "<thead><tr><td>head</td></tr></thead>"
                        "<tbody><tr><td>body</td></tr></tbody>"
                        "</table>"))]
    (is (zero? (tag-count out "thead, tfoot")))
    (is (= 3 (tag-count out "tbody tr")))
    (is (= ["head" "body" "foot"] (row-texts out))
      "a browser renders head rows first and foot rows last, whatever the
       source order")))

(deftest a-hidden-row-is-dropped-and-an-empty-one-is-not
  (testing "clean-html strips display:none, so a hidden row would become an
            ordinary empty row that nothing could tell from a real one"
    (let [out (lower (str "<table><tbody>"
                          "<tr><td>real</td></tr>"
                          "<tr style=\"display: none;\"><td>hidden</td></tr>"
                          "<tr style=\"DISPLAY:NONE\"><td>also hidden</td></tr>"
                          "</tbody></table>"))]
      (is (= 1 (tag-count out "tr")))
      (is (not (str/includes? out "hidden")))))
  (testing "a row holding only an image has no text and must survive"
    (let [out (lower "<table><tbody><tr><td><img src=\"/api/media/9\"></td></tr></tbody></table>")]
      (is (= 1 (tag-count out "tr")))
      (is (= 1 (tag-count out "img"))))))

(deftest colspan-becomes-real-cells-and-every-row-ends-the-same-width
  (let [out (lower (str "<table><tbody>"
                        "<tr><td colspan=\"3\">wide</td></tr>"
                        "<tr><td>a</td><td>b</td><td>c</td></tr>"
                        "</tbody></table>"))]
    (is (zero? (tag-count out "[colspan]")))
    (is (= [[3 3]] (row-widths out)))
    (is (str/includes? out "wide"))))

(deftest rowspan-is-removed-without-expansion
  (testing "expansion needs an occupancy grid and the corpus holds none — see
            plans/wiki-table-lowering.md"
    (let [out (lower "<table><tbody><tr><td rowspan=\"2\">a</td><td>b</td></tr><tr><td>c</td></tr></tbody></table>")]
      (is (zero? (tag-count out "[rowspan]")))
      (is (= [[2 2]] (row-widths out))
        "the short row is still padded, so the table is rectangular even though
         the span itself is not reconstructed"))))

(deftest a-short-row-is-padded-rather-than-left-ragged
  (testing "balanceCells runs on text-change and not on load, so a ragged table
            would change shape under the user at the first keystroke"
    (is (= [[2 2 2]]
          (row-widths (lower (str "<table><tbody>"
                                  "<tr><td>a</td><td>b</td></tr>"
                                  "<tr><td>c</td></tr>"
                                  "<tr></tr>"
                                  "</tbody></table>")))))))

(deftest a-nested-table-does-not-widen-the-row-that-held-it
  (testing "row-cells counts direct children only"
    (is (= [[1] [1]]
          (row-widths (lower (str "<table><tbody><tr><td>outer"
                                  "<table><tbody><tr><td>inner</td></tr></tbody></table>"
                                  "</td></tr></tbody></table>")))))))

(deftest a-list-inside-a-cell-stays-inside-the-cell
  (testing "TableCell extends Block, whose allowedChildren holds no list, so
            Parchment would split the row and unwrap the cell"
    (let [out (lower (str "<table><tbody><tr><td>"
                          "<div class=\"plainlist\"><ul><li>first</li><li>second</li></ul></div>"
                          "</td></tr></tbody></table>"))]
      (is (zero? (tag-count out "td ul, td ol, td div, td li")))
      (is (= 1 (tag-count out "td")))
      (is (str/includes? out (str "first" tl/inline-block-separator "second"))
        "two former block siblings must stay tellable apart"))))

(deftest a-line-break-inside-a-cell-becomes-the-separator
  (testing "the tr matcher formats every op in the row delta, so a newline in a
            cell becomes a cell boundary"
    (let [out (lower "<table><tbody><tr><td>a<br>b</td></tr></tbody></table>")]
      (is (zero? (tag-count out "td br")))
      (is (= [[1]] (row-widths out)))
      (is (str/includes? out (str "a" tl/inline-block-separator "b"))))))

(deftest a-separator-is-never-left-with-nothing-on-one-side
  (testing "a trailing br gave `<td>a ·</td>` — the separator landed last and
            doc-html/squeeze-whitespace! trimmed its trailing space, leaving a
            mark the reader sees that separates nothing"
    (is (= "<table><tbody><tr><td>a</td></tr></tbody></table>"
          (lower "<table><tbody><tr><td>a<br></td></tr></tbody></table>"))))
  (testing "a leading br is removed the same way"
    (is (= "<table><tbody><tr><td>a</td></tr></tbody></table>"
          (lower "<table><tbody><tr><td><br>a</td></tr></tbody></table>"))))
  (testing "a run of br yields ONE separator, not one per break"
    (is (= (str "<table><tbody><tr><td>a" tl/inline-block-separator "b</td></tr></tbody></table>")
          (lower "<table><tbody><tr><td>a<br><br><br>b</td></tr></tbody></table>"))))
  (testing "an empty block sibling earns no separator"
    (let [out (lower "<table><tbody><tr><td><div>a</div><div></div></td></tr></tbody></table>")]
      (is (not (str/includes? out tl/inline-block-separator)))
      (is (str/includes? out "<td>a</td>")))))

(deftest a-br-nested-in-an-inline-element-is-handled
  (testing "the unwrap leaves inline elements nested, so a br is not always a
            direct child of the cell. Reading its sibling index as an index into
            the CELL threw IndexOutOfBoundsException on real PostgreSQL content."
    (let [out (lower "<table><tbody><tr><td><span>a<br>b</span></td></tr></tbody></table>")]
      (is (zero? (tag-count out "td br")))
      (is (= [[1]] (row-widths out)))
      (is (str/includes? out (str "a" tl/inline-block-separator "b")))))
  (testing "and a nested trailing br still leaves no mark"
    (let [out (lower "<table><tbody><tr><td><span>a<br></span></td></tr></tbody></table>")]
      (is (zero? (tag-count out "td br")))
      (is (not (str/includes? out "·")))))
  (testing "a br whose only preceding content is in an outer element still counts
            it — the test is over document order, not over siblings. Asserted on
            the rendered TEXT, because the separator lands inside the span and the
            markup therefore reads `a<span> · b</span>`."
    (let [out (lower "<table><tbody><tr><td>a<span><br>b</span></td></tr></tbody></table>")]
      (is (= [(str "a" tl/inline-block-separator "b")] (row-texts out))))))

(deftest the-separator-survives-the-later-whitespace-squeeze
  (testing "lower-tables! runs inside rewrite-for-quill!, which squeezes
            whitespace afterwards — the separator must reach storage intact"
    (let [out (dh/doc-html->quill-html
                (str "<table><tbody><tr><td>"
                  "<ul><li>first</li><li>second</li></ul>"
                  "</td><td>x</td></tr></tbody></table>"))]
      (is (str/includes? out (str "first" tl/inline-block-separator "second"))))
    (let [out (dh/doc-html->quill-html "<table><tbody><tr><td>a<br></td><td>x</td></tr></tbody></table>")]
      (is (not (str/includes? out "·"))
        "and a br that separates nothing must leave no mark behind")
      (is (str/includes? out "<td>a</td>")))))

(deftest an-absurd-colspan-does-not-inflate-the-document
  (testing "measured: colspan=5000 produced 5000 cells before max-colspan bounded
            it. The row is still rectangular, because padding sizes it from the
            real rows."
    (let [out (lower (str "<table><tbody>"
                          "<tr><td colspan=\"5000\">x</td></tr>"
                          "<tr><td>a</td><td>b</td></tr>"
                          "</tbody></table>"))]
      (is (zero? (tag-count out "[colspan]")))
      (is (= [[2 2]] (row-widths out)))
      (is (str/includes? out "x"))))
  (testing "a span within the bound still expands"
    (is (= [[3 3]]
          (row-widths (lower (str "<table><tbody>"
                                  "<tr><td colspan=\"3\">wide</td></tr>"
                                  "<tr><td>a</td><td>b</td><td>c</td></tr>"
                                  "</tbody></table>"))))))
  (testing "colspan=0 and a negative colspan are treated as one cell"
    (is (= [[1]] (row-widths (lower "<table><tbody><tr><td colspan=\"0\">x</td></tr></tbody></table>"))))
    (is (= [[1]] (row-widths (lower "<table><tbody><tr><td colspan=\"-3\">x</td></tr></tbody></table>"))))))

(deftest stray-row-content-is-reparented-into-a-cell
  (testing "TableRow's only allowed child is TableCell, so a stray run is hoisted
            out of the table entirely"
    (let [out (lower "<table><tbody><tr>stray<td>a</td></tr></tbody></table>")]
      (is (empty? (violations out)))
      (is (str/includes? out "stray"))
      (is (= [[2 2]] (row-widths (lower (str "<table><tbody>"
                                             "<tr>stray<td>a</td></tr>"
                                             "<tr><td>b</td><td>c</td></tr>"
                                             "</tbody></table>")))))))
  (testing "blank text between cells earns no column — squeeze-whitespace! drops
            it later, and wrapping it would manufacture an empty one"
    (is (= [[2]] (row-widths (lower "<table><tbody><tr><td>a</td>  <td>b</td></tr></tbody></table>"))))))

(deftest a-layout-table-carrying-nothing-is-removed-not-emptied
  (testing "an empty blockquote is a box with nothing in it"
    (let [out (lower "<table role=\"presentation\"><tbody><tr><td> </td></tr></tbody></table>")]
      (is (zero? (tag-count out "table")))
      (is (zero? (tag-count out "blockquote")))))
  (testing "a layout table's stray row content still crosses, because
            freememo.wikipedia calls this BEFORE stray content is reparented"
    (let [el (body "<table role=\"presentation\"><tbody><tr>notice<td>body</td></tr></tbody></table>")]
      (tl/tables->blockquotes! el tl/presentation-selector)
      (let [out (.html el)]
        (is (str/includes? out "notice"))
        (is (str/includes? out "body"))))))

(deftest the-adjacency-rule-has-one-definition
  (testing "delta-adjacent-followers is what the step fixes and what this suite
            asserts — a copy could not fail for a bug in the original"
    (is (= 1 (count (tl/delta-adjacent-followers
                      (body (str "<table><tbody><tr><td>a</td></tr></tbody></table>"
                                 "<span>\n </span>"
                                 "<table><tbody><tr><td>b</td></tr></tbody></table>"))))))
    (is (zero? (count (tl/delta-adjacent-followers
                        (body (str "<table><tbody><tr><td>a</td></tr></tbody></table>"
                                   "<p>prose</p>"
                                   "<table><tbody><tr><td>b</td></tr></tbody></table>")))))))
  (testing "content-bearing? counts an image, so an infobox logo row is not empty.
            The cells are wrapped in a real table because Jsoup discards a bare
            <td> outside one."
    (let [cell #(.selectFirst (body (str "<table><tbody><tr><td>" % "</td></tr></tbody></table>")) "td")]
      (is (tl/content-bearing? (cell "<img src=\"/api/media/9\">")))
      (is (not (tl/content-bearing? (cell "<span> </span>")))))))

(deftest two-tables-with-only-blank-spans-between-them-get-a-separator
  (let [out (lower banner-then-infobox-html)]
    (is (empty? (violations out)))
    (is (= 1 (tag-count out "blockquote")) "the ambox is the layout table")
    (is (= 1 (tag-count out "table")) "the infobox is the data table")))

(deftest two-genuinely-adjacent-data-tables-get-a-separator
  (testing "a nextElementSibling test is not the check — this is the case it
            does catch, and the blank-span case above is the one it misses"
    (let [out (lower (str "<table><tbody><tr><td>a</td></tr></tbody></table>"
                          "<table><tbody><tr><td>b</td></tr></tbody></table>"))]
      (is (= 2 (tag-count out "table")))
      (is (empty? (violations out)))
      (is (re-find #"</table><p><br></p><table" out)
        "the separator is Quill's own empty line, between the two tables"))))

(deftest prose-between-two-tables-needs-no-separator
  (let [out (lower (str "<table><tbody><tr><td>a</td></tr></tbody></table>"
                        "<p>real prose</p>"
                        "<table><tbody><tr><td>b</td></tr></tbody></table>"))]
    (is (= 1 (tag-count out "p")) "no separator was added")
    (is (empty? (violations out)))))

;; ---------------------------------------------------------------------------
;; Whole-document invariant
;; ---------------------------------------------------------------------------

(deftest the-infobox-shape-satisfies-every-postcondition
  (let [out (lower infobox-html)]
    (is (empty? (violations out)))
    (is (= [[2 2 2 2 2]] (row-widths out))
      "seven source rows less the two hidden ones, every one two cells wide")
    (is (= 1 (tag-count out "img")) "the logo row survived")
    (is (str/includes? out (str "github.com/x" tl/inline-block-separator "gitlab.com/y")))))

(deftest the-invariant-survives-the-sanitizer
  (testing "clean-html runs after this namespace and must not reintroduce a
            violation"
    (is (empty? (violations (cleaner/clean-html (lower banner-then-infobox-html)))))
    (is (empty? (violations (cleaner/clean-html (lower infobox-html))))))
  (testing "the separator has to survive sanitizing, or the merge comes back.
            The banner fixture cannot show this — its ambox becomes a blockquote,
            so only one table is left and no separator is due."
    (let [out (cleaner/clean-html
                (lower (str "<table><tbody><tr><td>a</td></tr></tbody></table>"
                            "<table><tbody><tr><td>b</td></tr></tbody></table>")))]
      (is (str/includes? out "<p><br></p>"))
      (is (empty? (violations out))))))

(deftest clean-html-no-longer-admits-a-span-attribute
  (testing "nothing reads either attribute and Quill cannot represent them, so
            admitting them only let a path with no lowering reintroduce a
            ragged row"
    (let [out (cleaner/clean-html "<table><tbody><tr><td colspan=\"2\" rowspan=\"3\">x</td></tr></tbody></table>")]
      (is (zero? (tag-count out "[colspan]")))
      (is (zero? (tag-count out "[rowspan]")))
      (is (str/includes? out "x")))))

(deftest the-lowering-is-idempotent
  (testing "a second pass must change nothing, or a re-import corrupts a row"
    (let [once (lower infobox-html)]
      (is (= once (lower once))))
    (let [once (lower banner-then-infobox-html)]
      (is (= once (lower once))
        "the separator must not attract a second separator"))))

(deftest counts-name-every-step
  (let [c (counts infobox-html)]
    (is (= #{:tables-unnested :stray-cells-created :captions-lifted
             :layout-tables-quoted :header-cells-retagged :table-sections-folded
             :hidden-rows-dropped :span-cells-added :padding-cells-added
             :cell-blocks-inlined :table-separators-inserted}
          (set (keys c)))
      "a step whose name never reaches the report discards silently")
    (is (= 4 (:header-cells-retagged c)))
    (is (= 2 (:hidden-rows-dropped c)))
    (is (pos? (:cell-blocks-inlined c))))
  (testing "span expansion and row padding are counted apart — one report number
            could not tell a colspan from a short row"
    (let [c (counts (str "<table><tbody>"
                         "<tr><td colspan=\"3\">wide</td></tr>"
                         "<tr><td>a</td></tr>"
                         "</tbody></table>"))]
      (is (= 2 (:span-cells-added c)))
      (is (= 2 (:padding-cells-added c))))))

(deftest a-table-free-document-is-untouched
  (let [html "<p>prose</p><ul><li>item</li></ul>"]
    (is (= html (lower html)))
    (is (every? zero? (vals (counts html))))))
