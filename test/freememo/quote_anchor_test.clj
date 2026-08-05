(ns freememo.quote-anchor-test
  "Asserts freememo.quote-anchor places a NeoReader quote in an EPUB chapter.

   Needs no database. Every fixture reproduces a case measured in the reference
   export recorded in plans/boox-neoreader-google-drive-sync.md, and each case
   is the reason a rule in `quote-anchor` exists. A rule whose case is deleted
   from here can regress with nothing to catch it.

   The chapter HTML in each fixture is shaped like an imported EPUB chapter:
   `epub.clj` stores the spine item's body after `clean-html`, so headings,
   paragraphs and inline emphasis are what a chapter actually holds."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.quote-anchor :as anchor])
  (:import [org.jsoup Jsoup]))

(defn- hl
  ([chapter quote] (hl chapter quote 0))
  ([chapter quote index] {:chapter chapter :quote quote :notes [] :index index}))

(defn- outcomes [chapters highlights]
  (mapv :outcome (anchor/place-highlights chapters highlights)))

(defn- marked
  "Place, mark, and return the map of chapter position to rewritten HTML."
  [chapters highlights]
  (anchor/mark-chapters chapters (anchor/place-highlights chapters highlights)))

(def ^:private tint-selector
  (str "span[style*=" (subs anchor/highlight-color 1) "]"))

(defn- tinted-text
  "The text inside every tint span of `html`, in document order."
  [html]
  (mapv #(.text ^org.jsoup.nodes.Element %)
    (.select (Jsoup/parseBodyFragment (or html "")) tint-selector)))

;; ── 4.3 — the export's own damage must match ───────────────────────

(deftest normalization-absorbs-the-exports-damage
  (testing "4.3 — `curse of knowledgeis` matches a source that has the space"
    ;; The source italicizes the term, so NeoReader joined two inline elements
    ;; with no separator and lost the space before `is`.
    (is (= [:placed]
          (outcomes [{:title "5. Avoid Illusions of Knowing"
                      :html (str "<p>The <i>curse of knowledge</i> is our tendency to "
                              "underestimate how long it will take another person.</p>")}]
            [(hl "5. Avoid Illusions of Knowing"
               "curse of knowledgeis our tendency to underestimate")]))))

  (testing "4.3 — `hindsight bias,or` matches the same way"
    (is (= [:placed]
          (outcomes [{:title "C" :html "<p>the <i>hindsight bias,</i> or what is often called</p>"}]
            [(hl "C" "hindsight bias,or what is often called")]))))

  (testing "4.3 — curly quotation marks fold to their ASCII form"
    (is (= [:placed]
          (outcomes [{:title "C" :html "<p>and it says \"practice like you play\" out loud</p>"}]
            [(hl "C" (str "and it says " \u201c "practice like you play" \u201d
                       " out loud"))]))))

  (testing "4.3 — normalize drops whitespace and leaves nothing curly"
    (let [n (anchor/normalize (str "a b\t c\n" \u2018 "d" \u2019))]
      (is (= "abc'd'" n) "space, tab and newline all go; the curly pair folds")
      (is (not (str/includes? n " ")))))

  (testing "4.3 — normalize of nil is the empty string, not a crash"
    (is (= "" (anchor/normalize nil)))))

;; ── The two chapter readers must not drift ─────────────────────────

(deftest chapter-text-agrees-with-the-position-index
  (testing "phase 1 matches against the same characters phase 2 tints"
    ;; A drift here would tint text the placement never chose. Both derive from
    ;; the same traversal, and this asserts it end to end: a placement found in
    ;; `chapter-text` must land on exactly that text after marking.
    (let [html (str "<h2>Head</h2><p>alpha <i>beta</i>gamma</p>"
                 "<ul><li>delta</li><li>epsilon</li></ul>")
          chapters [{:title "C" :html html}]
          text (anchor/chapter-text html)]
      (is (= "Headalphabetagammadeltaepsilon" text))
      (is (= ["betagamma"] (tinted-text (get (marked chapters [(hl "C" "betagamma")]) 0)))
        "the marked characters are the ones chapter-text reported"))))

;; ── 4.4 — a quote that spans element boundaries ────────────────────

(deftest matches-a-quote-that-crosses-elements
  (let [chapters [{:title "8. Make It Stick"
                   :html (str "<h2>Generation</h2>"
                           "<p>In 2013, John McPhee published a piece in the New Yorker "
                           "about writer's block.</p>")}]
        highlights [(hl "8. Make It Stick"
                      "Generation In 2013, John McPhee published a piece in the New Yorker")]]

    (testing "4.4 — the highlight places even though it crosses a heading"
      (is (= [:placed] (outcomes chapters highlights))))

    (testing "4.4 — the mark covers both elements"
      (let [doc (Jsoup/parseBodyFragment (get (marked chapters highlights) 0))]
        (is (some? (.selectFirst doc (str "h2 " tint-selector)))
          "the heading carries a tint span")
        (is (some? (.selectFirst doc (str "p " tint-selector)))
          "the paragraph carries one too")
        (is (str/includes? (.text (.body doc)) "about writer's block.")
          "text outside the quote survives"))))

  (testing "4.4 — a quote spanning three text nodes marks all three"
    (let [chapters [{:title "C" :html "<p>one <i>two</i> <b>three</b> four</p>"}]
          html (get (marked chapters [(hl "C" "one two three")]) 0)]
      (is (= ["one" "two" "three"] (tinted-text html))
        "a fully covered middle node is wrapped whole")))

  (testing "4.4 — a quote covering a whole middle element wraps it entirely"
    (let [chapters [{:title "C" :html "<p>a<i>MIDDLE</i>z tail</p>"}]
          html (get (marked chapters [(hl "C" "aMIDDLEz")]) 0)]
      (is (= ["a" "MIDDLE" "z"] (tinted-text html)))
      (is (str/includes? (.text (Jsoup/parseBodyFragment html)) "tail")))))

;; ── 4.5 — the reading-order cursor resolves ambiguity ──────────────

(deftest cursor-resolves-a-repeated-quote
  (testing "4.5 — `reflection` lands after its predecessor, not at the first occurrence"
    (let [chapters [{:title "C"
                     :html (str "<p>A first mention of reflection here.</p>"
                             "<p>Then a marker sentence.</p>"
                             "<p>And daily reflection, as a form of practice.</p>")}]
          ps (anchor/place-highlights chapters
               [(hl "C" "Then a marker sentence" 0) (hl "C" "reflection" 1)])]
      (is (= [:placed :placed] (mapv :outcome ps)))
      (is (>= (:start (second ps)) (:end (first ps)))
        "the second placement starts at or after the end of the first")))

  (testing "4.5 — the marked HTML proves the later occurrence was chosen"
    (let [chapters [{:title "C"
                     :html "<p>first reflection</p><p>marker</p><p>second reflection</p>"}]
          html (get (marked chapters [(hl "C" "marker" 0) (hl "C" "reflection" 1)]) 0)
          ps (.select (Jsoup/parseBodyFragment html) "p")]
      (is (not (str/includes? (.html (.get ps 0)) "background-color"))
        "the first paragraph's `reflection` stays untinted")
      (is (str/includes? (.html (.get ps 2)) "background-color")
        "the third paragraph's `reflection` carries the mark")))

  (testing "4.5 — the cursor does not move when a highlight fails to place"
    (is (= [:unmatched :placed]
          (outcomes [{:title "C" :html "<p>alpha beta gamma</p>"}]
            [(hl "C" "absent from the chapter" 0) (hl "C" "alpha" 1)]))
      "a miss must not consume the cursor and block the next quote")))

;; ── 4.1 and 4.6 — chapter resolution and the forward search ────────

(deftest resolves-chapters-and-falls-back-when-absent
  (let [chapters [{:title "1. Learning Is Misunderstood" :html "<p>alpha one</p>"}
                  {:title "2. To Learn, Retrieve" :html "<p>beta two</p>"}]]

    (testing "4.1 — a heading matching a chapter title places in that chapter"
      (let [ps (anchor/place-highlights chapters [(hl "2. To Learn, Retrieve" "beta two")])]
        (is (= [:placed] (mapv :outcome ps)))
        (is (= 1 (:chapter-pos (first ps))))))

    (testing "4.1 — the title match ignores case and whitespace"
      (let [ps (anchor/place-highlights chapters [(hl "2.  to learn,   retrieve" "beta two")])]
        (is (= [:placed] (mapv :outcome ps)))
        (is (= 1 (:chapter-pos (first ps))))))

    (testing "4.6 — a heading matching nothing searches the whole book"
      (let [ps (anchor/place-highlights chapters [(hl "Chapter The Publisher Renamed" "beta two")])]
        (is (= [:fallback] (mapv :outcome ps)))
        (is (= 1 (:chapter-pos (first ps))))))

    (testing "4.6 — a resolved chapter is the only place its highlights may land"
      (is (= [:unmatched]
            (outcomes chapters [(hl "1. Learning Is Misunderstood" "beta two")]))
        "the quote lives in chapter 2, but the export named chapter 1"))))

(deftest duplicate-chapter-titles-do-not-strand-later-highlights
  ;; Regression: resolving a title to its FIRST chapter only, under a book-wide
  ;; cursor, made every highlight of a duplicate-titled later chapter
  ;; :unmatched — the resolved chapter lay behind the cursor and there was no
  ;; forward search. `epub.clj`'s extract-chapter-title takes the first h1/h2/h3
  ;; of a spine item, so consecutive items can share a title.
  (let [chapters [{:title "Notes" :html "<p>early note text</p>"}
                  {:title "Chapter One" :html "<p>body of chapter one</p>"}
                  {:title "Notes" :html "<p>late note text</p>"}]]

    (testing "a highlight belonging to the second chapter of that title places"
      (let [ps (anchor/place-highlights chapters
                 [(hl "Chapter One" "body of chapter one" 0)
                  (hl "Notes" "late note text" 1)])]
        (is (= [:placed :placed] (mapv :outcome ps)))
        (is (= [1 2] (mapv :chapter-pos ps)))))

    (testing "a highlight belonging to the first chapter of that title still places"
      (let [ps (anchor/place-highlights chapters [(hl "Notes" "early note text")])]
        (is (= [:placed] (mapv :outcome ps)))
        (is (= 0 (:chapter-pos (first ps))))))

    (testing "the cursor resets per chapter, so an earlier chapter is reachable"
      (let [ps (anchor/place-highlights chapters
                 [(hl "Notes" "late note text" 0)
                  (hl "Chapter One" "body of chapter one" 1)])]
        (is (= [:placed :placed] (mapv :outcome ps))
          "chapter 1 is not blocked by a placement recorded for chapter 2")))))

;; ── Tinting mechanics ──────────────────────────────────────────────

(deftest tinting-is-exact-and-repeatable
  (testing "the tint covers the quote and nothing else"
    (is (= ["MARKME"]
          (tinted-text (get (marked [{:title "C" :html "<p>keep MARKME drop</p>"}]
                              [(hl "C" "MARKME")]) 0)))))

  (testing "two marks in one text node both land"
    (is (= ["AAA" "BBB"]
          (tinted-text (get (marked [{:title "C" :html "<p>one AAA two BBB three</p>"}]
                              [(hl "C" "AAA" 0) (hl "C" "BBB" 1)]) 0)))))

  (testing "a mark at the very start of a text node needs no leading split"
    (is (= ["START"]
          (tinted-text (get (marked [{:title "C" :html "<p>START then rest</p>"}]
                              [(hl "C" "START")]) 0)))))

  (testing "a mark covering a whole text node needs no split at all"
    (is (= ["whole"]
          (tinted-text (get (marked [{:title "C" :html "<p>whole</p>"}]
                              [(hl "C" "whole")]) 0)))))

  (testing "the colour written is the Quill popover's Orange"
    (is (= "#faebdd" anchor/highlight-color))
    (is (not= "#44C2FF" anchor/highlight-color)
      "sharing the extract highlight's colour would make the two indistinguishable")))

;; ── mark-chapters returns only what changed ────────────────────────

(deftest marks-only-the-chapters-that-received-a-placement
  (let [chapters [{:title "A" :html "<p>alpha</p>"}
                  {:title "B" :html "<p>beta</p>"}
                  {:title "C" :html "<p>gamma</p>"}]]

    (testing "an untouched chapter is absent from the result"
      (let [out (marked chapters [(hl "B" "beta")])]
        (is (= [1] (vec (keys out))))
        (is (nil? (get out 0)) "chapter A is not re-serialized")
        (is (nil? (get out 2)) "chapter C is not re-serialized")))

    (testing "no placements means no chapter is rewritten"
      (is (= {} (marked chapters [(hl "B" "not present anywhere")]))))

    (testing "two chapters each holding a placement both come back"
      (let [out (marked chapters [(hl "A" "alpha" 0) (hl "C" "gamma" 1)])]
        (is (= #{0 2} (set (keys out))))
        (is (= ["alpha"] (tinted-text (get out 0))))
        (is (= ["gamma"] (tinted-text (get out 2))))))))

;; ── Degenerate input ───────────────────────────────────────────────

(deftest handles-degenerate-input
  (testing "an empty chapter list leaves every highlight unmatched"
    (is (= [:unmatched] (outcomes [] [(hl "C" "anything")]))))

  (testing "a blank quote places nothing, since it would match everywhere"
    (is (= [:unmatched] (outcomes [{:title "C" :html "<p>text</p>"}] [(hl "C" "   ")]))))

  (testing "a chapter with no HTML reads as empty and matches nothing"
    (is (= "" (anchor/chapter-text nil)))
    (is (= [:unmatched] (outcomes [{:title "C" :html nil}] [(hl "C" "text")]))))

  (testing "no highlights yields no placements and no rewritten chapters"
    (is (= [] (anchor/place-highlights [{:title "C" :html "<p>x</p>"}] [])))
    (is (= {} (marked [{:title "C" :html "<p>x</p>"}] []))))

  (testing "a nil chapter heading takes the forward search"
    (let [ps (anchor/place-highlights [{:title "C" :html "<p>findme</p>"}]
               [(hl nil "findme")])]
      (is (= [:fallback] (mapv :outcome ps)))))

  (testing "placements echo the highlight's own :index"
    (let [ps (anchor/place-highlights [{:title "C" :html "<p>x</p>"}]
               [(hl "C" "x" 41) (hl "C" "absent" 42)])]
      (is (= [41 42] (mapv :index ps))
        "including the unmatched one, so a caller can join back by index"))))
