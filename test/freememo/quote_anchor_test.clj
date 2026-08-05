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

(defn- place
  "Index `chapters`, anchor `highlights`, return [indexed placements]."
  [chapters highlights]
  (let [indexed (anchor/index-chapters chapters)]
    [indexed (anchor/anchor-highlights indexed highlights)]))

(defn- hl
  ([chapter quote] (hl chapter quote 0))
  ([chapter quote index] {:chapter chapter :quote quote :notes [] :index index}))

(defn- marked-html
  "Anchor every placement for chapter 0, tint it, and return the HTML."
  [chapters highlights]
  (let [[indexed placements] (place chapters highlights)
        ranges (keep (fn [p] (when (= 0 (:chapter-pos p)) [(:start p) (:end p)]))
                 placements)]
    (anchor/mark-anchors! (first indexed) ranges)
    (anchor/chapter-html (first indexed))))

(defn- tinted-text
  "The text inside every tint span of `html`, in document order."
  [html]
  (->> (.select (Jsoup/parseBodyFragment html)
         (str "span[style*=" (subs anchor/highlight-color 1) "]"))
    (mapv #(.text ^org.jsoup.nodes.Element %))))

;; ── 4.3 — the export's own damage must match ───────────────────────

(deftest normalization-absorbs-the-exports-damage
  (testing "4.3 — `curse of knowledgeis` matches a source that has the space"
    ;; The source italicizes the term, so NeoReader joined two inline elements
    ;; with no separator and lost the space before `is`.
    (let [chapters [{:title "5. Avoid Illusions of Knowing"
                     :html (str "<p>The <i>curse of knowledge</i> is our tendency to "
                             "underestimate how long it will take another person.</p>")}]
          [_ ps] (place chapters
                   [(hl "5. Avoid Illusions of Knowing"
                      "curse of knowledgeis our tendency to underestimate")])]
      (is (= :placed (:outcome (first ps))))))

  (testing "4.3 — `hindsight bias,or` matches the same way"
    (let [chapters [{:title "C" :html "<p>the <i>hindsight bias,</i> or what is often called</p>"}]
          [_ ps] (place chapters [(hl "C" "hindsight bias,or what is often called")])]
      (is (= :placed (:outcome (first ps))))))

  (testing "4.3 — curly quotation marks fold to their ASCII form"
    (let [chapters [{:title "C" :html "<p>and it says \"practice like you play\" out loud</p>"}]
          [_ ps] (place chapters
                   [(hl "C" (str "and it says " \u201c "practice like you play" \u201d
                              " out loud"))])]
      (is (= :placed (:outcome (first ps))))))

  (testing "4.3 — normalize drops whitespace and leaves nothing curly"
    (let [n (anchor/normalize (str "a b\t c\n" \u2018 "d" \u2019))]
      (is (= "abc'd'" n) "space, tab and newline all go; the curly pair folds")
      (is (not (str/includes? n " ")))))

  (testing "4.3 — normalize of nil is the empty string, not a crash"
    (is (= "" (anchor/normalize nil)))))

;; ── 4.4 — a quote that spans element boundaries ────────────────────

(deftest matches-a-quote-that-crosses-elements
  (let [chapters [{:title "8. Make It Stick"
                   :html (str "<h2>Generation</h2>"
                           "<p>In 2013, John McPhee published a piece in the New Yorker "
                           "about writer's block.</p>")}]
        quote-text "Generation In 2013, John McPhee published a piece in the New Yorker"
        highlights [(hl "8. Make It Stick" quote-text)]]

    (testing "4.4 — the highlight places even though it crosses a heading"
      (let [[_ ps] (place chapters highlights)]
        (is (= :placed (:outcome (first ps))))))

    (testing "4.4 — the mark covers both elements"
      (let [html (marked-html chapters highlights)
            doc (Jsoup/parseBodyFragment html)
            sel (str "span[style*=" (subs anchor/highlight-color 1) "]")]
        (is (some? (.selectFirst doc (str "h2 " sel)))
          "the heading carries a tint span")
        (is (some? (.selectFirst doc (str "p " sel)))
          "the paragraph carries one too")
        (is (str/includes? (.text (.body doc)) "about writer's block.")
          "text outside the quote survives untinted")))))

;; ── 4.5 — the reading-order cursor resolves ambiguity ──────────────

(deftest cursor-resolves-a-repeated-quote
  (testing "4.5 — `reflection` lands after its predecessor, not at the first occurrence"
    (let [chapters [{:title "C"
                     :html (str "<p>A first mention of reflection here.</p>"
                             "<p>Then a marker sentence.</p>"
                             "<p>And daily reflection, as a form of practice.</p>")}]
          [_ ps] (place chapters
                   [(hl "C" "Then a marker sentence" 0)
                    (hl "C" "reflection" 1)])
          second-start (:start (second ps))
          first-end (:end (first ps))]
      (is (= [:placed :placed] (mapv :outcome ps)))
      (is (>= second-start first-end)
        "the second placement starts at or after the end of the first")))

  (testing "4.5 — tinting picks the later occurrence, so the text proves the choice"
    (let [chapters [{:title "C"
                     :html (str "<p>first reflection</p><p>marker</p><p>second reflection</p>")}]
          html (marked-html chapters
                 [(hl "C" "marker" 0) (hl "C" "reflection" 1)])
          doc (Jsoup/parseBodyFragment html)
          ps (.select doc "p")]
      (is (not (str/includes? (.html (.get ps 0)) "background-color"))
        "the first paragraph's `reflection` stays untinted")
      (is (str/includes? (.html (.get ps 2)) "background-color")
        "the third paragraph's `reflection` carries the mark")))

  (testing "4.5 — the cursor does not move when a highlight fails to place"
    (let [chapters [{:title "C" :html "<p>alpha beta gamma</p>"}]
          [_ ps] (place chapters
                   [(hl "C" "absent from the chapter" 0)
                    (hl "C" "alpha" 1)])]
      (is (= [:unmatched :placed] (mapv :outcome ps))
        "a miss must not consume the cursor and block the next quote"))))

;; ── 4.1 and 4.6 — chapter resolution and the forward search ────────

(deftest resolves-chapters-and-falls-back-when-absent
  (let [chapters [{:title "1. Learning Is Misunderstood" :html "<p>alpha one</p>"}
                  {:title "2. To Learn, Retrieve" :html "<p>beta two</p>"}]]

    (testing "4.1 — a heading matching a chapter title places in that chapter"
      (let [[_ ps] (place chapters [(hl "2. To Learn, Retrieve" "beta two")])]
        (is (= :placed (:outcome (first ps))))
        (is (= 1 (:chapter-pos (first ps))))))

    (testing "4.1 — the title match ignores case and whitespace"
      (let [[_ ps] (place chapters [(hl "2.  to learn,   retrieve" "beta two")])]
        (is (= :placed (:outcome (first ps))))
        (is (= 1 (:chapter-pos (first ps))))))

    (testing "4.6 — a heading matching nothing searches the rest of the book"
      (let [[_ ps] (place chapters [(hl "Chapter The Publisher Renamed" "beta two")])]
        (is (= :fallback (:outcome (first ps))))
        (is (= 1 (:chapter-pos (first ps))))))

    (testing "4.6 — a resolved chapter is the only place its highlights may land"
      (let [[_ ps] (place chapters [(hl "1. Learning Is Misunderstood" "beta two")])]
        (is (= :unmatched (:outcome (first ps)))
          "the quote lives in chapter 2, but the export named chapter 1")))

    (testing "4.6 — the forward search respects the cursor's chapter"
      (let [[_ ps] (place chapters
                     [(hl "2. To Learn, Retrieve" "beta" 0)
                      (hl "Renamed" "alpha one" 1)])]
        (is (= :placed (:outcome (first ps))))
        (is (= :unmatched (:outcome (second ps)))
          "chapter 1 lies behind the cursor, so its text is out of reach")))))

;; ── Tinting mechanics ──────────────────────────────────────────────

(deftest tinting-is-exact-and-repeatable
  (testing "the tint covers the quote and nothing else"
    (let [html (marked-html [{:title "C" :html "<p>keep MARKME drop</p>"}]
                 [(hl "C" "MARKME")])]
      (is (= ["MARKME"] (tinted-text html)))))

  (testing "two marks in one text node both land"
    (let [html (marked-html [{:title "C" :html "<p>one AAA two BBB three</p>"}]
                 [(hl "C" "AAA" 0) (hl "C" "BBB" 1)])]
      (is (= ["AAA" "BBB"] (tinted-text html)))))

  (testing "a mark at the very start of a text node needs no leading split"
    (let [html (marked-html [{:title "C" :html "<p>START then rest</p>"}]
                 [(hl "C" "START")])]
      (is (= ["START"] (tinted-text html)))))

  (testing "a mark covering a whole text node needs no split at all"
    (let [html (marked-html [{:title "C" :html "<p>whole</p>"}] [(hl "C" "whole")])]
      (is (= ["whole"] (tinted-text html)))))

  (testing "the colour written is the Quill popover's Orange"
    (is (= "#faebdd" anchor/highlight-color))
    (is (not= "#44C2FF" anchor/highlight-color)
      "sharing the extract highlight's colour would make the two indistinguishable")))

;; ── Degenerate input ───────────────────────────────────────────────

(deftest handles-degenerate-input
  (testing "an empty chapter list leaves every highlight unmatched"
    (let [[_ ps] (place [] [(hl "C" "anything")])]
      (is (= [:unmatched] (mapv :outcome ps)))))

  (testing "a blank quote places nothing, since it would match everywhere"
    (let [[_ ps] (place [{:title "C" :html "<p>text</p>"}] [(hl "C" "   ")])]
      (is (= [:unmatched] (mapv :outcome ps)))))

  (testing "a chapter with no HTML indexes to empty and matches nothing"
    (let [[indexed ps] (place [{:title "C" :html nil}] [(hl "C" "text")])]
      (is (= "" (get-in (first indexed) [:index :text])))
      (is (= [:unmatched] (mapv :outcome ps)))))

  (testing "no highlights yields no placements"
    (let [[_ ps] (place [{:title "C" :html "<p>x</p>"}] [])]
      (is (= [] ps))))

  (testing "the position index maps one entry per normalized character"
    (let [indexed (anchor/index-chapters [{:title "C" :html "<p>a b</p><p>c</p>"}])
          idx (:index (first indexed))]
      (is (= "abc" (:text idx)))
      (is (= 3 (count (:positions idx)))))))
