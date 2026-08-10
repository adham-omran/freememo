(ns freememo.doc-html-test
  "Asserts freememo.doc-html rewrites every documentation construct Quill's
   Delta cannot hold into one it can, and leaves everything else alone.

   Why fixtures and not generation: the losing side is Quill 2.0.3 in a browser
   and the rewriting side is Jsoup on the JVM, so the coupling cannot be a
   shared constant and cannot be driven from here. Each fixture is therefore
   hand-authored from real Sphinx output, and the browser half is checked by
   `playwright/probe-doc-html-render.mjs`, which loads this namespace's real
   output into a real Quill and proves the round trip is a fixed point.

   Needs no database — this namespace requires only clojure.string and Jsoup.

   Motivating regression: a 2361-page Blender manual imported with every count
   correct and every page rendering flat, because the verification counted rows
   and links and never compared a page against the page it came from."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.doc-html :as doc-html]
            [freememo.html-cleaner :as cleaner]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- rewrite
  "The rewrite alone, without the sanitizer the importers apply after it."
  [html]
  (doc-html/doc-html->quill-html html))

(defn- through-pipeline
  "The full importer order: rewrite, sanitize, then drop the spans the
   sanitizer just made bare. This is what reaches `topics.content`."
  [html]
  (-> html rewrite cleaner/clean-html doc-html/drop-bare-spans))

(defn- tag-count
  [html tag]
  (count (re-seq (re-pattern (str "<" tag "\\b")) html)))

;; ---------------------------------------------------------------------------
;; 1.4.1 — admonitions
;; ---------------------------------------------------------------------------

(def ^:private admonition-html
  (str "<div class=\"admonition note\">"
    "<p class=\"admonition-title\">Note</p>"
    "<p>Recovered files might need to be reloaded.</p>"
    "</div>"))

(deftest admonition-becomes-a-blockquote-with-a-bold-title
  (let [out (rewrite admonition-html)]
    (is (= 1 (tag-count out "blockquote")))
    (is (zero? (tag-count out "div")))
    (is (str/includes? out "<strong>Note</strong>")
      "the title line carries the only bold in the box")
    (is (str/includes? out "Recovered files might need to be reloaded.")
      "the body text is untouched")))

(deftest admonition-survives-the-sanitizer
  (is (= 1 (tag-count (through-pipeline admonition-html) "blockquote"))
    "blockquote is already in clean-html's safelist, so no allow-list change"))

(deftest version-and-deprecation-directives-are-boxed-too
  (testing "Sphinx emits these WITHOUT the admonition class"
    (doseq [cls ["versionadded" "versionchanged" "versionremoved" "deprecated"]]
      (let [out (rewrite (str "<div class=\"" cls "\"><p>"
                           "<span class=\"versionmodified\">New in version 4.2:</span>"
                           " It does the thing.</p></div>"))]
        (is (= 1 (tag-count out "blockquote")) (str cls " must become a blockquote"))
        (is (zero? (tag-count out "div")) (str cls " must not stay a div"))))))

(deftest topic-and-sidebar-are-deliberately-not-boxed
  (testing "they label with p.topic-title / p.sidebar-title, which nothing reads —
            admitting them would give a quote whose heading reads as body text.
            Add them WITH their title selector or not at all."
    (doseq [cls ["topic" "sidebar"]]
      (let [out (rewrite (str "<div class=\"" cls "\">"
                           "<p class=\"" cls "-title\">Heading</p>"
                           "<p>Body.</p></div>"))]
        (is (zero? (tag-count out "blockquote")) (str cls " must stay unboxed"))
        (is (= 1 (tag-count out "div")) (str cls " must stay a div"))))))

(deftest a-version-label-is-bolded-like-an-admonition-title
  (is (str/includes?
        (rewrite (str "<div class=\"versionadded\"><p>"
                   "<span class=\"versionmodified\">New in version 4.2:</span>"
                   " Detail.</p></div>"))
        "<strong>New in version 4.2:</strong>")
    "the label line carries the bold, matching p.admonition-title"))

(deftest a-bare-layout-div-is-not-an-admonition
  (testing "SuperMemo emits 10391 attribute-less divs; none may become a quote"
    (let [out (rewrite "<div><p>Ordinary prose in a layout wrapper.</p></div>")]
      (is (zero? (tag-count out "blockquote")))
      (is (= 1 (tag-count out "div")))))
  (testing "a div whose first p merely reads like a title is still a div"
    (let [out (rewrite "<div><p>Note</p><p>Not a Sphinx admonition.</p></div>")]
      (is (zero? (tag-count out "blockquote"))
        "selection is by class, never by title text"))))

;; ---------------------------------------------------------------------------
;; 1.4.2 / 1.5.1 — definition lists
;; ---------------------------------------------------------------------------

(def ^:private definition-list-html
  (str "<dl><dt>Splash Screen</dt>"
    "<dd><p>Open the Splash Screen.</p></dd>"
    "<dt>About Blender</dt>"
    "<dd><p>Shows this information:</p>"
    "<ul><li><p><strong>Version</strong>: The Blender version.</p></li>"
    "<li><p><strong>Date</strong>: When it was compiled.</p></li></ul></dd></dl>"))

(deftest definition-list-flattens-to-terms-and-indented-bodies
  (let [out (rewrite definition-list-html)]
    (doseq [tag ["dl" "dt" "dd"]]
      (is (zero? (tag-count out tag)) (str "<" tag "> must not survive")))
    (is (str/includes? out "<p><strong>Splash Screen</strong></p>")
      "a term becomes a bold paragraph")
    (is (str/includes? out "<p class=\"ql-indent-1\">Open the Splash Screen.</p>")
      "a body block carries the indent")))

(deftest a-list-inside-a-body-indents-its-items-not-itself
  (testing "Quill's indent is a per-line format and a list's lines are its items"
    (let [out (rewrite definition-list-html)]
      (is (= 2 (count (re-seq #"<li class=\"ql-indent-1\">" out))))
      (is (not (re-find #"<ul class=\"ql-indent-1\"" out))
        "an indent on the <ul> itself would be dropped on load"))))

(deftest a-body-block-that-is-not-a-line-gets-no-indent
  (testing "a class clean-html strips is a request the caller never learns failed"
    (doseq [[tag html] [["hr"    "<dl><dt>T</dt><dd><p>a</p><hr></dd></dl>"]
                        ["table" (str "<dl><dt>T</dt><dd>"
                                   "<table><tr><td>x</td></tr></table></dd></dl>")]]]
      (let [out (rewrite html)]
        (is (not (re-find (re-pattern (str "<" tag "[^>]*ql-indent")) out))
          (str "<" tag "> must not be given an indent it cannot carry"))))))

(deftest only-one-indent-level-is-ever-emitted
  (testing "1.5.1 ships flat; per-depth indent is 1.5.3 and deliberately unbuilt"
    (let [out (rewrite (str "<dl><dt>Outer</dt><dd><p>Body.</p>"
                         "<dl><dt>Inner</dt><dd><p>Inner body.</p></dd></dl>"
                         "</dd></dl>"))]
      (is (= #{"ql-indent-1"} (set (re-seq #"ql-indent-\d" out)))
        "no ql-indent-2 and no conflicting pair of classes"))))

(deftest a-nested-term-inherits-its-parent-body-indent
  (testing "deepest-first rewriting is what stops a sub-term reading as a sibling"
    (let [out (rewrite (str "<dl><dt>Outer</dt><dd><p>Body.</p>"
                         "<dl><dt>Inner</dt><dd><p>Inner body.</p></dd></dl>"
                         "</dd></dl>"))]
      (is (str/includes? out "<p class=\"ql-indent-1\"><strong>Inner</strong></p>")
        "the depth-2 term is indented, not flush with the depth-1 term")
      (is (str/includes? out "<p><strong>Outer</strong></p>")
        "the depth-1 term stays flush"))))

(deftest loose-body-content-is-wrapped-not-nested
  (testing "a theme may put bare text in a <dd>; no block may land inside a <p>"
    (let [out (rewrite "<dl><dt>T</dt><dd>Bare text<ul><li>Item</li></ul></dd></dl>")]
      (is (str/includes? out "<p class=\"ql-indent-1\">Bare text</p>"))
      (is (not (re-find #"<p[^>]*>[^<]*<ul" out))
        "the list must not be nested inside the synthetic paragraph"))))

(deftest a-foreign-child-of-a-dl-is-carried-across
  (testing "losing markup silently is not acceptable, whatever a theme emits"
    (let [out (rewrite "<dl><dt>T</dt><dd><p>B</p></dd><span>stray</span></dl>")]
      (is (str/includes? out "stray")))))

;; ---------------------------------------------------------------------------
;; 1.4.3 — keyboard keys
;; ---------------------------------------------------------------------------

(deftest keyboard-keys-become-inline-code
  (let [html (str "<dl><dt>New <kbd class=\"kbd docutils\">Ctrl</kbd>-"
               "<kbd class=\"kbd docutils\">N</kbd></dt>"
               "<dd><p>Clears the scene.</p></dd></dl>")
        out (rewrite html)]
    (is (zero? (tag-count out "kbd")))
    (is (= 2 (tag-count out "code")))
    (is (str/includes? out "<strong>New <code")
      "the term's bold run wraps the keys, so keys must convert first")))

(deftest keyboard-keys-survive-the-sanitizer
  (is (= 2 (tag-count (through-pipeline
                        "<p>Press <kbd>Ctrl</kbd>-<kbd>N</kbd></p>") "code"))
    "clean-html has code in its safelist but not kbd"))

;; ---------------------------------------------------------------------------
;; 1.4.4 — figures
;; ---------------------------------------------------------------------------

(deftest figure-becomes-a-paragraph-and-keeps-its-caption
  (let [out (rewrite (str "<figure class=\"align-default\">"
                       "<img alt=\"a\" src=\"/api/media/1\">"
                       "<figcaption>The topbar</figcaption></figure>"))]
    (is (zero? (tag-count out "figure")))
    (is (zero? (tag-count out "figcaption")))
    (is (= 1 (tag-count out "p")))
    (is (str/includes? out "<em>The topbar</em>")
      "the caption survives as emphasis rather than vanishing")
    (is (str/includes? out "src=\"/api/media/1\""))))

(deftest a-figure-holding-a-block-unwraps-instead-of-retagging
  (testing "retagging would emit <p><table>…</table></p>, which parsers split"
    (let [out (rewrite (str "<figure><table><tr><td>cell</td></tr></table>"
                         "<figcaption>Caption</figcaption></figure>"))]
      (is (zero? (tag-count out "figure")))
      (is (= 1 (tag-count out "table")))
      (is (not (re-find #"<p[^>]*>\s*<table" out))
        "the table must not be nested inside a paragraph")
      (is (str/includes? out "<em>Caption</em>")))))

(deftest an-image-figure-in-a-definition-body-keeps-the-indent
  (testing "figures flatten BEFORE definition lists, so the indent lands on a
            block that survives to clean-html"
    (let [out (through-pipeline
                "<dl><dt>T</dt><dd><figure><img src=\"/api/media/1\"></figure></dd></dl>")]
      (is (str/includes? out "ql-indent-1")
        "the promoted paragraph must carry the body indent")
      (is (str/includes? out "src=\"/api/media/1\"")))))

(deftest a-block-figure-in-a-definition-body-does-not-lose-the-indent-silently
  (testing "regression: unwrapping the figure AFTER indenting it discarded the
            class with the element and dropped the body to the left margin"
    (let [out (through-pipeline
                (str "<dl><dt>T</dt><dd><figure>"
                  "<table><tr><td>cell</td></tr></table></figure></dd></dl>"))]
      (is (= 1 (tag-count out "table")))
      (is (not (re-find #"<p[^>]*>\s*<table" out))
        "still no table inside a paragraph")
      (is (zero? (tag-count out "figure"))))))

;; ---------------------------------------------------------------------------
;; 1.4.5 — whitespace
;; ---------------------------------------------------------------------------

(deftest a-term-written-across-lines-loses-its-flanking-spaces
  (testing "the bug dropping blank nodes alone does not fix"
    (let [out (rewrite "<dl>\n <dt>\n  Splash Screen\n </dt>\n <dd>\n  <p>Body.</p>\n </dd>\n</dl>")]
      (is (str/includes? out "<strong>Splash Screen</strong>"))
      (is (not (re-find #"<strong>\s|\s</strong>" out))
        ".ql-editor is pre-wrap, so a flanking space is visible"))))

(deftest inner-whitespace-collapses-to-one-space
  (is (= "<p>a b c</p>" (rewrite "<p>a    b\n\n  c</p>"))))

(deftest whitespace-between-inline-siblings-is-kept
  (testing "trimming happens only where a text node touches its parent boundary"
    (is (= "<p>a <strong>b</strong> c</p>"
          (rewrite "<p>a <strong>b</strong> c</p>")))))

(deftest preformatted-whitespace-is-untouched
  (doseq [html ["<pre>  indented\n    more</pre>"
                "<p><code>  spaced  </code></p>"
                (str "<div class=\"ql-code-block-container\">"
                  "<div class=\"ql-code-block\">  (inc 1)</div></div>")]]
    (is (str/includes? (rewrite html) "  ")
      (str "significant whitespace lost in: " html))))

;; ---------------------------------------------------------------------------
;; 1.4.6 — bare spans, after the sanitizer
;; ---------------------------------------------------------------------------

(deftest bare-spans-unwrap-and-attributed-ones-do-not
  (is (= "<p>text</p>" (doc-html/drop-bare-spans "<p><span>text</span></p>")))
  (is (str/includes?
        (doc-html/drop-bare-spans "<p><span style=\"color: #e03e3e\">a</span></p>")
        "color: #e03e3e")
    "a span carrying a surviving format must not be unwrapped"))

(deftest a-sphinx-reference-span-only-goes-bare-after-sanitizing
  (let [html "<p><a href=\"/viewer/topic/1\"><span class=\"std std-ref\">Link</span></a></p>"]
    (is (str/includes? (doc-html/drop-bare-spans html) "std-ref")
      "before clean-html the class is still there, so this is a no-op")
    (is (zero? (tag-count (through-pipeline html) "span"))
      "in importer order the sanitizer strips the class first")))

;; ---------------------------------------------------------------------------
;; Whole-construct invariants
;; ---------------------------------------------------------------------------

(deftest hr-is-left-for-the-blot-to-carry
  (testing "the one construct this seam cannot answer"
    (is (= 1 (tag-count (through-pipeline "<p>a</p><hr class=\"docutils\"><p>b</p>") "hr"))
      "hr must reach the database intact; quill-field's divider blot renders it")))

(deftest no-target-construct-survives-a-full-page
  (let [page (str "<h1>Topbar</h1>"
               "<figure><img src=\"/api/media/1\" alt=\"a\"></figure>"
               definition-list-html
               admonition-html
               "<hr class=\"docutils\">"
               "<dl><dt>Quit <kbd>Ctrl</kbd>-<kbd>Q</kbd></dt>"
               "<dd><p>Closes Blender.</p></dd></dl>")
        out (through-pipeline page)]
    (doseq [tag ["dl" "dt" "dd" "kbd" "figure" "figcaption"]]
      (is (zero? (tag-count out tag)) (str "<" tag "> survived a full page")))
    (is (= 1 (tag-count out "hr")))
    (is (= 1 (tag-count out "blockquote")))
    (is (= 2 (tag-count out "code")))))

(deftest the-rewrite-is-idempotent
  (testing "a second pass must change nothing, so a re-run cannot corrupt a row"
    (let [once (rewrite definition-list-html)]
      (is (= once (rewrite once))))))

(deftest counts-name-every-rewrite
  (let [page (str definition-list-html admonition-html
               "<figure><img src=\"/api/media/1\" alt=\"a\"></figure>"
               "<p>Press <kbd>Ctrl</kbd>-<kbd>N</kbd></p>")
        doc (org.jsoup.Jsoup/parseBodyFragment page)
        counts (doc-html/rewrite-for-quill! (.body doc))]
    (is (= {:admonitions-to-blockquotes 1
            :keys-to-code 2
            :terms-flattened 2
            :definition-bodies-flattened 2
            :figures-flattened 1}
          (dissoc counts :whitespace-nodes-dropped))
      "sphinx-import folds these into its report, which claims nothing is
       discarded silently")
    (is (contains? counts :whitespace-nodes-dropped))))

(deftest nil-passes-through
  (is (nil? (doc-html/doc-html->quill-html nil)))
  (is (nil? (doc-html/drop-bare-spans nil))))
