(ns freememo.markdown-test
  "Covers freememo.markdown's `$` interpretation and the math-protecting
   renderer the Mistral OCR lane runs on.

   Two failures motivate the file, and neither raises anything.

   `dollar-math->tex` decides what a `$` means. Read it as math and a price list
   turns into italic notation; read it as currency and every formula stays
   literal text. The currency fixtures below are the boundary.

   `markdown-with-math->html` exists because flexmark implements CommonMark
   backslash escapes and so eats `\\,` `\\\\` `\\{` `\\}` `\\|` `\\%` — while
   leaving `\\frac` and `\\sqrt` intact, which is what makes the damage easy to
   miss. `destroyed-by-flexmark` is the regression guard: every fixture there
   renders wrongly, or not at all, if the protection is removed.

   Needs no database."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [freememo.markdown :as markdown]
            [freememo.html-cleaner :as cleaner]))

;; ── dollar-math->tex: currency vs math ──────────────────────────────────────

(def ^:private currency-fixtures
  "Text a `$` pass must leave completely alone. Each is prose a textbook or a
   price table really contains."
  ["<p>Costs $5 and $10 today.</p>"
   "<p>Revenue was $6,245 in Q1.</p>"
   "<p>Between $10 and $20 per unit.</p>"
   "<p>Rose from $5 to $8, a jump.</p>"
   "<p>Priced $5-$10 each.</p>"
   "<p>$1.5M vs $2.5M revenue.</p>"
   "<p>Let $p = $10 be the price.</p>"])

(deftest t1-currency-is-not-math
  (doseq [html currency-fixtures]
    (is (= html (markdown/dollar-math->tex html)) html)))

(deftest t2-real-math-is-converted
  (is (= "<p>Given \\(x^2 + 1\\) we get \\(x \\geq 0\\).</p>"
        (markdown/dollar-math->tex "<p>Given $x^2 + 1$ we get $x \\geq 0$.</p>"))))

(deftest t3-display-becomes-bracket-form
  (testing "the downgrade to inline is math/display->inline's job, not this one's"
    (is (= "<p>\\[\\int_0^1 x^2\\,dx\\]</p>"
          (markdown/dollar-math->tex "<p>$$\\int_0^1 x^2\\,dx$$</p>")))))

(deftest t4-math-and-currency-in-one-line
  (is (= "<p>The set \\(S\\) costs $12.</p>"
        (markdown/dollar-math->tex "<p>The set $S$ costs $12.</p>")))
  (is (= "<p>A price of $100 and math \\(y_i\\) mixed.</p>"
        (markdown/dollar-math->tex "<p>A price of $100 and math $y_i$ mixed.</p>"))))

(deftest t5-code-is-excluded
  (testing "a shell variable is not a formula"
    (doseq [html ["<pre><code>echo $HOME $PATH</code></pre>"
                  "<p>Use <code>$x$</code> inline.</p>"]]
      (is (= html (markdown/dollar-math->tex html)) html))))

(deftest t6-dollar-pass-is-idempotent
  (let [once (markdown/dollar-math->tex "<p>Given $x^2$ and $$y$$.</p>")]
    (is (= once (markdown/dollar-math->tex once)))))

(deftest t7-nil-in-nil-out
  (is (nil? (markdown/dollar-math->tex nil))))

;; ── unwrap-non-math-dollars ─────────────────────────────────────────────────

(deftest t8-plain-numbers-are-unwrapped
  (testing "$6,245$ is a writer's habit; as math it mis-spaces to \"6, 245\""
    (is (= "The total was 6,245 dollars."
          (markdown/unwrap-non-math-dollars "The total was $6,245$ dollars.")))
    (is (= "We have $x$ and 42 and 3.5%."
          (markdown/unwrap-non-math-dollars "We have $x$ and $42$ and $3.5%$.")))))

;; ── markdown-with-math->html: the flexmark protection ───────────────────────

(def ^:private destroyed-by-flexmark
  "TeX that CommonMark backslash-escaping strips. Left is the source formula;
   the second element names what breaks when it is unprotected."
  [["\\int_0^1 f(x)\\,dx"                          "\\, thin space"]
   ["\\begin{matrix} a & b \\\\ c & d \\end{matrix}" "\\\\ row separator"]
   ["\\left\\{ x \\mid x > 0 \\right\\}"            "\\{ \\} delimiters"]
   ["\\|v\\|_2 = \\sqrt{\\sum_i v_i^2}"             "\\| norm bars"]
   ["P(A \\cap B) = 30\\%"                          "\\% (bare % is a KaTeX comment)"]])

(defn- sole-tex
  "The one TeX body in `html`, with HTML entities decoded — what KaTeX will be
   handed. Fails loudly rather than returning nil, so a missing region reads as
   a test error and not as a passing comparison against nothing."
  [html]
  (let [m (re-find #"\\[\(\[]([\s\S]*?)\\[\)\]]" (str html))]
    (assert m (str "no math region in: " (pr-str html)))
    (-> (second m)
      (str/replace "&lt;" "<")
      (str/replace "&gt;" ">")
      (str/replace "&amp;" "&"))))

(deftest t9-display-math-survives-flexmark
  (doseq [[tex what] destroyed-by-flexmark]
    (is (= tex (sole-tex (markdown/markdown-with-math->html (str "$$" tex "$$"))))
      what)))

(deftest t10-inline-math-survives-flexmark
  (doseq [[tex what] destroyed-by-flexmark]
    (is (= tex (sole-tex (markdown/markdown-with-math->html (str "text $" tex "$ text"))))
      what)))

(deftest t11-tex-delimiters-in-the-source-survive
  (testing "a model that already emits \\(…\\) is destroyed identically by flexmark"
    (doseq [[tex what] destroyed-by-flexmark]
      (is (= tex (sole-tex (markdown/markdown-with-math->html (str "\\(" tex "\\)")))) what)
      (is (= tex (sole-tex (markdown/markdown-with-math->html (str "\\[" tex "\\]")))) what))))

(deftest t12-region-kind-is-preserved
  (testing "display stays \\[…\\] here; math/display->inline owns the downgrade"
    (is (str/includes? (markdown/markdown-with-math->html "$$x^2$$") "\\[x^2\\]"))
    (is (str/includes? (markdown/markdown-with-math->html "a $x^2$ b") "\\(x^2\\)"))))

(deftest t13-tex-is-html-escaped
  (testing "the TeX bypassed flexmark's escaping, so an unescaped < corrupts the DOM"
    (let [html (markdown/markdown-with-math->html "$$a < b \\& c$$")]
      (is (str/includes? html "&lt;"))
      (is (str/includes? html "&amp;"))
      (is (not (str/includes? html "<p>a <")))
      (testing "and survives the Jsoup pass that follows in run-ocr"
        (is (= "a < b \\& c" (sole-tex (cleaner/clean-html html))))))))

(deftest t14-markdown-structure-still-renders
  (let [html (markdown/markdown-with-math->html
               "## Section 2.1\n\nFor $n \\geq 1$:\n\n- first\n- second\n\n| a | b |\n|---|---|\n| $x_1$ | 2 |\n")]
    (is (str/includes? html "<h2>Section 2.1</h2>"))
    (is (str/includes? html "<li>first</li>"))
    (is (str/includes? html "<table>"))
    (is (str/includes? html "\\(n \\geq 1\\)"))
    (is (str/includes? html "\\(x_1\\)"))))

(deftest t15-region-count-is-preserved
  (testing "the placeholder invariant: one region out per region in"
    (let [html (markdown/markdown-with-math->html
                 "$a$ then $$b$$ then \\(c\\) then \\[d\\] and $12 in cash.")]
      (is (= 4 (count (re-seq #"\\[\(\[]" html))))
      (is (str/includes? html "$12 in cash")))))

(deftest t16-currency-still-survives-the-renderer
  (is (str/includes? (markdown/markdown-with-math->html "Between $10 and $20 per unit.")
        "Between $10 and $20 per unit.")))

(deftest t17-blank-in-nil-out
  (testing "run-ocr's str/blank? guard turns this into \"Empty OCR response\""
    (is (nil? (markdown/markdown-with-math->html nil)))
    (is (nil? (markdown/markdown-with-math->html "")))))

(deftest t18-placeholder-is-not-mistaken-for-content
  (testing "text that merely looks like the sentinel must not become math"
    (let [html (markdown/markdown-with-math->html "Serial QQMATHQQ0QQ shipped.")]
      (is (str/includes? html "QQMATHQQ0QQ"))
      (is (not (str/includes? html "\\("))))))

(deftest t19-sentinel-collision-does-not-forge-a-region
  (testing "source carrying BOTH real math and the literal sentinel: a fixed
            prefix would rewrite the source's own text into a second region"
    (let [html (markdown/markdown-with-math->html "Math $x^2$ and serial QQMATHQQ0QQ here.")]
      (is (= 1 (count (re-seq #"\\\(" html))))
      (is (str/includes? html "\\(x^2\\)"))
      (is (str/includes? html "QQMATHQQ0QQ")))))
