(ns freememo.math-test
  "Covers freememo.math's pure conversions: the Anki boundary round-trip, the
   cloze brace spacing that keeps TeX from closing a deletion, and the escaping
   fixtures — TeX containing <, > or & is where a silent, meaning-changing
   corruption is most likely, since every stored formula passes through Jsoup
   (clean-html) and back.

   The DOM-based editor-boundary fns (stored->editor-html, editor->stored-html)
   are CLJS-only and not covered here; they are verified in a real browser."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.math :as math]
            [freememo.cloze :as cloze]
            [freememo.html-cleaner :as cleaner]
            [freememo.text :as text]))

;; ── strip-tex ───────────────────────────────────────────────────────────────

(deftest t1-strip-tex-removes-regions
  (is (= "a  b" (math/strip-tex "a \\(x^2\\) b"))))

(deftest t2-strip-tex-multiple-and-nil
  (testing "every region goes; nil is \"\", not a crash"
    (is (= "ab" (math/strip-tex "a\\(x\\)\\(y\\)b")))
    (is (= "" (math/strip-tex nil)))))

(deftest t3-strip-tex-leaves-unpaired-delimiter
  (testing "an unclosed \\( is not a region and must survive untouched"
    (is (= "a \\(x" (math/strip-tex "a \\(x")))))

;; ── Cloze × TeX braces ──────────────────────────────────────────────────────

(deftest t4-cloze-validate-accepts-formula-in-deletion
  (testing "TeX }} inside a deletion is not counted as the cloze closer"
    (is (nil? (cloze/validate "a {{c1::\\(x^{a^{b}}\\)}} b")))))

(deftest t5-cloze-validate-still-catches-real-imbalance
  (testing "stripping math must not mask a genuinely unclosed cloze"
    (is (= "Unclosed cloze: a {{cN::...}} is missing its closing }}"
          (cloze/validate "{{c1::\\(x^2\\)")))))

(deftest t6-space-tex-close-braces-leaves-no-double
  (let [out (math/space-tex-close-braces "\\(x^{a^{b^{c}}}\\)")]
    (is (not (re-find #"\}\}" out)) out)))

(deftest t7-space-tex-close-braces-only-inside-math
  (testing "a cloze's own }} is outside the math region and must not be spaced"
    (is (= "{{c1::\\(x^{a^{b} }\\)}}"
          (math/space-tex-close-braces "{{c1::\\(x^{a^{b}}\\)}}")))))

(deftest t8-space-tex-close-braces-idempotent
  (testing "the anki-overlay diff applies this to the local side on every pass"
    (let [once (math/space-tex-close-braces "\\(x^{a^{b^{c}}}\\)")]
      (is (= once (math/space-tex-close-braces once))))))

;; ── Anki boundary ───────────────────────────────────────────────────────────

(deftest t9-stored-to-anki
  (is (= "<p><anki-mathjax>x \\times y</anki-mathjax></p>"
        (math/stored->anki-html "<p>\\(x \\times y\\)</p>" "basic"))))

(deftest t10-anki-to-stored
  (is (= "<p>\\(x \\times y\\)</p>"
        (math/anki->stored-html "<p><anki-mathjax>x \\times y</anki-mathjax></p>"))))

(deftest t11-anki-round-trip
  (doseq [html ["<p>\\(x^2\\)</p>"
                "before \\(a\\) middle \\(b\\) after"
                "<p>no math at all</p>"
                ""]]
    (testing html
      (is (= html (-> (math/stored->anki-html html "basic") math/anki->stored-html))))))

(deftest t12-anki-mathjax-with-attributes
  (testing "a block flag written by Anki is dropped, not left in the TeX"
    (is (= "\\(x\\)" (math/anki->stored-html "<anki-mathjax block=\"true\">x</anki-mathjax>")))))

(deftest t13-nil-inputs
  (is (= "" (math/stored->anki-html nil "basic")))
  (is (= "" (math/anki->stored-html nil)))
  (is (= "" (math/space-tex-close-braces nil)))
  (is (= "" (math/stored->compare-html nil "cloze"))))

;; ── Push and compare must agree on which kinds get brace spacing ────────────
;; Regression: spacing on push but unconditionally when comparing made every
;; BASIC card with nested-brace TeX read as Anki-modified forever — and
;; library_cards treats anki-modified rows as delete-danger.

(deftest t19-basic-is-not-brace-spaced
  (testing "basic fields have no cloze semantics, so their TeX is untouched"
    (is (= "<anki-mathjax>x^{a^{b}}</anki-mathjax>"
          (math/stored->anki-html "\\(x^{a^{b}}\\)" "basic")))))

(deftest t20-cloze-kinds-are-brace-spaced
  (doseq [kind ["cloze" "overlapping"]]
    (testing kind
      (is (= "<anki-mathjax>x^{a^{b} }</anki-mathjax>"
            (math/stored->anki-html "\\(x^{a^{b}}\\)" kind))))))

(deftest t21-compare-form-matches-round-tripped-anki-form
  (testing "every kind: local compare form ≡ what comes back from Anki"
    (doseq [kind ["basic" "cloze" "overlapping" "occlusion" "score"]]
      (let [local "\\(x^{a^{b}}\\)"
            from-anki (-> (math/stored->anki-html local kind) math/anki->stored-html)]
        (is (= from-anki (math/stored->compare-html local kind)) kind)))))

(deftest t22-cloze-bearing-predicate
  (is (true? (math/cloze-bearing-kind? "cloze")))
  (is (true? (math/cloze-bearing-kind? "overlapping")))
  (doseq [k ["basic" "occlusion" "score" nil]]
    (is (false? (math/cloze-bearing-kind? k)) (str k))))

;; ── Escaping: the highest-risk path (spec §2.2) ──────────────────────────────

(def ^:private escape-fixtures
  ["<p>\\(a &lt; b\\)</p>"
   "<p>\\(a &gt; b\\)</p>"
   "<p>\\(f &amp; g\\)</p>"
   "<p>\\(\\begin{align} a &amp;= b \\end{align}\\)</p>"])

(deftest t14-escaped-tex-survives-anki-round-trip
  (testing "both forms carry TeX as HTML text, so entities pass through unchanged"
    (doseq [html escape-fixtures]
      (is (= html (-> (math/stored->anki-html html "basic") math/anki->stored-html)) html))))

(deftest t15-escaped-tex-survives-clean-html
  (testing "clean-html must not alter a stored formula (spec §8.2)"
    (doseq [html escape-fixtures]
      (let [cleaned (cleaner/clean-html html)]
        (is (= html cleaned) (str "in: " html " out: " cleaned))))))

(deftest t16-clean-html-preserves-plain-math
  (testing "the whole point of the delimiter form: no allow-list entry needed"
    (is (= "<p>\\(x \\times (y+z)\\)</p>"
          (cleaner/clean-html "<p>\\(x \\times (y+z)\\)</p>")))))

(deftest t17-strip-html-keeps-math-for-search
  (testing "content_text feeds search snippets, so the TeX must survive stripping"
    (let [txt (text/strip-html "<p>The identity \\(x \\times y\\) matters</p>")]
      (is (= "The identity \\(x \\times y\\) matters" txt)))))

(deftest t18-strip-html-decodes-entities-in-math
  (testing "strip-html decodes, so search matches the literal TeX the user typed"
    (is (= "\\(a < b\\)" (text/strip-html "<p>\\(a &lt; b\\)</p>")))))
