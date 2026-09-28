(ns freememo.ocr-math-test
  "Covers `freememo.ocr-math/html-math->tex`, the pass that turns math an OCR
   model wrote as HTML into `\\(TeX\\)`. Fixtures come from page 3 of Rosen,
   \"The Economics of Superstars\" (1981), which `gemini-3.5-flash-lite` stored
   with `<sub>` subscripts and a literal `&partial;`.

   Needs no database."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [freememo.ocr :as ocr]
            [freememo.ocr-math :as ocr-math]
            [freememo.html-cleaner :as cleaner]))

(defn- p [s] (str "<p>" s "</p>"))

(def ^:private conversions
  "[input expected] pairs, both as HTML. §3.7 of plans/ocr-math-html-to-tex.md."
  [;; Equation paragraphs: the number and trailing punctuation stay outside.
   ["(9) <em>vmh</em><sub><em>m</em></sub>(<em>q</em>, <em>m</em>) + <em>vh</em>(<em>q</em>, <em>m</em>) − <em>s</em> − <em>C</em>′(<em>m</em>) = 0;"
    "(9) \\(vmh_{m}(q, m) + vh(q, m) - s - C'(m) = 0\\);"]
   ["(12) <em>R</em>′(<em>q</em>) = <em>vmh</em><sub><em>q</em></sub> &gt; 0."
    "(12) \\(R'(q) = vmh_{q} &gt; 0\\)."]
   ["(11) &partial;<em>m</em>/&partial;<em>q</em> = −<em>v</em>(<em>h</em><sub><em>q</em></sub> + <em>mh</em><sub><em>qm</em></sub>)"
    "(11) \\(\\partial m/\\partial q = -v(h_{q} + mh_{qm})\\)"]
   ;; The same equation without <em>: a script's base is not a prose word.
   ["(10) 2vh<sub>m</sub> + vmh<sub>mm</sub> − C″ &lt; 0."
    "(10) \\(2vh_{m} + vmh_{mm} - C'' &lt; 0\\)."]
   ;; Inline in prose: greedy over the relation, stopped by the sentence end.
   ["since <em>h</em><sub><em>q</em></sub> &gt; 0. Finally"
    "since \\(h_{q} &gt; 0\\). Finally"]
   ["increases with q if <em>h</em><sub><em>qm</em></sub>&gt;0. Next"
    "increases with q if \\(h_{qm}&gt;0\\). Next"]
   ;; An unmatched parenthesis stays outside the formula.
   ["positive (since <em>h</em><sub><em>q</em></sub> &gt; 0) here"
    "positive (since \\(h_{q} &gt; 0\\)) here"]])

(def ^:private unchanged
  "Blocks the pass must leave byte-identical."
  ["Differentiate (9) with respect to <em>q</em>:"
   "market size on service quality.<sup>7</sup> In that case"
   "<sup>7</sup>This version of the model has a strong family resemblance"
   "\\(x^2\\) already"
   "Costs 5 = 5 dollars"])

(deftest converts-html-math
  (doseq [[in out] conversions]
    (is (= (p out) (ocr-math/html-math->tex (p in))) in)))

(deftest leaves-prose-footnotes-and-tex-alone
  (doseq [s unchanged]
    (is (= (p s) (ocr-math/html-math->tex (p s))) s)))

(deftest leaves-code-alone
  (let [html "<pre><code>h<sub>q</sub> = 0</code></pre>"]
    (is (= html (ocr-math/html-math->tex html)))))

(deftest is-idempotent
  (doseq [[in] conversions]
    (let [once (ocr-math/html-math->tex (p in))]
      (is (= once (ocr-math/html-math->tex once)) in))))

(deftest nil-in-nil-out
  (is (nil? (ocr-math/html-math->tex nil))))

(deftest repairs-an-invented-entity
  (testing "§3.3: the sanitizer no longer shows the entity as text"
    (let [out (cleaner/clean-html (ocr-math/html-math->tex "<p>&partial;m is a symbol</p>"))]
      (is (not (re-find #"partial" out)) out)
      (is (re-find #"∂" out) out))))

(deftest translates-symbols-and-escapes-specials
  (is (= (p "\\(\\alpha_{i} \\le \\Gamma \\% x\\)")
         (ocr-math/html-math->tex (p "α<sub>i</sub> ≤ Γ % x")))))

(deftest both-lanes-store-tex
  (testing "§3.2: the conversion sits where both OCR shapes meet"
    (let [chat {:choices [{:message {:content "```html\n<p>since <em>h</em><sub><em>q</em></sub> &gt; 0. Finally</p>\n```"}}]}
          plugin {:choices [{:message {:annotations
                                       [{:type "file"
                                         :file {:content [{:type "text" :text "since *h*<sub>*q*</sub> > 0. Finally"}]}}]}}]}]
      (doseq [[shape body] [[:chat chat] [:plugin plugin]]]
        (is (= "<p>since \\(h_{q} &gt; 0\\). Finally</p>"
               ;; flexmark ends the :plugin lane's HTML with a newline.
               (str/trim (ocr/response->page-html shape body)))
            (name shape))))))
