(ns freememo.article-sections-test
  "Pure article boundary and content preservation checks."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [freememo.article-sections :as article])
  (:import [org.jsoup Jsoup]))

(defn- body [html]
  (.body (Jsoup/parseBodyFragment html)))

(defn- text [section]
  (.text (body (:content section))))

(deftest nested-headings-have-only-their-own-content
  (let [result (article/sections
                 (str "<p>Lead</p><h2>Parent</h2><p>Own text</p>"
                      "<h3>Child</h3><p>Child text</p>"
                      "<h4>Grandchild</h4><p>Grandchild text</p>"
                      "<h3>Sibling</h3><p>Sibling text</p><h2>Next</h2>")
                 {:mode :headings})]
    (is (= [0 1 2 3 4 5] (mapv :index result)))
    (is (= [nil nil 1 2 1 nil] (mapv :parent-index result)))
    (is (= ["Introduction" "Parent" "Child" "Grandchild" "Sibling" "Next"]
           (mapv :title result)))
    (is (= ["Lead" "Parent Own text" "Child Child text"
            "Grandchild Grandchild text" "Sibling Sibling text" "Next"]
           (mapv text result)))))

(deftest all-heading-levels-and-skipped-levels
  (let [html (apply str (map #(str "<h" % ">Level " % "</h" % ">") (range 1 7)))
        result (article/sections html {:mode :headings})]
    (is (= [nil 0 1 2 3 4] (mapv :parent-index result))))
  (is (= [nil 0 0 nil]
         (mapv :parent-index
               (article/sections "<h2>A</h2><h4>B</h4><h3>C</h3><h1>D</h1>"
                                 {:mode :headings})))))

(deftest nested-wrapper-attributes-survive
  (let [html (str "<article class='source'><div style='color: red'>Lead"
                  "<h2 id='first'>First</h2><p><strong>Bold</strong></p>"
                  "<section data-section='child'><h3>Child</h3>Tail</section>"
                  "</div></article>")
        result (article/sections html {:mode :headings})]
    (is (= ["Lead" "First Bold" "Child Tail"] (mapv text result)))
    (doseq [section result]
      (is (some? (.selectFirst (body (:content section)) "article.source > div[style]"))))
    (is (some? (.selectFirst (body (:content (nth result 1))) "h2#first + p > strong")))
    (is (some? (.selectFirst (body (:content (nth result 2))) "section[data-section=child] > h3")))))

(deftest horizontal-lines-split-without-empty-sections
  (let [result (article/sections
                 "<div><hr><p>One</p><hr><hr><p>Two</p><hr></div>"
                 {:mode :lines})]
    (is (= ["One" "Two"] (mapv text result)))
    (is (= [nil nil] (mapv :parent-index result)))
    (is (every? #(empty? (.select (body (:content %)) "hr")) result))))

(deftest splitmarks-are-whole-paragraph-literal-matches
  (let [html (str "<p>First</p><p>  <strong>[.*]</strong>  </p><p>Second</p>"
                  "<p>Inline [.*] occurrence</p><p>[.*] suffix</p><p>[.*]</p>"
                  "<p>Third</p>")
        result (article/sections html {:mode :splitmark :splitmark " [.*] "})]
    (is (= ["First" "Second Inline [.*] occurrence [.*] suffix" "Third"]
           (mapv text result)))
    (is (every? nil? (map :parent-index result)))))

(deftest splitmarks-preserve-case-and-internal-whitespace
  (let [html "<p>First</p><p>STOP</p><p>stop</p><p>Middle</p><p>a  b</p><p>Last</p>"]
    (is (= ["First STOP" "Middle a b Last"]
           (mapv text (article/sections html {:mode :splitmark :splitmark "stop"}))))
    (is (= html (:content (first (article/sections html {:mode :splitmark :splitmark "a b"})))))))

(deftest blocks-group-four-content-blocks
  (let [html (str "<article><div>"
                  (str/join "\n" (map #(str "<p>Paragraph " % "</p>") (range 1 10)))
                  "</div></article>")
        result (article/sections html {:mode :blocks})]
    (is (= [4 4 1] (mapv #(count (.select (body (:content %)) "p")) result)))
    (is (= [nil nil nil] (mapv :parent-index result)))
    (is (= (mapv #(str "Paragraph " %) (range 1 10))
           (vec (mapcat #(map (fn [element] (.text element))
                             (.select (body (:content %)) "p")) result))))))

(deftest rich-blocks-remain-whole
  (let [html (str "<div><p>Before</p>"
                  "<table data-kind='example'><tbody><tr><td>A</td><td>B</td></tr></tbody></table>"
                  "<ul><li>One <strong>bold</strong></li><li>Two</li></ul>"
                  "<pre><code>  x &lt; y\n    z</code></pre>"
                  "<div class='ql-code-block-container' data-language='clojure'>"
                  "<div class='ql-code-block'>(inc 1)</div>"
                  "<div class='ql-code-block'>(inc 2)</div></div><p>After</p></div>")
        result (article/sections html {:mode :blocks})
        first-body (body (:content (first result)))
        second-body (body (:content (second result)))]
    (is (= 2 (count result)))
    (is (= 2 (count (.select first-body "table[data-kind=example] td"))))
    (is (= 2 (count (.select first-body "ul > li"))))
    (is (= "  x < y\n    z" (.wholeText (.selectFirst first-body "pre > code"))))
    (is (= 2 (count (.select second-body ".ql-code-block-container > .ql-code-block"))))
    (is (= "(inc 1) (inc 2) After" (.text second-body)))))

(deftest unwrapped-inline-text-and-images-survive
  (let [html "Lead <em>inline</em><img src='lead.png'><h2>Title</h2>Tail<img src='tail.png'>"
        result (article/sections html {:mode :headings})]
    (is (= ["Lead inline" "Title Tail"] (mapv text result)))
    (is (= ["lead.png" "tail.png"]
           (mapv #(.attr (.selectFirst (body (:content %)) "img") "src") result))))
  (doseq [mode [:headings :lines :splitmark :blocks]]
    (let [html "<img src='only.png' alt='Only image'>"
          result (article/sections html {:mode mode :splitmark "CUT"})]
      (is (= 1 (count result)))
      (is (= html (:content (first result)))))))

(deftest grouping-retains-unwrapped-inline-runs
  (let [html (str "Intro <em>with emphasis</em> and text"
                  "<p>Second</p><img src='image.png'><p>Fourth</p><p>Fifth</p>")
        result (article/sections html {:mode :blocks})]
    (is (= 2 (count result)))
    (is (= ["Intro with emphasis and text Second Fourth" "Fifth"]
           (mapv text result)))
    (is (= 1 (count (.select (body (:content (first result))) "img"))))))

(deftest no-boundary-retains-the-exact-source
  (let [html "<div class='original'>Unwrapped <span>text</span><img src='a.png'></div>"]
    (doseq [mode [:headings :lines :splitmark :blocks]]
      (is (= [{:index 0 :parent-index nil :title "Unwrapped text" :content html}]
             (article/sections html {:mode mode :splitmark "CUT"}))))))

(deftest invalid-options-fail-at-the-parser-boundary
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"supported article boundary mode"
                       (article/sections "<p>Text</p>" {:mode :guess})))
  (doseq [splitmark [nil "" "  " 42]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"nonblank custom splitmark"
                         (article/sections "<p>Text</p>" {:mode :splitmark :splitmark splitmark}))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"HTML text"
                       (article/sections 42 {:mode :headings}))))

(deftest empty-content-has-no-sections
  (doseq [html [nil "" " \n "]]
    (is (= [] (article/sections html {:mode :headings})))))

(defn- blue-spans [document]
  (.select document "span[style*=#44C2FF]"))

(defn- blue-text [html]
  (mapv #(.text %) (blue-spans (body html))))

(defn- normalized-html [html]
  (let [document (Jsoup/parseBodyFragment html)]
    (.prettyPrint (.outputSettings document) false)
    (.html (.body document))))

(defn- without-blue-spans [html]
  (let [document (Jsoup/parseBodyFragment html)]
    (.prettyPrint (.outputSettings document) false)
    (doseq [span (vec (blue-spans document))]
      (.unwrap span))
    (.html (.body document))))

(deftest highlighting-follows-heading-sections-without-including-descendants
  (let [html (str "<article>Lead<h2>Parent</h2><p>Own text</p>"
                  "<section><h4>Child</h4><p>Child text</p></section>"
                  "<h3>Sibling</h3><p>Sibling text</p></article>")
        result (article/highlight-sections html {:mode :headings} [1])]
    (is (= ["Parent" "Own text"] (blue-text result)))
    (is (= (normalized-html html) (without-blue-spans result)))
    (is (= ["Parent" "Own text" "Sibling" "Sibling text"]
           (blue-text (article/highlight-sections html {:mode :headings} [1 3]))))
    (is (= ["Child" "Child text"]
           (blue-text (article/highlight-sections html {:mode :headings} [2]))))))

(deftest highlighting-identical-paragraphs-uses-node-identity
  (let [html "<p>Repeated</p><hr><p>Repeated</p><hr><p>Repeated</p>"
        result (body (article/highlight-sections html {:mode :lines} [1]))
        paragraphs (.select result "p")]
    (is (= [0 1 0] (mapv #(count (blue-spans %)) paragraphs)))
    (is (= 2 (count (.select result "hr"))))))

(deftest highlighting-preserves-splitmarks-and-excluded-sections
  (let [html "<p>Before</p><p> <strong>CUT</strong> </p><p>After</p>"
        options {:mode :splitmark :splitmark "CUT"}
        result (article/highlight-sections html options [1])]
    (is (= ["After"] (blue-text result)))
    (is (= (normalized-html html) (without-blue-spans result)))
    (is (= ["Before" "After"] (mapv text (article/sections result options))))))

(deftest highlighting-follows-explicit-four-block-groups
  (let [html (str "<div>"
                  (apply str (map #(str "<p>Paragraph " % "</p>") (range 1 10)))
                  "</div>")
        result (article/highlight-sections html {:mode :blocks} [1])]
    (is (= ["Paragraph 5" "Paragraph 6" "Paragraph 7" "Paragraph 8"]
           (blue-text result)))
    (is (= (normalized-html html) (without-blue-spans result)))))

(deftest highlighting-preserves-rich-structure-and-inline-media
  (let [html (str "<article class='source'><h2 id='first'>First</h2>"
                  "<p data-kind='text'>Bare <strong>bold <em>nested</em></strong> text"
                  "<img src='/api/media/42' alt='Example'></p>"
                  "<ul><li>One</li><li><a href='/example'>Two</a></li></ul>"
                  "<table><tbody><tr><td>A</td><td>B</td></tr></tbody></table>"
                  "<pre><code>  x &lt; y\n    z</code></pre>"
                  "<div class='ql-code-block-container' data-language='clojure'>"
                  "<div class='ql-code-block'>(inc 1)</div></div>"
                  "<h3>Excluded</h3><p>Keep plain</p></article>")
        result (article/highlight-sections html {:mode :headings} [0])
        document (body result)]
    (is (= (normalized-html html) (without-blue-spans result)))
    (is (some? (.selectFirst document "p[data-kind=text] > strong > em > span")))
    (is (= "/api/media/42" (.attr (.selectFirst document "img") "src")))
    (is (= "  x < y\n    z" (.wholeText (.selectFirst document "pre > code"))))
    (is (= 2 (count (.select document "table td > span"))))
    (is (= 2 (count (.select document "ul > li span"))))
    (is (some? (.selectFirst document ".ql-code-block-container > .ql-code-block > span")))
    (is (zero? (count (.select document "h3 span, h3 + p span"))))))

(deftest highlighting-preserves-existing-background-colors
  (let [html (str "<p>Plain <span style='background-color: yellow'>Yellow <strong>bold</strong></span>"
                  "<mark>Marked</mark><span style='background-color: #44C2FF'>Blue</span>"
                  "<span style='background-color: transparent'>Clear</span></p>")
        result (article/highlight-sections html {:mode :headings} [0])
        document (body result)]
    (is (= ["Plain" "Blue" "Clear"] (blue-text result)))
    (is (zero? (count (.select document "span[style*=yellow] span, mark span"))))
    (is (= "Yellow bold" (.text (.selectFirst document "span[style*=yellow]"))))
    (is (= 0 (count (.select document "span[style*=#44C2FF] span"))))))

(deftest highlighting-no-boundary-uses-the-whole-article
  (let [html "<div class='original'>Unwrapped <em>text</em><img src='only.png'></div>"]
    (doseq [mode [:headings :lines :splitmark :blocks]]
      (let [options {:mode mode :splitmark "CUT"}
            result (article/highlight-sections html options [0])]
        (is (= ["Unwrapped" "text"] (blue-text result)))
        (is (= (normalized-html html) (without-blue-spans result)))
        (is (= #{:index :parent-index :title :content}
               (set (keys (first (article/sections html options))))))))))

(deftest highlighting-empty-or-invisible-selections-retains-exact-source
  (doseq [html [nil "" " \n " "<p>Text</p>"]]
    (is (= html (article/highlight-sections html {:mode :headings} [])))
    (is (= html (article/highlight-sections html {:mode :headings} nil))))
  (doseq [html ["<img src='only.png'>" "<p>  <br> </p>"
               "<p><span style='background-color: yellow'>Already marked</span></p>"]]
    (is (= html (article/highlight-sections html {:mode :headings} [0])))))

(deftest invalid-highlight-indices-fail-at-the-parser-boundary
  (doseq [indices [[-1] [1] [nil] [0.5] "0" 0]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"existing article sections"
                         (article/highlight-sections "<p>Text</p>" {:mode :headings} indices))))
  (is (= ["Text"] (blue-text (article/highlight-sections "<p>Text</p>"
                                                      {:mode :headings} [0 0])))))
