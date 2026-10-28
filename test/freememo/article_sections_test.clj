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
