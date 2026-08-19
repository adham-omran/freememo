(ns freememo.capture-quill-fidelity-test
  "Every tag a capture stores must have a Quill blot.

   This is the check the Sphinx importer shipped without. That build was
   declared verified against 2361 pages, every one of which rendered flat,
   because the verification counted rows and links and never compared a page
   against what Quill could hold.

   `quill-tags` below is not a guess. It was read out of the vendored
   `quill@2.0.3/quill.js` bundle by grepping every `tagName=` assignment on a
   registered blot. Re-derive it after any Quill upgrade:

     grep -oE 'tagName=\"[A-Z0-9]+\"' resources/public/freememo/vendor/quill@2.0.3/quill.js
     grep -oE 'tagName=\\[\"[A-Z0-9\",]+\\]' resources/public/freememo/vendor/quill@2.0.3/quill.js

   Needs no database."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [freememo.page-pipeline :as pp]))

(def ^:private quill-tags
  "Tags Quill 2.0.3 registers a blot for, lowercased.

   `div` and `span` are in the set, but conditionally: Quill holds a `div` only
   as a code block and a `span` only when it carries a recognized inline
   format. `bare-structural-tags` below is what actually gates them."
  #{"a" "br" "code" "div" "iframe" "img" "li" "ol" "p" "span"
    "table" "tbody" "td" "tr" "u"
    "em" "i" "h1" "h2" "h3" "h4" "h5" "h6" "s" "strike" "strong" "b" "sub" "sup"
    "blockquote" "pre" "ul"})

(defn- tag-census
  "Every distinct tag name in `html`, lowercased."
  [html]
  (->> (re-seq #"(?i)<([a-z][a-z0-9]*)\b" (or html ""))
       (map (comp str/lower-case second))
       set))

(def ^:private real-page
  "A page carrying every construct a browser DOM throws at the pipeline."
  (str "<html><head><title>Fidelity Probe</title>"
    "<meta name='citation_title' content='Fidelity Probe'></head><body>"
    "<nav><a href='/'>nav</a></nav>"
    "<header>header</header>"
    "<main>"
    "<section><div class='wrapper'><div class='inner'>"
    "<h2>Heading</h2>"
    "<p>Text with <span class='fancy'>a styled span</span>, <cite>a citation</cite>, "
    "<q>a quotation</q>, <small>small print</small>, <strong>bold</strong> and "
    "<a href='https://example.com'>a link</a>.</p>"
    "<figure><img src='https://example.com/i.png' alt='x'>"
    "<figcaption>A caption</figcaption></figure>"
    "<dl><dt>Term</dt><dd><p>Definition</p></dd></dl>"
    "<table><caption>Table caption</caption>"
    "<colgroup><col></colgroup>"
    "<thead><tr><th>Header cell</th></tr></thead>"
    "<tbody><tr><td>Body cell</td></tr></tbody>"
    "<tfoot><tr><td>Foot cell</td></tr></tfoot></table>"
    "<ul><li>Item</li></ul>"
    "<blockquote>Quoted</blockquote>"
    "<pre><code>code()</code></pre>"
    "</div></div></section>"
    "</main>"
    "<footer>footer</footer></body></html>"))

(deftest every-stored-tag-has-a-quill-blot
  (let [stored (:html (pp/page->document real-page "https://example.com/probe"
                        {:browser-dom? true}))
        tags (tag-census stored)
        orphans (remove quill-tags tags)]
    (is (empty? orphans)
      (str "stored tags Quill 2.0.3 would drop on load: " (pr-str (vec orphans))
        "\nstored html: " stored))))

(deftest the-fetch-path-is-not-held-to-this
  (testing "importers keep their own corpus — this test is about captures only"
    (let [stored (:html (pp/page->document real-page "https://example.com/probe"))
          tags (tag-census stored)]
      (is (seq (remove quill-tags tags))
        "if the fetch path ever became blot-clean too, delete this test rather than invert it"))))

(deftest no-text-is-lost
  (let [stored (:html (pp/page->document real-page "https://example.com/probe"
                        {:browser-dom? true}))]
    (testing "every construct's content survives its rewrite"
      (doseq [fragment ["Heading" "a styled span" "a citation" "a quotation"
                        "small print" "bold" "a link" "A caption" "Term"
                        "Definition" "Table caption" "Header cell" "Body cell"
                        "Foot cell" "Item" "Quoted" "code()"]]
        (is (str/includes? stored fragment) (str "lost: " fragment))))))

(deftest page-furniture-is-gone
  (let [stored (:html (pp/page->document real-page "https://example.com/probe"
                        {:browser-dom? true}))]
    (doseq [fragment ["nav" "header" "footer"]]
      (is (not (str/includes? stored (str ">" fragment "<")))
        (str "chrome text survived: " fragment)))))
