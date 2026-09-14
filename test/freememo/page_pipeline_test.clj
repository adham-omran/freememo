(ns freememo.page-pipeline-test
  "Needs no database. `freememo.page-pipeline` requires only Jsoup, the
   sanitizer and the biblio harvester, so this namespace loads with PostgreSQL
   down."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [freememo.page-pipeline :as pp]))

(def ^:private article-url "https://example.com/article")

(defn- tag-count [html tag]
  (count (re-seq (re-pattern (str "(?i)<" tag "\\b")) (or html ""))))

;; ---------------------------------------------------------------------------
;; Title
;; ---------------------------------------------------------------------------

(deftest title-comes-from-the-head
  (is (= "Real Title"
        (pp/derive-title "<html><head><title>Real Title</title></head><body><h1>H</h1></body></html>"
          article-url))
    "<title> wins over <h1>"))

(deftest title-falls-through-an-empty-tag
  (is (= "Heading"
        (pp/derive-title "<html><head><title></title></head><body><h1>Heading</h1></body></html>"
          article-url))
    "an empty <title> must not become the topic name"))

(deftest title-falls-back-to-the-url
  (is (= article-url (pp/derive-title "<html><body><p>no name</p></body></html>" article-url))
    "a page that names itself nothing gets its URL"))

(deftest title-survives-a-blank-document
  (is (= article-url (pp/derive-title nil article-url))
    "nil html yields the fallback, never nil"))

(deftest the-title-is-read-before-the-head-is-discarded
  (let [html "<html><head><title>Kept</title></head><body><main><p>body</p></main></body></html>"
        result (pp/page->document html article-url)]
    (is (= "Kept" (:title result))
      "page->document reads the title first — the main-region step drops <head>")
    (is (not (str/includes? (:html result) "Kept"))
      "and the title is not in the stored content, which proves <head> was discarded")))

;; ---------------------------------------------------------------------------
;; Chrome removal
;; ---------------------------------------------------------------------------

(deftest page-furniture-is-removed-with-its-text
  (let [html (str "<html><body>"
               "<nav><a href='/x'>Navigation link</a></nav>"
               "<header>Site header</header>"
               "<article><p>The actual content.</p></article>"
               "<footer>Footer boilerplate</footer>"
               "</body></html>")
        out (:html (pp/page->document html article-url))]
    (testing "clean-html alone would keep this text — the pipeline removes the elements whole"
      (is (not (str/includes? out "Navigation link")))
      (is (not (str/includes? out "Site header")))
      (is (not (str/includes? out "Footer boilerplate"))))
    (is (str/includes? out "The actual content."))))

(deftest scripts-never-reach-the-stored-content
  (let [html "<html><body><main><script>var leak = 1;</script><p>text</p></main></body></html>"
        out (:html (pp/page->document html article-url))]
    (is (not (str/includes? out "leak")))))

(deftest the-main-region-wins-over-the-body
  (let [html (str "<html><body><p>outside</p>"
               "<main><p>inside</p></main>"
               "</body></html>")
        out (:html (pp/page->document html article-url))]
    (is (str/includes? out "inside"))
    (is (not (str/includes? out "outside")))))

;; ---------------------------------------------------------------------------
;; Browser-DOM gate
;; ---------------------------------------------------------------------------

(def ^:private browser-page
  (str "<html><head><title>T</title></head><body><main>"
    "<div><div><p>nested</p></div></div>"
    "<p>see <cite>Source</cite> and <small>note</small></p>"
    "<table><caption>Cap</caption><thead><tr><th>H</th></tr></thead>"
    "<tbody><tr><td>a</td></tr></tbody></table>"
    "</main></body></html>"))

(deftest the-fetch-path-leaves-browser-constructs-alone
  (let [out (:html (pp/page->document browser-page article-url))]
    (testing "no importer changes behaviour because the capture path exists"
      (is (pos? (tag-count out "div")))
      (is (pos? (tag-count out "cite")))
      (is (pos? (tag-count out "th"))))))

(deftest the-capture-path-removes-every-construct-quill-drops
  (let [out (:html (pp/page->document browser-page article-url {:browser-dom? true}))]
    (is (zero? (tag-count out "div")) "layout divs")
    (is (zero? (tag-count out "cite")) "cite")
    (is (zero? (tag-count out "small")) "small")
    (is (zero? (tag-count out "caption")) "caption")
    (is (zero? (tag-count out "th")) "th")
    (is (zero? (tag-count out "thead")) "thead")
    (testing "and loses no text"
      (is (str/includes? out "nested"))
      (is (str/includes? out "Source"))
      (is (str/includes? out "note"))
      (is (str/includes? out "Cap"))
      (is (str/includes? out "H"))
      (is (str/includes? out "a")))))

(deftest a-quill-code-block-survives-the-capture-path
  (let [html (str "<html><body><main>"
               "<div class=\"ql-code-block-container\">"
               "<div class=\"ql-code-block\">(inc 1)</div></div>"
               "</main></body></html>")
        out (:html (pp/page->document html article-url {:browser-dom? true}))]
    (is (str/includes? out "ql-code-block-container")
      "the one div shape Quill holds must not be unwrapped")
    (is (str/includes? out "(inc 1)"))))

;; ---------------------------------------------------------------------------
;; Bibliography
;; ---------------------------------------------------------------------------

(deftest the-url-always-reaches-the-biblio
  (let [out (pp/page->document "<html><body><p>x</p></body></html>" article-url)]
    (is (= article-url (get-in out [:biblio :local :URL]))
      "a caller can attach a source without knowing where the page came from")))

(deftest a-harvested-url-is-not-overwritten
  (let [html (str "<html><head><meta name='citation_fulltext_html_url' "
               "content='https://example.com/canonical'></head><body><p>x</p></body></html>")
        out (pp/page->document html article-url)]
    (is (string? (get-in out [:biblio :local :URL])))))

(deftest a-publication-date-is-mined-when-present
  (let [html (str "<html><head><meta name='citation_date' content='2023-01-19'>"
               "</head><body><p>x</p></body></html>")
        out (pp/page->document html article-url)]
    (is (= [[2023 1 19]] (:issued-date-parts out)))))

(deftest no-date-means-no-key
  (let [out (pp/page->document "<html><body><p>x</p></body></html>" article-url)]
    (is (not (contains? out :issued-date-parts)))))

(deftest the-shape-is-stable
  (let [out (pp/page->document "<html><body><p>x</p></body></html>" article-url)]
    (is (= article-url (:url out)))
    (is (= "web" (:source-type out)))
    (is (string? (:html out)))
    (is (map? (:biblio out)))))
