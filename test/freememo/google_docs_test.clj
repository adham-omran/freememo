(ns freememo.google-docs-test
  "A Google Doc's Markdown export, through the stored-HTML pipeline
   (plans/google-docs-pull.md, C1). No database and no network."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.google-docs :as gdocs]))

(def ^:private export
  "Drive's `text/markdown` export of a test Doc, byte for byte (2026-10-21),
   less the image payload. The Doc came from an uploaded Markdown file, which
   Google turned into plain paragraphs, so its code is escaped prose here."
  (str "# Export test\n\n"
    "A paragraph with **bold**, *italic* and a [link](https://example.org/a?b=1).\n\n"
    "{:chunks\n\n \\[{:body\n\n   \\[:call\n\n"
    "- First point  \n  - Nested point  \n- Second point\n\n"
    "| Name | Value |\n| :---- | :---- |\n| a | 1 |\n\n"
    "![a red dot][image1]\n\nThe end.\n\n"
    "[image1]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR4XmP4z8BQDwAEgAF/xUpVogAAAABJRU5ErkJggg==>"))

(def ^:private fenced
  "A code block as the Markdown export writes one, a fence with a language."
  "Before.\n\n```clojure\n(defn f [x]\n  (inc x))\n```\n\nAfter.\n")

(deftest the-export-keeps-its-structure
  (let [html (gdocs/stored-html export)]
    (testing "emphasis and the link, with no Google redirect"
      (is (str/includes? html "<strong>bold</strong>"))
      (is (str/includes? html "<em>italic</em>"))
      (is (str/includes? html "href=\"https://example.org/a?b=1\"")))
    (testing "an escaped bracket reads as a bracket"
      (is (str/includes? html "[{:body"))
      (is (not (str/includes? html "\\["))))
    (testing "the nested list is nested"
      (is (re-find #"<li>First point\s*<ul>\s*<li>Nested point" html)))
    (testing "the table is a table"
      (is (str/includes? html "<table>"))
      (is (str/includes? html "<td>a</td>")))
    (testing "the image is kept, as a data: URI for the caller to re-host"
      (is (re-find #"<img src=\"data:image/png;base64,[^\"]+\" alt=\"a red dot\"" html)))))

(deftest a-fence-is-a-code-block-with-its-language
  (let [html (gdocs/stored-html fenced)]
    (is (str/includes? html "<pre data-language=\"clojure\"><code>(defn f [x]\n  (inc x))"))
    (is (str/includes? html "<p>After.</p>"))))

(deftest a-blank-export-stores-nothing
  (is (nil? (gdocs/stored-html "")))
  (is (nil? (gdocs/stored-html nil))))

(deftest the-hash-ignores-image-links-and-nothing-else
  (let [a "<p>x</p><img src=\"data:image/png;base64,AAAA\">"
        b "<p>x</p><img src=\"data:image/png;base64,BBBB\">"
        c "<p>y</p><img src=\"data:image/png;base64,AAAA\">"]
    (is (= (gdocs/content-hash a) (gdocs/content-hash b)))
    (is (not= (gdocs/content-hash a) (gdocs/content-hash c)))
    (is (= 64 (count (gdocs/content-hash a))))))
