(ns freememo.text-test
  "Plain-text conversion contracts that do not need a database."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.text :as text]))

(def ^:private lined text/html->text-with-line-breaks)

(deftest entities-are-decoded
  (testing "the operators from the reported defect"
    (is (= "does ?< differ from ?> & why"
          (lined "<p>does <code>?&lt;</code> differ from <code>?&gt;</code> &amp; why</p>")))))

(deftest quill-code-lines-stay-separate-and-indented
  (is (= "(m/sp\n  (m/? task))"
        (lined (str "<div class=\"ql-code-block-container\">"
                 "<select class=\"ql-ui\"><option>Plain</option><option>Clojure</option></select>"
                 "<div class=\"ql-code-block\">(m/sp</div>"
                 "<div class=\"ql-code-block\">  (m/? task))</div>"
                 "</div>")))))

(deftest pre-keeps-its-own-line-breaks
  (is (= "(defn f []\n  1)" (lined "<pre>(defn f []\n  1)</pre>"))))

(deftest blocks-start-lines
  (is (= "Debounce\nIntro.\none\ntwo\nQuoted."
        (lined (str "<h2>Debounce</h2><p>Intro.</p>"
                 "<ul><li>one</li><li>two</li></ul>"
                 "<blockquote>Quoted.</blockquote>")))))

(deftest flowing-whitespace-collapses
  (is (= "lots of space" (lined "<p>  lots   of\n   space </p>")))
  (is (= "a b" (lined "<p>a <strong> b</strong></p>"))))

(deftest breaks-and-blank-lines
  (testing "a <br> starts a line"
    (is (= "a\nb" (lined "<p>a<br>b</p>"))))
  (testing "a run of empty Quill paragraphs is one blank line"
    (is (= "a\n\nc" (lined "<p>a</p><p><br></p><p><br></p><p><br></p><p>c</p>")))))

(deftest table-cells-are-tab-separated
  (is (= "a\tb\nc\td"
        (lined "<table><tr><td>a</td><td>b</td></tr><tr><th>c</th><th>d</th></tr></table>"))))

(deftest blank-input-is-empty
  (is (= "" (lined nil)))
  (is (= "" (lined "  ")))
  (is (= "" (lined "<p><br></p>"))))
