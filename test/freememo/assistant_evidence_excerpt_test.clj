(ns freememo.assistant-evidence-excerpt-test
  "Evidence excerpt contracts that do not need a database."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [freememo.assistant-evidence-excerpt :as excerpt])
  (:import
   [org.jsoup Jsoup]))

(deftest html-is-split-at-semantic-blocks
  (let [blocks (excerpt/evidence-blocks
                 (str "<h2 id=\"intro\">Introduction</h2>"
                   "<p>First <strong>paragraph</strong>.</p>"
                   "<p>Second paragraph.</p>"))]
    (is (= ["Introduction" "First paragraph." "Second paragraph."]
          (mapv :text blocks)))
    (is (= ["intro" "intro" "intro"] (mapv :anchor blocks)))
    (is (= ["Introduction" "Introduction" "Introduction"]
          (mapv :section-title blocks)))
    (is (= [1 2 3] (mapv :block-index blocks)))
    (is (str/includes? (:html (second blocks)) "<strong>paragraph</strong>"))))

(deftest excerpts-are-valid-and-bounded
  (let [source (str "<p>" (str/join " " (repeat 700 "evidence")) "</p>")
        blocks (excerpt/evidence-blocks source)]
    (is (< 1 (count blocks)))
    (is (every? #(<= (count (:text %)) excerpt/max-visible-chars) blocks))
    (doseq [{:keys [html text]} blocks]
      (testing "each HTML excerpt is a complete fragment"
        (let [body (.body (Jsoup/parseBodyFragment html))]
          (is (= 1 (.childrenSize body)))
          (is (= text (.text body))))))))

(deftest unsafe-html-does-not-enter-a-retained-block
  (let [[block] (excerpt/evidence-blocks
                  "<p onclick=\"steal()\">Safe<script>steal()</script> text</p>")]
    (is (= "Safe text" (:text block)))
    (is (not (str/includes? (:html block) "onclick")))
    (is (not (str/includes? (:html block) "script")))))

(defn- quill-code-block [lines]
  (str "<div class=\"ql-code-block-container\">"
    (apply str (map #(str "<div class=\"ql-code-block\">" % "</div>") lines))
    "</div>"))

(deftest a-code-block-keeps-its-lines-and-decoded-text
  (let [[block] (excerpt/evidence-blocks
                  (quill-code-block ["(m/sp" "  (m/? (m/?&lt; flow)))"]))]
    (is (= "(m/sp\n  (m/? (m/?< flow)))" (:text block)))))

(deftest a-long-code-block-splits-between-lines
  (let [lines (mapv #(str "  (step-" % " ?&lt;)") (range 150))
        decoded (mapv #(str/replace % "&lt;" "<") lines)
        blocks (excerpt/evidence-blocks (quill-code-block lines))]
    (is (< 1 (count blocks)))
    (is (every? #(<= (count (:text %)) excerpt/max-visible-chars) blocks))
    (testing "the parts rejoin to the whole block, so every cut fell on a line break"
      (is (= (str/join "\n" decoded) (str/join "\n" (map :text blocks)))))
    (testing "each part starts with an indented line"
      (is (every? #(str/starts-with? (:text %) "  (step-") blocks)))
    (testing "each part's HTML is preformatted"
      (is (every? #(str/starts-with? (:html %) "<pre>") blocks)))))
