(ns freememo.gdoc-html-test
  "A Google Docs HTML export, through the normalizer and the whole stored-HTML
   pipeline (plans/google-docs-pull.md, C1). No database and no network."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.gdoc-html :as gh]
            [freememo.google-docs :as gdocs]))

(def ^:private export
  "The shape Drive's `text/html` export writes: formatting as classes in a
   `<style>` block, one list element per level, links wrapped by google.com,
   and comments as markers plus a list at the end."
  (str "<html><head><meta content=\"text/html; charset=UTF-8\" http-equiv=\"content-type\">"
    "<style type=\"text/css\">"
    "ol.lst-kix_a-0{list-style-type:none}"
    ".c1{font-weight:700}.c2{font-style:italic}.c3{text-decoration:underline}"
    ".c4{text-align:center}.c5{color:#000000;font-weight:400}"
    ".c6{background-color:#ffffff;max-width:468pt}"
    "</style></head><body class=\"c6 doc-content\">"
    "<h1 class=\"c4\"><span class=\"c5\">Title</span></h1>"
    "<p class=\"c4\"><span class=\"c1\">bold</span><span class=\"c5\"> and </span>"
    "<span class=\"c1 c2\">both</span><sup><a href=\"#cmnt1\" id=\"cmnt_ref1\">[a]</a></sup></p>"
    "<ul class=\"c0 lst-kix_a-0 start\"><li class=\"c7\"><span class=\"c5\">one</span></li></ul>"
    "<ul class=\"c0 lst-kix_a-1 start\"><li class=\"c7\"><span class=\"c3\">nested</span></li></ul>"
    "<ul class=\"c0 lst-kix_a-0\"><li class=\"c7\"><span class=\"c5\">two</span></li></ul>"
    "<p><a href=\"https://www.google.com/url?q=https://example.org/a%3Fb%3D1&amp;sa=D&amp;ust=1\">link</a></p>"
    "<div><p><a href=\"#cmnt_ref1\" id=\"cmnt1\">[a]</a><span>A comment</span></p></div>"
    "</body></html>"))

(deftest class-styles-reads-single-class-rules-only
  (is (= {"c1" {"font-weight" "700"} "c2" {"font-style" "italic"}}
         (gh/class-styles ".c1{font-weight:700} ol.x-0{list-style:none} .c2{font-style:italic}"))))

(deftest export-styles-become-tags-and-quill-classes
  (let [html (gh/export->html export)]
    (testing "bold and italic are tags, and a 400 weight is not bold"
      (is (str/includes? html "<strong>bold</strong>"))
      (is (str/includes? html "<strong><em>both</em></strong>"))
      (is (not (str/includes? html "<strong>Title"))))
    (testing "underline survives inside a list item"
      (is (str/includes? html "<u>nested</u>")))
    (testing "alignment is a Quill class and no Google class remains"
      (is (str/includes? html "<h1 class=\"ql-align-center\">"))
      (is (not (re-find #"class=\"c\d" html))))
    (testing "the three sibling lists are one ol in Quill's flat form"
      (is (= 1 (count (re-seq #"<ol" html))))
      (is (not (str/includes? html "<ul")))
      (is (re-find #"<li (?=[^>]*data-list=\"bullet\")(?=[^>]*class=\"ql-indent-1\")[^>]*><u>nested</u></li>" html))
      (is (str/includes? html "<li data-list=\"bullet\">two</li>")))
    (testing "a google.com redirect is unwrapped"
      (is (str/includes? html "href=\"https://example.org/a?b=1\"")))
    (testing "comment markers and the comment list are gone"
      (is (not (str/includes? html "[a]")))
      (is (not (str/includes? html "A comment"))))
    (testing "no style block is left"
      (is (not (str/includes? html "<style"))))))

(deftest the-stored-html-keeps-what-clean-html-would-strip
  (let [html (gdocs/stored-html export)]
    (is (str/includes? html "<strong>bold</strong>"))
    (is (str/includes? html "data-list=\"bullet\""))
    (is (str/includes? html "ql-indent-1"))
    (is (str/includes? html "ql-align-center"))))

(deftest unwrap-google-link-leaves-other-links-alone
  (is (= "https://x.org/" (gh/unwrap-google-link "https://www.google.com/url?q=https://x.org/&sa=D")))
  (is (= "https://x.org/" (gh/unwrap-google-link "https://x.org/")))
  (is (nil? (gh/unwrap-google-link nil))))

(deftest the-hash-ignores-image-links-and-nothing-else
  (let [a "<p>x</p><img src=\"https://lh7.googleusercontent.com/one\">"
        b "<p>x</p><img src=\"https://lh7.googleusercontent.com/two\">"
        c "<p>y</p><img src=\"https://lh7.googleusercontent.com/one\">"]
    (is (= (gh/content-hash a) (gh/content-hash b)))
    (is (not= (gh/content-hash a) (gh/content-hash c)))
    (is (= 64 (count (gh/content-hash a))))))
