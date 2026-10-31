(ns freememo.wikipedia-test
  "Wikipedia formula import checks. These checks need no database."
  (:require [clj-http.client :as http]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.desktop-link :as desktop-link]
            [freememo.math :as math]
            [freememo.url-validate :as url-validate]
            [freememo.wikipedia :as wikipedia]
            [freememo.youtube-url :as yt])
  (:import [org.jsoup Jsoup]))

(defn- formula-html
  [annotation-html display? image-name]
  (let [placement (if display? "display" "inline")]
    (str "<span class=\"mwe-math-element\" typeof=\"mw:Extension/math\""
      " data-mw='{\"name\":\"math\",\"attrs\":{},\"body\":{\"extsrc\":\"original-source\"}}'>"
      "<span class=\"mwe-math-mathml-" placement " mwe-math-mathml-a11y\">"
      "<math xmlns=\"http://www.w3.org/1998/Math/MathML\""
      (when display? " display=\"block\"") ">"
      "<semantics><mrow><mi>MathML duplicate</mi></mrow>"
      (when (some? annotation-html)
        (str "<annotation encoding=\"application/x-tex\">"
          annotation-html "</annotation>"))
      "</semantics></math></span>"
      "<img class=\"mwe-math-fallback-image-" placement "\""
      " src=\"//upload.wikimedia.org/" image-name ".svg\""
      " alt=\"{\\displaystyle fallback}\"/></span>")))

(defn- imported-article
  [body-html]
  (with-redefs [http/get (fn [_ _] {:status 200 :body body-html})
                url-validate/safe-url? (constantly true)
                desktop-link/offline-message (constantly nil)
                wikipedia/fetch-wikipedia-summary (constantly nil)]
    (let [article (wikipedia/fetch-url "https://en.wikipedia.org/wiki/Torus")]
      (is (true? (:success article)) (pr-str article))
      (is (= "wikipedia" (:source-type article)))
      article)))

(defn- parsed-import
  [body-html]
  (Jsoup/parse (:html (imported-article body-html))))

(defn- formula-sources
  [document]
  (mapv second (re-seq math/tex-region-re (.wholeText (.body document)))))

(deftest normalized-annotations-become-one-stored-formula-each
  (let [doc (parsed-import
              (str "<p>Area "
                (formula-html "{\\displaystyle A=4\\pi^{2}Rr}" false "area")
                " and volume "
                (formula-html "{\\displaystyle V=2\\pi^{2}Rr^{2}}" false "volume")
                ".</p>"))]
    (is (= ["{\\displaystyle A=4\\pi^{2}Rr}"
            "{\\displaystyle V=2\\pi^{2}Rr^{2}}"]
          (formula-sources doc)))
    (is (.isEmpty (.select doc "math, annotation, img")))
    (is (not (str/includes? (.text doc) "MathML duplicate")))
    (is (not (str/includes? (.text doc) "original-source")))
    (is (not (str/includes? (.text doc) "fallback")))))

(deftest formulas-retain-tex-operators-entities-braces-and-backslashes
  (let [tex-html "{\\displaystyle \\begin{aligned}x&amp;=\\{a&lt;b\\}\\\\y&amp;=c&gt;d\\end{aligned}}"
        tex "{\\displaystyle \\begin{aligned}x&=\\{a<b\\}\\\\y&=c>d\\end{aligned}}"
        article (imported-article (str "<p>" (formula-html tex-html false "operators") "</p>"))
        doc (Jsoup/parse (:html article))]
    (is (= [tex] (formula-sources doc)))
    (testing "HTML-sensitive source remains text after another HTML parse"
      (is (= [tex] (formula-sources (Jsoup/parse (.html (.body doc)))))))
    (is (str/includes? (:html article) "&lt;b"))
    (is (str/includes? (:html article) "&amp;="))
    (is (.isEmpty (.select doc "b, math, img")))))

(deftest formula-source-whitespace-survives-document-conversion
  (let [tex-html "{\\displaystyle \\begin{aligned}\nx&amp;=\\text{two  spaces}\t\\\\\ny&amp;=\\text{A &amp; B}\\end{aligned}}"
        tex "{\\displaystyle \\begin{aligned}\nx&=\\text{two  spaces}\t\\\\\ny&=\\text{A & B}\\end{aligned}}"
        doc (parsed-import (str "<p>" (formula-html tex-html true "whitespace") "</p>"))]
    (is (= [tex] (formula-sources doc)))
    (is (.isEmpty (.select doc "math, img")))))

(deftest formulas-without-usable-tex-retain-only-their-own-fallback
  (let [doc (parsed-import
              (str "<p>"
                (formula-html "x^{2}" false "converted")
                (formula-html nil false "missing")
                (formula-html " \n\t " true "blank")
                (str/replace (formula-html "not-tex" false "wrong-encoding")
                  "application/x-tex" "text/plain")
                "</p>"))]
    (is (= ["x^{2}"] (formula-sources doc)))
    (is (= ["https://upload.wikimedia.org/missing.svg"
            "https://upload.wikimedia.org/blank.svg"
            "https://upload.wikimedia.org/wrong-encoding.svg"]
          (mapv #(.attr % "src") (.select doc "img"))))
    (is (.isEmpty (.select doc "math, annotation")))
    (is (not (str/includes? (.text doc) "MathML duplicate"))))
  (testing "a fallback without a MathML element remains an image"
    (let [doc (parsed-import
                "<p><img class=\"mwe-math-fallback-image-inline\" src=\"//upload.wikimedia.org/standalone.svg\" alt=\"fallback\"></p>")]
      (is (= "https://upload.wikimedia.org/standalone.svg"
            (.attr (.selectFirst doc "img") "src")))
      (is (empty? (formula-sources doc))))))

(deftest inline-formulas-preserve-words-links-and-ordinary-images
  (let [doc (parsed-import
              (str "<p>Start " (formula-html "x+y" false "sum")
                " then <a href=\"./Circle\">circle</a> and "
                (formula-html "z" false "variable") " end.</p>"
                "<p><img src=\"//upload.wikimedia.org/torus.png\" alt=\"A torus\"></p>"))]
    (is (= "Start \\(x+y\\) then circle and \\(z\\) end."
          (.text (.selectFirst doc "p"))))
    (is (= "https://en.wikipedia.org/wiki/Circle"
          (.attr (.selectFirst doc "a") "href")))
    (is (= ["https://upload.wikimedia.org/torus.png"]
          (mapv #(.attr % "src") (.select doc "img"))))
    (is (= "A torus" (.attr (.selectFirst doc "img") "alt")))))

(deftest standalone-equations-remain-in-separate-paragraphs
  (doseq [[opening closing] [["<dl><dd>" "</dd></dl>"]
                             ["<p>" "</p>"]]]
    (let [doc (parsed-import
                (str "<p>Before the equation.</p>" opening
                  (formula-html "{\\displaystyle \\begin{aligned}x&amp;=1\\\\y&amp;=2\\end{aligned}}"
                    true "aligned")
                  closing "<p>After the equation.</p>"))
          paragraphs (mapv #(.text %) (.select doc "p"))]
      (is (= ["Before the equation."
              "\\({\\displaystyle \\begin{aligned}x&=1\\\\y&=2\\end{aligned}}\\)"
              "After the equation."]
            paragraphs)
        (str "Equation container: " opening))
      (is (= 1 (count (formula-sources doc))))
      (is (.isEmpty (.select doc "img"))))))

(deftest display-equations-split-nested-formatting-without-losing-prose
  (let [doc (parsed-import
              (str "<p id=\"parameters\">Start <em>italic <strong>"
                "<a href=\"./Torus#Geometry\" title=\"Geometry\">linked before "
                (formula-html "x=1" true "first-display")
                " between " (formula-html "y=2" true "second-display")
                " linked after</a></strong> italic after</em> end.</p>"))
        paragraphs (vec (.select doc "p"))
        links (.select doc "p > em > strong > a")]
    (is (= ["Start italic linked before" "\\(x=1\\)" "between"
            "\\(y=2\\)" "linked after italic after end."]
          (mapv #(.text %) paragraphs)))
    (is (= ["x=1" "y=2"] (formula-sources doc)))
    (is (= [0 0] (mapv #(.childrenSize %) [(nth paragraphs 1) (nth paragraphs 3)]))
      "Each equation paragraph contains only its stored formula text.")
    (is (= ["linked before" "between" "linked after"] (mapv #(.text %) links)))
    (is (= (repeat 3 "https://en.wikipedia.org/wiki/Torus#Geometry")
          (map #(.attr % "href") links)))
    (is (= (repeat 3 "Geometry") (map #(.attr % "title") links)))
    (is (= ["italic linked before" "between" "linked after italic after"]
          (mapv #(.text %) (.select doc "p > em"))))
    (is (= 1 (.size (.select doc "#parameters"))))
    (is (= "parameters" (.id (first paragraphs))))
    (is (.isEmpty (.select doc "math, img")))))

(deftest formulas-survive-table-and-list-conversion-in-source-order
  (let [doc (parsed-import
              (str "<table><tbody><tr><td>Radius "
                (formula-html "R" false "radius")
                "</td><td>Value " (formula-html "r" false "minor-radius")
                "</td></tr></tbody></table>"
                "<ul><li>Surface " (formula-html "A" false "surface")
                "</li><li>Volume " (formula-html "V" false "capacity")
                "</li></ul>"))]
    (is (= ["R" "r" "A" "V"] (formula-sources doc)))
    (is (= ["Radius \\(R\\)" "Value \\(r\\)"]
          (mapv #(.text %) (.select doc "td"))))
    (is (= ["Surface \\(A\\)" "Volume \\(V\\)"]
          (mapv #(.text %) (.select doc "li"))))
    (is (.isEmpty (.select doc "math, img")))))

(deftest a-youtube-link-is-refused-before-any-request
  (with-redefs [http/get (fn [& _] (throw (ex-info "fetched" {})))
                desktop-link/offline-message (constantly nil)]
    (doseq [url ["https://www.youtube.com/watch?v=dQw4w9WgXcQ"
                 "https://youtu.be/dQw4w9WgXcQ"]]
      (is (= {:success false :error yt/rejection-message}
            (wikipedia/fetch-url url))
        url))))
