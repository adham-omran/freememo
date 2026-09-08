(ns freememo.code-language-html-test
  "Needs NO database. `freememo.code-language-html` requires only
   `freememo.code-languages` (a no-requires vocabulary) and Jsoup, so this suite
   runs with nothing started — which is the reason the transform lives in its
   own namespace instead of beside the flashcard writes."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [freememo.code-language-html :as h]))

(def ^:private two-line-block
  (str "<div class=\"ql-code-block-container\" spellcheck=\"false\" data-language=\"python\">"
       "<div class=\"ql-code-block\" data-language=\"python\">x = 1</div>"
       "<div class=\"ql-code-block\" data-language=\"python\">y = 2</div>"
       "</div>"))

(def ^:private prose "<p>Before</p><h2>A heading</h2><p>After</p>")

(deftest counts-blocks-not-lines
  (testing "a container is one block however many lines it holds"
    (is (= 1 (h/code-block-count two-line-block))))
  (testing "two containers are two blocks"
    (is (= 2 (h/code-block-count (str two-line-block prose two-line-block)))))
  (testing "no code block"
    (is (= 0 (h/code-block-count prose)))
    (is (= 0 (h/code-block-count "")))
    (is (= 0 (h/code-block-count nil))))
  (testing "a line with no container ancestor still counts as a block"
    (is (= 1 (h/code-block-count "<div class=\"ql-code-block\">orphan</div>")))))

(deftest sets-every-line-in-every-block
  (let [out (h/set-code-language (str two-line-block prose two-line-block) "zig")]
    (is (= 4 (count (re-seq #"class=\"ql-code-block\" data-language=\"zig\"" out)))
      "every one of the four lines carries the new language")
    (is (not (str/includes? out "python"))
      "no line keeps the old language")))

(deftest adds-the-attribute-to-a-line-that-lacks-it
  (testing "a line reading as plain is exactly what this command repairs"
    (let [out (h/set-code-language "<div class=\"ql-code-block\">a = 1</div>" "zig")]
      (is (str/includes? out "data-language=\"zig\"")))))

(deftest never-adds-the-attribute-to-a-container-that-lacks-it
  (let [in  "<div class=\"ql-code-block-container\"><div class=\"ql-code-block\">a</div></div>"
        out (h/set-code-language in "zig")]
    (is (= 1 (count (re-seq #"data-language" out)))
      "the line gained the attribute and the container did not")
    (is (str/includes? out "<div class=\"ql-code-block-container\"><div"))))

(deftest updates-a-container-that-has-it
  (is (str/includes? (h/set-code-language two-line-block "zig")
        "class=\"ql-code-block-container\" spellcheck=\"false\" data-language=\"zig\"")))

(deftest leaves-everything-else-alone
  (let [out (h/set-code-language (str two-line-block prose) "zig")]
    (is (str/includes? out prose) "prose siblings are untouched"))
  (testing "inline formatting and attributes inside a code line survive"
    (let [in  "<div class=\"ql-code-block\" data-language=\"python\">a &lt; b</div>"
          out (h/set-code-language in "zig")]
      (is (str/includes? out "a &lt; b")))))

(deftest nil-html-is-nil
  (is (nil? (h/set-code-language nil "zig")))
  (is (nil? (h/relanguage-field nil "zig"))))

(deftest rejects-a-language-outside-the-vocabulary
  (testing "an unchecked value would reach stored HTML — the assert is the gate"
    (is (thrown? AssertionError (h/set-code-language two-line-block "perl")))
    (is (thrown? AssertionError (h/set-code-language two-line-block "")))
    (is (thrown? AssertionError (h/set-code-language two-line-block nil)))))

(deftest relanguage-field-reports-no-change-as-nil
  (testing "so an unchanged card never reaches update-flashcard-versioned!"
    (is (nil? (h/relanguage-field prose "zig")) "no code block")
    (is (nil? (h/relanguage-field (h/set-code-language two-line-block "zig") "zig"))
      "already that language"))
  (testing "a real change comes back as HTML"
    (is (some? (h/relanguage-field two-line-block "zig")))))

(deftest plain-is-a-legitimate-target
  (testing "clearing every block's highlighting is a thing users ask for"
    (is (str/includes? (h/set-code-language two-line-block "plain")
          "data-language=\"plain\""))))
