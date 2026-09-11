(ns freememo.quill-markdown-test
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.quill-markdown :as markdown]))

(deftest block-shortcuts
  (testing "headings"
    (doseq [level (range 1 7)]
      (is (= {:marker-length (inc level)
              :format "header"
              :value level}
             (markdown/block-shortcut (str (apply str (repeat level "#")) " ")))))
    (is (= {:marker-length 2 :format "header" :value 1}
           (markdown/block-shortcut "# Pasted heading"))))
  (testing "other blocks"
    (is (= {:marker-length 2 :format "blockquote" :value true}
           (markdown/block-shortcut "> ")))
    (is (= {:marker-length 2 :format "blockquote" :value true}
           (markdown/block-shortcut (str ">" \u00a0))))
    (is (= {:marker-length 2 :format "list" :value "bullet"}
           (markdown/block-shortcut "* ")))
    (is (= {:marker-length 3 :format "list" :value "ordered"}
           (markdown/block-shortcut "1. ")))
    (is (= {:marker-length 4 :format "list" :value "unchecked"}
           (markdown/block-shortcut "[ ] ")))
    (is (= {:marker-length 4 :format "list" :value "checked"}
           (markdown/block-shortcut "[x] ")))
    (is (= {:marker-length 3 :format "code-block" :value "plain"}
           (markdown/block-shortcut "```")))))

(deftest block-markers-only-match-at-line-start
  (doseq [text ["text # " "####### " "a> " "a 12. item"]]
    (is (nil? (markdown/block-shortcut text)) text)))

(deftest inline-shortcuts
  (is (= {:kind "bold" :start 7 :marker-length 2 :content-length 4}
         (markdown/inline-shortcut "before **bold**")))
  (is (= {:kind "bold" :start 0 :marker-length 2 :content-length 4}
         (markdown/inline-shortcut "**bold** next")))
  (is (nil? (markdown/inline-shortcut "**bold__")))
  (is (= {:kind "strike" :start 0 :marker-length 2 :content-length 6}
         (markdown/inline-shortcut "~~strike~~")))
  (is (= {:kind "italic" :start 2 :marker-length 1 :content-length 6}
         (markdown/inline-shortcut "a *italic*")))
  (is (nil? (markdown/inline-shortcut "*italic_")))
  (is (= {:kind "code" :start 0 :marker-length 1 :content-length 4}
         (markdown/inline-shortcut "`code`")))
  (is (= {:kind "link" :start 0 :label-length 4
          :url "https://example.com" :tail-length 22}
         (markdown/inline-shortcut "[link](https://example.com)"))))

(deftest incomplete-inline-markers-stay-literal
  (doseq [text ["**" "****" "* *" "`" "~~" "[label](url with space)"]]
    (is (nil? (markdown/inline-shortcut text)) text)))

(deftest fenced-code-ranges
  (is (= [[0 2] [4 6]]
         (markdown/fenced-ranges ["```" "code" "```" "text"
                                  "```js" "more" "```"])))
  (is (= [] (markdown/fenced-ranges ["```" "unfinished"]))))
