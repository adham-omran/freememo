(ns freememo.html-cleaner-test
  "Asserts freememo.html-cleaner/clean-html preserves every format the custom
   format menu (freememo.format-menu) can apply, and still strips what it must.

   Why fixtures and not generation: the emitting side is Quill 2.0.3 in the
   browser, the sanitizing side is Jsoup on the JVM, so the coupling cannot be
   a shared constant and cannot be exercised by driving a real editor from
   here. Each fixture is therefore hand-authored to match Quill's emitted DOM,
   with the upstream source that fixes its shape cited beside it — a Quill
   upgrade diffs those files against these fixtures.

   Motivating regression: `ql-align-right`, `ql-direction-rtl` and inline text
   `color` were all dropped on every card save while `ql-align-center` and
   `background-color` survived, so Align Right, RTL and every text-colour
   preset silently reverted when a card was reopened."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.html-cleaner :as cleaner])
  (:import [org.jsoup Jsoup]))

;; ---------------------------------------------------------------------------
;; Fixtures — one entry per format-menu control (format_menu.cljs:332-393).
;; :keeps are the substrings that carry the format; each MUST survive verbatim.
;; ---------------------------------------------------------------------------

(def ^:private menu-format-fixtures
  [{:control "Block type — Heading 1/2/3"                  ; header.ts:144
    :html "<h1>A</h1><h2>B</h2><h3>C</h3>"
    :keeps ["<h1>" "<h2>" "<h3>"]}

   {:control "Block type — Code block"                     ; code.ts:46-50
    :html (str "<div class=\"ql-code-block-container\" spellcheck=\"false\">"
            "<div class=\"ql-code-block\" data-language=\"clojure\">(inc 1)</div></div>")
    :keeps ["ql-code-block-container" "ql-code-block" "data-language=\"clojure\""]}

   {:control "Size — small/large/huge"                     ; size.ts:66-69 (SizeClass)
    :html (str "<span class=\"ql-size-small\">a</span>"
            "<span class=\"ql-size-large\">b</span>"
            "<span class=\"ql-size-huge\">c</span>")
    :keeps ["ql-size-small" "ql-size-large" "ql-size-huge"]}

   {:control "Text colour preset"                          ; color.ts:46 (ColorStyle)
    :html "<span style=\"color: rgb(224, 62, 62)\">a</span>"
    :keeps ["color: rgb(224, 62, 62)"]}

   {:control "Text colour preset, hex form"                ; same, pre-serialization
    :html "<span style=\"color: #e03e3e\">a</span>"
    :keeps ["color: #e03e3e"]}

   {:control "Background colour preset"                    ; background.ts:58
    :html "<span style=\"background-color: rgb(251, 228, 228)\">a</span>"
    :keeps ["background-color: rgb(251, 228, 228)"]}

   {:control "Text + background on one span"
    :html "<span style=\"color: #e03e3e; background-color: #fbe4e4\">a</span>"
    :keeps ["color: #e03e3e" "background-color: #fbe4e4"]}

   {:control "Bold / Italic / Underline / Strike"
    :html "<strong>a</strong><em>b</em><u>c</u><s>d</s>"
    :keeps ["<strong>" "<em>" "<u>" "<s>"]}

   {:control "Inline code"                                 ; code.ts:43
    :html "<p><code>conj</code></p>"
    :keeps ["<code>"]}

   {:control "Bullet list"                                 ; list.ts:8,13,53 + attachUI
    :html (str "<ol><li data-list=\"bullet\">"
            "<span class=\"ql-ui\" contenteditable=\"false\"></span>a</li></ol>")
    :keeps ["<ol>" "data-list=\"bullet\"" "ql-ui"]}

   {:control "Numbered list"
    :html "<ol><li data-list=\"ordered\">a</li></ol>"
    :keeps ["data-list=\"ordered\""]}

   {:control "Align — Center"                              ; align.ts:10 (AlignClass)
    :html "<p class=\"ql-align-center\">a</p>"
    :keeps ["ql-align-center"]}

   {:control "Align — Right"                               ; align.ts:6 whitelist
    :html "<p class=\"ql-align-right\">a</p>"
    :keeps ["ql-align-right"]}

   {:control "Align — Justify"
    :html "<p class=\"ql-align-justify\">a</p>"
    :keeps ["ql-align-justify"]}

   {:control "Right-to-left"                               ; direction.ts:19,23
    :html "<p class=\"ql-direction-rtl\">ا</p>"
    :keeps ["ql-direction-rtl"]}

   {:control "Align Right + RTL together"                  ; quill.snow.css pairs them
    :html "<p class=\"ql-direction-rtl ql-align-right\">ا</p>"
    :keeps ["ql-direction-rtl" "ql-align-right"]}

   ;; `class` is allow-listed per tag (html_cleaner `class-tags`), so every
   ;; block element Quill can apply a block format to needs its own coverage —
   ;; a per-tag omission would strip alignment on headings or list items only.
   {:control "Block formats on every tag that can carry them"
    :html (str "<h2 class=\"ql-align-right\">a</h2>"
            "<ol><li data-list=\"ordered\" class=\"ql-direction-rtl\">b</li></ol>"
            "<table><tbody><tr><td class=\"ql-align-center\">c</td></tr></tbody></table>"
            "<blockquote class=\"ql-align-justify\">d</blockquote>"
            "<div class=\"ql-code-block ql-align-right\" data-language=\"plain\">e</div>")
    :keeps ["<h2 class=\"ql-align-right\">" "ql-direction-rtl"
            "<td class=\"ql-align-center\">" "<blockquote class=\"ql-align-justify\">"
            "ql-code-block ql-align-right"]}

   {:control "Insert table"                                ; table.ts:7,12,61,121,128
    :html (str "<table><tbody><tr data-row=\"a1b2\">"
            "<td data-row=\"a1b2\">x</td></tr></tbody></table>")
    :keeps ["<table>" "<tbody>" "data-row=\"a1b2\"" "<td"]}

   {:control "Insert image (re-hosted, relative src)"      ; resize module writes width
    :html "<p><img src=\"/api/media/12\" width=\"300\"></p>"
    :keeps ["/api/media/12" "width=\"300\""]}

   {:control "Cloze markers are plain text"                ; editor_actions.cljc:51-52
    :html "<p>Kaleida was funded to {{c1::$40 million}} in {{c2::1991}}</p>"
    :keeps ["{{c1::$40 million}}" "{{c2::1991}}"]}

   ;; Not reachable from the menu (no indent or font control; Quill's Tab
   ;; bindings are removed in a11y/free-quill-tab!) but reachable by pasting
   ;; Quill HTML, and inside Quill's own whitelists — indent.ts:126, font.ts:81.
   {:control "Indent 1 / 5 / 8 on list items"
    :html (str "<ol><li data-list=\"ordered\" class=\"ql-indent-1\">a</li>"
            "<li data-list=\"ordered\" class=\"ql-indent-5\">b</li>"
            "<li data-list=\"ordered\" class=\"ql-indent-8\">c</li></ol>")
    :keeps ["ql-indent-1" "ql-indent-5" "ql-indent-8"]}

   {:control "Font serif / monospace"
    :html (str "<span class=\"ql-font-serif\">a</span>"
            "<span class=\"ql-font-monospace\">b</span>")
    :keeps ["ql-font-serif" "ql-font-monospace"]}

   ;; Import shapes that share the same allow-list (wikipedia.clj:180,
   ;; epub.clj:315) — RTL articles rely on dir/lang surviving.
   {:control "Imported RTL block keeps dir + lang"
    :html "<p dir=\"rtl\" lang=\"ar\">مرحبا</p>"
    :keeps ["dir=\"rtl\"" "lang=\"ar\""]}])

(def ^:private must-strip-fixtures
  [{:scenario "ql-cursor — transient editor state"
    :html "<p><span class=\"ql-cursor\">﻿</span>a</p>"
    :drops ["ql-cursor"]}

   {:scenario "ql-resize-style — why the resize toolbar is omitted"
    :html "<p><img src=\"/api/media/1\" class=\"ql-resize-style\"></p>"
    :drops ["ql-resize-style"]}

   {:scenario "hljs token OUTSIDE a code block — carve-out does not leak"
    :html "<p><span class=\"hljs-keyword\">defn</span></p>"
    :drops ["hljs-keyword"]}

   {:scenario "script element"
    :html "<p>a</p><script>alert(1)</script>"
    :drops ["<script" "alert(1)"]}

   {:scenario "event handler attribute"
    :html "<p onclick=\"alert(1)\">a</p>"
    :drops ["onclick"]}

   {:scenario "javascript: href"
    :html "<a href=\"javascript:alert(1)\">a</a>"
    :drops ["javascript:"]}

   {:scenario "language picker <select> inside a code block"
    :html (str "<div class=\"ql-code-block-container\">"
            "<select class=\"ql-ui\"><option>Plain</option><option>Clojure</option></select>"
            "<div class=\"ql-code-block\">x</div></div>")
    :drops ["Plain" "Clojure" "<option"]}

   {:scenario "colour value that is a function call"
    :html "<span style=\"color: expression(1)\">a</span>"
    :drops ["expression"]}

   {:scenario "url() in a colour-shaped property"
    :html "<span style=\"background-color: url(http://evil/x)\">a</span>"
    :drops ["url("]}

   {:scenario "shorthand background is not a safe colour prop"
    :html "<span style=\"background: url(http://evil/x)\">a</span>"
    :drops ["url("]}

   {:scenario "colour keyword that erases the text"
    :html "<span style=\"color: transparent\">a</span>"
    :drops ["transparent"]}

   {:scenario "colour keywords that defer to an unknown context"
    :html (str "<span style=\"color: inherit\">a</span>"
            "<span style=\"background-color: currentColor\">b</span>"
            "<p style=\"color: unset\">c</p>")
    :drops ["inherit" "currentColor" "unset"]}

   {:scenario "non-colour declaration smuggled beside a colour one"
    :html "<span style=\"color: red; position: fixed\">a</span>"
    :drops ["position"]}

   {:scenario "unknown ql-* class is not admitted by prefix"
    :html "<p class=\"ql-align-left ql-font-sans\">a</p>"
    :drops ["ql-align-left" "ql-font-sans"]}

   {:scenario "out-of-whitelist indent level"
    :html "<li data-list=\"ordered\" class=\"ql-indent-9\">a</li>"
    :drops ["ql-indent-9"]}

   {:scenario "data-list value outside bullet/ordered"
    :html "<li data-list=\"checked\">a</li>"
    :drops ["data-list"]}])

;; ---------------------------------------------------------------------------
;; Assertions
;; ---------------------------------------------------------------------------

(deftest menu-formats-survive-clean-html
  (doseq [{:keys [control html keeps]} menu-format-fixtures]
    (testing control
      (let [out (cleaner/clean-html html)]
        (doseq [carrier keeps]
          (is (str/includes? out carrier)
            (str control " lost " (pr-str carrier) " → " out)))))))

(deftest unsafe-and-transient-markup-is-stripped
  (doseq [{:keys [scenario html drops]} must-strip-fixtures]
    (testing scenario
      (let [out (cleaner/clean-html html)]
        (doseq [carrier drops]
          (is (not (str/includes? out carrier))
            (str scenario " kept " (pr-str carrier) " → " out)))))))

(deftest code-block-carve-out-passes-syntax-spans
  (testing "hljs spans and arbitrary data-language survive INSIDE the container"
    (let [out (cleaner/clean-html
                (str "<div class=\"ql-code-block-container\">"
                  "<div class=\"ql-code-block\" data-language=\"clojure\">"
                  "<span class=\"hljs-keyword\">defn</span> "
                  "<span class=\"hljs-title function_\">f</span></div></div>"))]
      (is (str/includes? out "hljs-keyword"))
      (is (str/includes? out "hljs-title function_"))
      (is (str/includes? out "data-language=\"clojure\"")))))

(deftest format-classes-land-on-their-own-element
  (testing "a surviving token is a real class, not a substring in some attribute"
    ;; The fixture assertions above are substring matches, which would also pass
    ;; if a token survived inside the wrong attribute or on the wrong element.
    ;; Re-parse the output and check class membership structurally instead.
    (let [doc (Jsoup/parseBodyFragment
                (cleaner/clean-html
                  (str "<p class=\"ql-direction-rtl ql-align-right\">a</p>"
                    "<h2 class=\"ql-align-center\">b</h2>"
                    "<span class=\"ql-size-huge ql-font-monospace\">c</span>"
                    "<ol><li data-list=\"ordered\" class=\"ql-indent-5\">d</li></ol>")))
          has-class? (fn [selector cls]
                       (some-> (.selectFirst doc selector) (.hasClass cls)))]
      (is (has-class? "p" "ql-direction-rtl"))
      (is (has-class? "p" "ql-align-right"))
      (is (has-class? "h2" "ql-align-center"))
      (is (has-class? "span" "ql-size-huge"))
      (is (has-class? "span" "ql-font-monospace"))
      (is (has-class? "li" "ql-indent-5"))
      (is (= "ordered" (some-> (.selectFirst doc "li") (.attr "data-list")))))))

(deftest clean-html-is-nil-preserving
  (testing "nil in, nil out — callers pass optional card fields straight through"
    (is (nil? (cleaner/clean-html nil)))))

(deftest clean-html-llm-inherits-the-allow-list
  (testing "colour and align survive the LLM variant; <img> still does not"
    (let [out (cleaner/clean-html-llm
                (str "<p class=\"ql-align-right\">"
                  "<span style=\"color: #e03e3e\">a</span>"
                  "<img src=\"/api/media/1\"></p>"))]
      (is (str/includes? out "ql-align-right"))
      (is (str/includes? out "color: #e03e3e"))
      (is (not (str/includes? out "<img"))))))

(deftest strip-ql-tokens-leaves-formats-alone
  (testing "the topic-content save path unwraps ql-token spans only"
    (let [out (cleaner/strip-ql-tokens
                (str "<p class=\"ql-align-right\">"
                  "<span class=\"ql-token hljs-keyword\">defn</span></p>"))]
      (is (not (str/includes? out "ql-token")))
      (is (str/includes? out "defn"))
      (is (str/includes? out "ql-align-right")))))
