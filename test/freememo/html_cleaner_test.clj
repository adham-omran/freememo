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

   {:control "Insert audio (clip, looping)"                ; audio_clip.cljc stored form
    :html "<p>a</p><audio controls loop src=\"/api/media/7\"></audio>"
    :keeps ["<audio" "controls" "loop" "/api/media/7"]}

   {:control "Insert audio (clip, not looping)"            ; same, loop absent
    :html "<audio controls src=\"/api/media/7\"></audio>"
    :keeps ["<audio" "controls" "/api/media/7"]}

   {:control "Cloze markers are plain text"                ; editor_actions.cljc:51-52
    :html "<p>Kaleida was funded to {{c1::$40 million}} in {{c2::1991}}</p>"
    :keeps ["{{c1::$40 million}}" "{{c2::1991}}"]}

   ;; The menu's Decrease/Increase indent buttons and the document editor's Tab
   ;; (a11y/install-tab-indent!) apply indent. Pasting Quill HTML reaches font.
   ;; Both are inside Quill's own whitelists — indent.ts:126, font.ts:81.
   {:control "Indent 1 / 5 / 8 on list items"
    :html (str "<ol><li data-list=\"ordered\" class=\"ql-indent-1\">a</li>"
            "<li data-list=\"ordered\" class=\"ql-indent-5\">b</li>"
            "<li data-list=\"ordered\" class=\"ql-indent-8\">c</li></ol>")
    :keeps ["ql-indent-1" "ql-indent-5" "ql-indent-8"]}

   {:control "Increase indent on a paragraph"               ; format_menu.cljs
    :html "<p class=\"ql-indent-2\">indented</p>"
    :keeps ["ql-indent-2"]}

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

   {:scenario "data: src on an audio clip — util/storable-image-mime-types note"
    :html "<audio controls src=\"data:text/html;base64,PHNjcmlwdD4=\"></audio>"
    :drops ["data:text/html"]}

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

(deftest a-fence-language-survives-on-pre
  ;; `freememo.repo-markdown` moves a fence's `language-x` class to
  ;; `data-language` on the `pre`, because the class is stripped.
  (is (str/includes? (cleaner/clean-html "<pre data-language=\"zig\"><code>x</code></pre>")
        "data-language=\"zig\""))
  (is (not (str/includes? (cleaner/clean-html "<pre data-language=\"a b\"><code>x</code></pre>")
             "data-language"))
    "a value that is not an identifier is dropped"))

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

;; ── Anchor targets ─────────────────────────────────────────────────
;; `id` is what an in-document anchor lands on. It is allowed only on the
;; blocks Quill can hold it on — its Parchment attributor is BLOCK-scoped, so
;; an `id` on a span or an `a` dies in the Delta on the first edit, and storing
;; it would promise a target that does not survive editing.

(deftest id-survives-on-the-blocks-quill-can-hold-it-on
  (doseq [tag ["h1" "h2" "h3" "h4" "h5" "h6" "p"]]
    (testing tag
      (let [out (cleaner/clean-html (str "<" tag " id=\"intro\">Heading</" tag ">"))]
        (is (str/includes? out "id=\"intro\"")
          (str tag " lost its anchor target → " out))))))

(deftest id-is-dropped-where-quill-cannot-keep-it
  (doseq [[scenario html] [["inline span" "<p><span id=\"s\">x</span></p>"]
                           ["anchor element" "<p><a id=\"a\" href=\"https://e.com\">x</a></p>"]
                           ["layout div" "<div id=\"d\">x</div>"]]]
    (testing scenario
      (is (not (str/includes? (cleaner/clean-html html) "id="))
        (str scenario " kept an id the editor would drop")))))

(deftest an-id-carries-no-other-attribute-with-it
  (testing "allowing id does not reopen the element to event handlers"
    (let [out (cleaner/clean-html "<h2 id=\"a\" onclick=\"steal()\" class=\"evil\">H</h2>")]
      (is (str/includes? out "id=\"a\""))
      (is (not (str/includes? out "onclick")))
      (is (not (str/includes? out "evil"))))))

(deftest anchor-href-shapes-the-editor-must-keep
  (doseq [[scenario html carrier] [["in-document fragment" "<a href=\"#intro\">g</a>" "href=\"#intro\""]
                                   ["relative with fragment" "<a href=\"../x/y.html#f\">r</a>" "href=\"../x/y.html#f\""]
                                   ["mail handler" "<a href=\"mailto:a@b.c\">m</a>" "href=\"mailto:a@b.c\""]
                                   ["phone handler" "<a href=\"tel:+1234\">t</a>" "href=\"tel:+1234\""]
                                   ["in-app topic link" "<a href=\"/viewer/topic/12#s\">t</a>" "href=\"/viewer/topic/12#s\""]]]
    (testing scenario
      (is (str/includes? (cleaner/clean-html html) carrier)
        (str scenario " lost its href")))))

(deftest script-bearing-hrefs-still-lose-the-attribute
  (doseq [[scenario html] [["javascript" "<a href=\"javascript:alert(1)\">j</a>"]
                           ["vbscript" "<a href=\"vbscript:x\">v</a>"]
                           ["data document" "<a href=\"data:text/html,x\">d</a>"]]]
    (testing scenario
      (is (not (str/includes? (cleaner/clean-html html) "href="))
        (str scenario " kept an executable href")))))

(deftest a-fragment-keeps-no-anchor-targets
  (testing "an extract or card field drops the ids a whole document keeps"
    (doseq [tag ["h1" "h2" "h3" "h4" "h5" "h6" "p"]]
      (let [html (str "<" tag " id=\"intro\">Heading</" tag ">")]
        (is (str/includes? (cleaner/clean-html html) "id=\"intro\"")
          (str tag ": the document variant keeps it"))
        (is (not (str/includes? (cleaner/clean-html-fragment html) "id="))
          (str tag ": the fragment variant must not"))))))

(deftest the-fragment-variant-differs-from-the-document-variant-only-in-id
  (doseq [{:keys [control html]} menu-format-fixtures]
    (testing control
      (is (= (cleaner/clean-html html) (cleaner/clean-html-fragment html))
        (str control " diverged between the two sanitizers")))))

(deftest clean-html-fragment-is-nil-preserving
  (is (nil? (cleaner/clean-html-fragment nil)))
  (is (= "" (cleaner/clean-html-fragment ""))))

(deftest generated-cards-carry-no-anchor-targets
  (testing "clean-html-llm inherits the fragment rule, not the document rule"
    (is (not (str/includes? (cleaner/clean-html-llm "<h2 id=\"x\">H</h2>") "id=")))))

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

(deftest cell-spans-are-removed-and-the-cells-survive
  (testing "Quill 2.0.3 has no representation for colspan or rowspan — cell merge
            and split are out of scope for the editor — so a span reaching storage
            is a ragged row waiting to happen. `table-lowering/rectangularize-rows!`
            expands colspan into real cells before this runs, and this removal
            closes the path for a caller that runs no lowering."
    (let [out (cleaner/clean-html
                (str "<table><tbody><tr>"
                  "<td colspan=\"2\" rowspan=\"3\">a</td>"
                  "<th colspan=\"4\" rowspan=\"5\">b</th>"
                  "</tr></tbody></table>"))]
      (is (not (str/includes? out "colspan")))
      (is (not (str/includes? out "rowspan")))
      (is (str/includes? out "<td>a</td>"))
      (is (str/includes? out "<th>b</th>")
        "the cells and their text survive; only the span attributes go")))
  (testing "Safelist/relaxed admits both on td and th already, so html_cleaner has
            to REMOVE them — dropping an addAttributes call would change nothing"
    (is (not (str/includes? (cleaner/clean-html "<td colspan=\"9\">x</td>") "9")))))

(deftest data-row-still-survives-on-a-cell
  (testing "Quill writes its own row identity there, and `clean-html` must keep
            it or an app-authored table loses the ids that stop its rows merging"
    (let [out (cleaner/clean-html "<table><tbody><tr><td data-row=\"row-ab12\">x</td></tr></tbody></table>")]
      (is (str/includes? out "data-row=\"row-ab12\"")))))

(deftest strip-ql-tokens-leaves-formats-alone
  (testing "the topic-content save path unwraps ql-token spans only"
    (let [out (cleaner/strip-ql-tokens
                (str "<p class=\"ql-align-right\">"
                  "<span class=\"ql-token hljs-keyword\">defn</span></p>"))]
      (is (not (str/includes? out "ql-token")))
      (is (str/includes? out "defn"))
      (is (str/includes? out "ql-align-right")))))
