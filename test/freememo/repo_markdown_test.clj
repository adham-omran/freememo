(ns freememo.repo-markdown-test
  "The rendered view of a repo Markdown file (plans/markdown-rendered-code-pane.md).
   No database: `render` takes the path lookup as a function."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.repo-markdown :as rm])
  (:import [org.jsoup Jsoup]))

(def ^:private sha "0123456789abcdef0123456789abcdef01234567")

(def ^:private origin
  {:forge "github" :repo-url "https://github.com/o/r" :commit-sha sha})

(defn- render
  ([md] (render md {}))
  ([md topics] (rm/render md "grid/docs/ir-spec.md" #(select-keys topics %) origin)))

(defn- hrefs [html]
  (mapv #(.attr % "href") (.select (Jsoup/parseBodyFragment html) "a")))

(deftest a-blank-source-renders-nothing
  (is (nil? (render "")))
  (is (nil? (render nil))))

(deftest headings-carry-github-slugs
  (let [html (render "## 4. Module structure\n\n### `src/ir.zig` & WASM ABI (§10)\n")]
    (is (str/includes? html "id=\"4-module-structure\""))
    (is (str/includes? html "id=\"srcirzig--wasm-abi-10\""))))

(deftest tables-render
  (let [html (render "| Form | Where |\n|---|---|\n| EDN | `ir.clj` |\n")]
    (is (str/includes? html "<table>"))
    (is (str/includes? html "<th>Form</th>"))
    (is (str/includes? html "<code>ir.clj</code>"))))

(deftest a-fence-keeps-its-language-on-pre
  (let [html (render "```zig\nconst x = 1;\n```\n")]
    (is (str/includes? html "<pre data-language=\"zig\"><code>const x = 1;"))
    (is (not (str/includes? html "language-zig")))))

(deftest footnotes-are-anchorable-and-have-no-dead-backref
  (let [html (render "Text.[^1]\n\n[^1]: The note.\n")]
    (is (str/includes? html "href=\"#fn-1\""))
    (is (str/includes? html "<p id=\"fn-1\">The note.</p>")
      "the id moved from the li, which clean-html strips, to its p")
    (is (not (str/includes? html "fnref-1"))
      "the backref pointed at a sup that keeps no id")))

(deftest math-survives-as-tex
  (let [html (render "Inline $x^2$ and\n\n$$\\frac{a}{b}$$\n")]
    (is (str/includes? html "\\(x^2\\)"))
    (is (str/includes? html "\\[\\frac{a}{b}\\]"))))

(deftest images-script-and-strikethrough
  (let [html (render "![alt](img.png) ~~gone~~ <script>alert(1)</script>\n\n- [x] done\n")]
    (is (not (str/includes? html "<img")))
    (is (not (str/includes? html "<script")))
    (is (str/includes? html "<s>gone</s>"))
    (is (str/includes? html "[x] done"))))

(deftest link-target-classifies-each-href-shape
  (let [f "grid/docs/ir-spec.md"]
    (is (= [:keep] (rm/link-target f "#4-module-structure")))
    (is (= [:keep] (rm/link-target f "https://example.org/x")))
    (is (= [:keep] (rm/link-target f "mailto:a@b.c")))
    (is (= [:keep] (rm/link-target f "//cdn.example.org/x")))
    (is (= [:repo "grid/docs/ir-impl.md" nil] (rm/link-target f "ir-impl.md")))
    (is (= [:repo "grid/docs/ir-impl.md" "x"] (rm/link-target f "./ir-impl.md#x")))
    (is (= [:repo "grid/src/ir.zig" nil] (rm/link-target f "../src/ir.zig?plain=1")))
    (is (= [:repo "README.md" nil] (rm/link-target f "/README.md")))
    (is (= [:repo "grid/docs/my file.md" nil] (rm/link-target f "my%20file.md")))
    (is (= [:repo "grid/src" nil] (rm/link-target f "../src/")))
    (is (= [:unwrap] (rm/link-target f "../../../outside.md")))))

(deftest links-resolve-to-topics-then-the-forge
  (let [html (render (str "[a](ir-impl.md#x) [b](../src/ir.zig) [c](#4-x) "
                       "[d](https://example.org) [e](../../../out.md)")
               {"grid/docs/ir-impl.md" 42})]
    (is (= ["/viewer/topic/42#x"
            (str "https://github.com/o/r/blob/" sha "/grid/src/ir.zig")
            "#4-x"
            "https://example.org"]
          (hrefs html)))
    (is (str/includes? html " e") "a link out of the repository keeps its text")))

(deftest a-link-to-the-file-itself-is-a-fragment
  (is (= ["#x"] (hrefs (render "[a](ir-spec.md#x) [b](./ir-spec.md)" {"grid/docs/ir-spec.md" 7})))))

(deftest links-resolve-in-one-lookup
  (let [!calls (atom [])]
    (rm/render "[a](a.md) [b](b.md) [a again](a.md)" "docs/x.md"
      (fn [paths] (swap! !calls conj paths) {}) origin)
    (is (= [#{"docs/a.md" "docs/b.md"}] @!calls))))

(deftest a-missing-file-with-no-origin-is-unwrapped
  (let [html (rm/render "[a](a.md)" "docs/x.md" (constantly {}) nil)]
    (is (= [] (hrefs html)))
    (is (str/includes? html ">a<"))))

(deftest a-selection-becomes-quill-shaped
  (let [out (rm/selection->extract-html
              (str "<p>Intro</p>"
                "<pre data-language=\"zig\"><code class=\"hljs\">"
                "<span class=\"hljs-keyword\">const</span> x = 1;\nconst y;\n</code></pre>"
                "<pre data-language=\"nope\"><code>z</code></pre>"))]
    (is (str/includes? out "<div class=\"ql-code-block\" data-language=\"zig\">const x = 1;</div>"))
    (is (str/includes? out "<div class=\"ql-code-block\" data-language=\"zig\">const y;</div>"))
    (is (str/includes? out "data-language=\"plain\">z</div>")
      "a language outside freememo.code-languages reads plain")
    (is (not (str/includes? out "<pre")))
    (is (not (str/includes? out "hljs-keyword")))))
