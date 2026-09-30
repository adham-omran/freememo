(ns freememo.repo-markdown
  "A repo `.md` file rendered for reading in the code pane
   (plans/markdown-rendered-code-pane.md).

   The stored source never changes. This namespace turns it into sanitized HTML
   at read time, and turns a selection of that HTML back into something an
   extract topic — a Quill document — can hold.

   Rendering is one pass over one Jsoup tree, in this order, and the order is
   load-bearing:

     1. flexmark, with math lifted out first (`md/render-with-lifted-math`),
        because flexmark's backslash escapes damage TeX.
     2. The rewrites below, BEFORE `clean-html`. Each one reads a class or an
        attribute the sanitizer strips: a fence's `language-x` class, a
        footnote item's `id` on `li`, a backref's class.
     3. `clean-html`, because a repository's Markdown may carry raw HTML.

   The link rewrite needs the database for one question — which of these paths
   are topics under the same commit — and asks it once per render. Everything
   else here is pure, so `repo_markdown_test` runs with the database down."
  (:require [clojure.string :as str]
            [freememo.code-language-html :as code-html]
            [freememo.code-languages :as langs]
            [freememo.doc-html :as doc-html]
            [freememo.forge :as forge]
            [freememo.html-cleaner :as cleaner]
            [freememo.markdown :as md])
  (:import [com.vladsch.flexmark.parser Parser]
           [com.vladsch.flexmark.html HtmlRenderer]
           [com.vladsch.flexmark.util.data MutableDataSet]
           [com.vladsch.flexmark.ext.tables TablesExtension]
           [com.vladsch.flexmark.ext.gfm.strikethrough StrikethroughExtension]
           [com.vladsch.flexmark.ext.gfm.tasklist TaskListExtension]
           [com.vladsch.flexmark.ext.footnotes FootnoteExtension]
           [org.jsoup Jsoup]
           [org.jsoup.nodes Document Element TextNode]))

;; The "document" profile. `freememo.markdown`'s profile stays as it is: its
;; callers (assistant replies, OCR, changelog mail) want no heading ids.
(defonce ^:private options
  (doto (MutableDataSet.)
    (.set Parser/EXTENSIONS [(TablesExtension/create)
                             (StrikethroughExtension/create)
                             (TaskListExtension/create)
                             (FootnoteExtension/create)])
    ;; GitHub-style slugs, so a `(#4-module-structure)` link written against
    ;; GitHub's rendering lands here too.
    (.set HtmlRenderer/GENERATE_HEADER_ID true)
    (.set HtmlRenderer/RENDER_HEADER_ID true)))

(defonce ^:private parser (.build (Parser/builder options)))
(defonce ^:private renderer (.build (HtmlRenderer/builder options)))

(defn- flexmark-html [md]
  (.render renderer (.parse parser md)))

(def ^:private language-re #"[A-Za-z0-9_-]+")

;; ── Rewrites before the sanitizer ──────────────────────────────────

(defn- fences->data-language!
  "Post: every `pre > code.language-x` is `pre[data-language=x] > code`, with no
   class left on the `code`. A language that is not an identifier is dropped."
  [^Element root]
  (doseq [^Element code (.select root "pre > code[class]")]
    (let [lang (some #(second (re-matches #"language-(.+)" %)) (.classNames code))]
      (.removeAttr code "class")
      (when (and lang (re-matches language-re lang))
        (.attr (.parent code) "data-language" ^String lang)))))

(defn- footnotes->anchorable!
  "Post: each footnote's `id` sits on its first `p`, the only block of the two
   that `clean-html` keeps an `id` on, and no backref link remains. A backref
   points at the reference's `sup`, which can carry no `id` after sanitizing,
   so keeping it would leave a dead link (§1.4, Q4)."
  [^Element root]
  (doseq [^Element a (.select root "a.footnote-backref")]
    (.remove a))
  (doseq [^Element li (.select root "li[id^=fn-]")]
    (when-let [^Element p (.selectFirst li "> p")]
      (.attr p "id" (.id li))
      (.removeAttr li "id"))))

(defn- tasks->markers!
  "Post: each task-list checkbox is the text `[x] ` or `[ ] `, and each `del`
   is an `s`, which `clean-html` keeps. The same two rewrites as
   `md/parse-markdown`."
  [^Element root]
  (doseq [^Element input (.select root "input[type=checkbox]")]
    ;; flexmark puts an `&nbsp;` after the box, which would double the gap.
    (when-let [^TextNode next-text (let [n (.nextSibling input)] (when (instance? TextNode n) n))]
      (.text next-text (str/replace (.getWholeText next-text) #"^[\s\u00A0]+" "")))
    (.replaceWith input (TextNode. (if (.hasAttr input "checked") "[x] " "[ ] "))))
  (doseq [^Element del (.select root "del")]
    (.tagName del "s")))

(defn- drop-images!
  "Post: no `img` remains. Images in repo Markdown are out of scope (Q1)."
  [^Element root]
  (doseq [^Element img (.select root "img")]
    (.remove img)))

;; ── Links ──────────────────────────────────────────────────────────

(def ^:private scheme-re #"^[A-Za-z][A-Za-z0-9+.-]*:.*")

(defn- normalize-segments
  "Resolve `.` and `..` in `segments`. Post: the segments, or nil when a `..`
   climbs out of the repository root."
  [segments]
  (reduce (fn [acc seg]
            (case seg
              ("" ".") acc
              ".." (if (seq acc) (pop acc) (reduced nil))
              (conj acc seg)))
    [] segments))

(defn- percent-decode [s]
  (try (java.net.URLDecoder/decode (str/replace s "+" "%2B") "UTF-8")
       (catch IllegalArgumentException _ s)))

(defn link-target
  "What `href`, written in the file at `file-path`, points at.

   Post: one of
     [:keep]                       a fragment, a URL with a scheme, or `//host`
     [:repo repo-path fragment]    a path inside the repository
     [:unwrap]                     a path that climbs out of the repository
   `fragment` is nil or the text after `#`. A query string is dropped: nothing
   in a repository serves one."
  [file-path href]
  (let [href (str/trim (str href))]
    (cond
      (or (str/blank? href) (str/starts-with? href "#")
        (str/starts-with? href "//") (re-matches scheme-re href))
      [:keep]

      :else
      (let [[path-part fragment] (str/split href #"#" 2)
            path-part (first (str/split path-part #"\?" 2))
            dir-segments (butlast (str/split (str file-path) #"/"))
            base (if (str/starts-with? path-part "/") [] dir-segments)
            segments (normalize-segments
                       (concat base (str/split (percent-decode path-part) #"/")))]
        (cond
          (nil? segments) [:unwrap]
          (empty? segments) [:unwrap]
          :else [:repo (str/join "/" segments) (not-empty fragment)])))))

(defn repo-link-paths
  "The repository paths the links under `root` name, so one query can resolve
   them all. Pre : `root` holds flexmark output for the file at `file-path`."
  [file-path ^Element root]
  (into #{}
    (keep (fn [^Element a]
            (let [[kind path] (link-target file-path (.attr a "href"))]
              (when (= :repo kind) path))))
    (.select root "a[href]")))

(defn- with-fragment [url fragment]
  (if fragment (str url "#" fragment) url))

(defn rewrite-links!
  "Point every link at where it resolves (§1.5).

   Pre : `path->topic-id` maps the repository paths that are topics to their ids.
         `origin` is {:forge :repo-url :commit-sha}, or nil when unknown.
   Post: a link to `file-path` itself reads `#fragment`, or is unwrapped when
         it names none. A path that is a topic reads
         `/viewer/topic/<id>[#fragment]`. Any other path reads the forge's page
         for it, or is unwrapped to its text when `origin` is nil or the path
         leaves the repository. Fragment and scheme links are unchanged."
  [^Element root file-path path->topic-id {:keys [forge repo-url commit-sha] :as origin}]
  (doseq [^Element a (.select root "a[href]")]
    (let [[kind path fragment] (link-target file-path (.attr a "href"))
          topic-id (get path->topic-id path)]
      (cond
        (= :keep kind) nil
        (= :unwrap kind) (.unwrap a)
        ;; The file itself: a navigation would land where it started and never
        ;; scroll, so it becomes the fragment it names.
        (= path file-path) (if fragment (.attr a "href" (str "#" fragment)) (.unwrap a))
        topic-id (.attr a "href" (with-fragment (str "/viewer/topic/" topic-id) fragment))
        (and origin forge repo-url commit-sha)
        (.attr a "href" (with-fragment (forge/blob-url forge repo-url commit-sha path) fragment))
        :else (.unwrap a)))))

;; ── Render ─────────────────────────────────────────────────────────

(defn- parse-body ^Document [html]
  (doto (Jsoup/parseBodyFragment html)
    (-> .outputSettings (.prettyPrint false))))

(defn render
  "Sanitized HTML for Markdown `source`, the file at `file-path`.

   Pre : `resolve-paths` takes a set of repository paths and returns
         {path topic-id} for those that are topics. It is called once.
         `origin` is as `rewrite-links!` takes it.
   Post: nil for a blank source. Otherwise HTML that has passed `clean-html`,
         with heading ids, fences as `pre[data-language]`, anchorable footnotes,
         no image, and every link resolved."
  [source file-path resolve-paths origin]
  (when-let [html (md/render-with-lifted-math source flexmark-html)]
    (let [doc (parse-body html)
          body (.body doc)]
      (fences->data-language! body)
      (footnotes->anchorable! body)
      (tasks->markers! body)
      (drop-images! body)
      (rewrite-links! body file-path (resolve-paths (repo-link-paths file-path body)) origin)
      (cleaner/clean-html (.html body)))))

;; ── Extract ────────────────────────────────────────────────────────

(defn- fences->quill-code-blocks!
  "Post: every `pre` is a Quill code-block container, keeping its language when
   `freememo.code-languages` knows it and reading `plain` otherwise. The text is
   the `pre`'s text, so the highlight.js token spans the pane added are gone."
  [^Element root]
  (doseq [^Element pre (.select root "pre")]
    (let [lang (.attr pre "data-language")
          lang (if (contains? langs/code-language-keys lang) lang "plain")
          text (str/replace (.wholeText pre) #"\n$" "")]
      (.before pre ^String (code-html/source->code-block-html text lang))
      (.remove pre))))

(defn selection->extract-html
  "HTML a user selected in the rendered pane, made into what an extract topic
   holds (§4.2).

   Pre : `html` is the pane's selected HTML — sanitized server output that the
         browser then highlighted, with math already turned back into TeX text.
   Post: nil iff `html` is nil. Otherwise HTML that has been through
         `doc-html/rewrite-for-quill!` (tables lowered) with each fence a Quill
         code block. It is NOT sanitized: `create-extract-topic-safe!` runs
         `clean-html-fragment` on it, and sanitizing twice is the trap
         `doc-html` names."
  [html]
  (when html
    (let [doc (parse-body html)
          body (.body doc)]
      (fences->quill-code-blocks! body)
      (doc-html/rewrite-for-quill! body)
      (.html body))))
