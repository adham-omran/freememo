(ns freememo.gdoc-html
  "Google Docs export HTML, made into the HTML a snapshot stores
   (plans/google-docs-pull.md, C1).

   A Drive `text/html` export carries its formatting in a `<style>` block, as
   classes: `.c3{font-weight:700}` and `<span class=\"c3\">`. `clean-html` keeps
   only Quill's class tokens, so it would strip every one of them, and every
   bold word, italic and list level would be lost on the way in. This namespace
   therefore runs FIRST, while the classes still mean something, and turns
   them into the tags and Quill classes the rest of the pipeline keeps:

     font-weight ≥ 600      → <strong>
     font-style: italic     → <em>
     text-decoration        → <u>, <s>
     vertical-align         → <sup>, <sub>
     text-align             → ql-align-*
     a `lst-kix_…-N` class  → ql-indent-N on the <li>
     <ul>/<ol> runs         → one <ol> of <li data-list=…>, Quill's flat form

   Google writes each list level as its own `<ul>` or `<ol>` sibling. Quill
   stores a list as ONE `<ol>` whose items carry `data-list` and an indent
   class, and the `.fm-rich` list rules render exactly that form, so a run of
   sibling lists becomes one `<ol>`. Numbering then continues across levels
   the way it does in the Doc.

   Two more rewrites remove what Google adds around the content. A link is
   wrapped as `https://www.google.com/url?q=<target>&…`, and this unwraps it.
   Comments export as `[a]` markers in the text plus a list at the end, and
   this removes both, because a snapshot holds the Doc and not its discussion.

   Inv : does NOT sanitize. The caller still owes `doc-html/rewrite-for-quill!`
         and then `clean-html`, in that order."
  (:require [clojure.string :as str])
  (:import (org.jsoup Jsoup)
           (org.jsoup.nodes Document Element)
           (java.net URLDecoder)))

(defn- declarations
  "Post: {property value} for a CSS declaration block such as
   `font-weight:700;color:#000000`. Properties are lower case."
  [block]
  (into {}
    (keep (fn [decl]
            (let [[p v] (str/split decl #":" 2)]
              (when (and p v)
                [(str/lower-case (str/trim p)) (str/lower-case (str/trim v))]))))
    (str/split (or block "") #";")))

(defn class-styles
  "The rules of a Google export's `<style>` text that style one class.

   Post: {class-name {property value}}. A rule whose selector is anything other
         than a bare `.name` is ignored — Google writes the formatting of the
         content as single-class rules, and the other rules style the page."
  [css]
  (reduce
    (fn [acc [_ selector block]]
      (let [selector (str/trim selector)]
        (if-let [[_ cls] (re-matches #"\.([A-Za-z0-9_-]+)" selector)]
          (update acc cls merge (declarations block))
          acc)))
    {}
    (re-seq #"([^{}]+)\{([^}]*)\}" (or css ""))))

(defn- style-of
  "Post: the merged declarations of every class of `el`."
  [styles ^Element el]
  (transduce (map #(get styles % {})) merge {} (.classNames el)))

(defn- bold? [decls]
  (when-let [w (get decls "font-weight")]
    (or (= w "bold")
        (when-let [[_ n] (re-matches #"(\d+)" w)]
          (>= (Long/parseLong n) 600)))))

(defn- inline-wrappers
  "Post: the tag names a span with `decls` stands for, outermost first."
  [decls]
  (let [decoration (get decls "text-decoration" "")
        valign (get decls "vertical-align" "")]
    (cond-> []
      (bold? decls) (conj "strong")
      (= "italic" (get decls "font-style")) (conj "em")
      (str/includes? decoration "underline") (conj "u")
      (str/includes? decoration "line-through") (conj "s")
      (= "super" valign) (conj "sup")
      (= "sub" valign) (conj "sub"))))

(defn- spans->semantic-tags!
  "Post: every styled `span` is replaced by the nested tags its classes stand
   for, around its children. A span that stands for nothing is unwrapped."
  [styles ^Element root]
  (doseq [^Element span (reverse (vec (.select root "span")))]
    (let [tags (inline-wrappers (style-of styles span))]
      (if (empty? tags)
        (.unwrap span)
        (let [outer (.ownerDocument span)
              [first-el :as els] (mapv #(.createElement ^Document outer ^String %) tags)
              inner (peek els)]
          (doseq [[^Element parent ^Element child] (partition 2 1 els)]
            (.appendChild parent child))
          (doseq [n (vec (.childNodes span))]
            (.appendChild ^Element inner n))
          (.replaceWith span first-el))))))

(def ^:private align-classes
  {"center" "ql-align-center" "right" "ql-align-right" "justify" "ql-align-justify"})

(defn- list-level
  "Post: the nesting level a Google list class names, or nil. Google names a
   list `lst-kix_<id>-<level>`, level 0 being the outermost."
  [^Element el]
  (some (fn [c] (some-> (re-matches #"lst-kix_.*-(\d+)" c) second Long/parseLong))
    (.classNames el)))

(defn- blocks->quill-classes!
  "Post: a block that Google aligns carries the `ql-align-*` class, a nested
   list item carries `ql-indent-N`, and no block keeps a Google class."
  [styles ^Element root]
  ;; Every class is read before any is written: an <li>'s level is on its
  ;; parent list, which comes first in document order.
  (let [planned (mapv (fn [^Element el]
                        (let [align (align-classes (get (style-of styles el) "text-align"))
                              level (when (= "li" (.tagName el))
                                      (list-level (.parent el)))]
                          [el (cond-> []
                                align (conj align)
                                (and level (<= 1 level 8)) (conj (str "ql-indent-" level)))]))
                  (.select root "p, h1, h2, h3, h4, h5, h6, li, ul, ol"))]
    (doseq [[^Element el quill] planned]
      (if (seq quill)
        (.attr el "class" ^String (str/join " " quill))
        (.removeAttr el "class")))))

(defn- list-element? [^Element el]
  (contains? #{"ul" "ol"} (.tagName el)))

(defn- lists->quill-form!
  "Post: every run of adjacent `ul`/`ol` siblings is one `ol`, and each item
   carries `data-list` \"bullet\" or \"ordered\" from the list it came from.
   Pre : the items' indent classes are already set, because moving an item
         out of its list loses the class that names its level."
  [^Element root]
  (doseq [^Element lst (vec (.select root "ul, ol"))]
    (let [kind (if (= "ol" (.tagName lst)) "ordered" "bullet")]
      (doseq [^Element li (.children lst)]
        (when (= "li" (.tagName li))
          (.attr li "data-list" ^String kind)))))
  (doseq [^Element lst (vec (.select root "ul, ol"))]
    (when (.parent lst)
      (let [prev (.previousElementSibling lst)]
        (if (and prev (list-element? prev))
          (do (doseq [n (vec (.childNodes lst))] (.appendChild ^Element prev n))
              (.remove lst))
          (do (.tagName lst "ol")
              (.removeAttr lst "start")))))))

(defn unwrap-google-link
  "Post: the target of a `https://www.google.com/url?q=…` link, or `href`
   unchanged when it is not one."
  [href]
  (if-let [[_ q] (and href
                   (re-find #"^https?://(?:www\.)?google\.com/url\?(?:.*&)?q=([^&]*)" href))]
    (URLDecoder/decode ^String q "UTF-8")
    href))

(defn- unwrap-links! [^Element root]
  (doseq [^Element a (.select root "a[href]")]
    (.attr a "href" ^String (unwrap-google-link (.attr a "href")))))

(defn- drop-comments!
  "Post: no comment marker and no comment list remain.
   A marker in the text is `<sup><a id=\"cmnt_refN\" href=\"#cmntN\">`.
   An entry of the list at the end is a block holding
   `<a id=\"cmntN\" href=\"#cmnt_refN\">`. The two are told apart by which way
   the link points, because both ids start with `cmnt`."
  [^Element root]
  (doseq [^Element a (vec (.select root "a[href^=#cmnt_ref]"))]
    (when-let [^Element block (or (.closest a "div") (.closest a "p"))]
      (.remove block)))
  (doseq [^Element a (vec (.select root "a[id^=cmnt_ref]"))]
    (.remove ^Element (or (.closest a "sup") a))))

(defn export->html
  "A Drive `text/html` export, as body HTML for the rest of the pipeline.

   Pre : `export` is the export's text, or nil.
   Post: nil iff `export` is nil. Otherwise the body's HTML with the rewrites
         in the namespace docstring applied and no `<style>` left."
  [export]
  (when export
    (let [doc (Jsoup/parse ^String export)
          styles (class-styles (str/join "\n" (map #(.data ^Element %) (.select doc "style"))))
          body (.body doc)]
      (.prettyPrint (.outputSettings doc) false)
      (drop-comments! body)
      (unwrap-links! body)
      (spans->semantic-tags! styles body)
      (blocks->quill-classes! styles body)
      (lists->quill-form! body)
      (doseq [^Element el (vec (.select body "[class]"))]
        (when-not (some #(str/starts-with? % "ql-") (.classNames el))
          (.removeAttr el "class")))
      (.html body))))

(defn content-hash
  "The digest a Pull compares to decide that the Doc did not change.

   Pre : `html` is the pipeline's sanitized output, BEFORE image re-hosting.
   Post: a hex SHA-256 of `html` with every `<img src>` blanked.
   Why : an export may name each image by a signed link that differs per
         request (unknown until plans/google-docs-pull.md row 1.4 runs). With the
         src in the digest, every Pull of an unchanged Doc would make a snapshot.
         The cost is that a change that only swaps one image for another makes
         none."
  [html]
  (let [masked (str/replace (or html "") #"(<img\b[^>]*?\bsrc=\")[^\"]*(\")" "$1$2")
        md (java.security.MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" (bit-and % 0xff))
                 (.digest md (.getBytes ^String masked "UTF-8"))))))
