(ns freememo.assistant-render-tree
  "Safe render-tree conversion and Electric rendering for assistant Markdown.

   The server keeps the existing Flexmark and HTML-cleaner boundary. It then
   converts that safe HTML into data. The client renders the data with Electric.

   Electric owns every Markdown element and text node. KaTeX owns only the
   descendants of a dedicated math leaf."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.math :as math]
   #?(:clj [freememo.markdown :as markdown]))
  #?(:clj (:import [org.jsoup Jsoup]
                   [org.jsoup.nodes Element TextNode])))

(def ^:private allowed-element-tags
  "Elements that the assistant renderer accepts after HTML cleaning."
  #{"a" "b" "blockquote" "br" "caption" "cite" "code" "col" "colgroup"
    "dd" "div" "dl" "dt" "em" "figcaption" "figure" "h1" "h2" "h3"
    "h4" "h5" "h6" "hr" "i" "li" "ol" "p" "pre" "q" "s" "small"
    "span" "strike" "strong" "sub" "sup" "table" "tbody" "td" "tfoot"
    "th" "thead" "tr" "u" "ul"})

(def ^:private allowed-attribute-names
  "Attributes that can survive the HTML cleaner and enter the render tree."
  #{"abbr" "axis" "cite" "class" "data-language" "data-list" "data-row"
    "dir" "href" "id" "lang" "scope" "span" "start" "style" "summary"
    "title" "type" "width"})

(def ^:private allowed-style-properties
  "Style properties that the HTML cleaner permits on safe raw HTML."
  #{"background-color" "color"})

(def ^:private allowed-attribute-keys
  (into #{} (map keyword) allowed-attribute-names))

(def ^:private allowed-style-keys
  (into #{} (map keyword) allowed-style-properties))

(def ^:private math-region-pattern
  #"\\\[([\s\S]*?)\\\]|\\\(([\s\S]*?)\\\)")

(defn- style-attribute->map [style]
  (into {}
    (keep
      (fn [declaration]
        (let [[property value] (str/split declaration #":" 2)
              property (some-> property str/trim)
              value (some-> value str/trim)]
          (when (and (allowed-style-properties property) (seq value))
            [(keyword property) value]))))
    (str/split (or style "") #";")))

(defn- allowed-render-attributes [attributes]
  (let [attributes (select-keys (or attributes {}) allowed-attribute-keys)]
    (if-let [style (:style attributes)]
      (let [style (select-keys style allowed-style-keys)]
        (cond-> (dissoc attributes :style)
          (seq style) (assoc :style style)))
      attributes)))

#?(:clj
   (defn- element-attributes [^Element element]
     (reduce
       (fn [attributes attribute]
         (let [attribute-name (.getKey attribute)
               attribute-value (.getValue attribute)]
           (if-not (allowed-attribute-names attribute-name)
             attributes
             (case attribute-name
               "class" (assoc attributes :class attribute-value)
               "style" (let [style-map (style-attribute->map attribute-value)]
                         (cond-> attributes (seq style-map) (assoc :style style-map)))
               (assoc attributes (keyword attribute-name) attribute-value)))))
       {}
       (.asList (.attributes element)))))

(defn- text-render-node [structural-key text]
  {:assistant-render-node/key structural-key
   :assistant-render-node/kind :text
   :assistant-render-node/text text})

(defn- math-render-node [structural-key tex display?]
  {:assistant-render-node/key structural-key
   :assistant-render-node/kind :math
   :assistant-render-node/tex tex
   :assistant-render-node/display? display?})

#?(:clj
   (defn- text->render-nodes [text structural-key math-enabled?]
     (if-not math-enabled?
       [(text-render-node (conj structural-key 0) text)]
       (let [matcher (re-matcher math-region-pattern text)]
         (loop [cursor 0
                segment-index 0
                render-nodes []]
           (if (.find matcher)
             (let [match-start (.start matcher)
                   match-end (.end matcher)
                   prefix (subs text cursor match-start)
                   display-tex (.group matcher 1)
                   inline-tex (.group matcher 2)
                   render-nodes
                   (cond-> render-nodes
                     (seq prefix)
                     (conj (text-render-node
                             (conj structural-key segment-index) prefix)))
                   segment-index (if (seq prefix) (inc segment-index) segment-index)]
               (recur match-end
                 (inc segment-index)
                 (conj render-nodes
                   (math-render-node
                     (conj structural-key segment-index)
                     (or display-tex inline-tex "")
                     (some? display-tex)))))
             (let [suffix (subs text cursor)]
               (cond-> render-nodes
                 (or (seq suffix) (empty? render-nodes))
                 (conj (text-render-node
                         (conj structural-key segment-index) suffix))))))))))

#?(:clj (declare html-node->render-nodes))

#?(:clj
   (defn- html-children->render-nodes [html-children parent-key math-enabled?]
     (into []
       (mapcat
         (fn [[child-index html-child]]
           (html-node->render-nodes
             html-child (conj parent-key child-index) math-enabled?)))
       (map-indexed vector html-children))))

#?(:clj
   (defn- html-node->render-nodes [html-node structural-key math-enabled?]
     (cond
       (instance? TextNode html-node)
       (text->render-nodes (.getWholeText ^TextNode html-node)
         structural-key math-enabled?)

       (instance? Element html-node)
       (let [^Element element html-node
             tag (.tagName element)
             child-math-enabled? (and math-enabled? (not (#{"code" "pre"} tag)))
             children (html-children->render-nodes
                        (.childNodes element) structural-key child-math-enabled?)]
         (if (allowed-element-tags tag)
           [{:assistant-render-node/key structural-key
             :assistant-render-node/kind :element
             :assistant-render-node/tag tag
             :assistant-render-node/attributes (element-attributes element)
             :assistant-render-node/children children}]
           children))

       :else [])))

#?(:clj
   (defn- safe-html->render-tree [safe-html]
     (let [document (Jsoup/parseBodyFragment (or safe-html ""))]
       (.prettyPrint (.outputSettings document) false)
       (html-children->render-nodes (.childNodes (.body document)) [] true))))

(defn markdown->render-tree
  "Convert assistant Markdown into a safe render tree.

   Pre: `markdown-source` is a string or nil. The caller inserts evidence links.
   Post: the result is a vector of safe element, text, and math nodes.
   Invariant: the client never parses or inserts HTML from this result."
  [markdown-source]
  #?(:clj
     (if (str/blank? markdown-source)
       []
       (safe-html->render-tree
         (markdown/markdown-with-math->html markdown-source)))
     :cljs []))

(e/defn MathLeaf [render-node]
  (e/client
    (let [tex (:assistant-render-node/tex render-node)
          display? (:assistant-render-node/display? render-node)]
      (if display?
        (dom/div
          (dom/props {:class "assistant-markdown-math assistant-markdown-math--display"})
          (math/render-tex-leaf! dom/node tex true)
          (e/on-unmount #(math/invalidate-tex-leaf! dom/node)))
        (dom/span
          (dom/props {:class "assistant-markdown-math assistant-markdown-math--inline"})
          (math/render-tex-leaf! dom/node tex false)
          (e/on-unmount #(math/invalidate-tex-leaf! dom/node)))))))

(e/defn RenderNode [render-node]
  (e/client
    (let [kind (:assistant-render-node/kind render-node)]
      (cond
        (= :text kind)
        (dom/text (:assistant-render-node/text render-node))

        (= :math kind)
        (MathLeaf render-node)

        (= :element kind)
        (let [tag (:assistant-render-node/tag render-node)
              children (:assistant-render-node/children render-node)]
          (if (allowed-element-tags tag)
            (dom/element tag
              (dom/props
                (allowed-render-attributes
                  (:assistant-render-node/attributes render-node)))
              (e/for [child (e/diff-by :assistant-render-node/key children)]
                (RenderNode child)))
            (e/for [child (e/diff-by :assistant-render-node/key children)]
              (RenderNode child))))

        :else
        (dom/text "")))))

(e/defn RenderTree [render-tree]
  (e/client
    (e/for [render-node (e/diff-by :assistant-render-node/key render-tree)]
      (RenderNode render-node))))
