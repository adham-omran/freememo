(ns freememo.assistant-evidence-excerpt
  "Build bounded, retained evidence blocks from stored document HTML."
  (:require
   [clojure.string :as str]
   [freememo.html-cleaner :as cleaner]
   [freememo.text :as text])
  (:import
   [org.jsoup Jsoup]
   [org.jsoup.nodes Element Node TextNode]))

(def max-visible-chars
  "Maximum visible characters in one retained evidence block."
  1000)

(def ^:private evidence-block-tags
  #{"address" "blockquote" "dl" "figure" "h1" "h2" "h3" "h4" "h5" "h6"
    "hr" "ol" "p" "pre" "table" "ul"})

(def ^:private heading-tags
  #{"h1" "h2" "h3" "h4" "h5" "h6"})

(defn- compact-output! [doc]
  (.prettyPrint (.outputSettings doc) false)
  doc)

(defn- code-block-container? [^Element element]
  (.hasClass element "ql-code-block-container"))

(defn- evidence-block? [^Element element]
  (or (contains? evidence-block-tags (.normalName element))
    (code-block-container? element)))

(defn- semantic-nodes [^Node node]
  (cond
    (instance? TextNode node)
    (when-not (str/blank? (.getWholeText ^TextNode node)) [node])

    (instance? Element node)
    (let [element ^Element node]
      (if (evidence-block? element)
        [element]
        (mapcat semantic-nodes (.childNodes element))))

    :else
    (mapcat semantic-nodes (.childNodes node))))

(defn- text-parts [value]
  (loop [remaining (str/trim (or value ""))
         parts []]
    (if (<= (count remaining) max-visible-chars)
      (cond-> parts (not (str/blank? remaining)) (conj remaining))
      (let [window (subs remaining 0 max-visible-chars)
            last-space (max (.lastIndexOf window " ") (.lastIndexOf window "\n"))
            cut (if (>= last-space (quot max-visible-chars 2)) last-space max-visible-chars)
            part (str/trim (subs remaining 0 cut))
            tail (str/trim (subs remaining cut))]
        (recur tail (cond-> parts (not (str/blank? part)) (conj part)))))))

(defn- long-element-html [^Element element part]
  (let [tag (.normalName element)
        wrapper (if (or (= "pre" tag) (code-block-container? element)) "pre" "p")]
    (cleaner/clean-html-fragment
      (str "<" wrapper ">" (text/escape-html part) "</" wrapper ">"))))

(defn- node-blocks [^Node node]
  (cond
    (instance? TextNode node)
    (mapv (fn [part] {:text part :html nil})
      (text-parts (.getWholeText ^TextNode node)))

    (instance? Element node)
    (let [element ^Element node
          visible (str/trim (.text element))
          safe-html (cleaner/clean-html-fragment (.outerHtml element))]
      (cond
        (str/blank? visible) []
        (<= (count visible) max-visible-chars)
        [{:text visible :html safe-html}]
        :else
        (mapv (fn [part]
                {:text part :html (long-element-html element part)})
          (text-parts visible))))

    :else []))

(defn- short-label [value]
  (let [value (some-> value (str/replace #"\s+" " ") str/trim not-empty)]
    (when value
      (if (> (count value) 64)
        (str (subs value 0 63) "…")
        value))))

(defn evidence-blocks
  "Split stored HTML into retained source blocks.

   Each block has plain text, optional safe HTML, a section label, and an anchor.
   The HTML ends at an element boundary and has at most max-visible-chars of text."
  [html]
  (let [doc (compact-output! (Jsoup/parseBodyFragment
                               (or (cleaner/clean-html html) "")))
        nodes (vec (semantic-nodes (.body doc)))]
    (loop [[node & remaining] nodes
           section nil
           anchor nil
           block-index 1
           result []]
      (if-not node
        result
        (let [heading? (and (instance? Element node)
                         (contains? heading-tags (.normalName ^Element node)))
              next-section (if heading?
                             (or (short-label (.text ^Element node)) section)
                             section)
              next-anchor (if (and heading? (.hasAttr ^Element node "id"))
                            (.attr ^Element node "id")
                            anchor)
              blocks (node-blocks node)
              indexed (map-indexed
                        (fn [part-index block]
                          (assoc block
                            :anchor next-anchor
                            :section-title next-section
                            :block-index (+ block-index part-index)))
                        blocks)]
          (recur remaining next-section next-anchor (+ block-index (count blocks))
            (into result indexed)))))))
