(ns freememo.extractor
  "Heuristic HTML segmentation using Jsoup. Splits content into sections
   by headings, or falls back to paragraph grouping when no headings exist.
   Also produces annotated HTML with extract highlights for Quill display."
  (:require [clojure.string :as str])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Element]))

(def ^:private heading-tags #{"h1" "h2" "h3" "h4" "h5" "h6"})

(def ^:private block-tags #{"p" "div" "blockquote" "table" "ul" "ol" "pre" "figure" "dl" "hr"})

(def ^:private leaf-block-tags
  "Content blocks treated as opaque leaves during the flatten walk. `div` is
   intentionally excluded — divs are descended into so headings inside
   wrappers like <div id=\"content\"> get surfaced."
  #{"p" "blockquote" "table" "ul" "ol" "pre" "figure" "dl" "hr"})

(defn- has-descendant-heading? [^Element el]
  (some? (.selectFirst el "h1, h2, h3, h4, h5, h6")))

(defn- flatten-content
  "Depth-first walk producing a document-order seq of headings and leaf-ish
   content blocks. A container whose subtree contains a heading is descended
   into rather than emitted, so structures like <div id=\"content\">…<h2/>
   surface their headings without double-counting the wrapper."
  [^Element el]
  (let [tag (.tagName el)]
    (cond
      (contains? heading-tags tag)
      [el]

      (and (contains? leaf-block-tags tag)
           (not (has-descendant-heading? el)))
      [el]

      :else
      (mapcat flatten-content (.children el)))))

(def ^:private extract-highlight-color
  "Background color for extracted sections. Matches manual extract highlight (#44C2FF)
   so auto-extracted sections look identical to user-selected extracts in Quill."
  "#44C2FF")

(defn- heading? [^Element el]
  (contains? heading-tags (.tagName el)))

(defn- trivial? [^String html]
  (or (str/blank? html)
      (str/blank? (-> (Jsoup/parseBodyFragment html) .body .text))))

(defn- heading-split
  "Split body children into sections at heading boundaries.
   Returns [{:title \"...\" :content \"<h2>...</h2><p>...</p>\"}]."
  [children]
  (loop [remaining (seq children)
         current-title "Introduction"
         current-parts []
         sections []]
    (if-not remaining
      ;; Flush last section
      (let [final (if (seq current-parts)
                    (conj sections {:title current-title
                                    :content (str/join current-parts)})
                    sections)]
        final)
      (let [^Element el (first remaining)]
        (if (heading? el)
          ;; Flush previous section, start new one
          (let [new-title (.text el)
                flushed (if (seq current-parts)
                          (conj sections {:title current-title
                                          :content (str/join current-parts)})
                          sections)]
            (recur (next remaining)
                   new-title
                   [(.outerHtml el)]
                   flushed))
          ;; Append to current section
          (recur (next remaining)
                 current-title
                 (conj current-parts (.outerHtml el))
                 sections))))))

(defn- paragraph-group
  "Fallback: group block elements into chunks of ~4.
   Returns [{:title \"...\" :content \"...\"}]."
  [children]
  (let [blocks (filter (fn [^Element el]
                         (contains? block-tags (.tagName el)))
                       children)
        blocks (if (empty? blocks) children blocks)
        chunks (partition-all 4 blocks)]
    (mapv (fn [chunk]
            (let [first-text (.text ^Element (first chunk))
                  title (if (> (count first-text) 80)
                          (str (subs first-text 0 77) "...")
                          first-text)
                  title (if (str/blank? title) "Extract" title)]
              {:title title
               :content (str/join (map #(.outerHtml ^Element %) chunk))}))
          chunks)))

(defn extract-sections
  "Segment HTML into discrete sections using heading-based splitting.
   Falls back to paragraph grouping when no headings are found.
   Returns [{:title \"Section heading\" :content \"<h2>...</h2><p>...</p>\"}].
   Skips trivial (empty/whitespace-only) sections."
  [html-content]
  (when (and html-content (not (str/blank? html-content)))
    (let [doc (Jsoup/parseBodyFragment html-content)
          body (.body doc)
          flat (vec (mapcat flatten-content (.children body)))
          has-headings? (some heading? flat)
          raw-sections (if has-headings?
                         (heading-split flat)
                         (paragraph-group flat))]
      (vec (remove #(trivial? (:content %)) raw-sections)))))

(defn- highlight-element!
  "Wrap an element's inner HTML in a background-color span. Mutates in-place.
   Matches how Quill renders manual extract highlights."
  [^Element el]
  (let [inner-html (.html el)]
    (when-not (str/blank? inner-html)
      (.html el (str "<span style=\"background-color: " extract-highlight-color ";\">"
                     inner-html "</span>")))))

(defn extract-and-annotate
  "Segment HTML into sections AND produce annotated HTML with highlighted extract bodies.
   Returns {:sections [...] :annotated-html \"...\"}
   - :sections — same format as extract-sections (for content_items)
   - :annotated-html — full HTML with extracted sections highlighted (for page display in Quill)
   Highlights the entire section body (heading + content), matching manual extract appearance."
  [html-content]
  (when (and html-content (not (str/blank? html-content)))
    (let [doc (Jsoup/parseBodyFragment html-content)
          body (.body doc)
          flat (vec (mapcat flatten-content (.children body)))
          has-headings? (some heading? flat)
          ;; Extract sections from current (unmodified) HTML — captures outerHtml snapshots
          raw-sections (if has-headings?
                         (heading-split flat)
                         (paragraph-group flat))
          sections (vec (remove #(trivial? (:content %)) raw-sections))]
      (when (seq sections)
        ;; Now annotate: highlight ALL elements belonging to extracted sections.
        ;; Mutations on `flat` elements propagate to the parsed `body` because
        ;; they're shared DOM references, so (.html body) below picks up the
        ;; highlight spans even when the headings are nested inside wrappers.
        (if has-headings?
          (let [section-titles (set (map :title sections))]
            (loop [remaining flat
                   inside? false]
              (when remaining
                (let [^Element child (first remaining)]
                  (if (heading? child)
                    (let [now-inside? (contains? section-titles (.text child))]
                      (when now-inside?
                        (highlight-element! child))
                      (recur (next remaining) now-inside?))
                    (do
                      (when inside?
                        (highlight-element! child))
                      (recur (next remaining) inside?)))))))
          (let [chunk-first-texts (set (map (fn [s]
                                              (let [t (:title s)]
                                                (if (str/ends-with? t "...")
                                                  (subs t 0 (- (count t) 3))
                                                  t)))
                                            sections))]
            (loop [remaining flat
                   inside? false]
              (when remaining
                (let [^Element child (first remaining)
                      starts-chunk? (some #(str/starts-with? (.text child) %) chunk-first-texts)]
                  (if starts-chunk?
                    (do (highlight-element! child)
                        (recur (next remaining) true))
                    (do (when inside?
                          (highlight-element! child))
                        (recur (next remaining) inside?))))))))
        {:sections sections
         :annotated-html (.html body)}))))
