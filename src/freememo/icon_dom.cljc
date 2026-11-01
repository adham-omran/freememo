(ns freememo.icon-dom
  (:require [freememo.icon-paths :as icon-paths]))

(defn root-attributes [size class title]
  {:width (str size) :height (str size) :viewBox "0 0 24 24"
   :fill "none" :stroke "currentColor" :stroke-width "2"
   :stroke-linecap "round" :stroke-linejoin "round"
   :aria-hidden (if title "false" "true") :class (when class class)})

(defn install!
  "Update one SVG root and its trusted icon children. The Electric root owns
   their lifetime. Tooltip attributes remain with the tooltip component."
  [node glyph size class title]
  #?(:cljs
     (let [document (.-ownerDocument node)
           children (.createDocumentFragment document)]
       (doseq [[key value] (root-attributes size class title)]
         (if (some? value)
           (.setAttribute node (name key) (str value))
           (.removeAttribute node (name key))))
       (doseq [[tag attrs] (get icon-paths/paths glyph)]
         (let [child (.createElementNS document "http://www.w3.org/2000/svg" (name tag))]
           (doseq [[key value] attrs]
             (.setAttribute child (name key) (str value)))
           (.appendChild children child)))
       (.replaceChildren node children)
       nil)
     :clj nil))
