(ns freememo.walkthrough-text
  "The model writes a step's label and summary as plain text with Markdown
   inline code: `SpliceDef.spawnDef`. These functions read that one construct
   and nothing else of Markdown (plans/commit-range-walkthrough.md §6.6.2).

   Pure, and on both peers."
  (:require [clojure.string :as str]))

(defn code-spans
  "Split `s` at backtick pairs.
   Post: a vector of [:text t] and [:code c] whose texts, with each code
   span's backticks put back, concatenate to `s`. A backtick with no partner
   on its line stays text."
  [s]
  (into []
    (map (fn [[whole code]] (if code [:code code] [:text whole])))
    (re-seq #"`([^`\n]+)`|[^`]+|`" (str s))))

(defn plain
  "`s` with the backticks of its code spans removed, for a place that shows
   text only, such as a step chip."
  [s]
  (apply str (map second (code-spans s))))
