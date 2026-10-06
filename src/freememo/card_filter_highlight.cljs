(ns freememo.card-filter-highlight
  "Marks card-table filter matches with one CSS Custom Highlight, `card-filter`.

   A CSS highlight changes no node. That matters twice here: each cell's content
   is innerHTML that math/set-html! owns, and the cell sits in a virtual-scroll
   row Electric recycles (plans/card-table-filter.md §6). The same mechanism as
   freememo.answer-selection-menu and freememo.question-pin-dom.

   One Highlight is shared by every cell. Each cell's ranges are kept in a
   WeakMap, so a cell repaints by removing exactly its own ranges.")

(def ^:private highlight-name "card-filter")

(defonce ^:private !highlight (atom nil))

(defonce ^:private painted
  ;; cell node → JS array of the Ranges last added for it
  (js/WeakMap.))

(defn- registry []
  (when (exists? js/Highlight) (some-> js/CSS .-highlights)))

(defn- shared-highlight
  "The one `card-filter` Highlight, registered on first use."
  [^js reg]
  (or @!highlight
      (let [h (js/Highlight.)]
        (.set reg highlight-name h)
        (reset! !highlight h))))

(defn- text-segments
  "The text nodes under `node`, in document order, each with its folded text.
   A node whose lower-casing changes its length keeps its original text, so
   every offset into the folded text is an offset into the node."
  [^js node]
  (let [walker (.createTreeWalker js/document node js/NodeFilter.SHOW_TEXT)]
    (loop [out []]
      (if-let [^js t (.nextNode walker)]
        (let [s (.-data t)
              folded (.toLowerCase s)]
          (recur (conj out [t (if (= (.-length folded) (.-length s)) folded s)])))
        out))))

(defn- position
  "[text-node offset] for index `i` of the joined text. `starts` holds each
   segment's start index. An index at a segment boundary resolves to the END of
   the earlier segment when `end?`, so a range never begins or ends in a
   neighbouring node it does not cover."
  [segments starts i end?]
  (loop [k (dec (count segments))]
    (let [start (nth starts k)]
      (if (or (zero? k) (if end? (< start i) (<= start i)))
        [(first (nth segments k)) (- i start)]
        (recur (dec k))))))

(defn- match-ranges
  "A Range for every occurrence of `q` in the text under `node`. A match may
   span adjacent text nodes, e.g. a word split by a <code> element."
  [^js node q]
  (let [segments (text-segments node)
        starts (vec (reductions + 0 (map (comp count second) segments)))
        joined (apply str (map second segments))
        n (count q)]
    (loop [from 0 out []]
      (let [i (.indexOf joined q from)]
        (if (neg? i)
          out
          (let [[sn so] (position segments starts i false)
                [en eo] (position segments starts (+ i n) true)
                r (.createRange js/document)]
            (.setStart r sn so)
            (.setEnd r en eo)
            (recur (+ i n) (conj out r))))))))

(defn paint!
  "Replace `node`'s ranges in the `card-filter` highlight with its matches of `q`.
   Pre:  `q` is nil or a lower-cased query (card-filter/query).
   Post: the highlight holds one range per occurrence of `q` in `node`'s text,
         and no range of an earlier paint of `node`. A nil `q` leaves none.
         Returns nil. A browser without the Highlight API paints nothing."
  [^js node q]
  (when-let [reg (registry)]
    (let [^js h (shared-highlight reg)]
      (doseq [r (or (.get painted node) [])] (.delete h r))
      (if (and node q)
        (let [ranges (match-ranges node q)]
          (doseq [r ranges] (.add h r))
          (.set painted node (into-array ranges)))
        (some->> node (.delete painted)))))
  nil)
