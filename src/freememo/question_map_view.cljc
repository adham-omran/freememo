(ns freememo.question-map-view
  "The rules of the question map tab (plans/question-map-tab.md §4.3): a
   root's state, the order of the trail, the done count, and the text of a
   citation.

   No requires, so both peers and the Clojure tests read the same rules.
   A root here is a row of `question-map/map-card`.")

(defn root-state
  "The state of `root` (§4.3.3), the first match of:
   status `learned`, status `next`, a known mark of either source, else `open`."
  [{:keys [status known_sources]}]
  (cond (= "learned" status) "learned"
        (= "next" status) "next"
        (seq known_sources) "known"
        :else "open"))

(defn done?
  "True when `root` counts toward the tab label: state `learned` or `known`."
  [root]
  (contains? #{"learned" "known"} (root-state root)))

(defn done-count
  "The number of done roots in `roots`."
  [roots]
  (count (filter done? roots)))

(defn done-time
  "When `root` became done: `:learned_at` for a learned root, `:known_at`
   (its earliest mark) for a known one, nil otherwise or when unrecorded."
  [root]
  (case (root-state root)
    "learned" (:learned_at root)
    "known" (:known_at root)
    nil))

(defn- trail-key
  "Done roots first, by done time with an unrecorded time first, then the
   `next` root, then `open` roots. The id breaks ties, and is the order the
   tutor added roots in."
  [root]
  (let [state (root-state root)]
    [(case state "learned" 0 "known" 0 "next" 1 2)
     (done-time root)
     (:id root)]))

(defn trail
  "`roots` in the order of the trail (§4.3.2)."
  [roots]
  (vec (sort-by trail-key roots)))

(def quote-label-chars 60)

(defn quote-label
  "The visible text of a citation: its quote with runs of whitespace
   collapsed, cut to `quote-label-chars` with an ellipsis."
  [quote]
  (let [s (apply str (interpose " " (re-seq #"\S+" (str quote))))]
    (if (> (count s) quote-label-chars)
      (str (subs s 0 quote-label-chars) "…")
      s)))

(defn distinct-citations
  "`citations` with each quote once, in their first order (§4.3.5)."
  [citations]
  (let [seen (volatile! #{})]
    (into []
      (filter (fn [{:keys [quote]}]
                (when-not (contains? @seen quote)
                  (vswap! seen conj quote)
                  true)))
      citations)))

(defn tab-label
  "The Map tab's label, for example `Map 2 of 4`."
  [roots]
  (str "Map " (done-count roots) " of " (count roots)))
