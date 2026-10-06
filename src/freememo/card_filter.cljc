(ns freememo.card-filter
  "The card table's row filter: text, kind and unsynced, combined with AND.

   Pure and platform-free, so the server runs it over the rows it already holds
   and a test runs it with no database (plans/card-table-filter.md §1).

   Vocabulary:
     filters      {:text string-or-nil :kind kind-string-or-nil :unsynced? bool}.
                  A blank :text and a nil :kind each match every row.
     search text  one lower-cased, tag-free string per display row, covering
                  question, answer and cloze of EVERY member. It is computed
                  once per query result, never per keystroke, and it is kept in
                  a vector parallel to the rows — a row map gains no key, so no
                  row on the wire grows (§1.2)."
  (:require
   [clojure.string :as str]
   [freememo.anki-fields :as fields]
   [freememo.card-components :as cc]
   [freememo.card-rows :as card-rows]))

(def kind-options
  "[value label] for the kind select. \"\" is no kind filter. The five values
   are every card kind (CLAUDE.md, Database)."
  [["" "All kinds"]
   ["basic" "Basic"]
   ["cloze" "Cloze"]
   ["overlapping" "Overlapping"]
   ["occlusion" "Occlusion"]
   ["score" "Score"]])

(defn query
  "The text a row must contain: trimmed and lower-cased, or nil when blank."
  [text]
  (let [q (str/lower-case (str/trim (or text "")))]
    (when-not (str/blank? q) q)))

(defn active?
  "True when any filter narrows the rows."
  [{:keys [text kind unsynced?]}]
  (boolean (or (query text) (some? kind) unsynced?)))

(defn- member-text [m]
  (str (fields/strip-html (or (:flashcards/question m) "")) " "
       (fields/strip-html (or (:flashcards/answer m) "")) " "
       (fields/strip-html (or (:flashcards/cloze m) ""))))

(defn row-search-text
  "Search text for one display row.
   Pre:  `row` is a display row from card-rows/collapse-groups (it has :members).
   Post: lower-cased plain text of every member's question, answer and cloze."
  [row]
  (str/lower-case (str/join " " (map member-text (:members row)))))

(defn row-matches?
  "True when `row`, whose search text is `text`, passes every filter.
   Pre:  `text` is (row-search-text row)."
  [{:keys [kind unsynced?] :as filters} row text]
  (let [q (query (:text filters))]
    (and (or (nil? q) (str/includes? text q))
         (or (nil? kind) (= kind (:flashcards/kind row)))
         (or (not unsynced?) (card-rows/any-member? cc/unsynced? row)))))

(defn filter-rows
  "The rows that pass `filters`, in their original order.
   Pre:  (count texts) = (count rows), and (nth texts i) is the search text of
         (nth rows i).
   Post: a vector, holding every row when no filter is active."
  [filters rows texts]
  (if (active? filters)
    (into [] (keep-indexed (fn [i row] (when (row-matches? filters row (nth texts i)) row)))
      rows)
    (vec rows)))
