(ns freememo.search
  "Global content search. Queries topics.content_text (plain text derived from
   content HTML) using pg_trgm. Fuzzy mode = ILIKE substring; exact mode =
   whole-word regex. Both modes are case-insensitive and share a GIN trigram
   index. Snippets are built server-side around the first match."
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [clojure.string :as str]
            [freememo.db :as db]
            [taoensso.telemere :as tel]))

(def ^:private result-limit 10000)
(def ^:private min-query-len 2)
(def ^:private snippet-window 25)

(defn- regex-escape
  "Escape POSIX regex metacharacters so the query is matched literally."
  [s]
  (str/replace s #"[\\^$.|?*+()\[\]{}]" "\\\\$0"))

(defn- escape-html [s]
  (-> (str s)
    (str/replace "&" "&amp;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")))

(defn- build-snippet
  "Return an HTML snippet: up to snippet-window chars before and after the first
   case-insensitive occurrence of q in plain-text, with the match wrapped in
   <mark>. Escapes HTML special chars. No ellipsis markers — the UI scroll-
   centers the mark, so edges are visually handled by overflow clipping.
   Returns nil when there is no match."
  [plain-text q]
  (when (and plain-text q (pos? (count plain-text)) (pos? (count q)))
    (let [lc-text (str/lower-case plain-text)
          lc-q (str/lower-case q)
          pos (str/index-of lc-text lc-q)]
      (when pos
        (let [q-len (count q)
              start (max 0 (- pos snippet-window))
              end (min (count plain-text) (+ pos q-len snippet-window))
              before (subs plain-text start pos)
              matched (subs plain-text pos (+ pos q-len))
              after (subs plain-text (+ pos q-len) end)]
          (str (escape-html before)
            "<mark>" (escape-html matched) "</mark>"
            (escape-html after)))))))

(defn- kind-clause
  "Return a SQL fragment for the kind filter. Friendly labels:
   \"all\" → no filter; \"topics\" → basic (incl. extracts & chapters);
   \"pages\" → page; \"articles\" → web; \"epub\" → basic under epub root;
   \"markdown\" → markdown."
  [kind-filter]
  (case kind-filter
    "topics" " AND t.kind = 'basic'"
    "pages" " AND t.kind = 'page'"
    "articles" " AND t.kind = 'web'"
    "epub" " AND t.kind = 'basic' AND EXISTS (SELECT 1 FROM topics p WHERE p.id = t.parent_id AND p.kind = 'epub')"
    "markdown" " AND t.kind = 'markdown'"
    ;; "all" or unknown
    ""))

(def ^:private select-cols
  "Shared SELECT list. Walks up to 2 levels to find a root ancestor for
   the \"source\" (e.g., PDF filename for an extract-of-page-of-pdf)."
  "t.id, t.title, t.kind, t.parent_id, t.page_number,
   t.content_text, t.created_at,
   COALESCE(p2.title, p1.title, t.title) AS source_title")

(def ^:private source-joins
  "LEFT JOIN parent and grandparent topics (handles extract → page → pdf)."
  "LEFT JOIN topics p1 ON p1.id = t.parent_id
   LEFT JOIN topics p2 ON p2.id = p1.parent_id")

(defn search-content
  "Search topics.content_text for user-id. Returns at most result-limit rows.
   - query: string. Trimmed. If shorter than min-query-len returns [].
   - mode: :fuzzy (ILIKE substring, ordered by word_similarity DESC) or
           :exact (whole-word case-insensitive regex, ordered by created_at DESC).
   - kind-filter: friendly label string (see kind-clause).
   Returns vec of {:id :title :kind :parent-id :page-number :snippet
                    :source-title :created-at}."
  [user-id query mode kind-filter]
  (try
    (let [q (when query (str/trim query))]
      (if (or (nil? q) (< (count q) min-query-len))
        []
        (let [kind-sql (kind-clause kind-filter)
              [sql params]
              (case mode
                :exact
                [(str "SELECT " select-cols "
                       FROM topics t " source-joins "
                       WHERE t.user_id = ?
                         AND t.content_text IS NOT NULL
                         AND t.content_text ~* ?"
                   kind-sql
                   " ORDER BY t.created_at DESC LIMIT ?")
                 [user-id (str "\\m" (regex-escape q) "\\M") result-limit]]

                ;; default :fuzzy
                [(str "SELECT " select-cols ",
                              word_similarity(?, t.content_text) AS rank
                       FROM topics t " source-joins "
                       WHERE t.user_id = ?
                         AND t.content_text IS NOT NULL
                         AND t.content_text ILIKE '%' || ? || '%'"
                   kind-sql
                   " ORDER BY rank DESC, t.created_at DESC LIMIT ?")
                 [q user-id q result-limit]])
              rows (jdbc/execute! db/ds
                     (into [sql] params)
                     {:builder-fn rs/as-unqualified-maps})]
          (mapv (fn [row]
                  {:id (:id row)
                   :title (:title row)
                   :kind (:kind row)
                   :parent-id (:parent_id row)
                   :page-number (:page_number row)
                   :source-title (:source_title row)
                   :snippet (build-snippet (:content_text row) q)
                   :created-at (:created_at row)})
            rows))))
    (catch Exception e
      (tel/error! {:id ::search-content} e)
      [])))
