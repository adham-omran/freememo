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
;; Fuzzy-mode rerank cap. word_similarity ranking over full content_text is
;; O(matched-bytes) — 92% of query time on a non-selective match (a term
;; present in most docs). We retrieve the N shortest matching docs cheaply,
;; then rank only those. Distinct from result-limit: fuzzy output is capped
;; here, not at result-limit.
(def ^:private candidate-limit 200)
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
   \"markdown\" → markdown; \"supermemo\" → an imported SuperMemo collection;
   \"code\" → a file imported from a git forge."
  [kind-filter]
  (case kind-filter
    "topics" " AND t.kind = 'basic'"
    "pages" " AND t.kind = 'page'"
    "articles" " AND t.kind = 'web'"
    "epub" " AND t.kind = 'basic' AND EXISTS (SELECT 1 FROM topics p WHERE p.id = t.parent_id AND p.kind = 'epub')"
    "markdown" " AND t.kind = 'markdown'"
    "supermemo" " AND t.kind = 'supermemo'"
    ;; A poem's verses reach `content_text` through the rendered fallback
    ;; (plans/poem-package-format.md §7), so a full-text search already returns
    ;; poem rows. Without this clause a reader can see them under "All kinds"
    ;; and cannot narrow to them.
    "poem" " AND t.kind = 'poem'"
    "code" " AND t.kind = 'code'"
    ;; "all" or unknown
    ""))

(def ^:private base-cols
  "The columns both match branches return. Walks up to 2 levels to find a root
   ancestor for the \"source\" (e.g., PDF filename for an
   extract-of-page-of-pdf)."
  "t.id, t.title, t.kind, t.parent_id, t.page_number, t.created_at,
   COALESCE(p2.title, p1.title, t.title) AS source_title")

;; Two branches match, and a UNION ALL joins them, because they read different
;; tables and each has its own trigram index (plans/git-forge-integration.md
;; §4.5). An OR across the join could use neither index.
;;
;;   notes   topics.content_text — every topic kind, a code topic's notes included
;;   source  topic_code.source   — the text of a file imported from a git forge
;;
;; The source branch returns the whole source to the outer query, never to the
;; client: the outer query cuts `match_text` to a window around the first match,
;; so a one-megabyte file costs a snippet on the wire. A topic that matches in
;; both branches appears twice in SQL, and `search-content` keeps the first.

(def ^:private notes-cols (str base-cols ", t.content_text, NULL::text AS source"))
(def ^:private source-cols (str base-cols ", NULL::text AS content_text, tc.source"))

(def ^:private snippet-margin
  "Characters kept before the first match in a source window. More than
   `snippet-window`, so a lower-casing that shifts the position by a few
   characters still leaves the match inside the window."
  60)

(defn- distinct-by-id
  "A transducer that keeps the first row for each `:id`. The SQL order is the
   ranking, so the first row is the better-ranked match."
  []
  (fn [rf]
    (let [seen (volatile! #{})]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc row]
         (if (contains? @seen (:id row))
           acc
           (do (vswap! seen conj (:id row))
               (rf acc row))))))))

(def ^:private source-joins
  "LEFT JOIN parent and grandparent topics (handles extract → page → pdf)."
  "LEFT JOIN topics p1 ON p1.id = t.parent_id
   LEFT JOIN topics p2 ON p2.id = p1.parent_id")

(defn search-content
  "Search topics.content_text and topic_code.source for user-id.
   - query: string. Trimmed. If shorter than min-query-len returns [].
   - mode: :fuzzy or :exact (whole-word case-insensitive regex, newest first,
           up to result-limit rows).
   - kind-filter: friendly label string (see kind-clause).
   Returns vec of {:id :title :kind :parent-id :page-number :snippet
                    :source-title :created-at}.

   Fuzzy mode is a two-phase retrieve-then-rerank: it ILIKE-matches, takes the
   candidate-limit (200) SHORTEST matching documents, then ranks only those by
   word_similarity. So fuzzy results are the similarity-ranked top of the 200
   shortest matches — not the global top-by-similarity. Shortest-first is the
   candidate proxy because a substring match fills a larger fraction of a short
   document, which tends to score higher on word_similarity. This trades exact
   global ranking for bounded latency on non-selective queries; selective
   queries (< 200 matches) are unaffected — the cap never binds."
  [user-id query mode kind-filter]
  (try
    (let [q (when query (str/trim query))]
      (if (or (nil? q) (< (count q) min-query-len))
        []
        (let [kind-sql (kind-clause kind-filter)
              pattern (str "\\m" (regex-escape q) "\\M")
              ;; `match_text` is the notes whole, or a window of the source that
              ;; starts `snippet-margin` characters before the first match.
              window (fn [position-sql]
                       (str "COALESCE(cand.content_text,
                               substring(cand.source FROM greatest(1, " position-sql " - "
                         snippet-margin ") FOR " (* 2 snippet-margin) " + length(?)))"))
              [sql params]
              (case mode
                :exact
                [(str "SELECT cand.id, cand.title, cand.kind, cand.parent_id, cand.page_number,
                              cand.created_at, cand.source_title,
                              " (window "regexp_instr(cand.source, ?, 1, 1, 0, 'i')") " AS match_text
                       FROM ((SELECT " notes-cols "
                              FROM topics t " source-joins "
                              WHERE t.user_id = ?
                                AND t.staged_delete_id IS NULL
                                AND t.content_text IS NOT NULL
                                AND t.content_text ~* ?" kind-sql "
                              ORDER BY t.created_at DESC LIMIT ?)
                             UNION ALL
                             (SELECT " source-cols "
                              FROM topic_code tc JOIN topics t ON t.id = tc.topic_id " source-joins "
                              WHERE t.user_id = ?
                                AND t.staged_delete_id IS NULL
                                AND tc.source ~* ?" kind-sql "
                              ORDER BY t.created_at DESC LIMIT ?)) cand
                       ORDER BY cand.created_at DESC LIMIT ?")
                 [pattern q user-id pattern result-limit user-id pattern result-limit result-limit]]

                ;; default :fuzzy — retrieve-then-rerank. Each branch takes the
                ;; candidate-limit shortest ILIKE matches WITHOUT computing
                ;; word_similarity; the outer query ranks only those candidates.
                ;; A source candidate is ranked on its window, never on the whole
                ;; file: word_similarity is O(bytes), which is the cost the
                ;; candidate cap exists to bound.
                [(str "SELECT m.*, word_similarity(?, m.match_text) AS rank
                       FROM (SELECT cand.id, cand.title, cand.kind, cand.parent_id, cand.page_number,
                                    cand.created_at, cand.source_title,
                                    " (window "strpos(lower(cand.source), lower(?))") " AS match_text
                             FROM ((SELECT " notes-cols "
                                    FROM topics t " source-joins "
                                    WHERE t.user_id = ?
                                      AND t.staged_delete_id IS NULL
                                      AND t.content_text IS NOT NULL
                                      AND t.content_text ILIKE '%' || ? || '%'" kind-sql "
                                    ORDER BY length(t.content_text) ASC LIMIT ?)
                                   UNION ALL
                                   (SELECT " source-cols "
                                    FROM topic_code tc JOIN topics t ON t.id = tc.topic_id " source-joins "
                                    WHERE t.user_id = ?
                                      AND t.staged_delete_id IS NULL
                                      AND tc.source ILIKE '%' || ? || '%'" kind-sql "
                                    ORDER BY length(tc.source) ASC LIMIT ?)) cand) m
                       ORDER BY rank DESC, m.created_at DESC")
                 [q q q user-id q candidate-limit user-id q candidate-limit]])
              rows (jdbc/execute! db/ds
                     (into [sql] params)
                     {:builder-fn rs/as-unqualified-maps})]
          (into []
            (comp (distinct-by-id)
              (map (fn [row]
                     {:id (:id row)
                      :title (:title row)
                      :kind (:kind row)
                      :parent-id (:parent_id row)
                      :page-number (:page_number row)
                      :source-title (:source_title row)
                      :snippet (build-snippet (:match_text row) q)
                      :created-at (:created_at row)})))
            rows))))
    (catch Exception e
      (tel/error! {:id ::search-content} e)
      [])))
