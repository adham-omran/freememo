(ns freememo.kg-dedup
  "Near-duplicate fact detection — the h3 detector and the sweep that applies it.

   THE PROBLEM. `insert-kg-facts!` dedups through two partial unique indexes keyed
   on (subject_entity_id, predicate_id, object, graph_topic_id). Re-distilling a
   document defeats them, because the model rewords all three parts between runs.
   Measured on production: four documents have been distilled more than once, and
   of 49 hand-labelled duplicate pairs across them the indexes caught 4 —
   recall 0.08. On a re-extracted page, 25% of the later run's facts restate a
   fact the earlier run already produced (38% at a looser similarity band).

   THE DETECTOR. `duplicate-pair?` is h3, the best of 21 candidates scored against
   those labels: precision 0.90, recall 0.78, F1 0.84. Three branches, each
   catching a failure the others miss —

     1. flat trigram      whole-triple rewording
                          `hex digit|represents|4 binary bits`
                       ⟷ `hex digit|represents|exactly 4 binary bits`
     2. subject-anchored  same subject, predicate drifted, object stable
                          `cobit 2019|is-based-on|more than 25 years of development`
                       ⟷ `cobit 2019|builds-on|more than 25 years of development`
     3. containment       one triple's words contain the other's, trigram low
                          `site map|maps|route symbol`            (trigram 0.39)
                       ⟷ `site map|maps|a backticked route symbol to an hfql expression`

   Branch 2 also closes a gap the indexes cannot: they are disjoint by
   construction (one WHERE object_entity_id IS NOT NULL, the other WHERE
   object_literal IS NOT NULL), so the SAME text stored once as an entity and once
   as a literal never collides. Three of the four exact duplicates on production
   came from exactly that. `obj_eq` compares the rendered value, not the column.

   WHAT IT CANNOT DO. Paraphrase with disjoint vocabulary — `hfql |
   lacks-knowledge-of | business domain` vs `hfql | is-not | omniscient` scores
   trigram 0.11, containment 0.25. Ten of the 49 labelled duplicates are that
   shape and no lexical method reaches them; that residual is what the (installed,
   unused) pgvector and fuzzystrmatch groundwork is for.

   WHY HYBRID. Trigram comes from pg_trgm because the thresholds were fitted
   against `pg_trgm.similarity` and this project carries no Clojure trigram
   implementation to reproduce its word-padding and Jaccard exactly. Containment
   and the predicate itself are plain set arithmetic and live here, where they are
   readable and testable without a database.

   See plans/kg-duplicate-fact-sweep.md."
  (:require
   [freememo.db :as db]
   [freememo.settings :as settings]
   [freememo.commands :as commands]
   [freememo.toasts :as toasts]
   [clojure.set :as set]
   [clojure.string :as str]
   [taoensso.telemere :as tel]))

(def max-facts-per-page
  "Page keys holding more than this many approved facts are skipped, and the skip
   is logged rather than silently absorbed. The sweep is O(n²) in a page's fact
   count; production's largest page key holds 79 (document 8915, the page_number
   IS NULL group) = 3081 pairs, and no document holds more than 213 facts in
   total, so this bound is ~5× headroom over anything observed."
  400)

;; ---------------------------------------------------------------------------
;; Pure — the detector
;; ---------------------------------------------------------------------------

(defn- words
  "Distinct whitespace-separated tokens of a rendered triple.
   Set semantics on both sides, matching the SQL the thresholds were fitted with
   (`string_to_array` + INTERSECT over `min(count(DISTINCT …))`)."
  [s]
  (into #{} (remove str/blank?) (str/split (str s) #"\s+")))

(defn containment
  "Overlap of two triples' word sets against the SMALLER set — asymmetric, and
   that asymmetry is the point: it scores 1.0 when one triple's words wholly
   contain the other's, which is the shape a re-extraction produces when it adds a
   qualifier (`maps | route symbol` → `maps | a backticked route symbol to an hfql
   expression`). Jaccard scores that pair 0.45 and trigram 0.39.
   Post: a double in [0.0, 1.0]; 0.0 when either side has no words."
  [a b]
  (let [wa (words a) wb (words b)
        smaller (min (count wa) (count wb))]
    (if (zero? smaller)
      0.0
      (/ (double (count (set/intersection wa wb))) smaller))))

(defn duplicate-pair?
  "h3. True when `pair` restates a claim the other fact already carries.

   `pair` is one row of db/kg-duplicate-candidate-pairs; `thresholds` is
   settings/get-kg-dedup-thresholds' map. nil similarity reads as 0.0 — a missing
   score must never satisfy a branch.
   Pre:  thresholds' three values are in [0.0, 1.0] (settings clamps them).
   Post: true iff any of the three branches fires."
  [{:keys [flat_sim obj_sim subj_eq obj_eq keep_flat dup_flat]}
   {min-flat :flat-trigram min-obj :object-trigram min-containment :containment}]
  (let [flat-sim (double (or flat_sim 0.0))
        obj-sim (double (or obj_sim 0.0))]
    (boolean
      (or (>= flat-sim min-flat)
          (and subj_eq (or obj_eq (>= obj-sim min-obj)))
          (>= (containment keep_flat dup_flat) min-containment)))))

(defn duplicate-ids
  "The `dup_id`s among `pairs` that h3 marks, deduplicated.

   A fact can be the later side of several duplicate pairs (three of production's
   labelled duplicates all pair against one `just-in-time compiler | translates |
   bytecodes into machine language`); it is marked once.

   The result is NOT guaranteed disjoint from the pairs' keep_ids. Given a→b and
   b→c, both b and c are marked, so c's stated justification (b) is itself
   withdrawn. That is the intended reading for the common case — a, b and c all
   restate one claim and only a survives — but it is UNMEASURED for a weak b→c
   beside a strong a→b. Marks are reversible per fact, which is why this is
   acceptable rather than resolved.
   Post: a set of fact ids to mark."
  [pairs thresholds]
  (into #{}
    (comp (filter #(duplicate-pair? % thresholds)) (map :dup_id))
    pairs))

;; ---------------------------------------------------------------------------
;; The sweep
;; ---------------------------------------------------------------------------

(defn sweep-document!
  "Mark near-duplicate facts in one document. The whole detector, end to end.

   `new-ids` nil sweeps every approved pair on every page — the user-invoked
   sweep. A non-empty collection restricts comparison to (pre-existing × new),
   which is what the automatic post-distill sweep passes: same-run pair behaviour
   is unmeasured, and pre-existing × new is the comparison the labels cover.

   Reads thresholds ONCE and passes them down; the O(n²) predicate must not carry
   a settings round trip.

   Does NOT bump an invalidation channel and does NOT toast — callers own that, so
   an all-documents run reports once rather than per document.
   Pre:  graph-topic-id owned by user-id.
   Post: {:marked n :undo-id id-or-nil :pairs n :skipped-pages [k…]}; :marked 0
         with :undo-id nil when the detector is disabled or finds nothing."
  ([user-id graph-topic-id] (sweep-document! user-id graph-topic-id nil nil))
  ([user-id graph-topic-id new-ids] (sweep-document! user-id graph-topic-id new-ids nil))
  ([user-id graph-topic-id new-ids thresholds]
   (let [thresholds (or thresholds (settings/get-kg-dedup-thresholds user-id))]
     (if-not (:enabled thresholds)
       {:marked 0 :undo-id nil :pairs 0 :skipped-pages []}
       (let [skipped (db/kg-oversized-page-keys user-id graph-topic-id max-facts-per-page)
             pairs (db/kg-duplicate-candidate-pairs user-id graph-topic-id
                     new-ids max-facts-per-page)
             ids (duplicate-ids pairs thresholds)
             {:keys [marked undo-id]} (if (seq ids)
                                        (db/mark-kg-facts-duplicate! user-id (vec ids))
                                        {:marked 0 :undo-id nil})]
         (when (seq skipped)
           (tel/log! {:level :warn :id ::pages-skipped
                      :data {:user-id user-id :root graph-topic-id
                             :pages skipped :max-facts-per-page max-facts-per-page}}
             "Duplicate sweep skipped oversized page(s); those facts were not compared"))
         (tel/log! {:level :info :id ::sweep-complete
                    :data {:user-id user-id :root graph-topic-id
                           :scope (if (seq new-ids) :new-vs-existing :all)
                           :pairs (count pairs) :marked marked}}
           "Duplicate sweep complete")
         {:marked marked :undo-id undo-id
          :pairs (count pairs) :skipped-pages skipped})))))

;; ---------------------------------------------------------------------------
;; Commands — the two user-invoked entry points
;; ---------------------------------------------------------------------------

(defn- push-disabled-toast!
  "Both manual sweeps refuse the same way when the detector is switched off, and
   both say where to switch it back on. Returning a silent {:marked 0} instead
   would read as \"no duplicates here\", which is a different and wrong answer."
  [user-id]
  (toasts/push! user-id
    {:level :info :dedup? false
     :message "Duplicate detection is off — turn it on in Settings → AI Features"
     :actions [{:label "Settings" :nav :settings}]})
  {:success true :marked 0 :disabled? true})

(defn- push-failure-toast!
  "A sweep failure must reach the user. The calling button carries the message
   only in its e/Token error slot, which nothing in the Facts view renders."
  [user-id ^Exception e]
  (toasts/push! user-id
    {:level :error :dedup? false
     :message (str "Duplicate sweep failed: " (.getMessage e))})
  {:success false :error (.getMessage e)})

(defn sweep-document-command!
  "User-invoked sweep of one document: sweep, bump, toast with Undo.

   The enabled check lives here rather than inside sweep-document! so a switched
   off detector is reported once, at the boundary, instead of once per document.
   Pre:  graph-topic-id owned by user-id.
   Post: {:success true :marked n} — plus :disabled? true when the detector is
         off — or {:success false :error msg}. Every outcome toasts."
  [user-id graph-topic-id]
  (try
    (let [thresholds (settings/get-kg-dedup-thresholds user-id)]
      (if-not (:enabled thresholds)
        (push-disabled-toast! user-id)
        (let [{:keys [marked undo-id]}
              (sweep-document! user-id graph-topic-id nil thresholds)]
          (commands/bump! user-id :sweep-duplicates)
          (toasts/push! user-id
            (cond-> {:level :success :dedup? false
                     :message (if (pos? marked)
                                (str "Marked " marked " duplicate fact"
                                  (when (not= 1 marked) "s"))
                                "No duplicates found")}
              undo-id (assoc :actions [{:label "Undo" :undo-id undo-id}])))
          {:success true :marked marked})))
    (catch Exception e
      (tel/error! {:id ::sweep-document :data {:user-id user-id :root graph-topic-id}} e)
      (push-failure-toast! user-id e))))

(defn sweep-all-documents-command!
  "User-invoked sweep of every document holding approved facts.

   One transaction and one undo entry PER DOCUMENT, never one across all: the undo
   log is capped at 100 rows per user, so a per-document entry over a large library
   would evict unrelated history — but a single transaction spanning every document
   would hold row locks across the user's whole graph. Per-document is the balance,
   and only the last document's entry carries an Undo action on the toast.

   Thresholds are read ONCE and threaded through every document; the sweep is
   already O(n²) per page and must not add a settings round trip per document.

   A failure on one document is logged and skipped; the rest still run, and the
   result names how many failed rather than reporting partial work as complete.
   Post: {:success true :marked n :documents n :failed n}, or :disabled? true, or
         {:success false :error msg}. Every outcome toasts."
  [user-id]
  (try
    (let [thresholds (settings/get-kg-dedup-thresholds user-id)]
      (if-not (:enabled thresholds)
        (push-disabled-toast! user-id)
        (let [docs (db/kg-documents-with-facts user-id)
              {:keys [marked failed undo-id]}
              (reduce
                (fn [acc doc-id]
                  (try
                    (let [{:keys [marked undo-id]}
                          (sweep-document! user-id doc-id nil thresholds)]
                      (-> acc
                        (update :marked + marked)
                        (cond-> undo-id (assoc :undo-id undo-id))))
                    (catch Exception e
                      (tel/log! {:level :warn :id ::sweep-document-failed
                                 :data {:user-id user-id :root doc-id
                                        :error (ex-message e)}}
                        "Duplicate sweep failed for one document; continuing")
                      (update acc :failed inc))))
                {:marked 0 :failed 0 :undo-id nil}
                docs)]
          (commands/bump! user-id :sweep-duplicates-all)
          (toasts/push! user-id
            (cond-> {:level (if (pos? failed) :warning :success) :dedup? false
                     :message (str "Marked " marked " duplicate fact"
                                (when (not= 1 marked) "s")
                                " across " (count docs) " document"
                                (when (not= 1 (count docs)) "s")
                                (when (pos? failed)
                                  (str " (" failed " document(s) failed)")))}
              undo-id (assoc :actions [{:label "Undo last document" :undo-id undo-id}])))
          {:success true :marked marked :documents (count docs) :failed failed})))
    (catch Exception e
      (tel/error! {:id ::sweep-all-documents :data {:user-id user-id}} e)
      (push-failure-toast! user-id e))))
