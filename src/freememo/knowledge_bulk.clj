(ns freememo.knowledge-bulk
  "Bulk operations of the Knowledge tables (plans/knowledge-tables-selection-and-bulk.md
   section 5). Plain server code: the Electric views call these through `e/server`.

   Every fn here has the same shape, and `knowledge-facts/reject-facts!*` is the
   reference for it:
     - ownership is re-checked in SQL, never trusted from the client's id list;
     - the write is ONE transaction (or, for runs, one pass over owned ids);
     - one audit line and one `commands/bump!`, however many rows moved. The changed rows confirm a success, and the
       Undo history lists the undoable ones (plans/remove-toasts.md 2.1, 2.2);
     - the result is {:success true ...} or {:success false :error msg}, and an
       exception is logged and returned, never thrown into the reactive graph.

   A count of 0 is a success with no bump or audit line: nothing changed, and
   a bump would re-query every consumer for no reason."
  (:require
   [clojure.string :as str]
   [freememo.commands :as commands]
   [freememo.credits :as credits]
   [freememo.db :as db]
   [freememo.kg-extract :as kg]
   [freememo.kg-questions :as kgq]
   [freememo.logging :as log]
   [freememo.settings :as settings]
   [freememo.user-state :as us]
   [taoensso.telemere :as tel]))

(defn- failure [id user-id ^Exception e]
  (tel/error! {:id id :data {:user-id user-id}} e)
  {:success false :error (or (.getMessage e) "Unexpected error")})

(defn failure-text
  "The words a failed `result` shows beside the control that asked.
   The control stays on screen through the result, so the failure renders there
   through `InlineError`: the view spends its token with `(t (failure-text r))`
   (plans/remove-toasts.md 2.5).
   Post: nil for a success, else a non-blank string."
  [result]
  (when (false? (:success result))
    (let [text (:error result)]
      (if (str/blank? (str text)) "That action failed." text))))

;; ---------------------------------------------------------------------------
;; Questions
;; ---------------------------------------------------------------------------

(defn reject-questions!
  "Reject a selection of questions as one operation: one undo entry, one bump, one
   audit line. The Undo history lists the entry.
   Pre:  question-ids is the user's selection; foreign ids are skipped in SQL.
   Post: {:success true :rejected n}, or
         {:success false :error msg}. n counts only owned questions that were not
         already rejected."
  [user-id question-ids]
  (try
    (let [{:keys [rejected]} (db/reject-kg-questions! user-id question-ids)]
      (when (pos? rejected)
        (commands/bump! user-id :bulk-reject-questions)
        (log/audit! {:id ::reject-questions :user-id user-id :action :delete
                     :entity :kg-question :entity-id nil :n rejected}))
      {:success true :rejected rejected})
    (catch Exception e
      (failure ::reject-questions user-id e))))

(defn set-questions-flags!
  "Set one curation flag on a selection of questions with a single UPDATE. No undo:
   the opposite bulk action reverses it, and no FSRS column is written.
   Pre:  flags is {:flagged bool} or {:suspended bool}, exactly one key.
   Post: {:success true :changed n} where n counts the questions whose flag
         actually changed (approved, owned, and not already in the target state), or
         {:success false :error msg}."
  [user-id question-ids flags]
  (try
    (let [[flag value] (when (= 1 (count (keep (fn [[k v]] (when (some? v) k)) flags)))
                         (first (filter (comp some? val) flags)))]
      (if-not (and (#{:flagged :suspended} flag) (boolean? value))
        {:success false :error "Choose one flag and a true or false value."}
        (let [changed (db/set-kg-questions-flag! user-id question-ids flag value)
              command (if (= flag :flagged) :bulk-flag-questions :bulk-suspend-questions)]
          (when (pos? changed)
            (commands/bump! user-id command)
            (log/audit! {:id ::set-questions-flags :user-id user-id :action :update
                         :entity :kg-question :entity-id nil :n changed}))
          {:success true :changed changed})))
    (catch Exception e
      (failure ::set-questions-flags user-id e))))

;; ---------------------------------------------------------------------------
;; Facts
;; ---------------------------------------------------------------------------

(defn unmark-duplicate-facts!
  "Return duplicate-marked facts to approved. Rows in any other status are left
   alone. The questions the duplicate mark retired stay retired (see
   db/unmark-kg-fact-duplicate!). No undo.
   Post: {:success true :restored n} or {:success false :error msg}."
  [user-id fact-ids]
  (try
    (let [restored (db/unmark-kg-facts-duplicate! user-id fact-ids)]
      (when (pos? restored)
        (commands/bump! user-id :bulk-unmark-duplicate-facts)
        (log/audit! {:id ::unmark-duplicate-facts :user-id user-id :action :update
                     :entity :kg-fact :entity-id nil :n restored}))
      {:success true :restored restored})
    (catch Exception e
      (failure ::unmark-duplicate-facts user-id e))))

;; ---------------------------------------------------------------------------
;; Entities
;; ---------------------------------------------------------------------------

(defn merge-entities!
  "Merge every entity in `absorbed-ids` into `keep-id` as ONE transaction. A failure
   on any of them leaves all of them unmerged. Irreversible, so there is no undo.
   Pre:  absorbed-ids non-empty, keep-id not among them, every id owned by user-id.
         A violation refuses the whole merge before anything is written.
   Post: {:success true :merged n} or {:success false :error msg}."
  [user-id keep-id absorbed-ids]
  (try
    (let [label (db/get-kg-entity-label user-id keep-id)]
      (if (nil? label)
        {:success false :error "Entity not found"}
        (let [merged (db/merge-kg-entities-into! user-id keep-id absorbed-ids)]
          (commands/bump! user-id :bulk-merge-entities)
          (log/audit! {:id ::merge-entities :user-id user-id :action :delete
                       :entity :kg-entity :entity-id keep-id :n merged})
          {:success true :merged merged})))
    (catch Exception e
      (failure ::merge-entities user-id e))))

(defn entity-choices
  "The selected entities, for the merge step's pick-one-to-keep list. Reads by id,
   so entities the current filter hides are included.
   Post: vector of {:id :label :fact_count}, most facts first; foreign ids absent."
  [user-id entity-ids]
  (db/kg-entity-labels user-id entity-ids))

;; ---------------------------------------------------------------------------
;; Documents: runs on the existing executors
;; ---------------------------------------------------------------------------

(defn run-gate
  "The cheap credit check for a bulk run, before any run starts.

   Each run already checks credits itself, in `kg-llm/resolve-model+gate!` on the
   executor thread. For a bulk start that is not enough: N runs would fail one by
   one and report N 'out of credits' failures. This asks the same two questions once,
   up front, so one refusal replaces N failures.

   It cannot know what a run will cost, so a run can still hit zero balance part of
   the way through a large batch. That run then fails with its own error, as a single
   run does today.
   Post: {:ok true}, or {:ok false :error msg :error-type kw}. :error-type is
         :insufficient-credits when a top-up would fix it."
  [user-id]
  (if (str/blank? (settings/get-openrouter-api-key user-id))
    {:ok false :error "OpenRouter API key not configured"}
    (let [gate (credits/check-cost-billed-balance! user-id)]
      (if (:ok gate)
        gate
        (assoc gate :error-type :insufficient-credits)))))

(defn- start-runs!
  "Shared body of the two bulk starts. `running-key` names the in-flight set and
   `start!` is the executor entry point.
   Returns {:running-ids [...] :started-ids [...]} for the owned ids it was given."
  [user-id doc-ids running-key start!]
  (let [running @(us/get-atom user-id running-key)
        {running-ids true started-ids false} (group-by (comp boolean running) doc-ids)]
    (doseq [id started-ids]
      (start! user-id id))
    {:running-ids (vec running-ids) :started-ids (vec started-ids)}))

(defn start-distills!
  "Start a distill run for each selected document the user owns, on the existing
   executor (two threads, so most queue behind the first two). A document that
   already has a run in flight is skipped, which `kg/start-distill!` would do anyway.
   The Activity monitor discovers each run through :distilling-docs.
   Pre:  doc-ids are root topic ids; non-root, foreign and staged ids are dropped by
         the ownership check.
   Post: {:success true :started n :running m}, where m counts documents that
         already ran. {:success false :error msg} when the credit gate refuses (no
         run starts), when ids were given and none is an owned document, or on error."
  [user-id doc-ids]
  (try
    (if (empty? doc-ids)
      {:success true :started 0 :running 0}
      (let [owned (db/owned-root-topic-ids user-id doc-ids)
            gate (when (seq owned) (run-gate user-id))]
        (cond
          (empty? owned) {:success false :error "No selected document was found."}
          (not (:ok gate)) (select-keys (assoc gate :success false) [:success :error :error-type])
          :else
          (let [{:keys [running-ids started-ids]}
                (start-runs! user-id owned :distilling-docs kg/start-distill!)
                started (count started-ids)
                running (count running-ids)]
            (when (or (pos? started) (pos? running))
              (log/audit! {:id ::start-distills :user-id user-id :action :create
                           :entity :kg-distill-run :entity-id nil :n started})
              (when (pos? started) (commands/bump! user-id :bulk-distill)))
            {:success true :started started :running running}))))
    (catch Exception e
      (failure ::start-distills user-id e))))

(defn start-question-generations!
  "Start an atomic question run for each selected document that has at least one
   approved fact. A document with none is skipped and counted, because a run on it
   would only fail with 'Every fact already has a question'.
   Pre:  doc-ids as in start-distills!.
   Post: {:success true :started n :skipped k :running m}. k counts owned documents
         with no approved facts, m counts those already generating. The refusals of
         start-distills! apply, and a refusal starts nothing."
  [user-id doc-ids]
  (try
    (if (empty? doc-ids)
      {:success true :started 0 :skipped 0 :running 0}
      (let [owned (db/owned-root-topic-ids user-id doc-ids)
            with-facts (set (db/kg-documents-with-facts-among user-id owned))
            skipped (- (count owned) (count with-facts))
            gate (when (seq with-facts) (run-gate user-id))]
        (cond
          (empty? owned) {:success false :error "No selected document was found."}
          (and gate (not (:ok gate)))
          (select-keys (assoc gate :success false) [:success :error :error-type])
          :else
          (let [{:keys [running-ids started-ids]}
                (start-runs! user-id (filterv with-facts owned)
                  :generating-questions kgq/start-atomic-generation!)
                started (count started-ids)
                running (count running-ids)]
            (log/audit! {:id ::start-question-generations :user-id user-id :action :create
                         :entity :kg-question-run :entity-id nil :n started})
            (when (pos? started) (commands/bump! user-id :bulk-generate-questions))
            ;; The Activity monitor shows each run, and the view reads
            ;; `:skipped` and `:running` into a ResultLine at its batch bar.
            {:success true :started started :skipped skipped :running running}))))
    (catch Exception e
      (failure ::start-question-generations user-id e))))
