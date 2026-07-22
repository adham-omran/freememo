(ns freememo.kg-questions
  "Question generation from the knowledge graph (plan M3).

   Atomic: one question per approved fact not yet covered, batched ~15 facts
   per LLM call, background run per document (executor + toasts + cancel,
   mirroring kg-extract). Synthesis: one call over an entity's fact
   neighborhood, run inline from the UI (single call, e/Offload).

   Questions land status='approved' (curate-by-exception, same as facts);
   costs of all calls are summed and billed once (endpoint :kg.questions)."
  (:require
   [freememo.db :as db]
   [freememo.credits :as credits]
   [freememo.commands :as commands]
   [freememo.toasts :as toasts]
   [freememo.kg-llm :as llm]
   [freememo.user-state :as us]
   [freememo.cards :as cards]
   [taoensso.telemere :as tel]
   [clojure.string :as str]
   [missionary.core :as m])
  (:import [missionary Cancelled]
           [java.util.concurrent Executors]))

(defn- fact->prompt-row
  "DB fact row → the {:id :s :p :o} shape both prompts consume. :alt true
   marks facts with graph siblings (multiple true answers) — the atomic
   prompt phrases those non-exclusively."
  [{:keys [id subject_label predicate_label object_label object_literal
           has_alternatives]}]
  (cond-> {:id id :s subject_label :p predicate_label
           :o (or object_label object_literal)}
    has_alternatives (assoc :alt true)))

;; ---------------------------------------------------------------------------
;; Atomic generation
;; ---------------------------------------------------------------------------

(def ^:private atomic-batch-size 15)

(defn- persist-atomic-batch!
  "Persist the model's [{:id :q :a}] for one batch. Entries whose :id is not
   in the batch, or with blank :q/:a, are dropped (model output, mitigated
   here). Post: count persisted."
  [user-id model-id batch parsed]
  (let [valid-ids (into #{} (map :id) batch)]
    (count
      (for [{:keys [id q a]} (when (sequential? parsed) parsed)
            :when (and (valid-ids id)
                    (string? q) (not (str/blank? q))
                    (string? a) (not (str/blank? a)))]
        (db/create-kg-question! user-id "atomic" q a [id] model-id)))))

(defn generate-atomic-questions!
  "One atomic question for every approved-but-uncovered fact of the document.
   Synchronous — run via start-atomic-generation!.
   Post: {:success true :questions n :batches n :batch-errors n} with costs
         billed, or {:success false :error msg :error-type kw?}. A failed
         batch is skipped, never aborts the run."
  [user-id graph-topic-id]
  (try
    (let [{:keys [api-key entry model-slug]} (llm/resolve-model+gate! user-id :atomic)
          facts (db/facts-without-atomic-question user-id graph-topic-id)
          _ (when (empty? facts)
              (throw (ex-info "Every fact already has a question." {})))
          prompt (cards/load-prompt-template "kg-question-atomic.md")
          batches (partition-all atomic-batch-size (map fact->prompt-row facts))
          results (mapv (fn [batch]
                          (llm/interrupt-checkpoint!)
                          (try
                            (let [{:keys [parsed cost]} (llm/chat! api-key model-slug prompt
                                                          (pr-str (vec batch))
                                                          {:feature :kg-atomic :user-id user-id})]
                              {:persisted (persist-atomic-batch! user-id (:id entry) batch parsed)
                               :cost cost})
                            (catch InterruptedException e (throw e))
                            (catch Exception e
                              (tel/log! {:level :warn :id ::atomic-batch-failed
                                         :data {:root graph-topic-id
                                                :error (ex-message (llm/root-cause e))}}
                                "Atomic question batch failed; skipping")
                              {:persisted 0 :cost 0.0 :failed true})))
                    batches)
          total-cost (reduce + 0.0 (map :cost results))]
      (credits/record-cost-charge! user-id :kg.questions (:id entry) total-cost)
      (tel/log! {:level :info :id ::atomic-generation-complete
                 :data {:user-id user-id :root graph-topic-id
                        :facts (count facts)
                        :questions (reduce + (map :persisted results))
                        :batch-errors (count (filter :failed results))
                        :cost-usd total-cost}}
        "Atomic question generation complete")
      {:success true
       :questions (reduce + (map :persisted results))
       :batches (count batches)
       :batch-errors (count (filter :failed results))})
    ;; A cancelled run must surface as Cancelled to m/via, not as a failure map.
    (catch InterruptedException e (throw e))
    (catch Exception e
      (if (llm/insufficient-credits? e)
        (tel/log! {:level :info :id ::spend-refused :data {:user-id user-id}}
          "Question generation refused: out of credits")
        (tel/error! {:id ::generate-atomic-questions} e))
      {:success false
       :error (ex-message (llm/root-cause e))
       :error-type (when (llm/insufficient-credits? e) :insufficient-credits)})))

;; ---------------------------------------------------------------------------
;; Synthesis generation — single call, callers run it via e/Offload
;; ---------------------------------------------------------------------------

(defn generate-synthesis-questions!
  "1–3 multi-fact questions over an entity's approved neighborhood. Inline
   (one LLM call); pushes its own outcome toast so UI callers stay dumb.
   Pre:  entity owned by user-id.
   Post: {:success true :questions n} (0 when the model finds no multi-fact
         angle or the neighborhood is < 2 facts), or {:success false ...}."
  [user-id entity-id entity-label]
  (try
    (let [facts (db/entity-fact-neighborhood user-id entity-id)]
      (if (< (count facts) 2)
        (do (toasts/push! user-id
              {:level :error
               :message "Needs at least two facts touching this entity."})
          {:success true :questions 0})
        (let [{:keys [api-key entry model-slug]} (llm/resolve-model+gate! user-id :synthesis)
              {:keys [parsed cost]} (llm/chat! api-key model-slug
                                      (cards/load-prompt-template "kg-question-synthesis.md")
                                      (pr-str {:entity entity-label
                                               :facts (mapv fact->prompt-row facts)})
                                      {:feature :kg-synthesis :user-id user-id})
              valid-ids (into #{} (map :id) facts)
              persisted (count
                          (for [{:keys [q a fact-ids]} (when (sequential? parsed) parsed)
                                :let [ids (filterv valid-ids fact-ids)]
                                :when (and (string? q) (not (str/blank? q))
                                        (string? a) (not (str/blank? a))
                                        (>= (count ids) 2))]
                            (db/create-kg-question! user-id "synthesis" q a ids (:id entry))))]
          (credits/record-cost-charge! user-id :kg.questions (:id entry) cost)
          (commands/bump! user-id :generate-questions)
          (toasts/push! user-id
            {:level :success
             :message (str persisted " synthesis question"
                        (when (not= 1 persisted) "s") " for “" entity-label "”")})
          {:success true :questions persisted})))
    (catch Exception e
      (if (llm/insufficient-credits? e)
        (tel/log! {:level :info :id ::spend-refused :data {:user-id user-id}}
          "Synthesis generation refused: out of credits")
        (tel/error! {:id ::generate-synthesis-questions} e))
      (toasts/push! user-id
        {:level :error
         :message (ex-message (llm/root-cause e))
         :actions (if (llm/insufficient-credits? e)
                    [{:label "Top up credits" :nav :settings}]
                    [])})
      {:success false :error (ex-message (llm/root-cause e))})))

;; ---------------------------------------------------------------------------
;; Async entry point for atomic runs — mirrors kg-extract/start-distill!
;; ---------------------------------------------------------------------------

(defonce question-executor (Executors/newFixedThreadPool 2))

(def ^:private generation-timeout-ms (* 20 60 1000))

(defn start-atomic-generation!
  "Submit an atomic-generation run for (uid, root-topic-id). No-op when one is
   already in flight for that document. Progress in :generating-questions,
   cancel fn in :question-cancellers; completion toasts + bumps
   :generate-questions (:kg-mutations)."
  [uid root-topic-id]
  (when-not (contains? @(us/get-atom uid :generating-questions) root-topic-id)
    (swap! (us/get-atom uid :generating-questions) conj root-topic-id)
    (tel/log! {:level :info :id ::atomic-generation-started
               :data {:user-id uid :root root-topic-id}}
      "Atomic question generation started")
    (let [cancel-fn
          ((m/timeout
             (m/via question-executor (generate-atomic-questions! uid root-topic-id))
             generation-timeout-ms
             {:success false :error "Question generation timed out after 20 minutes"})
           (fn [result]
             (swap! (us/get-atom uid :question-cancellers) dissoc root-topic-id)
             (swap! (us/get-atom uid :generating-questions) disj root-topic-id)
             (if (:success result)
               (do (commands/bump! uid :generate-questions)
                 (toasts/push! uid
                   {:level :success
                    :message (str (:questions result) " question"
                               (when (not= 1 (:questions result)) "s")
                               " generated"
                               (when (pos? (:batch-errors result 0))
                                 (str " (" (:batch-errors result) " batch(es) failed)")))}))
               (toasts/push! uid
                 {:level :error
                  :message (:error result)
                  :actions (if (= :insufficient-credits (:error-type result))
                             [{:label "Top up credits" :nav :settings}]
                             [])})))
           (fn [e]
             (swap! (us/get-atom uid :question-cancellers) dissoc root-topic-id)
             (swap! (us/get-atom uid :generating-questions) disj root-topic-id)
             (when-not (instance? Cancelled e)
               (tel/error! {:id ::atomic-generation-task} e)
               (toasts/push! uid {:level :error :message (ex-message e)}))))]
      (swap! (us/get-atom uid :question-cancellers) assoc root-topic-id cancel-fn))
    nil))

(defn cancel-atomic-generation!
  "Cancel an in-flight atomic run for (uid, root-topic-id), if any."
  [uid root-topic-id]
  (when-let [cancel-fn (get @(us/get-atom uid :question-cancellers) root-topic-id)]
    (cancel-fn)
    nil))
