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
  "DB fact row → the {:id :s :p :o} shape the synthesis prompt consumes."
  [{:keys [id subject_label predicate_label object_label object_literal]}]
  {:id id :s subject_label :p predicate_label
   :o (or object_label object_literal)})

(defn- persist-questions!
  "Persist the model's [{:q :a :fact-ids}] for one call. An entry is dropped unless
   :q and :a are non-blank strings and :fact-ids yields ≥ min-facts ids drawn from
   `valid-ids` — model output is never trusted, and mitigation belongs here rather
   than at each caller.
   Pre:  `valid-ids` is the set of fact ids this call was allowed to write about.
   Post: {:persisted n :covered #{fact-id}}; :covered is every fact id a persisted
         question linked, so callers can tell which targets went unanswered."
  [user-id kind model-id valid-ids min-facts parsed]
  (reduce
    (fn [acc {:keys [q a fact-ids]}]
      (let [ids (filterv valid-ids fact-ids)]
        (if (and (string? q) (not (str/blank? q))
              (string? a) (not (str/blank? a))
              (>= (count ids) min-facts))
          (do (db/create-kg-question! user-id kind q a ids model-id)
            (-> acc (update :persisted inc) (update :covered into ids)))
          acc)))
    {:persisted 0 :covered #{}}
    (when (sequential? parsed) parsed)))

;; ---------------------------------------------------------------------------
;; Atomic generation — clustered, one single-answer question per fact
;; ---------------------------------------------------------------------------

;; Target facts per LLM call. Clusters are packed up to this many targets, so a
;; call's workload matches the old per-fact batching regardless of cluster width.
(def ^:private atomic-batch-targets 15)

;; Discriminating facts sent per cluster member. Enough to identify a member
;; uniquely; more only dilutes the prompt.
(def ^:private discriminators-per-member 6)

(defn- batch-clusters
  "Pack clusters into calls of ≤ `max-targets` target facts, keeping every cluster
   whole. A split cluster would hide siblings from the model, which is exactly what
   lets it reveal one member's answer inside another's question — so a cluster wider
   than the cap forms its own oversized call instead.
   Post: vector of non-empty cluster vectors; concatenating them reproduces the
         input order."
  [max-targets clusters]
  (->> clusters
    (reduce (fn [{:keys [done current n] :as acc} cluster]
              (let [size (count (:targets cluster))]
                (if (and (seq current) (> (+ n size) max-targets))
                  {:done (conj done current) :current [cluster] :n size}
                  (assoc acc :current (conj current cluster) :n (+ n size)))))
      {:done [] :current [] :n 0})
    ((fn [{:keys [done current]}]
       (cond-> done (seq current) (conj current))))))

(defn- cluster->prompt-row
  "One cluster as the EDN the atomic prompt consumes.

   :members carries every sibling so the model can avoid naming one inside
   another's question; :targets names the ids that still need a question;
   :discriminators are the member's OWN other facts, which is what makes a
   single-answer question possible at all.

   The cluster's own facts are never offered as discriminators — using one would
   put the answer in the stem. Discriminator rows carry no fact id either, so the
   model structurally cannot return one in :fact-ids.
   Pre:  `discriminators` maps object-entity-id → rows from kg-entity-discriminators.
   Post: {:s :p :members [{:id :o :discriminators [{:s :p :o}]}] :targets [id]}."
  [discriminators {:keys [subject_label predicate_label members targets]}]
  (let [own-fact-ids (into #{} (map :id) members)]
    {:s subject_label
     :p predicate_label
     :members (mapv (fn [{:keys [id object_label object_literal object_entity_id]}]
                      (let [rows (->> (get discriminators object_entity_id)
                                   (remove (comp own-fact-ids :fact_id))
                                   (mapv #(select-keys % [:s :p :o])))]
                        (cond-> {:id id :o (or object_label object_literal)}
                          (seq rows) (assoc :discriminators rows))))
                members)
     :targets (vec (sort targets))}))

(defn generate-atomic-questions!
  "One single-answer question per approved-but-uncovered fact of the document.

   Facts are grouped into ambiguity clusters (one subject + predicate, N objects):
   a cluster of N yields N questions, each identifying its own member, rather than
   one \"name one of…\" set question that no answer can be graded against.
   Synchronous — run via start-atomic-generation!.

   Post: {:success true :questions n :batches n :batch-errors n :targets n
          :omitted n} with costs billed, or {:success false :error msg
          :error-type kw?}. :omitted counts target facts no question was written
          for — a member with no distinguishing fact of its own is deliberately
          skipped, which leaves it uncovered and therefore retried, and re-billed,
          on the next run. A failed batch is skipped, never aborts the run."
  [user-id graph-topic-id]
  (try
    (let [{:keys [api-key entry model-slug]} (llm/resolve-model+gate! user-id :atomic)
          clusters (db/fact-clusters-without-question user-id graph-topic-id)
          _ (when (empty? clusters)
              (throw (ex-info "Every fact already has a question." {})))
          all-targets (into #{} (mapcat :targets) clusters)
          prompt (cards/load-prompt-template "kg-question-atomic.md")
          batches (batch-clusters atomic-batch-targets clusters)
          results (mapv (fn [batch]
                          (llm/interrupt-checkpoint!)
                          (let [targets (into #{} (mapcat :targets) batch)]
                            (try
                              (let [object-ids (into [] (comp (mapcat :members)
                                                          (keep :object_entity_id))
                                                 batch)
                                    discriminators (db/kg-entity-discriminators
                                                     user-id object-ids
                                                     discriminators-per-member)
                                    payload (mapv #(cluster->prompt-row discriminators %) batch)
                                    {:keys [parsed cost]}
                                    (llm/chat! api-key model-slug prompt (pr-str payload)
                                      {:feature :kg-atomic :user-id user-id})
                                    {:keys [persisted covered]}
                                    (persist-questions! user-id "atomic" (:id entry)
                                      targets 1 parsed)]
                                {:persisted persisted :cost cost
                                 :omitted (count (remove covered targets))})
                              (catch InterruptedException e (throw e))
                              (catch Exception e
                                (tel/log! {:level :warn :id ::atomic-batch-failed
                                           :data {:root graph-topic-id
                                                  :error (ex-message (llm/root-cause e))}}
                                  "Atomic question batch failed; skipping")
                                {:persisted 0 :cost 0.0 :failed true
                                 :omitted (count targets)}))))
                    batches)
          total-cost (reduce + 0.0 (map :cost results))
          questions (reduce + (map :persisted results))
          omitted (reduce + (map :omitted results))]
      (credits/record-cost-charge! user-id :kg.questions (:id entry) total-cost)
      ;; :omitted is logged because it is the run's non-idempotent remainder — those
      ;; facts come back as targets, and cost money again, every time this runs.
      (tel/log! {:level :info :id ::atomic-generation-complete
                 :data {:user-id user-id :root graph-topic-id
                        :clusters (count clusters)
                        :targets (count all-targets)
                        :questions questions
                        :omitted omitted
                        :batch-errors (count (filter :failed results))
                        :cost-usd total-cost}}
        "Atomic question generation complete")
      {:success true
       :questions questions
       :batches (count batches)
       :batch-errors (count (filter :failed results))
       :targets (count all-targets)
       :omitted omitted})
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
              ;; Same validator as atomic; only the fact-count floor differs — a
              ;; synthesis question that draws on one fact is an atomic question.
              {:keys [persisted]} (persist-questions! user-id "synthesis" (:id entry)
                                    valid-ids 2 parsed)]
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
                    ;; :omitted is surfaced, not just logged: without it a run that
                    ;; writes 12 questions for 40 facts looks like a silent failure.
                    :message (str (:questions result) " question"
                               (when (not= 1 (:questions result)) "s")
                               " generated"
                               (when (pos? (:omitted result 0))
                                 (str " · " (:omitted result)
                                   " fact(s) skipped — too few distinguishing facts"))
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
