(ns freememo.kg-extract
  "Two-pass document distillation into the knowledge graph (see
   plans/knowledge-graph-quizzes.md M2).

   Pass 1 — extraction: per page, LLM emits {:s :p :o|:lit} triples
   constrained toward the user's approved predicate vocabulary.
   Pass 2 — linking: distinct extracted labels are trigram-matched against
   kg_entities; one batched LLM call decides merge-vs-new per label.

   Facts and predicates land status='approved' — curation is by exception in
   the Knowledge facts browser (decision revised 2026-07-04). Costs of all
   calls are summed and billed once (endpoint :kg.distill), mirroring
   freememo.cards/generate-cards*."
  (:require
   [freememo.db :as db]
   [freememo.credits :as credits]
   [freememo.commands :as commands]
   [freememo.toasts :as toasts]
   [freememo.kg-llm :as llm]
   [freememo.user-state :as us]
   [freememo.text :as text]
   [freememo.cards :as cards]
   [taoensso.telemere :as tel]
   [clojure.string :as str]
   [missionary.core :as m])
  (:import [missionary Cancelled]
           [java.util.concurrent Executors Semaphore]))

(defn slugify
  "Predicate label → slug satisfying kg_predicates' CHECK
   (^[a-z0-9]+(-[a-z0-9]+)*$). nil when nothing IRI-safe survives —
   callers must skip such facts."
  [label]
  (let [s (-> (str label)
            str/lower-case
            (str/replace #"[^a-z0-9]+" "-")
            (str/replace #"^-+|-+$" ""))]
    (when-not (str/blank? s) s)))

(defn- valid-triple?
  "Shape gate for pass-1 output: :s and :p non-blank strings, exactly one of
   :o / :lit a non-blank string. Invalid entries are dropped, not fatal —
   blame: model output, mitigated here."
  [{:keys [s p o lit]}]
  (and (string? s) (not (str/blank? s))
       (string? p) (not (str/blank? p))
       (= 1 (count (filter #(and (string? %) (not (str/blank? %))) [o lit])))
       (or (nil? o) (string? o))
       (or (nil? lit) (string? lit))))

;; ---------------------------------------------------------------------------
;; LLM calls
;; ---------------------------------------------------------------------------

(defn- extraction-prompt
  "kg-extract.md + the user's approved vocabulary (empty on a fresh graph —
   the model then proposes everything, per the no-seed decision)."
  [vocab]
  (str (cards/load-prompt-template "kg-extract.md")
    (if (seq vocab)
      (str/join "\n" (map #(str "- " (:label %)) vocab))
      "(none yet — propose predicates freely)")))

(defn- extract-page-triples!
  "Pass 1 for one page. Post: {:triples [...] :cost usd}; triples are
   shape-valid and tagged :page. Retries malformed output once."
  [api-key model-slug prompt page-number page-text]
  (loop [attempt 1]
    (let [result (try (llm/chat! api-key model-slug prompt page-text)
                   (catch clojure.lang.ExceptionInfo e
                     (if (and (= 1 attempt) (:raw (ex-data e)))
                       ::retry
                       (throw e))))]
      (if (= ::retry result)
        (recur 2)
        (let [{:keys [parsed cost]} result
              triples (if (sequential? parsed) parsed [])
              valid (filterv valid-triple? triples)]
          (when (< (count valid) (count triples))
            (tel/log! {:level :warn :id ::invalid-triples-dropped
                       :data {:page page-number
                              :dropped (- (count triples) (count valid))}}
              "Dropped shape-invalid triples from extraction output"))
          {:triples (mapv #(assoc % :page page-number) valid)
           :cost cost})))))

(def ^:private extract-page-concurrency
  "Bounds concurrent per-page extraction calls — mirrors page-ocr's
   compare-ocr-limiter; a long PDF's pages must not fire N OpenRouter calls
   at once."
  4)

(defn- extract-pages!
  "Pass 1 over every page, bounded-concurrent (extract-page-concurrency at a
   time) instead of one page at a time, serially. Post: a seq of
   extract-page-triples!'s results (or a {:failed true} stub on error),
   order matching `pages`.
   Checks llm/interrupt-checkpoint! before fan-out and again after each page
   completes, so a cancelled run unwinds without waiting out the rest — the
   handful of pages already in flight when cancellation lands keep running to
   completion in the background, same blast radius as the prior sequential
   loop's one in-flight call, just bounded by extract-page-concurrency
   instead of 1."
  [api-key model-slug prompt pages root-topic-id]
  (llm/interrupt-checkpoint!)
  (let [limiter (Semaphore. extract-page-concurrency)
        futs (mapv
               (fn [{:keys [page text]}]
                 (future
                   (.acquire limiter)
                   (try
                     (extract-page-triples! api-key model-slug prompt page text)
                     (catch InterruptedException e (throw e))
                     (catch Exception e
                       (tel/log! {:level :warn :id ::page-extraction-failed
                                  :data {:root root-topic-id :page page
                                         :error (ex-message (llm/root-cause e))}}
                         "Page extraction failed; skipping page")
                       {:triples [] :cost 0.0 :failed true})
                     (finally (.release limiter)))))
               pages)]
    (mapv (fn [f] (llm/interrupt-checkpoint!) (deref f)) futs)))

(defn- link-entities!
  "Pass 2. Distinct labels → {lower-label entity-id}, creating rows for :new.
   Labels without trigram candidates skip the LLM. A failed/invalid LLM
   decision degrades to :new (a duplicate entity is reviewable later; a wrong
   merge is not). Post: every input label has an id; :cost is the LLM spend."
  [api-key model-slug user-id labels]
  (let [distinct-labels (into [] (comp (map str/trim) (distinct)) labels)
        by-lower (into {} (map (juxt str/lower-case identity)) distinct-labels)
        ;; One batched trigram query for every label instead of a round trip
        ;; per label; the LLM merge decision itself was already one call.
        candidates-by-label (db/find-entity-link-candidates-batch user-id (vals by-lower) 6)
        candidates (into {}
                     (keep (fn [[_ label]]
                             (when-let [cs (seq (get candidates-by-label label))]
                               [label (mapv #(select-keys % [:id :label :aliases]) cs)])))
                     by-lower)
        {:keys [decisions cost]}
        (if (empty? candidates)
          {:decisions {} :cost 0.0}
          (try
            (let [{:keys [parsed cost]} (llm/chat! api-key model-slug
                                          (cards/load-prompt-template "kg-link.md")
                                          (pr-str candidates))]
              {:decisions (if (map? parsed) parsed {}) :cost cost})
            (catch Exception e
              (tel/log! {:level :warn :id ::link-degraded
                         :data {:labels (count candidates) :error (ex-message e)}}
                "Entity-link call failed; treating all as new entities")
              {:decisions {} :cost 0.0})))
        valid-ids (into #{} (comp (mapcat val) (map :id)) candidates)
        id-of (fn [label]
                (let [d (get decisions label)]
                  (when (and (integer? d) (valid-ids d)) d)))
        ;; Labels with no merge decision are new — batch-insert them all in
        ;; one round trip instead of one insert-kg-entity! per label.
        new-labels (into [] (comp (map val) (remove id-of)) by-lower)
        new-ids (zipmap new-labels (db/insert-kg-entities! user-id new-labels))]
    {:by-label (into {}
                 (map (fn [[lower label]]
                        [lower (or (id-of label) (get new-ids label))]))
                 by-lower)
     :cost cost}))

;; ---------------------------------------------------------------------------
;; Orchestrator
;; ---------------------------------------------------------------------------

(defn- document-pages
  "Pages with text under root-topic-id; falls back to the root topic's own
   content (wiki/web imports have no page children). Post: seq of
   {:page nilable-int :text string}, text non-blank, HTML stripped."
  [root-topic-id]
  (let [pages (->> (db/get-context-pages root-topic-id 1 Integer/MAX_VALUE)
                (keep (fn [row]
                        (let [t (text/strip-html (:topics/content row))]
                          (when-not (str/blank? t)
                            {:page (:topics/page_number row) :text t})))))]
    (if (seq pages)
      pages
      (when-let [t (text/strip-html (:topics/content (db/get-topic root-topic-id)))]
        (when-not (str/blank? t)
          [{:page nil :text t}])))))

(defn distill-document!
  "Distill root-topic-id's pages into facts (status='approved'). Synchronous —
   run it on the executor via start-distill!.
   Pre:  root-topic-id belongs to user-id (callers pass ids from the user's
         own tree — violation = caller bug); OpenRouter key configured.
   Post: {:success true :pages n :page-errors n :facts n :entities n}
         with facts inserted as 'proposed' and the summed cost billed, or
         {:success false :error msg :error-type kw?}. Per-page extraction
         failures skip the page, never abort the run."
  [user-id root-topic-id]
  (try
    ;; Extraction and entity-linking are independently configurable steps
    ;; (:extract / :link). model-slug drives extraction; link-slug drives
    ;; linking. entry (the extraction model) is what facts record as
    ;; :source_model and what the summed distill cost bills under.
    (let [{:keys [api-key entry model-slug]} (llm/resolve-model+gate! user-id :extract)
          link-slug (:model-slug (llm/resolve-model+gate! user-id :link))
          pages (or (seq (document-pages root-topic-id))
                    (throw (ex-info "No page text. Extract text first." {})))
          prompt (extraction-prompt (db/get-kg-approved-predicates user-id))
          extractions (extract-pages! api-key model-slug prompt pages root-topic-id)
          _ (llm/interrupt-checkpoint!)
          triples (into [] (mapcat :triples) extractions)
          entity-labels (into [] (comp (mapcat (juxt :s :o)) (remove nil?)) triples)
          {:keys [by-label] link-cost :cost}
          (if (seq entity-labels)
            (link-entities! api-key link-slug user-id entity-labels)
            {:by-label {} :cost 0.0})
          entity-id #(get by-label (str/lower-case (str/trim %)))
          predicate-id (memoize
                         (fn [p-label]
                           (when-let [slug (slugify p-label)]
                             (:id (db/get-or-create-kg-predicate! user-id slug p-label)))))
          rows (into []
                 (keep (fn [{:keys [s p o lit page]}]
                         (when-let [pid (predicate-id p)]
                           {:user_id user-id
                            :subject_entity_id (entity-id s)
                            :predicate_id pid
                            :object_entity_id (some-> o entity-id)
                            :object_literal lit
                            :object_datatype nil
                            :graph_topic_id root-topic-id
                            :page_number page
                            :status "approved" ; curate by exception, not pre-approval
                            :source_model (:id entry)})))
                 ;; s = o self-loops say nothing; drop them.
                 (remove #(and (:o %) (= (entity-id (:s %)) (entity-id (:o %)))) triples))
          inserted (or (db/insert-kg-facts! rows) [])
          total-cost (reduce + link-cost (map :cost extractions))]
      (credits/record-cost-charge! user-id :kg.distill (:id entry) total-cost)
      (tel/log! {:level :info :id ::distill-complete
                 :data {:user-id user-id :root root-topic-id
                        :pages (count pages)
                        :page-errors (count (filter :failed extractions))
                        :triples (count triples) :facts (count inserted)
                        :cost-usd total-cost}}
        "Document distillation complete")
      {:success true
       :pages (count pages)
       :page-errors (count (filter :failed extractions))
       :facts (count inserted)
       :entities (count by-label)})
    ;; A cancelled run must surface as Cancelled to m/via, not as a failure map.
    (catch InterruptedException e (throw e))
    (catch Exception e
      (if (llm/insufficient-credits? e)
        (tel/log! {:level :info :id ::spend-refused :data {:user-id user-id}}
          "Distillation refused: out of credits")
        (tel/error! {:id ::distill-document} e))
      {:success false
       :error (ex-message (llm/root-cause e))
       :error-type (when (llm/insufficient-credits? e) :insufficient-credits)})))

;; ---------------------------------------------------------------------------
;; Async entry point — mirrors pdf-action-dropdowns/start-ocr-scan!
;; ---------------------------------------------------------------------------

(defonce distill-executor (Executors/newFixedThreadPool 2))

(def ^:private distill-timeout-ms
  "20 min — a long PDF at ~2 LLM calls/page."
  (* 20 60 1000))

(defn start-distill!
  "Submit a distill run for (uid, root-topic-id) on the bounded executor.
   No-op when one is already in flight for that document. Tracks progress in
   the :distilling-docs set, a cancel fn in :distill-cancellers; completion
   toasts + bumps :distill (:kg-mutations)."
  [uid root-topic-id]
  (when-not (contains? @(us/get-atom uid :distilling-docs) root-topic-id)
    (swap! (us/get-atom uid :distilling-docs) conj root-topic-id)
    (tel/log! {:level :info :id ::distill-started
               :data {:user-id uid :root root-topic-id}}
      "Distillation started")
    (let [cancel-fn
          ((m/timeout
             (m/via distill-executor (distill-document! uid root-topic-id))
             distill-timeout-ms
             {:success false :error "Distillation timed out after 20 minutes"})
           (fn [result]
             (swap! (us/get-atom uid :distill-cancellers) dissoc root-topic-id)
             (swap! (us/get-atom uid :distilling-docs) disj root-topic-id)
             (if (:success result)
               (do (commands/bump! uid :distill)
                 (toasts/push! uid
                   {:level :success
                    :message (str (:facts result) " fact"
                               (when (not= 1 (:facts result)) "s")
                               " added"
                               (when (pos? (:page-errors result 0))
                                 (str " (" (:page-errors result) " page(s) failed)"))
                               " — browse Knowledge to curate")}))
               (toasts/push! uid
                 {:level :error
                  :message (:error result)
                  :actions (if (= :insufficient-credits (:error-type result))
                             [{:label "Top up credits" :nav :settings}]
                             [])})))
           (fn [e]
             (swap! (us/get-atom uid :distill-cancellers) dissoc root-topic-id)
             (swap! (us/get-atom uid :distilling-docs) disj root-topic-id)
             (when-not (instance? Cancelled e)
               (tel/error! {:id ::distill-task} e)
               (toasts/push! uid {:level :error :message (ex-message e)}))))]
      (swap! (us/get-atom uid :distill-cancellers) assoc root-topic-id cancel-fn))
    nil))

(defn cancel-distill!
  "Cancel an in-flight distill run for (uid, root-topic-id), if any."
  [uid root-topic-id]
  (when-let [cancel-fn (get @(us/get-atom uid :distill-cancellers) root-topic-id)]
    (cancel-fn)
    nil))
