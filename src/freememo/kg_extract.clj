(ns freememo.kg-extract
  "Two-pass document distillation into the knowledge graph (see
   plans/knowledge-graph-quizzes.md M2).

   Pass 1 — extraction: per page, LLM emits {:s :p :o|:lit} triples
   constrained toward the user's approved predicate vocabulary.
   Pass 2 — linking: distinct extracted labels are trigram-matched against
   kg_entities; one batched LLM call decides merge-vs-new per label.

   THE TWO PASSES PERSIST AT DIFFERENT GRANULARITIES, and that is the whole page
   skip. Pass 1 writes each page's triples to `kg_staged_triples` as soon as that
   page returns, with a row in `kg_page_extractions` recording the digest of the
   text it read. Pass 2 runs ONCE per invocation over every staged triple the
   document holds, including triples an earlier cancelled invocation left behind.

   Two properties follow, and neither held before:

     A re-distill pays only for pages whose text changed. `classify-page` skips
     the rest, so the cost of re-running scales with edits rather than with the
     document.

     A cancel or the 20-minute timeout is a pause, not a loss. Extraction used to
     hold every page's triples in memory and insert once at the end, so a cancel
     at page 400 of 500 discarded 400 pages of paid LLM calls — and a document
     past roughly 300 pages could never finish inside the timeout at all.

   What the skip does NOT gate on: the prompt, the extraction model, and the
   predicate vocabulary. All three are recorded on the extraction row and never
   compared, because the vocabulary grows on every curation and an auto-compared
   prompt hash would re-bill every page of every document over a typo fix.
   `db/clear-document-extraction-records!` is the deliberate path for reaching
   already-extracted pages after such a change. See
   plans/kg-page-skip-extraction.md.

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
   [freememo.llm-json :as llm-json]
   [freememo.kg-dedup :as kg-dedup]
   [freememo.kg-page-digest :as digest]
   [freememo.user-state :as us]
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
   the model then proposes everything, per the no-seed decision).

   Takes `template` rather than loading it, so the caller can record the
   template's version on the extraction rows without reading the file twice."
  [template vocab]
  (str template
    (if (seq vocab)
      (str/join "\n" (map #(str "- " (:label %)) vocab))
      "(none yet — propose predicates freely)")))

(defn- prompt-template-version
  "A short digest of the extraction template, recorded on every extraction row.

   Forensic only — `classify-page` never reads it. That is what lets it be a
   content hash instead of a hand-bumped constant: a hand-bumped constant goes
   stale silently, and an auto-compared hash would re-bill every page of every
   document the moment someone fixes a typo in the prompt. Recorded, it answers
   \"which prompt produced these facts?\" and costs nothing.

   The VOCABULARY is deliberately excluded, even though the model sees it: it grows
   on every curation, so including it would make this column change constantly and
   say nothing about the prompt.

   Post: 12 hex characters, or nil for a blank template."
  [template]
  (some-> (digest/digest-of template) (subs 0 12)))

(defn- extract-page-triples!
  "Pass 1 for one page. Post: {:triples [...] :cost usd :tokens [m …]};
   triples are shape-valid and tagged :page. Unusable model output is retried
   inside llm/chat!, whose :cost and :tokens cover both attempts."
  [api-key model-fields prompt page-number page-text]
  (let [{:keys [parsed cost tokens]} (llm/chat! api-key model-fields prompt
                                       (llm-json/write-prompt-input {:page-text page-text})
                                       :triples :vector
                                       {:feature :kg-extract})
        triples (if (sequential? parsed) parsed [])
        valid (filterv valid-triple? triples)]
    (when (< (count valid) (count triples))
      (tel/log! {:level :warn :id ::invalid-triples-dropped
                 :data {:page page-number
                        :dropped (- (count triples) (count valid))}}
        "Dropped shape-invalid triples from extraction output"))
    {:triples (mapv #(assoc % :page page-number) valid)
     :cost cost
     :tokens tokens}))

(def attempt-cap
  "How many times one page's extraction may fail on unchanged text before this
   stops paying to retry it.

   The cap is scoped to the page's content digest, so editing the page resets the
   count (`db/record-page-extraction-failure!` owns that rule). A capped page is
   excluded from selection AND reported per document, because a page nobody can
   see was abandoned is the failure this log exists to prevent.

   Two, because a transient provider error clears on the first retry and a
   deterministic one — content the model refuses, a page too long for the context
   — will not clear on the tenth. `db/kg-extraction-progress` MUST be called with
   this value, or the surface disagrees with the run."
  2)

(def ^:private extract-page-concurrency
  "Bounds concurrent per-page extraction calls — mirrors page-ocr's
   compare-ocr-limiter; a long PDF's pages must not fire N OpenRouter calls
   at once."
  4)

(defn- record-failure-quietly!
  "Count a failed attempt, swallowing any database error.

   A throw here would replace the page's outcome with an exception and lose the
   cost the caller still has to bill, so this path reports and returns. The price
   of swallowing is one uncounted attempt, which costs one extra retry — strictly
   better than losing the run."
  [ctx page-state error]
  (try
    (db/record-page-extraction-failure!
      (assoc ctx
        :page-topic-id (:page_topic_id page-state)
        :content-digest (:digest page-state)
        :error-message (ex-message (llm/root-cause error))))
    (catch InterruptedException e (throw e))
    (catch Exception e
      (tel/log! {:level :warn :id ::failure-record-failed
                 :data {:page (:page_number page-state)
                        :error (ex-message e)}}
        "Could not record a failed page extraction; its attempt is uncounted"))))

(defn- extract-and-stage-page!
  "One page's whole unit of work: extract, then persist, before returning.

   Persisting HERE rather than after the fan-out is what makes a cancelled or
   timed-out run resumable. The previous shape held every page's triples in memory
   and inserted once at the end, so a cancel at page 400 of 500 discarded 400 pages
   of paid LLM calls and the document could never finish inside the 20-minute
   timeout.

   Pre : `page-state` carries :page_topic_id, :page_number, :content_text and the
         :digest of that text.
   Post: {:cost usd :tokens [m …] :extracted true}, or the same with :failed
         true. `:cost` and `:tokens` are what the provider charged whether or
         not the write succeeded — a lost write
         does not un-bill the call. Exactly one extraction row is written either
         way, so the page is never left unaccounted."
  [api-key model-fields prompt ctx page-state]
  (let [outcome (try
                  {:kind :extracted
                   :result (extract-page-triples! api-key model-fields prompt
                             (:page_number page-state) (:content_text page-state))}
                  (catch InterruptedException e (throw e))
                  (catch Exception e {:kind :failed :error e}))]
    (case (:kind outcome)
      :extracted
      (let [{:keys [triples cost tokens]} (:result outcome)]
        (try
          (db/record-page-extraction!
            (assoc ctx
              :page-topic-id (:page_topic_id page-state)
              :content-digest (:digest page-state)
              :triples triples))
          {:cost cost :tokens tokens :extracted true}
          (catch InterruptedException e (throw e))
          (catch Exception e
            (tel/log! {:level :warn :id ::page-staging-failed
                       :data {:page (:page_number page-state)
                              :triples (count triples)
                              :error (ex-message e)}}
              "Extracted a page but could not stage it; the page will be retried")
            (record-failure-quietly! ctx page-state e)
            {:cost cost :tokens tokens :failed true})))

      :failed
      (do
        (tel/log! {:level :warn :id ::page-extraction-failed
                   :data {:root (:graph-topic-id ctx)
                          :page (:page_number page-state)
                          :error (ex-message (llm/root-cause (:error outcome)))}}
          "Page extraction failed; the page keeps its place in the queue")
        (record-failure-quietly! ctx page-state (:error outcome))
        {:cost 0.0 :failed true}))))

(defn- extract-and-stage-pages!
  "Pass 1 over the pages that need it, bounded-concurrent (extract-page-concurrency
   at a time), each page persisted as it completes.

   Checks llm/interrupt-checkpoint! before fan-out and again after each page
   completes, so a cancelled run unwinds without waiting out the rest. The handful
   of pages already in flight when cancellation lands run to completion in the
   background, and — unlike before — their work survives, because each one writes
   its own row.

   Pre : every member of `page-states` needs extraction. Deciding that is
         `classify-page`'s job, not this function's.
   Post: a seq of extract-and-stage-page! results, order matching `page-states`."
  [api-key model-fields prompt ctx page-states]
  (llm/interrupt-checkpoint!)
  (let [limiter (Semaphore. extract-page-concurrency)
        futs (mapv (fn [state]
                     (future
                       (.acquire limiter)
                       (try
                         (extract-and-stage-page! api-key model-fields prompt ctx state)
                         (finally (.release limiter)))))
               page-states)]
    (mapv (fn [f] (llm/interrupt-checkpoint!) (deref f)) futs)))

(defn- link-decision-valid?
  "Check one entity-link decision against the offered candidate IDs.
   Pre:  valid-ids contains only IDs sent to the model.
   Post: true for \"new\" or an offered integer ID."
  [valid-ids decision]
  (or (= "new" decision)
    (and (integer? decision) (contains? valid-ids decision))))

(defn- link-entities!
  "Pass 2. Distinct labels → {lower-label entity-id}, creating rows for new.
   Labels without trigram candidates skip the LLM. A failed/invalid LLM
   decision degrades to new (a duplicate entity is reviewable later; a wrong
   merge is not). Post: every input label has an id; :cost is the LLM spend and
   :tokens its usage-tokens maps, one per completion."
  [api-key model-fields user-id labels]
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
        {:keys [decisions cost tokens]}
        (if (empty? candidates)
          {:decisions {} :cost 0.0 :tokens []}
          (try
            (let [{:keys [parsed cost tokens]} (llm/chat! api-key model-fields
                                                 (cards/load-prompt-template "kg-link.md")
                                                 (llm-json/write-prompt-input
                                                   {:candidates candidates})
                                                 :decisions :map
                                                 {:feature :kg-link})]
              {:decisions (into {}
                            (map (fn [[label decision]]
                                   [(if (keyword? label)
                                      (subs (str label) 1)
                                      (str label))
                                    decision]))
                            parsed)
               :cost cost
               :tokens tokens})
            (catch Exception e
              (tel/log! {:level :warn :id ::link-degraded
                         :data {:labels (count candidates) :error (ex-message e)}}
                "Entity-link call failed; treating all as new entities")
              {:decisions {} :cost 0.0 :tokens []})))
        valid-ids (into #{} (comp (mapcat val) (map :id)) candidates)
        id-of (fn [label]
                (let [d (get decisions label)]
                  (when (and (link-decision-valid? valid-ids d)
                          (integer? d))
                    d)))
        ;; Labels with no merge decision are new — batch-insert them all in
        ;; one round trip instead of one insert-kg-entity! per label.
        new-labels (into [] (comp (map val) (remove id-of)) by-lower)
        new-ids (zipmap new-labels (db/insert-kg-entities! user-id new-labels))]
    {:by-label (into {}
                 (map (fn [[lower label]]
                        [lower (or (id-of label) (get new-ids label))]))
                 by-lower)
     :cost cost
     :tokens tokens}))

;; ---------------------------------------------------------------------------
;; Orchestrator
;; ---------------------------------------------------------------------------

(defn classify-page
  "Whether a page needs extraction, is already accounted for, or has been given up
   on. Pure, so the skip rule is testable with no database and no LLM.

   Three outcomes, and the ORDER of the tests matters:

     :skip    the stored digest and version match, and the page is either linked
              (`fact_count` non-nil, which includes a barren page's 0) or still
              holds staged triples waiting for this invocation's link.
     :at-cap  same digest and version, nothing linked and nothing staged, and
              `attempt-cap` attempts already spent on this exact text.
     :extract everything else — never attempted, text changed, digest generation
              bumped, or retryable below the cap.

   `has_staged` is half of the :skip test and not an optimisation. Without it, an
   account import — which drops `kg_staged_triples` as in-flight state — would
   arrive with pages whose digest matches and whose facts do not exist, and no
   later run would ever revisit them.

   Pre : `state` carries :digest (of the page's CURRENT text) and the stored
         :content_digest, :digest_version, :fact_count, :attempt_count,
         :has_staged. The stored five are nil/false for a page never attempted.
   Post: one of :skip, :at-cap, :extract."
  [attempt-cap digest-version {:keys [digest content_digest digest_version
                                      fact_count attempt_count has_staged]}]
  (let [current? (and (some? content_digest)
                   (= content_digest digest)
                   (= digest_version digest-version))]
    (cond
      (not current?) :extract
      (or (some? fact_count) has_staged) :skip
      (>= (or attempt_count 0) attempt-cap) :at-cap
      :else :extract)))

(defn- document-page-states
  "Every extractable page of the document, each carrying its current digest.

   Falls back to the root topic's own content when the document has no page
   children, which is the wiki, web and markdown shape. The fallback's extraction
   row keys on the ROOT topic id, so nothing downstream needs a nil-page branch.

   Post: seq of `db/document-page-extraction-state` rows plus :digest, page order."
  [root-topic-id]
  (let [with-digest (fn [row]
                      (assoc row :digest (digest/digest-of (:content_text row))))
        pages (mapv with-digest (db/document-page-extraction-state root-topic-id))]
    (if (seq pages)
      pages
      (when-let [root (db/root-topic-extraction-state root-topic-id)]
        [(with-digest root)]))))

(defn distill-document!
  "Distill root-topic-id's pages into facts (status='approved'). Synchronous —
   run it on the executor via start-distill!.

   Pages already extracted from their current text are SKIPPED, and each page
   extracted here is persisted before the next one starts. Together those make a
   re-distill cost only the pages that changed, and make a cancelled or timed-out
   run resumable rather than a total loss. See plans/kg-page-skip-extraction.md.

   The two passes persist at different granularities on purpose. Extraction is per
   page, because it is the expensive one — one call per page, and the calls a
   cancelled run loses are the whole cost. Linking is one call per INVOCATION over
   every staged triple the document has accumulated, because linking per page would
   multiply calls by page count and match each page's labels against a nearly empty
   kg_entities.

   Pre:  root-topic-id belongs to user-id (callers pass ids from the user's
         own tree — violation = caller bug); OpenRouter key configured.
   Post: {:success true :pages n :skipped n :at-cap n :page-errors n :facts n
         :entities n} with facts inserted as 'approved' and this invocation's cost
         billed, or {:success false :error msg :error-type kw?}. `:pages` counts
         pages this invocation extracted, not pages the document holds. Per-page
         extraction failures skip the page, never abort the run."
  [user-id root-topic-id]
  (try
    ;; Extraction and entity-linking are independently configurable steps
    ;; (:extract / :link). model-fields drives extraction; link-model-fields drives
    ;; linking. entry (the extraction model) is what facts record as
    ;; :source_model and what the summed distill cost bills under.
    (let [{:keys [api-key entry model-fields]} (llm/resolve-model+gate! user-id :extract)
          link-model-fields (:model-fields (llm/resolve-model+gate! user-id :link))
          all-states (or (seq (document-page-states root-topic-id))
                         (throw (ex-info "No page text. Extract text first." {})))
          by-outcome (group-by #(classify-page attempt-cap digest/digest-version %)
                       all-states)
          to-extract (vec (:extract by-outcome))
          template (cards/load-prompt-template "kg-extract.md")
          prompt (extraction-prompt template (db/get-kg-approved-predicates user-id))
          ;; Recorded on every extraction row, never compared — see
          ;; kg-page-digest's docstring for why the skip gates on content alone.
          ctx {:user-id user-id
               :graph-topic-id root-topic-id
               :digest-version digest/digest-version
               :prompt-version (prompt-template-version template)
               :extract-model-id (:id entry)}
          extractions (extract-and-stage-pages! api-key model-fields prompt ctx to-extract)
          _ (llm/interrupt-checkpoint!)
          ;; Everything unlinked for this document, including whatever an earlier
          ;; cancelled invocation left staged. This is the set one linking call
          ;; covers, and the reason a resumed run needs no special case.
          staged (db/staged-triples-for-document user-id root-topic-id)
          triples (mapv (fn [row]
                          {:s (:subject_label row) :p (:predicate_label row)
                           :o (:object_label row) :lit (:object_literal row)
                           :page (:page_number row)})
                    staged)
          staged-extraction-ids (into #{} (map :page_extraction_id) staged)
          entity-labels (into [] (comp (mapcat (juxt :s :o)) (remove nil?)) triples)
          {:keys [by-label] link-cost :cost link-tokens :tokens}
          (if (seq entity-labels)
            (link-entities! api-key link-model-fields user-id entity-labels)
            {:by-label {} :cost 0.0 :tokens []})
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
          ;; ONLY after the insert. Marking a page linked before its facts exist
          ;; would make the selector skip a page whose facts never landed, and no
          ;; later run would revisit it — the one way this design can lose content
          ;; permanently.
          linked-pages (db/finalize-linked-pages! user-id staged-extraction-ids)
          ;; Near-duplicate sweep over what this run just added, scoped to
          ;; (pre-existing × new). Restricting it to the new ids is what makes the
          ;; automatic path safe: same-run pair behaviour is UNMEASURED, so a
          ;; first distill of a fresh document must mark nothing, while an
          ;; incremental re-distill still catches what it restates. The
          ;; user-invoked sweep in freememo.kg-dedup compares every pair.
          ;;
          ;; Total: the facts are already inserted and the provider is already
          ;; paid, so a sweep failure must not turn a successful distill into a
          ;; failed one. It is also NOT billed — no model call is involved.
          duplicates (try
                       (:marked (kg-dedup/sweep-document! user-id root-topic-id inserted))
                       (catch InterruptedException e (throw e))
                       (catch Exception e
                         (tel/log! {:level :warn :id ::dedup-sweep-failed
                                    :data {:user-id user-id :root root-topic-id
                                           :error (ex-message e)}}
                           "Duplicate sweep failed after distillation; facts kept as-is")
                         0))
          total-cost (reduce + link-cost (map :cost extractions))
          total-tokens (into (vec link-tokens) (mapcat :tokens) extractions)]
      (credits/record-cost-charge! user-id :kg.distill (:id entry) total-cost
        total-tokens)
      ;; :skipped and :at-cap are logged because they are what the page skip
      ;; bought and what it withheld. A capped page is reported here AND per
      ;; document in the Knowledge view, because a page silently given up on is
      ;; the failure this log exists to prevent.
      (tel/log! {:level :info :id ::distill-complete
                 :data {:user-id user-id :root root-topic-id
                        :pages (count to-extract)
                        :skipped (count (:skip by-outcome))
                        :at-cap (count (:at-cap by-outcome))
                        :linked-pages linked-pages
                        :page-errors (count (filter :failed extractions))
                        :triples (count triples) :facts (count inserted)
                        :duplicates duplicates
                        :cost-usd total-cost}}
        "Document distillation complete")
      {:success true
       :pages (count to-extract)
       :skipped (count (:skip by-outcome))
       :at-cap (count (:at-cap by-outcome))
       :page-errors (count (filter :failed extractions))
       :facts (count inserted)
       :duplicates duplicates
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

(defn distill-summary
  "One sentence for the completion toast.

   The skip makes \"0 facts added\" a normal and CONFUSING outcome: a re-run that
   read nothing because nothing changed is a success, and the old wording read as a
   failure with no reason given. So a run that extracted no pages leads with why.

   Capped pages are named here as well as in the log and the Pages column, because
   a toast is the one surface a user is certainly looking at when the run ends.

   Pre : `result` is a successful `distill-document!` return.
   Post: a non-blank string."
  [{:keys [facts pages skipped at-cap duplicates page-errors]}]
  (let [n (or facts 0)
        parts (cond-> []
                (pos? n) (conj (str n " fact" (when (not= 1 n) "s") " added"))
                (and (zero? n) (zero? (or pages 0)) (pos? (or skipped 0)))
                (conj (str "Nothing to do — all " skipped " page"
                        (when (not= 1 skipped) "s") " already extracted"))
                (and (zero? n) (pos? (or pages 0)))
                (conj (str "No new facts from " pages " page"
                        (when (not= 1 pages) "s")))
                (pos? (or duplicates 0)) (conj (str duplicates " marked duplicate"))
                (pos? (or page-errors 0)) (conj (str page-errors " page"
                                                  (when (not= 1 page-errors) "s")
                                                  " failed"))
                (pos? (or at-cap 0)) (conj (str at-cap " page"
                                             (when (not= 1 at-cap) "s")
                                             " given up on after "
                                             attempt-cap " tries")))]
    (if (seq parts)
      (str (str/join ", " parts)
        (when (pos? n) " — browse Knowledge to curate"))
      "Nothing to extract")))

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
                    :message (distill-summary result)}))
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
      (swap! (us/get-atom uid :distill-cancellers) assoc root-topic-id cancel-fn)
      ;; After the canceller: discovery reads cancellability when it opens the row.
      (us/work-started! uid))
    nil))

(defn cancel-distill!
  "Cancel an in-flight distill run for (uid, root-topic-id), if any."
  [uid root-topic-id]
  (when-let [cancel-fn (get @(us/get-atom uid :distill-cancellers) root-topic-id)]
    (cancel-fn)
    nil))
