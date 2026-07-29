(ns freememo.kg-llm
  "Shared LLM plumbing for the knowledge-graph pipelines (extract, questions,
   grading): credit gate + model resolution, one-shot EDN-prompted completion,
   cancellation checkpoint, error taxonomy. Extracted when the third pipeline
   (grading) would have made the third copy."
  (:require
   [freememo.settings :as settings]
   [freememo.credits :as credits]
   [freememo.openrouter :as openrouter]
   [freememo.card-models :as card-models]
   [freememo.llm-edn :as llm-edn]
   [taoensso.telemere :as tel]))

(defn root-cause [e]
  (if-let [c (.getCause ^Throwable e)] (recur c) e))

(defn insufficient-credits? [e]
  (= ::insufficient-credits (:type (ex-data (root-cause e)))))

(defn interrupt-checkpoint!
  "m/via cancellation only lands when the body exits, and blocking HTTP never
   notices the interrupt — so multi-call loops must check. Throws between LLM
   calls when the run was cancelled; m/via converts it to Cancelled."
  []
  (when (Thread/interrupted)
    (throw (InterruptedException. "run cancelled"))))

(defn resolve-model+gate!
  "Shared pipeline preamble: OpenRouter key present, credit gate passed, model
   resolved from the user's per-step KG model setting (settings/get-kg-model) —
   each KG lane carries its own knob.
   Pre:  step ∈ the KG step registry (settings/kg-model-steps).
   Post: {:api-key :entry :model-slug}; throws on any failure —
   ::insufficient-credits typed for the gate."
  [user-id step]
  (let [api-key (settings/get-openrouter-api-key user-id)]
    (when (empty? api-key) (throw (ex-info "OpenRouter API key not configured" {})))
    (let [gate (credits/check-cost-billed-balance! user-id)]
      (when-not (:ok gate)
        (throw (ex-info (:error gate) {:type ::insufficient-credits}))))
    (let [model-id (settings/get-kg-model user-id step)
          entry (or (card-models/resolve-model model-id)
                    (throw (ex-info (str "Unknown model: " model-id) {})))]
      {:api-key api-key :entry entry :model-slug (:openrouter-model entry)})))

(def ^:private max-attempts
  "Completions per chat! call: one retry when a response yields no usable
   content. 2 preserves the bound the KG lanes had while extraction and grading
   each hand-rolled their own single retry."
  2)

(defn chat!
  "One completion; returns {:parsed value :cost usd}.

   Retries once when the response yields no usable content — empty, truncated,
   no choices, or unparseable (openrouter/retryable-cause? is the single owner of
   that set). A model refusal is terminal and throws on the first attempt. Retry
   lives here, not at the call sites: all six shared this need and only two ever
   implemented it, keyed on a `:raw` value that was nil for the very failure it
   was meant to catch.
   Optional `ctx` {:feature kw :user-id id} tags the transport and no-content
   observability signals (see freememo.openrouter/post!).
   Pre:  api-key configured; slug a registry :openrouter-model.
   Post: {:parsed collection :cost usd} where :cost is summed across attempts —
         callers bill that sum. Throws the final attempt's exception on failure
         (ex-data carries :type); a failed call bills nothing, and the spend it
         absorbed is logged here as ::unbilled-spend. Callers cannot report that
         cost — the exception carries no accumulator, and kg-questions' skipped
         batch truthfully reports :cost 0.0 for what it BILLED — so the platform
         only sees absorbed spend if this frame records it."
  ([api-key slug system-prompt user-content]
   (chat! api-key slug system-prompt user-content nil))
  ([api-key slug system-prompt user-content ctx]
   (loop [attempt 1
          cost-acc 0.0]
     (let [body (openrouter/chat-completion! api-key
                  {:model slug
                   :messages [{:role "system" :content system-prompt}
                              {:role "user" :content user-content}]}
                  ctx)
           cost-acc' (+ cost-acc (double (or (-> body :usage :cost) 0)))
           result (try {:parsed (llm-edn/parse-response
                                  (openrouter/message-content body ctx))}
                    (catch clojure.lang.ExceptionInfo e
                      (if (and (openrouter/retryable-cause? e) (< attempt max-attempts))
                        ::retry
                        (do (tel/log! {:level :warn :id ::unbilled-spend
                                       :data {:cost-usd cost-acc' :attempts attempt
                                              :cause (:type (ex-data e))
                                              :feature (:feature ctx)
                                              :user-id (:user-id ctx)}}
                              "KG completion failed; provider spend absorbed, not billed")
                            (throw e)))))]
       (if (= ::retry result)
         ;; A cancelled run must not spend the retry — blocking HTTP never
         ;; notices the interrupt, so this loop checks like every other.
         (do (interrupt-checkpoint!)
             (recur (inc attempt) cost-acc'))
         (assoc result :cost cost-acc'))))))
