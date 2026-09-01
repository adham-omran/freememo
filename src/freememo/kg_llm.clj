(ns freememo.kg-llm
  "Shared LLM plumbing for the knowledge-graph pipelines (extract, questions,
   grading): credit gate + model resolution, strict JSON completion,
   cancellation checkpoint, error taxonomy. Extracted when the third pipeline
   (grading) would have made the third copy."
  (:require
   [freememo.settings :as settings]
   [freememo.credits :as credits]
   [freememo.openrouter :as openrouter]
   [freememo.card-models :as card-models]
   [freememo.llm-json :as llm-json]
   [taoensso.telemere :as tel]))

(defn root-cause [e]
  (if-let [c (.getCause ^Throwable e)] (recur c) e))

(defn- lane-visible-failure
  "Remove the private parser cause before a failure leaves this boundary.
   Pre:  failure is an ExceptionInfo from response extraction or JSON parsing.
   Post: JSON failures keep their application message and typed metadata.
         Their Jackson cause does not reach lane logs, storage, or user output.
         Other failures pass through unchanged."
  [failure]
  (if (= ::llm-json/unparseable (:type (ex-data failure)))
    (ex-info (ex-message failure) (ex-data failure))
    failure))

(defn- completion-failure-log-data
  "Build safe metadata for a terminal completion failure.
   Pre:  model-fields is the request projection. Failure is an ExceptionInfo.
   Post: identifies the lane, model, provider, attempt, and failure category.
         Contains no prompt, user content, or raw model response."
  [model-fields body observability cost-usd attempt failure]
  {:cost-usd cost-usd
   :attempt attempt
   :cause (:type (ex-data failure))
   :feature (:feature observability)
   :user-id (:user-id observability)
   :model (:model model-fields)
   :provider (or (:provider body)
               (-> model-fields :provider :only first))})

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
   Post: {:api-key :entry :model-fields}; :model-fields requires JSON output
   from a compatible provider route and preserves any provider pin. Throws on any failure —
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
      {:api-key api-key
       :entry entry
       :model-fields (openrouter/structured-chat-completion-fields entry {})})))

(def ^:private max-attempts
  "Completions per chat! call: one retry when a response yields no usable
   content. 2 preserves the bound the KG lanes had while extraction and grading
   each hand-rolled their own single retry."
  2)

(defn chat!
  "One structured completion operation. Returns {:parsed value :cost usd}.

   Retries once when the response yields no usable content — empty, truncated,
   no choices, or unparseable (openrouter/retryable-cause? is the single owner of
   that set). A model refusal is terminal and throws on the first attempt. Retry
   lives here, not at the call sites: all six shared this need and only two ever
   implemented it, keyed on a `:raw` value that was nil for the very failure it
   was meant to catch.
   `root-key` and `root-type` declare the JSON response field to return.
   Optional `observability` {:feature kw :user-id id} tags the transport and no-content
   observability signals (see freememo.openrouter/post!).
   Pre:  api-key configured; model-fields is the :model-fields of
         resolve-model+gate!; root-key is a keyword; root-type is :map or :vector.
   Post: {:parsed collection :cost usd} where :cost is summed across attempts —
         callers bill that sum. Throws the final attempt's exception on failure
         (ex-data carries :type); a failed call bills nothing, and the spend it
         absorbed is logged here as ::unbilled-spend. Callers cannot report that
         cost — the exception carries no accumulator, and kg-questions' skipped
         batch truthfully reports :cost 0.0 for what it BILLED — so the platform
         only sees absorbed spend if this frame records it."
  ([api-key model-fields system-prompt user-content root-key root-type]
   (chat! api-key model-fields system-prompt user-content root-key root-type nil))
  ([api-key model-fields system-prompt user-content root-key root-type observability]
   (loop [attempt 1
          cost-acc 0.0]
     (let [body (openrouter/chat-completion! api-key
                  (assoc model-fields
                    :messages [{:role "system" :content system-prompt}
                               {:role "user" :content user-content}])
                  observability)
           cost-acc' (+ cost-acc (double (or (-> body :usage :cost) 0)))
           result (try {:parsed (llm-json/parse-response-field
                                  (openrouter/message-content body observability)
                                  root-key root-type)}
                    (catch clojure.lang.ExceptionInfo e
                      (if (and (openrouter/retryable-cause? e) (< attempt max-attempts))
                        ::retry
                        (do (tel/log! {:level :warn :id ::unbilled-spend
                                       :data (completion-failure-log-data
                                               model-fields body observability
                                               cost-acc' attempt e)}
                              "KG completion failed; provider spend absorbed, not billed")
                            (throw (lane-visible-failure e))))))]
       (if (= ::retry result)
         ;; A cancelled run must not spend the retry — blocking HTTP never
         ;; notices the interrupt, so this loop checks like every other.
         (do (interrupt-checkpoint!)
             (recur (inc attempt) cost-acc'))
         (assoc result :cost cost-acc'))))))
