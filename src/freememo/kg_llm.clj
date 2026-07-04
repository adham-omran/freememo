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
   [freememo.llm-edn :as llm-edn]))

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
  "Shared pipeline preamble: OpenRouter key present, credit gate passed,
   model resolved from the user's card-model setting (one knob steers every
   KG lane). Post: {:api-key :entry :model-slug}; throws on any failure —
   ::insufficient-credits typed for the gate."
  [user-id]
  (let [api-key (settings/get-openrouter-api-key user-id)]
    (when (empty? api-key) (throw (ex-info "OpenRouter API key not configured" {})))
    (let [gate (credits/check-cost-billed-balance! user-id)]
      (when-not (:ok gate)
        (throw (ex-info (:error gate) {:type ::insufficient-credits}))))
    (let [model-id (settings/get-model user-id)
          entry (or (card-models/resolve-model model-id)
                    (throw (ex-info (str "Unknown model: " model-id) {})))]
      {:api-key api-key :entry entry :model-slug (:openrouter-model entry)})))

(defn chat!
  "One completion; returns {:parsed value :cost usd}. Throws on API error or
   unparseable output (parse failures carry :raw in ex-data — callers decide
   retry policy)."
  [api-key slug system-prompt user-content]
  (let [body (openrouter/chat-completion! api-key
               {:model slug
                :messages [{:role "system" :content system-prompt}
                           {:role "user" :content user-content}]})]
    {:parsed (llm-edn/parse-response (-> body :choices first :message :content))
     :cost (double (or (-> body :usage :cost) 0))}))
