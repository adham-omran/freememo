(ns freememo.credits
  "Credit pricing + spend orchestration.

   Pass-through cost × markup (see plans/credits-wayl-payment-system.md §5.2/§5.4).
   `charge-iqd` is pure; `check-balance!`/`record-charge!` gate and debit a
   completed AI action; `start-checkout!` creates a Wayl top-up link.

   OpenAI token convention: prompt_tokens includes cached_tokens, so billable
   input = prompt − cached. reasoning_tokens are counted within completion_tokens
   and billed at the output rate — tracked for the ledger, not priced separately."
  (:require
   [freememo.config :as config]
   [freememo.db :as db]
   [freememo.wayl :as wayl]
   [taoensso.telemere :as tel]))

;; ── Pure cost computation (§5.2) ──

(defn- require-rates!
  "Resolve [rates fx markup] for a model or throw, failing closed (§5.2.4)."
  [model]
  (let [rates (config/model-rates model)
        fx (config/fx-iqd-per-usd)
        markup (config/markup)]
    (when (nil? rates)
      (throw (ex-info (str "No credit rate configured for model: " model)
               {:type ::unpriced-model :model model})))
    (when (or (nil? fx) (nil? markup))
      (throw (ex-info "Credits misconfigured: missing fx-iqd-per-usd or markup"
               {:type ::credits-misconfigured :fx fx :markup markup})))
    [rates fx markup]))

(defn- attempt-cost-usd
  "USD cost of one OpenAI call. billable input = prompt − cached (clamped ≥ 0)."
  [{:keys [input cached-input output]}
   {:keys [prompt-tokens cached-tokens completion-tokens]}]
  (let [prompt     (or prompt-tokens 0)
        cached     (or cached-tokens 0)
        completion (or completion-tokens 0)
        billable   (max 0 (- prompt cached))]
    (/ (+ (* billable input)
          (* cached cached-input)
          (* completion output))
       1e6)))

(defn charge-iqd
  "IQD to debit for one user action = ceil(Σ attempts USD × fx × markup).
   Pre:  model priced; attempts non-empty seq of {:prompt-tokens :cached-tokens
         :completion-tokens} (non-negative).
   Post: non-negative long; a cached-heavy call costs strictly less than the
         same totals with no cache.
   Throws (fails closed) when model/fx/markup are unconfigured."
  [model attempts]
  (let [[rates fx markup] (require-rates! model)
        usd (reduce + 0.0 (map #(attempt-cost-usd rates %) attempts))]
    (long (Math/ceil (* usd fx markup)))))

;; ── Usage extraction ──

(defn usage->tokens
  "Normalize an OpenAI :usage map to the token shape charge-iqd/ledger expect."
  [usage]
  {:prompt-tokens     (or (:prompt_tokens usage) 0)
   :cached-tokens     (or (get-in usage [:prompt_tokens_details :cached_tokens]) 0)
   :completion-tokens (or (:completion_tokens usage) 0)
   :reasoning-tokens  (or (get-in usage [:completion_tokens_details :reasoning_tokens]) 0)})

;; ── Spend path (§5.4) ──

(defn check-balance!
  "Gate before an AI action.
   Pre:  user-id, model.
   Post: {:ok true} to proceed, or {:ok false :error msg} to refuse.
   Self-host (credits disabled) always proceeds. Official requires fully-priced
   config + balance > 0 (one overshoot allowed by debiting after success)."
  [user-id model]
  (if-not (config/credits-enabled?)
    {:ok true}
    (cond
      (or (nil? (config/model-rates model))
          (nil? (config/fx-iqd-per-usd))
          (nil? (config/markup)))
      {:ok false :error (str "Credit pricing is not configured for \"" model "\".")}

      (<= (db/get-credit-balance user-id) 0)
      {:ok false :error "Out of credits. Top up in Settings to keep using AI features."}

      :else {:ok true})))

(defn record-charge!
  "Debit credits for a completed action. No-op when credits are disabled.
   `attempts` is a non-empty seq of usage-token maps (one per LLM attempt;
   all attempts are billed per §5.4.5). Returns charged IQD (long) or nil."
  [user-id endpoint model attempts]
  (when (config/credits-enabled?)
    (let [cost (charge-iqd model attempts)
          new-bal (db/debit-credits! user-id cost
                    {:endpoint (name endpoint) :model model :attempts attempts})]
      (tel/log! {:level :info :id ::credit-debit
                 :data {:user-id user-id :endpoint endpoint :model model
                        :cost-iqd cost :balance-after new-bal :attempts (count attempts)}}
        "Credit debit")
      cost)))

;; ── Purchase (§5.5) ──

(defn start-checkout!
  "Create a pending order + Wayl top-up link for a preset amount.
   Pre:  credits enabled; amount-iqd is a configured preset; base-url is the
         app's public HTTPS origin.
   Post: a pending credit_orders row exists; returns {:ok true :url u} or
         {:ok false :error msg} (order marked failed on provider error)."
  [user-id amount-iqd base-url]
  (cond
    (not (config/credits-enabled?))
    {:ok false :error "Credits are not enabled"}

    (not (contains? (set (config/presets)) amount-iqd))
    {:ok false :error "Invalid top-up amount"}

    :else
    (let [reference-id (str "fm-" (random-uuid))]
      (db/insert-credit-order! user-id reference-id amount-iqd)
      (let [r (wayl/create-link! {:reference-id reference-id
                                  :amount-iqd amount-iqd
                                  :webhook-url (str base-url "/api/wayl/webhook")
                                  :redirection-url (str base-url "/credits/return")})]
        (if (:ok r)
          (do (db/set-order-wayl-fields! reference-id (:code r) (:link-id r))
              {:ok true :url (:url r)})
          (do (db/fail-credit-order! reference-id)
              {:ok false :error (:error r)}))))))

;; ── Cost-estimate table (§5.8.3) ──

(def ^:private estimate-basis
  "Mean observed tokens per action type (prod logs, 2026-03..05). Used to render
   the static cost-estimate table. {:label endpoint :prompt :cached :completion}."
  [{:label "OCR a page"            :model-key :ocr   :prompt-tokens 907   :cached-tokens 0    :completion-tokens 526}
   {:label "Generate basic cards"  :model-key :basic :prompt-tokens 9345  :cached-tokens 577  :completion-tokens 68}
   {:label "Generate cloze cards"  :model-key :cloze :prompt-tokens 15266 :cached-tokens 4887 :completion-tokens 94}])

(defn cost-estimates
  "Static per-action IQD estimates for the given model, or nil when pricing is
   unconfigured. Returns a vec of {:label :iqd}."
  [model]
  (when (and (config/model-rates model) (config/fx-iqd-per-usd) (config/markup))
    (mapv (fn [{:keys [label] :as basis}]
            {:label label :iqd (charge-iqd model [basis])})
      estimate-basis)))
