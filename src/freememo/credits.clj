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
   [taoensso.telemere :as tel]
   [clojure.string :as str]))

(defn request-base-url
  "Public origin for an incoming Ring request — `https://freememo.net` in prod,
   `http://localhost:8080` in dev. Used for Wayl webhook + redirection URLs so
   neither path needs a config knob. Honors X-Forwarded-Proto + Host when a
   reverse proxy is in front; falls back to :scheme + :server-* otherwise."
  [request]
  (let [scheme (or (some-> (get-in request [:headers "x-forwarded-proto"]) str/trim not-empty)
                   (name (or (:scheme request) :http)))
        host (or (get-in request [:headers "host"])
                 (str (:server-name request)
                   (when-let [port (:server-port request)]
                     (str ":" port))))]
    (str scheme "://" host)))

;; ── Pure cost computation (§5.2) ──

(defn resolve-markup
  "Effective markup multiplier for a user: their DB override, else the config
   default (nil when neither is set). No floor — an admin-set override is trusted
   as-is, including promotional below-cost values."
  [user-id]
  (or (db/get-user-markup user-id) (config/markup)))

(defn- require-rates!
  "Resolve [rates fx markup] for a user+model or throw, failing closed (§5.2.4).
   markup is the user's effective rate (override or config default)."
  [user-id model]
  (let [rates (config/model-rates model)
        fx (config/fx-iqd-per-usd)
        markup (resolve-markup user-id)]
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
         :completion-tokens} (non-negative); user-id for per-user markup.
   Post: non-negative long; a cached-heavy call costs strictly less than the
         same totals with no cache.
   Throws (fails closed) when model/fx/markup are unconfigured."
  [user-id model attempts]
  (let [[rates fx markup] (require-rates! user-id model)
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
          (nil? (resolve-markup user-id)))
      {:ok false :error (str "Credit pricing is not configured for \"" model "\".")}

      (<= (db/get-credit-balance user-id) 0)
      {:ok false :error "Out of credits. Top up in Settings to keep using AI features."}

      :else {:ok true})))

(def ^:private debit-retry-sleeps-ms
  "Backoff before debit retries 2 and 3 — absorbs transient DB failures
   (user-row lock contention, pool blips) without holding the offload/worker
   thread for more than ~600ms total."
  [100 500])

(defn record-charge!
  "Debit credits for a completed action — total (never throws). No-op when credits
   are disabled. `attempts` is a non-empty seq of usage-token maps (one per LLM
   attempt; all attempts are billed per §5.4.5).
   Cost is computed once; the debit itself is retried up to 3 attempts with
   short backoff, logging :warn ::credit-debit-retry per failed attempt.
   Returns charged IQD (long) on success, or nil when credits are disabled OR all
   debit attempts failed. Exhaustion logs ::credit-charge-failed with the inputs
   needed to recompute the charge via charge-iqd (user-id, endpoint, model,
   attempts), so a silently-missed charge can be reconciled by hand — billing must
   never discard the user's completed AI result. ::credit-charge-failed also
   triggers the alert email when SMTP is configured (see freememo.logging/init!)."
  [user-id endpoint model attempts]
  (when (config/credits-enabled?)
    (try
      (let [cost (charge-iqd user-id model attempts)
            max-attempts (inc (count debit-retry-sleeps-ms))]
        (loop [attempt 1]
          (let [result (try
                         {:balance (db/debit-credits! user-id cost
                                     {:endpoint (name endpoint) :model model :attempts attempts})}
                         (catch Exception e {:error e}))]
            (if-let [e (:error result)]
              (if (< attempt max-attempts)
                (do (tel/log! {:level :warn :id ::credit-debit-retry
                               :data {:user-id user-id :endpoint endpoint :model model
                                      :attempt attempt :max-attempts max-attempts
                                      :error (ex-message e)}}
                      "Credit debit failed, retrying")
                    (Thread/sleep (long (nth debit-retry-sleeps-ms (dec attempt))))
                    (recur (inc attempt)))
                (throw e)) ; outer catch logs ::credit-charge-failed
              (let [new-bal (:balance result)]
                (tel/log! {:level :info :id ::credit-debit
                           :data {:user-id user-id :endpoint endpoint :model model
                                  :cost-iqd cost :balance-after new-bal :attempts (count attempts)
                                  :debit-attempt attempt}}
                  "Credit debit")
                cost)))))
      (catch Exception e
        (tel/error! {:id ::credit-charge-failed
                     :data {:user-id user-id :endpoint endpoint :model model :attempts attempts}}
          e)
        nil))))

;; ── Purchase (§5.5) ──

(defn start-checkout!
  "Create a pending order + Wayl top-up link for a preset amount.
   Pre:  credits enabled; amount-iqd is a configured preset; base-url is the
         app's public HTTPS origin; country-code is ISO-3166 alpha-2 or nil.
   Post: a pending credit_orders row exists; returns {:ok true :url u} or
         {:ok false :error msg} (order marked failed on provider error).
         Non-IQ (incl. nil) country → URL has `currency=usd` appended."
  [user-id amount-iqd base-url country-code]
  (cond
    (not (config/credits-enabled?))
    {:ok false :error "Credits are not enabled"}

    (not (contains? (set (config/presets)) amount-iqd))
    {:ok false :error "Invalid top-up amount"}

    :else
    (let [reference-id (str "fm-" (random-uuid))
          usd? (not= "IQ" country-code)]
      (db/insert-credit-order! user-id reference-id amount-iqd)
      (let [r (wayl/create-link! {:reference-id reference-id
                                  :amount-iqd amount-iqd
                                  :webhook-url (str base-url "/api/wayl/webhook")
                                  :redirection-url (str base-url "/credits/return")
                                  :currency-suffix? usd?})]
        (if (:ok r)
          (do (db/set-order-wayl-fields! reference-id (:code r) (:link-id r))
              {:ok true :url (:url r)})
          (do (db/fail-credit-order! reference-id)
              {:ok false :error (:error r)}))))))

;; ── Reconciliation (§5.10.2) — webhook miss / dev fallback ──

(defn reconcile-order!
  "Look up an order's state at Wayl and credit idempotently if Wayl reports it
   paid and our order is still pending. Lets the return page recover when the
   webhook can't reach us (dev/localhost) or was dropped in prod.
   Returns the same shape as `db/complete-credit-order!` plus {:ok bool}."
  [reference-id]
  (let [w (wayl/get-link-status reference-id)]
    (if-not (:ok w)
      {:ok false :error (:error w)}
      (let [s (:status w)]
        (cond
          (#{"Complete" "Delivered"} s)
          (let [r (db/complete-credit-order! reference-id)]
            (assoc r :ok true :wayl-status s))

          (#{"Cancelled" "Rejected" "Returned"} s)
          (do (db/fail-credit-order! reference-id)
              {:ok true :credited false :wayl-status s})

          :else
          {:ok true :credited false :wayl-status s})))))

;; ── USD display formatting ──

(defn iqd->usd-str
  "Format an IQD amount as a USD approximation string for in-app display only.
   Pre: iqd is a non-negative number or nil.
   Post: nil when iqd is nil or fx-iqd-per-usd is nil/0;
         '~$0.00' when iqd is 0;
         '<$0.01' when 0 < usd < 0.01;
         '~$X.YY' otherwise.
   The on-page disclaimer covers the 'approximate' framing."
  [iqd]
  (when iqd
    (when-let [fx (config/fx-iqd-per-usd)]
      (when (pos? fx)
        (let [usd (/ iqd (double fx))]
          (cond
            (zero? iqd) "~$0.00"
            (< usd 0.01) "<$0.01"
            :else (format "~$%.2f" usd)))))))

;; ── Cost-estimate table (§5.8.3) ──

(def ^:private estimate-basis
  "Mean observed tokens per action type (prod logs, 2026-03..05). Used to render
   the static cost table. Each row adds :unit (plural noun) + :units-per-action
   (billable units one action yields — 1 page for OCR, card-count notes, default
   2, for card generation) so the view can show per-unit cost and balance counts."
  [{:label "OCR a page" :unit "pages"       :units-per-action 1 :model-key :ocr   :prompt-tokens 907   :cached-tokens 0    :completion-tokens 526}
   {:label "Basic note" :unit "basic notes" :units-per-action 2 :model-key :basic :prompt-tokens 9345  :cached-tokens 577  :completion-tokens 68}
   {:label "Cloze note" :unit "cloze notes" :units-per-action 2 :model-key :cloze :prompt-tokens 15266 :cached-tokens 4887 :completion-tokens 94}])

(defn cost-estimates
  "Static cost estimates for the given model, or nil when pricing is unconfigured.
   Returns a vec of {:label :unit :units-per-action :iqd :unit-cost}: :iqd is the
   per-action charge; :unit-cost = round(:iqd / :units-per-action) for display; the
   view derives balance counts as (quot balance :iqd) × :units-per-action.
   Reflects the user's effective markup (override or config default)."
  [user-id model]
  (when (and (config/model-rates model) (config/fx-iqd-per-usd) (resolve-markup user-id))
    (mapv (fn [{:keys [label unit units-per-action] :as basis}]
            (let [iqd (charge-iqd user-id model [basis])]
              {:label label :unit unit :units-per-action units-per-action
               :iqd iqd :unit-cost (Math/round (/ (double iqd) units-per-action))}))
      estimate-basis)))
