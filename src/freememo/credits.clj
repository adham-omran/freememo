(ns freememo.credits
  "Credit pricing + spend orchestration.

   Pass-through cost × markup (see plans/credits-wayl-payment-system.md §5.2/§5.4).
   All AI lanes bill from OpenRouter's returned usage.cost: `check-cost-billed-balance!`
   gates and `record-cost-charge!` debits a completed action; `start-checkout!`
   creates a Wayl top-up link."
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

(defn charge-iqd-from-usd
  "IQD to debit for an action billed by a provider-reported USD cost (the OCR
   lane — OpenRouter returns usage.cost). = ceil(usd × fx × markup).
   Pre:  usd >= 0 (nil → 0); fx + markup configured for user.
   Post: non-negative long. Throws (fails closed) when fx/markup unconfigured."
  [user-id usd]
  (let [fx (config/fx-iqd-per-usd)
        markup (resolve-markup user-id)]
    (when (or (nil? fx) (nil? markup))
      (throw (ex-info "Credits misconfigured: missing fx-iqd-per-usd or markup"
               {:type ::credits-misconfigured :fx fx :markup markup})))
    (long (Math/ceil (* (max 0.0 (double (or usd 0))) fx markup)))))

;; ── Spend path (§5.4) ──

(defn check-cost-billed-balance!
  "Gate before an AI action billed from a provider-reported USD cost (OCR + card
   generation). Requires only fx + markup configured and a positive balance — the
   cost comes from OpenRouter's usage.cost, with no per-model rate table.
   Pre:  user-id. Post: {:ok true} to proceed, or {:ok false :error msg}.
   Self-host (credits disabled) always proceeds."
  [user-id]
  (if-not (config/credits-enabled?)
    {:ok true}
    (cond
      (or (nil? (config/fx-iqd-per-usd)) (nil? (resolve-markup user-id)))
      {:ok false :error "Credit pricing is not configured."}

      (<= (db/get-credit-balance user-id) 0)
      {:ok false :error "Out of credits. Top up in Settings to keep using AI features."}

      :else {:ok true})))

(def ^:private debit-retry-sleeps-ms
  "Backoff before debit retries 2 and 3 — absorbs transient DB failures
   (user-row lock contention, pool blips) without holding the offload/worker
   thread for more than ~600ms total."
  [100 500])

(defn- debit-with-retry!
  "Debit a precomputed `cost` IQD — total (never throws). Retries the debit up to
   3 attempts with short backoff, logging :warn ::credit-debit-retry per failure.
   `ledger` is the db/debit-credits! annotation {:endpoint :model :attempts}.
   Returns `cost` on success, or nil after exhaustion. Exhaustion logs
   ::credit-charge-failed with `fail-data` (the inputs needed to reconcile the
   charge by hand) and triggers the alert email when SMTP is configured.
   Pre: cost >= 0. Post: balance debited by cost, or nil + alert on total failure."
  [user-id endpoint model cost ledger fail-data]
  (try
    (let [max-attempts (inc (count debit-retry-sleeps-ms))]
      (loop [attempt 1]
        (let [result (try {:balance (db/debit-credits! user-id cost ledger)}
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
                                :cost-iqd cost :balance-after new-bal :debit-attempt attempt}}
                "Credit debit")
              cost)))))
    (catch Exception e
      (tel/error! {:id ::credit-charge-failed :data fail-data} e)
      nil)))

(defn record-cost-charge!
  "Debit credits for a completed OCR action billed from a provider-reported USD
   cost (OpenRouter usage.cost) — total (never throws). No-op when credits are
   disabled. Cost is ceil(usd × fx × markup) via charge-iqd-from-usd, then
   debited with retry. Returns charged IQD (long), or nil when disabled OR all
   debit attempts failed. `model` is the registry id (for the ledger row)."
  [user-id endpoint model usd]
  (when (config/credits-enabled?)
    (debit-with-retry! user-id endpoint model
      (charge-iqd-from-usd user-id usd)
      {:endpoint (name endpoint) :model model :attempts nil}
      {:user-id user-id :endpoint endpoint :model model :usd usd})))

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

