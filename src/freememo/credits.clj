(ns freememo.credits
  "Credit pricing + spend orchestration.

   Pass-through cost × markup (see plans/credits-wayl-payment-system.md §5.2/§5.4).
   All AI lanes bill from OpenRouter's returned usage.cost: `check-cost-billed-balance!`
   gates and `record-cost-charge!` debits a completed action; `start-checkout!`
   creates a Wayl top-up link."
  (:require
   [clojure.string :as str]
   [freememo.commands :as commands]
   [freememo.config :as config]
   [freememo.db :as db]
   [freememo.storage-meter :as storage-meter]
   [freememo.wayl :as wayl]
   [taoensso.telemere :as tel]))

(defn request-base-url
  "Public origin for Wayl webhook + redirection URLs — `https://freememo.net`
   in prod, `http://localhost:8080` in dev.
   APP_BASE_URL (or :deployment :app-base-url) wins when set, with any trailing
   slash removed. Production sets it because the request cannot tell the
   scheme: the Cloudflare tunnel ends TLS, and Caddy then sends
   X-Forwarded-Proto `http`. From 2026-08-26 to 2026-10-30 every Wayl link
   carried http:// URLs for that reason.
   With no configured value, the origin comes from X-Forwarded-Proto + Host,
   else from :scheme + :server-*."
  [request]
  (if-let [configured (some-> (config/env-or-config "APP_BASE_URL" [:deployment :app-base-url])
                              str str/trim not-empty (str/replace #"/+$" ""))]
    configured
    (let [scheme (or (some-> (get-in request [:headers "x-forwarded-proto"]) str/trim not-empty)
                     (name (or (:scheme request) :http)))
          host (or (get-in request [:headers "host"])
                   (str (:server-name request)
                     (when-let [port (:server-port request)]
                       (str ":" port))))]
      (str scheme "://" host))))

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
   `ledger` is the db/debit-credits! annotation {:endpoint :model :attempts},
   built by `ledger-annotation`.
   Returns `cost` on success, or nil after exhaustion. Exhaustion logs
   ::credit-charge-failed with `fail-data` (the inputs needed to reconcile the
   charge by hand) and triggers the alert email when SMTP is configured.
   Pre: cost >= 0. The caller owns invalid cost input.
   Post: a successful debit increments :credits-refresh once and returns cost.
   A failed debit returns nil, logs an alert, and does not publish a refresh.
   Invariant: the refresh follows the committed ledger write. This function owns
   a missing or early refresh."
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
              (commands/bump! user-id :debit-credits)
              (tel/log! {:level :info :id ::credit-debit
                         :data {:user-id user-id :endpoint endpoint :model model
                                :cost-iqd cost :balance-after new-bal :debit-attempt attempt}}
                "Credit debit")
              cost)))))
    (catch Exception e
      (tel/error! {:id ::credit-charge-failed :data fail-data} e)
      nil)))

(defn ledger-annotation
  "The db/debit-credits! annotation for one AI charge.
   Pre:  endpoint is a keyword; `tokens` is a seq of token maps keyed as
         openrouter/token-keys, one per billed completion, or nil when the
         caller knows none.
   Post: {:endpoint str :model model :attempts v}. :attempts is nil when
         `tokens` is empty, so the row stores NULL token counts rather than 0."
  [endpoint model tokens]
  {:endpoint (name endpoint) :model model :attempts (not-empty (vec tokens))})

(defn record-cost-charge!
  "Debit credits for a completed AI action billed from a provider-reported USD
   cost (OpenRouter usage.cost) — total (never throws). No-op when credits are
   disabled. Cost is ceil(usd × fx × markup) via charge-iqd-from-usd, then
   debited with retry. Returns charged IQD (long), or nil when disabled OR all
   debit attempts failed. `model` is the registry id (for the ledger row).
   `tokens` is one openrouter/usage-tokens map per billed completion; the
   ledger row stores their sums and count. It is required so that no caller
   drops the counts by omission — pass nil only when none are known."
  [user-id endpoint model usd tokens]
  (when (config/credits-enabled?)
    (debit-with-retry! user-id endpoint model
      (charge-iqd-from-usd user-id usd)
      (ledger-annotation endpoint model tokens)
      {:user-id user-id :endpoint endpoint :model model :usd usd})))

;; ── Purchase (§5.5) ──

(declare top-up-packs iqd-display?)

(defn start-checkout!
  "Create a pending order + Wayl top-up link for a preset amount.
   Pre:  credits enabled; amount is the :amount of one of
         (top-up-packs country-code), so IQD for Iraq and whole USD for
         every other country; base-url is the app's public HTTPS origin;
         country-code is ISO-3166 alpha-2 or nil.
   Post: a pending credit_orders row exists for the pack's IQD; returns
         {:ok true :url u} or {:ok false :error msg} (order marked failed on
         provider error). Non-IQ (incl. nil) country → URL has
         `currency=usd` appended."
  [user-id amount base-url country-code]
  (let [pack (some #(when (= amount (:amount %)) %) (top-up-packs country-code))]
    (cond
      (not (config/credits-enabled?))
      {:ok false :error "Credits are not enabled"}

      (nil? pack)
      {:ok false :error "Invalid top-up amount"}

      :else
      (let [amount-iqd (:iqd pack)
            reference-id (str "fm-" (random-uuid))
            usd? (not (iqd-display? country-code))]
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
              {:ok false :error (:error r)})))))))

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
            (when (:credited r) (storage-meter/top-up-landed! (:user-id r)))
            (assoc r :ok true :wayl-status s))

          (#{"Cancelled" "Rejected" "Returned"} s)
          (do (db/fail-credit-order! reference-id)
              {:ok true :credited false :wayl-status s})

          :else
          {:ok true :credited false :wayl-status s})))))

;; ── Abandoned checkouts ──

(def abandon-after-minutes
  "Age at which a pending order counts as abandoned. Every completed payment
   so far finished within 5 minutes of its link being created."
  30)

(def abandon-lookback-hours
  "The sweep ignores pending orders older than this, so a deploy does not mail
   about every order that was abandoned before the sweep existed."
  24)

(defn- sweep-order!
  "Check one pending order at Wayl. Credit it when Wayl reports it paid; else
   mail about it once. Returns the outcome keyword."
  [{:keys [reference-id user-id kind amount-iqd created-at email display-name tries paid]}]
  (let [r (reconcile-order! reference-id)]
    (cond
      (not (:ok r))
      ;; wayl/get-link-status already mailed the lookup failure. The order
      ;; stays unmarked, so the next sweep tries it again.
      :lookup-failed

      (or (:credited r) (:licensed r))
      (do (commands/bump! user-id :credits-granted)
          (tel/log! {:level :info :id ::sweep-credited
                     :data {:reference-id reference-id :user-id user-id :email email
                            :kind kind :amount-iqd amount-iqd}}
            "Paid checkout credited by the sweep: the buyer never reached the return page")
          :credited)

      (= :already (:reason r))
      :already-complete

      :else
      (do (tel/log! {:level :info :id ::checkout-abandoned
                     :data {:reference-id reference-id :user-id user-id
                            :email email :name display-name
                            :kind kind :amount-iqd amount-iqd :created-at (str created-at)
                            :wayl-status (:wayl-status r)
                            :orders-by-user tries :paid-orders-by-user paid}}
            "Checkout abandoned: the user went to Wayl and did not pay")
          (db/mark-order-abandon-alerted! reference-id)
          :abandoned))))

(defn sweep-abandoned-checkouts!
  "Hourly sweep job. Each pending order between `abandon-after-minutes` and
   `abandon-lookback-hours` old is checked at Wayl once more: a paid one is
   credited (the buyer closed the tab before the return page), and an unpaid
   one logs ::checkout-abandoned, which freememo.logging mails, once per order.
   Returns the outcome per reference-id. No-op when credits are off."
  []
  (when (config/credits-enabled?)
    (into {}
      (for [o (db/unalerted-pending-orders abandon-after-minutes abandon-lookback-hours)]
        [(:reference_id o)
         (sweep-order! {:reference-id (:reference_id o) :user-id (:user_id o)
                        :kind (:kind o) :amount-iqd (:amount_iqd o)
                        :created-at (:created_at o) :email (:email o)
                        :display-name (:display_name o)
                        :tries (:tries o) :paid (:paid o)})]))))

(db/register-sweep-job! ::abandoned-checkouts #'sweep-abandoned-checkouts!)

;; ── Display currency ──
;;
;; Iraq sees prices in IQD. Every other country, and an unknown one, sees them
;; in USD. The Wayl checkout follows the same rule, so a label and the checkout
;; page always name one currency. :fx-iqd-per-usd is set to the rate Wayl shows
;; USD at, so a USD label is the amount the checkout page shows, not an
;; approximation of it.

(defn iqd-display?
  "True when this country sees prices in IQD. Only Iraq does."
  [country-code]
  (= "IQ" country-code))

(defn group-digits
  "A whole number with comma thousands separators: 50000 → \"50,000\".
   Locale-fixed, so a server locale cannot turn the comma into a dot."
  [n]
  (String/format java.util.Locale/US "%,d" (object-array [(long n)])))

(defn usd-str
  "A USD amount as a price: \"$40\" when whole, \"$3.29\" otherwise,
   \"<$0.01\" for a positive amount below one cent. Rounds to the cent."
  [usd]
  (let [cents (Math/round (* 100.0 (double usd)))]
    (cond
      (and (pos? usd) (zero? cents)) "<$0.01"
      (zero? (mod cents 100)) (str "$" (group-digits (quot cents 100)))
      :else (str "$" (group-digits (quot cents 100))
              (String/format java.util.Locale/US ".%02d" (object-array [(mod cents 100)]))))))

(defn usd->iqd
  "IQD that Wayl charges for a USD amount, at the configured rate. nil when the
   rate is unconfigured."
  [usd]
  (when-let [fx (config/fx-iqd-per-usd)]
    (when (pos? fx)
      (long (Math/ceil (* (double usd) fx))))))

(defn top-up-packs
  "The top-up buttons this country sees, each {:amount a :iqd n :label s}.
   :amount is in the pack's own currency (IQD for Iraq, whole USD otherwise)
   and is what start-checkout! takes. :iqd is what Wayl charges and how many
   credits the pack adds. A USD pack is absent while the rate is unconfigured."
  [country-code]
  (if (iqd-display? country-code)
    (mapv (fn [iqd] {:amount iqd :iqd iqd :label (str (group-digits iqd) " IQD")})
      (config/presets))
    (into []
      (keep (fn [usd]
              (when-let [iqd (usd->iqd usd)]
                {:amount usd :iqd iqd
                 :label (str (usd-str usd) " (" (group-digits iqd) " credits)")})))
      (config/presets-usd))))

(defn credits-label
  "A credit amount for display: 12340 → \"12,340 credits\". The same in every
   country: a balance is in credits, and only a price names a currency."
  [credits]
  (str (group-digits (or credits 0)) " credits"))

(defn top-up-note
  "The sentence under the top-up buttons: how many credits a unit of this
   country's currency buys."
  [country-code]
  (let [fx (config/fx-iqd-per-usd)]
    (if (or (iqd-display? country-code) (nil? fx))
      "You get 1 credit for each 1 IQD you pay."
      (str "You get " (group-digits fx) " credits for each $1."))))
