(ns freememo.desktop-license
  "Desktop licensing (plans/electron-local-db-sync.md §13), server side.

   A Sync License is a row in `desktop_licenses`. One row covers sync and AI
   on every Release, with no end date (ADR 0002). `access` is the only function that decides
   what an account may do (13.4). The sync routes, the id-block route and the
   AI proxy all call it.

   | State       | When                                  | Sync          | AI  |
   |-------------|---------------------------------------|---------------|-----|
   | :licensed   | a license exists                     | full          | yes |
   | :trial      | the trial is open, or within 60 days  | full          | yes |
   |             | of the first sign-in                  |               |     |
   | :unlicensed | no license, trial over                | none (O12)    | no  |

   Local use never depends on this (13.4.4). The desktop keeps working on its
   own database in every state."
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.config :as config]
            [freememo.credits :as credits]
            [freememo.db :as db])
  (:import [java.time Duration Instant]))

(def trial-open?
  "While true, the trial has no end date: every account that no license covers
   is in :trial, and the Settings page offers no purchase. Set it to false to
   start the 60-day trial (P4) and sales."
  true)
(def trial-days "P4" 60)

(defn price-iqd
  "The license price in IQD, the currency Wayl charges in, or nil while the
   price or the exchange rate is unconfigured. The USD price is
   `:desktop-license-usd` in config.edn, which public history excludes (O14)."
  []
  (when-let [usd (config/desktop-license-usd)]
    (some-> (config/fx-iqd-per-usd) (* usd) Math/ceil long)))

(defn price-label
  "The license price as this country sees it: \"60,800 credits\" in Iraq,
   \"$40 (60,800 credits)\" elsewhere. nil while the price is unconfigured."
  [country-code]
  (when-let [iqd (price-iqd)]
    (let [in-credits (str (credits/group-digits iqd) " credits")]
      (if (credits/iqd-display? country-code)
        in-credits
        (str (credits/usd-str (config/desktop-license-usd)) " (" in-credits ")")))))

(def ^:private operations
  {:full #{:meta :graves :upload :download :blob :finish :full-upload :full-download :ai}
   :download #{:meta :download :blob :finish :full-download}
   :none #{:meta}})

(defn allows?
  "Whether `access` (from `access`) permits `operation`."
  [access operation]
  (contains? (operations (:sync access)) operation))

(defn- plus-days ^Instant [^Instant t days]
  (.plus t (Duration/ofDays days)))

(defn- ->instant ^Instant [t]
  (cond (instance? Instant t) t
        (instance? java.sql.Timestamp t) (.toInstant ^java.sql.Timestamp t)
        (instance? java.time.OffsetDateTime t) (.toInstant ^java.time.OffsetDateTime t)
        :else (throw (ex-info "not a timestamp" {:value t}))))

(defn access
  "13.4 — what the account may do from a Local Install.
   Pre:  the account may have no desktop_accounts row.
   Post: {:state :sync (:full|:download|:none) :ai? bool, and the dates the
         Settings page shows}.
   Inv:  the answer depends only on the database and `now`, never on anything
         the client sends, so editing the desktop's cached copy (13.6)
         unlocks nothing."
  ([conn user-id] (access conn user-id (Instant/now)))
  ([conn user-id ^Instant now]
   (let [opts {:builder-fn rs/as-unqualified-lower-maps}
         account (jdbc/execute-one! conn ["SELECT trial_started_at
                                           FROM desktop_accounts WHERE user_id = ?" user-id] opts)
         licenses (jdbc/execute! conn ["SELECT id FROM desktop_licenses WHERE user_id = ?" user-id] opts)
         trial-ends (some-> (:trial_started_at account) ->instant (plus-days trial-days))
         base {:trial-ends (some-> trial-ends str)}]
     (cond
       (seq licenses)
       (assoc base :state :licensed :sync :full :ai? true)

       trial-open?
       (assoc base :state :trial :sync :full :ai? true :trial-open? true :trial-ends nil)

       (and trial-ends (.isBefore now trial-ends))
       (assoc base :state :trial :sync :full :ai? true
         :trial-days-left (.toDays (Duration/between now trial-ends)))

       :else
       (assoc base :state :unlicensed :sync :none :ai? false)))))

(defn reason
  "The refusal code the desktop shows for a state that refuses an operation
   (13.4.3)."
  [access]
  (case (:state access)
    :unlicensed "license-required"
    "license-refused"))

;; ── Purchase (13.1) ────────────────────────────────────────────────

(defn buy-with-credits!
  "13.1.2 — debit the license price and write the license in one transaction.
   Post: {:ok true} with exactly one new license, or {:ok false :error msg}
         with nothing written (an insufficient balance writes nothing)."
  [user-id]
  (if-let [price (price-iqd)]
    (jdbc/with-transaction [tx db/ds]
      (let [opts {:builder-fn rs/as-unqualified-lower-maps}
            balance (:credit_balance_iqd
                     (jdbc/execute-one! tx ["SELECT credit_balance_iqd FROM users WHERE id = ? FOR UPDATE"
                                            user-id] opts))]
        (if (or (nil? balance) (< balance price))
          {:ok false :error "Not enough credits for a desktop license. Top up in Settings, or pay by card."}
          (let [after (- balance price)]
            (jdbc/execute! tx ["UPDATE users SET credit_balance_iqd = ? WHERE id = ?" after user-id])
            (jdbc/execute! tx ["INSERT INTO credit_transactions
                                  (user_id, kind, amount_iqd, balance_after, endpoint)
                                VALUES (?, 'debit', ?, ?, 'desktop-license')"
                               user-id price after])
            (db/grant-desktop-license! tx user-id "credits" nil)
            {:ok true}))))
    {:ok false :error "Desktop license pricing is not configured."}))

(defn start-checkout!
  "13.1.3 — a pending desktop_license order and its Wayl link.
   Post: see `credits/open-order!`. The license is written when the webhook
         completes the order (`db/complete-credit-order!`)."
  [user-id base-url country-code]
  (if-let [price (price-iqd)]
    (credits/open-order! user-id
      {:kind "desktop_license"
       :reference-id (str "fm-dl-" (random-uuid))
       :amount-iqd price
       :label "FreeMemo Sync License"
       :base-url base-url
       :currency-suffix? (not= "IQ" country-code)})
    {:ok false :error "Desktop license pricing is not configured."}))
