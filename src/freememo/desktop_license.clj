(ns freememo.desktop-license
  "Desktop licensing (plans/electron-local-db-sync.md §13), server side.

   A license is a row in `desktop_licenses`. One bought during desktop major N
   covers majors N and N + 1 (P2). `access` is the only function that decides
   what an account may do (13.4). The sync routes, the id-block route and the
   AI proxy all call it.

   | State       | When                                  | Sync          | AI  |
   |-------------|---------------------------------------|---------------|-----|
   | :licensed   | a license covers the client's major   | full          | yes |
   | :trial      | within 60 days of the first sign-in   | full          | yes |
   | :grace      | a license, none covering, < 30 days   | download only | no  |
   | :expired    | a license, none covering, ≥ 30 days   | none          | no  |
   | :unlicensed | no license, trial over                | none (O12)    | no  |

   Local use never depends on this (13.4.4). The desktop keeps working on its
   own database in every state."
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.config :as config]
            [freememo.db :as db]
            [freememo.desktop-version :as desktop-version]
            [freememo.wayl :as wayl])
  (:import [java.time Duration Instant]))

(def trial-days "P4" 60)
(def grace-days "O15" 30)
(def price-usd "O14. Purchases are final, like REAPER's." 40)

(defn price-iqd
  "The license price in IQD, the currency Wayl charges in, or nil while the
   exchange rate is unconfigured."
  []
  (some-> (config/fx-iqd-per-usd) (* price-usd) Math/ceil long))

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
  "13.4 — what the account may do from a desktop running `client-major`.
   Pre:  `conn` is writable; the account may have no desktop_accounts row.
   Post: {:state :sync (:full|:download|:none) :ai? bool, and the dates the
         Settings page shows}. The first call in grace records its start, so
         grace runs 30 days from the first sync the license did not cover.
   Inv:  the answer depends only on the database and `now`, never on anything
         the client sends except its major, so editing the desktop's cached
         copy (13.6) unlocks nothing."
  ([conn user-id client-major] (access conn user-id client-major (Instant/now)))
  ([conn user-id client-major ^Instant now]
   (let [opts {:builder-fn rs/as-unqualified-lower-maps}
         account (jdbc/execute-one! conn ["SELECT trial_started_at, grace_started_at
                                           FROM desktop_accounts WHERE user_id = ?" user-id] opts)
         licenses (jdbc/execute! conn ["SELECT major_from, major_to FROM desktop_licenses
                                        WHERE user_id = ? ORDER BY major_to DESC" user-id] opts)
         covering (first (filter #(<= (:major_from %) client-major (:major_to %)) licenses))
         trial-ends (some-> (:trial_started_at account) ->instant (plus-days trial-days))
         base {:client-major client-major
               :covered-majors (mapv (juxt :major_from :major_to) licenses)
               :trial-ends (some-> trial-ends str)}]
     (cond
       covering
       (assoc base :state :licensed :sync :full :ai? true)

       (and trial-ends (.isBefore now trial-ends))
       (assoc base :state :trial :sync :full :ai? true
         :trial-days-left (.toDays (Duration/between now trial-ends)))

       (seq licenses)
       (let [started (or (some-> (:grace_started_at account) ->instant)
                       (do (jdbc/execute! conn ["UPDATE desktop_accounts SET grace_started_at = ?
                                                 WHERE user_id = ? AND grace_started_at IS NULL"
                                                (java.sql.Timestamp/from now) user-id])
                           now))
             grace-ends (plus-days started grace-days)]
         (if (.isBefore now grace-ends)
           (assoc base :state :grace :sync :download :ai? false :grace-ends (str grace-ends))
           (assoc base :state :expired :sync :none :ai? false :grace-ends (str grace-ends))))

       :else
       (assoc base :state :unlicensed :sync :none :ai? false)))))

(defn reason
  "The refusal code the desktop shows for a state that refuses an operation
   (13.4.3)."
  [access]
  (case (:state access)
    :grace "license-grace-download-only"
    :expired "license-expired"
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
   Post: {:ok true :url u} or {:ok false :error msg}. The license is written
         when the webhook completes the order (`db/complete-credit-order!`)."
  [user-id base-url country-code]
  (if-let [price (price-iqd)]
    (let [reference-id (str "fm-dl-" (random-uuid))]
      (db/insert-credit-order! user-id reference-id price "desktop_license")
      (let [r (wayl/create-link! {:reference-id reference-id
                                  :amount-iqd price
                                  :label (str "FreeMemo desktop license (versions "
                                           desktop-version/desktop-major " and "
                                           (inc desktop-version/desktop-major) ")")
                                  :webhook-url (str base-url "/api/wayl/webhook")
                                  :redirection-url (str base-url "/credits/return")
                                  :currency-suffix? (not= "IQ" country-code)})]
        (if (:ok r)
          (do (db/set-order-wayl-fields! reference-id (:code r) (:link-id r))
              {:ok true :url (:url r)})
          (do (db/fail-credit-order! reference-id)
              {:ok false :error (:error r)}))))
    {:ok false :error "Desktop license pricing is not configured."}))
