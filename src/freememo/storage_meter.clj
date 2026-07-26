(ns freememo.storage-meter
  "GB-month storage rent (plans/incremental-video.md §4.6).

   Two prices, deliberately separate (decision R3): the quota tier caps HOW MUCH
   may be stored, this meter prices HOW LONG it is stored. A top-up therefore
   buys rent, not a bigger cap — collapsing them would charge twice for one
   resource.

   Accrual is lazy (decision G2): the charge for an interval is computed when
   somebody looks, from `elapsed × bytes`, rather than by a daily job walking
   every user. The hourly sweep covers the gap lazy accrual cannot — a dormant
   account never triggers a lookup, so it would otherwise store bytes free
   forever (decision T1).

   Running dry does not delete anything the user wrote (decision H3): playback
   is blocked, a grace window opens, and only after it expires are the BYTES
   unlinked. The topic, its transcript, its extracts, its cards and its
   schedule all survive — which is exactly why video extracts copy their text
   in at creation instead of deriving it on read."
  (:require
   [freememo.config :as config]
   [freememo.db :as db]
   [freememo.logging :as log]
   [freememo.toasts :as toasts]
   [taoensso.telemere :as tel]))

;; ── Tunables ───────────────────────────────────────────────────────────────

(def ^:private bytes-per-gb 1073741824)

(def ^:private month-ms
  "The 'month' in GB-month: 30 days, fixed. A calendar month would make the
   same stored library cost 3 % more in March than in February for no reason
   the user could act on."
  (* 30 24 60 60 1000))

(defn rate-iqd-per-gb-month
  "Storage price. `STORAGE_IQD_PER_GB_MONTH` env, else config.edn
   [:credits :storage-iqd-per-gb-month]. nil disables metering entirely —
   the self-host default, where storage is the operator's own disk."
  []
  (or (some-> (System/getenv "STORAGE_IQD_PER_GB_MONTH") parse-double)
    (some-> (get-in config/config [:credits :storage-iqd-per-gb-month]) double)))

(def grace-days
  "How long a zero-balance user's bytes survive before reclamation.
   Default 14 days — long enough to notice an email and top up."
  (or (some-> (System/getenv "STORAGE_GRACE_DAYS") parse-long) 14))

(def ^:private stale-meter-hours
  "How old `last_metered_at` must be before the sweep accrues for a user who
   has not logged in. Matched to the sweep's own hourly cadence."
  1)

(def upload-session-ttl-hours
  "How long an untouched upload session keeps its reservation before the sweep
   reaps it (§4.3 3.3). Generous: a slow 8 GB upload over a poor connection
   must not be reaped mid-flight."
  6)

(defn metering-enabled?
  "Metering runs only in a credits-enabled deployment WITH a configured rate.
   Both conditions matter: a self-host install must never debit, and an
   official deployment that forgot the rate must fail open rather than
   silently bill zero and then reclaim."
  []
  (boolean (and (config/credits-enabled?) (rate-iqd-per-gb-month))))

(defn storage-rate-label
  "The storage price as a display string — \"5 credits per GB-month\" — or nil
   when metering is off.

   The single formatter for this number: the Credits panel and the Storage card
   both show it, and they must not disagree. An integer rate reads as \"5\", not
   \"5.00\".

   Post: nil, or a non-empty string. nil means the user is never charged rent,
   so the caller must say nothing rather than say \"0\"."
  []
  (when (metering-enabled?)
    (let [r (double (rate-iqd-per-gb-month))
          n (if (== r (Math/rint r)) (str (long r)) (format "%.2f" r))]
      (str n " credit" (when-not (= "1" n) "s") " per GB-month"))))

;; ── Pure pricing ───────────────────────────────────────────────────────────

(defn micro-iqd-for
  "Rent in micro-IQD for holding `usage-bytes` over `elapsed-ms`.

   micro because one hourly tick of a modest library prices below a single IQD;
   `db/accrue-storage-charge!` carries the remainder so nothing is lost to
   rounding. Pure — the meter's arithmetic is testable without a database."
  ^long [rate ^long usage-bytes ^long elapsed-ms]
  (if (or (nil? rate) (not (pos? usage-bytes)) (not (pos? elapsed-ms)))
    0
    (long (Math/round (* (double rate)
                        (/ (double usage-bytes) bytes-per-gb)
                        (/ (double elapsed-ms) month-ms)
                        1000000.0)))))

;; ── Accrual ────────────────────────────────────────────────────────────────

(defonce ^{:private true
           :doc "user-id -> epoch ms of the last accrual attempt. Purely a lock-
   contention damper: accrue! is already idempotent, but every call takes a
   `FOR UPDATE` on the users row, and the viewer would otherwise fire one per
   navigation."}
  !last-attempt (atom {}))

(def ^:private attempt-throttle-ms 300000) ; 5 min

(defn- transition-grace!
  "Open or close the grace window after an accrual.

   Opening is conditional on actually holding video bytes: a user with no video
   and no balance is simply a free-tier user, not someone whose data is counting
   down. `db/set-storage-grace!` only writes on a real transition, so the
   deadline is measured from the FIRST zero-balance observation, not the latest
   sweep, and the toast below fires once per entry rather than hourly."
  [user-id]
  (let [{:keys [credit_balance_iqd storage_grace_started_at has_video]}
        (db/get-storage-meter-state user-id)]
    (cond
      (and (<= (or credit_balance_iqd 0) 0) has_video (nil? storage_grace_started_at))
      (do (db/set-storage-grace! user-id (java.sql.Timestamp/from (java.time.Instant/now)))
          (tel/log! {:level :warn :id ::grace-opened :data {:user-id user-id}}
            "Storage grace window opened")
          ;; §4.6 6.6 — one notification per entry.
          (toasts/push! user-id
            {:level :error
             :message (str "Out of credits. Video playback is paused; your videos are kept for "
                        grace-days " days.")
             :actions [{:label "Top up credits" :nav :settings}]})
          :opened)

      (and (pos? (or credit_balance_iqd 0)) (some? storage_grace_started_at))
      (do (db/set-storage-grace! user-id nil)
          (tel/log! {:level :info :id ::grace-cleared :data {:user-id user-id}}
            "Storage grace window cleared")
          :cleared)

      :else nil)))

(defn accrue!
  "§4.6 6.1 — accrue rent for `user-id` up to now, then reconcile the grace flag.

   Called on user access (session boot) and by the sweep. Total: a metering
   failure is logged and swallowed, because it must never break the page the
   user was trying to open.

   Post: {:charged n …} when a pass ran, nil when throttled or disabled."
  ([user-id] (accrue! user-id false))
  ([user-id force?]
   (when (metering-enabled?)
     (let [now (System/currentTimeMillis)
           last (get @!last-attempt user-id 0)]
       (when (or force? (> (- now last) attempt-throttle-ms))
         (swap! !last-attempt assoc user-id now)
         (try
           (let [rate (rate-iqd-per-gb-month)
                 result (db/accrue-storage-charge! user-id
                          (fn [{:keys [usage-bytes elapsed-ms]}]
                            (micro-iqd-for rate usage-bytes elapsed-ms)))]
             (when (pos? (:charged result 0))
               (tel/log! {:level :info :id ::storage-charge
                          :data {:user-id user-id :cost-iqd (:charged result)
                                 :balance-after (:balance-after result)
                                 :elapsed-ms (:elapsed-ms result)}}
                 "Storage rent debited"))
             (transition-grace! user-id)
             result)
           (catch Exception e
             (tel/error! {:id ::accrue :data {:user-id user-id}} e)
             nil)))))))

(defn playback-blocked?
  "§4.6 6.3 — whether `/api/video/:id` should refuse.

   A pure flag read: the grace column is maintained by `accrue!`, so the
   playback path stays a single indexed lookup instead of a row lock on every
   Range request — and a media element issues a lot of Range requests."
  [user-id]
  (and (metering-enabled?)
       (some? (db/storage-grace-started-at user-id))))

;; ── Sweep jobs (§4.6 6.4) ──────────────────────────────────────────────────

(defn sweep-accrue!
  "Accrue for users whose meter has gone stale — decision T1.

   Lazy accrual alone leaves a hole: a user who never logs in never triggers a
   lookup, never accrues, never reaches zero, never enters grace, and is never
   swept. This closes it."
  []
  (when (metering-enabled?)
    (let [ids (db/users-due-for-metering stale-meter-hours)]
      (doseq [id ids] (accrue! id true))
      (when (seq ids)
        (log/log-info (str "Storage meter: accrued for " (count ids) " stale account(s)")))
      (count ids))))

(defn sweep-reclaim!
  "Unlink the video bytes of users whose grace window has expired — decision H3.
   Everything except the bytes survives (§4.6 6.5)."
  []
  (when (metering-enabled?)
    (let [ids (db/users-with-expired-grace grace-days)]
      (doseq [id ids]
        (let [r (db/reclaim-user-videos! id)]
          (when (pos? (:videos r 0))
            (toasts/push! id
              {:level :error
               :message (str "Storage grace expired — " (:videos r)
                          " video file(s) were removed. Transcripts, extracts and cards were kept.")
               :actions [{:label "Top up credits" :nav :settings}]}))))
      (count ids))))

(defn sweep-reap-uploads!
  "§4.3 3.3 — reclaim abandoned upload sessions. Runs regardless of metering:
   the leak is a large object plus a byte reservation, and both exist in a
   self-host install too."
  []
  (db/reap-stale-video-uploads! upload-session-ttl-hours))

;; Registered at namespace load so db.clj carries no dependency on this ns.
;; Registration is idempotent per label, so a REPL reload replaces rather than
;; duplicates the job.
(db/register-sweep-job! ::accrue-stale #'sweep-accrue!)
(db/register-sweep-job! ::reclaim-expired-grace #'sweep-reclaim!)
(db/register-sweep-job! ::reap-upload-sessions #'sweep-reap-uploads!)
