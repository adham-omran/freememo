(ns freememo.outreach
  "One-off personalised mail to named existing users (server-only, REPL-driven).

   Distinct from `freememo.changelog`, which broadcasts release notes to
   opted-in subscribers as a single BCC message. This namespace addresses each
   recipient individually by name, so it needs one message per recipient and a
   per-recipient delivery log.

   Operator flow (run from the repo root on the host that holds config.edn):
     (preview-booking-link)     ; renders every message, sends NOTHING
     (send-booking-link-test! \"you@example.com\" \"You\")  ; one preflight mail
     (send-booking-link!)       ; the real send, resumable

   `send-booking-link!` skips anyone who already has a `booking-link-event-type`
   row, so a re-run after a partial failure resumes instead of double-mailing.

   Requires no Electric namespace — it runs under the JVM default stack."
  (:require
   [clojure.string :as str]
   [freememo.config :as config]
   [freememo.db :as db]
   [freememo.settings :as settings]
   [postal.core :as postal]
   [taoensso.telemere :as tel]))

;; ---------------------------------------------------------------------------
;; Booking-link campaign — the one-off this namespace was built for
;; ---------------------------------------------------------------------------

(def booking-link-recipient-ids
  "The exact users who receive the booking-link mail.

   A literal list, not a query predicate: users created between fixing this
   list and running the send MUST NOT silently join the batch. Deliberately
   excluded — 10 (Dustin Getz), 16 (operator's own account), 25 (explicitly
   set email_updates=false)."
  [2 8 9 11 12 14 15 17 18 19 20 21 22 23 24 26 27 28 29 30
   31 32 33 34 35 36 37])

(def booking-link-event-type
  "user_events.event_type marking a delivered booking-link mail. This value is
   the idempotency key: bump the version suffix only for a resend that should
   reach the same people again."
  "outreach_booking_link_v1")

(def booking-link-subject
  "Names the product, because the From address is a personal domain and 20 of
   the 27 recipients never opted into mail from it."
  "FreeMemo — new booking link")

(def reply-to-address
  "The signature says \"Best, Adham\", so replies must reach a monitored inbox
   rather than the sending mailbox."
  "adham@hyperfiddle.net")

(def ^:private inter-send-delay-ms
  "Pause between individual messages. 27 recipients ≈ 1 minute."
  2000)

(defn- footer-text
  "Post: the footer, worded for users who signed up but never subscribed to
   updates — most of this batch did not."
  []
  (str "\n\n---\n"
    "You're receiving this because you signed up for FreeMemo.\n"
    "Manage your email preferences in Settings: "
    settings/app-base-url "/settings"))

(defn booking-link-body
  "Post: the full plain-text message addressed to `display-name`.
   Pre:  display-name is non-blank — enforced upstream by `resolve-addressees`."
  [display-name]
  (str "Hey " display-name ",\n"
    "\n"
    "Quick update, I've moved to a new, permanent booking link so this one "
    "won't change on you again: https://adham-omran.com/meet\n"
    "\n"
    "The 2,500 credit offer for hopping on a call still stands if you're "
    "interested.\n"
    "\n"
    "Best, Adham"
    (footer-text)))

;; ---------------------------------------------------------------------------
;; Recipient resolution
;; ---------------------------------------------------------------------------

(defn resolve-addressees
  "Look up every id in `booking-link-recipient-ids`.
   Pre:  each id exists and has a non-blank email AND display_name — run the
         display-name backfill before calling.
   Post: vector of {:user-id :email :display-name}, ascending by id, exactly
         as long as booking-link-recipient-ids.
   Throws rather than returning a partial set: a missing display_name would
   otherwise mail out \"Hey ,\"."
  []
  (let [rows (db/get-mail-addressees booking-link-recipient-ids)
        addressees (mapv (fn [r]
                           {:user-id (:users/id r)
                            :email (:users/email r)
                            :display-name (:users/display_name r)})
                     rows)
        found-ids (set (map :user-id addressees))
        missing (remove found-ids booking-link-recipient-ids)
        incomplete (filterv #(or (str/blank? (:email %))
                                 (str/blank? (:display-name %)))
                     addressees)]
    (when (seq missing)
      (throw (ex-info "Outreach recipients missing from users table"
               {:missing-ids (vec missing)
                :asked (count booking-link-recipient-ids)
                :found (count addressees)})))
    (when (seq incomplete)
      (throw (ex-info "Outreach recipients missing email or display_name"
               {:user-ids (mapv :user-id incomplete)})))
    addressees))

(defn- pending-addressees
  "Post: [all-addressees pending] where pending excludes anyone already
   carrying `booking-link-event-type`."
  []
  (let [addressees (resolve-addressees)
        already-sent (db/user-ids-with-event booking-link-event-type)]
    [addressees (filterv #(not (contains? already-sent (:user-id %))) addressees)]))

;; ---------------------------------------------------------------------------
;; Message construction
;; ---------------------------------------------------------------------------

(defn- booking-link-message
  "postal message map for one addressee.
   Post: exactly one :to, no :bcc, single-part text/plain (postal's add-body!
   uses .setText with utf-8 for a string body)."
  [from addressee]
  {:from from
   :to (:email addressee)
   :reply-to reply-to-address
   :subject booking-link-subject
   :body (booking-link-body (:display-name addressee))})

(defn- smtp-connection
  "Post: the postal connection map for `smtp`, implicit TLS on port 465."
  [smtp]
  {:host (:host smtp) :port (:port smtp)
   :user (:user smtp) :pass (:pass smtp) :ssl true})

;; ---------------------------------------------------------------------------
;; Preview (dry run)
;; ---------------------------------------------------------------------------

(defn preview-booking-link
  "DRY RUN: print every message that `send-booking-link!` would send.
   Post: {:addressees n :already-sent n :pending n}; no SMTP traffic and no
   database write. Works with SMTP unconfigured."
  []
  (let [[addressees pending] (pending-addressees)
        smtp (config/smtp-config)]
    (println "── booking-link outreach — DRY RUN (nothing sent) ──")
    (println "Subject:    " booking-link-subject)
    (println "From:       " (or (:from smtp) "(SMTP NOT configured)"))
    (println "Reply-To:   " reply-to-address)
    (println "SMTP:       " (if smtp
                              (str (:host smtp) ":" (:port smtp)
                                " as " (:user smtp))
                              "NOT configured — send-booking-link! would no-op"))
    (println "Recipients: " (count addressees)
      (str "(" (count pending) " pending, "
        (- (count addressees) (count pending)) " already sent)"))
    (doseq [a pending]
      (println (str "\n── " (:user-id a) " → " (:email a) " ──"))
      (println (booking-link-body (:display-name a))))
    (println "\n── end of dry run ──")
    {:addressees (count addressees)
     :already-sent (- (count addressees) (count pending))
     :pending (count pending)}))

;; ---------------------------------------------------------------------------
;; Send
;; ---------------------------------------------------------------------------

(defn- send-to-addressee!
  "Send one message and, on success, record it.
   Post: {:user-id :email :ok? bool :error ?string}; a user_events row exists
   iff :ok? is true. Never throws — one bad address must not abort the batch."
  [smtp from addressee]
  (let [base {:user-id (:user-id addressee) :email (:email addressee)}]
    (try
      (let [result (postal/send-message (smtp-connection smtp)
                     (booking-link-message from addressee))]
        (if (zero? (:code result))
          (do (db/insert-user-event! (:user-id addressee) booking-link-event-type
                {:email (:email addressee)
                 :from from
                 :subject booking-link-subject})
              (assoc base :ok? true))
          (assoc base :ok? false
            :error (str (:error result) " — " (:message result)))))
      (catch Exception e
        (assoc base :ok? false :error (.getMessage e))))))

(defn send-booking-link-test!
  "Send one preflight message to an arbitrary address.
   Pre:  config/smtp-config non-nil.
   Post: one message delivered to `address`; NO user_events row is written, so
   this can target a non-user and cannot pollute the idempotency key."
  [address display-name]
  (if-let [smtp (config/smtp-config)]
    (let [from (:from smtp)
          result (postal/send-message (smtp-connection smtp)
                   (booking-link-message from {:email address
                                               :display-name display-name}))]
      (tel/log! {:level :info :id ::test-send
                 :data {:to address :code (:code result)}}
        "Booking-link test send")
      result)
    {:code 99 :error :no-smtp :message "config/smtp-config returned nil"}))

(defn send-booking-link!
  "Send the booking-link mail to every pending recipient, one message each.
   Pre:  config/smtp-config non-nil; `resolve-addressees` preconditions hold.
   Post: one delivered message and one user_events row per {:ok? true} result;
         already-logged recipients are skipped, so re-running after a partial
         failure resumes rather than double-mailing.
   Post: {:sent n :skipped n :failed n :results [...]}, or
         {:sent 0 :reason :no-smtp} when SMTP is unconfigured."
  []
  (let [smtp (config/smtp-config)
        [addressees pending] (pending-addressees)
        skipped (- (count addressees) (count pending))]
    (if (nil? smtp)
      (do (tel/log! {:level :warn :id ::outreach} "No SMTP config — nothing sent")
          {:sent 0 :skipped skipped :failed 0 :results [] :reason :no-smtp})
      (let [from (:from smtp)
            results (reduce (fn [acc addressee]
                              (when (seq acc) (Thread/sleep inter-send-delay-ms))
                              (conj acc (send-to-addressee! smtp from addressee)))
                      [] pending)
            sent (count (filter :ok? results))
            failed (- (count results) sent)]
        (tel/log! {:level :info :id ::outreach
                   :data {:event booking-link-event-type
                          :sent sent :skipped skipped :failed failed}}
          "Booking-link outreach finished")
        (doseq [r results]
          (println (if (:ok? r) "  ok  " "  FAIL")
            (:user-id r) (:email r) (or (:error r) "")))
        {:sent sent :skipped skipped :failed failed :results results}))))
