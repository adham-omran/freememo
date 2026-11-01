(ns freememo.receipt
  "Mail the buyer a receipt after a credit purchase, with a PDF attached.

   A sweep, not a call at the point of payment. An order completes on one of two
   paths (the Wayl webhook, or the return page's reconcile), and an SMTP outage or
   a deploy can fall between the payment and the mail. So every minute the sweep
   asks the database which complete credit orders still owe a receipt, and
   `db/settle-next-receipt!` records each outcome in the transaction that locks
   the order. A crash after the send and before the commit sends that receipt a
   second time: at least once, never zero times.

   Both app colors run the sweep during a deploy. The row lock (SKIP LOCKED) is
   what keeps them from mailing one order twice.

   A failed attempt waits `db/receipt-retry-interval` before the next, until the
   order is 24 hours old.

   Receipts are off when `config/receipt-mail-config` is nil, which is
   everywhere but production. See plans/payment-receipt-email.md."
  (:require [clojure.string :as str]
            [freememo.config :as config]
            [freememo.db :as db]
            [freememo.typst :as typst]
            [postal.core :as postal]
            [taoensso.telemere :as tel])
  (:import [javax.mail.internet InternetAddress]
           [java.time Instant ZoneOffset]
           [java.time.format DateTimeFormatter]
           [java.util Locale]
           [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

(def cutoff
  "Orders completed before this instant never get a receipt. Receipts began on
   2026-10-30, and this date covers that day's buyers, who paid before the
   feature shipped. It also keeps an order copied from another deployment out
   of the sweep, because its completed_at is older."
  (Instant/parse "2026-10-30T00:00:00Z"))

(def ^:private template "typst/receipt.typ")

(def ^:private sweep-period-ms 60000)

(defn receipt-number
  "Pre: order-id is a positive integer. Post: \"FM-\" and the id, zero-padded
   to six digits. Ids are not gapless: an order that stayed pending used one."
  [order-id]
  (format "FM-%06d" order-id))

(defn- grouped
  "12345 → \"12,345\", in every JVM locale."
  [n]
  (String/format Locale/ROOT "%,d" (object-array [n])))

(def ^:private paid-at-format
  (DateTimeFormatter/ofPattern "d MMMM yyyy, HH:mm 'UTC'" Locale/ENGLISH))

(defn receipt-fields
  "Every string a receipt shows, for the PDF and the email body alike.
   Pre:  order is the map `db/settle-next-receipt!` passes; mail is
         `config/receipt-mail-config`, whose :from parses as an address.
   Post: {:seller {:name :site :email} :number :paid-at :line-item :amount
         :credits}, all strings. :site is \"\" when unset. :email is the
         address in :from, the one a reply reaches, and not the SMTP login.
   One credit is one IQD: `db/complete-credit-order!` credits amount_iqd as is."
  [order mail]
  (let [amount (:amount-iqd order)]
    {:seller {:name "FreeMemo" :site (or (:site mail) "")
              :email (.getAddress (InternetAddress. ^String (:from mail)))}
     :number (receipt-number (:id order))
     :paid-at (.format paid-at-format (.atZone ^Instant (:completed-at order) ZoneOffset/UTC))
     :line-item "FreeMemo credits"
     :amount (str (grouped amount) " IQD")
     :credits (grouped amount)}))

(defn receipt-body
  "The plain-text body of the buyer's mail. It holds every field the PDF holds,
   so the mail is a record on its own if the attachment is lost."
  [{:keys [number paid-at amount credits]}]
  (str "Payment received. Thank you.\n\n"
       "  Amount paid    " amount "\n"
       "  Credits added  " credits "\n"
       "  Order          " number "\n"
       "  Date           " paid-at "\n\n"
       "The receipt is attached as a PDF.\n"
       "Questions: reply to this email.\n"))

(defn receipt-message
  "The postal message for one receipt.
   Pre: fields from `receipt-fields`; pdf is the rendered bytes."
  [mail fields to subject pdf]
  {:from (:from mail)
   :to to
   :subject subject
   :body [{:type "text/plain; charset=utf-8" :content (receipt-body fields)}
          {:type :attachment
           :content pdf
           :content-type "application/pdf"
           :file-name (str "freememo-receipt-" (:number fields) ".pdf")}]})

(defn- deliver!
  "Post: {:ok true} or {:ok false :error S}. Never throws."
  [mail message]
  (try
    (let [result (postal/send-message (config/smtp-connection mail) message)]
      (if (zero? (:code result))
        {:ok true}
        {:ok false :error (str (:error result) ": " (:message result))}))
    (catch Exception e
      {:ok false :error (ex-message e)})))

(defn settle-order!
  "Do the receipt work for one locked order, and say what to record.
   Pre:  order is the map `db/settle-next-receipt!` passes; mail is non-nil.
   Post: :sent when the buyer's mail went out; :no-email when the buyer has no
         address; :gave-up when the order is 24 hours old and still owes a
         receipt; nil when rendering or sending failed and the next sweep
         should retry. Never throws for a render or SMTP failure.
   The copy to :copy-to goes out only after the buyer's mail, and its failure
   changes nothing: re-sending the buyer's receipt to retry the copy would be
   worse than losing the copy."
  [mail order]
  (let [log-data {:order-id (:id order) :user-id (:user-id order)}]
    (cond
      (str/blank? (:email order))
      (do (tel/log! {:level :info :id ::receipt-no-email :data log-data}
            "No receipt: the buyer has no email address")
          :no-email)

      (:expired? order)
      (do (tel/log! {:level :error :id ::receipt-gave-up :data log-data}
            "No receipt: still unsent 24 hours after payment. Send it by hand")
          :gave-up)

      :else
      (let [fields (receipt-fields order mail)
            rendered (typst/render-pdf template fields)]
        (if-not (:ok rendered)
          (do (tel/log! {:level :warn :id ::receipt-render-failed
                         :data (assoc log-data :reason (:reason rendered) :error (:error rendered))}
                "Receipt PDF failed; the next sweep retries")
              nil)
          (let [sent (deliver! mail (receipt-message mail fields (:email order)
                                      (str "FreeMemo receipt " (:number fields))
                                      (:pdf rendered)))]
            (if-not (:ok sent)
              (do (tel/log! {:level :warn :id ::receipt-send-failed
                             :data (assoc log-data :error (:error sent))}
                    "Receipt mail failed; the next sweep retries")
                  nil)
              (do (when-let [copy-to (:copy-to mail)]
                    (let [copied (deliver! mail (receipt-message mail fields copy-to
                                                  (str "[receipt] " (:number fields)
                                                       " user " (:user-id order))
                                                  (:pdf rendered)))]
                      (when-not (:ok copied)
                        (tel/log! {:level :warn :id ::receipt-copy-failed
                                   :data (assoc log-data :error (:error copied))}
                          "Receipt sent; the copy failed"))))
                  (tel/log! {:level :info :id ::receipt-sent
                             :data (assoc log-data :number (:number fields))}
                    "Receipt sent")
                  :sent))))))))

(defn sweep!
  "Settle every order that owes a receipt, each at most once per pass.
   Post: a vector of {:order-id :outcome}, empty when receipts are off or
         nothing is owed. An exception from one order is logged and counts as a
         failed attempt, and the pass goes on. A database failure propagates.
   Invariant: `settle` never throws, so every order the loop takes is recorded,
         and `db/settle-next-receipt!` does not return it again this pass."
  []
  (if-let [mail (config/receipt-mail-config)]
    (let [settle (fn [order]
                   (try (settle-order! mail order)
                     (catch Throwable t
                       (tel/error! {:id ::receipt-settle-failed :data {:order-id (:id order)}} t)
                       nil)))]
      (loop [results []]
        (if-let [result (db/settle-next-receipt! cutoff settle)]
          (recur (conj results result))
          results)))
    []))

(defonce ^:private scheduler (atom nil))

(defn start!
  "Run `sweep!` every minute. Idempotent: once per process.
   Pre:  `db/setup-schema` has run.
   Post: :started, or :already-running.
   Invariant: a throwing pass never stops the schedule, because
         `scheduleAtFixedRate` cancels every later run once its task throws."
  []
  (if @scheduler
    :already-running
    (let [exec (Executors/newSingleThreadScheduledExecutor)]
      (.scheduleAtFixedRate ^ScheduledExecutorService exec
        (fn [] (try (sweep!)
                 (catch Throwable t (tel/error! {:id ::sweep-failed} t))))
        sweep-period-ms sweep-period-ms TimeUnit/MILLISECONDS)
      (reset! scheduler exec)
      (tel/log! {:level :info :id ::sweeper-started
                 :data {:enabled? (some? (config/receipt-mail-config))}}
        "Receipt sweeper started")
      :started)))
