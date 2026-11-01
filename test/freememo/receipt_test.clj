(ns freememo.receipt-test
  "Payment receipts: the fields, the outcome of one order, and the sweep. Typst,
   SMTP and the database are stubbed, so this runs with no PostgreSQL."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.config :as config]
            [freememo.db :as db]
            [freememo.receipt :as receipt]
            [freememo.typst :as typst]
            [postal.core :as postal])
  (:import [java.time Instant]))

(def ^:private mail
  {:host "smtp.example" :port 465 :user "login-not-an-address" :pass "x"
   :from "FreeMemo <mail@example.com>" :copy-to "owner@example.com"
   :site "freememo.net"})

(def ^:private order
  {:id 123 :user-id 50 :reference-id "ref-1" :amount-iqd 150000
   :completed-at (Instant/parse "2026-10-30T15:52:40Z")
   :email "buyer@example.com" :expired? false})

(defmacro ^:private with-stubs
  "Run body with Typst returning `pdf-result` and postal answering each send
   with (deliver message). Binds `sent` to an atom of every message sent."
  [[sent pdf-result deliver] & body]
  `(let [~sent (atom [])]
     (with-redefs [typst/render-pdf (fn [_# _#] ~pdf-result)
                   postal/send-message (fn [_# message#]
                                         (swap! ~sent conj message#)
                                         (~deliver message#))]
       ~@body)))

(def ^:private ok-pdf {:ok true :pdf (.getBytes "%PDF")})
(defn- delivered [_] {:code 0 :error :SUCCESS :message "messages sent"})

(deftest the-fields-are-the-strings-both-documents-show
  (let [fields (receipt/receipt-fields order mail)]
    (is (= {:seller {:name "FreeMemo" :site "freememo.net" :email "mail@example.com"}
            :number "FM-000123"
            :paid-at "30 October 2026, 15:52 UTC"
            :line-item "FreeMemo credits"
            :amount "150,000 IQD"
            :credits "150,000"}
           fields))
    (testing "the body carries every field the PDF carries"
      (let [body (receipt/receipt-body fields)]
        (doseq [s ["FM-000123" "30 October 2026, 15:52 UTC" "150,000 IQD" "150,000"]]
          (is (str/includes? body s)))))
    (testing "an unset site is an empty string, which the template omits"
      (is (= "" (get-in (receipt/receipt-fields order (dissoc mail :site)) [:seller :site]))))))

(deftest a-paid-order-mails-the-buyer-then-the-copy
  (with-stubs [sent ok-pdf delivered]
    (is (= :sent (receipt/settle-order! mail order)))
    (is (= [["buyer@example.com" "FreeMemo receipt FM-000123"]
            ["owner@example.com" "[receipt] FM-000123 user 50"]]
           (map (juxt :to :subject) @sent)))
    (let [[text attachment] (:body (first @sent))]
      (is (str/includes? (:content text) "150,000 IQD"))
      (is (= {:type :attachment :content-type "application/pdf"
              :file-name "freememo-receipt-FM-000123.pdf"}
             (dissoc attachment :content))))))

(deftest no-copy-address-sends-the-buyer-mail-alone
  (with-stubs [sent ok-pdf delivered]
    (is (= :sent (receipt/settle-order! (dissoc mail :copy-to) order)))
    (is (= ["buyer@example.com"] (map :to @sent)))))

(deftest a-failed-copy-still-records-the-receipt
  (with-stubs [sent ok-pdf (fn [m] (if (= "owner@example.com" (:to m))
                                     (throw (ex-info "mailbox full" {}))
                                     (delivered m)))]
    (is (= :sent (receipt/settle-order! mail order))
        "retrying the copy would mail the buyer a second receipt")
    (is (= 2 (count @sent)))))

(deftest a-failure-before-the-buyer-mail-leaves-the-order-for-the-next-sweep
  (testing "Typst fails: nothing is sent"
    (with-stubs [sent {:ok false :reason :missing-binary :error "no typst"} delivered]
      (is (nil? (receipt/settle-order! mail order)))
      (is (empty? @sent))))
  (testing "SMTP refuses: no copy is sent either"
    (with-stubs [sent ok-pdf (fn [_] {:code 2 :error :SMTP :message "refused"})]
      (is (nil? (receipt/settle-order! mail order)))
      (is (= 1 (count @sent)))))
  (testing "SMTP throws"
    (with-stubs [_sent ok-pdf (fn [_] (throw (java.net.ConnectException. "down")))]
      (is (nil? (receipt/settle-order! mail order))))))

(deftest no-address-and-expiry-are-recorded-without-a-send
  (with-stubs [sent ok-pdf delivered]
    (is (= :no-email (receipt/settle-order! mail (assoc order :email nil))))
    (is (= :no-email (receipt/settle-order! mail (assoc order :email "  "))))
    (is (= :gave-up (receipt/settle-order! mail (assoc order :expired? true))))
    (is (empty? @sent))))

(defn- fake-settle-next
  "A stand-in for db/settle-next-receipt! over `orders`: a stamped order leaves
   `orders`, and a failed one is hidden as if it waited out the retry interval."
  [orders attempted]
  (fn [_cutoff settle]
    (when-let [o (first (remove #(@attempted (:id %)) @orders))]
      (let [outcome (settle o)]
        (if outcome
          (swap! orders (fn [os] (remove #(= (:id o) (:id %)) os)))
          (swap! attempted conj (:id o)))
        {:order-id (:id o) :outcome outcome}))))

(deftest one-bad-order-does-not-stop-the-sweep-or-loop-it
  (let [orders (atom [(assoc order :id 1) (assoc order :id 2) (assoc order :id 3)])]
    (with-redefs [config/receipt-mail-config (constantly mail)
                  db/settle-next-receipt! (fake-settle-next orders (atom #{}))
                  receipt/settle-order! (fn [_ o]
                                          (case (:id o)
                                            1 (throw (ex-info "bug" {}))
                                            2 nil
                                            3 :sent))]
      (is (= [{:order-id 1 :outcome nil} {:order-id 2 :outcome nil} {:order-id 3 :outcome :sent}]
             (receipt/sweep!)))
      (is (= [1 2] (map :id @orders)) "failed orders stay owed for the next pass"))))

(deftest receipts-are-off-without-smtp
  (with-redefs [config/receipt-mail-config (constantly nil)
                db/settle-next-receipt! (fn [& _] (throw (ex-info "must not query" {})))]
    (is (= [] (receipt/sweep!)))))

(defn- config-from
  "Run config fns against `values` alone, keyed by env var name, so the
   machine's environment cannot change the answer."
  [values receipts-on? f]
  (with-redefs-fn {#'config/env-or-config (fn [env-var _path] (get values env-var))
                   #'config/receipts-enabled? (constantly receipts-on?)}
    f))

(def ^:private smtp-env {"SMTP_HOST" "h" "SMTP_USER" "mail@example.com" "SMTP_PASS" "p"})

(deftest receipts-need-the-production-switch-and-no-alert-address
  (testing "SMTP alone, switch off: no receipts, as in dev"
    (is (nil? (config-from smtp-env false config/receipt-mail-config))))
  (testing "switch on, no ALERT_TO: receipts on, alerts off"
    (is (nil? (config-from smtp-env true config/smtp-config)))
    (is (= {:host "h" :port 465 :user "mail@example.com" :pass "p"
            :from "FreeMemo <mail@example.com>" :copy-to nil :site nil}
           (config-from smtp-env true config/receipt-mail-config))))
  (testing "env names for every receipt key"
    (is (= ["Billing <b@example.com>" "owner@example.com" "freememo.net"]
           ((juxt :from :copy-to :site)
            (config-from (assoc smtp-env "RECEIPT_FROM" "Billing <b@example.com>"
                           "RECEIPT_COPY_TO" "owner@example.com" "RECEIPT_SITE" "freememo.net")
              true config/receipt-mail-config))))))

(deftest every-smtp-connection-is-bounded-and-picks-its-tls
  (let [conn (config/smtp-connection {:host "h" :port 465 :user "u" :pass "p"})]
    (is (= [true false] ((juxt :ssl :tls) conn)) "465 is implicit TLS")
    (is (every? pos? ((juxt :connectiontimeout :timeout :writetimeout) conn))
        "JavaMail waits forever without these"))
  (is (= [false true] ((juxt :ssl :tls) (config/smtp-connection {:host "h" :port 587 :user "u" :pass "p"})))
      "any other port is STARTTLS"))
