(ns freememo.outreach-test
  "Pure checks on the outreach campaigns: bodies, idempotency keys and recipient
   lists. No database and no SMTP — `send-campaign!` is exercised only on its
   open-items refusal, which returns before either is touched.
   See plans/group-outreach-2026-10.md."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.outreach :as outreach])
  (:import (java.time LocalDateTime)))

(def ^:private addressee
  {:display-name "Ada" :signup-month "July"})

(def ^:private october-campaign-ids
  [:active-study-session :new-getting-started :lapsed-why-stopped])

(deftest every-body-greets-links-and-ends-with-the-footer
  (doseq [[id {:keys [body]}] outreach/campaigns]
    (testing (name id)
      (let [text (body addressee)]
        (is (str/starts-with? text "Hey Ada,\n"))
        (is (str/includes? text outreach/booking-link-url))
        (is (str/includes? text "You're receiving this because you signed up for FreeMemo."))))))

(deftest the-lapsed-body-names-the-signup-month
  (is (str/includes? (outreach/why-stopped-body addressee)
        "You tried FreeMemo in July and haven't been back.")))

(deftest the-booking-link-body-is-the-text-already-sent
  ;; 24 users received this exact text on 2026-07-29.
  (is (str/starts-with? (outreach/booking-link-body addressee)
        (str "Hey Ada,\n\n"
          "Quick update, I've moved to a new, permanent booking link so this one "
          "won't change on you again: https://adham-omran.com/meet\n\n"
          "The 2,500 credit offer for hopping on a call still stands if you're "
          "interested.\n\nBest, Adham\n\n---\n"))))

(deftest event-types-are-unique-per-campaign
  ;; The event type is the idempotency key; a shared one would make a later
  ;; campaign skip everyone an earlier one reached.
  (let [types (map :event-type (vals outreach/campaigns))]
    (is (= (count types) (count (set types))))
    (is (= "outreach_booking_link_v1"
          (get-in outreach/campaigns [:booking-link :event-type])))))

(deftest october-campaigns-share-no-recipient
  (let [ids (mapcat #(get-in outreach/campaigns [% :recipient-ids])
              october-campaign-ids)]
    (is (= (count ids) (count (set ids))))
    (is (= 30 (count ids)))))

(deftest month-name-reads-both-timestamp-types
  (let [t (LocalDateTime/of 2026 9 18 20 42)]
    (is (= "September" (outreach/month-name t)))
    (is (= "September" (outreach/month-name (java.sql.Timestamp/valueOf t))))))

(deftest a-campaign-with-open-items-sends-nothing
  (doseq [id october-campaign-ids]
    (let [r (outreach/send-campaign! id)]
      (is (= :open-items (:reason r)))
      (is (zero? (:sent r))))))

(deftest an-unknown-campaign-id-throws
  (is (thrown? clojure.lang.ExceptionInfo (outreach/send-campaign! :no-such-campaign)))
  (is (thrown? clojure.lang.ExceptionInfo
        (outreach/send-campaigns! {:campaigns [:lapsed-why-stopped :no-such-campaign]}))))
