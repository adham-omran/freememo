(ns freememo.storage-copy-contract-test
  "The public cost explanation follows configured storage metering."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [freememo.config :as config]
            [freememo.db :as db]
            [freememo.landing-copy :as copy]
            [freememo.storage-meter :as meter]
            [freememo.toasts :as toasts]))

(deftest configured-price-and-disabled-metering-agree-with-public-copy
  (doseq [[credits? rate expected] [[false nil nil] [false 5 nil] [true nil nil]
                                   [true 1 "1 credit per GB-month"]
                                   [true 5 "5 credits per GB-month"]]]
    (with-redefs [config/credits-enabled? (constantly credits?)
                  meter/rate-iqd-per-gb-month (constantly rate)]
      (is (= (some? expected) (meter/metering-enabled?)))
      (is (= expected (meter/storage-rate-label)))))
  (let [text (apply str (mapcat identity copy/faq-items))]
    (is (str/includes? text "When hosted storage metering is enabled"))
    (is (str/includes? text "Settings shows the storage rate"))
    (is (str/includes? text "transcripts, extracts, and cards stay"))))

(deftest grace-behavior-only-applies-to-metered-video
  (let [grace (atom nil)
        writes (atom [])]
    (with-redefs [config/credits-enabled? (constantly true)
                  meter/rate-iqd-per-gb-month (constantly 5)
                  db/get-storage-meter-state (fn [_] {:credit_balance_iqd 0 :has_video true
                                                     :storage_grace_started_at @grace})
                  db/set-storage-grace! (fn [_ value] (swap! writes conj value) (reset! grace value))
                  db/storage-grace-started-at (fn [_] @grace)
                  toasts/push! (fn [& _])]
      (is (= :opened (#'meter/transition-grace! 1)))
      (is (meter/playback-blocked? 1))
      (is (nil? (#'meter/transition-grace! 1)))
      (is (= 1 (count @writes)))
      (with-redefs [db/get-storage-meter-state (fn [_] {:credit_balance_iqd 2 :has_video true
                                                     :storage_grace_started_at @grace})]
        (is (= :cleared (#'meter/transition-grace! 1))))
      (is (not (meter/playback-blocked? 1))))
    (with-redefs [config/credits-enabled? (constantly false)
                  db/storage-grace-started-at (fn [_] (throw (ex-info "Unexpected read" {})))]
      (is (not (meter/playback-blocked? 1))))))
