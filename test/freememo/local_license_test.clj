(ns freememo.local-license-test
  "Unit tests for ticket 07 of plans/local-install.md: the reminder page, and
   the Settings text of each license state."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.desktop-section :as section]
            [freememo.local-license :as ll]))

(defn- app [_] {:status 200 :body "the app"})

(defn- handler [license]
  (ll/wrap-reminder app {:origin "https://freememo.test" :license-fn (constantly license)}))

(def ^:private get-root {:request-method :get :uri "/"})

(deftest the-reminder-comes-first-for-each-nag-state-and-only-once
  (doseq [state ["unlicensed" "expired" "grace"]]
    (let [h (handler {"state" state})]
      (testing state
        (is (str/includes? (:body (h get-root)) "Buy a Sync License"))
        (is (str/includes? (:body (h get-root)) "https://freememo.test/settings#settings-desktop"))
        (is (= 303 (:status (h {:request-method :post :uri ll/continue-path}))))
        (is (= "the app" (:body (h get-root))) "after Continue, the library opens")))))

(deftest no-reminder-without-a-nag-state
  (doseq [license [nil {"state" "trial"} {"state" "licensed"}]]
    (is (= "the app" (:body ((handler license) get-root))))))

(deftest only-the-first-page-is-held
  (let [h (handler {"state" "unlicensed"})]
    (is (= "the app" (:body (h {:request-method :get :uri "/api/desktop/status"}))))))

(deftest settings-has-its-own-text-for-each-state
  (let [texts (map section/license-text
                [{"state" "trial" "trial-days-left" 12 "trial-ends" "2026-12-01T00:00:00Z"}
                 {"state" "licensed" "covered-majors" [[1 2]]}
                 {"state" "grace" "grace-ends" "2026-12-09T00:00:00Z"}
                 {"state" "expired"}
                 {"state" "unlicensed"}])]
    (is (= 5 (count (set texts))))
    (is (str/includes? (first texts) "12 days left"))
    (is (str/includes? (nth texts 2) "2026-12-09"))))

(deftest sync-shows-its-refusal-reason
  (is (str/includes? (section/sync-text {:state :license-refused :license {"state" "unlicensed"}}) "trial has ended"))
  (is (str/includes? (section/sync-text {:state :license-refused
                                         :license {"state" "expired" "grace-ends" "2026-12-09T00:00:00Z"}})
        "grace period ended on 2026-12-09")))
