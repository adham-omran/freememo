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
                 {"state" "licensed"}
                 {"state" "unlicensed"}])]
    (is (= 3 (count (set texts))))
    (is (str/includes? (first texts) "12 days left"))))

(deftest sync-shows-its-refusal-reason
  (is (str/includes? (section/sync-text {:state :license-refused :license {"state" "unlicensed"}}) "trial has ended"))
  (is (str/includes? (section/sync-text {:state :license-refused
                                         :license {"state" "expired" "grace-ends" "2026-12-09T00:00:00Z"}})
        "grace period ended on 2026-12-09")))

(deftest sync-text-differs-on-a-local-install
  (let [signed-out (section/sync-text {:state :signed-out})
        local-signed-out (section/sync-text {:state :signed-out :local-install? true})
        update (section/sync-text {:state :update-required})
        local-update (section/sync-text {:state :update-required :local-install? true})]
    (is (str/includes? signed-out "Quit FreeMemo"))
    (is (not (str/includes? local-signed-out "Quit FreeMemo")))
    (is (str/includes? local-signed-out "sign in again"))
    (is (str/includes? update "freememo.net"))
    (is (str/includes? local-update "Install the new version"))))

(deftest the-grace-reminder-shows-its-end-date
  (let [body (:body ((handler {"state" "grace" "grace-ends" "2026-12-09T00:00:00Z"}) get-root))]
    (is (str/includes? body "until 2026-12-09."))))

(deftest no-buy-link-while-the-license-is-not-for-sale
  (let [h (ll/wrap-reminder app {:origin "https://freememo.test" :buy? (constantly false)
                                 :license-fn (constantly {"state" "unlicensed"})})
        body (:body (h get-root))]
    (is (not (str/includes? body "Buy a Sync License")))
    (is (str/includes? body "Continue to my library"))))
