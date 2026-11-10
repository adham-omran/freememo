(ns freememo.local-install-license-test
  "Seam 1 for ticket 07 of plans/local-install.md: the license reminder is the
   first page of a launch, and local use works in every license state.

   A stub plays freememo.net (`freememo.local-install-stub`) and answers the
   sync meta step with the license of the state under test. That is the
   \"clock past the trial\": the server decides the state from its clock, and
   the Local Install only reads the answer. The first start caches the license.
   The second start, on the same data directory, shows the reminder first."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.local-install-harness :as h]
            [freememo.local-install-stub :as stub]))

(defn- status [li cookie]
  (json/parse-string (:body (h/http (h/url li "/api/desktop/status") {:cookie cookie}))))

(defn- wait-for [ms f]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (or (try (f) (catch Exception _ nil))
          (when (< (System/currentTimeMillis) deadline)
            (Thread/sleep 500)
            (recur))))))

(defn- post-continue [li cookie]
  (h/http (h/url li "/license-reminder/continue")
    {:method :post :cookie cookie :headers {"Origin" (str "http://127.0.0.1:" (:port li))}}))

(deftest past-grace-the-reminder-comes-first-and-the-library-opens
  (let [license {"state" "expired" "grace-ends" "2026-12-09T00:00:00Z"}
        s (stub/start! {:meta-status 200 :meta-body {"license" license "sync" "none"}})
        li1 (h/start-local-install! {:origin (:origin s)})
        data-dir (:data-dir li1)]
    (try
      (let [cookie1 (:cookie (h/sign-in! li1))]
        (testing "the first start caches the license and sync shows its refusal"
          (is (wait-for 90000 #(= "expired" (get-in (status li1 cookie1) ["license" "state"]))))
          (is (wait-for 30000 #(= "license-refused" (get (status li1 cookie1) "state")))))
        (h/stop! li1))
      (let [li (h/start-local-install! {:data-dir data-dir :account nil :origin (:origin s)})
            cookie (:cookie (h/sign-in! li))]
        (try
          (testing "the first page of the launch is the reminder"
            (let [page (h/http (h/url li "/") {:cookie cookie})]
              (is (= 200 (:status page)))
              (is (str/includes? (:body page) "Sync License"))
              ;; `desktop-license/trial-open?` is true, so nothing is for sale: no buy link,
              ;; as in Settings.
              (is (not (str/includes? (:body page) "settings#settings-desktop")))))
          (testing "Continue opens the library, and the reminder does not come back"
            (is (= 303 (:status (post-continue li cookie))))
            (let [page (h/http (h/url li "/") {:cookie cookie})]
              (is (= 200 (:status page)))
              (is (not (str/includes? (:body page) "Buy a Sync License")))))
          (finally (when (h/alive? li) (h/stop! li)))))
      (finally
        (stub/stop! s)
        (h/delete-tree! data-dir)))))
