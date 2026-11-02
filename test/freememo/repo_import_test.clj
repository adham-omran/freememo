(ns freememo.repo-import-test
  "Cancel marks and the background-import hand-off
   (plans/repo-import-progress.md §1.2, §3.3). No database and no network: both
   run against the per-user atoms alone."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [freememo.repo-import :as repo-import]
            [freememo.user-state :as us]))

(def ^:private user-id 987654321)
(def ^:private work-key "gitlab.com/owner/name")

(defn- reset-state! []
  (doseq [k [:importing-repos :repo-import-errors :repo-import-cancellers]]
    (reset! (us/get-atom user-id k) {}))
  (reset! (us/get-atom user-id :repo-import-cancelled) #{})
  (reset! (us/get-atom user-id :run-summaries) {})
  (reset! (us/get-atom user-id :run-errors) {})
  (reset! @#'repo-import/!import-runs {}))

(use-fixtures :each (fn [t] (reset-state!) (t) (reset-state!)))

(def ^:private run-as-activity @#'repo-import/run-as-activity)

(defn- cancel! []
  ((get @(us/get-atom user-id :repo-import-cancellers) work-key)))

(deftest a-cancelled-run-is-not-a-failure
  (let [r (run-as-activity user-id work-key "downloading"
            (fn []
              (cancel!)
              (throw (ex-info "stopped at the next check" {}))))]
    (is (= {:ok false :cancelled? true} r))
    (is (empty? @(us/get-atom user-id :repo-import-errors))
      "the reporter would close the row as failed")))

(deftest a-run-leaves-no-mark-behind
  (run-as-activity user-id work-key "downloading"
    (fn [] (cancel!) (throw (ex-info "stopped" {}))))
  (testing "every atom the run published is clear"
    (is (empty? @(us/get-atom user-id :importing-repos)))
    (is (empty? @(us/get-atom user-id :repo-import-cancellers)))
    (is (empty? @(us/get-atom user-id :repo-import-cancelled))))
  (testing "the next run of the same repository is not cancelled by the old mark"
    (is (= {:ok true} (run-as-activity user-id work-key "downloading" (fn [] {:ok true}))))))

(deftest a-throw-without-a-cancel-is-still-a-failure
  (is (thrown? clojure.lang.ExceptionInfo
        (run-as-activity user-id work-key "downloading" (fn [] (throw (ex-info "boom" {}))))))
  (is (= "boom" (get @(us/get-atom user-id :repo-import-errors) work-key))))

;; ── Background hand-off ────────────────────────────────────────────
;; The picker's Close no longer pushes anything: the run's Activity row holds the
;; summary or the error (plans/remove-toasts.md 2.4.2, 2.6).

(def ^:private settle-run! @#'repo-import/settle-run!)

(defn- seed-run! [stage-id]
  (swap! @#'repo-import/!import-runs assoc stage-id
    {:user-id user-id :delivery (promise)}))

(def ^:private success
  {:ok true :topic-id 42 :path "owner/name" :report {:topics-created 7}})

(deftest a-settled-run-reaches-the-waiting-picker
  (seed-run! "s1")
  (settle-run! "s1" success)
  (is (= success (deref (get-in @@#'repo-import/!import-runs ["s1" :delivery]) 0 nil))))

(deftest a-successful-import-keeps-its-count-for-the-activity-row
  (run-as-activity user-id work-key "creating topics" (fn [] success))
  (is (= "Imported owner/name: 7 topics."
        (get @(us/get-atom user-id :run-summaries) [:repo-import work-key]))))

(deftest a-step-with-nothing-to-count-keeps-no-summary
  (run-as-activity user-id work-key "cloning" (fn [] {:ok true :dir "x"}))
  (is (nil? (get @(us/get-atom user-id :run-summaries) [:repo-import work-key]))))

(deftest an-unchanged-update-says-so
  (run-as-activity user-id work-key "fetching" (fn [] {:ok true :unchanged? true :topic-id 1}))
  (is (= (str work-key " is already up to date.")
        (get @(us/get-atom user-id :run-summaries) [:repo-import work-key]))))

(deftest a-failed-run-keeps-its-error-and-no-summary
  (run-as-activity user-id work-key "creating topics" (fn [] success))
  (run-as-activity user-id work-key "creating topics" (fn [] {:ok false :error "disk full"}))
  (is (= "disk full" (get @(us/get-atom user-id :repo-import-errors) work-key)))
  (is (nil? (get @(us/get-atom user-id :run-summaries) [:repo-import work-key]))))
