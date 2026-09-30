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
  (reset! (us/get-atom user-id :toasts) [])
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

(def ^:private settle-run! @#'repo-import/settle-run!)

(defn- seed-run! [stage-id]
  (swap! @#'repo-import/!import-runs assoc stage-id
    {:user-id user-id :delivery (promise)}))

(def ^:private success
  {:ok true :topic-id 42 :path "owner/name" :report {:topics-created 7}})

(defn- toasts [] @(us/get-atom user-id :toasts))

(deftest close-before-the-result-toasts-once-at-settle
  (seed-run! "s1")
  (repo-import/background-import! user-id "s1")
  (is (empty? (toasts)))
  (settle-run! "s1" success)
  (is (= 1 (count (toasts))))
  (is (= [{:label "Open" :nav [:viewer {:type :topic :topic-id 42}]}]
        (:actions (first (toasts))))))

(deftest close-after-the-result-toasts-once-at-close
  (seed-run! "s2")
  (settle-run! "s2" success)
  (is (empty? (toasts)) "the picker is still open and navigates instead")
  (repo-import/background-import! user-id "s2")
  (is (= 1 (count (toasts))))
  (repo-import/background-import! user-id "s2")
  (is (= 1 (count (toasts))) "a second Close does not toast again"))

(deftest a-run-nobody-closed-never-toasts
  (seed-run! "s3")
  (settle-run! "s3" success)
  (is (empty? (toasts)))
  (is (= success (deref (get-in @@#'repo-import/!import-runs ["s3" :delivery]) 0 nil))))

(deftest a-cancelled-run-toasts-nothing
  (seed-run! "s4")
  (repo-import/background-import! user-id "s4")
  (settle-run! "s4" {:ok false :cancelled? true})
  (is (empty? (toasts))))

(deftest another-users-close-is-ignored
  (seed-run! "s5")
  (repo-import/background-import! (inc user-id) "s5")
  (settle-run! "s5" success)
  (is (empty? (toasts))))
