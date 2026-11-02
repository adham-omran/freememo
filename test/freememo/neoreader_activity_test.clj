(ns freememo.neoreader-activity-test
  "A NeoReader sync writes its outcome where the Activity reporter reads it
   (plans/remove-toasts.md 2.4.4). Needs no database."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [freememo.activity-registry :as registry]
   [freememo.api :as api]
   [freememo.user-state :as us]))

(def ^:private uid -4713)
(def ^:private topic 42)

(defn- clear! []
  (reset! (us/get-atom uid :neoreader-syncs) #{})
  (reset! (us/get-atom uid :neoreader-sync-errors) {})
  (reset! (us/get-atom uid :neoreader-sync-results) {}))

(use-fixtures :each (fn [f] (clear!) (try (f) (finally (clear!)))))

(def ^:private record! @#'api/record-neoreader-outcome!)

(deftest a-failed-sync-stores-its-error-for-the-row
  (record! uid topic {:ok false :error "Google Drive said no"})
  (is (= "Google Drive said no" (registry/work-error "neoreader-sync" uid (str topic))))
  (is (nil? (registry/work-summary "neoreader-sync" uid (str topic)))))

(deftest a-finished-sync-stores-its-summary-and-clears-the-error
  (swap! (us/get-atom uid :neoreader-sync-errors) assoc topic "old error")
  (record! uid topic {:ok true :kind "epub"
                      :counts {:placed 3 :fallback 0 :out-of-order 0
                               :partial 0 :unmatched 0}
                      :sections-written 2 :notes 0 :titles-resolved 0})
  (is (nil? (registry/work-error "neoreader-sync" uid (str topic))))
  (is (re-find #"^Marked 3 highlights" (registry/work-summary "neoreader-sync" uid (str topic)))))

(deftest a-pick-request-records-nothing
  (testing "the browser answers it at once, so there is nothing to report"
    (record! uid topic {:ok false :needs-pick? true :error "Choose the file"})
    (is (nil? (registry/work-error "neoreader-sync" uid (str topic))))
    (is (nil? (registry/work-summary "neoreader-sync" uid (str topic))))))
