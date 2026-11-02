(ns freememo.activity-reporter-test
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [freememo.activity :as activity]
   [freememo.activity-registry :as registry]
   [freememo.activity-reporter :as reporter]
   [freememo.user-state :as us]))

(def foreign-exam-row
  {:id 449
   :user_id 8
   :type "exam-form-generation"
   :work_key "material/2"})

(defn- reset-local-activity-ownership! []
  (reset! (var-get
            (ns-resolve 'freememo.activity-reporter
              'locally-observed-activity-ids))
    #{}))

(use-fixtures :each
  (fn [test-fn]
    (reset-local-activity-ownership!)
    (try
      (test-fn)
      (finally (reset-local-activity-ownership!)))))

(deftest reporter-does-not-close-work-owned-by-another-process
  (let [terminations (atom [])]
    (with-redefs [us/users-with-state (constantly #{})
                  registry/server-tier-types
                  (constantly #{"exam-form-generation"})
                  activity/live-rows-for-tick (fn [_types] [foreign-exam-row])
                  registry/read-state (fn [& _args] nil)
                  activity/terminate!
                  (fn [& args] (swap! terminations conj args))]
      (is (= {:opened 0 :refreshed 0 :closed 0}
            (reporter/tick-once!)))
      (is (empty? @terminations)
        "a foreign row must become interrupted through staleness"))))

(deftest reporter-closes-work-that-this-process-observed
  (let [state-reads (atom 0)
        terminations (atom [])]
    (with-redefs [us/users-with-state (constantly #{8})
                  registry/server-tier-types
                  (constantly #{"exam-form-generation"})
                  registry/live-work
                  (fn [_type _user-id]
                    [{:work-key "material/2" :subject-id nil}])
                  registry/read-state
                  (fn [& _args]
                    (when (= 1 (swap! state-reads inc))
                      {:phase "generating" :cancellable false}))
                  activity/start! (fn [& _args] 450)
                  activity/live-rows-for-tick
                  (fn [_types] [(assoc foreign-exam-row :id 450)])
                  registry/work-error (fn [& _args] nil)
                  activity/terminate!
                  (fn [& args] (swap! terminations conj args))]
      (is (= {:opened 1 :refreshed 0 :closed 1}
            (reporter/tick-once!)))
      (is (= [[8 450 "success" {:error nil :retry nil :summary nil}]] @terminations)))))

(deftest reporter-closes-with-the-owner-summary-unless-it-failed
  (doseq [[err summary expected]
          [[nil "12 sent to Anki" ["success" {:error nil :retry nil :summary "12 sent to Anki"}]]
           ["boom" "12 sent to Anki" ["failure" {:error "boom" :retry nil :summary nil}]]]]
    (reset-local-activity-ownership!)
    (let [state-reads (atom 0)
          terminations (atom [])]
      (with-redefs [us/users-with-state (constantly #{8})
                    registry/server-tier-types (constantly #{"anki-reconcile"})
                    registry/live-work
                    (fn [_type _user-id] [{:work-key "collection" :subject-id nil}])
                    registry/read-state
                    (fn [& _args]
                      (when (= 1 (swap! state-reads inc)) {:phase "writing"}))
                    activity/start! (fn [& _args] 452)
                    activity/live-rows-for-tick
                    (fn [_types] [{:id 452 :user_id 8 :type "anki-reconcile"
                                   :work_key "collection"}])
                    registry/work-error (fn [& _args] err)
                    registry/work-summary (fn [& _args] summary)
                    activity/terminate!
                    (fn [& args] (swap! terminations conj args))]
        (reporter/tick-once!)
        (is (= [(into [8 452] expected)] @terminations) (pr-str err))))))

(def anki-row
  {:id 451 :user_id 8 :type "anki-reconcile" :work_key "collection"})

(deftest reporter-leaves-a-stale-reading-for-staleness
  ;; An Anki sync whose driver stopped heartbeating still names its work in the
  ;; lease. The row must neither be refreshed (that would keep a dead run
  ;; looking alive) nor closed (that would record it as a success): leaving it
  ;; is what lets the staleness rule mark it interrupted.
  (let [reports (atom [])
        starts (atom [])
        terminations (atom [])]
    (swap! (var-get (ns-resolve 'freememo.activity-reporter
                      'locally-observed-activity-ids))
      conj 451)
    (with-redefs [us/users-with-state (constantly #{8})
                  registry/server-tier-types (constantly #{"anki-reconcile"})
                  registry/live-work
                  (fn [_type _user-id] [{:work-key "collection" :subject-id nil}])
                  registry/read-state
                  (fn [& _args] {:phase "writing" :stale? true :cancellable true})
                  activity/start! (fn [& args] (swap! starts conj args) 451)
                  activity/report! (fn [& args] (swap! reports conj args) :done)
                  activity/live-rows-for-tick (fn [_types] [anki-row])
                  registry/work-error (fn [& _args] nil)
                  activity/terminate! (fn [& args] (swap! terminations conj args))]
      (is (= {:opened 0 :refreshed 0 :closed 0} (reporter/tick-once!)))
      (is (empty? @starts) "discovery must not reopen a dead run")
      (is (empty? @reports) "a stale row must not be refreshed")
      (is (empty? @terminations) "a stale row must not be closed as a success"))))
