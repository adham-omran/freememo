(ns freememo.run-outcomes-test
  "A background run keeps its error or its summary where the Activity reporter
   reads it, instead of pushing a toast (plans/remove-toasts.md 1.1.5, 1.1.6,
   2.4.2). No database and no network: every check runs against the per-user
   atoms and pure fns."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [freememo.activity :as activity]
            [freememo.activity-registry :as registry]
            [freememo.commands :as commands]
            [freememo.kg-extract :as kg]
            [freememo.kg-questions :as kgq]
            [freememo.knowledge-bulk :as bulk]
            [freememo.transcribe :as transcribe]
            [freememo.user-state :as us]))

(def ^:private uid 987650001)

(defn- reset-state! []
  (reset! (us/get-atom uid :run-errors) {})
  (reset! (us/get-atom uid :run-summaries) {})
  (reset! (us/get-atom uid :transcribing-topics) #{}))

(use-fixtures :each (fn [t] (reset-state!) (t) (reset-state!)))

(deftest the-four-silent-types-now-publish-an-error
  (doseq [type [:transcribe :distill-doc :distill-repo :generate-questions]]
    (testing (name type)
      (is (nil? (registry/work-error type uid "7")) "nothing before a run fails")
      (registry/record-error! uid type 7 {:success false :error "model said no"})
      (is (= "model said no" (:error (registry/work-error type uid "7"))))
      (is (= "model said no" (activity/error-message (registry/work-error type uid "7"))))
      (registry/clear-run! uid type 7)
      (is (nil? (registry/work-error type uid "7")) "the next run starts clean"))))

(deftest a-credit-refusal-keeps-its-kind
  (registry/record-error! uid :distill-doc 7
    {:success false :error "Out of credits" :error-type :insufficient-credits})
  (is (= "insufficient-credits"
        (activity/error-kind (registry/work-error :distill-doc uid "7")))))

(deftest a-blank-error-still-closes-as-a-failure-with-words
  (registry/record-error! uid :transcribe 7 {:success false :error nil})
  (is (= "The run failed." (activity/error-message (registry/work-error :transcribe uid "7")))))

(deftest the-types-with-a-count-publish-a-summary
  (doseq [type [:distill-doc :distill-repo :generate-questions :card-gen :repo-import]]
    (testing (name type)
      (is (nil? (registry/work-summary type uid "7")))
      (registry/record-summary! uid type 7 "3 things")
      (is (= "3 things" (registry/work-summary type uid "7"))))))

(deftest only-a-run-a-command-can-repeat-offers-retry
  (is (= {:command :distill-doc :payload {:topic-id 7}} (registry/work-retry :distill-doc "7")))
  (is (= {:command :generate-questions :payload {:topic-id 7}}
        (registry/work-retry :generate-questions "7")))
  (is (= {:command :transcribe :payload {:topic-id 7}} (registry/work-retry :transcribe "7")))
  (is (nil? (registry/work-retry :distill-repo "7"))
    "the repo's directory is deleted when the run ends")
  (is (nil? (registry/work-retry :page-ocr "7/1"))))

(deftest a-distill-summary-holds-counts-and-never-a-failure
  (let [text (kg/distill-summary {:facts 4 :pages 3 :skipped 1 :duplicates 2
                                  :page-errors 2 :at-cap 1})]
    (is (re-find #"4 facts added" text))
    (is (re-find #"2 marked duplicate" text))
    (is (not (re-find #"failed|given up" text))))
  (is (= "Nothing to extract" (kg/distill-summary {})))
  (is (re-find #"Nothing to do" (kg/distill-summary {:facts 0 :pages 0 :skipped 5}))))

(deftest failed-pages-go-to-the-banner-text
  (is (nil? (kg/distill-failure-text {:page-errors 0 :at-cap 0})))
  (let [text (kg/distill-failure-text {:page-errors 2 :at-cap 1})]
    (is (re-find #"2 pages failed" text))
    (is (re-find #"1 page given up on" text))))

(deftest a-question-summary-holds-counts-and-failed-batches-go-to-the-banner
  (let [r {:questions 12 :omitted 3 :batch-errors 2}]
    (is (= "12 questions generated. 3 facts skipped because too few facts set them apart"
          (kgq/generation-summary r)))
    (is (not (re-find #"batch" (kgq/generation-summary r))))
    (is (= "Generating questions: 2 batches failed." (kgq/generation-failure-text r))))
  (is (nil? (kgq/generation-failure-text {:questions 1 :batch-errors 0}))))

(deftest a-failed-transcription-keeps-its-error-before-membership-drops
  (let [membership-at-record (atom nil)]
    (with-redefs [transcribe/transcribe-topic! (fn [_ _] {:success false :error "no audio"})
                  commands/bump! (fn [& _])
                  us/work-started! (fn [& _])
                  registry/record-error!
                  (fn [u type k e]
                    (reset! membership-at-record
                      (contains? @(us/get-atom u :transcribing-topics) k))
                    (swap! (us/get-atom u :run-errors) assoc [type (str k)] e))]
      (transcribe/start-transcribe! uid 55)
      (let [deadline (+ (System/currentTimeMillis) 5000)]
        (while (and (nil? @membership-at-record) (< (System/currentTimeMillis) deadline))
          (Thread/sleep 20)))
      (is (true? @membership-at-record)
        "the reporter must find the error when it sees the work gone")
      (is (= {:success false :error "no audio"}
            (get @(us/get-atom uid :run-errors) [:transcribe "55"]))))))

(deftest a-bulk-failure-reads-as-words-and-a-success-as-nothing
  (is (nil? (bulk/failure-text {:success true :started 1})))
  (is (= "Out of credits"
        (bulk/failure-text {:success false :error "Out of credits"
                            :error-type :insufficient-credits})))
  (is (= "That action failed." (bulk/failure-text {:success false}))))
