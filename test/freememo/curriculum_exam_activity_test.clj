(ns freememo.curriculum-exam-activity-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [freememo.activity-registry :as registry]
   [freememo.cards :as cards]
   [freememo.credits :as credits]
   [freememo.curriculum :as curriculum]
   [freememo.curriculum-exam :as exam]
   [freememo.kg-llm :as llm]
   [freememo.user-state :as us])
  (:import
   [java.util.concurrent Executors]))

(defn- wait-until
  [predicate timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond
        (predicate) true
        (< (System/currentTimeMillis) deadline)
        (do (Thread/sleep 10) (recur))
        :else false))))

(deftest exam-form-generation-outlives-the-start-request
  (let [start-var (ns-resolve 'freememo.curriculum-exam 'start-form-generation!)
        user-id 987654321
        subject {:block-id 42}
        work-key "block/42"
        release (promise)]
    (testing "the server owns the long-running generation task"
      (is start-var "start-form-generation! must start a detached server task")
      (when start-var
        (with-redefs [exam/generate-form!
                      (fn [_user-id _subject _form-kind _threshold]
                        @release
                        {:success true :form-id 99})]
          (let [started-at (System/nanoTime)
                response (start-var user-id subject "standard" 70)
                elapsed-ms (/ (- (System/nanoTime) started-at) 1000000.0)]
            (is (:success response))
            (is (< elapsed-ms 250.0) "the start request must not await the model")
            (is (contains? @(us/get-atom user-id :exam-form-generations) work-key))
            (is (fn? (registry/owner-cancel-fn
                       :exam-form-generation user-id work-key)))
            (is (= "generating" (:phase
                                  (registry/read-state
                                    :exam-form-generation user-id work-key))))
            (deliver release :continue)
            (is (wait-until
                  #(not (contains?
                          @(us/get-atom user-id :exam-form-generations) work-key))
                  1000)
              "the server must clear activity state after completion")))))))

(deftest exam-form-generation-is-cancellable-from-the-activity-manager
  (let [user-id 987654322
        subject {:material-id 43}
        work-key "material/43"
        release (promise)]
    (with-redefs [exam/generate-form!
                  (fn [_user-id _subject _form-kind _threshold]
                    @release
                    {:success true :form-id 100})]
      (is (:success (exam/start-form-generation! user-id subject "standard" 70)))
      (let [cancel (registry/owner-cancel-fn
                     :exam-form-generation user-id work-key)]
        (is (fn? cancel))
        (cancel)
        (is (contains?
              @(us/get-atom user-id :exam-form-generation-cancelled) work-key))
        (is (wait-until
              #(not (contains?
                      @(us/get-atom user-id :exam-form-generations) work-key))
              1000))
        (is (not (contains?
                   @(us/get-atom user-id :exam-form-generation-errors) work-key)))
        (is (empty? @(us/get-atom user-id :toasts))))
      (deliver release :continue))))

(deftest form-generation-shutdown-waits-for-running-work
  (let [drain (ns-resolve 'freememo.curriculum-exam
                'drain-form-generation-executor!)
        executor (Executors/newSingleThreadExecutor)
        started (promise)
        release (promise)]
    (try
      (.submit executor
        ^Runnable (fn [] (deliver started true) @release))
      @started
      (is drain "the shutdown path needs a testable executor drain")
      (when drain
        (let [result (future (drain executor 1000))]
          (Thread/sleep 50)
          (is (not (realized? result))
            "shutdown must wait while accepted work is still running")
          (deliver release true)
          (is (= :drained (deref result 1000 :test-timeout)))))
      (finally
        (deliver release true)
        (.shutdownNow executor)))))

(deftest production-shutdown-gives-exam-generation-time-to-finish
  (let [compose (slurp "docker-compose.prod.yml")
        prod-source (slurp "src-prod/prod.cljc")]
    (is (str/includes? compose "stop_grace_period: 21m")
      "Docker must wait longer than the 20-minute form-generation timeout")
    (is (str/includes? prod-source
          "(curriculum-exam/await-form-generation-shutdown!)")
      "the production JVM must drain accepted form-generation work")))

(def exam-rubric
  {:factual_accuracy "Facts match the evidence."
   :reasoning "The claim follows from the evidence."
   :source_evidence "The answer uses the evidence."
   :integration "The answer connects relevant points."
   :completeness "The answer covers the prompt."})

(defn- exam-question
  ([index]
   (exam-question index "frozen evidence"))
  ([index quote]
   {:role (nth ["explain" "compare" "apply" "evaluate" "synthesize"] index)
    :prompt (str "Question " index)
    :grading_guide "Use the cited evidence."
    :evidence [{:material_id 43 :topic_id 44 :quote quote}]
    :rubric exam-rubric}))

(deftest citation-validation-accepts-formatting-only-differences
  (let [user-id 987654324
        subject {:material-id 43}
        saves (atom 0)
        source "Frozen\u00a0evidence\nsays \u201cspaced \u2014 text\u201d\u2026"
        quote "Frozen evidence says \"spaced - text\"..."
        generated-form
        {:blueprint {:assessed_outcomes ["Outcome"] :unassessed_outcomes []}
         :questions (mapv #(exam-question % quote) (range 5))}]
    (with-redefs [curriculum/subject-evidence
                  (fn [_connectable _user-id _subject]
                    {:materials [{:material-id 43
                                  :topic-id 44
                                  :digest "digest"
                                  :evidence [{:topic-id 44
                                              :title "Evidence"
                                              :kind "markdown"
                                              :page nil
                                              :text source}]}]
                     :source-versions ["digest"]})
                  llm/resolve-model+gate!
                  (fn [_user-id _step]
                    {:api-key "test" :entry {:id "test-model"} :model-fields {}})
                  llm/chat!
                  (fn [& _args] {:parsed generated-form :cost 0.01})
                  cards/load-prompt-template (fn [_name] "prompt")
                  curriculum/save-draft-form!
                  (fn [& _args] (swap! saves inc) 102)
                  curriculum/record-form-generation-charge! (fn [& _args] nil)
                  credits/record-cost-charge! (fn [& _args] 1)]
      (let [result (exam/generate-form! user-id subject "standard" 70)]
        (is (:success result))
        (is (= 1 @saves))))))

(deftest citation-validation-remains-bound-to-the-declared-source
  (let [validate (ns-resolve 'freememo.curriculum-exam 'validate-citations!)
        materials [{:material-id 43
                    :evidence [{:topic-id 44 :text "The source makes one claim."}]}]
        question (exam-question 0 "The source makes one claim.")]
    (testing "a paraphrase remains invalid"
      (is (thrown-with-msg?
            clojure.lang.ExceptionInfo
            #"not in the frozen source"
            (validate materials
              [(assoc-in question [:evidence 0 :quote]
                 "The material presents a single assertion.")]))))
    (testing "an exact quote under the wrong topic remains invalid"
      (is (thrown-with-msg?
            clojure.lang.ExceptionInfo
            #"not in the frozen source"
            (validate materials
              [(assoc-in question [:evidence 0 :topic_id] 45)]))))))

(deftest failed-form-validation-records-the-completed-model-cost
  (let [user-id 987654325
        subject {:material-id 43}
        charges (atom [])
        saves (atom 0)
        generated-form
        {:blueprint {:assessed_outcomes ["Outcome"] :unassessed_outcomes []}
         :questions (mapv #(exam-question % "invented evidence") (range 5))}]
    (with-redefs [curriculum/subject-evidence
                  (fn [_connectable _user-id _subject]
                    {:materials [{:material-id 43
                                  :topic-id 44
                                  :digest "digest"
                                  :evidence [{:topic-id 44 :text "frozen evidence"}]}]
                     :source-versions ["digest"]})
                  llm/resolve-model+gate!
                  (fn [_user-id _step]
                    {:api-key "test" :entry {:id "test-model"} :model-fields {}})
                  llm/chat!
                  (fn [& _args] {:parsed generated-form :cost 0.25
                                 :tokens [{:input-tokens 900 :output-tokens 40}]})
                  cards/load-prompt-template (fn [_name] "prompt")
                  curriculum/save-draft-form!
                  (fn [& _args] (swap! saves inc) 103)
                  credits/record-cost-charge!
                  (fn [& args] (swap! charges conj args) 12)]
      (let [result (exam/generate-form! user-id subject "standard" 70)]
        (is (false? (:success result)))
        (is (zero? @saves))
        (is (= [[user-id :curriculum.exam-form "test-model" 0.25
                 [{:input-tokens 900 :output-tokens 40}]]]
              @charges))))))

(deftest failed-grade-validation-records-the-completed-model-cost
  (let [user-id 987654326
        sitting-id 104
        charges (atom [])]
    (with-redefs [curriculum/sitting-for-grade
                  (fn [_user-id _sitting-id]
                    {:questions [] :blueprint {} :answers {} :threshold 70})
                  llm/resolve-model+gate!
                  (fn [_user-id _step]
                    {:api-key "test" :entry {:id "test-model"} :model-fields {}})
                  llm/chat! (fn [& _args] {:parsed {} :cost 0.5
                                           :tokens [{:input-tokens 700}]})
                  cards/load-prompt-template (fn [_name] "prompt")
                  credits/record-cost-charge!
                  (fn [& args] (swap! charges conj args) 24)]
      (let [result (exam/grade-sitting! user-id sitting-id)]
        (is (false? (:success result)))
        (is (= [[user-id :curriculum.exam-grade "test-model" 0.5
                 [{:input-tokens 700}]]]
              @charges))))))

(deftest cancelled-generation-does-not-save-a-late-form
  (let [user-id 987654323
        subject {:material-id 43}
        work-key "material/43"
        saves (atom 0)
        charges (atom 0)
        generated-form
        {:blueprint {:assessed_outcomes ["Outcome"] :unassessed_outcomes []}
         :questions (mapv exam-question (range 5))}]
    (swap! (us/get-atom user-id :exam-form-generation-cancelled) conj work-key)
    (with-redefs [curriculum/subject-evidence
                  (fn [_connectable _user-id _subject]
                    {:materials [{:material-id 43
                                  :topic-id 44
                                  :digest "digest"
                                  :evidence [{:topic-id 44
                                              :title "Evidence"
                                              :kind "markdown"
                                              :page nil
                                              :text "frozen evidence"}]}]
                     :source-versions ["digest"]})
                  llm/resolve-model+gate!
                  (fn [_user-id _step]
                    {:api-key "test" :entry {:id "test-model"} :model-fields {}})
                  llm/chat!
                  (fn [& _args] {:parsed generated-form :cost 0.01})
                  cards/load-prompt-template (fn [_name] "prompt")
                  curriculum/save-draft-form!
                  (fn [& _args] (swap! saves inc) 101)
                  credits/record-cost-charge!
                  (fn [& _args] (swap! charges inc) 1)]
      (let [result (exam/generate-form! user-id subject "standard" 70)]
        (is (:cancelled? result))
        (is (zero? @saves) "a cancelled OpenRouter result must not save a form")
        (is (= 1 @charges) "the completed OpenRouter call still costs credits")))))
