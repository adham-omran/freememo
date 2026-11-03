(ns freememo.assessment-generation-test
  "The two-call exam generator of plans/blueprint-first-exams.md: a stray plan
   is corrected, a bad quote is replaced and flagged, and a paid call never
   ends in nothing while one question can be served."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.assessment :as assessment]
            [freememo.cards :as cards]
            [freememo.credits :as credits]
            [freememo.exam-grid :as grid]
            [freememo.kg-llm :as llm]
            [freememo.openrouter :as openrouter]
            [freememo.reply-schema :as reply-schema]))

(def demand
  "Aggregate demand is the total spending on goods and services in an economy. When demand falls, firms cut output and lay off workers.")
(def multiplier
  "The multiplier means that one extra unit of government spending raises total income by more than one unit. Each round of spending becomes another person's income.")
(def liquidity
  "In a liquidity trap, lower interest rates fail to raise investment because people prefer to hold money. Monetary policy then loses its force.")

(def evidence
  {:materials [{:material-id 7 :title "Keynes"
                :evidence [{:topic-id 1 :text demand} {:topic-id 2 :text multiplier} {:topic-id 3 :text liquidity}]}]
   :facts []})

(def blueprint
  {:themes [{:id "t1" :title "Aggregate demand" :topic_ids [1] :weight 40}
            {:id "t2" :title "The multiplier" :topic_ids [2] :weight 30}
            {:id "t3" :title "Liquidity trap" :topic_ids [3] :weight 30}]
   :outcomes [{:id "o1" :statement "Explain how falling demand cuts output." :level "understand" :theme_ids ["t1"]}
              {:id "o2" :statement "Predict the effect of new spending on income." :level "apply" :theme_ids ["t2"]}
              {:id "o3" :statement "Evaluate monetary policy in a trap." :level "evaluate" :theme_ids ["t3"]}]
   :unassessed_outcomes []})

(defn- mcq [slot-id quote topic-id]
  {:slot_id slot-id :format "multiple-choice"
   :prompt (str "Question " slot-id ": what follows when demand falls?")
   :evidence [{:material_id 7 :topic_id topic-id :quote quote}]
   :options [{:id "A" :text "Firms cut output"} {:id "B" :text "Prices double"}
             {:id "C" :text "Wages rise"} {:id "D" :text "Exports stop"}]
   :correct_option_id "A"})

(defn- essay [slot-id format quote topic-id]
  {:slot_id slot-id :format format
   :prompt (str "Essay " slot-id ": explain the multiplier.")
   :evidence [{:material_id 7 :topic_id topic-id :quote quote}]
   :grading_guide "Spending becomes income."
   :rubric [{:criterion "reasoning" :expectation "Trace each round of spending."}]})

(defn- answers-for
  "One good question per slot of the mixed grid at minutes."
  [minutes]
  (vec (for [{:keys [id format]} (grid/slots :mixed minutes)]
         (if (= format "multiple-choice")
           (mcq id "When demand falls, firms cut output and lay off workers." 1)
           (essay id format "Each round of spending becomes another person's income." 2)))))

(defn- generate
  "Run generate! with the two replies, and return the result or the thrown ex-data."
  ([blueprint-reply questions-reply] (generate blueprint-reply questions-reply {:target-minutes 20}))
  ([blueprint-reply questions-reply options]
   (let [calls (atom [])]
     (with-redefs [llm/resolve-model+gate! (fn [& _] {:api-key "key" :entry {:id "model"} :model-fields {}})
                   cards/load-prompt-template (constantly "prompt")
                   llm/chat! (fn [_ _ input contract _]
                               (swap! calls conj [(:name contract) (json/parse-string input true)])
                               {:parsed (if (= "assessment_blueprint" (:name contract)) blueprint-reply questions-reply)
                                :cost 0.1})
                   credits/record-cost-charge! (fn [& _] 2)]
       (try {:result (assessment/generate! 1 evidence options) :calls @calls}
            (catch clojure.lang.ExceptionInfo e {:error (ex-data e) :message (ex-message e) :calls @calls}))))))

(defn- with-slots [bp minutes]
  (assoc bp :slots (vec (map-indexed (fn [i {:keys [id format]}]
                                       {:id id :format format :role nil
                                        :level (if (= format "multiple-choice") "understand" "apply")
                                        :outcome_id (["o1" "o2" "o3"] (mod i 3))})
                          (grid/slots :mixed minutes)))))

(deftest a-good-exam-fills-the-grid-and-charges-both-calls
  (let [{:keys [result calls]} (generate (with-slots blueprint 20) (answers-for 20))
        bp (:blueprint result)]
    (is (= ["assessment_blueprint" "assessment_questions"] (map first calls)))
    (is (= (count (grid/slots :mixed 20)) (count (:questions result))))
    (is (= 4 (:charged-credits result)) "two calls at 2 credits each")
    (is (empty? (:dropped_questions bp)))
    (is (empty? (:plan_warnings bp)))
    (is (= 20 (:target_minutes bp)))
    (is (= 20 (:estimated_minutes bp)))
    (is (= #{"fill-in-the-blank" "long-essay"} (set (map :format (:omitted_formats bp)))))
    (is (every? #(= [] (:flags %)) (:questions result)))
    (testing "every question carries its slot's points and level"
      (is (= (grid/slot-points (grid/slots :mixed 20)) (reduce + (map :points (:questions result)))))
      (is (every? :level (:questions result))))
    (testing "the second call sees the plan's slots with their outcomes"
      (let [slots (get-in (second calls) [1 :slots])]
        (is (= (count (grid/slots :mixed 20)) (count slots)))
        (is (every? :outcome slots))))))

(deftest a-stray-plan-is-corrected-and-reported-never-refused
  (let [stray (-> blueprint
                  (assoc :themes [(first (:themes blueprint))])
                  (assoc :slots [{:id "s1" :format "multiple-choice" :level "remember" :outcome_id "nope"}
                                 {:id "s2" :format "multiple-choice" :level "remember" :outcome_id "o1"}
                                 {:id "s99" :format "long-essay" :level "create" :outcome_id "o1"}]))
        {:keys [result]} (generate stray (answers-for 20))
        bp (:blueprint result)
        warnings (str/join " " (:plan_warnings bp))]
    (is (= (count (grid/slots :mixed 20)) (count (:questions result))) "every grid slot is still written")
    (is (str/includes? warnings "1 themes"))
    (is (str/includes? warnings "Slot s1 named no outcome"))
    (testing "recall stays at a fifth of the points"
      (is (<= (reduce + (map :points (filter #(= "remember" (:level %)) (:questions result))))
              (* 0.2 (reduce + (map :points (:questions result)))))))
    (testing "an essay slot with no valid level takes apply"
      (is (= "apply" (:level (some #(when (= "short-essay" (:format %)) %) (:questions result))))))))

(deftest a-paraphrased-quote-is-repaired-silently
  (let [answers (assoc-in (answers-for 20) [0 :evidence 0 :quote]
                  "When demand falls firms cut output and lay off the workers.")
        {:keys [result]} (generate (with-slots blueprint 20) answers)
        q (first (:questions result))]
    (is (= "When demand falls, firms cut output and lay off workers." (get-in q [:evidence 0 :quote])))
    (is (= [] (:flags q)))
    (is (some? (get-in q [:evidence 0 :original_quote])))))

(deftest a-quote-from-the-wrong-section-is-found-in-its-real-one
  (let [answers (assoc-in (answers-for 20) [0 :evidence 0 :topic_id] 3)
        {:keys [result]} (generate (with-slots blueprint 20) answers)
        q (first (:questions result))]
    (is (= 1 (get-in q [:evidence 0 :topic_id])))
    (is (= [] (:flags q)))))

(deftest an-invented-quote-keeps-its-question-and-flags-it
  (let [answers (assoc-in (answers-for 20) [0 :evidence 0 :quote]
                  "Keynes wrote the General Theory in 1936 during a depression.")
        {:keys [result]} (generate (with-slots blueprint 20) answers)
        q (first (:questions result))]
    (is (= (count (grid/slots :mixed 20)) (count (:questions result))))
    (is (= ["quote-replaced"] (map :kind (:flags q))))
    (testing "the saved quote is still a passage of the source"
      (is (some #(str/includes? (assessment/normalize-text %) (get-in q [:evidence 0 :quote]))
                [demand multiplier liquidity])))))

(deftest a-broken-question-is-dropped-and-the-rest-are-saved
  (let [answers (-> (answers-for 20)
                    (assoc-in [0 :options] [{:id "A" :text "Same"} {:id "B" :text "Same"}])
                    (update 1 dissoc :prompt)
                    (assoc-in [2 :evidence 0 :quote] "zzz qqq")
                    (pop))
        {:keys [result]} (generate (with-slots blueprint 20) answers)
        dropped (get-in result [:blueprint :dropped_questions])]
    (is (= (- (count (grid/slots :mixed 20)) 4) (count (:questions result))))
    (is (= [1 2 3 13] (mapv :question dropped)))
    (is (every? :slot_id dropped))
    (is (every? :reason dropped))
    (is (< (get-in result [:blueprint :estimated_minutes]) 20))))

(deftest refusals-remain-only-for-no-source-and-no-question
  (testing "the model says the source cannot support an exam"
    (let [{:keys [error calls]} (generate {:insufficient true :reason "Too thin."} [])]
      (is (= ::assessment/insufficient-evidence (:type error)))
      (is (= 2 (:charged-credits error)) "the paid call is still reported")
      (is (= 1 (count calls)))))
  (testing "no question survives"
    (let [{:keys [error]} (generate (with-slots blueprint 20) [])]
      (is (= ::assessment/invalid-assessment (:type error)))
      (is (= 4 (:charged-credits error))))))

(deftest the-minutes-run-from-5-to-90
  (is (= :target-minutes (:field (:error (generate blueprint [] {:target-minutes 91}))))))

(deftest the-essay-grid-keeps-its-roles-and-levels
  (let [slots (grid/slots :essay 30)
        bp (assoc blueprint :slots (mapv #(assoc (select-keys % [:id :format :role]) :level "remember" :outcome_id "o2") slots))
        answers (mapv #(essay (:id %) (:format %) "Each round of spending becomes another person's income." 2) slots)
        {:keys [result]} (generate bp answers {:target-minutes 30 :grid :essay})]
    (is (= (mapv :level slots) (mapv :level (:questions result))))
    (is (= (mapv :role slots) (mapv :role (:questions result))))
    (is (some #(and (= "multiple-choice" (:format %)) (str/includes? (:reason %) "essay questions only"))
              (get-in result [:blueprint :omitted_formats])))))

(deftest both-calls-ask-for-their-schemas-through-the-real-chat
  (let [requests (atom [])]
    (with-redefs [llm/resolve-model+gate! (fn [& _] {:api-key "key"
                                                     :entry {:id "model" :supported-parameters
                                                             #{:response_format :structured_outputs}}
                                                     :model-fields {:response_format {:type "json_object"}}})
                  cards/load-prompt-template (constantly "prompt")
                  openrouter/chat-completion! (fn [_ body & _]
                                                (swap! requests conj body)
                                                (let [name (get-in body [:response_format :json_schema :name])
                                                      content (if (= "assessment_blueprint" name)
                                                                {:blueprint (with-slots blueprint 20)}
                                                                {:questions (answers-for 20)})]
                                                  {:choices [{:message {:content (json/generate-string content)}
                                                              :finish_reason "stop"}]
                                                   :usage {:cost 0.1}}))
                  credits/record-cost-charge! (fn [& _] 1)]
      (let [result (assessment/generate! 1 evidence {:target-minutes 20})
            [first-request second-request] @requests
            input (json/parse-string (:content (second (:messages first-request))))]
        (is (= (count (grid/slots :mixed 20)) (count (:questions result))))
        (is (= (reply-schema/wire-schema assessment/blueprint-reply)
               (get-in first-request [:response_format :json_schema :schema])))
        (is (= (reply-schema/wire-schema assessment/questions-reply)
               (get-in second-request [:response_format :json_schema :schema])))
        (is (= "s1" (get-in input ["grid" "slots" 0 "id"])))
        (is (= 7 (get-in input ["source" "materials" 0 "material_id"])))))))

(deftest word-similarity-is-an-ordered-overlap
  (is (= 1.0 (assessment/word-similarity ["a" "b"] ["a" "b"])))
  (is (= 0.0 (assessment/word-similarity ["a"] ["b"])))
  (is (= 0.5 (assessment/word-similarity ["a" "b"] ["b" "c"]))))
