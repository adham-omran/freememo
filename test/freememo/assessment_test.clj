(ns freememo.assessment-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.assessment :as assessment]
            [freememo.cards :as cards]
            [freememo.credits :as credits]
            [freememo.kg-llm :as llm]
            [freememo.openrouter :as openrouter]
            [freememo.reply-schema :as reply-schema]))

(def citation {:material_id 7 :topic_id 9 :quote "Plants use light to make sugar."})
(def evidence {:materials [{:material-id 7 :evidence [{:topic-id 9 :text (:quote citation)}]}]
               :facts [{:id 13}]})
(def questions
  [{:id "blank" :format "fill-in-the-blank" :points 10 :prompt "Plants use ____ to make sugar."
    :case_sensitive false :accepted_answers ["light" "sunlight"] :evidence [citation] :fact_ids [13 99]}
   {:id "choice" :format "multiple-choice" :points 30 :prompt "What provides the energy?"
    :options [{:id "A" :text "Light"} {:id "B" :text "Sugar"}] :correct_option_id "A" :evidence [citation]}
   {:id "essay" :format "short-essay" :points 60 :prompt "Explain the link between light and sugar."
    :rubric {:reasoning "Connect light and sugar."} :evidence [citation] :fact_ids [13]}])
(def form {:blueprint {:assessed_outcomes [{:id "photosynthesis" :statement "Connect light with sugar production."
                                           :question_ids ["blank" "choice" "essay"] :evidence [citation]}]
                       :unassessed_outcomes [{:statement "Compare light intensities." :reason "The source has no intensity evidence."}]
                       :omitted_formats [{:format "long-essay" :reason "The source is too short."}]}
           :questions questions})

(deftest mixed-form-freezes-sources-and-filters-fact-links
  (let [validated (assessment/validate-form! form evidence {:target-minutes 15})]
    (is (= 3 (count (:questions validated))))
    (is (= [13] (get-in validated [:questions 0 :fact_ids])))
    (is (= (:materials evidence) (get-in validated [:blueprint :evidence_packets])))
    (is (= 15 (get-in validated [:blueprint :target_minutes])))))

(deftest unsupported-compositions-and-false-citations-fail
  ;; Strict mode, the curriculum exam's contract: any defect refuses the form.
  (doseq [bad [(assoc form :questions [])
               (assoc-in form [:questions 0 :accepted_answers] [])
               (assoc-in form [:questions 1 :correct_option_id] "C")
               (assoc-in form [:questions 0 :evidence 0 :topic_id] 10)
               (assoc-in form [:blueprint :omitted_formats] [])
               (assoc-in form [:blueprint :assessed_outcomes 0 :question_ids] ["other"])]]
    (is (thrown? clojure.lang.ExceptionInfo (assessment/validate-form! bad evidence {})))))

(defn- salvage [bad]
  (assessment/validate-form! bad evidence {:drop-invalid? true :target-minutes 30}))

(deftest drop-mode-keeps-the-questions-that-pass
  (testing "a bad question is removed and named, and the rest survive"
    (let [validated (salvage (-> form
                                 (assoc-in [:blueprint :estimated_minutes] 20)
                                 (assoc-in [:questions 0 :accepted_answers] [])))]
      (is (= ["choice" "essay"] (mapv :id (:questions validated))))
      (is (= [{:question 1 :format "fill-in-the-blank" :prompt "Plants use ____ to make sugar."
               :reason "A fill-in-the-blank question has no accepted answers."}]
            (get-in validated [:blueprint :dropped_questions])))
      (is (= ["choice" "essay"] (get-in validated [:blueprint :assessed_outcomes 0 :question_ids])))
      (is (= 18 (get-in validated [:blueprint :estimated_minutes])) "90 of 100 points remain")
      (is (= {"fill-in-the-blank" "The questions in this format did not pass the checks."
              "long-essay" "The source is too short."}
            (into {} (map (juxt :format :reason)) (get-in validated [:blueprint :omitted_formats]))))))
  (testing "a false quote removes only its own question"
    (let [validated (salvage (assoc-in form [:questions 2 :evidence 0 :quote] "Plants eat sugar."))]
      (is (= ["blank" "choice"] (mapv :id (:questions validated))))
      (is (= [3] (mapv :question (get-in validated [:blueprint :dropped_questions]))))))
  (testing "a repeated prompt keeps its first question"
    (let [validated (salvage (assoc-in form [:questions 2 :prompt] "What provides the energy?"))]
      (is (= ["blank" "choice"] (mapv :id (:questions validated))))
      (is (= "Two questions have the same ID or the same text."
            (get-in validated [:blueprint :dropped_questions 0 :reason])))))
  (testing "a clean form loses nothing and keeps the model's estimate"
    (let [validated (salvage (assoc-in form [:blueprint :estimated_minutes] 20))]
      (is (= 3 (count (:questions validated))))
      (is (= [] (get-in validated [:blueprint :dropped_questions])))
      (is (= 20 (get-in validated [:blueprint :estimated_minutes]))))))

(deftest drop-mode-prunes-outcomes-to-what-remains
  (let [two-outcomes (assoc-in form [:blueprint :assessed_outcomes]
                       [{:id "blank-recall" :statement "Recall what plants use."
                         :question_ids ["blank"] :evidence [citation]}
                        {:id "energy" :statement "Explain where the energy comes from."
                         :question_ids ["choice" "essay"] :evidence [citation]}])]
    (testing "an outcome whose questions all fail is reported as not assessed"
      (let [validated (salvage (assoc-in two-outcomes [:questions 0 :accepted_answers] []))]
        (is (= ["energy"] (mapv :id (get-in validated [:blueprint :assessed_outcomes]))))
        (is (some #(= {:statement "Recall what plants use."
                       :reason "The questions written for this outcome did not pass the checks."} %)
              (get-in validated [:blueprint :unassessed_outcomes])))))
    (testing "an outcome with a false quote is not assessed, and its questions go with it"
      (let [validated (salvage (assoc-in two-outcomes [:blueprint :assessed_outcomes 1 :evidence 0 :quote]
                                 "Plants eat sugar."))]
        (is (= ["blank"] (mapv :id (:questions validated))))
        (is (= #{"A question is not linked to a learning outcome."}
              (set (map :reason (get-in validated [:blueprint :dropped_questions])))))
        (is (some #(= "The quotes given for this outcome do not match the material." (:reason %))
              (get-in validated [:blueprint :unassessed_outcomes])))))))

(deftest drop-mode-refuses-a-form-with-no-question-left
  (let [bad (update form :questions (fn [qs] (mapv #(assoc % :evidence []) qs)))
        e (try (salvage bad) nil (catch clojure.lang.ExceptionInfo e e))]
    (is (= "A question quotes nothing from the material." (ex-message e)))
    (is (= 1 (:question (ex-data e))))
    (is (= ::assessment/invalid-assessment (:type (ex-data e))))))

(deftest a-false-quote-names-the-question-that-made-it
  (let [bad (assoc-in form [:questions 2 :evidence 0 :quote] "Plants eat sugar.")
        e (try (assessment/validate-form! bad evidence {}) nil
               (catch clojure.lang.ExceptionInfo e e))]
    (is (= "A quoted passage does not match the material word for word." (ex-message e)))
    (is (= 3 (:question (ex-data e))))))

(defn- letter-of
  "The letter the validated exam gave the option with this text."
  [exam text]
  (some #(when (= text (:text %)) (:id %)) (get-in exam [:questions 1 :options])))

(deftest objective-grading-needs-no-model-and-weights-points
  (let [exam (-> (assessment/validate-form! form evidence {})
                (update :questions #(subvec % 0 2)))]
    (with-redefs [llm/resolve-model+gate! (fn [& _] (throw (ex-info "Unexpected model call" {})))]
      (is (= 100 (:raw-score (assessment/grade! 1 exam {"blank" " SUNLIGHT "
                                                         "choice" (str/lower-case (letter-of exam "Light"))}))))
      (let [grade (assessment/grade! 1 exam {"blank" "water" "choice" "Light"})]
        (is (= 75 (:raw-score grade)))
        (is (zero? (:cost grade)))
        (is (= [13] (:fact_ids (first (:gaps grade)))))
      (is (zero? (:raw-score (assessment/grade! 1 exam {}))))))))

(deftest essay-deductions-stay-with-the-question-and-its-facts
  (let [question (last questions)
        grade {:criteria {:reasoning 2} :explanation "The answer omits the connection."
               :deductions [{:criterion "reasoning" :category "connection"
                             :reason "The answer does not connect the energy source with the product."
                             :evidence [citation] :fact_ids [13 99]}]}
        result (assessment/validate-free-grade! question grade (:materials evidence) "Plants make sugar.")]
    (is (= 30.0 (:points result)))
    (is (= [13] (get-in result [:gaps 0 :fact_ids])))
    (is (thrown? clojure.lang.ExceptionInfo
          (assessment/validate-free-grade! question (assoc grade :deductions []) (:materials evidence) "Plants.")))
    (is (thrown? clojure.lang.ExceptionInfo
          (assessment/validate-free-grade! question (assoc-in grade [:deductions 0 :evidence 0 :topic_id] 10)
            (:materials evidence) "Plants.")))
    (is (thrown? clojure.lang.ExceptionInfo
          (assessment/validate-free-grade! question grade (:materials evidence) "I do not know")))))

(deftest mixed-grading-combines-local-and-cited-model-scores
  (let [exam (assessment/validate-form! form evidence {})]
    (with-redefs [llm/resolve-model+gate! (fn [& _] {:api-key "key" :entry {:id "model"} :model-fields {}})
                  cards/load-prompt-template (constantly "prompt")
                  llm/chat! (fn [& _] {:parsed {:question_grades [{:question_id "essay" :criteria {:reasoning 4}
                                                                  :explanation "Complete." :deductions []}]
                                               :explanation "All targets met."} :cost 0.2})
                  credits/record-cost-charge! (fn [& _] 3)]
      (let [grade (assessment/grade! 1 exam {"blank" "light" "choice" (letter-of exam "Sugar")
                                             "essay" "Light supplies the energy to produce sugar."})]
        (is (= 70 (:raw-score grade)))
        (is (= 3 (:charged-credits grade)))
        (is (= 1 (count (:gaps grade))))))))

(deftest bounded-source-snapshot-preserves-originals-and-discloses-exclusions
  (testing "a long section is read whole: no section cap"
    (let [long-text (apply str (repeat 88407 "x"))
          bounded (assessment/bounded-evidence {:materials [{:material-id 7 :evidence [{:topic-id 1 :text long-text}]}]})]
      (is (= 88407 (count (get-in bounded [:materials 0 :evidence 0 :text]))))
      (is (zero? (get-in bounded [:source_limits :omitted_characters])))))
  (testing "the total budget cuts in section order"
    (let [source (apply str (repeat 500000 "x"))
          original {:materials [{:material-id 7 :evidence (mapv (fn [i] {:topic-id i :text source}) (range 3))}]}
          bounded (assessment/bounded-evidence original)]
      (is (= 500000 (count (get-in original [:materials 0 :evidence 2 :text]))))
      (is (= 200000 (count (get-in bounded [:materials 0 :evidence 2 :text]))))
      (is (= assessment/max-source-characters (get-in bounded [:source_limits :included_characters])))
      (is (= 300000 (get-in bounded [:source_limits :omitted_characters])))
      (is (get-in bounded [:materials 0 :evidence 2 :text_omitted])))))

(deftest facts-must-fit-the-included-excerpts-and-count-limit
  (let [fact {:id 1 :graph_topic_id 7 :subject_label "Plants" :predicate_label "use" :object_literal "light"}
        original (assoc evidence :facts (conj (mapv #(assoc fact :id %) (range 1 602))
                                          (assoc fact :id 999 :object_literal "absent detail")))
        bounded (assessment/bounded-evidence original)]
    (is (= 500 (count (:facts bounded))))
    (is (= 102 (get-in bounded [:source_limits :omitted_facts])))
    (is (= 602 (count (:facts original))))
    (is (not-any? #(= 999 (:id %)) (:facts bounded)))))

(deftest grading-does-not-repair-arithmetic-or-accept-invalid-scores
  (let [exam {:blueprint {:evidence_packets (:materials evidence)}
              :questions [{:id "ratio" :format "fill-in-the-blank" :points 10
                           :accepted_answers ["1/2"] :evidence [citation]}]}]
    (is (= 100 (:raw-score (assessment/grade! 1 exam {"ratio" " 1/2 "}))))
    (is (= 0 (:raw-score (assessment/grade! 1 exam {"ratio" "2/4"})))))
  (doseq [points [-1 11 Double/NaN Double/POSITIVE_INFINITY]]
    (is (thrown? clojure.lang.ExceptionInfo
          (assessment/normalized-score [{:points points :max_points 10}]))))
  (is (thrown? clojure.lang.ExceptionInfo
        (assessment/validate-form!
          (assoc-in form [:questions 1 :options] [{:id "A" :text "Light"} {:id "a" :text "Sugar"}]) evidence {})))
  (is (thrown? clojure.lang.ExceptionInfo
        (assessment/validate-free-grade! (last questions)
          {:criteria {:reasoning 4 :factual_accuracy 4} :explanation "Complete." :deductions []}
          (:materials evidence) "Light makes sugar."))))

(deftest scientific-case-and-option-id-meaning-survive-grading
  (let [exam {:blueprint {:evidence_packets (:materials evidence)}
              :questions [{:id "chemical" :format "fill-in-the-blank" :points 10
                           :accepted_answers ["Co"] :evidence [citation]}]}]
    (is (= 100 (:raw-score (assessment/grade! 1 exam {"chemical" " Co "}))))
    (is (= 0 (:raw-score (assessment/grade! 1 exam {"chemical" "CO"}))))
    (let [choice {:id "choice" :format "multiple-choice" :points 10 :evidence [citation]
                  :options [{:id "A" :text "B"} {:id "B" :text "A"}] :correct_option_id "A"}
          exam (assoc exam :questions [choice])]
      (is (= 0 (:raw-score (assessment/grade! 1 exam {"choice" "B"}))))
      (is (= 100 (:raw-score (assessment/grade! 1 exam {"choice" "a"})))))))

(deftest objective-grading-preserves-mathematical-notation
  (doseq [[accepted incorrect] [["x²" "x2"] ["H₂O" "H2O"] ["𝑥" "x"]]]
    (let [exam {:blueprint {:evidence_packets (:materials evidence)}
                :questions [{:id "notation" :format "fill-in-the-blank" :points 10
                             :accepted_answers [accepted] :evidence [citation]}]}
          incorrect-grade (assessment/grade! 1 exam {"notation" incorrect})]
      (is (= 100 (:raw-score (assessment/grade! 1 exam {"notation" (str "\u00a0" accepted " \n")}))))
      (is (zero? (:raw-score incorrect-grade)))
      (is (= "recall" (get-in incorrect-grade [:gaps 0 :category])))))
  (let [exam {:blueprint {:evidence_packets (:materials evidence)}
              :questions [{:id "notation" :format "fill-in-the-blank" :points 10
                           :accepted_answers ["café’s energy"] :evidence [citation]}]}]
    (is (= 100 (:raw-score (assessment/grade! 1 exam {"notation" "cafe\u0301's\u00a0energy"}))))))

(deftest multiple-choice-options-preserve-distinct-notation
  (let [validated (assessment/validate-form!
                    (assoc-in form [:questions 1 :options]
                      [{:id "A" :text "x²"} {:id "B" :text "x2"}]) evidence {})
        exam (assoc validated :questions [(get-in validated [:questions 1])])]
    (is (= 100 (:raw-score (assessment/grade! 1 exam {"choice" "x²"}))))
    (is (zero? (:raw-score (assessment/grade! 1 exam {"choice" "x2"})))))
  ;; IDs that differ only in notation are distinct, so the answer key stays
  ;; on "Square" when the IDs become letters.
  (let [validated (assessment/validate-form!
                    (-> form
                        (assoc-in [:questions 1 :options]
                          [{:id "x²" :text "Square"} {:id "x2" :text "Product"}])
                        (assoc-in [:questions 1 :correct_option_id] "x²")) evidence {})
        question (get-in validated [:questions 1])
        letter-of (fn [text] (some #(when (= text (:text %)) (:id %)) (:options question)))
        exam (assoc validated :questions [question])]
    (is (= 100 (:raw-score (assessment/grade! 1 exam {"choice" (letter-of "Square")}))))
    (is (zero? (:raw-score (assessment/grade! 1 exam {"choice" (letter-of "Product")}))))))

(def four-options
  [{:id "opt-portable" :text "portable"} {:id "opt-composable" :text "composable"}
   {:id "opt-performant" :text "performant"} {:id "opt-versatile" :text "versatile"}])

(defn- lettered [options correct]
  (-> (assessment/validate-form!
        (-> form (assoc-in [:questions 1 :options] options)
                 (assoc-in [:questions 1 :correct_option_id] correct)) evidence {})
      (get-in [:questions 1])))

(deftest multiple-choice-options-get-letter-ids
  (let [question (lettered four-options "opt-portable")
        correct (some #(when (= (:id %) (:correct_option_id question)) %) (:options question))]
    (testing "the model's IDs become A to D, and the key follows the correct text"
      (is (= ["A" "B" "C" "D"] (mapv :id (:options question))))
      (is (= "portable" (:text correct)))
      (is (= (set (map :text four-options)) (set (map :text (:options question))))))
    (testing "the shuffle is the same on every check of the same question"
      (is (= question (lettered four-options "opt-portable"))))
    (testing "the correct option is not fixed to the position the model wrote"
      (is (< 1 (count (set (for [n (range 12)]
                             (let [prompt (str "Which design goal " n "?")
                                   q (-> (assessment/validate-form!
                                           (-> form (assoc-in [:questions 1 :options] four-options)
                                                    (assoc-in [:questions 1 :correct_option_id] "opt-portable")
                                                    (assoc-in [:questions 1 :prompt] prompt)) evidence {})
                                         (get-in [:questions 1]))]
                               (:correct_option_id q))))))))))

(deftest a-marked-letter-grades-as-that-letter
  (let [question (lettered four-options "opt-portable")
        right (:correct_option_id question)
        wrong (some #(when (not= right (:id %)) (:id %)) (:options question))
        exam {:blueprint {:evidence_packets (:materials evidence)} :questions [question]}
        score #(:raw-score (assessment/grade! 1 exam {"choice" %}))]
    (doseq [answer [right (str/lower-case right) (str right ".") (str "(" right ")") (str right ")") "portable"]]
      (is (= 100 (score answer)) answer))
    (doseq [answer [wrong (str "(" wrong ")") "versatile"]]
      (is (zero? (score answer)) answer))))

(deftest a-grade-reads-criteria-written-as-a-list
  (let [exam (assessment/validate-form! form evidence {})]
    (with-redefs [llm/resolve-model+gate! (fn [& _] {:api-key "key" :entry {:id "model"} :model-fields {}})
                  cards/load-prompt-template (constantly "prompt")
                  llm/chat! (fn [& _] {:parsed {:question_grades [{:question_id "essay"
                                                                  :criteria [{:criterion "reasoning" :score 4}]
                                                                  :explanation "Complete." :deductions []}]
                                               :explanation "All targets met."} :cost 0.2})
                  credits/record-cost-charge! (fn [& _] 1)]
      (let [graded (assessment/grade! 1 exam {"blank" "light" "choice" "B" "essay" "Light makes sugar."})]
        (is (= {:reasoning 4} (:criteria (some #(when (= "essay" (:question_id %)) %) (:criteria graded)))))))))
