(ns freememo.assessment-test
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.assessment :as assessment]
            [freememo.cards :as cards]
            [freememo.credits :as credits]
            [freememo.kg-llm :as llm]))

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
  (doseq [bad [(assoc form :questions [])
               (assoc-in form [:questions 0 :accepted_answers] [])
               (assoc-in form [:questions 1 :correct_option_id] "C")
               (assoc-in form [:questions 0 :evidence 0 :topic_id] 10)
               (assoc-in form [:blueprint :omitted_formats] [])
               (assoc-in form [:blueprint :assessed_outcomes 0 :question_ids] ["other"])]]
    (is (thrown? clojure.lang.ExceptionInfo (assessment/validate-form! bad evidence {})))))

(deftest objective-grading-needs-no-model-and-weights-points
  (let [exam (-> (assessment/validate-form! form evidence {})
                (update :questions #(subvec % 0 2)))]
    (with-redefs [llm/resolve-model+gate! (fn [& _] (throw (ex-info "Unexpected model call" {})))]
      (is (= 100 (:raw-score (assessment/grade! 1 exam {"blank" " SUNLIGHT " "choice" "a"}))))
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
      (let [grade (assessment/grade! 1 exam {"blank" "light" "choice" "B" "essay" "Light supplies the energy to produce sugar."})]
        (is (= 70 (:raw-score grade)))
        (is (= 3 (:charged-credits grade)))
        (is (= 1 (count (:gaps grade))))))))

(deftest bounded-source-snapshot-preserves-originals-and-discloses-exclusions
  (let [source (apply str (repeat 6000 "x"))
        original {:materials [{:material-id 7 :evidence (mapv (fn [i] {:topic-id i :text source}) (range 30))}]}
        bounded (assessment/bounded-evidence original)]
    (is (= 6000 (count (get-in original [:materials 0 :evidence 0 :text]))))
    (is (= 5000 (count (get-in bounded [:materials 0 :evidence 0 :text]))))
    (is (= 0 (count (get-in bounded [:materials 0 :evidence 29 :text]))))
    (is (= 120000 (get-in bounded [:source_limits :included_characters])))
    (is (= 60000 (get-in bounded [:source_limits :omitted_characters])))
    (is (get-in bounded [:materials 0 :evidence 29 :text_omitted]))))

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

(deftest generated-coverage-discloses-truncated-source-and-excluded-facts
  (let [source (assoc-in evidence [:materials 0 :evidence 0 :text]
                 (str (:quote citation) (apply str (repeat 6000 "x"))))]
    (with-redefs [llm/resolve-model+gate! (fn [& _] {:api-key "key" :entry {:id "model"} :model-fields {}})
                  cards/load-prompt-template (constantly "prompt")
                  llm/chat! (fn [& _] {:parsed form :cost 0.1})
                  credits/record-cost-charge! (fn [& _] 1)]
      (let [generated (assessment/generate! 1 source {:target-minutes 15})
            blueprint (:blueprint generated)]
        (is (= 5000 (count (get-in blueprint [:evidence_packets 0 :evidence 0 :text]))))
        (is (pos? (get-in blueprint [:source_limits :omitted_characters])))
        (is (= 1 (get-in blueprint [:source_limits :omitted_facts])))
        (is (some #(= "source-snapshot-limit" (:id %)) (:unassessed_outcomes blueprint)))
        (is (some #(= "fact-snapshot-limit" (:id %)) (:unassessed_outcomes blueprint)))))))

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
  (let [validated (assessment/validate-form!
                    (-> form
                        (assoc-in [:questions 1 :options]
                          [{:id "x²" :text "Square"} {:id "x2" :text "Product"}])
                        (assoc-in [:questions 1 :correct_option_id] "x²")) evidence {})
        exam (assoc validated :questions [(get-in validated [:questions 1])])]
    (is (= 100 (:raw-score (assessment/grade! 1 exam {"choice" "x²"}))))
    (is (zero? (:raw-score (assessment/grade! 1 exam {"choice" "x2"}))))))
