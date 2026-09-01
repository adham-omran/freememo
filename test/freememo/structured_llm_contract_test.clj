(ns freememo.structured-llm-contract-test
  "Covers each structured LLM lane at the JSON wire seam."
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [freememo.card-models :as card-models]
   [freememo.cards :as cards]
   [freememo.cards-from-facts :as cards-from-facts]
   [freememo.credits :as credits]
   [freememo.db :as db]
   [freememo.kg-extract :as kg-extract]
   [freememo.kg-grade :as kg-grade]
   [freememo.kg-llm :as kg-llm]
   [freememo.kg-questions :as kg-questions]
   [freememo.llm-json :as llm-json]
   [freememo.openrouter :as openrouter]
   [freememo.settings :as settings]))

(defn- json-response [root-key root-value]
  (llm-json/write-prompt-input {root-key root-value}))

(defn- invalid-response? [raw-text root-key root-type]
  (try
    (llm-json/parse-response-field raw-text root-key root-type)
    false
    (catch clojure.lang.ExceptionInfo e
      (openrouter/retryable-cause? e))))

(def ^:private generated-card-valid? @#'cards/generated-card-valid?)
(def ^:private triple-valid? @#'kg-extract/valid-triple?)
(def ^:private link-decision-valid? @#'kg-extract/link-decision-valid?)
(def ^:private validated-question @#'kg-questions/validated-question)
(def ^:private validate-grade! @#'kg-grade/validate-grade!)
(def ^:private candidate-fact-for-id @#'cards-from-facts/candidate-fact-for-id)
(def ^:private completion-failure-log-data
  @#'kg-llm/completion-failure-log-data)

(defn- every-card-valid? [card-type cards]
  (and (vector? cards)
    (every? #(generated-card-valid? card-type %) cards)))

(defn- every-triple-valid? [triples]
  (and (vector? triples) (every? triple-valid? triples)))

(defn- every-link-decision-valid? [decisions]
  (and (map? decisions)
    (every? #(link-decision-valid? #{42} %) (vals decisions))))

(defn- every-question-valid? [valid-ids minimum-facts questions]
  (and (vector? questions)
    (every? #(some? (validated-question valid-ids minimum-facts %)) questions)))

(defn- grade-valid? [grade]
  (try
    (validate-grade! grade)
    true
    (catch clojure.lang.ExceptionInfo _ false)))

(defn- every-fact-id-valid? [fact-ids]
  (let [candidates {3 {:id 3} 7 {:id 7} 12 {:id 12}}]
    (and (vector? fact-ids)
      (every? #(some? (candidate-fact-for-id candidates %)) fact-ids))))

(def ^:private lane-contracts
  [{:lane :basic
    :prompt "basic.md"
    :root-key :cards
    :root-type :vector
    :valid-value [{:q "What?" :a "This."}]
    :valid-domain? #(every-card-valid? "basic" %)
    :wrong-domain-value [{:q "What?"}]}
   {:lane :cloze
    :prompt "cloze.md"
    :root-key :cards
    :root-type :vector
    :valid-value [{:c "The {{c1::aorta}} receives blood."}]
    :valid-domain? #(every-card-valid? "cloze" %)
    :wrong-domain-value [{:q "What?" :a "This."}]}
   {:lane :overlapping
    :prompt "overlapping.md"
    :root-key :cards
    :root-type :vector
    :valid-value [{:question "What are the steps?" :items ["One" "Two"]}]
    :valid-domain? #(every-card-valid? "overlapping" %)
    :wrong-domain-value [{:question "What are the steps?" :items "One"}]}
   {:lane :kg-extract
    :prompt "kg-extract.md"
    :root-key :triples
    :root-type :vector
    :valid-value [{:s "heart" :p "pumps" :o "blood"}]
    :valid-domain? every-triple-valid?
    :wrong-domain-value [{:s "heart" :p "pumps" :o "blood" :lit "blood"}]}
   {:lane :kg-link
    :prompt "kg-link.md"
    :root-key :decisions
    :root-type :map
    :valid-value {:aorta 42 :atrium "new"}
    :valid-domain? every-link-decision-valid?
    :wrong-domain-value {:aorta "maybe"}}
   {:lane :kg-atomic
    :prompt "kg-question-atomic.md"
    :root-key :questions
    :root-type :vector
    :valid-value [{:q "Which chamber pumps blood?" :a "The left ventricle." :fact-ids [7]}]
    :valid-domain? #(every-question-valid? #{7} 1 %)
    :wrong-domain-value [{:q "Which chamber?" :a "The left ventricle." :fact-ids 7}]}
   {:lane :kg-synthesis
    :prompt "kg-question-synthesis.md"
    :root-key :questions
    :root-type :vector
    :valid-value [{:q "How do they relate?" :a "They work together." :fact-ids [3 7]}]
    :valid-domain? #(every-question-valid? #{3 7} 2 %)
    :wrong-domain-value [{:q "How?" :a "This way." :fact-ids [3]}]}
   {:lane :kg-grade
    :prompt "kg-grade.md"
    :root-key :grade
    :root-type :map
    :valid-value {:verdict "partial" :explanation "One fact is missing."
                  :missed-fact-ids [7] :matched-keywords ["aorta"]}
    :valid-domain? grade-valid?
    :wrong-domain-value {:verdict "maybe" :explanation "Unclear."}}
   {:lane :card-grade
    :prompt "card-grade.md"
    :root-key :grade
    :root-type :map
    :valid-value {:verdict "correct" :explanation "Correct."}
    :valid-domain? grade-valid?
    :wrong-domain-value {:verdict "correct"}}
   {:lane :fact-select
    :prompt "select-facts.md"
    :root-key :fact_ids
    :root-type :vector
    :valid-value [3 7 12]
    :valid-domain? every-fact-id-valid?
    :wrong-domain-value [3 "seven"]}])

(deftest every-lane-has-a-strict-response-contract
  (doseq [{:keys [lane root-key root-type valid-value valid-domain?
                  wrong-domain-value]} lane-contracts]
    (testing (name lane)
      (is (= valid-value
            (llm-json/parse-response-field
              (json-response root-key valid-value) root-key root-type)))
      (is (invalid-response? "{:legacy true}" root-key root-type))
      (is (invalid-response? (json-response :wrong-root valid-value)
            root-key root-type))
      (is (invalid-response? (json-response root-key
                               (if (= root-type :vector) {} []))
            root-key root-type))
      (is (true? (valid-domain? valid-value)))
      (is (false? (valid-domain? wrong-domain-value))))))

(deftest every-lane-prompt-demands-json-only
  (doseq [{:keys [lane prompt]} lane-contracts]
    (testing (name lane)
      (let [text (slurp (io/resource (str "prompts/" prompt)))]
        (is (str/includes? text "Return one JSON object"))
        (is (not (re-find #"(?i)\bEDN\b|Clojure/EDN|Clojure map" text)))
        (is (not (re-find #"`:[a-z]" text)))
        (is (not (str/includes? text "```")))))))

(deftest every-kg-lane-retries-malformed-json-and-a-missing-root
  (doseq [{:keys [lane root-key root-type valid-value]}
          (remove #(#{:basic :cloze :overlapping} (:lane %)) lane-contracts)
          [failure-name invalid-content]
          [["malformed JSON" "{:legacy true}"]
           ["missing root" (json-response :wrong-root valid-value)]]]
    (testing (str (name lane) ": " failure-name)
      (let [attempts (atom 0)
            responses [{:choices [{:message {:content invalid-content}
                                   :finish_reason "stop"}]
                        :usage {:cost 0.001}}
                       {:choices [{:message {:content (json-response root-key valid-value)}
                                   :finish_reason "stop"}]
                        :usage {:cost 0.002}}]]
        (with-redefs [openrouter/chat-completion!
                      (fn [_ _ _]
                        (let [index (swap! attempts inc)]
                          (nth responses (dec index))))]
          (is (= {:parsed valid-value :cost 0.003}
                (kg-llm/chat! "key" {:model "model"} "prompt" "{}"
                  root-key root-type)))
          (is (= 2 @attempts)))))))

(deftest card-generation-retries-invalid-json-and-bills-all-attempts
  (let [attempts (atom 0)
        charged (atom nil)
        request-bodies (atom [])
        responses [{:choices [{:message {:content "{:cards []}"}
                              :finish_reason "stop"}]
                    :usage {:cost 0.001}}
                   {:choices [{:message {:content "{\"cards\":[{\"q\":\"What?\",\"a\":\"This.\"}]}"}
                              :finish_reason "stop"}]
                    :usage {:cost 0.002}}]]
    (with-redefs [settings/get-openrouter-api-key (constantly "key")
                  settings/get-card-count (constantly 1)
                  settings/get-model (constantly "gemini-3-flash")
                  settings/get-card-gen-max-retries (constantly 2)
                  settings/get-reasoning (constantly "low")
                  settings/get-verbosity (constantly "high")
                  settings/get-effective-system-prompt (fn [_ _] "System")
                  credits/check-cost-billed-balance! (constantly {:ok true})
                  credits/record-cost-charge!
                  (fn [_ endpoint model-id cost]
                    (reset! charged [endpoint model-id cost])
                    3.0)
                  openrouter/chat-completion!
                  (fn [_ request-body _]
                    (swap! request-bodies conj request-body)
                    (let [index (swap! attempts inc)]
                      (nth responses (dec index))))]
      (is (= {:success true
              :cards [{:q "What?" :a "This."}]
              :cost-credits 3.0}
            (cards/generate-basic-cards
              {:content "Source"
               :card-count 1
               :user-id 7
               :pre-prompt "Ask this"
               :pre-prompt-mode "clarify"
               :goal "Pass the exam"})))
      (is (= 2 @attempts))
      (is (= [:cards.basic "gemini-3-flash" 0.003] @charged))
      (doseq [request-body @request-bodies]
        (is (= {:type "json_object"} (:response_format request-body)))
        (is (true? (get-in request-body [:provider :require_parameters])))
        (is (= "low" (:reasoning_effort request-body)))
        (is (not (contains? request-body :verbosity)))
        (is (= {:content "Source"
                :card-count 1
                :options {:card-type "basic"
                          :pre-prompt "Ask this"
                          :pre-prompt-mode "clarify"
                          :learning-goal "Pass the exam"}}
              (json/parse-string-strict
                (get-in request-body [:messages 1 :content]) true)))))))

(def ^:private card-lanes
  [{:lane :basic
    :generate cards/generate-basic-cards
    :valid-value [{:q "What?" :a "This."}]
    :wrong-domain-value [{:q "What?"}]}
   {:lane :cloze
    :generate cards/generate-cloze-cards
    :valid-value [{:c "The {{c1::aorta}} receives blood."}]
    :wrong-domain-value [{:c ""}]}
   {:lane :overlapping
    :generate cards/generate-overlapping-cards
    :valid-value [{:question "What are the steps?" :items ["One" "Two"]}]
    :wrong-domain-value [{:question "What are the steps?" :items "One"}]}])

(deftest every-card-lane-retries-malformed-json-and-a-missing-root
  (doseq [{:keys [lane generate valid-value]} card-lanes
          [failure-name invalid-content]
          [["malformed JSON" "{:legacy true}"]
           ["missing root" (json-response :wrong-root valid-value)]]]
    (testing (str (name lane) ": " failure-name)
      (let [attempts (atom 0)
            responses [(json-response :cards valid-value)]]
        (with-redefs [settings/get-openrouter-api-key (constantly "key")
                      settings/get-card-count (constantly 1)
                      settings/get-model (constantly "gemini-3-flash")
                      settings/get-card-gen-max-retries (constantly 2)
                      settings/get-reasoning (constantly "low")
                      settings/get-verbosity (constantly "high")
                      settings/get-effective-system-prompt (fn [_ _] "System")
                      credits/check-cost-billed-balance! (constantly {:ok true})
                      credits/record-cost-charge! (constantly 1.0)
                      openrouter/chat-completion!
                      (fn [_ _ _]
                        (if (= 1 (swap! attempts inc))
                          {:choices [{:message {:content invalid-content}
                                      :finish_reason "stop"}]
                           :usage {:cost 0.001}}
                          {:choices [{:message {:content (first responses)}
                                      :finish_reason "stop"}]
                           :usage {:cost 0.002}}))]
          (is (= valid-value
                (:cards (generate {:content "Source"
                                   :card-count 1
                                   :user-id 7}))))
          (is (= 2 @attempts)))))))

(deftest every-card-lane-rejects-valid-json-with-invalid-card-fields
  (doseq [{:keys [lane generate wrong-domain-value]} card-lanes]
    (testing (name lane)
      (with-redefs [settings/get-openrouter-api-key (constantly "key")
                    settings/get-card-count (constantly 1)
                    settings/get-model (constantly "gemini-3-flash")
                    settings/get-card-gen-max-retries (constantly 1)
                    settings/get-reasoning (constantly "low")
                    settings/get-verbosity (constantly "high")
                    settings/get-effective-system-prompt (fn [_ _] "System")
                    credits/check-cost-billed-balance! (constantly {:ok true})
                    credits/record-cost-charge! (constantly 1.0)
                    openrouter/chat-completion!
                    (fn [_ _ _]
                      {:choices [{:message
                                  {:content (json-response
                                              :cards wrong-domain-value)}
                                  :finish_reason "stop"}]
                       :usage {:cost 0.001}})]
        (let [result (generate {:content "Source"
                                :card-count 1
                                :user-id 7})]
          (is (false? (:success result)))
          (is (str/includes? (:error result) "invalid card data")))))))

(deftest kg-terminal-json-failure-does-not-expose-private-response-content
  (let [private-content "TOPSECRET_PRIVATE_TEXT"
        raw-response (str "{\"triples\":[" private-content "]}")
        failure
        (with-redefs [openrouter/chat-completion!
                      (fn [_ _ _]
                        {:choices [{:message {:content raw-response}
                                    :finish_reason "stop"}]
                         :usage {:cost 0.001}})]
          (try
            (kg-llm/chat! "key" {:model "model"} "prompt" "{}"
              :triples :vector {:feature :kg-extract :user-id 7})
            (catch clojure.lang.ExceptionInfo e e)))]
    (is (= :freememo.llm-json/unparseable (:type (ex-data failure))))
    (is (= "The model returned invalid structured JSON" (ex-message failure)))
    (is (nil? (ex-cause failure)))
    (is (not (str/includes? (str failure) private-content)))
    (is (not (str/includes? (pr-str (ex-data failure)) private-content)))))

(deftest kg-terminal-failure-log-identifies-the-request-without-content
  (let [failure (ex-info "Invalid" {:type :freememo.llm-json/unparseable})
        data (completion-failure-log-data
               {:model "z-ai/glm-5.3" :provider {:only ["z-ai"]}}
               {}
               {:feature :kg-grade :user-id 7}
               0.003 2 failure)]
    (is (= {:cost-usd 0.003
            :attempt 2
            :cause :freememo.llm-json/unparseable
            :feature :kg-grade
            :user-id 7
            :model "z-ai/glm-5.3"
            :provider "z-ai"}
          data))
    (is (not (contains? data :prompt)))
    (is (not (contains? data :response)))))

(deftest fact-select-accepts-its-snake-case-json-root
  (let [attempts (atom 0)
        candidates [{:id 7
                     :subject_label "heart"
                     :predicate_label "pumps"
                     :object_label "blood"}]]
    (with-redefs [kg-llm/resolve-model+gate!
                  (constantly {:api-key "key"
                               :entry {:id "glm-5.3-flash"}
                               :model-fields {:model "z-ai/glm-5.3-flash"}})
                  openrouter/chat-completion!
                  (fn [_ _ _]
                    (swap! attempts inc)
                    {:choices [{:message {:content "{\"fact_ids\": []}"}
                                :finish_reason "stop"}]
                     :usage {:cost 0.001}})
                  credits/record-cost-charge! (constantly 1.0)
                  db/get-fact-mastery (constantly {})]
      (is (= {:success true :facts [] :cost 0.001}
            (cards-from-facts/select-relevant-facts
              8 11910 "unrelated passage" candidates)))
      (is (= 1 @attempts))
      (is (invalid-response? "{\"fact-ids\": []}" :fact_ids :vector)))))

(deftest all-card-kinds-parse-their-json-envelope
  (doseq [[lane generate response expected]
          [[:basic cards/generate-basic-cards
            "{\"cards\":[{\"q\":\"What?\",\"a\":\"This.\"}]}"
            [{:q "What?" :a "This."}]]
           [:cloze cards/generate-cloze-cards
            "{\"cards\":[{\"c\":\"The {{c1::aorta}}.\"}]}"
            [{:c "The {{c1::aorta}}."}]]
           [:overlapping cards/generate-overlapping-cards
            "{\"cards\":[{\"question\":\"What are the steps?\",\"items\":[\"One\",\"Two\"]}]}"
            [{:question "What are the steps?" :items ["One" "Two"]}]]]]
    (testing (name lane)
      (with-redefs [settings/get-openrouter-api-key (constantly "key")
                    settings/get-card-count (constantly 1)
                    settings/get-model (constantly "gemini-3-flash")
                    settings/get-card-gen-max-retries (constantly 1)
                    settings/get-reasoning (constantly "low")
                    settings/get-verbosity (constantly "high")
                    settings/get-effective-system-prompt (fn [_ _] "System")
                    credits/check-cost-billed-balance! (constantly {:ok true})
                    credits/record-cost-charge! (constantly 1.0)
                    openrouter/chat-completion!
                    (fn [_ _ _]
                      {:choices [{:message {:content response}
                                  :finish_reason "stop"}]
                       :usage {:cost 0.001}})]
        (is (= expected
              (:cards (generate {:content "Source"
                                  :card-count 1
                                  :user-id 7}))))))))
