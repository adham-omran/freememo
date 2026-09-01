(ns freememo.llm-json-test
  "Covers the strict JSON contract for structured LLM inputs and responses."
  (:require
   [cheshire.core :as json]
   [clojure.test :refer [deftest is testing]]
   [freememo.llm-json :as llm-json]))

(defn- parse-failure [raw-text root-key root-type]
  (try
    (llm-json/parse-response-field raw-text root-key root-type)
    (catch clojure.lang.ExceptionInfo e e)))

(deftest writes-prompt-input-as-json
  (let [value {:content "text" :card-count 2 :facts [{:id 7}]}
        encoded (llm-json/write-prompt-input value)]
    (is (= value (json/parse-string-strict encoded true)))
    (is (not (re-find #":content|:card-count|:facts" encoded)))))

(deftest parses-required-vector-field
  (is (= [{:q "What?" :a "This."}]
        (llm-json/parse-response-field
          "{\"cards\":[{\"q\":\"What?\",\"a\":\"This.\"}]}"
          :cards :vector))))

(deftest parses-required-map-field
  (is (= {:verdict "correct" :explanation "Correct."}
        (llm-json/parse-response-field
          "{\"grade\":{\"verdict\":\"correct\",\"explanation\":\"Correct.\"}}"
          :grade :map))))

(deftest realizes-nested-arrays
  (let [cards (llm-json/parse-response-field
                "{\"cards\":[{\"items\":[\"a\",\"b\"]}]}"
                :cards :vector)]
    (is (vector? cards))
    (is (vector? (:items (first cards))))))

(deftest rejects-non-object-and-invalid-root-fields
  (testing "the top-level value must be an object"
    (is (= :freememo.llm-json/unparseable
          (:type (ex-data (parse-failure "[]" :cards :vector))))))
  (testing "the named field must exist"
    (is (= :freememo.llm-json/unparseable
          (:type (ex-data (parse-failure "{\"items\":[]}" :cards :vector))))))
  (testing "the named field must have the declared type"
    (is (= :freememo.llm-json/unparseable
          (:type (ex-data (parse-failure "{\"cards\":{}}" :cards :vector)))))))

(deftest rejects-every-legacy-or-repaired-form
  (doseq [raw-text [""
                    "{:cards []}"
                    "{'cards': []}"
                    "```json\n{\"cards\":[]}\n```"
                    "Here are the cards: {\"cards\":[]}"
                    "{\"cards\":[]} trailing prose"
                    "{\"cards\":[],}"
                    "{\"cards\":[]} {\"cards\":[]}"]]
    (is (= :freememo.llm-json/unparseable
          (:type (ex-data (parse-failure raw-text :cards :vector))))
      (str "must reject: " (pr-str raw-text)))))

(deftest failure-is-typed-without-raw-content
  (let [failure (parse-failure "{:cards []}" :cards :vector)
        data (ex-data failure)]
    (is (= :freememo.llm-json/unparseable (:type data)))
    (is (= :cards (:root-key data)))
    (is (= :vector (:root-type data)))
    (is (= 11 (:response-length data)))
    (is (not (contains? data :raw)))
    (is (some? (ex-cause failure)))))

(deftest unknown-root-type-is-a-caller-error
  (let [failure (parse-failure "{\"cards\":[]}" :cards :sequence)]
    (is (= "Unknown structured response root type" (ex-message failure)))
    (is (nil? (:type (ex-data failure))))))
