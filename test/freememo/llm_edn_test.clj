(ns freememo.llm-edn-test
  "Covers freememo.llm-edn/parse-response across the wrapper variants models
   emit despite the EDN instruction: bare EDN, triple-backtick fences, JSON,
   single-backtick-wrapped EDN (the Gemini 3 Flash bug), and leading prose.
   Unparseable input must still throw."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.llm-edn :as llm-edn]))

(def ^:private cards
  [{:q "What is an IR?" :a "Internal compiler data structure"}
   {:q "What does an IR enable?" :a "Optimization and translation"}])

(deftest parses-bare-edn
  (is (= cards (llm-edn/parse-response (pr-str cards)))))

(deftest parses-triple-fenced-edn
  (is (= cards (llm-edn/parse-response (str "```clojure\n" (pr-str cards) "\n```")))))

(deftest parses-json-fallback
  (testing "JSON with keywordized keys yields the identical shape"
    (is (= cards
          (llm-edn/parse-response
            "[{\"q\": \"What is an IR?\", \"a\": \"Internal compiler data structure\"},
              {\"q\": \"What does an IR enable?\", \"a\": \"Optimization and translation\"}]")))))

(deftest parses-single-backtick-wrapped-edn
  (testing "regression: Gemini 3 Flash wrapped its EDN vector in single backticks"
    (is (= cards (llm-edn/parse-response (str "`" (pr-str cards) "`"))))))

(deftest parses-leading-prose
  (testing "bracket extraction survives conversational preamble"
    (is (= cards (llm-edn/parse-response (str "Here are your cards: " (pr-str cards)))))))

(deftest throws-on-unparseable
  (testing "no bracketed payload → ex-info with the parse-failure message"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
          #"Failed to parse model response"
          (llm-edn/parse-response "Sorry, I can't do that.")))))
