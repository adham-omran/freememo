(ns freememo.openrouter-test
  "Covers freememo.openrouter/message-content and retryable-cause? — the gate
   between a 200 response and the EDN parser — and model-fields, the one place a
   registry entry becomes request fields.

   Anchor: a production card generation received a 200 whose
   choices[0].message.content was null, and the parser reported it as
   unreadable EDN. Each cause below must classify distinctly, and the terminal
   one (refusal) must not be retried."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.card-models :as card-models]
            [freememo.llm-edn :as llm-edn]
            [freememo.ocr-models :as ocr-models]
            [freememo.openrouter :as openrouter]))

(defn- body
  "A parsed 200 chat-completion body with one choice built from `choice`."
  [choice & {:as extra}]
  (merge {:choices [choice] :usage {:cost 0.001 :completion_tokens 12}} extra))

(defn- cause-of [b]
  (try (openrouter/message-content b)
    (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest returns-content-when-present
  (is (= "[{:q \"a\" :a \"b\"}]"
        (openrouter/message-content (body {:message {:content "[{:q \"a\" :a \"b\"}]"}
                                           :finish_reason "stop"})))))

(deftest classifies-silent-empty
  (testing "the incident: content null, finish_reason stop, no refusal"
    (is (= :freememo.openrouter/empty-content
          (cause-of (body {:message {:content nil} :finish_reason "stop"})))))
  (testing "blank-but-present content is the same cause, not a parse failure"
    (is (= :freememo.openrouter/empty-content
          (cause-of (body {:message {:content "   \n"} :finish_reason "stop"})))))
  (testing "structured (non-string) content cannot leak past the string post-condition"
    (is (= :freememo.openrouter/empty-content
          (cause-of (body {:message {:content [{:type "text" :text "hi"}]}
                           :finish_reason "stop"}))))))

(deftest classifies-refusal
  (testing "OpenAI-family refusal field"
    (is (= :freememo.openrouter/refused
          (cause-of (body {:message {:content nil :refusal "I can't help with that."}
                           :finish_reason "stop"})))))
  (testing "content_filter finish reason"
    (is (= :freememo.openrouter/refused
          (cause-of (body {:message {:content nil} :finish_reason "content_filter"})))))
  (testing "provider-native safety stop"
    (is (= :freememo.openrouter/refused
          (cause-of (body {:message {:content nil} :finish_reason "stop"
                           :native_finish_reason "SAFETY"}))))))

(deftest classifies-truncation
  (testing "reasoning consumed the budget — OpenAI wording"
    (is (= :freememo.openrouter/truncated
          (cause-of (body {:message {:content nil} :finish_reason "length"})))))
  (testing "Gemini native wording"
    (is (= :freememo.openrouter/truncated
          (cause-of (body {:message {:content ""} :finish_reason "error"
                           :native_finish_reason "MAX_TOKENS"}))))))

(deftest classifies-missing-choices
  (testing "no choices at all"
    (is (= :freememo.openrouter/no-choices
          (cause-of {:choices [] :usage {}}))))
  (testing "choice without a message"
    (is (= :freememo.openrouter/no-choices
          (cause-of (body {:finish_reason "error"})))))
  (testing "200 carrying a body-level provider error"
    (is (= :freememo.openrouter/no-choices
          (cause-of (body {:message {:content nil}}
                      :error {:message "upstream provider failed"}))))))

(deftest refusal-outranks-truncation
  (testing "fixed precedence: a refused+truncated response is terminal"
    (is (= :freememo.openrouter/refused
          (cause-of (body {:message {:content nil :refusal "No."}
                           :finish_reason "length"}))))))

(deftest throw-carries-diagnostic-shape
  (testing "the fields the incident report lacked"
    (let [data (try (openrouter/message-content
                      (body {:message {:content nil :refusal "No."}
                             :finish_reason "content_filter"
                             :native_finish_reason "SAFETY"})
                      {:feature :cards :user-id 7})
                 (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= "content_filter" (:finish-reason data)))
      (is (= "SAFETY" (:native-finish-reason data)))
      (is (= "No." (:refusal data)))
      (is (= 1 (:choices data)))
      (is (= {:cost 0.001 :completion_tokens 12} (:usage data)))
      (is (= :cards (:feature data)))
      (is (= 7 (:user-id data))))))

(deftest retryable-causes-are-retryable
  (doseq [cause [:freememo.openrouter/empty-content
                 :freememo.openrouter/truncated
                 :freememo.openrouter/no-choices
                 :freememo.llm-edn/unparseable]]
    (is (true? (openrouter/retryable-cause? (ex-info "x" {:type cause})))
      (str cause " must be retryable"))))

(deftest terminal-and-untyped-failures-are-not-retryable
  (testing "a refusal is deterministic — retrying resends identical input"
    (is (false? (openrouter/retryable-cause?
                  (ex-info "x" {:type :freememo.openrouter/refused})))))
  (testing "transport and precondition failures must not consume attempts"
    (doseq [e [(ex-info "OpenRouter HTTP 401" {:type :freememo.openrouter/api-error
                                               :status 401})
               (ex-info "OpenRouter API key not configured" {})
               (ex-info "No content provided" {})
               (ex-info "out of credits" {:type :freememo.cards/insufficient-credits})
               (Exception. "socket reset")
               nil]]
      (is (false? (openrouter/retryable-cause? e))
        (str "must not retry: " (some-> e ex-message))))))

(deftest parser-cause-is-the-one-the-predicate-names
  (testing "llm-edn's typed throw and openrouter's retryable set agree"
    (let [e (try (llm-edn/parse-response "prose with no payload")
               (catch clojure.lang.ExceptionInfo ex ex))]
      (is (true? (openrouter/retryable-cause? e))))))

(deftest model-fields-carries-the-pin-only-when-the-entry-has-one
  (testing "an entry without :provider yields the slug alone"
    (is (= {:model "google/gemini-3-flash-preview"}
          (openrouter/model-fields {:id "gemini-3-flash"
                                    :openrouter-model "google/gemini-3-flash-preview"}))))
  (testing "a pinned entry passes its provider object verbatim"
    (is (= {:model "z-ai/glm-5.3-flash" :provider {:only ["z-ai"]}}
          (openrouter/model-fields {:id "glm-5.3-flash"
                                    :openrouter-model "z-ai/glm-5.3-flash"
                                    :provider {:only ["z-ai"]}})))))

(deftest every-chat-registry-entry-yields-a-slug
  (testing "both registries, every entry that names an OpenRouter model"
    (doseq [entry (concat card-models/registry ocr-models/registry)
            :when (:openrouter-model entry)]
      (is (string? (:model (openrouter/model-fields entry))) (:id entry)))))

(deftest glm-5-3-flash-is-pinned-to-z-ai-in-both-registries
  (testing "the pin is the reason the entry exists at its price"
    (doseq [resolve [card-models/resolve-model ocr-models/resolve-model]]
      (is (= {:model "z-ai/glm-5.3-flash" :provider {:only ["z-ai"]}}
            (openrouter/model-fields (resolve "glm-5.3-flash")))))))
