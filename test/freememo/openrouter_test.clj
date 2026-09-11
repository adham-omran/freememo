(ns freememo.openrouter-test
  "Covers freememo.openrouter/message-content and retryable-cause? — the gate
   between a 200 response and the JSON parser. It also covers the request-field
   seam where registry capabilities become OpenRouter parameters.

   Anchor: a production card generation received a 200 whose
   choices[0].message.content was null, and the parser reported it as
   unreadable structured output. Each cause below must classify distinctly, and the terminal
   one (refusal) must not be retried."
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.card-models :as card-models]
            [freememo.llm-json :as llm-json]
            [freememo.ocr-models :as ocr-models]
            [freememo.openrouter :as openrouter])
  (:import [java.io ByteArrayInputStream]))

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
                 :freememo.llm-json/unparseable]]
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
  (testing "llm-json's typed throw and openrouter's retryable set agree"
    (let [e (try (llm-json/parse-response-field "prose with no payload"
                   :cards :vector)
               (catch clojure.lang.ExceptionInfo ex ex))]
      (is (true? (openrouter/retryable-cause? e))))))

(deftest chat-completion-fields-projects-model-capabilities
  (testing "a supported optional parameter remains"
    (let [entry (card-models/resolve-model "claude-sonnet-5")]
      (is (= {:model "anthropic/claude-sonnet-5"
              :reasoning_effort "low"
              :verbosity "high"}
            (openrouter/chat-completion-fields entry
              {:reasoning_effort "low" :verbosity "high" :temperature 0.2})))))
  (testing "unsupported optional parameters are omitted"
    (let [entry (card-models/resolve-model "claude-haiku-4.5")]
      (is (= {:model "anthropic/claude-haiku-4.5"}
            (openrouter/chat-completion-fields entry
              {:reasoning_effort "low" :verbosity "high"})))))
  (testing "a text-only request gets no structured fields"
    (let [fields (openrouter/chat-completion-fields
                   (card-models/resolve-model "gpt-5.1")
                   {:reasoning_effort "low"})]
      (is (not (contains? fields :response_format)))
      (is (not (get-in fields [:provider :require_parameters]))))))

(deftest structured-chat-fields-require-json-compatible-routes
  (testing "an unpinned model gets JSON mode and parameter-compatible routing"
    (is (= {:model "google/gemini-3-flash-preview"
            :reasoning_effort "low"
            :response_format {:type "json_object"}
            :provider {:require_parameters true}}
          (openrouter/structured-chat-completion-fields
            (card-models/resolve-model "gemini-3-flash")
            {:reasoning_effort "low" :verbosity "high"}))))
  (testing "the GLM provider routing is merged, not replaced"
    (is (= {:model "z-ai/glm-5.3-flash"
            :reasoning_effort "low"
            :response_format {:type "json_object"}
            :provider {:sort "price"
                       :max_price {:prompt 0.15 :completion 0.50}
                       :require_parameters true}}
          (openrouter/structured-chat-completion-fields
            (card-models/resolve-model "glm-5.3-flash")
            {:reasoning_effort "low" :verbosity "high"}))))
  (testing "an incompatible registry entry fails before transport"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
          #"does not support structured JSON"
          (openrouter/structured-chat-completion-fields
            {:id "bad" :openrouter-model "example/bad"
             :supported-parameters #{:reasoning_effort}}
            {})))))

(deftest reasoning-none-is-dropped-only-where-the-provider-refuses-it
  (testing "a mandatory-reasoning entry loses the field"
    ;; Measured 2026-09-08: OpenRouter answers reasoning_effort "none" on these
    ;; with HTTP 400 `Reasoning is mandatory for this endpoint and cannot be
    ;; disabled`. The Settings dropdown offers None, so before this every card
    ;; generation, KG step and assistant turn by such a user failed.
    (doseq [id ["gemini-3.6-flash" "gemini-3.8-flash" "gemini-3.5-flash-lite"
                "glm-5.3-flash"]]
      (let [fields (openrouter/chat-completion-fields
                     (card-models/resolve-model id) {:reasoning_effort "none"})]
        (is (not (contains? fields :reasoning_effort)) id))))
  (testing "the same entry keeps every other effort value"
    (is (= "low" (:reasoning_effort
                   (openrouter/chat-completion-fields
                     (card-models/resolve-model "gemini-3.8-flash")
                     {:reasoning_effort "low"})))))
  (testing "an entry that permits none keeps it"
    ;; Ling 3.0 Flash measured 2026-09-11: "none" returns HTTP 200 with 0
    ;; reasoning tokens on both of its providers, DeepInfra and Novita.
    (doseq [id ["gpt-5.1" "gemini-3-flash" "claude-sonnet-5" "ling-3.0-flash"]]
      (is (= "none" (:reasoning_effort
                      (openrouter/chat-completion-fields
                        (card-models/resolve-model id)
                        {:reasoning_effort "none"})))
        id)))
  (testing "the flagged set is the one measured against OpenRouter"
    (is (= #{"gemini-3.6-flash" "gemini-3.8-flash" "gemini-3.5-flash-lite"
             "glm-5.3-flash"}
          (into #{} (comp (filter :reasoning-mandatory?) (map :id))
            card-models/registry))))
  (testing "the OCR registry carries no reasoning flag, because its lane sends
            no reasoning_effort — ocr.clj builds its body from model-fields
            alone. A flag there would be metadata no code reads."
    (is (empty? (filter :reasoning-mandatory? ocr-models/registry)))))

(deftest a-retired-id-resolves-to-its-successor-in-both-registries
  (testing "the retired id is gone from the picker"
    (doseq [registry [card-models/registry ocr-models/registry]]
      (is (not (some #{"gemini-3.5-flash"} (map :id registry))))))
  (testing "a stored selection runs the successor, not a fallback default"
    (doseq [[resolve effective] [[card-models/resolve-model card-models/effective-id]
                                 [ocr-models/resolve-model ocr-models/effective-id]]]
      (is (= "gemini-3.6-flash" (effective "gemini-3.5-flash")))
      (is (= "google/gemini-3.6-flash"
            (:openrouter-model (resolve "gemini-3.5-flash"))))))
  (testing "an unknown id still resolves to nil, for the caller to reject"
    (is (nil? (card-models/resolve-model "no-such-model")))
    (is (= "no-such-model" (card-models/effective-id "no-such-model"))))
  (testing "a retired entry names no OpenRouter model"
    ;; The redirect is the only way to call one. An entry that kept its slug
    ;; could still be reached by a path that reads the registry directly.
    (doseq [entry (concat card-models/retired ocr-models/retired)]
      (is (nil? (:openrouter-model entry)) (:id entry))
      (is (some? (:label entry)) (:id entry))
      (is (some? (:superseded-by entry)) (:id entry)))))

(deftest every-card-model-supports-json-object-output
  (doseq [entry card-models/registry]
    (is (contains? (:supported-parameters entry) :response_format)
      (:id entry))))

(deftest model-fields-carries-the-pin-only-when-the-entry-has-one
  (testing "an entry without :provider yields the slug alone"
    (is (= {:model "google/gemini-3-flash-preview"}
          (openrouter/model-fields {:id "gemini-3-flash"
                                    :openrouter-model "google/gemini-3-flash-preview"}))))
  (testing "a routed entry passes its provider object verbatim"
    (is (= {:model "z-ai/glm-5.3-flash"
            :provider {:sort "price" :max_price {:prompt 0.15 :completion 0.50}}}
          (openrouter/model-fields
            {:id "glm-5.3-flash"
             :openrouter-model "z-ai/glm-5.3-flash"
             :provider {:sort "price"
                        :max_price {:prompt 0.15 :completion 0.50}}})))))

(deftest every-chat-registry-entry-yields-a-slug
  (testing "both registries, every entry that names an OpenRouter model"
    (doseq [entry (concat card-models/registry ocr-models/registry)
            :when (:openrouter-model entry)]
      (is (string? (:model (openrouter/model-fields entry))) (:id entry)))))

(deftest stream-chunk-metadata-keeps-only-reported-facts
  (is (= {:generation-id "gen-1"
          :resolved-model "provider/model"
          :provider-name "Provider"
          :input-tokens 120
          :output-tokens 35
          :reasoning-tokens 9
          :cached-tokens 80
          :cache-write-tokens 12
          :web-search-requests 1
          :cost-usd 0.0042}
        (openrouter/stream-chunk-metadata
          {:id "gen-1"
           :model "provider/model"
           :provider "Provider"
           :usage {:prompt_tokens 120
                   :completion_tokens 35
                   :cost 0.0042
                   :server_tool_use {:web_search_requests 1}
                   :prompt_tokens_details {:cached_tokens 80
                                           :cache_write_tokens 12}
                   :completion_tokens_details {:reasoning_tokens 9}}})))
  (is (= {} (openrouter/stream-chunk-metadata {:choices []}))))

(deftest native-web-search-support-and-sources-are-explicit
  (is (card-models/native-web-search-supported? "gpt-5.1"))
  (is (not (card-models/native-web-search-supported? "glm-5.3-flash")))
  (is (every? (comp card-models/web-search-supported? :id)
        card-models/registry))
  (is (= {:tools [{:type "openrouter:web_search"
                   :parameters {:engine "auto"}}]
          :tool_choice "required"}
        (openrouter/web-search-fields)))
  (is (= [{:title "OpenRouter docs" :url "https://openrouter.ai/docs"}]
        (openrouter/web-source-links
          [{:type "url_citation"
            :url_citation {:title "OpenRouter docs"
                           :url "https://openrouter.ai/docs"}}
           {:type "url_citation"
            :url_citation {:title "Duplicate"
                           :url "https://openrouter.ai/docs"}}
           {:type "file_citation"
            :file_citation {:file_id "ignored"}}]))))

(deftest streaming-preserves-whitespace-and-publishes-early-metadata
  (let [sse (str "data: {\"id\":\"gen-1\",\"model\":\"provider/model\","
              "\"choices\":[{\"delta\":{\"content\":\"Hello\"}}]}\n\n"
              "data: {\"choices\":[{\"delta\":{\"content\":\" \"}}]}\n\n"
              "data: {\"choices\":[{\"delta\":{\"content\":\"world\"}}],"
              "\"usage\":{\"prompt_tokens\":7,\"completion_tokens\":3,"
              "\"cost\":0.001}}\n\n"
              "data: [DONE]\n\n")
        deltas (atom [])
        metadata (atom [])]
    (with-redefs [http/post
                  (fn [_ _]
                    {:status 200 :headers {}
                     :body (ByteArrayInputStream. (.getBytes sse "UTF-8"))})]
      (let [result (openrouter/chat-completion-stream!
                     "key" {:stream true :messages []} {:feature :test}
                     #(swap! deltas conj %)
                     #(swap! metadata conj %))]
        (is (= ["Hello" " " "world"] @deltas))
        (is (= "Hello world" (:text result)))
        (is (= "gen-1" (:generation-id (first @metadata))))
        (is (= 7 (:input-tokens (:metadata result))))
        (is (= 3 (:output-tokens (:metadata result))))
        (is (= 0.001 (:cost-usd (:metadata result))))))))

(deftest glm-5-3-flash-is-price-capped-in-both-registries
  (testing "the cap is the reason the entry exists at its price"
    (doseq [resolve [card-models/resolve-model ocr-models/resolve-model]]
      (is (= {:model "z-ai/glm-5.3-flash"
              :provider {:sort "price"
                         :max_price {:prompt 0.15 :completion 0.50}}}
            (openrouter/model-fields (resolve "glm-5.3-flash"))))))
  (testing "the cap reaches the wire as max_price, not max-price"
    ;; A hyphen is not a silent overcharge — OpenRouter answers an unknown
    ;; provider key with HTTP 400 `Unrecognized key: "max-price"` (measured
    ;; 2026-09-08). It is loud, but it takes both lanes down at once, and no
    ;; equality assertion above can see it: the map compares equal either way.
    ;; Only the encoded name can.
    (doseq [resolve [card-models/resolve-model ocr-models/resolve-model]]
      (let [encoded (json/generate-string
                      (openrouter/model-fields (resolve "glm-5.3-flash")))]
        (is (str/includes? encoded "\"max_price\"") encoded)
        (is (not (str/includes? encoded "max-price")) encoded)))))

(deftest api-error-carries-the-throttling-facts
  (testing "a 429 reports when a retry may work, not prose alone"
    (let [data (ex-data (openrouter/api-error
                          {:status 429
                           :headers {"Retry-After" "30"
                                     "x-ratelimit-remaining" "0"
                                     "X-RateLimit-Reset" "1788856800"}
                           :body "{\"error\":{\"message\":\"Provider returned error\"}}"}))]
      (is (= 429 (:status data)))
      (is (= 30 (:retry-after-s data)))
      (is (= "0" (get-in data [:rate-limit :remaining])))
      (is (= "1788856800" (get-in data [:rate-limit :reset])))))
  (testing "an HTTP-date Retry-After keeps its raw value and reports no seconds"
    (let [data (ex-data (openrouter/api-error
                          {:status 429
                           :headers {"retry-after" "Wed, 08 Sep 2026 08:41:00 GMT"}
                           :body ""}))]
      (is (nil? (:retry-after-s data)))
      (is (= "Wed, 08 Sep 2026 08:41:00 GMT" (get-in data [:rate-limit :retry-after])))))
  (testing "a response with no throttling headers carries no :rate-limit key"
    (let [data (ex-data (openrouter/api-error {:status 401 :headers {} :body ""}))]
      (is (= 401 (:status data)))
      (is (not (contains? data :rate-limit)))
      (is (not (contains? data :retry-after-s))))))
