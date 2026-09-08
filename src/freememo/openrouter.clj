(ns freememo.openrouter
  "Single transport for OpenRouter calls (chat completions + audio transcription).
   base-url + bearer auth live here only. `post!` returns HTTP status as data
   (never throws on 4xx/5xx); the typed helpers map a non-200 to an ::api-error
   ex-info carrying the provider's error message.

   `message-content` is the second gate: a 200 whose body carries no usable
   assistant content is a distinct failure class from a transport error, and the
   provider tells you which one (refusal, truncation, filter, silent empty) in
   fields no caller was reading. It classifies, logs the shape, and throws a
   typed cause; `retryable-cause?` is the single owner of which causes a fresh
   identical request may resolve."
  (:require
   [clj-http.client :as http]
   [cheshire.core :as json]
   [clojure.string :as str]
   [taoensso.telemere :as tel]
   [freememo.logging :as log])
  (:import
   [java.io BufferedReader InputStreamReader]
   [java.nio.charset StandardCharsets]))

(def ^:private base-url "https://openrouter.ai/api/v1")

(defn- path->feature
  "Coarse feature tag from an OpenRouter `path`, used when the caller supplies no
   `:feature` in ctx. :chat | :transcription | :other."
  [path]
  (cond
    (= path "/chat/completions")     :chat
    (= path "/audio/transcriptions") :transcription
    :else                            :other))

(def ^:private error-body-max
  "Characters of a non-JSON error body kept in the message. A proxy or CDN
   failure answers with a whole HTML page, and that page in a log line buries
   every other signal in the file."
  500)

(defn- provider-message
  "The provider's own error text from a non-2xx `body`, or nil.

   The decode is the point. clj-http parses JSON only for unexceptional statuses
   (`:coerce` defaults to `:unexceptional`), so an error body arrives here as a
   String — and `(get-in \"…\" [:error :message])` is nil, not a message. Every
   4xx therefore reported its status code and nothing else, which is how an
   invalid model slug reached production as a bare `OpenRouter HTTP 400`.
   Pre:  `body` is clj-http's coerced body — a parsed map or a String.
   Post: a non-blank string, or nil when the body carries neither a message nor
         any text; a non-JSON body yields its own text, truncated."
  [body]
  (let [parsed (if (string? body)
                 (try (json/parse-string body true) (catch Exception _ nil))
                 body)
        message (when (map? parsed) (get-in parsed [:error :message]))
        text (when (string? body) (str/trim body))]
    (cond
      (not (str/blank? (str message))) (str message)
      (str/blank? text) nil
      (> (count text) error-body-max) (str (subs text 0 error-body-max) "…")
      :else text)))

(defn- header-value
  "Value of `header-name` in a clj-http `headers` map, matched without regard to
   case. Single owner of that lookup — clj-http lowercases header names, but a
   `with-redefs`'d http/post in a test supplies whatever the test author wrote,
   and both string and keyword keys appear.
   Pre:  `header-name` is lowercase. Post: the value, or nil."
  [headers header-name]
  (some (fn [[k v]]
          (when (and (or (string? k) (keyword? k))
                  (= header-name (str/lower-case (name k))))
            v))
    headers))

(def ^:private rate-limit-headers
  "The throttling headers a non-200 carries, as fact key -> header name. Read
   together because they answer the question one alone cannot: whether OUR key
   hit a cap, or whether the upstream provider threw the 429. A 429 whose
   :remaining is above zero came from the provider, not from OpenRouter."
  {:retry-after "retry-after"
   :limit       "x-ratelimit-limit"
   :remaining   "x-ratelimit-remaining"
   :reset       "x-ratelimit-reset"})

(defn- retry-after-seconds
  "Seconds to wait from a `Retry-After` value, or nil.
   RFC 9110 permits a seconds count or an HTTP date. Only the count is read: a
   date needs a clock comparison, and a wrong one here would schedule a retry.
   The raw value stays in :rate-limit either way, so nothing is lost.
   Pre:  `v` is a header value or nil. Post: a non-negative long, or nil."
  [v]
  (let [t (str/trim (str v))]
    (when (re-matches #"\d+" t)
      (parse-long t))))

(defn api-error
  "The exception for a non-200 OpenRouter `resp`, carrying the provider's own
   message. Single owner of that mapping — `ok-body` and the OCR lane both raise
   through it, so a status-only message cannot come back at one call site and
   not the other.
   Pre:  `resp` is a clj-http response whose :status is not 200. Its :headers are
         the response's own — a caller that rebuilds the map to re-read a
         streamed body must carry them over, or the throttling facts are lost.
   Post: an ExceptionInfo whose message is the provider's text when it sent any,
         else \"OpenRouter HTTP <status>\"; ex-data {:type ::api-error :status},
         plus :rate-limit and :retry-after-s when the response reported them.
         :status is the field to dispatch on — the message is prose and changes
         with the provider. A 429 read as bare prose says \"Provider returned
         error\", which names neither the status nor when a retry may work."
  [resp]
  (let [headers (:headers resp)
        facts (into {} (keep (fn [[fact header-name]]
                               (when-let [v (header-value headers header-name)]
                                 [fact v])))
                rate-limit-headers)
        retry-s (some-> (:retry-after facts) retry-after-seconds)]
    (ex-info (or (provider-message (:body resp))
               (str "OpenRouter HTTP " (:status resp)))
      (cond-> {:type ::api-error :status (:status resp)}
        (seq facts) (assoc :rate-limit facts)
        retry-s (assoc :retry-after-s retry-s)))))

(defn post!
  "POST `body` (a map, JSON-encoded) to `path` under the OpenRouter base URL with
   bearer `api-key`. Returns clj-http's {:status :body}, :body parsed as JSON with
   keyword keys; never throws on HTTP status — the caller maps the error.
   Optional `ctx` {:feature kw :user-id id} enriches the observability signal.
   Pre:  api-key non-blank; path begins with '/'.
   Post: {:status int :body any}. Side effect: one `log/external!` signal per call
         (::call), carrying feature + latency + outcome + the provider that served
         the call (a pinned model's line names the pin's target), plus the
         provider's error text on a failing status — the operator reads that line first, and a
         caller that maps the response itself must not be the only way to see the
         cause; a network throw is logged :error then re-thrown (contract for HTTP
         status unchanged)."
  ([api-key path body] (post! api-key path body nil))
  ([api-key path body ctx]
   (let [t-start (System/nanoTime)
         feature (or (:feature ctx) (path->feature path))
         ms      #(long (/ (- (System/nanoTime) t-start) 1000000))]
     (try
       (let [resp (http/post (str base-url path)
                    {:headers {"Authorization" (str "Bearer " api-key)}
                     :content-type :json :as :json :throw-exceptions false
                     :body (json/generate-string body)})
             failed? (>= (int (:status resp)) 400)
             served-by (when (map? (:body resp)) (:provider (:body resp)))]
         (log/external! (cond-> {:id ::call :feature feature :user-id (:user-id ctx)
                                 :status (:status resp)
                                 :outcome (if failed? :error :ok)
                                 :duration-ms (ms)}
                          served-by (assoc :provider served-by)
                          failed? (assoc :error (provider-message (:body resp)))))
         resp)
       (catch Throwable e
         (log/external! {:id ::call :feature feature :user-id (:user-id ctx)
                         :outcome :error :error (.getMessage e) :duration-ms (ms)})
         (throw e))))))

(defn- ok-body
  "Parsed body of a 200 `resp`, else throw `api-error` with the provider message.
   Post violation (non-200 from the API) surfaces here, not to the HTTP layer."
  [resp]
  (if (= 200 (:status resp))
    (:body resp)
    (throw (api-error resp))))

(defn model-fields
  "The chat-completion body fields that select the model for a registry `entry`
   (freememo.card-models or freememo.ocr-models): `:model` is the OpenRouter
   slug, and `:provider` is the entry's provider-routing object when it pins
   one. The one place an entry becomes request fields, so no lane can send the
   slug and drop the pin.
   Pre:  entry is a resolved registry map with a non-blank :openrouter-model.
         :provider, when present, is OpenRouter's `provider` request object as a
         map (e.g. {:only [\"z-ai\"]}) and is sent verbatim.
   Post: {:model slug} or {:model slug :provider m}; no other keys."
  [entry]
  (cond-> {:model (:openrouter-model entry)}
    (:provider entry) (assoc :provider (:provider entry))))

(defn chat-completion-fields
  "Build model and supported optional fields for one chat completion.
   Pre:  entry is a resolved model registry entry. `requested-parameters` is a
         map of optional OpenRouter fields.
   Post: model-fields plus only keys declared in entry :supported-parameters.
         This function never adds structured-output routing fields."
  [entry requested-parameters]
  (merge (model-fields entry)
    (select-keys requested-parameters (:supported-parameters entry))))

(defn structured-chat-completion-fields
  "Build model fields for a strict JSON-object chat completion.
   Pre:  entry declares :response_format in :supported-parameters.
   Post: chat-completion-fields plus JSON mode and provider.require_parameters.
         Existing provider routing fields, including :only pins, are preserved.
   A missing response_format capability is a caller or registry bug."
  [entry requested-parameters]
  (when-not (contains? (:supported-parameters entry) :response_format)
    (throw (ex-info "Model does not support structured JSON output"
             {:model-id (:id entry)
              :openrouter-model (:openrouter-model entry)})))
  (-> (chat-completion-fields entry requested-parameters)
    (assoc :response_format {:type "json_object"})
    (update :provider #(assoc (or % {}) :require_parameters true))))

(defn chat-completion!
  "POST /chat/completions with request map `body` (:model, :messages, …).
   Optional `ctx` {:feature kw :user-id id} tags the observability signal.
   Post: the parsed 200 body; throws ::api-error on non-200."
  ([api-key body] (chat-completion! api-key body nil))
  ([api-key body ctx] (ok-body (post! api-key "/chat/completions" body ctx))))

(defn stream-chunk-metadata
  "Normalize provider facts carried by one OpenRouter streaming chunk.
   Pre: chunk is a parsed SSE JSON object. Post: a map containing only reported
   facts. Missing values stay absent, so callers never confuse zero with unknown."
  [chunk]
  (let [usage (:usage chunk)
        prompt-details (:prompt_tokens_details usage)
        completion-details (:completion_tokens_details usage)
        router (:openrouter_metadata chunk)]
    (cond-> {}
      (:id chunk) (assoc :generation-id (:id chunk))
      (:model chunk) (assoc :resolved-model (:model chunk))
      (or (:provider chunk) (:provider_name router))
      (assoc :provider-name (or (:provider chunk) (:provider_name router)))
      (some? (:prompt_tokens usage)) (assoc :input-tokens (:prompt_tokens usage))
      (some? (:completion_tokens usage)) (assoc :output-tokens (:completion_tokens usage))
      (some? (or (:reasoning_tokens usage) (:reasoning_tokens completion-details)))
      (assoc :reasoning-tokens
        (or (:reasoning_tokens usage) (:reasoning_tokens completion-details)))
      (some? (:cached_tokens prompt-details))
      (assoc :cached-tokens (:cached_tokens prompt-details))
      (some? (:cache_write_tokens prompt-details))
      (assoc :cache-write-tokens (:cache_write_tokens prompt-details))
      (some? (:cost usage)) (assoc :cost-usd (:cost usage)))))

(defn- parse-sse-data
  "Parse one SSE data payload. [DONE] returns nil.
   A provider error becomes an ExceptionInfo so the run fails with its message."
  [payload]
  (when-not (= "[DONE]" payload)
    (let [chunk (json/parse-string payload true)]
      (if-let [error (:error chunk)]
        (throw (ex-info (or (:message error) "OpenRouter stream failed.")
                 {:type ::stream-error :provider-error error}))
        chunk))))

(defn- generation-id-header [headers]
  (header-value headers "x-generation-id"))

(defn chat-completion-stream!
  "Stream one chat completion and drain it to its terminal SSE event.

   `on-delta` receives each non-empty text delta. `on-metadata` receives each
   newly reported fact and the final merged facts. The caller decides whether a
   delta still belongs in local state. This function never treats local Stop as
   remote cancellation.

   Pre: body contains :stream true. Callbacks do not block indefinitely.
   Post: returns {:text :metadata :headers}. Every accepted SSE delta appears in
   :text once. Non-200 responses throw api-error. The response stream closes.
   Invariant: the reader continues through [DONE], so terminal usage remains
   available after local delivery stops."
  [api-key body ctx on-delta on-metadata]
  (let [started-ns (System/nanoTime)
        first-token-ns (volatile! nil)
        elapsed-ms #(long (/ (- (System/nanoTime) started-ns) 1000000))]
    (try
      (let [response (http/post (str base-url "/chat/completions")
                       {:headers {"Authorization" (str "Bearer " api-key)
                                  "X-OpenRouter-Metadata" "enabled"}
                        :content-type :json
                        :as :stream
                        :throw-exceptions false
                        :body (json/generate-string (assoc body :stream true))})]
        (when-not (= 200 (:status response))
          (with-open [reader (BufferedReader.
                               (InputStreamReader. (:body response) StandardCharsets/UTF_8))]
            (throw (api-error (assoc response :body (slurp reader))))))
        (with-open [reader (BufferedReader.
                             (InputStreamReader. (:body response) StandardCharsets/UTF_8))]
          (loop [text (StringBuilder.)
                 metadata (cond-> {}
                            (generation-id-header (:headers response))
                            (assoc :generation-id
                              (generation-id-header (:headers response))))]
            (if-let [line (.readLine reader)]
              (if (str/starts-with? line "data:")
                (let [payload (str/trim (subs line 5))]
                  (if-let [chunk (parse-sse-data payload)]
                    (let [delta (-> chunk :choices first :delta :content)
                          chunk-metadata (stream-chunk-metadata chunk)]
                      (when (and (string? delta) (not (empty? delta)))
                        (when-not @first-token-ns
                          (vreset! first-token-ns (System/nanoTime)))
                        (.append text delta)
                        (on-delta delta))
                      (when (seq chunk-metadata)
                        (on-metadata chunk-metadata))
                      (recur text (merge metadata chunk-metadata)))
                    (let [facts (cond-> metadata
                                  @first-token-ns
                                  (assoc :first-token-ms
                                    (long (/ (- (long @first-token-ns) started-ns) 1000000)))
                                  true (assoc :provider-latency-ms (elapsed-ms)))]
                      (on-metadata facts)
                      (log/external! {:id ::stream-call :feature (or (:feature ctx) :assistant)
                                      :user-id (:user-id ctx) :outcome :ok
                                      :duration-ms (elapsed-ms)
                                      :provider (:provider-name facts)})
                      {:text (str text) :metadata facts :headers (:headers response)})))
                (recur text metadata))
              (let [facts (cond-> metadata
                            @first-token-ns
                            (assoc :first-token-ms
                              (long (/ (- (long @first-token-ns) started-ns) 1000000)))
                            true (assoc :provider-latency-ms (elapsed-ms)))]
                (on-metadata facts)
                {:text (str text) :metadata facts :headers (:headers response)})))))
      (catch Throwable error
        ;; :status and :retry-after-s, not the message alone. `post!` has logged
        ;; its status since it was written; this lane did not, so a 429 reached
        ;; the operator as Z.ai's prose "Provider returned error" and the status
        ;; was reachable only by opening the caller's stacktrace.
        (let [data (ex-data error)]
          (log/external! (cond-> {:id ::stream-call :feature (or (:feature ctx) :assistant)
                                  :user-id (:user-id ctx) :outcome :error
                                  :duration-ms (elapsed-ms) :error (ex-message error)}
                           (:status data) (assoc :status (:status data))
                           (:retry-after-s data) (assoc :retry-after-s (:retry-after-s data))
                           (:rate-limit data) (assoc :rate-limit (:rate-limit data)))))
        (throw error)))))

(defn generation-metadata!
  "Get finalized OpenRouter metadata for generation-id.
   Post: normalized provider facts. Non-200 responses throw api-error."
  [api-key generation-id ctx]
  (let [response (http/get (str base-url "/generation")
                   {:headers {"Authorization" (str "Bearer " api-key)}
                    :query-params {:id generation-id}
                    :as :json :throw-exceptions false})]
    (when-not (= 200 (:status response))
      (throw (api-error response)))
    (let [generation (:data (:body response))]
      (cond-> {}
        (:id generation) (assoc :generation-id (:id generation))
        (:model generation) (assoc :resolved-model (:model generation))
        (:provider_name generation) (assoc :provider-name (:provider_name generation))
        (some? (:tokens_prompt generation)) (assoc :input-tokens (:tokens_prompt generation))
        (some? (:tokens_completion generation)) (assoc :output-tokens (:tokens_completion generation))
        (some? (:native_tokens_reasoning generation))
        (assoc :reasoning-tokens (:native_tokens_reasoning generation))
        (some? (:native_tokens_cached generation))
        (assoc :cached-tokens (:native_tokens_cached generation))
        (some? (:total_cost generation)) (assoc :cost-usd (:total_cost generation))
        (some? (:latency generation)) (assoc :provider-latency-ms (:latency generation))
        (some? (:generation_time generation)) (assoc :generation-ms (:generation_time generation))))))

(def ^:private refusal-finish-reasons
  "finish_reason / native_finish_reason values meaning the model declined.
   Retrying resends identical input and earns the identical refusal, so these
   are terminal."
  #{"content_filter" "SAFETY" "PROHIBITED_CONTENT" "BLOCKLIST"})

(def ^:private truncation-finish-reasons
  "finish_reason / native_finish_reason values meaning generation stopped at a
   token ceiling. Reasoning models can burn the whole budget on thoughts and
   emit no content at all; a fresh attempt may fit."
  #{"length" "MAX_TOKENS"})

(def ^:private no-content-causes
  "Every cause `no-content-cause` can classify: its learner-facing message and
   whether a fresh identical request may resolve it. One map rather than three
   parallel lists, so a new cause cannot arrive with a missing message or an
   undecided retry policy. Ordered to mirror the classifier's precedence.

   Messages are authored here rather than pattern-matched downstream: the
   incident that motivated this gate reported a nil content to the learner as an
   unreadable *parse*, so the message must name what actually happened."
  {::refused       {:message "The model declined to generate from this content."
                    :retryable? false}
   ::no-choices    {:message "The model returned no response."
                    :retryable? true}
   ::truncated     {:message "The model's response was cut off before it produced content."
                    :retryable? true}
   ::empty-content {:message "The model returned an empty response."
                    :retryable? true}})

(defn- no-content-cause
  "Cause keyword for a 200 chat-completion `body` that yielded no usable content.
   Precedence (fixed — providers do not guarantee these are exclusive): refusal
   beats a missing choice beats truncation beats a silent empty.
   Pre:  `body` is a parsed 200 body whose message content is blank/absent.
   Post: a key of `no-content-causes`. Returning anything else is a design bug;
         `message-content` falls back to a generic message rather than asserting,
         because an AssertionError is not an Exception and would escape the
         ExceptionInfo catches in cards/kg-llm."
  [body]
  (let [choice (-> body :choices first)
        finish [(:finish_reason choice) (:native_finish_reason choice)]]
    (cond
      (or (not (str/blank? (str (-> choice :message :refusal))))
        (some refusal-finish-reasons finish))
      ::refused

      (or (nil? choice) (nil? (:message choice)) (some? (:error body)))
      ::no-choices

      (some truncation-finish-reasons finish)
      ::truncated

      :else ::empty-content)))

(defn message-content
  "The assistant message content of a 200 chat-completion `body`.
   Optional `ctx` {:feature kw :user-id id} enriches the diagnostic signal.
   Pre:  `body` is a parsed 200 body from `chat-completion!`.
   Post: a non-blank string. Otherwise throws ex-info whose ex-data carries
         :type (a `no-content-cause` keyword) plus the response-shape fields
         that identify the cause after the fact — finish reasons, refusal text,
         choice count, usage. Side effect: one ::no-content :warn signal per
         throw, so a recurrence is diagnosable without the raw body."
  ([body] (message-content body nil))
  ([body ctx]
   (let [content (-> body :choices first :message :content)]
     ;; string? guards the post-condition: a structured-content provider (parts
     ;; vector) is no-content here rather than a surprise type downstream.
     (if (and (string? content) (not (str/blank? content)))
       content
       (let [choice (-> body :choices first)
             cause  (no-content-cause body)
             data   {:type cause
                     :finish-reason (:finish_reason choice)
                     :native-finish-reason (:native_finish_reason choice)
                     :refusal (-> choice :message :refusal)
                     :choices (count (:choices body))
                     :api-error (get-in body [:error :message])
                     :usage (:usage body)
                     :feature (:feature ctx)
                     :user-id (:user-id ctx)}]
         (tel/log! {:level :warn :id ::no-content :data data}
           (str "OpenRouter 200 with no usable content (" (name cause) ")"))
         ;; Generic fallback message only if a cause has no no-content-causes
         ;; entry — see that fn's Post.
         (throw (ex-info (get-in no-content-causes [cause :message]
                           "The model returned no usable response.")
                  data)))))))

(def ^:private retryable-causes
  "Causes a fresh identical request may resolve — derived from
   `no-content-causes` so the retry policy has one home, plus the parser's own
   cause written as a literal (the transport namespace must not depend on the
   parser). Every untyped failure (::api-error, missing key, empty input) is
   absent and therefore terminal: retrying a 401 three times spends three calls
   to learn nothing."
  (into #{:freememo.llm-json/unparseable}
    (keep (fn [[cause {:keys [retryable?]}]] (when retryable? cause)))
    no-content-causes))

(defn retryable-cause?
  "True when `e` carries a cause a retry may resolve. Single owner of that
   mapping — callers dispatch on this, never on their own cause set.
   Pre:  `e` is a Throwable or nil. Post: boolean."
  [e]
  (contains? retryable-causes (:type (ex-data e))))

(defn transcription!
  "POST /audio/transcriptions with request map `body` (:input_audio, :model, …).
   Optional `ctx` {:feature kw :user-id id} tags the observability signal.
   Post: the parsed 200 body ({:text :usage …}); throws ::api-error on non-200."
  ([api-key body] (transcription! api-key body nil))
  ([api-key body ctx] (ok-body (post! api-key "/audio/transcriptions" body ctx))))
