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
   [freememo.logging :as log]))

(def ^:private base-url "https://openrouter.ai/api/v1")

(defn- path->feature
  "Coarse feature tag from an OpenRouter `path`, used when the caller supplies no
   `:feature` in ctx. :chat | :transcription | :other."
  [path]
  (cond
    (= path "/chat/completions")     :chat
    (= path "/audio/transcriptions") :transcription
    :else                            :other))

(defn post!
  "POST `body` (a map, JSON-encoded) to `path` under the OpenRouter base URL with
   bearer `api-key`. Returns clj-http's {:status :body}, :body parsed as JSON with
   keyword keys; never throws on HTTP status — the caller maps the error.
   Optional `ctx` {:feature kw :user-id id} enriches the observability signal.
   Pre:  api-key non-blank; path begins with '/'.
   Post: {:status int :body any}. Side effect: one `log/external!` signal per call
         (::call), carrying feature + latency + outcome; a network throw is logged
         :error then re-thrown (contract for HTTP status unchanged)."
  ([api-key path body] (post! api-key path body nil))
  ([api-key path body ctx]
   (let [t-start (System/nanoTime)
         feature (or (:feature ctx) (path->feature path))
         ms      #(long (/ (- (System/nanoTime) t-start) 1000000))]
     (try
       (let [resp (http/post (str base-url path)
                    {:headers {"Authorization" (str "Bearer " api-key)}
                     :content-type :json :as :json :throw-exceptions false
                     :body (json/generate-string body)})]
         (log/external! {:id ::call :feature feature :user-id (:user-id ctx)
                         :status (:status resp)
                         :outcome (if (< (int (:status resp)) 400) :ok :error)
                         :duration-ms (ms)})
         resp)
       (catch Throwable e
         (log/external! {:id ::call :feature feature :user-id (:user-id ctx)
                         :outcome :error :error (.getMessage e) :duration-ms (ms)})
         (throw e))))))

(defn- ok-body
  "Parsed body of a 200 `resp`, else throw ::api-error with the provider message.
   Post violation (non-200 from the API) surfaces here, not to the HTTP layer."
  [resp]
  (let [body (:body resp)]
    (if (= 200 (:status resp))
      body
      (throw (ex-info (or (get-in body [:error :message])
                          (str "OpenRouter HTTP " (:status resp)))
               {:type ::api-error :status (:status resp)})))))

(defn chat-completion!
  "POST /chat/completions with request map `body` (:model, :messages, …).
   Optional `ctx` {:feature kw :user-id id} tags the observability signal.
   Post: the parsed 200 body; throws ::api-error on non-200."
  ([api-key body] (chat-completion! api-key body nil))
  ([api-key body ctx] (ok-body (post! api-key "/chat/completions" body ctx))))

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
  (into #{:freememo.llm-edn/unparseable}
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
