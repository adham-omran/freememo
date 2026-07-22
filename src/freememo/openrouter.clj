(ns freememo.openrouter
  "Single transport for OpenRouter calls (chat completions + audio transcription).
   base-url + bearer auth live here only. `post!` returns HTTP status as data
   (never throws on 4xx/5xx); the typed helpers map a non-200 to an ::api-error
   ex-info carrying the provider's error message."
  (:require
   [clj-http.client :as http]
   [cheshire.core :as json]
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

(defn transcription!
  "POST /audio/transcriptions with request map `body` (:input_audio, :model, …).
   Optional `ctx` {:feature kw :user-id id} tags the observability signal.
   Post: the parsed 200 body ({:text :usage …}); throws ::api-error on non-200."
  ([api-key body] (transcription! api-key body nil))
  ([api-key body ctx] (ok-body (post! api-key "/audio/transcriptions" body ctx))))
