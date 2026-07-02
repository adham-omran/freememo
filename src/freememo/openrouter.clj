(ns freememo.openrouter
  "Single transport for OpenRouter calls (chat completions + audio transcription).
   base-url + bearer auth live here only. `post!` returns HTTP status as data
   (never throws on 4xx/5xx); the typed helpers map a non-200 to an ::api-error
   ex-info carrying the provider's error message."
  (:require
   [clj-http.client :as http]
   [cheshire.core :as json]))

(def ^:private base-url "https://openrouter.ai/api/v1")

(defn post!
  "POST `body` (a map, JSON-encoded) to `path` under the OpenRouter base URL with
   bearer `api-key`. Returns clj-http's {:status :body}, :body parsed as JSON with
   keyword keys; never throws on HTTP status — the caller maps the error.
   Pre:  api-key non-blank; path begins with '/'.
   Post: {:status int :body any}."
  [api-key path body]
  (http/post (str base-url path)
    {:headers {"Authorization" (str "Bearer " api-key)}
     :content-type :json :as :json :throw-exceptions false
     :body (json/generate-string body)}))

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
   Post: the parsed 200 body; throws ::api-error on non-200."
  [api-key body]
  (ok-body (post! api-key "/chat/completions" body)))

(defn transcription!
  "POST /audio/transcriptions with request map `body` (:input_audio, :model, …).
   Post: the parsed 200 body ({:text :usage …}); throws ::api-error on non-200."
  [api-key body]
  (ok-body (post! api-key "/audio/transcriptions" body)))
