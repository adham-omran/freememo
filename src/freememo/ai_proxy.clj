(ns freememo.ai-proxy
  "The freememo.net proxy for a desktop's AI calls
   (plans/electron-local-db-sync.md 4.10, D7).

   A desktop sends each OpenRouter request here, with its desktop token, and
   this namespace forwards it to openrouter.ai with the platform key. Three
   things happen here and nowhere on the desktop (4.10.2):
   - the license gate (13.4, O13: AI needs a covering license or the trial),
   - the credit gate and the charge, from the usage.cost OpenRouter reports,
   - the model allowlist, so a desktop cannot name a model the server refuses.

   A streamed completion is relayed line by line as it arrives (4.10.3), and
   billed after its last line. When the stream never reported a cost, the
   generation's own record is read instead, as the assistant does."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [cheshire.core :as json]
            [clj-http.client :as http]
            [ring.core.protocols :as ring-protocols]
            [taoensso.telemere :as tel]
            [freememo.card-models :as card-models]
            [freememo.config :as config]
            [freememo.credits :as credits]
            [freememo.ocr :as ocr]
            [freememo.ocr-models :as ocr-models]
            [freememo.openrouter :as openrouter]
            [freememo.settings :as settings]
            [freememo.sync-server :as sync-server])
  (:import [java.io BufferedReader InputStreamReader OutputStream]
           [java.nio.charset StandardCharsets]))

(def ^:private upstream "https://openrouter.ai/api/v1")

(def ^:private fixed-models
  "Models the app calls by slug rather than through a registry."
  #{"openai/whisper-1" "google/gemini-3-flash-preview"})

(defn allowed-models
  "OpenRouter slugs this deployment lets a desktop call: the allowed card and
   OCR registry entries, and the fixed transcription and OCR-plugin models."
  []
  (let [card-ids (set (settings/card-model-ids))
        ocr-ids (set (ocr/allowed-ocr-model-ids))]
    (into fixed-models
      (concat (keep #(when (card-ids (:id %)) (:openrouter-model %)) card-models/registry)
        (keep #(when (ocr-ids (:id %)) (:openrouter-model %)) ocr-models/registry)))))

(defn- json-response [status body]
  {:status status :headers {"Content-Type" "application/json"} :body (json/generate-string body)})

(defn- charge!
  "Bill one proxied call. A call with no reported cost is looked up by its
   generation id; a call with neither is logged, because it went unbilled."
  [user-id path model usage generation-id]
  (let [cost (or (:cost usage)
               (when generation-id
                 (some (fn [delay-ms]
                         (Thread/sleep (long delay-ms))
                         (try (:cost-usd (openrouter/generation-metadata!
                                           (config/platform-openrouter-api-key) generation-id nil))
                              (catch Exception _ nil)))
                   [250 1000 3000])))]
    (if (number? cost)
      (credits/record-cost-charge! user-id (keyword "desktop" (str/replace path "/" "."))
        model cost (when usage [(openrouter/usage-tokens usage)]))
      (tel/log! {:level :warn :id ::unbilled :data {:user-id user-id :path path :model model
                                                    :generation-id generation-id}}
        "A proxied desktop AI call reported no cost and went unbilled"))))

(defn- relay-stream
  "A Ring body that copies the upstream SSE stream to the desktop as it
   arrives, then bills from the usage the stream reported."
  [upstream-body user-id path model generation-id]
  (reify ring-protocols/StreamableResponseBody
    (write-body-to-stream [_ _ out]
      (let [usage (volatile! nil)
            gen-id (volatile! generation-id)]
        (try
          (with-open [^BufferedReader reader (BufferedReader.
                                               (InputStreamReader. ^java.io.InputStream upstream-body
                                                 StandardCharsets/UTF_8))
                      ^OutputStream out out]
            (loop []
              (when-let [line (.readLine reader)]
                (.write out (.getBytes (str line "\n") StandardCharsets/UTF_8))
                (if (str/blank? line) (.flush out))
                (when (str/starts-with? line "data:")
                  (let [data (str/trim (subs line 5))]
                    (when-not (= "[DONE]" data)
                      (when-let [chunk (try (json/parse-string data true) (catch Exception _ nil))]
                        (when (:usage chunk) (vreset! usage (:usage chunk)))
                        (when (:id chunk) (vswap! gen-id #(or % (:id chunk))))))))
                (recur))))
          (finally
            (charge! user-id path model @usage @gen-id)))))))

(defn- forward
  "POST `body` to OpenRouter at `path` and answer the desktop with its reply."
  [user-id path body]
  (let [model (:model body)
        stream? (true? (:stream body))
        response (http/post (str upstream path)
                   {:headers {"Authorization" (str "Bearer " (config/platform-openrouter-api-key))
                              "X-OpenRouter-Metadata" "enabled"}
                    :content-type :json
                    :as (if stream? :stream :byte-array)
                    :throw-exceptions false
                    :body (json/generate-string body)})
        generation-id (get-in response [:headers "x-generation-id"])
        headers (cond-> {"Content-Type" (get-in response [:headers "content-type"] "application/json")}
                  generation-id (assoc "X-Generation-Id" generation-id))]
    (cond
      (not= 200 (:status response))
      {:status (:status response) :headers headers
       :body (if stream? (:body response) (io/input-stream ^bytes (:body response)))}

      stream?
      {:status 200 :headers headers
       :body (relay-stream (:body response) user-id path model generation-id)}

      :else
      (let [bytes (:body response)
            parsed (try (json/parse-string (String. ^bytes bytes StandardCharsets/UTF_8) true)
                        (catch Exception _ nil))]
        (charge! user-id path model (:usage parsed) (or generation-id (:id parsed)))
        {:status 200 :headers headers :body (io/input-stream ^bytes bytes)}))))

(defn- proxied-path [request]
  (subs (:uri request) (count "/api/desktop/ai")))

(defn post-handler
  "POST /api/desktop/ai/chat/completions and /audio/transcriptions."
  [request]
  (sync-server/with-access request :ai
    (fn [{:keys [user-id]} raw-body _]
      (let [path (proxied-path request)
            body (some-> raw-body walk/keywordize-keys)
            gate (credits/check-cost-billed-balance! user-id)]
        (cond
          (not (#{"/chat/completions" "/audio/transcriptions"} path))
          (json-response 404 {:error "not-found"})
          (nil? body) (json-response 400 {:error "bad-request"})
          (not (contains? (allowed-models) (:model body)))
          (json-response 403 {:error {:message (str "The model " (:model body)
                                                 " is not available to the desktop app.")}})
          (not (:ok gate)) (json-response 402 {:error {:message (:error gate)}})
          :else (forward user-id path body))))))

(defn get-handler
  "GET /api/desktop/ai/models and /generation. The catalog needs no license;
   a generation record does, because it belongs to a billed call."
  [request]
  (let [path (proxied-path request)]
    (sync-server/with-access request (if (= path "/models") :meta :ai)
      (fn [_ _ _]
        (case path
          "/models" (let [r (http/get (str upstream "/models") {:as :byte-array :throw-exceptions false})]
                      {:status (:status r) :headers {"Content-Type" "application/json"}
                       :body (io/input-stream ^bytes (:body r))})
          "/generation" (let [r (http/get (str upstream "/generation")
                                  {:headers {"Authorization" (str "Bearer " (config/platform-openrouter-api-key))}
                                   :query-params {:id (get-in request [:params "id"])}
                                   :as :byte-array :throw-exceptions false})]
                          {:status (:status r) :headers {"Content-Type" "application/json"}
                           :body (io/input-stream ^bytes (:body r))})
          (json-response 404 {:error "not-found"}))))))
