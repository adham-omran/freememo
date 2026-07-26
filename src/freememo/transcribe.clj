(ns freememo.transcribe
  "Audio transcription via OpenRouter (OpenAI Whisper). Reads an audio topic's
   stored bytes, transcribes them, and writes the result into the topic's Quill
   content body.

   Billed against credits like every other AI lane: gated by
   `credits/check-cost-billed-balance!` and charged from OpenRouter's returned
   `usage.cost` via `record-cost-charge!`. It was free until now — the original
   docstring called it a storage-only feature — but it is an LLM call with a
   real per-minute cost, and leaving it unbilled meant the money was invisible
   in the ledger as well as uncharged."
  (:require
   [clojure.string :as str]
   [freememo.credits :as credits]
   [freememo.db :as db]
   [freememo.logging :as log]
   [freememo.openrouter :as openrouter]
   [freememo.settings :as settings]
   [freememo.toasts :as toasts]
   [freememo.user-state :as us]
   [freememo.commands :as commands]
   [missionary.core :as m]
   [taoensso.telemere :as tel])
  (:import
   [missionary Cancelled]
   [java.util Base64]
   [java.util.concurrent Executors]))

(def ^:private model "openai/whisper-1")

(def ^:private timeout-ms 120000)

(defn- mime->format
  "OpenRouter input_audio.format token for an audio MIME (no leading dot).
   Defaults to \"mp3\"."
  [mime]
  (case mime
    "audio/mp4" "m4a"
    "audio/wav" "wav"
    "audio/webm" "webm"
    "audio/ogg" "ogg"
    "audio/flac" "flac"
    "mp3"))

(defn- escape-html [s]
  (-> s
    (str/replace "&" "&amp;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")))

(defn- text->html
  "Wrap Whisper plain text as HTML paragraphs, splitting on blank lines and
   HTML-escaping each segment. Returns \"\" for blank input."
  [text]
  (->> (str/split (or text "") #"\n{2,}")
    (map str/trim)
    (remove str/blank?)
    (map #(str "<p>" (escape-html %) "</p>"))
    (str/join)))

(defn transcribe-topic!
  "Transcribe an audio topic's stored bytes into its content body.
   Pre  : topic-id refers to an audio topic owned by user-id with a topic_files row.
   Post : {:success true} with topics.content set to the transcript HTML, or
          {:success false :error S}. Replaces any existing content.
   Throws ::api-error (openrouter) on a non-200 response — the caller's error
   handler reports it."
  [user-id topic-id]
  (let [api-key (settings/get-openrouter-api-key user-id)
        file-row (db/get-topic-file topic-id)
        gate (credits/check-cost-billed-balance! user-id)]
    (cond
      (str/blank? api-key)
      {:success false :error "OpenRouter API key not configured"}

      (nil? file-row)
      {:success false :error "Audio file not found"}

      ;; Checked before the call, not after: an out-of-credits user must not
      ;; incur provider cost we then cannot charge for. No-op in self-host.
      (not (:ok gate))
      {:success false :error (:error gate)}

      :else
      (let [^bytes audio-bytes (:topic_files/file_data file-row)
            fmt (mime->format (:topic_files/mime_type file-row))
            ;; Auto-detect (nil) is Whisper's default and is usually right, but
            ;; on short or quiet audio it mis-identifies the language and then
            ;; hallucinates fluent text in the one it guessed — measured: a 10 s
            ;; English clip returned as repeated Arabic. The setting is the
            ;; escape hatch; same behaviour as the video pipeline.
            language (settings/get-transcribe-language user-id)
            t-start (System/nanoTime)
            body (openrouter/transcription! api-key
                   (cond-> {:input_audio {:data (.encodeToString (Base64/getEncoder) audio-bytes)
                                          :format fmt}
                            :model model}
                     language (assoc :language language)))
            duration-ms (long (/ (- (System/nanoTime) t-start) 1000000))
            text (:text body)
            cost-usd (get-in body [:usage :cost])]
        (tel/log! {:level :info :id ::transcription
                   :data {:user-id user-id :topic-id topic-id
                          :model model :duration-ms duration-ms
                          :language (or language :auto)
                          :cost-usd cost-usd
                          :chars (count text)}}
          "Whisper transcription")
        ;; Charge the completed call even when it returned no usable text — the
        ;; provider billed us either way. Total: a billing failure logs
        ;; ::credit-charge-failed and returns nil, never discarding the result.
        (credits/record-cost-charge! user-id :transcribe model cost-usd)
        (if (str/blank? text)
          {:success false :error "Transcription returned no text"}
          (do (db/update-topic-content! topic-id (text->html text))
              {:success true}))))))

(defonce ^:private executor (Executors/newFixedThreadPool 2))

(defn start-transcribe!
  "Launch transcription as a Missionary task on a bounded executor.
   Adds topic-id to the per-user :transcribing-topics set for the duration;
   bumps :refresh on success (the editor reloads reactively); pushes an error
   toast on failure or timeout. Returns nil immediately."
  [user-id topic-id]
  (swap! (us/get-atom user-id :transcribing-topics) conj topic-id)
  (log/log-info (str "Transcription started topic-id=" topic-id))
  ((m/timeout
     (m/via executor (transcribe-topic! user-id topic-id))
     timeout-ms
     {:success false :error "Transcription timed out after 120 seconds"})
   (fn [result]
     (if (:success result)
       (do (log/log-info (str "Transcription complete topic-id=" topic-id))
           (commands/bump! user-id :transcribe))
       (do (log/log-warn (str "Transcription failed topic-id=" topic-id " error=" (:error result)))
           (toasts/push! user-id {:level :error :message (:error result)})))
     (swap! (us/get-atom user-id :transcribing-topics) disj topic-id))
   (fn [e]
     (when-not (instance? Cancelled e)
       (tel/error! {:id ::transcribe :data {:user-id user-id :topic-id topic-id}} e)
       (toasts/push! user-id {:level :error :message (.getMessage e)}))
     (swap! (us/get-atom user-id :transcribing-topics) disj topic-id)))
  nil)
