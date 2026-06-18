(ns freememo.transcribe
  "Audio transcription via OpenAI Whisper. Reads an audio topic's stored bytes,
   transcribes them, and writes the result into the topic's Quill content body.
   Not billed against credits (storage-only feature); self-host uses the user's
   own key."
  (:require
   [clojure.string :as str]
   [freememo.db :as db]
   [freememo.logging :as log]
   [freememo.settings :as settings]
   [freememo.toasts :as toasts]
   [freememo.user-state :as us]
   [missionary.core :as m]
   [taoensso.telemere :as tel]
   [wkok.openai-clojure.api :as api])
  (:import
   [missionary Cancelled]
   [java.io File]
   [java.util.concurrent Executors]))

(def ^:private model "whisper-1")

(def ^:private timeout-ms 120000)

(defn- mime->ext
  "File extension (with leading dot) for an audio MIME, so the temp file Whisper
   reads carries a format hint. Defaults to .mp3."
  [mime]
  (case mime
    "audio/mp4" ".m4a"
    "audio/wav" ".wav"
    "audio/webm" ".webm"
    "audio/ogg" ".ogg"
    "audio/flac" ".flac"
    ".mp3"))

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
   Invariant: the temp file written for the API call is always deleted."
  [user-id topic-id enc-key]
  (let [api-key (settings/get-openai-api-key user-id enc-key)
        file-row (db/get-topic-file topic-id)]
    (cond
      (str/blank? api-key)
      {:success false :error "OpenAI API key not configured"}

      (nil? file-row)
      {:success false :error "Audio file not found"}

      :else
      (let [^bytes audio-bytes (:topic_files/file_data file-row)
            ext (mime->ext (:topic_files/mime_type file-row))
            tmp (File/createTempFile "transcribe" ext)]
        (try
          (with-open [out (clojure.java.io/output-stream tmp)]
            (.write out audio-bytes))
          (let [t-start (System/nanoTime)
                resp (api/create-transcription {:file tmp :model model}
                                               {:api-key api-key})
                duration-ms (long (/ (- (System/nanoTime) t-start) 1000000))
                text (:text resp)]
            (tel/log! {:level :info :id ::transcription
                       :data {:user-id user-id :topic-id topic-id
                              :model model :duration-ms duration-ms
                              :chars (count text)}}
              "Whisper transcription")
            (if (str/blank? text)
              {:success false :error "Transcription returned no text"}
              (do (db/update-topic-content! topic-id (text->html text))
                  {:success true})))
          (finally
            (.delete tmp)))))))

(defonce ^:private executor (Executors/newFixedThreadPool 2))

(defn start-transcribe!
  "Launch transcription as a Missionary task on a bounded executor.
   Adds topic-id to the per-user :transcribing-topics set for the duration;
   bumps :refresh on success (the editor reloads reactively); pushes an error
   toast on failure or timeout. Returns nil immediately."
  [user-id topic-id enc-key]
  (swap! (us/get-atom user-id :transcribing-topics) conj topic-id)
  (log/log-info (str "Transcription started topic-id=" topic-id))
  ((m/timeout
     (m/via executor (transcribe-topic! user-id topic-id enc-key))
     timeout-ms
     {:success false :error "Transcription timed out after 120 seconds"})
   (fn [result]
     (if (:success result)
       (do (log/log-info (str "Transcription complete topic-id=" topic-id))
           (swap! (us/get-atom user-id :refresh) inc))
       (do (log/log-info (str "Transcription failed topic-id=" topic-id " error=" (:error result)))
           (toasts/push! user-id {:level :error :message (:error result)})))
     (swap! (us/get-atom user-id :transcribing-topics) disj topic-id))
   (fn [e]
     (when-not (instance? Cancelled e)
       (log/log-info (str "Transcription error topic-id=" topic-id " ex=" (.getMessage e)))
       (toasts/push! user-id {:level :error :message (.getMessage e)}))
     (swap! (us/get-atom user-id :transcribing-topics) disj topic-id)))
  nil)
