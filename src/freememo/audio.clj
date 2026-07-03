(ns freememo.audio
  "Business logic for audio document storage. Audio topics hold a single audio
   blob in `topic_files` and an (initially empty) Quill content body that the
   Transcribe action fills via `freememo.transcribe`."
  (:require
   [clojure.string :as str]
   [freememo.db :as db]
   [freememo.quota :as quota]
   [taoensso.telemere :as tel]))

(def max-bytes
  "OpenAI Whisper rejects files over 25 MB; cap audio uploads at that boundary
   so a stored file is always transcribable. Stricter than the global per-file
   storage cap."
  (* 25 1024 1024))

(def score-max-bytes
  "Score recordings are never transcribed, so Whisper's 25 MB cap doesn't
   apply; bound them at 200 MB to keep blobs sane."
  (* 200 1024 1024))

(defn filename->mime
  "Best-effort audio MIME from a filename extension. Staging carries no
   content-type, so the upload's MIME is re-derived here for storage/serving.
   Defaults to audio/mpeg for unknown extensions."
  [filename]
  (let [lower (some-> filename str/lower-case)]
    (cond
      (nil? lower) "audio/mpeg"
      (str/ends-with? lower ".mp3") "audio/mpeg"
      (str/ends-with? lower ".mpeg") "audio/mpeg"
      (str/ends-with? lower ".mpga") "audio/mpeg"
      (or (str/ends-with? lower ".m4a") (str/ends-with? lower ".mp4")) "audio/mp4"
      (str/ends-with? lower ".wav") "audio/wav"
      (str/ends-with? lower ".webm") "audio/webm"
      (or (str/ends-with? lower ".ogg") (str/ends-with? lower ".oga")) "audio/ogg"
      (str/ends-with? lower ".flac") "audio/flac"
      :else "audio/mpeg")))

(defn save-audio!
  "Persist an audio upload as a new audio topic.
   Pre  : file-bytes is a non-empty byte array; filename names an audio file.
   Post : {:success true :id topic-id} with usage_bytes raised by the byte size,
          or {:success false :error S :code C} on quota violation.
   No bump here: callers are command boundaries and bump :import-document
   (freememo.commands single-authority rule)."
  [user-id filename ^bytes file-bytes mime-type]
  (try
    (let [file-size (alength file-bytes)
          topic (db/create-audio-topic! user-id filename file-bytes file-size mime-type)
          topic-id (:topics/id topic)]
      {:success true :id topic-id})
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)]
        (if (quota/quota-error? data)
          (case (:reason data)
            :file-too-large {:success false
                             :error (str "File exceeds per-file limit (" (:limit data) " bytes)")
                             :code "file-too-large"
                             :limit (:limit data) :incoming (:incoming data)}
            :over-quota {:success false
                         :error "Storage quota exceeded — delete documents to free space"
                         :code "over-quota"
                         :used (:used data) :limit (:limit data) :incoming (:incoming data)})
          (do (tel/error! {:id ::save-audio} e)
              {:success false :error (str "Failed to save audio: " (.getMessage e))}))))
    (catch Exception e
      (tel/error! {:id ::save-audio} e)
      {:success false :error (str "Failed to save audio: " (.getMessage e))})))
