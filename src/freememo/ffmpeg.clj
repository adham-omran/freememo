(ns freememo.ffmpeg
  "ffmpeg / ffprobe subprocess wrappers.

   Both binaries are provisioned in the runtime image (Dockerfile `apt-get
   install … ffmpeg`), alongside `magick` and `sfdp`. Absence is reported as a
   typed failure rather than a crash, so a self-host install without ffmpeg
   degrades to 'video stored but not processed' instead of erroring on upload.

   Every call is bounded: a malformed or adversarial container must not pin a
   worker thread forever."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [taoensso.telemere :as tel])
  (:import
   [java.util.concurrent TimeUnit]))

(def ^:private probe-timeout-ms
  "ffprobe only reads container headers; a minute is generous."
  60000)

(def ^:private transcode-timeout-ms
  "Audio-only extraction is demux-bound and runs far faster than realtime, but a
   multi-hour 4K source on slow disk is still minutes of work. 30 min is the
   wall beyond which something is wrong, not slow."
  1800000)

(defn- exec!
  "Run `args`, capturing stdout and stderr, bounded by `timeout-ms`.
   Post: {:ok true :out S} on exit 0; otherwise
         {:ok false :error S :reason :missing-binary|:timeout|:exit}.
   Never throws."
  [args timeout-ms]
  (try
    (let [pb (ProcessBuilder. ^java.util.List (vec args))
          proc (.start pb)
          ;; Drain both pipes concurrently: ffmpeg writes progress to stderr
          ;; continuously, and a full OS pipe buffer would deadlock the process
          ;; while we sat in waitFor.
          out-fut (future (slurp (.getInputStream proc)))
          err-fut (future (slurp (.getErrorStream proc)))
          finished? (.waitFor proc timeout-ms TimeUnit/MILLISECONDS)]
      (if-not finished?
        (do (.destroyForcibly proc)
            (future-cancel out-fut)
            (future-cancel err-fut)
            {:ok false :reason :timeout
             :error (str "Timed out after " (quot timeout-ms 1000) "s")})
        (let [out (deref out-fut 5000 "")
              err (deref err-fut 5000 "")
              code (.exitValue proc)]
          (if (zero? code)
            {:ok true :out out :err err}
            {:ok false :reason :exit
             :error (or (not-empty (str/trim (str/join " " (take-last 3 (str/split-lines err)))))
                      (str "exit " code))}))))
    (catch java.io.IOException _
      {:ok false :reason :missing-binary
       :error (str (first args) " is not installed on this server")})
    (catch Exception e
      {:ok false :reason :exit :error (.getMessage e)})))

(defn available?
  "True when both binaries respond to -version. Cheap enough to call per upload;
   used to decide whether to offer processing at all."
  []
  (and (:ok (exec! ["ffprobe" "-version"] probe-timeout-ms))
       (:ok (exec! ["ffmpeg" "-version"] probe-timeout-ms))))

(defn probe-duration-ms
  "§4.4 4.3 — container duration of `file` in milliseconds.
   Post: {:ok true :duration-ms n} or {:ok false :error S}. A container with no
   duration header (a growing or truncated file) yields :ok false rather than 0,
   so the caller stores NULL instead of a lie."
  [^java.io.File file]
  (let [r (exec! ["ffprobe" "-v" "error"
                 "-show_entries" "format=duration"
                 "-of" "default=noprint_wrappers=1:nokey=1"
                 (.getAbsolutePath file)]
            probe-timeout-ms)]
    (if-not (:ok r)
      r
      (let [secs (some-> (:out r) str/trim parse-double)]
        (if (and secs (pos? secs))
          {:ok true :duration-ms (long (Math/round (* 1000.0 secs)))}
          {:ok false :error "Could not read a duration from this file"})))))

(defn extract-audio!
  "§4.4 4.4 — write `in`'s audio track to `out` as MP3, 32 kbps, mono, 16 kHz.

   MP3 rather than Opus, which would halve the bytes: `score_audio.cljs`
   documents that AnkiMobile/iOS plays neither OGG containers nor Opus, and the
   same blob is what wavesurfer decodes and what Whisper is fed. One format
   across all three beats a smaller one that breaks a shipped platform.

   16 kHz mono is Whisper's own working rate — resampling higher would cost
   bytes for no transcription accuracy. 32 kbps is 14.4 MB/hour, so the 25 MB
   Whisper cap binds at ~1.7 h, which is why chunking (4.5) exists.

   Pre:  `in` is a readable media file. Post: {:ok true} with `out` written,
         or {:ok false :error S} — including for a video with no audio track."
  [^java.io.File in ^java.io.File out]
  (exec! ["ffmpeg" "-nostdin" "-y" "-v" "error"
         "-i" (.getAbsolutePath in)
         "-vn" "-ac" "1" "-ar" "16000"
         "-c:a" "libmp3lame" "-b:a" "32k"
         (.getAbsolutePath out)]
    transcode-timeout-ms))

(defn split-audio!
  "§4.4 4.5 — cut `in` into `segment-seconds` pieces inside `dir`.

   `-c copy` keeps the existing MP3 frames, so this is a remux: no re-encode,
   no quality loss, and boundaries land on frame edges. A boundary can therefore
   sit up to one frame (1152 samples = 72 ms at our 16 kHz rate) off the nominal
   multiple. The error does NOT accumulate — each piece begins exactly where the
   previous one ended, so chunk i still starts within one frame of
   i × segment-seconds, which is what `video/offset-segments` assumes.
   (Measured on a 40 s source split at 15 s: starts at 0, 14.976, 29.988.)

   Post: {:ok true :files [f0 f1 …]} in index order, or {:ok false :error S}."
  [^java.io.File in ^java.io.File dir ^long segment-seconds]
  (let [pattern (.getAbsolutePath (io/file dir "chunk%04d.mp3"))
        r (exec! ["ffmpeg" "-nostdin" "-y" "-v" "error"
                 "-i" (.getAbsolutePath in)
                 "-f" "segment" "-segment_time" (str segment-seconds)
                 "-c" "copy" "-reset_timestamps" "1"
                 pattern]
            transcode-timeout-ms)]
    (if-not (:ok r)
      r
      (let [files (->> (.listFiles dir)
                    (filter #(re-matches #"chunk\d{4}\.mp3" (.getName ^java.io.File %)))
                    (sort-by #(.getName ^java.io.File %))
                    vec)]
        (if (seq files)
          {:ok true :files files}
          {:ok false :error "Audio split produced no chunks"})))))

(defn with-temp-dir
  "Call `f` with a fresh temp directory, deleting it and everything under it
   afterwards — including on a throw. Video temp files are hundreds of MB; a
   leaked one fills the disk long before anyone notices."
  [prefix f]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory prefix
                       (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (f dir)
      (finally
        (try
          (doseq [^java.io.File file (reverse (file-seq dir))] (.delete file))
          (catch Exception e
            (tel/log! {:level :warn :id ::temp-cleanup :data {:dir (str dir)}}
              (.getMessage e))))))))
