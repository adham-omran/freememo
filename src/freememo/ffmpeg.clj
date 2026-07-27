(ns freememo.ffmpeg
  "ffmpeg / ffprobe subprocess wrappers.

   Both binaries are provisioned in the runtime image (Dockerfile `apt-get
   install … ffmpeg`), alongside `magick` and `sfdp`. Absence is reported as a
   typed failure rather than a crash, so a self-host install without ffmpeg
   degrades to 'video stored but not processed' instead of erroring on upload.

   Every call is bounded: a malformed or adversarial container must not pin a
   worker thread forever."
  (:require
   [cheshire.core :as json]
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

(defn probe-media
  "§4.4 4.3 / §12.4 1.2 — container, duration and first video/audio codec of
   `file`, in one ffprobe call.

   One call rather than three: ffprobe only reads headers, but each invocation
   is a process spawn, and the pipeline needs all three facts at the same point.
   The container name is what decides whether a remux is needed — never the
   filename or the browser-declared MIME, both of which the user controls.

   Post: {:ok true :container S :duration-ms n-or-nil :video-codec S-or-nil
          :audio-codec S-or-nil} or {:ok false :error S}.
   `:duration-ms` is nil — not 0, not an error — for a container with no
   duration header (a stream-muxed or truncated file), so the caller stores
   NULL instead of a lie. `:container` is ffprobe's comma-joined family name,
   e.g. \"matroska,webm\" or \"mov,mp4,m4a,3gp,3g2,mj2\"."
  [^java.io.File file]
  (let [r (exec! ["ffprobe" "-v" "error" "-of" "json"
                 "-show_entries" "format=format_name,duration:stream=codec_type,codec_name"
                 (.getAbsolutePath file)]
            probe-timeout-ms)]
    (if-not (:ok r)
      r
      (try
        (let [parsed (json/parse-string (:out r) true)
              streams (:streams parsed)
              codec-of (fn [t] (some #(when (= t (:codec_type %)) (:codec_name %)) streams))
              secs (some-> (get-in parsed [:format :duration]) parse-double)]
          {:ok true
           :container (get-in parsed [:format :format_name])
           :duration-ms (when (and secs (pos? secs)) (long (Math/round (* 1000.0 secs))))
           :video-codec (codec-of "video")
           :audio-codec (codec-of "audio")})
        (catch Exception e
          {:ok false :error (str "Could not read this file's format: " (.getMessage e))})))))

(defn remux-to-mp4!
  "§12.4 1.3/1.4 — rewrite `in` into `out` as MP4 without re-encoding video.

   A container rewrite, not a transcode: the video stream is copied frame for
   frame, so §4.11 11.2 (no re-encode, no resolution change) holds. Measured at
   ≈335 MB/s — a 192 MB source remuxed in 0.60 s.

   `audio-mode` is :copy or :aac. :aac re-encodes ONLY the audio track, for
   codecs MP4 can carry but browsers cannot decode — Chromium plays an AC-3
   track as silence with no error event, which is worse than a visible failure.

   Streams are mapped explicitly. ffmpeg's default selection picks the audio
   stream with the most channels, which on a film with a 5.1 commentary track is
   the wrong one; `0:a:0?` takes the first and tolerates a video-only source
   rather than erroring on it.

   `+faststart` moves `moov` to the head (measured: offset 36, right after
   `ftyp`). Without it the index sits at the tail and every seek costs a second
   Range round-trip — and an indexless source stays unseekable.

   Pre:  `in` is a readable media file; `out` does not need to exist.
   Post: {:ok true} with `out` written, or {:ok false :error S} — including for
         a payload MP4 cannot hold, which is the caller's fallback signal."
  [^java.io.File in ^java.io.File out audio-mode]
  (exec! (concat
           ["ffmpeg" "-nostdin" "-y" "-v" "error"
            "-i" (.getAbsolutePath in)
            "-map" "0:v:0" "-map" "0:a:0?"
            "-c:v" "copy"]
           (if (= :aac audio-mode)
             ["-c:a" "aac" "-b:a" "128k"]
             ["-c:a" "copy"])
           ["-movflags" "+faststart" (.getAbsolutePath out)])
    transcode-timeout-ms))

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
