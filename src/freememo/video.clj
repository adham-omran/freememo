(ns freememo.video
  "Video post-upload pipeline: probe → extract audio → chunk → transcribe.

   Runs once per uploaded video, off the reactive path. Every stage is
   independently useful, so a failure late in the chain leaves the earlier
   results in place: a video whose transcription fails still has a duration, a
   waveform, and playback.

   Billed against credits from OpenRouter's returned `usage.cost`, like every
   other AI lane: gated once before the chunk loop, charged once after it as a
   single ledger row for the whole run. Storage rent is billed separately and
   continuously by `freememo.storage-meter`."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [freememo.commands :as commands]
   [freememo.config :as config]
   [freememo.credits :as credits]
   [freememo.db :as db]
   [freememo.ffmpeg :as ffmpeg]
   [freememo.largeobj :as lo]
   [freememo.logging :as log]
   [freememo.openrouter :as openrouter]
   [freememo.quota :as quota]
   [freememo.video-format :as vfmt]
   [freememo.settings :as settings]
   [freememo.toasts :as toasts]
   [freememo.user-state :as us]
   [missionary.core :as m]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]
   [taoensso.telemere :as tel])
  (:import
   [missionary Cancelled]
   [java.util Base64]
   [java.util.concurrent Executors]))

(def ^:private model "openai/whisper-1")

(def usd-per-audio-minute
  "§15.3 5.1 — `model`'s list price, for the pre-flight ESTIMATE only.

   Lives beside `model` rather than in `config.clj` because it is a property of
   whisper-1, not of the deployment: changing the model is what changes the rate,
   and a config knob would let the two drift apart silently.

   The CHARGE never reads this. Every AI lane bills from OpenRouter's returned
   `usage.cost` and the credit gate has no rate table on purpose
   (`credits/check-cost-billed-balance!`), so this is the one place in the
   codebase that predicts a price. Expect the estimate and the charge to differ."
  (or (some-> (System/getenv "VIDEO_TRANSCRIBE_USD_PER_MINUTE") parse-double)
    0.006))

(defn transcription-cost-estimate
  "Credits `seconds` of audio would cost to transcribe, or nil when unknowable.

   Pre:  `seconds` is a number or nil.
   Post: a non-negative long, or NIL when there is no honest figure to show —
         credits disabled (self-host), fx or markup unconfigured, or a duration
         we could not read. Callers render nil as 'no estimate', never as 0.
   Invariant: indicative only. `record-cost-charge!` debits the provider's
         reported cost, which this does not consult.

   fx/markup are pre-checked rather than caught: `charge-iqd-from-usd` throws
   when either is missing (it fails closed on the debit side, which is right
   there and wrong here — an unconfigured deployment should show no estimate,
   not 500 a modal)."
  [user-id seconds]
  (when (and (config/credits-enabled?)
          (number? seconds) (pos? seconds)
          (some? (config/fx-iqd-per-usd))
          (some? (credits/resolve-markup user-id)))
    (credits/charge-iqd-from-usd user-id (* (/ (double seconds) 60.0) usd-per-audio-minute))))

(def max-video-bytes
  "Hard ceiling on one declared upload, independent of the user's quota.
   Default 8 GB — a bound on how much a single bad request can reserve, not a
   product limit; the quota is what gates normal use.

   Deliberately NOT `quota/get-user-upload-max`: that cap is the single-HTTP-
   request ceiling (100 MB), and a chunked upload is many requests. Applying it
   to the whole file rejected every video over 100 MB — the exact limit chunking
   was built to get past."
  (or (some-> (System/getenv "VIDEO_MAX_BYTES") parse-long)
    (* 8 1024 1024 1024)))

(defn upload-limits
  "The two ceilings a video upload must clear, with display labels.
   `:per-video` is `max-video-bytes`; `:remaining` is the user's unused quota,
   nil when their cap is unlimited.

   Lives here, in the domain namespace, rather than in `video-http`: the import
   modal needs it, and a UI component requiring the HTTP-handler namespace is
   backwards — it also broke Electric's JVM reload, which loaded the modal
   before the handler ns and failed on an unresolved var.

   Read-only. The authoritative gates are `max-video-bytes` (checked at the
   route) and `quota/reserve-bytes!` (checked in the reservation transaction);
   this exists so the modal can state them before the user picks a file."
  [user-id]
  (let [remaining (quota/remaining-bytes db/ds user-id)]
    {:per-video max-video-bytes
     :per-video-label (vfmt/format-bytes max-video-bytes)
     :remaining remaining
     :remaining-label (when remaining (vfmt/format-bytes remaining))}))

(def segment-seconds
  "§4.4 4.5 — audio chunk length fed to Whisper.

   900 s at 32 kbps mono is ~3.6 MB, comfortably inside the 25 MB request cap
   with headroom for base64's 4/3 expansion. Configurable because a provider
   with a tighter body limit is the likely reason to change it."
  (or (some-> (System/getenv "VIDEO_TRANSCRIBE_SEGMENT_SECONDS") parse-long)
    900))

(def ^:private chunk-timeout-ms
  "Per-chunk Whisper budget. A 15-minute chunk transcribes in well under a
   minute; 5 min is the outlier wall."
  300000)

(defn supported-mime?
  "Whether we accept this container for upload.

   No longer 'whether a browser can play it': since §12 every non-MP4 container
   is remuxed to MP4 at ingest, so acceptance is about what ffmpeg can demux,
   not what the browser can decode. Matroska is admitted on that basis — it
   plays in Chromium but not, as far as we have measured, in Safari."
  [mime]
  (contains? #{"video/mp4" "video/webm" "video/ogg" "video/quicktime"
               "video/x-matroska"}
    (some-> mime str/lower-case str/trim)))

(defn filename->mime
  "Best-effort video MIME from a filename extension. Defaults to video/mp4.

   Honest labelling matters here even though the label is provisional: it seeds
   `topic_videos.remux_pending`, and a Matroska file recorded as video/mp4 —
   which is what the missing .mkv branch used to produce — would be served with
   a 24-hour cache lifetime before the pipeline replaced its bytes."
  [filename]
  (let [lower (some-> filename str/lower-case)]
    (cond
      (nil? lower) "video/mp4"
      (str/ends-with? lower ".mkv") "video/x-matroska"
      (str/ends-with? lower ".webm") "video/webm"
      (str/ends-with? lower ".ogv") "video/ogg"
      (or (str/ends-with? lower ".mov") (str/ends-with? lower ".qt")) "video/quicktime"
      :else "video/mp4")))

;; ── Container normalization (§12.4 1) ──────────────────────────────────────

(defn mp4-container?
  "Whether ffprobe's comma-joined format family names an MP4.

   Matched against the family, not the filename: ffprobe reports
   \"mov,mp4,m4a,3gp,3g2,mj2\" for MP4 and \"matroska,webm\" for Matroska, so a
   Matroska file named `lecture.mp4` is correctly classified as needing a remux."
  [container]
  (contains? (set (str/split (or container "") #",")) "mp4"))

(def browser-safe-audio
  "Audio codecs every target browser decodes from an MP4.

   AC-3, DTS, TrueHD and Vorbis are all legal in MP4 and all copy cleanly — and
   Chromium then plays them as silence with no error event (measured: 0 audio
   bytes decoded from an AC-3 track). Anything outside this set is re-encoded to
   AAC, which is a transcode of the audio track only; §4.11 11.2's ban stays
   intact for video."
  #{"aac" "mp3"})

(defn remux-stored-video!
  "§12.4 1 — make a video topic's stored bytes a canonical, seekable MP4.

   Stage 0 of the pipeline, run where `source` has already been streamed out of
   the large object: remuxing anywhere else would pay a second full read of a
   file that can be gigabytes.

   Three outcomes, all of which leave a usable video:
     :skipped   already an MP4 — nothing written, flag cleared
     :remuxed   object replaced; returns the MP4 to continue the pipeline with
     :failed    MP4 cannot hold this payload; the original bytes stay, and
                duration, waveform, transcript and cards all still work

   Remuxing is also the repair for an indexless Matroska file: written without
   Cues it has no duration header and Chromium reports `seekable` [0,0], which
   silently disables resume, transcript click-to-seek and extract ranges. The
   MP4 written here always carries an index.

   Pre:  `source` holds topic-id's stored bytes; `dir` is writable temp space
         with room for a second copy of them.
   Post: {:outcome kw :file F :probe P}, where `:file` is the file the rest of
         the pipeline must read — `source` unless a remux replaced it — and
         topic_videos.remux_pending is FALSE.
   Invariant: on :remuxed, byte_size, mime_type and the object agree."
  [user-id topic-id ^java.io.File source ^java.io.File dir]
  (let [probe (ffmpeg/probe-media source)]
    (cond
      (not (:ok probe))
      (do (db/clear-video-remux-pending! topic-id)
          {:outcome :failed :file source :probe probe :error (:error probe)})

      (mp4-container? (:container probe))
      (do (db/clear-video-remux-pending! topic-id)
          {:outcome :skipped :file source :probe probe})

      ;; §12.4 2.4 — the remux writes a second full copy alongside the source.
      ;; Refusing up front turns a disk-full into a named failure with the
      ;; original bytes intact, rather than a half-written temp file and an
      ;; IOException from inside ffmpeg.
      (< (.getUsableSpace dir) (+ (.length source) (* 256 1024 1024)))
      (do (log/log-warn (str "Video remux skipped for lack of temp space topic=" topic-id
                          " need=" (.length source) " free=" (.getUsableSpace dir)))
          (db/clear-video-remux-pending! topic-id)
          {:outcome :failed :file source :probe probe
           :error "Not enough temporary disk space to convert this video"})

      :else
      (let [audio-mode (if (or (nil? (:audio-codec probe))
                             (contains? browser-safe-audio (:audio-codec probe)))
                         :copy
                         :aac)
            out (io/file dir "normalized.mp4")
            t-start (System/nanoTime)
            r (ffmpeg/remux-to-mp4! source out audio-mode)]
        (if-not (:ok r)
          (do (log/log-warn (str "Video remux failed topic=" topic-id
                              " container=" (:container probe)
                              " error=" (:error r)))
              (db/clear-video-remux-pending! topic-id)
              {:outcome :failed :file source :probe probe :error (:error r)})
          ;; Probe the OUTPUT for duration: an indexless source has none, and
          ;; the MP4 just written always does.
          (let [out-probe (ffmpeg/probe-media out)
                saved (db/replace-video-bytes! user-id topic-id out "video/mp4"
                        (:duration-ms out-probe))]
            (tel/log! {:level :info :id ::remux
                       :data {:user-id user-id :topic-id topic-id
                              :in-container (:container probe)
                              :video-codec (:video-codec probe)
                              :audio-codec (:audio-codec probe)
                              :audio-mode audio-mode
                              :in-bytes (.length source)
                              :out-bytes (.length out)
                              :ms (long (/ (- (System/nanoTime) t-start) 1000000))}}
              "Video remuxed to MP4")
            (if (:ok saved)
              {:outcome :remuxed :file out :probe out-probe}
              ;; The object could not be replaced, so the stored bytes are still
              ;; the original container — keep reading from it rather than from
              ;; an MP4 that exists only in temp.
              (do (db/clear-video-remux-pending! topic-id)
                  {:outcome :failed :file source :probe probe :error (:error saved)}))))))))

;; ── Transcription ──────────────────────────────────────────────────────────

(def ^:private iso-639-1
  "Whisper reports the detected language as an English NAME (\"english\",
   \"arabic\") but accepts only an ISO-639-1 CODE on input. Mapping the two is
   what lets chunk 0's detection be pinned onto chunks 1..n. A name absent here
   simply leaves later chunks on auto-detect."
  {"english" "en" "arabic" "ar" "french" "fr" "german" "de" "spanish" "es"
   "italian" "it" "portuguese" "pt" "russian" "ru" "turkish" "tr"
   "chinese" "zh" "japanese" "ja" "korean" "ko" "hindi" "hi" "dutch" "nl"
   "polish" "pl" "swedish" "sv" "ukrainian" "uk" "persian" "fa" "urdu" "ur"
   "indonesian" "id" "vietnamese" "vi" "hebrew" "he" "greek" "el"})

(defn- transcribe-chunk!
  "One Whisper call over one audio chunk.

   `response_format=verbose_json` + `timestamp_granularities [\"segment\"]` is
   what turns Whisper's flat text into timed segments (§4.4 4.6) — the plain
   call `freememo.transcribe` makes returns `:text` only, with no way to seek
   back into the media. Providers that are not OpenAI-compatible reject these
   two fields outright rather than ignoring them, which is the honest failure.

   `language` (ISO-639-1, or nil to auto-detect) is passed through because
   Whisper's auto-detection is not merely imprecise on short or quiet audio —
   it mis-identifies the language and then hallucinates fluent text in the one
   it guessed. Measured on a 10 s English clip: auto returned three Arabic
   segments, two of them identical; the same bytes with \"en\" transcribed
   correctly.

   Post: {:segments [{:start-ms :end-ms :text}] :language name-or-nil
          :cost-usd n-or-nil} in chunk-local time. The cost is per CHUNK; the
   caller sums it and charges once for the whole run."
  [api-key user-id ^bytes audio-bytes language]
  (let [body (openrouter/transcription! api-key
               (cond-> {:input_audio {:data (.encodeToString (Base64/getEncoder) audio-bytes)
                                      :format "mp3"}
                        :model model
                        :response_format "verbose_json"
                        :timestamp_granularities ["segment"]}
                 language (assoc :language language))
               {:feature :transcription :user-id user-id})]
    {:language (:language body)
     :cost-usd (get-in body [:usage :cost])
     :segments (->> (:segments body)
                 (keep (fn [s]
                         (let [text (some-> (:text s) str/trim)]
                           (when-not (str/blank? text)
                             {:start-ms (long (Math/round (* 1000.0 (double (or (:start s) 0)))))
                              :end-ms (long (Math/round (* 1000.0 (double (or (:end s) 0)))))
                              :text text}))))
                 vec)}))

(defn- offset-segments
  "§4.4 4.7 — shift a chunk's segment times into whole-video time.

   Whisper reports timestamps relative to the audio it was handed, so every
   chunk after the first restarts at zero. Without this the last segment of a
   three-hour video would claim to end at 15 minutes, and every transcript
   click would seek into the first chunk."
  [segments ^long chunk-index]
  (let [offset (* chunk-index segment-seconds 1000)]
    (map (fn [s] (-> s
                   (update :start-ms + offset)
                   (update :end-ms + offset)))
      segments)))

;; ── Pipeline ───────────────────────────────────────────────────────────────

(defn- stream-video-to-file!
  "§4.4 4.2 — copy the topic's large object to `file` without materializing it.

   `lo_read` in 256 KiB steps: a 700 MB video costs one buffer of heap, where
   the BYTEA path would have needed the whole array (and, above 1 GB, could not
   have stored it at all)."
  [topic-id ^java.io.File file]
  (jdbc/with-transaction [tx db/ds]
    (let [row (jdbc/execute-one! tx
                ["SELECT lo_oid FROM topic_videos WHERE topic_id = ?" topic-id]
                {:builder-fn rs/as-unqualified-maps})]
      (when row
        (lo/copy-to-file! tx (:lo_oid row) file)))))

(defn process-video!
  "Run the pipeline for one uploaded video: normalize → probe → extract audio →
   chunk → transcribe. `transcribe?` false stops after the audio.

   Pre:  topic-id is a kind='video' topic owned by user-id with a topic_videos
         row; ffmpeg and ffprobe are on PATH. `transcribe?` is explicit — see
         `start-processing!` for why there is no defaulting arity.
   Post: {:success true :segments n :transcribed true} with duration, extracted
         audio and transcript persisted; {:success true :segments 0 :transcribed
         false} when transcription was skipped; or {:success false :error S
         :stage kw}.
   Partial success is intentional: the container is normalized and duration and
   audio are committed before transcription starts, so an API failure still
   leaves a playable, waveform-scrubbable video. Re-running replaces the
   transcript wholesale and is a no-op for normalization (§12.4 5.1) — the
   second run probes an MP4 and skips.

   A skip (§15.3 2.2/2.3) reaches the SAME outcome the out-of-credits and
   missing-key branches already produce, and by the same route: it exits after
   `save-video-audio!` and before the credit gate. What differs is only that it
   is a success, so no error toast fires for something the user asked for."
  [user-id topic-id transcribe?]
  (let [api-key (settings/get-openrouter-api-key user-id)]
    (ffmpeg/with-temp-dir "fm-video-"
      (fn [dir]
        (let [source (io/file dir "source.bin")
              audio (io/file dir "audio.mp3")
              chunk-dir (io/file dir "chunks")]
          (.mkdirs chunk-dir)
          (let [copied (stream-video-to-file! topic-id source)]
            (cond
              (nil? copied)
              {:success false :error "Video not found" :stage :fetch}

              :else
              ;; Stage 0 (§12.4 1.1): normalize the container before anything
              ;; reads the file, so every later stage — and the browser — sees
              ;; one format. `media` is the file to work from: the remuxed MP4
              ;; when there is one, else the bytes as uploaded.
              (let [normalized (remux-stored-video! user-id topic-id source dir)
                    ^java.io.File media (:file normalized)
                    probe (:probe normalized)]
                ;; A container we cannot probe is very likely one the browser
                ;; cannot play either, but the bytes are already stored and paid
                ;; for — record what we can and keep going rather than failing
                ;; the whole upload. A remux has already written its duration.
                (when (and (not= :remuxed (:outcome normalized))
                        (:duration-ms probe))
                  (db/set-video-duration! topic-id (:duration-ms probe)))
                (let [extracted (ffmpeg/extract-audio! media audio)]
                  (cond
                    (not (:ok extracted))
                    (do (log/log-warn (str "Video audio extraction failed topic=" topic-id
                                        " error=" (:error extracted)))
                        {:success false :error (:error extracted) :stage :extract})

                    :else
                    (do
                      (db/save-video-audio! user-id topic-id
                        (java.nio.file.Files/readAllBytes (.toPath audio)))
                      ;; §15.3 2.2 — the cut, placed BEFORE the credit gate
                      ;; rather than as a branch inside it: the gate reads the
                      ;; user's balance from the DB, and a run the user asked not
                      ;; to bill has no reason to ask what they can afford.
                      (if-not transcribe?
                        {:success true :segments 0 :transcribed false}
                        (let [gate (credits/check-cost-billed-balance! user-id)]
                        (cond
                        (str/blank? api-key)
                        {:success false
                         :error "OpenRouter API key not configured — video stored without a transcript"
                         :stage :transcribe}

                        ;; Gated here, not at the top of the pipeline: duration,
                        ;; the extracted audio and the waveform are free (local
                        ;; ffmpeg) and are already committed above. An
                        ;; out-of-credits user therefore keeps a playable,
                        ;; scrubbable video and loses only the transcript —
                        ;; the partial-success shape §4.4 specifies.
                        (not (:ok gate))
                        {:success false :error (:error gate) :stage :transcribe}

                        :else
                        (let [split (ffmpeg/split-audio! audio chunk-dir segment-seconds)]
                          (if-not (:ok split)
                            {:success false :error (:error split) :stage :split}
                            (let [t-start (System/nanoTime)
                                  forced (settings/get-transcribe-language user-id)
                                  ;; Language is decided ONCE and applied to every
                                  ;; chunk: the user's setting if they pinned one,
                                  ;; else whatever chunk 0 detected. Re-detecting
                                  ;; per chunk lets a quiet passage midway through
                                  ;; a lecture flip the transcript into another
                                  ;; language for the rest of the file.
                                  !lang (atom forced)
                                  ;; One run = one ledger row, not one per chunk:
                                  ;; a 43-minute video is twelve calls, and twelve
                                  ;; rows for one action would bury the ledger.
                                  !cost (atom 0.0)
                                  results
                                  (try
                                    (mapv (fn [[i ^java.io.File f]]
                                            (let [r (transcribe-chunk! api-key user-id
                                                      (java.nio.file.Files/readAllBytes (.toPath f))
                                                      @!lang)]
                                              (swap! !cost + (or (:cost-usd r) 0))
                                              (when (nil? @!lang)
                                                (reset! !lang (get iso-639-1
                                                                (some-> (:language r) str/lower-case))))
                                              (offset-segments (:segments r) i)))
                                      (map-indexed vector (:files split)))
                                    (finally
                                      ;; In `finally` because a chunk that throws
                                      ;; midway has still cost real money for the
                                      ;; chunks that succeeded. Skipped at zero so
                                      ;; a no-op run writes no ledger row.
                                      (when (pos? @!cost)
                                        (credits/record-cost-charge!
                                          user-id :transcribe model @!cost))))
                                  segments (vec (apply concat results))
                                  ordered (map-indexed (fn [i s] (assoc s :ord i)) segments)]
                              (db/replace-video-transcript! topic-id ordered)
                              (tel/log! {:level :info :id ::transcription
                                         :data {:user-id user-id :topic-id topic-id
                                                :model model
                                                :language (or forced @!lang)
                                                :language-source (if forced :setting :detected)
                                                :chunks (count (:files split))
                                                :segments (count segments)
                                                :cost-usd @!cost
                                                :duration-ms (long (/ (- (System/nanoTime) t-start)
                                                                     1000000))}}
                                "Video transcription")
                              {:success true :segments (count segments)
                               :transcribed true})))))))))))))))))

;; ── Background execution (§4.4 4.8) ────────────────────────────────────────

(defonce ^:private executor
  ;; Two at a time, matching freememo.transcribe. ffmpeg is CPU-hungry and these
  ;; runs are minutes long; an unbounded pool would let a batch upload starve
  ;; the request threads.
  (Executors/newFixedThreadPool 2))

(def ^:private pipeline-timeout-ms
  "Whole-pipeline wall. Extraction (30 min max) plus twelve chunk calls at 5 min
   each does not fit under an hour in the worst case; 90 min bounds it."
  5400000)

(defn start-processing!
  "Launch `process-video!` in the background. Returns nil immediately.

   Deduplicated on (user, topic) through the `:processing-videos` set: a second
   request while one is in flight is dropped, which is what makes the UI's
   disabled state honest rather than decorative.

   Pre:  `transcribe?` is passed explicitly. There is deliberately NO 2-arity
         defaulting it to true (§15.3 2.5): true is the branch that spends
         credits, and a caller that inherits it by omission bills the user
         without having said so. Two call sites exist — the upload's finalize
         handler, which reads the user's preference, and the Transcribe button,
         which always passes true."
  [user-id topic-id transcribe?]
  (let [!in-flight (us/get-atom user-id :processing-videos)]
    (if (contains? @!in-flight topic-id)
      (do (log/log-info (str "Video processing already running topic=" topic-id)) nil)
      (do
        (swap! !in-flight conj topic-id)
        (commands/bump! user-id :process-video)
        (log/log-info (str "Video processing started topic=" topic-id
                        " transcribe=" (boolean transcribe?)))
        (letfn [(finish! []
                  (swap! !in-flight disj topic-id)
                  (commands/bump! user-id :process-video))]
          ((m/timeout
             (m/via executor (process-video! user-id topic-id transcribe?))
             pipeline-timeout-ms
             {:success false :error "Video processing timed out" :stage :timeout})
           (fn [result]
             (if (:success result)
               ;; §15.3 2.6 — `transcribed` distinguishes a skip from a run that
               ;; transcribed silence. Both report segments=0.
               (log/log-info (str "Video processing complete topic=" topic-id
                               " segments=" (:segments result)
                               " transcribed=" (boolean (:transcribed result))))
               (do (log/log-warn (str "Video processing failed topic=" topic-id
                                   " stage=" (:stage result) " error=" (:error result)))
                   (toasts/push! user-id {:level :error :message (:error result)})))
             (finish!))
           (fn [e]
             (when-not (instance? Cancelled e)
               (tel/error! {:id ::process-video :data {:user-id user-id :topic-id topic-id}} e)
               (toasts/push! user-id {:level :error :message (.getMessage e)}))
             (finish!))))
        nil))))
