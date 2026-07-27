(ns freememo.video-test
  "Pure-function tests for the incremental-video pieces whose arithmetic is
   easy to get subtly wrong and hard to notice: HTTP range parsing, chunk
   timestamp offsetting, and storage-rent pricing.

   Deliberately no database and no ffmpeg — the stateful paths are exercised
   against a live Postgres from the REPL; these are the parts a regression
   would silently corrupt."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.config :as config]
   [freememo.credits :as credits]
   [freememo.db :as db]
   [freememo.video :as video]
   [freememo.video-format :as vf]
   [freememo.video-http :as vh]
   [freememo.video-import-modal :as import-modal]
   [freememo.video-transcript :as vt]
   [freememo.right-side-panel :as rsp]
   [freememo.settings :as settings]
   [freememo.transcribe-language :as tlang]
   [freememo.storage-meter :as meter]))

;; ---------------------------------------------------------------------------
;; §4.5 5.1 — Range parsing
;; ---------------------------------------------------------------------------

(deftest range-parsing
  (testing "explicit ranges are inclusive and clamped to the file"
    (is (= [0 99] (vh/parse-range "bytes=0-99" 1000)))
    (is (= [0 0] (vh/parse-range "bytes=0-0" 1000)) "a one-byte range is legal")
    (is (= [100 999] (vh/parse-range "bytes=100-" 1000)) "open-ended runs to the last byte")
    (is (= [900 999] (vh/parse-range "bytes=900-5000" 1000)) "an over-long end clamps"))

  (testing "suffix form counts back from the end"
    (is (= [950 999] (vh/parse-range "bytes=-50" 1000)))
    (is (= [0 999] (vh/parse-range "bytes=-5000" 1000)) "a suffix longer than the file is the whole file"))

  (testing "unsatisfiable is distinct from unparsed — one is 416, the other 200"
    (is (= :unsatisfiable (vh/parse-range "bytes=1000-" 1000)))
    (is (= :unsatisfiable (vh/parse-range "bytes=-0" 1000)))
    (is (= :unsatisfiable (vh/parse-range "bytes=-10" 0)) "empty object, any suffix"))

  (testing "forms we do not serve fall back to a full 200 response"
    (is (nil? (vh/parse-range "bytes=0-1,5-6" 1000)) "multi-range needs multipart/byteranges")
    (is (nil? (vh/parse-range "items=0-1" 1000)) "non-bytes unit")
    (is (nil? (vh/parse-range "garbage" 1000)))
    (is (nil? (vh/parse-range nil 1000)))
    (is (nil? (vh/parse-range "bytes=-" 1000)))))

;; ---------------------------------------------------------------------------
;; §4.4 4.7 — chunk timestamp offsets
;; ---------------------------------------------------------------------------

(def ^:private offset-segments #'video/offset-segments)

(deftest chunk-offsets
  (let [seg [{:start-ms 0 :end-ms 4200 :text "a"}
             {:start-ms 4200 :end-ms 9000 :text "b"}]
        step (* video/segment-seconds 1000)]
    (testing "chunk 0 is unshifted"
      (is (= seg (vec (offset-segments seg 0)))))

    (testing "later chunks shift by index × segment length"
      (is (= [{:start-ms step :end-ms (+ step 4200) :text "a"}
              {:start-ms (+ step 4200) :end-ms (+ step 9000) :text "b"}]
            (vec (offset-segments seg 1))))
      (is (= (* 11 step) (:start-ms (first (offset-segments seg 11))))))

    (testing "a three-hour video's last chunk lands near the end, not near the chunk length"
      ;; 3 h at 900 s per chunk = 12 chunks; the 12th starts at 11 × 900 s.
      (let [last-start (:start-ms (first (offset-segments seg 11)))]
        (is (> last-start (* 2 60 60 1000))
          "without the offset this would read as 0 ms and every seek would land in chunk 0")))))

;; ---------------------------------------------------------------------------
;; §4.6 6.1 — storage rent
;; ---------------------------------------------------------------------------

(def ^:private gb 1073741824)
(def ^:private month-ms (* 30 24 60 60 1000))

(deftest storage-pricing
  (testing "one GB for one month costs exactly the configured rate"
    (is (= 500 (long (/ (meter/micro-iqd-for 500 gb month-ms) 1000000)))))

  (testing "price is linear in both bytes and time"
    (is (= (meter/micro-iqd-for 500 (* 2 gb) month-ms)
          (* 2 (meter/micro-iqd-for 500 gb month-ms))))
    (is (= (meter/micro-iqd-for 500 gb (* 2 month-ms))
          (* 2 (meter/micro-iqd-for 500 gb month-ms)))))

  (testing "degenerate inputs price at zero rather than throwing"
    (is (zero? (meter/micro-iqd-for nil gb month-ms)) "no rate configured ⇒ metering off")
    (is (zero? (meter/micro-iqd-for 500 0 month-ms)))
    (is (zero? (meter/micro-iqd-for 500 gb 0)))
    (is (zero? (meter/micro-iqd-for 500 gb -5)) "a clock that went backwards must not credit"))

  (testing "sub-IQD intervals still accrue — the carry is why this matters"
    ;; 100 MB for an hour prices at ~0.068 IQD. Rounded to whole IQD per tick it
    ;; would be 0 forever; in micro it is a real number the carry accumulates.
    (let [hour (meter/micro-iqd-for 500 (* 100 1024 1024) 3600000)]
      (is (pos? hour))
      (is (< hour 1000000) "and it is indeed below one whole IQD")))

  (testing "hourly ticks sum to the same charge as one daily tick"
    ;; The property the carry exists to preserve: tick frequency must not change
    ;; the bill. Tolerance is per-tick rounding, not a systematic drift.
    (let [bytes (* 100 1024 1024) hour 3600000
          hourly (reduce + 0 (repeat 24 (meter/micro-iqd-for 500 bytes hour)))
          daily (meter/micro-iqd-for 500 bytes (* 24 hour))]
      (is (< (abs (- hourly daily)) 24)
        "at most one micro-IQD of rounding per tick"))))

;; ---------------------------------------------------------------------------
;; Timestamps + MIME
;; ---------------------------------------------------------------------------

(deftest timestamp-formatting
  (is (= "0:00" (vf/format-ms 0)))
  (is (= "0:07" (vf/format-ms 7400)))
  (is (= "1:05" (vf/format-ms 65000)))
  (is (= "59:59" (vf/format-ms 3599000)))
  (is (= "1:00:00" (vf/format-ms 3600000)) "lecture-length videos need the hour field")
  (is (= "2:03:04" (vf/format-ms 7384000)))
  (is (= "0:00" (vf/format-ms nil)))
  (is (= "0:00" (vf/format-ms -100)) "a negative position formats as the start, not as garbage"))

(deftest mime-handling
  (testing "containers ffmpeg can demux are accepted — non-MP4 is remuxed at ingest"
    (is (video/supported-mime? "video/mp4"))
    (is (video/supported-mime? "VIDEO/WEBM") "case and whitespace tolerant")
    (is (video/supported-mime? "video/x-matroska"))
    (is (not (video/supported-mime? "application/octet-stream")))
    (is (not (video/supported-mime? nil))))

  (testing "extension is the fallback when the browser sends no usable type"
    (is (= "video/x-matroska" (video/filename->mime "lecture.MKV"))
      "labelled honestly, because the label seeds remux_pending")
    (is (= "video/webm" (video/filename->mime "lecture.WEBM")))
    (is (= "video/quicktime" (video/filename->mime "clip.mov")))
    (is (= "video/mp4" (video/filename->mime "unknown.bin")))
    (is (= "video/mp4" (video/filename->mime nil)))))

;; ---------------------------------------------------------------------------
;; §12.4 1 — container normalization
;; ---------------------------------------------------------------------------

(deftest container-classification
  (testing "ffprobe's format family decides, not the filename (§12.4 1.2)"
    (is (video/mp4-container? "mov,mp4,m4a,3gp,3g2,mj2"))
    (is (not (video/mp4-container? "matroska,webm"))
      "WebM shares Matroska's demuxer and is remuxed like any other non-MP4")
    (is (not (video/mp4-container? "avi")))
    (is (not (video/mp4-container? nil)))
    (is (not (video/mp4-container? ""))))

  (testing "a substring match would misclassify"
    (is (not (video/mp4-container? "mp4v-es"))
      "the family is comma-separated; only a whole element counts")))

(deftest audio-remux-mode
  (testing "codecs every browser decodes are copied"
    (is (contains? video/browser-safe-audio "aac"))
    (is (contains? video/browser-safe-audio "mp3")))

  (testing "codecs MP4 accepts but browsers decode as silence are not"
    (is (not (contains? video/browser-safe-audio "ac3"))
      "measured: Chromium decodes 0 audio bytes from AC-3 and raises no error")
    (is (not (contains? video/browser-safe-audio "dts")))
    (is (not (contains? video/browser-safe-audio "truehd")))
    (is (not (contains? video/browser-safe-audio "vorbis")))))

;; ---------------------------------------------------------------------------
;; Transcript follows the playhead — the lookup that runs at the clock's rate
;; ---------------------------------------------------------------------------

(deftest active-segment-lookup
  (let [starts [0 5000 10000 12500]
        at #(vt/active-segment-index starts %)]
    (testing "the segment holding the instant"
      (is (= 0 (at 0)) "exactly the first start")
      (is (= 0 (at 4999)))
      (is (= 1 (at 5000)) "a boundary belongs to the segment it opens")
      (is (= 2 (at 10001)))
      (is (= 3 (at 12500)))
      (is (= 3 (at 999999)) "past the last start stays on the last segment"))

    (testing "no active segment"
      (is (= -1 (at nil)) "no playhead yet — the video has not reported a time")
      (is (= -1 (vt/active-segment-index [] 1000)) "no transcript")
      (is (= -1 (vt/active-segment-index [] nil))))

    (testing "before the first segment starts"
      (is (= -1 (vt/active-segment-index [3000 8000] 2999))
        "silence at the head of a video highlights nothing, rather than segment 0")))

  (testing "a gap between segments keeps the previous one lit"
    ;; Starts-only is the decision this encodes: with end times, 6000 would fall
    ;; in a silence and blank the highlight.
    (is (= 0 (vt/active-segment-index [0 10000] 6000))))

  (testing "scales to a lecture-length transcript"
    (let [starts (vec (range 0 (* 2000 5000) 5000))]
      (is (= 2000 (count starts)))
      (is (= 0 (vt/active-segment-index starts 1)))
      (is (= 1234 (vt/active-segment-index starts (+ (* 1234 5000) 4999))))
      (is (= 1999 (vt/active-segment-index starts 99999999))))))

;; ---------------------------------------------------------------------------
;; §12.4 4.3 — the drop zone is the filter that actually holds
;; ---------------------------------------------------------------------------

(deftest dropped-file-filtering
  (testing "accepted extensions, case-insensitively"
    (is (import-modal/video-file? "lecture.mkv"))
    (is (import-modal/video-file? "LECTURE.MKV"))
    (is (import-modal/video-file? "clip.mp4"))
    (is (import-modal/video-file? "clip.mov")))

  (testing "everything else is rejected"
    (is (not (import-modal/video-file? "notes.pdf")))
    (is (not (import-modal/video-file? "mkv")) "bare word, no extension")
    (is (not (import-modal/video-file? "")))
    (is (not (import-modal/video-file? nil)))))

;; ---------------------------------------------------------------------------
;; §14.3 — playlist controls in the import modal
;; ---------------------------------------------------------------------------

(deftest playlist-target-default
  (testing "the count preselects the target, reproducing the old implicit rule"
    (is (= "separate" (import-modal/default-target 0)))
    (is (= "separate" (import-modal/default-target 1))
      "one video needs no container")
    (is (= "new" (import-modal/default-target 2))
      "two or more used to mean a playlist with no way to say otherwise")
    (is (= "new" (import-modal/default-target 17)))))

(deftest playlist-title-derivation
  (testing "the extension is stripped, nothing else"
    (is (= "lecture-01" (import-modal/derive-playlist-title "lecture-01.mkv")))
    (is (= "a.b.c" (import-modal/derive-playlist-title "a.b.c.mp4"))
      "only the LAST dot is the extension")
    (is (= "no-extension" (import-modal/derive-playlist-title "no-extension")))
    (is (= ".hidden" (import-modal/derive-playlist-title ".hidden"))
      "a leading dot is a name, not an empty stem with an extension"))

  (testing "nothing to prefill"
    (is (= "" (import-modal/derive-playlist-title nil)))
    (is (= "" (import-modal/derive-playlist-title "")))))

(deftest selection-messages
  (testing "silence when a pick kept everything"
    (is (nil? (import-modal/selection-message {:rejected [] :duplicates []}))))

  (testing "the two causes stay distinct"
    (is (= "Not a supported video: a.txt"
          (import-modal/selection-message {:rejected ["a.txt"] :duplicates []})))
    (is (= "Already staged: a.mp4"
          (import-modal/selection-message {:rejected [] :duplicates ["a.mp4"]})))
    (is (= "Not a supported video: a.txt · Already staged: b.mp4"
          (import-modal/selection-message {:rejected ["a.txt"] :duplicates ["b.mp4"]}))))

  (testing "long lists are summarised, not dumped"
    (is (= "Not a supported video: a, b, c and 2 more"
          (import-modal/selection-message {:rejected ["a" "b" "c" "d" "e"] :duplicates []})))))

(deftest selection-removal
  (let [staged [:a :b :c]]
    (is (= [:b :c] (import-modal/remove-from-selection staged 0)))
    (is (= [:a :c] (import-modal/remove-from-selection staged 1)) "middle")
    (is (= [:a :b] (import-modal/remove-from-selection staged 2)) "last")
    (is (= staged (import-modal/remove-from-selection staged 3)) "out of range is a no-op")
    (is (= staged (import-modal/remove-from-selection staged -1)))
    (is (= [] (import-modal/remove-from-selection [] 0)))))

;; ---------------------------------------------------------------------------
;; Transcription language — the list is .cljc so both peers resolve it
;; ---------------------------------------------------------------------------

(deftest transcribe-language-options
  (testing "auto-detect is first and carries a nil code"
    (is (= [nil "Auto-detect"] (first tlang/options))))

  (testing "codes are the non-nil entries; nil is not a code"
    (is (contains? tlang/codes "en"))
    (is (not (contains? tlang/codes nil)))
    (is (= (dec (count tlang/options)) (count tlang/codes))))

  (testing "code? gates what may be stored"
    (is (tlang/code? "en"))
    (is (not (tlang/code? "klingon")))
    (is (not (tlang/code? "")) "blank means auto-detect, not a code")
    (is (not (tlang/code? nil))))

  (testing "labels are unique — they key the picker's e/diff-by"
    (let [labels (map second tlang/options)]
      (is (= (count labels) (count (set labels)))))))

;; ---------------------------------------------------------------------------
;; Right-panel tab resolution — the Transcript tab only exists on video topics
;; ---------------------------------------------------------------------------

(deftest right-panel-tab-fallback
  (testing "a persisted transcript tab falls back on a non-video topic"
    (is (= "pins" (rsp/resolve-tab "transcript" false))
      "otherwise navigating video→PDF selects a tab that is not rendered"))

  (testing "it is honoured on a video topic"
    (is (= "transcript" (rsp/resolve-tab "transcript" true))))

  (testing "video topics default to the transcript when nothing is persisted"
    ;; This branch was unreachable until settings/get-assistant-tab stopped
    ;; defaulting to "pins" itself — the test passed while the app never did it.
    (is (= "transcript" (rsp/resolve-tab nil true)))
    (is (= "pins" (rsp/resolve-tab nil false))))

  (testing "get-assistant-tab returns nil for 'never chosen', not a default"
    (is (nil? (settings/get-assistant-tab -1 -1))
      "a default here would make resolve-tab's kind-aware branch dead code"))

  (testing "transcript is a persistable tab"
    (is (contains? settings/side-panel-tabs "transcript")
      "otherwise save-assistant-tab silently rewrites the user's choice to pins"))

  (testing "explicit choices pass through on both kinds"
    (is (= "assistant" (rsp/resolve-tab "assistant" true)))
    (is (= "assistant" (rsp/resolve-tab "assistant" false)))
    (is (= "pins" (rsp/resolve-tab "pins" true))))

  (testing "an unrecognized value is treated as no preference, not as pins"
    ;; "bogus" and nil both mean 'we have no usable preference', so the kind
    ;; decides in both cases. get-assistant-tab already filters unknown values
    ;; to nil, so this only guards direct callers — but the two must agree.
    (is (= (rsp/resolve-tab nil true) (rsp/resolve-tab "bogus" true)))
    (is (= (rsp/resolve-tab nil false) (rsp/resolve-tab "bogus" false)))
    (is (= "pins" (rsp/resolve-tab "bogus" false)))))

;; ---------------------------------------------------------------------------
;; Storage rate label — one formatter, two render sites
;; ---------------------------------------------------------------------------

(deftest storage-rate-label
  (testing "nil when metering is off — the caller must say nothing, not \"0\""
    (with-redefs [meter/metering-enabled? (constantly false)]
      (is (nil? (meter/storage-rate-label)))))

  (with-redefs [meter/metering-enabled? (constantly true)]
    (testing "an integer rate reads as an integer"
      (with-redefs [meter/rate-iqd-per-gb-month (constantly 5.0)]
        (is (= "5 credits per GB-month" (meter/storage-rate-label))))
      (with-redefs [meter/rate-iqd-per-gb-month (constantly 100.0)]
        (is (= "100 credits per GB-month" (meter/storage-rate-label)))))

    (testing "one credit is singular"
      (with-redefs [meter/rate-iqd-per-gb-month (constantly 1.0)]
        (is (= "1 credit per GB-month" (meter/storage-rate-label)))))

    (testing "a fractional rate keeps two places"
      (with-redefs [meter/rate-iqd-per-gb-month (constantly 0.5)]
        (is (= "0.50 credits per GB-month" (meter/storage-rate-label))))
      (with-redefs [meter/rate-iqd-per-gb-month (constantly 12.75)]
        (is (= "12.75 credits per GB-month" (meter/storage-rate-label)))))))

;; ---------------------------------------------------------------------------
;; §15.3 7.1 — Transcription cost estimate
;; ---------------------------------------------------------------------------

(deftest transcription-cost-estimate
  ;; fx and markup redefined rather than read from config: the assertions below
  ;; are the §10 pricing table, and a deployment that retunes either would make
  ;; a correct implementation fail.
  (with-redefs [config/credits-enabled? (constantly true)
                config/fx-iqd-per-usd (constantly 1320)
                credits/resolve-markup (constantly 2)]
    (testing "reproduces §10's published table exactly"
      (is (= 3 (video/transcription-cost-estimate 1 10)) "10 s clip")
      (is (= 80 (video/transcription-cost-estimate 1 300)) "5 min")
      (is (= 682 (video/transcription-cost-estimate 1 2580)) "43 min")
      (is (= 2852 (video/transcription-cost-estimate 1 10800)) "3 h lecture"))

    (testing "an unreadable duration yields nil, never 0 — the caller shows nothing"
      (is (nil? (video/transcription-cost-estimate 1 nil)))
      (is (nil? (video/transcription-cost-estimate 1 0)))
      (is (nil? (video/transcription-cost-estimate 1 -5)))
      (is (nil? (video/transcription-cost-estimate 1 "43")) "non-numeric is not a duration")))

  (testing "self-host shows no estimate — there is no charge to predict"
    (with-redefs [config/credits-enabled? (constantly false)
                  config/fx-iqd-per-usd (constantly 1320)
                  credits/resolve-markup (constantly 2)]
      (is (nil? (video/transcription-cost-estimate 1 2580)))))

  (testing "unconfigured pricing shows no estimate rather than throwing"
    ;; charge-iqd-from-usd fails closed on the debit side, which is right there
    ;; and wrong here — an unconfigured deployment must not 500 a modal.
    (with-redefs [config/credits-enabled? (constantly true)
                  config/fx-iqd-per-usd (constantly nil)
                  credits/resolve-markup (constantly 2)]
      (is (nil? (video/transcription-cost-estimate 1 2580))))
    (with-redefs [config/credits-enabled? (constantly true)
                  config/fx-iqd-per-usd (constantly 1320)
                  credits/resolve-markup (constantly nil)]
      (is (nil? (video/transcription-cost-estimate 1 2580))))))

;; ---------------------------------------------------------------------------
;; §15.3 7.2 — finalize's transcribe param
;; ---------------------------------------------------------------------------

(deftest transcribe-param-parsing
  (testing "absent means transcribe — a client that predates the flag stays correct"
    (is (true? (vh/transcribe-requested? {})))
    (is (true? (vh/transcribe-requested? {"session_id" "abc"}))))

  (testing "only the literal string \"false\" turns it off"
    (is (false? (vh/transcribe-requested? {"transcribe" "false"}))))

  (testing "everything else transcribes — the fail-safe direction is the default"
    (is (true? (vh/transcribe-requested? {"transcribe" "true"})))
    (is (true? (vh/transcribe-requested? {"transcribe" ""})))
    (is (true? (vh/transcribe-requested? {"transcribe" "yes"})))
    (is (true? (vh/transcribe-requested? {"transcribe" "0"}))
      "0 is not false here — URLSearchParams sends the string \"false\"")
    (is (true? (vh/transcribe-requested? {"transcribe" "FALSE"}))
      "case-sensitive by design: the only writer is our own boolean->string")))

;; ---------------------------------------------------------------------------
;; §15.3 7.4 — duration summed over the CURRENT selection
;; ---------------------------------------------------------------------------

(deftest selection-duration
  (let [a ["a.mp4" 100 1] b ["b.mp4" 200 2] c ["c.mkv" 300 3]]
    (testing "sums the files passed, not the map's keys"
      (is (= {:seconds 300.0 :unknown 0}
            (import-modal/selection-duration [a b] {a 100.0 b 200.0})))
      (is (= {:seconds 100.0 :unknown 0}
            (import-modal/selection-duration [a] {a 100.0 b 200.0}))
        "a removed file's leftover entry must not still be charged for"))

    (testing "unprobed and unreadable both count as unknown"
      (is (= {:seconds 100.0 :unknown 1}
            (import-modal/selection-duration [a b] {a 100.0}))
        "b never probed")
      (is (= {:seconds 100.0 :unknown 1}
            (import-modal/selection-duration [a b] {a 100.0 b nil}))
        "b probed, browser could not decode it")
      (is (= {:seconds 0.0 :unknown 2}
            (import-modal/selection-duration [a b] {a nil b nil}))
        "nothing readable — the caller falls back to the hourly rate"))

    (testing "edge shapes"
      (is (= {:seconds 0.0 :unknown 0} (import-modal/selection-duration [] {})))
      (is (= {:seconds 0.0 :unknown 1} (import-modal/selection-duration [c] {c 0.0}))
        "a zero duration is not a length")
      (is (= {:seconds 0.0 :unknown 1} (import-modal/selection-duration [c] {c -1.0}))
        "nor is a negative one"))))

;; ---------------------------------------------------------------------------
;; §15.3 5.6–5.8 / 6.1–6.3 — the two copy paths
;; ---------------------------------------------------------------------------

(deftest transcription-copy
  (testing "an exact figure is claimed only when every file's length was read"
    (is (= "≈ 682 credits to transcribe 43:00 of video"
          (import-modal/transcription-estimate-message {:credits 682 :hourly 951} 2580 0))))

  (testing "a partial selection is stated as a floor plus what is missing"
    (is (= "≈ 682 credits to transcribe 43:00 of video, plus 1 file of unknown length"
          (import-modal/transcription-estimate-message {:credits 682 :hourly 951} 2580 1)))
    (is (= "≈ 682 credits to transcribe 43:00 of video, plus 2 files of unknown length"
          (import-modal/transcription-estimate-message {:credits 682 :hourly 951} 2580 2))))

  (testing "no readable duration falls back to the rate"
    (is (= "Transcription costs about 951 credits per hour"
          (import-modal/transcription-estimate-message {:credits nil :hourly 951} 0 1))))

  (testing "one credit is singular — matches storage-rate-label's copy"
    (is (= "≈ 1 credit to transcribe 0:01 of video"
          (import-modal/transcription-estimate-message {:credits 1 :hourly 951} 1 0)))
    (is (= "No transcript. Transcribe to create one — about 1 credit for 0:04."
          (vt/empty-transcript-message 1 4000))))

  (testing "self-host says nothing at all"
    (is (nil? (import-modal/transcription-estimate-message {:credits nil :hourly nil} 0 1)))
    (is (nil? (import-modal/transcription-estimate-message {} 0 0))))

  (testing "the empty state never promises a transcript is coming"
    (let [msg (vt/empty-transcript-message 682 2580000)]
      (is (= "No transcript. Transcribe to create one — about 682 credits for 43:00." msg))
      (is (not (re-find #"yet|once|processed" msg))
        "the old copy was false for a video whose pipeline finished with a skip")))

  (testing "the figure is omitted, never the sentence"
    (is (= "No transcript. Transcribe to create one."
          (vt/empty-transcript-message nil 2580000))
      "credits disabled")
    (is (= "No transcript. Transcribe to create one."
          (vt/empty-transcript-message 682 nil))
      "duration never recorded")))

;; ---------------------------------------------------------------------------
;; §15.3 7.3 — the persisted default
;; ---------------------------------------------------------------------------

(deftest transcribe-default-setting
  (testing "absent means transcribe — the behaviour that shipped before the flag"
    (with-redefs [db/get-setting (constantly nil)]
      (is (true? (settings/get-video-transcribe-on-upload 1)))))

  (testing "both stored values round-trip"
    (with-redefs [db/get-setting (constantly "true")]
      (is (true? (settings/get-video-transcribe-on-upload 1))))
    (with-redefs [db/get-setting (constantly "false")]
      (is (false? (settings/get-video-transcribe-on-upload 1)))))

  (testing "an unrecognized stored value is not transcription"
    (with-redefs [db/get-setting (constantly "maybe")]
      (is (false? (settings/get-video-transcribe-on-upload 1)))
      "only \"true\" and absence mean true — a corrupt row must not bill"))

  (testing "the write coerces to a boolean string"
    (let [!written (atom nil)]
      (with-redefs [db/set-setting (fn [_ k v] (reset! !written [k v]))]
        (is (= {:success true} (settings/save-video-transcribe-on-upload 1 false)))
        (is (= [settings/VIDEO_TRANSCRIBE_ON_UPLOAD "false"] @!written))
        (settings/save-video-transcribe-on-upload 1 "anything truthy")
        (is (= [settings/VIDEO_TRANSCRIBE_ON_UPLOAD "true"] @!written)))))

  (testing "a failed write is reported, not thrown"
    (with-redefs [db/set-setting (fn [& _] (throw (Exception. "db down")))]
      (let [r (settings/save-video-transcribe-on-upload 1 false)]
        (is (false? (:success r)))
        (is (string? (:error r)))))))
