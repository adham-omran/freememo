(ns freememo.video-test
  "Pure-function tests for the incremental-video pieces whose arithmetic is
   easy to get subtly wrong and hard to notice: HTTP range parsing, chunk
   timestamp offsetting, and storage-rent pricing.

   Deliberately no database and no ffmpeg — the stateful paths are exercised
   against a live Postgres from the REPL; these are the parts a regression
   would silently corrupt."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.video :as video]
   [freememo.video-format :as vf]
   [freememo.video-http :as vh]
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
  (testing "only containers a browser can play are accepted — we never transcode"
    (is (video/supported-mime? "video/mp4"))
    (is (video/supported-mime? "VIDEO/WEBM") "case and whitespace tolerant")
    (is (not (video/supported-mime? "video/x-matroska")))
    (is (not (video/supported-mime? "application/octet-stream")))
    (is (not (video/supported-mime? nil))))

  (testing "extension is the fallback when the browser sends no usable type"
    (is (= "video/webm" (video/filename->mime "lecture.WEBM")))
    (is (= "video/quicktime" (video/filename->mime "clip.mov")))
    (is (= "video/mp4" (video/filename->mime "unknown.bin")))
    (is (= "video/mp4" (video/filename->mime nil)))))

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
