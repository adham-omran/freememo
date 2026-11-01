(ns freememo.youtube-clip-test
  "Resume, the end-of-clip pause, and range validation
   (plans/youtube-kind.md 3.7, 4.2, 4.5, 5.2). Pure, no database."
  (:require [clojure.test :refer [deftest is testing are]]
            [freememo.youtube-clip :as clip]))

(def ^:private a-clip {:start-ms 10000 :end-ms 20000})

(deftest a-root-resumes-at-its-position-unless-near-the-end
  (is (= 0 (clip/resume-ms {:last-pos-ms 0})))
  (is (= 0 (clip/resume-ms {})))
  (is (= 42000 (clip/resume-ms {:last-pos-ms 42000 :duration-ms 100000})))
  (is (= 42000 (clip/resume-ms {:last-pos-ms 42000})) "an unknown duration resumes")
  (is (= 0 (clip/resume-ms {:last-pos-ms 99500 :duration-ms 100000})) "within 1 s of the end"))

(deftest a-clip-resumes-inside-its-range-or-at-its-start
  (is (= 15000 (clip/resume-ms (assoc a-clip :last-pos-ms 15000))))
  (is (= 10000 (clip/resume-ms (assoc a-clip :last-pos-ms 10000))))
  (is (= 10000 (clip/resume-ms (assoc a-clip :last-pos-ms 20000))) "the end is outside")
  (is (= 10000 (clip/resume-ms (assoc a-clip :last-pos-ms 3000))))
  (is (= 10000 (clip/resume-ms a-clip))))

(deftest playback-across-the-end-is-a-crossing
  (is (clip/crossed-end? a-clip 19800 20050 1))
  (is (clip/crossed-end? a-clip 19900 20000 1) "landing exactly on the end")
  (testing "a late poll still counts"
    (is (clip/crossed-end? a-clip 19700 20200 1)))
  (testing "faster playback moves the clock further per poll"
    (is (clip/crossed-end? a-clip 19500 20400 2))
    (is (not (clip/crossed-end? a-clip 19500 20400 1)))))

(deftest a-seek-past-the-end-is-not-a-crossing
  (is (not (clip/crossed-end? a-clip 12000 25000 1)) "a jump from inside")
  (is (not (clip/crossed-end? a-clip 21000 21250 1)) "already past the end")
  (is (not (clip/crossed-end? a-clip 5000 5250 1)) "before the start")
  (is (not (clip/crossed-end? a-clip nil 20050 1)) "no previous poll")
  (is (not (clip/crossed-end? {} 19800 20050 1)) "a root video never pauses"))

(deftest after-a-seek-back-the-next-crossing-pauses-again
  ;; 5.2.2: a seek past the end, then back inside, then playback across it.
  (is (not (clip/crossed-end? a-clip 15000 30000 1)))
  (is (not (clip/crossed-end? a-clip 30000 18000 1)))
  (is (clip/crossed-end? a-clip 19900 20100 1)))

(deftest a-valid-range-has-no-error
  (is (nil? (clip/range-error 0 1000 nil {})))
  (is (nil? (clip/range-error 10000 20000 60000 {})))
  (is (nil? (clip/range-error 12000 18000 60000 a-clip)) "inside the parent clip")
  (is (nil? (clip/range-error 10000 20000 60000 a-clip)) "the parent's exact range"))

(deftest an-invalid-range-says-why
  (are [start end dur parent] (string? (clip/range-error start end dur parent))
    nil 1000 nil {}
    1000 nil nil {}
    -1 1000 nil {}
    5000 5000 nil {}
    6000 5000 nil {}
    1000 1500 nil {}
    1000 70000 60000 {}
    9000 15000 60000 a-clip
    15000 21000 60000 a-clip))

(deftest a-typed-time-is-read-in-ms
  (are [s ms] (= ms (clip/parse-time s))
    "0" 0
    "45" 45000
    "90" 90000
    "1:05" 65000
    "1:05.5" 65500
    "1:05.25" 65250
    "75:00" 4500000
    "1:02:03" 3723000
    " 2:00 " 120000
    "1:60" nil
    "1:02:60" nil
    "1:60:00" nil
    "" nil
    "abc" nil
    "1:2:3:4" nil
    nil nil))
