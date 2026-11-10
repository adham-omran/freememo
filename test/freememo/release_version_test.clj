(ns freememo.release-version-test
  "The Release Version of a commit (docs/adr/0002)."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.local-tray :as tray]
            [freememo.release-version :as rv]
            [freememo.update-feed :as uf])
  (:import [java.time Instant]))

(defn- at [iso] (rv/from-epoch-seconds (.getEpochSecond (Instant/parse iso))))

(deftest minute-of-the-month
  (testing "the first minute of a month"
    (is (= "26.11.0" (at "2026-11-01T00:00:00Z"))))
  (testing "10 November 2026, 14:32"
    (is (= "26.11.13832" (at "2026-11-10T14:32:00Z"))))
  (testing "the last minute of a 31-day month"
    (is (= "26.12.44639" (at "2026-12-31T23:59:59Z"))))
  (testing "a leap day"
    (is (= "28.2.40560" (at "2028-02-29T04:00:00Z")))
    (is (= "28.2.40320" (at "2028-02-29T00:00:00Z")))))

(deftest the-shell-script-agrees
  (doseq [iso ["2026-11-01T00:00:00Z" "2026-11-10T14:32:00Z" "2026-12-31T23:59:59Z"
               "2028-02-29T04:00:00Z" "2027-01-09T08:09:00Z" "2030-09-30T23:59:00Z"]]
    (let [secs (.getEpochSecond (Instant/parse iso))
          p (.start (ProcessBuilder. ^java.util.List ["sh" "scripts/release-version.sh" "--epoch" (str secs)]))
          out (.trim ^String (slurp (.getInputStream p)))]
      (is (zero? (.waitFor p)) iso)
      (is (= (rv/from-epoch-seconds secs) out) iso))))

(deftest it-does-not-depend-on-the-time-zone-of-the-computer
  (let [secs (.getEpochSecond (Instant/parse "2026-11-30T23:59:00Z"))
        zone (java.util.TimeZone/getDefault)]
    (try
      (doseq [id ["Pacific/Kiritimati" "America/Los_Angeles" "UTC"]]
        (java.util.TimeZone/setDefault (java.util.TimeZone/getTimeZone ^String id))
        (is (= "26.11.43199" (rv/from-epoch-seconds secs)) id))
      (finally (java.util.TimeZone/setDefault zone)))))

(deftest every-part-fits-the-windows-msi-limit
  (let [largest (at "2099-12-31T23:59:59Z")
        [y m n] (map parse-long (.split ^String largest "\\."))]
    (is (<= y 255))
    (is (<= m 255))
    (is (<= n 65535))
    (is (= 44639 n))))

(deftest a-later-commit-gives-a-larger-version-in-the-feed-order
  (let [times ["2026-11-30T23:59:00Z" "2026-12-01T00:00:00Z" "2026-12-01T00:01:00Z"
               "2026-12-31T23:59:00Z" "2027-01-01T00:00:00Z" "2027-01-01T00:09:00Z"
               "2027-01-02T00:00:00Z" "2027-02-28T10:00:00Z"]
        versions (map at times)]
    (doseq [[a b] (partition 2 1 versions)]
      (is (uf/newer? b a) (str b " is newer than " a))
      (is (not (uf/newer? a b))))
    (testing "numeric per part, not text"
      (is (uf/newer? "26.11.10000" "26.11.9999"))
      (is (uf/newer? "26.12.0" "26.11.44639")))))

(deftest a-dev-build-has-no-version-to-check
  (is (nil? (uf/installed-version)) "no manifest in a test run")
  (is (= "Version dev" (tray/version-label "dev")))
  (is (= "Version 26.11.13832" (tray/version-label "26.11.13832"))))
