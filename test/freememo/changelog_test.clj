(ns freememo.changelog-test
  "The privacy-correction release renders as a broadcast. A dry run reads
   the release, so a human can send exactly this text in ticket 04 of
   plans/paying-customer-commitments."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [freememo.changelog :as changelog]))

(def ^:private correction-opening
  "The privacy policy said that FreeMemo removes deleted data for good. That was wrong.")

(deftest privacy-correction-renders-in-the-broadcast
  (with-redefs [changelog/broadcast-recipients (constantly [])]
    (let [release (->> (changelog/parse-releases (slurp changelog/changelog-path))
                    (filter #(str/includes? (str/replace (:body %) #"\s+" " ") correction-opening))
                    first)
          payload (changelog/render-broadcast release)]
      (is (some? release) "CHANGELOG.md has a release with the correction")
      (is (some? payload) "the release has a For users section")
      (testing "the text is the approved wording"
        (doseq [s [correction-opening
                   "FreeMemo makes a backup every night, and a backup keeps a copy of deleted data for up to 6 months."
                   "Amazon Web Services, which stores the offsite backups, and Cloudflare, which carries every connection."
                   "Read the policy: https://freememo.net/privacy"]]
          (is (str/includes? (str/replace (:text payload) #"\s+" " ") s) s))))))
