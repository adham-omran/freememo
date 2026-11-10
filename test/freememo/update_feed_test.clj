(ns freememo.update-feed-test
  "The pure half of the update feed (plans/local-install/issues/08-update-feed.md).
   The whole path is in `freememo.local-install-update-test` (Seam 1)."
  (:require [clojure.test :refer [deftest is]]
            [freememo.update-feed :as uf]))

(deftest versions-compare-by-number-not-by-text
  (is (uf/newer? "2026.11.512" "2026.11.99"))
  (is (uf/newer? "2027.1.1" "2026.12.900"))
  (is (not (uf/newer? "2026.11.99" "2026.11.99")))
  (is (not (uf/newer? "2026.11.98" "2026.11.99")))
  (is (uf/newer? "2026.12" "2026.11.5")))

(deftest a-version-that-does-not-parse-is-never-newer
  (is (not (uf/newer? "latest" "2026.11.99")))
  (is (not (uf/newer? nil "2026.11.99")))
  (is (not (uf/newer? "2027.1.1" nil))))

(deftest available-needs-a-newer-version-and-a-url-for-this-os
  (let [feed {"current_version" "2027.1.1"
              "installers" {"macos" "https://x/mac.dmg" "linux" ""}}]
    (is (= {:version "2027.1.1" :url "https://x/mac.dmg"} (uf/available feed "2026.11.1" "macos")))
    (is (nil? (uf/available feed "2027.1.1" "macos")) "the same version shows nothing")
    (is (nil? (uf/available feed "2026.11.1" "windows")) "no URL for this OS")
    (is (nil? (uf/available feed "2026.11.1" "linux")) "a blank URL")
    (is (nil? (uf/available {} "2026.11.1" "macos")))))
