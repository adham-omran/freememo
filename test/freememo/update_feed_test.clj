(ns freememo.update-feed-test
  "The pure half of the update feed (plans/local-install/issues/08-update-feed.md).
   The whole path is in `freememo.local-install-update-test` (Seam 1)."
  (:require [clojure.test :refer [deftest is]]
            [freememo.desktop-link :as link]
            [freememo.peer :as peer]
            [freememo.sync-client :as sync-client]
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

(def ^:private feed {"current_version" "2027.1.1" "installers" {"linux" "https://x/l.AppImage" "macos" "https://x/m" "windows" "https://x/w"}})

(deftest an-unreadable-feed-clears-a-stale-update
  (let [opts {:origin "https://example.test" :version "2026.11.1"}
        available #(:update-available (do (with-redefs [peer/local-install? (constantly true)
                                                        sync-client/status (constantly {:state :idle})]
                                            (uf/banner-state))))]
    (with-redefs [uf/fetch-feed (constantly feed)] (uf/refresh! opts))
    (is (some? (available)))
    (with-redefs [uf/fetch-feed (fn [_] (throw (ex-info "offline" {})))] (uf/refresh! opts))
    (is (nil? (available)) "an unreadable feed shows nothing")))

(deftest an-offline-start-checks-again-when-the-network-returns
  (let [listeners (atom [])
        reads (atom 0)
        first-read (promise)]
    (with-redefs [link/on-online! #(swap! listeners conj %)
                  uf/fetch-feed (fn [_] (when (= 1 (swap! reads inc)) (deliver first-read true))
                                  (throw (ex-info "offline" {})))
                  uf/!timer (atom nil)]
      (uf/start! {:origin "https://example.test" :version "2026.11.1"})
      (is (= 1 (count @listeners)) "start! listens for the network")
      @first-read
      (Thread/sleep 100)
      ((first @listeners))
      (let [deadline (+ (System/currentTimeMillis) 5000)]
        (while (and (< @reads 2) (< (System/currentTimeMillis) deadline)) (Thread/sleep 20)))
      (is (= 2 @reads)))))
