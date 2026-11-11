(ns freememo.installer-gate-test
  "Ticket 20: the Runtime gate, as a pure decision."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.failure-banners :as fb]
            [freememo.installer-gate :as gate]))

(defn- feed [runtime]
  {"current_version" "2026.12.5" "runtime" runtime
   "installers" {"macos" "https://x/mac.dmg" "windows" "https://x/win.msi" "linux" "https://x/linux.deb"}})

(deftest needed-only-for-a-higher-runtime
  (testing "same Runtime: an App Update, not this gate"
    (is (nil? (gate/needed (feed 1) "2026.11.1" 1 "linux"))))
  (testing "higher Runtime with the link of this OS"
    (is (= {:version "2026.12.5" :url "https://x/win.msi"} (gate/needed (feed 2) "2026.11.1" 1 "windows"))))
  (testing "not newer, or no feed"
    (is (nil? (gate/needed (feed 2) "2026.12.5" 1 "linux")))
    (is (nil? (gate/needed nil "2026.11.1" 1 "linux")))))

(deftest decide-waits-for-the-sync
  (let [need {:version "2026.12.5" :url "https://x/linux.deb"}]
    (is (nil? (gate/decide nil 0)))
    (testing "edits pending (also when sync cannot finish, offline or refused): no link"
      (is (= {:status :unsynced} (gate/decide need 3))))
    (testing "nothing pending: the link"
      (is (= (assoc need :status :ready) (gate/decide need 0))))))

(deftest on-feed-syncs-then-shows-the-link
  (let [pending (atom 2) syncs (atom 0) seen (atom [])]
    (try
      (with-redefs [gate/pending-count (fn [] @pending)]
        (gate/on-feed! (feed 2)
          {:installed "2026.11.1"
           :sync! (fn [] (swap! syncs inc) (reset! pending 0))
           :on-change #(swap! seen conj %)})
        (let [deadline (+ (System/currentTimeMillis) 5000)]
          (while (and (< (count @seen) 1) (< (System/currentTimeMillis) deadline)) (Thread/sleep 50)))
        (is (= 1 @syncs))
        (is (= :ready (:status (first @seen)))))
      (finally (gate/reset-state!)))))

(deftest banners-for-the-gate
  (testing "unsynced: a message, no link"
    (let [[b :as bs] (fb/update-banners {:installer {:status :unsynced}})]
      (is (= 1 (count bs)))
      (is (re-find #"not yet synced" (:message b)))
      (is (nil? (:href b)))))
  (testing "ready: the link"
    (let [[b] (fb/update-banners {:installer {:status :ready :version "2026.12.5" :url "https://x/linux.deb"}})]
      (is (= "https://x/linux.deb" (:href b)))
      (is (= "Download the new installer" (:href-label b)))))
  (testing "no gate: nothing"
    (is (empty? (fb/update-banners {:installer nil})))))
