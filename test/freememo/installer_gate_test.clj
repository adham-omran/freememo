(ns freememo.installer-gate-test
  "Ticket 20: the Runtime gate, as a pure decision."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.desktop-version :as dv]
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
    (testing "edits pending (also offline, or signed out): no link, because a sync can still send them"
      (is (= {:status :unsynced} (gate/decide need 3)))
      (is (= {:status :unsynced} (gate/decide need 3 :offline)))
      (is (= {:status :unsynced} (gate/decide need 3 :signed-out))))
    (testing "nothing pending: the link"
      (is (= (assoc need :status :ready) (gate/decide need 0))))))

(deftest update-required-shows-the-link-at-once
  (let [need {:version "2026.12.5" :url "https://x/linux.deb"}]
    (testing "edits pending and the server refuses this install: link, with the count"
      (is (= (assoc need :status :ready :pending 3) (gate/decide need 3 :update-required))))
    (testing "nothing pending: the plain link"
      (is (= (assoc need :status :ready) (gate/decide need 0 :update-required))))
    (testing "banner says how many edits stay here"
      (let [[b] (fb/update-banners {:installer (gate/decide need 3 :update-required)})]
        (is (= "https://x/linux.deb" (:href b)))
        (is (re-find #"3 edits are not synced" (:message b)))
        (is (re-find #"stay on this computer" (:message b)))))))

(deftest no-sync-license-shows-the-link-at-once
  ;; With no Sync License the server refuses every sync, so the edits of this
  ;; install stay pending for good. Waiting for them hid the link for ever.
  (let [need {:version "2026.12.5" :url "https://x/linux.deb"}]
    (is (= (assoc need :status :ready) (gate/decide need 3 :license-refused)))
    (testing "the banner has the link and says nothing about sync"
      (let [[b :as bs] (fb/update-banners {:installer (gate/decide need 3 :license-refused)})]
        (is (= 1 (count bs)))
        (is (= "https://x/linux.deb" (:href b)))
        (is (= "This version of FreeMemo needs a new installer." (:message b)))))))

(deftest state-passes-the-sync-state-to-decide
  (try
    (with-redefs [gate/pending-count (constantly 4)]
      (reset! @#'gate/!need {:version "2026.12.5" :url "https://x/linux.deb"})
      (with-redefs-fn {(requiring-resolve 'freememo.sync-client/sync-state) (constantly :license-refused)}
        #(is (= :ready (:status (gate/state))) "no Sync License: the link")))
    (finally (gate/reset-state!))))

(deftest on-feed-syncs-then-shows-the-link
  (let [pending (atom 2) syncs (atom 0) seen (atom [])]
    (try
      (with-redefs [gate/pending-count (fn [] @pending)]
        (gate/on-feed! (feed (inc dv/runtime))
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
