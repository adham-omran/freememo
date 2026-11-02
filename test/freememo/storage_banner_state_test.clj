(ns freememo.storage-banner-state-test
  "The account-state banners and the video pane's lines
   (plans/remove-toasts.md 1.3). The pause banner derives from the grace column,
   the removal notice from three `users` columns, and Dismiss keeps the date the
   video pane still needs.

   Needs PostgreSQL with the `storage_removal_*` and `topic_videos.notice`
   columns. Skips when the port refuses connections."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.config :as config]
            [freememo.db :as db]
            [freememo.storage-meter :as meter]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT storage_removal_seen FROM users LIMIT 1"])
       (jdbc/execute-one! db/ds ["SELECT notice FROM topic_videos LIMIT 1"])
       true
       (catch Exception _ false)))

(defn- with-user! [test!]
  (let [uid (:id (jdbc/execute-one! db/ds
                   ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                    (str "storage-banner-" (random-uuid)) (str (random-uuid) "@example.test")]
                   opts))]
    (try (test! uid)
         (finally (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" uid])))))

(defmacro ^:private metered [& body]
  `(with-redefs [config/credits-enabled? (constantly true)
                 meter/rate-iqd-per-gb-month (constantly 5)]
     ~@body))

(deftest no-metering-no-account-banner
  (when (db-up?)
    (with-user! (fn [uid]
                  (with-redefs [config/credits-enabled? (constantly false)]
                    (is (nil? (meter/account-banner-state uid))))))))

(deftest the-pause-banner-follows-the-grace-column
  (when (db-up?)
    (with-user! (fn [uid]
                  (metered
                    (is (nil? (:paused (meter/account-banner-state uid))))
                    (db/set-storage-grace! uid (java.sql.Timestamp/valueOf "2026-11-02 10:00:00"))
                    (testing "the deadline is the grace start plus the grace days"
                      (is (= "16 November 2026"
                            (get-in (meter/account-banner-state uid) [:paused :until]))))
                    (testing "a banner read never writes: credits alone leave the window open"
                      (jdbc/execute! db/ds ["UPDATE users SET credit_balance_iqd = 5000 WHERE id = ?" uid])
                      (is (some? (:paused (meter/account-banner-state uid))))
                      (is (some? (db/storage-grace-started-at uid))))
                    (testing "a landed top-up closes the window and the banner goes with it"
                      (meter/top-up-landed! uid)
                      (is (nil? (:paused (meter/account-banner-state uid))))
                      (is (nil? (db/storage-grace-started-at uid)))))))))

(deftest the-removal-notice-is-shown-until-dismissed-and-keeps-its-date
  (when (db-up?)
    (with-user! (fn [uid]
                  (metered
                    (is (nil? (:removal (meter/account-banner-state uid))))
                    (db/record-storage-removal! uid 2)
                    (is (= 2 (get-in (meter/account-banner-state uid) [:removal :count])))
                    (testing "an unread notice adds the next sweep's count"
                      (db/record-storage-removal! uid 1)
                      (is (= 3 (get-in (meter/account-banner-state uid) [:removal :count]))))
                    (is (= :done (meter/dismiss-removal! uid)))
                    (is (nil? (:removal (meter/account-banner-state uid))))
                    (testing "Dismiss keeps the date for the video pane"
                      (is (some? (:at (db/get-storage-removal uid)))))
                    (testing "after Dismiss the next sweep starts a new count"
                      (db/record-storage-removal! uid 4)
                      (is (= 4 (get-in (meter/account-banner-state uid) [:removal :count])))))))))

(deftest the-video-pane-says-why-a-video-is-gone-or-paused
  (when (db-up?)
    (with-user! (fn [uid]
                  (metered
                    (let [topic (:id (jdbc/execute-one! db/ds
                                       ["INSERT INTO topics (user_id, title, kind, content)
                                         VALUES (?, 'V', 'video', '') RETURNING id" uid] opts))]
                      (testing "removed: no topic_videos row and a removal on record"
                        (db/record-storage-removal! uid 1)
                        (is (some? (:removed-on (meter/video-pane-status uid topic)))))
                      (testing "paused and noticed: a row, a grace window and a stored notice"
                        (jdbc/execute! db/ds
                          ["INSERT INTO topic_videos (topic_id, lo_oid, byte_size, mime_type)
                            VALUES (?, 0, 1, 'video/mp4')" topic])
                        (db/set-video-notice! topic "No audio track to transcribe.")
                        (db/set-storage-grace! uid (java.sql.Timestamp/valueOf "2026-11-02 10:00:00"))
                        (let [s (meter/video-pane-status uid topic)]
                          (is (true? (:paused? s)))
                          (is (nil? (:removed-on s)))
                          (is (= "No audio track to transcribe." (:notice s))))
                        (db/set-video-notice! topic nil)
                        (is (nil? (:notice (meter/video-pane-status uid topic)))))))))))
