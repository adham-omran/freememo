(ns freememo.storage-metered-copy-test
  "Settings storage copy follows the metric passed to the actual rent calculation."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [freememo.config :as config]
            [freememo.db :as db]
            [freememo.quota :as quota]
            [freememo.storage-copy :as copy]
            [freememo.storage-meter :as meter]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def opts {:builder-fn rs/as-unqualified-lower-maps})

(deftest settings-text-follows-the-configured-meter
  (is (not (str/includes? (copy/storage-usage nil) "billed")))
  (is (not (str/includes? (copy/credit-spend nil) "Storage costs")))
  (is (str/includes? (copy/credit-spend "5 credits per GB-month") "Storage costs 5 credits per GB-month"))
  (is (str/includes? (copy/storage-usage "5 credits per GB-month") "Stored library files are billed"))
  (is (not (str/includes? (copy/storage-usage "5 credits per GB-month") "everything else is free"))))

(deftest uploaded-files-and-video-both-reach-the-rent-calculation
  (let [fixture (jdbc/with-transaction [tx db/ds]
                  (let [user (:id (jdbc/execute-one! tx
                                    ["INSERT INTO users(username,password_hash,credit_balance_iqd) VALUES (?, ?, 100) RETURNING id"
                                     (str "storage-copy-test-" (random-uuid)) "not-a-login-hash"] opts))
                        root (:id (jdbc/execute-one! tx
                                    ["INSERT INTO topics(user_id,kind,title) VALUES (?, 'basic', 'Storage copy fixture') RETURNING id" user] opts))
                        document (:id (jdbc/execute-one! tx
                                        ["INSERT INTO topics(user_id,parent_id,kind,title) VALUES (?, ?, 'pdf', 'File') RETURNING id" user root] opts))
                        video (:id (jdbc/execute-one! tx
                                     ["INSERT INTO topics(user_id,parent_id,kind,title) VALUES (?, ?, 'video', 'Video') RETURNING id" user root] opts))
                        oid (:oid (jdbc/execute-one! tx ["SELECT lo_from_bytea(0, ?)::bigint AS oid" (byte-array 8192)] opts))]
                    (quota/check-and-bump! tx user 4096)
                    (jdbc/execute! tx ["INSERT INTO topic_files(topic_id,file_data,file_size,mime_type) VALUES (?, ?, 4096, 'application/pdf')"
                                       document (byte-array 4096)])
                    (quota/reserve-bytes! tx user 8192)
                    (jdbc/execute! tx ["INSERT INTO topic_videos(topic_id,lo_oid,byte_size,mime_type) VALUES (?, CAST(CAST(? AS bigint) AS oid), 8192, 'video/webm')"
                                       video oid])
                    (jdbc/execute! tx ["UPDATE users SET last_metered_at=now()-interval '30 days' WHERE id=?" user])
                    {:user user :root root :oid oid}))
        seen (atom nil)
        accrue db/accrue-storage-charge!]
    (try
      (is (= 12288 (db/subtree-file-bytes db/ds (:root fixture))))
      (is (= 12288 (quota/get-user-usage db/ds (:user fixture))))
      (with-redefs [config/credits-enabled? (constantly true)
                    meter/rate-iqd-per-gb-month (constantly 5)
                    db/accrue-storage-charge!
                    (fn [user charge]
                      (accrue user (fn [input] (reset! seen input) (charge input))))]
        (let [result (meter/accrue! (:user fixture) true)]
          (is (= 12288 (:usage-bytes @seen)))
          (is (pos? (:carry-micro result)))))
      (finally
        (jdbc/with-transaction [tx db/ds]
          (jdbc/execute! tx ["DELETE FROM users WHERE id=?" (:user fixture)])
          (jdbc/execute! tx ["SELECT lo_unlink(CAST(CAST(? AS bigint) AS oid))" (:oid fixture)]))))))
