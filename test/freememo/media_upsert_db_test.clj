(ns freememo.media-upsert-db-test
  "`db/upsert-media-row!` against PostgreSQL: concurrent stores of the same
   bytes for one user resolve to one row, one quota charge and no exception.

   Production shipped the check-then-insert shape, so two requests carrying the
   same pasted image both missed the lookup, and the loser of the insert raised
   `idx_media_user_sha`. The user saw \"Upload failed\" for an image already
   stored. Every test skips when the database is down."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.db :as db]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})

(defn- create-user! []
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
          (str "media-upsert-test-" (random-uuid))
          (str (random-uuid) "@example.test")]
         opts)))

(defn- usage-bytes [user-id]
  (:usage_bytes (jdbc/execute-one! db/ds
                  ["SELECT usage_bytes FROM users WHERE id = ?" user-id] opts)))

(defn- media-count [user-id]
  (:n (jdbc/execute-one! db/ds
        ["SELECT count(*) AS n FROM media WHERE user_id = ?" user-id] opts)))

(defn- random-bytes [n]
  (let [b (byte-array n)]
    (.nextBytes (java.util.Random.) b)
    b))

(defmacro ^:private with-user
  [sym & body]
  `(if-not (db-up?)
     (is true "PostgreSQL is unavailable; integration assertion skipped")
     (let [~sym (create-user!)]
       (try ~@body
         (finally
           (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" ~sym]))))))

(defn- store! [user-id bytes]
  (db/upsert-media-row! {:user-id user-id :kind "image"
                         :bytes bytes :mime-type "image/png"}))

(defn- store-concurrently!
  "Store `bytes` from `n` threads released together. Returns each result, or
   {:thrown msg}. The pause lets every thread reach the latch first — without
   it the early threads finish before the late ones start, and the race the
   test exists for never happens."
  [user-id bytes n]
  (let [ready (java.util.concurrent.CountDownLatch. n)
        gate (java.util.concurrent.CountDownLatch. 1)
        futs (mapv (fn [_]
                     (future
                       (.countDown ready)
                       (.await gate)
                       (try (store! user-id bytes)
                            (catch Exception e {:thrown (.getMessage e)}))))
                   (range n))]
    (.await ready)
    (Thread/sleep 20)
    (.countDown gate)
    (mapv deref futs)))

(deftest concurrent-stores-of-the-same-bytes-share-one-row
  (with-user user-id
    (dotimes [_ 10]
      (let [bytes (random-bytes 4096)
            before-usage (usage-bytes user-id)
            before-count (media-count user-id)
            results (store-concurrently! user-id bytes 8)]
        (testing "no store raises"
          (is (empty? (keep :thrown results))))
        (testing "every store names the same row"
          (is (= 1 (count (set (map :media/id results))))))
        (testing "exactly one store reports that it created the row"
          (is (= 1 (count (filter :media/inserted? results)))))
        (testing "one row, charged once"
          (is (= (inc before-count) (media-count user-id)))
          (is (= (+ before-usage (alength ^bytes bytes)) (usage-bytes user-id))))))))

(deftest a-repeat-store-reuses-the-row-and-charges-nothing
  (with-user user-id
    (let [bytes (random-bytes 2048)
          first-store (store! user-id bytes)
          after-first (usage-bytes user-id)
          second-store (store! user-id bytes)]
      (is (:media/inserted? first-store))
      (is (= {:media/id (:media/id first-store) :media/inserted? false}
             second-store))
      (is (= after-first (usage-bytes user-id))))))
