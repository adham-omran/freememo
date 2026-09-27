(ns freememo.topic-interval-bound-test
  "DB-backed checks for the upper bound on a topic's review interval
   (plans/topic-interval-upper-bound.md). Without the bound, the 27th advance
   at the default A-Factor of 2.0 computes NOW() + 2^27 days, and PostgreSQL
   rejects the UPDATE with `timestamp out of range`.

   SKIPs (not fails) when no database is reachable, like
   `getting-started-progress-test`. Every test creates its own user and
   deletes it."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- one [sql & params]
  (jdbc/execute-one! db/ds (into [sql] params) {:builder-fn rs/as-unqualified-maps}))

(defn- new-user! []
  (let [tag (str "interval-bound-" (System/nanoTime))]
    (:id (one "INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
           tag (str tag "@example.test")))))

(defn- new-topic! [uid]
  (:topics/id (db/create-topic! {:user-id uid :kind "basic" :title "Topic"})))

(defn- schedule
  "The stored interval, and how many days from now next_review_at is."
  [topic-id]
  (one "SELECT interval_days::float8 AS ivl,
               (EXTRACT(EPOCH FROM (next_review_at - NOW())) / 86400.0)::float8 AS ahead
          FROM topics WHERE id = ?"
    topic-id))

(defn- thrown-by [f]
  (try (f) nil (catch Exception e e)))

(def ^:private cap db/max-topic-interval-days)

;; The two reads of NOW() are microseconds apart, so the date and the stored
;; interval agree to far better than this.
(def ^:private one-minute (/ 1.0 1440))

(deftest ^:db advancing-past-the-old-overflow-stays-within-the-bound
  (if-not (db-up?)
    (println "SKIP advancing-past-the-old-overflow-stays-within-the-bound — no database")
    (do (db/setup-schema)
        (let [uid (new-user!)]
          (try
            (let [topic (new-topic! uid)]
              (testing "the first advance is unchanged: 1 day × 2.0"
                (is (nil? (thrown-by #(db/advance-topic! uid topic))))
                (is (= 2.0 (:ivl (schedule topic)))))
              (testing "40 advances pass the 27th, which used to throw"
                (doseq [n (range 2 41)]
                  (let [e (thrown-by #(db/advance-topic! uid topic))
                        {:keys [ivl ahead]} (schedule topic)]
                    (is (nil? e) (str "advance " n " threw " (some-> e ex-message)))
                    (is (<= ivl cap) (str "advance " n " stored interval " ivl))
                    (is (<= ahead cap) (str "advance " n " scheduled " ahead " days ahead"))
                    (is (< (Math/abs (- ivl ahead)) one-minute)
                      (str "advance " n ": interval " ivl " but date " ahead " days ahead")))))
              (testing "the run ends at the bound"
                (let [{:keys [ivl ahead]} (schedule topic)]
                  (is (= cap ivl))
                  (is (< (- cap ahead) one-minute)))))
            (finally
              (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" uid])))))))

(deftest ^:db a-stored-interval-above-the-bound-advances-to-the-bound
  (if-not (db-up?)
    (println "SKIP a-stored-interval-above-the-bound-advances-to-the-bound — no database")
    (do (db/setup-schema)
        (let [uid (new-user!)]
          (try
            ;; A SuperMemo import stores its u16 interval, up to 65535 days.
            (let [topic (new-topic! uid)]
              (one "UPDATE topics SET interval_days = 65535 WHERE id = ? RETURNING id" topic)
              (is (nil? (thrown-by #(db/advance-topic! uid topic))))
              (let [{:keys [ivl ahead]} (schedule topic)]
                (is (= cap ivl))
                (is (< (- cap ahead) one-minute))))
            (finally
              (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" uid])))))))

(deftest ^:db a-typed-postpone-stays-within-the-bound
  (if-not (db-up?)
    (println "SKIP a-typed-postpone-stays-within-the-bound — no database")
    (do (db/setup-schema)
        (let [uid (new-user!)]
          (try
            (let [topic (new-topic! uid)]
              (testing "an ordinary postpone is unchanged"
                (is (nil? (thrown-by #(db/postpone-topic! uid topic 7))))
                (is (< (Math/abs (- 7.0 (:ahead (schedule topic)))) one-minute)))
              (testing "a typed 1e9 days, which used to throw, lands at the bound"
                (is (nil? (thrown-by #(db/postpone-topic! uid topic 1000000000))))
                (let [{:keys [ivl ahead]} (schedule topic)]
                  (is (<= ahead cap))
                  (is (< (- cap ahead) one-minute))
                  (is (= 1.0 ivl) "postpone leaves the interval alone"))))
            (finally
              (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" uid])))))))
