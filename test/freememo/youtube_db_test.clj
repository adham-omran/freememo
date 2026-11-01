(ns freememo.youtube-db-test
  "The YouTube writers (plans/youtube-kind.md 1.2, 2.4, 2.7, 3.8, 4.2, 4.3,
   7.1, 7.2).

   Needs a real Postgres, because the properties are about what commits
   together. SKIPS when no database is reachable. Every test creates its own
   user."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- new-user! []
  (let [tag (str "youtube-" (System/nanoTime))]
    (:id (jdbc/execute-one! db/ds
           ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
            tag (str tag "@example.test")]
           {:builder-fn rs/as-unqualified-maps}))))

(defn- count-of [sql & params]
  (:n (jdbc/execute-one! db/ds (into [sql] params) {:builder-fn rs/as-unqualified-maps})))

(def ^:private vid "dQw4w9WgXcQ")

(deftest the-table-rejects-a-malformed-row
  (if-not (db-up?)
    (println "SKIP: no database")
    (let [uid (new-user!)
          tid (:topics/id (db/create-topic! {:user-id uid :kind "youtube" :title "t"}))
          insert! (fn [video-id start end]
                    (jdbc/execute! db/ds
                      ["INSERT INTO topic_youtube (topic_id, video_id, start_ms, end_ms)
                        VALUES (?, ?, ?, ?)" tid video-id start end]))]
      (is (thrown? Exception (insert! "short" nil nil)) "a malformed id")
      (is (thrown? Exception (insert! vid 1000 nil)) "half a range")
      (is (thrown? Exception (insert! vid 5000 5000)) "an empty range"))))

(deftest a-root-import-commits-source-topic-and-row-together
  (if-not (db-up?)
    (println "SKIP: no database")
    (let [uid (new-user!)
          r (db/create-youtube-topic! uid {:video-id vid :title "A talk"
                                           :channel "A channel" :start-ms 90000})
          row (db/get-topic-youtube (:topic-id r))
          topic (db/get-topic (:topic-id r))
          source (db/get-source (:topics/source_id topic))]
      (is (= "youtube" (:topics/kind topic)))
      (is (= vid (:video_id row)))
      (is (nil? (:start_ms row)) "a root has no range")
      (is (= 90000 (:last_pos_ms row)) "the URL's start time is the first resume position")
      (is (= "motion_picture" (:sources/csl_type source)))
      (is (= "https://www.youtube.com/watch?v=dQw4w9WgXcQ" (:sources/url source)))
      (is (= [{:literal "A channel"}] (get-in source [:sources/csl :author])))
      (testing "a forced failure on the row insert leaves no topic and no source"
        (let [before-topics (count-of "SELECT count(*) AS n FROM topics WHERE user_id = ?" uid)
              before-sources (count-of "SELECT count(*) AS n FROM sources WHERE user_id = ?" uid)]
          (is (thrown? Exception
                (db/create-youtube-topic! uid {:video-id "not-an-id" :title "Broken"})))
          (is (= before-topics (count-of "SELECT count(*) AS n FROM topics WHERE user_id = ?" uid)))
          (is (= before-sources (count-of "SELECT count(*) AS n FROM sources WHERE user_id = ?" uid))))))))

(deftest a-root-is-found-by-video-id-unless-staged-for-deletion
  (if-not (db-up?)
    (println "SKIP: no database")
    (let [uid (new-user!)
          {:keys [topic-id]} (db/create-youtube-topic! uid {:video-id vid :title "V"})]
      (is (= topic-id (:topic-id (db/find-youtube-root uid vid))))
      (is (nil? (db/find-youtube-root (new-user!) vid)) "another user's video")
      (let [staged (db/stage-topic-for-deletion! uid topic-id)]
        (is (some? staged))
        (is (nil? (db/find-youtube-root uid vid)) "a staged delete does not count")
        (is (some? (db/get-topic-youtube topic-id)) "staging keeps the row")
        (testing "undo restores the topic, and its row was never removed"
          (db/restore-staged-document! uid {:id (:entry-id staged)
                                            :entity_refs [topic-id]
                                            :snapshot {:cards []}})
          (is (= topic-id (:topic-id (db/find-youtube-root uid vid))))
          (is (= vid (:video_id (db/get-topic-youtube topic-id)))))))))

(deftest a-clip-is-a-child-over-a-valid-range
  (if-not (db-up?)
    (println "SKIP: no database")
    (let [uid (new-user!)
          {root :topic-id} (db/create-youtube-topic! uid {:video-id vid :title "V"})
          _ (db/save-youtube-position! uid root 0 600000)
          r (db/create-youtube-clip! uid root 10000 20000 "V 0:10–0:20")
          clip-row (db/get-topic-youtube (:id r))
          clip-topic (db/get-topic (:id r))]
      (is (:success r) (pr-str r))
      (is (= root (:topics/parent_id clip-topic)))
      (is (:topics/is_extract clip-topic))
      (is (= [vid 10000 20000 600000 10000]
             ((juxt :video_id :start_ms :end_ms :duration_ms :last_pos_ms) clip-row))
          "C1: the clip carries the video id and opens at its start")
      (testing "N1: a clip of a clip stays inside its range"
        (is (:success (db/create-youtube-clip! uid (:id r) 12000 18000 "inner")))
        (is (not (:success (db/create-youtube-clip! uid (:id r) 9000 18000 "outside")))))
      (testing "an invalid range creates nothing"
        (let [before (count-of "SELECT count(*) AS n FROM topics WHERE user_id = ?" uid)]
          (is (not (:success (db/create-youtube-clip! uid root 20000 10000 "backwards"))))
          (is (not (:success (db/create-youtube-clip! uid root 0 700000 "past the end"))))
          (is (= before (count-of "SELECT count(*) AS n FROM topics WHERE user_id = ?" uid)))))
      (testing "another user cannot cut a clip"
        (is (= "Video not found"
               (:error (db/create-youtube-clip! (new-user!) root 0 5000 "theft")))))
      (testing "a hard delete cascades the rows away"
        (db/delete-topic-for-user! uid root)
        (is (zero? (count-of "SELECT count(*) AS n FROM topic_youtube WHERE topic_id IN (?, ?)"
                             root (:id r))))))))

(deftest the-position-save-is-owner-scoped-and-keeps-the-first-duration
  (if-not (db-up?)
    (println "SKIP: no database")
    (let [uid (new-user!)
          {tid :topic-id} (db/create-youtube-topic! uid {:video-id vid :title "V"})]
      (is (= 1 (db/save-youtube-position! uid tid 5000 300000)))
      (is (= 1 (db/save-youtube-position! uid tid 7000 999999)))
      (is (= [7000 300000] ((juxt :last_pos_ms :duration_ms) (db/get-topic-youtube tid))))
      (is (= 0 (db/save-youtube-position! (new-user!) tid 1 nil)) "a forged owner writes nothing")
      (is (= 7000 (:last_pos_ms (db/get-topic-youtube tid)))))))
