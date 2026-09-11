(ns freememo.page-title-read-test
  "Owner-scoped title reads behind the tab title
   (plans/reactive-page-title.md §2).

   Needs PostgreSQL."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.curriculum :as curriculum]
            [freememo.db :as db]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})

(defn- create-user! []
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
          (str "page-title-test-" (random-uuid))
          (str (random-uuid) "@example.test")]
         opts)))

(defn- create-topic! [user-id title]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, kind, title) VALUES (?, 'basic', ?) RETURNING id"
          user-id title]
         opts)))

(defn- create-curriculum! [user-id title]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO curricula (user_id, title, daily_minutes) VALUES (?, ?, 30) RETURNING id"
          user-id title]
         opts)))

(deftest a-topic-title-reads-only-for-its-owner
  (let [owner (create-user!)
        stranger (create-user!)]
    (try
      (let [topic-id (create-topic! owner "Thermodynamics")]
        (testing "the owner reads the title"
          (is (= "Thermodynamics" (db/get-topic-title-for-user owner topic-id))))
        (testing "another user reads nil"
          (is (nil? (db/get-topic-title-for-user stranger topic-id))))
        (testing "a missing id reads nil"
          (is (nil? (db/get-topic-title-for-user owner Integer/MAX_VALUE))))
        (testing "a topic staged for deletion reads nil"
          (is (some? (db/stage-topic-for-deletion! owner topic-id)))
          (is (nil? (db/get-topic-title-for-user owner topic-id)))))
      (finally
        (jdbc/execute! db/ds ["DELETE FROM users WHERE id IN (?, ?)" owner stranger])))))

(deftest a-curriculum-title-reads-only-for-its-owner
  (let [owner (create-user!)
        stranger (create-user!)]
    (try
      (let [curriculum-id (create-curriculum! owner "Physics I")]
        (testing "the owner reads the title"
          (is (= "Physics I" (curriculum/curriculum-title owner curriculum-id))))
        (testing "another user reads nil"
          (is (nil? (curriculum/curriculum-title stranger curriculum-id))))
        (testing "a missing id reads nil"
          (is (nil? (curriculum/curriculum-title owner Integer/MAX_VALUE))))
        (testing "a curriculum staged for deletion reads nil"
          (curriculum/stage-curriculum-for-deletion! owner curriculum-id)
          (is (nil? (curriculum/curriculum-title owner curriculum-id)))))
      (finally
        (jdbc/execute! db/ds ["DELETE FROM users WHERE id IN (?, ?)" owner stranger])))))
