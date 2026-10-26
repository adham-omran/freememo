(ns freememo.gdoc-db-test
  "The Google Doc snapshot rules against a live database
   (plans/google-docs-pull.md, rows 3.1, 5.3 and 5.5). Tests skip when
   PostgreSQL is down."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- one [sql & params] (jdbc/execute-one! db/ds (into [sql] params) opts))

(defn- status-of [id] (:status (one "SELECT status FROM topics WHERE id = ?" id)))

(defn- in-queue? [user-id id]
  (contains? (set (map :topics/id (:rows (db/get-learning-queue user-id)))) id))

(deftest a-pull-retires-the-previous-snapshot-and-skips-an-equal-hash
  (when (db-up?)
    (let [u (:id (one "INSERT INTO users (username) VALUES (?) RETURNING id" (str "gdoc-" (System/nanoTime))))]
      (try
        (let [{:keys [root-id topic-id]} (db/create-gdoc-document!
                                           {:user-id u :title "Plan" :drive-file-id "file-1"
                                            :account-sub "sub-1"
                                            :snapshot-title "2026-10-21 10:00"
                                            :content "<p>one</p>" :hash "h1"})
              first-id topic-id]
          (testing "the root names its file and account, and is found by its file"
            (is (= {:drive_file_id "file-1" :google_account_sub "sub-1"}
                   (select-keys (db/gdoc-root u root-id) [:drive_file_id :google_account_sub])))
            (is (= root-id (db/find-gdoc-root u "file-1"))))
          (testing "the root is structural, and the snapshot is under review"
            (is (not (in-queue? u root-id)))
            (is (in-queue? u first-id)))
          (testing "an equal hash adds nothing"
            (is (= {:unchanged? true}
                   (db/add-gdoc-snapshot! {:user-id u :root-id root-id :title "t"
                                           :content "<p>one</p>" :hash "h1"})))
            (is (= 1 (count (db/gdoc-snapshots u root-id)))))
          (let [extract (:topics/id (db/create-topic! {:user-id u :parent-id first-id :title "ex"
                                                       :content "<p>one</p>" :extract? true}))
                {second-id :topic-id} (db/add-gdoc-snapshot! {:user-id u :root-id root-id
                                                              :title "2026-10-21 11:00"
                                                              :content "<p>two</p>" :hash "h2"})]
            (testing "F2: the earlier snapshot is done and out of the queue; the new one is due"
              (is (= "done" (status-of first-id)))
              (is (= "active" (status-of second-id)))
              (is (not (in-queue? u first-id)))
              (is (in-queue? u second-id)))
            (testing "the earlier snapshot's extract stays where it was, and stays due"
              (is (= first-id (:parent_id (one "SELECT parent_id FROM topics WHERE id = ?" extract))))
              (is (in-queue? u extract)))
            (testing "the snapshots read newest first"
              (is (= [second-id first-id] (mapv :id (db/gdoc-snapshots u root-id)))))
            (testing "a snapshot's content cannot be written"
              (is (nil? (db/update-topic-content! second-id "<p>edited</p>")))
              (is (= "<p>two</p>" (:content (one "SELECT content FROM topics WHERE id = ?" second-id)))))
            (testing "another user's root takes no snapshot"
              (is (nil? (db/add-gdoc-snapshot! {:user-id (inc u) :root-id root-id :title "t"
                                                :content "<p>x</p>" :hash "h3"}))))))
        (finally
          (jdbc/execute! db/ds ["DELETE FROM topics WHERE user_id = ?" u])
          (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" u]))))))
