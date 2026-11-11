(ns freememo.learn-queue-db-test
  "The lean Learn queue against a live database (spec LRN-002,
   plans/low-end-tablet-performance/issues/03-lean-learn-queue.md).
   Tests skip when PostgreSQL is down."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]
            [freememo.learn-page :as learn-page])
  (:import (java.security MessageDigest)))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- one [sql & params] (jdbc/execute-one! db/ds (into [sql] params) opts))

(defn- md5-hex [^String s]
  (let [d (.digest (MessageDigest/getInstance "MD5") (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) d))))

(defn- add-topic!
  "Insert one topic by SQL, so each of the four order keys is set exactly.
   `due` is a day offset from today, or nil for a topic that was never reviewed."
  [user-id {:keys [kind priority sm-rank status due dismissed? title parent-id]
            :or {kind "basic" priority 50 status "active" title "t"}}]
  (:id (one "INSERT INTO topics (user_id, kind, title, content, priority, sm_rank, status,
                                 next_review_at, dismissed, parent_id)
             VALUES (?, ?, ?, ?, ?, ?, ?,
                     CASE WHEN ?::int IS NULL THEN NULL ELSE CURRENT_DATE + ?::int END, ?, ?)
             RETURNING id"
         user-id kind title (str "<p>body of " title "</p>") priority sm-rank status
         due due (boolean dismissed?) parent-id)))

(defn- new-user! []
  (:id (one "INSERT INTO users (username) VALUES (?) RETURNING id" (str "lq-" (System/nanoTime)))))

(defn- ids-of [user-id] (mapv :topics/id (:rows (db/get-learning-queue-ids user-id))))

(defn- seed! [u]
  (let [root (add-topic! u {:title "root" :priority 10})]
    {:root root
     :late   (add-topic! u {:title "late" :priority 90 :parent-id root})
     :first  (add-topic! u {:title "first" :priority 5 :sm-rank 2 :parent-id root})
     :second (add-topic! u {:title "second" :priority 5 :sm-rank 7 :parent-id root})
     :nulls  (add-topic! u {:title "nulls" :priority 5 :parent-id root})
     :past   (add-topic! u {:title "past" :priority 60 :due -3 :parent-id root})
     ;; excluded
     :future (add-topic! u {:title "future" :due 4 :parent-id root})
     :done   (add-topic! u {:title "done" :status "done" :parent-id root})
     :dism   (add-topic! u {:title "dismissed" :dismissed? true :parent-id root})
     :page   (add-topic! u {:title "page" :kind "page" :parent-id root})
     :gdoc   (add-topic! u {:title "gdoc" :kind "gdoc"})
     :repo   (add-topic! u {:title "repo" :kind "repo"})
     :commit (add-topic! u {:title "commit" :kind "commit"})
     :dir    (add-topic! u {:title "dir" :kind "dir"})}))

(defn- cleanup! [u]
  (jdbc/execute! db/ds ["DELETE FROM topics WHERE user_id = ?" u])
  (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" u]))

(deftest lrn-002-membership-and-order
  (when (db-up?)
    (db/setup-schema)
    (let [u (new-user!)]
      (try
        (let [{:keys [root late first second nulls past future done dism page gdoc repo commit dir]} (seed! u)
              ids (ids-of u)
              today (:d (one "SELECT CURRENT_DATE::text AS d"))
              h (fn [id] (md5-hex (str id today)))]
          (testing "membership: due, active, not dismissed, not structural (gdoc included)"
            (is (= #{root late first second nulls past} (set ids)))
            (doseq [x [future done dism page gdoc repo commit dir]]
              (is (not (contains? (set ids) x)))))
          (testing "four-key order: priority, sm_rank NULLS LAST, daily hash, id"
            (is (= [first second nulls] (take 3 ids)))
            (is (= [root past late] (drop 3 ids))))
          (testing "equal keys fall to the daily hash, then the id"
            (let [a (add-topic! u {:title "tie-a" :priority 33})
                  b (add-topic! u {:title "tie-b" :priority 33})
                  c (add-topic! u {:title "tie-c" :priority 33})
                  band (filterv #{a b c} (ids-of u))]
              (is (= (vec (sort-by (juxt h identity) [a b c])) band))))
          (testing "the same order on repeated reads on one date"
            (is (= (ids-of u) (ids-of u) (ids-of u)))))
        (finally (cleanup! u))))))

(deftest a-failed-read-is-an-error-not-an-empty-queue
  (when (db-up?)
    (testing "the id read"
      (let [r (db/get-learning-queue-ids "not-a-user-id")]
        (is (= [] (:rows r)))
        (is (string? (:error r)))))
    (testing "the display read"
      (let [r (db/get-queue-display-rows "not-a-user-id" [1 2])]
        (is (= [] (:rows r)))
        (is (string? (:error r)))))))

(deftest lean-ids-plus-window-fetch-equal-the-full-query
  (when (db-up?)
    (db/setup-schema)
    (let [u (new-user!)]
      (try
        (seed! u)
        (dotimes [i 12] (add-topic! u {:title (str "bulk " i) :priority (mod i 4)}))
        (let [full (vec (:rows (db/get-learning-queue u)))
              lean (:rows (db/get-learning-queue-ids u))
              ids (mapv :topics/id lean)
              n (count ids)]
          (is (= (mapv :topics/id full) ids))
          (is (= (mapv :topics/kind full) (mapv :topics/kind lean)))
          (doseq [[offset limit] [[0 5] [3 4] [n 3] [(- n 2) 10] [0 n] [5 0]]
                  :let [from (min offset n)
                        to (min n (+ offset limit))
                        got (db/get-queue-display-rows u (subvec ids from to))]]
            (testing (str "window " offset "+" limit)
              (is (nil? (:error got)))
              (is (= (subvec full from to) (:rows got))))))
        (finally (cleanup! u))))))

(deftest a-window-row-names-its-source-document
  ;; plans/learn-queue-source-lines.md: line 1 of a queue row is the title of
  ;; the topic at the top of the tree, at any depth.
  (when (db-up?)
    (db/setup-schema)
    (let [u (new-user!)]
      (try
        (let [root (add-topic! u {:title "The Selfish Gene" :due 4})
              child (add-topic! u {:title "Chapter 1" :parent-id root :due 4})
              grandchild (add-topic! u {:title "Replicators" :parent-id child})
              own-root (add-topic! u {:title "Lone article"})
              by-id (into {} (map (juxt :topics/id identity))
                      (:rows (db/get-queue-display-rows u [grandchild own-root])))]
          (is (= "The Selfish Gene" (:topics/source_document_title (by-id grandchild))))
          (is (= "Lone article" (:topics/source_document_title (by-id own-root))))
          (testing "the queue row puts the Source Document on line 1, but not for a Source Document"
            (let [rows (vals (learn-page/queue-window-rows nil u [grandchild own-root] 0 2))
                  row-of (into {} (map (juxt :id identity)) rows)]
              (is (= "The Selfish Gene" (:source-document (row-of grandchild))))
              (is (nil? (:source-document (row-of own-root))))
              (is (= "Lone article" (:display-title (row-of own-root)))))))
        (finally (cleanup! u))))))

(deftest topics-has-the-analyze-scale-factor
  (when (db-up?)
    (db/setup-schema)
    (db/setup-schema)
    (is (= "autovacuum_analyze_scale_factor=0.02"
           (:options (one "SELECT array_to_string(reloptions, ',') AS options
                           FROM pg_class WHERE relname = 'topics'"))))))
