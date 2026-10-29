(ns freememo.option-queries-test
  "Real database boundaries for large user-defined option domains."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [freememo.db :as db]
            [freememo.option-queries :as options]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- one [sql & params]
  (jdbc/execute-one! db/ds (into [sql] params) {:builder-fn rs/as-unqualified-lower-maps}))

(defn- user! []
  (:id (one "INSERT INTO users(username) VALUES (?) RETURNING id" (str "audit-options-" (random-uuid)))))

(use-fixtures :each
  (fn [run]
    (jdbc/with-transaction [tx db/ds {:rollback-only true}]
      (with-redefs [db/ds tx] (run)))))

(defn- seed-many! [sql user-id]
  (one "SET LOCAL session_replication_role = replica")
  (try (one sql user-id)
       (finally (one "SET LOCAL session_replication_role = origin"))))

(deftest large-domains-return-bounded-suggestions-and-reach-rare-values (let [user-id (user!) other-user (user!)
                                                                              rare (:id (one "INSERT INTO topics(user_id,title,kind) VALUES (?, 'Z rare 100% _needle', 'markdown') RETURNING id" user-id))
                                                                              playlist (:id (one "INSERT INTO topics(user_id,title,kind) VALUES (?, 'Z rare playlist', 'video-playlist') RETURNING id" user-id))] (seed-many! "INSERT INTO topics(user_id,title,kind) SELECT ?, 'Common material ' || n, 'markdown' FROM generate_series(1,30000) n" user-id)
                                                                             (seed-many! "INSERT INTO topics(user_id,title,kind) SELECT ?, 'Common playlist ' || n, 'video-playlist' FROM generate_series(1,1000) n" user-id)
                                                                             (doseq [domain [:source :curriculum :playlist]] (is (= 50 (count (:options (options/search user-id domain nil {:query ""}))))))
                                                                             (is (= [rare] (mapv :id (:options (options/search user-id :curriculum nil {:query "100% _needle"})))))
                                                                             (is (= [playlist] (mapv :id (:options (options/search user-id :playlist nil {:query "Z rare playlist"})))))
                                                                             (is (= "Z rare 100% _needle" (:label (options/selected user-id :curriculum rare))))
                                                                             (is (= [] (mapv :id (:options (options/search user-id :curriculum "pdf" {:query "_needle"})))))
                                                                             (is (= rare (:id (options/selected user-id :curriculum rare))))
                                                                             (is (= nil (options/selected other-user :source rare)))
                                                                             (is (= nil (options/selected user-id :playlist rare)))
                                                                             (is (= [] (mapv :id (:options (options/search other-user :source nil {:query ""})))))
                                                                             (is (= true (options/any-playlist? user-id)))
                                                                             (is (= false (options/any-playlist? other-user)))))

(deftest schedule-picker-preserves-supported-page-contract (let [user-id (user!) pdf (:id (one "INSERT INTO topics(user_id,title,kind) VALUES (?, 'Book with pages', 'pdf') RETURNING id" user-id))] (one "INSERT INTO topics(user_id,title,kind) VALUES (?, 'Book without pages', 'pdf')" user-id)
                                                                (one "INSERT INTO topics(user_id,title,kind,parent_id,page_number) VALUES (?, 'Page', 'page', ?, 1)" user-id pdf)
                                                                (let [request {:query "Book" :id (random-uuid)} result (options/search user-id :schedule nil request)] (is (= request (:request result))) (is (= [{:id pdf :kind "pdf" :label "Book with pages" :description "1 pages"}] (:options result))))))

(deftest historical-models-search-without-an-unbounded-select (let [user-id (user!) other-user (user!)] (one "INSERT INTO credit_transactions(user_id,kind,amount_iqd,balance_after,model) SELECT ?, 'debit', -1, 0, 'model-' || n FROM generate_series(1,1000) n" user-id)
                                                                   (is (<= (count (:options (options/model-options user-id {:query ""}))) 51))
                                                                   (is (= ["" "model-1000"] (mapv :id (:options (options/model-options user-id {:query "model-1000"})))))
                                                                   (is (= {:id "model-1000" :label "model-1000"} (options/selected-model user-id "model-1000")))
                                                                   (is (= nil (options/selected-model other-user "model-1000")))))
