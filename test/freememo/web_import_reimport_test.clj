(ns freememo.web-import-reimport-test
  "Regression checks for URL imports during the staged-delete window.

   Needs PostgreSQL."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.db :as db]
            [freememo.web-import :as web-import]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})

(defn- create-user! []
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
          (str "web-reimport-test-" (random-uuid))
          (str (random-uuid) "@example.test")]
         opts)))

(deftest a-staged-deleted-url-import-is-not-an-existing-document
  (let [user-id (create-user!)
        url (str "https://example.test/" (random-uuid))]
    (try
      (let [topic-id (db/create-web-topic! user-id "Deleted article" "<p>Old</p>"
                       {:url url :source-type "webpage"})]
        (is (= topic-id (:topic-id
                          (web-import/find-existing-topic-for-url* user-id url))))
        (is (some? (db/stage-topic-for-deletion! user-id topic-id)))
        (testing "the import must not navigate to the hidden topic"
          (is (nil? (web-import/find-existing-topic-for-url* user-id url))))
        (testing "content and PDF duplicate checks also ignore hidden topics"
          (let [source-id (:sources/id (db/find-source-by-url user-id url))
                pdf-id (:id (jdbc/execute-one! db/ds
                              ["INSERT INTO topics (user_id, kind, title, source_id)
                                VALUES (?, 'pdf', 'Deleted screenshot', ?) RETURNING id"
                               user-id source-id]
                              opts))]
            (is (nil? (db/find-topic-by-content-digest user-id source-id "Old")))
            (is (= pdf-id (:topics/id (db/find-pdf-topic-by-source-id user-id source-id))))
            (is (some? (db/stage-topic-for-deletion! user-id pdf-id)))
            (is (nil? (db/find-pdf-topic-by-source-id user-id source-id)))))
        (testing "the replacement gets a numbered title and remains visible"
          (let [replacement-id (db/create-web-topic! user-id "Deleted article" "<p>New</p>"
                                 {:url url :source-type "webpage"})]
            (is (= "Deleted article (2)" (:topics/title (db/get-topic replacement-id))))
            (is (= replacement-id
                  (:topic-id (web-import/find-existing-topic-for-url* user-id url)))))))
      (finally
        (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" user-id])))))
