(ns freememo.merged-window-db-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [freememo.db :as db]
            [freememo.merged-window-db :as window-db]
            [freememo.merged-window :as window]))

(defn with-document [run]
  (let [uid (:users/id (jdbc/execute-one! db/ds
                        ["INSERT INTO users(username) VALUES (?) RETURNING id"
                         (str "focus-window-" (random-uuid))]))]
    (try
      (let [root (:topics/id (jdbc/execute-one! db/ds
                              ["INSERT INTO topics(user_id,kind,title) VALUES (?,'pdf','Focus window') RETURNING id" uid]))]
        (jdbc/with-transaction [tx db/ds]
          (jdbc/execute! tx ["SET LOCAL session_replication_role=replica"])
          (jdbc/execute! tx
            ["INSERT INTO topics(user_id,parent_id,kind,title,page_number,content,status)
              SELECT ?,?,'page','Page ' || n,n,'<p>Body ' || n || ' ' || repeat('x',3000) || '</p>','active'
              FROM generate_series(1,883) n" uid root]))
        (run uid root))
      (finally (jdbc/execute! db/ds ["DELETE FROM users WHERE id=?" uid])))))

(deftest an-883-page-document-reads-only-its-requested-range
  (with-document
    (fn [uid root]
      (let [first-page (window-db/read-window uid root (window/page-range 1 883))
            middle (window-db/read-window uid root (window/page-range 440 883))
            last-page (window-db/read-window uid root (window/page-range 883 883))]
        (is (= (vec (range 1 21)) (:page-numbers first-page)))
        (is (= (vec (range 421 441)) (:page-numbers middle)))
        (is (= [881 882 883] (:page-numbers last-page)))
        (is (< (count (:html first-page)) 65000))
        (is (not (str/includes? (:html first-page) "fm-page-21")))
        (is (empty? (:page-numbers (window-db/read-window -1 root [1 20]))))))))

(deftest an-old-range-save-cannot-empty-the-next-range
  (with-document
    (fn [uid root]
      (let [body (window-db/read-window uid root [1 20])
            next-body (window-db/read-window uid root [21 40])
            dirty (window/edit-entry
                    {:save-scope {:user-id uid :topic-id root :editor-doc-id root
                                  :pdf-root-id root :merged? true :page-numbers (:page-numbers body)}}
                    (str/replace (:html body) "Body 20 " "Body 20 edited ") "edit-1")
            saved (db/save-merged-pages! root (:html dirty) (get-in dirty [:save-scope :page-numbers]))
            after (window-db/read-window uid root [21 40])]
        (is (= [20] (:pages saved)))
        (is (= next-body after))
        (is (= 883 (:count (jdbc/execute-one! db/ds
                              ["SELECT count(*) AS count FROM topics WHERE parent_id=?" root]))))))))
