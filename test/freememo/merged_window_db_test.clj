(ns freememo.merged-window-db-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [freememo.db :as db]
            [freememo.editor-pane :as editor-pane]
            [freememo.topic-state :as topic-state]
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
            saved (editor-pane/save-editor-html!*
                    (merge (:save-scope dirty) {:html (:html dirty)}))
            after (window-db/read-window uid root [21 40])]
        (is (= {:success true :written 1} saved))
        (is (= next-body after))
        (is (= 883 (:count (jdbc/execute-one! db/ds
                              ["SELECT count(*) AS count FROM topics WHERE parent_id=?" root]))))))))

(deftest a-captured-save-cannot-write-a-foreign-document
  (with-document
    (fn [uid root]
      (let [body (window-db/read-window uid root [1 20])
            result (editor-pane/save-editor-html!*
                     {:user-id -1 :merged? true :pdf-root-id root
                      :page-numbers (:page-numbers body)
                      :html (str/replace (:html body) "Body 1 " "Foreign edit ")})]
        (is (false? (:success result)))
        (is (= body (window-db/read-window uid root [1 20])))))))

(deftest page-status-metadata-does-not-read-the-883-page-bodies
  (with-document
    (fn [_ root]
      (let [statements (atom [])
            execute! jdbc/execute!
            rows (with-redefs [jdbc/execute!
                               (fn [connection statement & options]
                                 (swap! statements conj (first statement))
                                 (apply execute! connection statement options))]
                   (window-db/page-metadata root))
            info (topic-state/get-page-info* 0 0 root 440)]
        (is (= 883 (count rows)))
        (is (= (vec (range 1 884)) (mapv :topics/page_number rows)))
        (is (every? #(= #{:topics/id :topics/page_number :topics/status} (set (keys %))) rows))
        (is (every? #(= "active" (:topics/status %)) rows))
        (is (every? #(not (re-find #"(?i)SELECT\s+\*|\bcontent(?:_text)?\b" %)) @statements))
        (is (= {:done 0 :total 883 :current-done? false}
              (select-keys info [:done :total :current-done?])))
        (is (= (:topics/id (nth rows 439)) (:topic-id info)))
        (is (str/ends-with? (:remaining-tooltip info) "and 863 more"))))))
