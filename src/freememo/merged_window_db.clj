(ns freememo.merged-window-db
  "Bounded page-body reads for Focus reader."
  (:require [freememo.db :as db]
            [freememo.merged-window :as window]
            [freememo.page-merge :as page-merge]
            [next.jdbc :as jdbc]))

(defn page-metadata [root-id]
  (jdbc/execute! db/ds
    ["SELECT id,page_number,status FROM topics
      WHERE parent_id=? AND kind='page' AND staged_delete_id IS NULL
      ORDER BY page_number" root-id]))

(defn read-window [user-id root-id [start end :as page-range]]
  (let [rows (jdbc/execute! db/ds
               ["SELECT p.id,p.page_number,p.content FROM topics p JOIN topics r ON r.id=p.parent_id
                 WHERE r.user_id=? AND p.parent_id=? AND p.kind='page' AND p.staged_delete_id IS NULL AND r.staged_delete_id IS NULL
                   AND p.page_number BETWEEN ? AND ?
                 ORDER BY p.page_number LIMIT ?" user-id root-id start end window/page-limit])]
    {:html (page-merge/merge-pages rows)
     :page-numbers (mapv :topics/page_number rows)
     :root-id root-id :range page-range}))
