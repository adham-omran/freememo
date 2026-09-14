(ns freememo.merged-pages-db-test
  "The database half of merged page reading (plans/merged-page-reading.md
   items 1.1, 3.2, 3.3, 3.6, 4.1 and 4.2).

   NEEDS POSTGRESQL. The `:each` fixture below writes rows. With the database
   port refusing connections, that fixture throws `ConnectException`.
   `clojure.test` does not catch it, so `run-tests` for this namespace throws.

   It creates its own PDF root and page rows, and deletes them afterwards. It
   touches no existing document.

   The pure boundary rules live in `freememo.page-merge-test`, which needs no
   database. This suite covers only what the database can contradict: that a
   split writes the changed page and no other, that no row is inserted or
   deleted, and that scheduling columns survive."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [freememo.db :as db]
            [freememo.page-merge :as pm]))

(def ^:private !fixture (atom nil))

(defn- make-document!
  "A PDF root with three page topics. Returns {:root <id> :pages {n id}}."
  []
  (let [uid (:users/id (jdbc/execute-one! db/ds
                         ["SELECT id FROM users ORDER BY id LIMIT 1"]))
        root (:topics/id (jdbc/execute-one! db/ds
                           ["INSERT INTO topics (user_id, kind, title)
                             VALUES (?, 'pdf', 'merged-pages-db-test') RETURNING id" uid]))
        pages (into {}
                (for [n [1 2 3]]
                  [n (:topics/id
                       (jdbc/execute-one! db/ds
                         ["INSERT INTO topics (user_id, parent_id, kind, title, content, page_number, status, priority)
                           VALUES (?, ?, 'page', ?, ?, ?, 'active', 50) RETURNING id"
                          uid root (str "Page " n) (str "<p>body " n "</p>") n]))]))]
    {:root root :pages pages :user uid}))

(defn- drop-document!
  "Removes the fixture. The topics subtree cascades from the root; the undo
   entry a staging test may have created does not, so it goes by user and
   action type. Both are scoped to rows this suite inserted."
  [{:keys [root user]}]
  (when root (jdbc/execute! db/ds ["DELETE FROM topics WHERE id = ?" root]))
  (when user
    (jdbc/execute! db/ds
      ["DELETE FROM undo_log WHERE user_id = ? AND entity_type = 'document'
         AND entity_refs = '{}'::jsonb AND snapshot = '{}'::jsonb" user])))

(use-fixtures :each
  (fn [t]
    (reset! !fixture (make-document!))
    (try (t) (finally (drop-document! @!fixture) (reset! !fixture nil)))))

(defn- page-row
  "`topics` carries no `updated_at`, so the write witness is Postgres's own
   `xmin` system column: it changes on every UPDATE of the row and on no read."
  [n]
  (jdbc/execute-one! db/ds
    ["SELECT content, content_text, status, page_number, priority, xmin::text AS xmin
        FROM topics WHERE id = ?" (get-in @!fixture [:pages n])]))

(defn- write-witness []
  (mapv #(:xmin (page-row %)) [1 2 3]))

(defn- page-count []
  (:n (jdbc/execute-one! db/ds
        ["SELECT count(*) AS n FROM topics WHERE parent_id = ? AND kind = 'page'"
         (:root @!fixture)])))

;; ── Merge ───────────────────────────────────────────────────────────────────

(deftest merged-document-carries-a-marker-per-page-and-the-page-list
  (let [{:keys [html page-numbers]} (db/merged-page-document (:root @!fixture))]
    (is (= [1 2 3] page-numbers))
    (doseq [n [1 2 3]]
      (is (str/includes? html (pm/marker-id n))))
    (is (str/includes? html "<p>body 2</p>"))))

(deftest merge-excludes-a-staged-deleted-page
  (testing "the merged document and the toolbar's page count must agree —
            count-pages applies the same exclusion"
    ;; `topics.staged_delete_id` references `undo_log(id)`, so staging needs a
    ;; real undo entry. Its subtree cascade deletes it with the root.
    (let [sd (:undo_log/id
               (jdbc/execute-one! db/ds
                 ["INSERT INTO undo_log (user_id, action_type, entity_type, entity_refs, snapshot)
                   VALUES (?, 'delete-document', 'document', '{}'::jsonb, '{}'::jsonb) RETURNING id"
                  (:user @!fixture)]))]
      (jdbc/execute! db/ds ["UPDATE topics SET staged_delete_id = ? WHERE id = ?"
                            sd (get-in @!fixture [:pages 2])])
      (let [{:keys [page-numbers html]} (db/merged-page-document (:root @!fixture))]
        (is (= [1 3] page-numbers))
        (is (not (str/includes? html (pm/marker-id 2))))))))

;; ── Split ───────────────────────────────────────────────────────────────────

(deftest an-untouched-round-trip-writes-nothing
  (testing "item 3.3 — the auto-save fires on every differing edit, so an
            unchanged page must not be written"
    (let [{:keys [html page-numbers]} (db/merged-page-document (:root @!fixture))
          before (write-witness)
          r (db/save-merged-pages! (:root @!fixture) html page-numbers)]
      (is (= 0 (:written r)))
      (is (= before (write-witness)) "no row's xmin moved"))))

(deftest one-page-edited-writes-exactly-that-page
  (let [{:keys [html page-numbers]} (db/merged-page-document (:root @!fixture))
        edited (str/replace html "<p>body 2</p>" "<p>body 2 edited</p>")
        r (db/save-merged-pages! (:root @!fixture) edited page-numbers)]
    (is (= 1 (:written r)))
    (is (= [2] (:pages r)))
    (is (str/includes? (:topics/content (page-row 2)) "body 2 edited"))
    (testing "its neighbours are byte-identical"
      (is (= "<p>body 1</p>" (:topics/content (page-row 1))))
      (is (= "<p>body 3</p>" (:topics/content (page-row 3)))))))

(deftest no-marker-reaches-a-stored-row
  (testing "item 3.1 — a marker in a stored row would reach content_text, the
            card-generation prompt, a search snippet and KG extraction"
    (let [{:keys [html page-numbers]} (db/merged-page-document (:root @!fixture))
          edited (str/replace html "<p>body 1</p>" "<p>body 1 edited</p>")]
      (db/save-merged-pages! (:root @!fixture) edited page-numbers)
      (doseq [n [1 2 3]]
        (let [{:topics/keys [content content_text]} (page-row n)]
          (testing (str "page " n)
            (is (not (str/includes? (or content "") pm/marker-id-prefix)))
            (is (not (str/includes? (or content_text "") "Page ")))))))))

(deftest a-lost-marker-absorbs-upward-and-empties-the-later-row
  (testing "items 4.1 and 4.2"
    (let [{:keys [html page-numbers]} (db/merged-page-document (:root @!fixture))
          edited (str/replace html (pm/marker-html 2) "")
          r (db/save-merged-pages! (:root @!fixture) edited page-numbers)]
      (is (= 2 (:written r)) "page 1 gains, page 2 empties")
      (is (str/includes? (:topics/content (page-row 1)) "body 1"))
      (is (str/includes? (:topics/content (page-row 1)) "body 2"))
      (is (= "" (:topics/content (page-row 2))))
      (testing "the row still exists — deleting it would orphan its cards"
        (is (= 3 (page-count)))
        (is (= 2 (:topics/page_number (page-row 2))))))))

(deftest a-split-inserts-and-deletes-no-row
  (let [{:keys [html page-numbers]} (db/merged-page-document (:root @!fixture))
        edited (str html (pm/marker-html 9) "<p>a page that does not exist</p>")]
    (db/save-merged-pages! (:root @!fixture) edited page-numbers)
    (is (= 3 (page-count)))
    (testing "the unknown page's text stays with the page that was open"
      (is (str/includes? (:topics/content (page-row 3)) "does not exist")))))

(deftest a-split-leaves-every-scheduling-column-alone
  (testing "item 3.6 — Done marks, the N/M figure and a schedule's progress"
    (jdbc/execute! db/ds ["UPDATE topics SET status = 'done', priority = 12.5 WHERE id = ?"
                          (get-in @!fixture [:pages 2])])
    (let [{:keys [html page-numbers]} (db/merged-page-document (:root @!fixture))
          edited (str/replace html "<p>body 2</p>" "<p>body 2 edited</p>")]
      (db/save-merged-pages! (:root @!fixture) edited page-numbers)
      (let [row (page-row 2)]
        (is (= "done" (:topics/status row)))
        (is (= 2 (:topics/page_number row)))
        (is (= 12.5 (:topics/priority row)))))))

(deftest a-page-absent-from-the-live-rows-is-not-written
  (testing "a page deleted in another tab must stay out of the write set"
    (let [{:keys [html]} (db/merged-page-document (:root @!fixture))
          r (db/save-merged-pages! (:root @!fixture) html [1 2 3 4 5])]
      (is (= 0 (:written r)))
      (is (= 3 (page-count))))))
