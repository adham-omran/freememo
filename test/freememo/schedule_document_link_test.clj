(ns freememo.schedule-document-link-test
  "Guards the Schedule tab's link to the document a schedule is for.

   Three DB-backed properties of `db/get-study-schedule`, the single reader
   behind `schedule/forecast`, and one source-text property of the href.

   The joined title arrives under `:topics/doc_title` and NOT `:doc_title` —
   next.jdbc qualifies an aliased column by the table it was selected FROM, and
   reading the bare key returns nil that renders as 'Untitled' with nothing in
   any log. A schedule over a staged-for-deletion document reads as absent,
   which is the condition `db/schedules-where` applies to both lists; before
   that filter the detail pane rendered a forecast for a document the user had
   deleted, and gave that pane a link pointing at it.

   The href test reads both files as TEXT and never requires
   `freememo.schedule-page`. Loading that namespace pulls Electric's CLJ
   expander, which needs more stack than a test JVM has — `dev/load-app-
   namespaces!` exists for exactly that. `account_transfer_format_test` reads
   `db.clj` as text for the same class of reason.

   The DB tests SKIP rather than fail when no Postgres is reachable, matching
   `fsrs_integration_test`. Run them with the dev database up."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- new-user! []
  (let [tag (str "sched-read-" (System/nanoTime))]
    (:id (jdbc/execute-one! db/ds
           ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
            tag (str tag "@example.test")]
           {:builder-fn rs/as-unqualified-maps}))))

(defn- new-root! [uid title]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, title, kind) VALUES (?, ?, 'pdf') RETURNING id"
          uid title]
         {:builder-fn rs/as-unqualified-maps})))

(defn- new-schedule! [uid root-id]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO study_schedules (user_id, root_topic_id, target_date)
           VALUES (?, ?, CURRENT_DATE + 30) RETURNING id"
          uid root-id]
         {:builder-fn rs/as-unqualified-maps})))

(defn- stage! [uid root-id]
  ;; The column, not `stage-topic-for-deletion!`: that fn hard-deletes cards and
  ;; frees usage bytes, and the read under test looks at one column. The undo
  ;; entry is real because `topics.staged_delete_id` FKs into `undo_log`.
  (let [entry (:id (jdbc/execute-one! db/ds
                     ["INSERT INTO undo_log (user_id, action_type, entity_type,
                                             entity_refs, snapshot)
                       VALUES (?, 'delete-document', 'document', '{}'::jsonb, '{}'::jsonb)
                       RETURNING id" uid]
                     {:builder-fn rs/as-unqualified-maps}))]
    (jdbc/execute-one! db/ds
      ["UPDATE topics SET staged_delete_id = ? WHERE id = ?" entry root-id])))

(defn- drop-user! [uid]
  (jdbc/execute-one! db/ds ["DELETE FROM users WHERE id = ?" uid]))

(deftest get-study-schedule-joins-the-document-title
  (if-not (db-up?)
    (println "SKIP get-study-schedule-joins-the-document-title — no database")
    (let [uid (new-user!)]
      (try
        (let [root (new-root! uid "Compilers: principles, techniques, and tools")
              sid (new-schedule! uid root)
              row (db/get-study-schedule uid sid)]
          (testing "the title arrives under the table-qualified key"
            (is (= "Compilers: principles, techniques, and tools"
                  (:topics/doc_title row)))
            (is (nil? (:doc_title row))
              "a bare :doc_title must stay nil — reading it renders 'Untitled'"))
          (testing "the schedule's own columns survive the join"
            (is (= sid (:study_schedules/id row)))
            (is (= root (:study_schedules/root_topic_id row)))
            (is (= [] (:study_schedules/excluded_pages row))
              "schedule-row->map still unwraps the array column")))
        (finally (drop-user! uid))))))

(deftest get-study-schedule-hides-a-staged-document
  (if-not (db-up?)
    (println "SKIP get-study-schedule-hides-a-staged-document — no database")
    (let [uid (new-user!)]
      (try
        (let [root (new-root! uid "Staged book")
              sid (new-schedule! uid root)]
          (is (some? (db/get-study-schedule uid sid))
            "readable while the document is live")
          (stage! uid root)
          (is (nil? (db/get-study-schedule uid sid))
            "absent once the document is staged — the same condition both lists apply"))
        (finally (drop-user! uid))))))

(deftest get-study-schedule-refuses-another-users-schedule
  (if-not (db-up?)
    (println "SKIP get-study-schedule-refuses-another-users-schedule — no database")
    (let [owner (new-user!)
          other (new-user!)]
      (try
        (let [sid (new-schedule! owner (new-root! owner "Owned book"))]
          (is (some? (db/get-study-schedule owner sid)))
          (is (nil? (db/get-study-schedule other sid))
            "the id comes from a URL, so ownership is checked here"))
        (finally (drop-user! owner) (drop-user! other))))))

;; --- the href agrees with the router (source text, no database) -----------

(defn- source [path] (slurp (io/file path)))

(deftest document-path-segments-match-the-router
  ;; `schedule-page/document-path` renders a URL string because an href needs
  ;; one, and `main/nav->route` builds the router's route form for the same
  ;; destination. Nothing links the two, so a renamed segment breaks the href
  ;; while the intercepted click keeps working — the defect would surface only
  ;; on a modifier-click, the path least often exercised.
  (let [page (source "src/freememo/schedule_page.cljc")
        main (source "src/freememo/main.cljc")
        literal (second (re-find #"\(defn document-path \[root-topic-id\]\s*\(str \"([^\"]+)\"" page))]
    (testing "document-path is still one string literal plus the id"
      (is (some? literal) "the fn's shape changed — reread it before trusting this test"))
    (let [segments (remove str/blank? (str/split literal #"/"))]
      (is (= ["viewer" "topic"] segments)
        "the href's segments")
      (testing "the router builds the same two segments for a bare topic"
        (is (str/includes? main
              (str "(list '" (first segments) " '" (second segments) " (:topic-id nav-map))"))
          "main/nav->route no longer builds this path — the href is now wrong")))))
