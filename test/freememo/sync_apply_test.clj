(ns freememo.sync-apply-test
  "plans/electron-local-db-sync.md 2.4, 2.9: one fixture per class, with a
   two-device conflict and the expected winner, plus the apply's refusals.

   The server side runs on `db/ds`, which must carry the server schema. The
   desktop side runs on a second database, DESKTOP_DB_NAME (default
   fm_desktop), set up with FREEMEMO_PEER=desktop. Tests skip what is down."
  (:require [clojure.test :refer [deftest is testing]]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]
            [freememo.sync-apply :as apply]
            [freememo.sync-schema :as schema]))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT freememo_sync_peer()"]) true (catch Exception _ false)))

(defn- one [conn sql & params] (jdbc/execute-one! conn (into [sql] params) opts))

(defn- xmin [] (:x (one db/ds "SELECT pg_snapshot_xmin(pg_current_snapshot())::text::bigint AS x")))

(defn- account!
  "A user with a token and one id block. Returns {:user :block-start}."
  []
  (let [u (:id (one db/ds "INSERT INTO users (username) VALUES (?) RETURNING id" (str "sync-apply-" (System/nanoTime))))
        tok (:id (one db/ds "INSERT INTO desktop_tokens (user_id, token_hash) VALUES (?, ?) RETURNING id"
                   u (str "h" (System/nanoTime))))
        n (:n (one db/ds "SELECT nextval('id_block_seq') AS n"))
        start (+ schema/client-id-floor (* n schema/block-size))]
    (jdbc/execute! db/ds ["INSERT INTO id_blocks (block_start, block_end, user_id, token_id) VALUES (?, ?, ?, ?)"
                          start (+ start schema/block-size) u tok])
    {:user u :block-start start}))

(defn- topic! [u & {:keys [parent title content] :or {title "T" content "<p>c</p>"}}]
  (:id (one db/ds "INSERT INTO topics (user_id, parent_id, title, content) VALUES (?, ?, ?, ?) RETURNING id"
         u parent title content)))

(defn- card! [topic-id question]
  (:id (one db/ds "INSERT INTO flashcards (topic_id, root_topic_id, question, answer, kind)
                   VALUES (?, ?, ?, 'a', 'basic') RETURNING id" topic-id topic-id question)))

(defn- wire [table key-map]
  (apply/read-row db/ds table key-map))

(defn- one-byte-file []
  (doto (java.io.File/createTempFile "blob" ".bin") (.deleteOnExit) (spit "x")))

(defn- upload! [u base rows]
  (apply/with-apply-tx db/ds
    #(apply/apply-rows! % {:peer :server :user-id u :base-usn base
                           :blob-file (fn [row _] (when (seq (get row "b")) (one-byte-file)))}
       rows)))

(defn- sha64 [] (format "%064x" (System/nanoTime)))

(defn- at [^long ms-from-now]
  (str (.plus (java.time.OffsetDateTime/now java.time.ZoneOffset/UTC) (java.time.Duration/ofMillis ms-from-now))))

;; ── Class 2 (6.1) ──────────────────────────────────────────────────

(deftest class-2-concurrent-edit-newer-wins-and-the-loser-is-history
  (when (db-up?)
    (let [{u :user} (account!)
          t (topic! u)
          c (card! t "original")
          base (xmin)
          _ (jdbc/execute! db/ds ["UPDATE flashcards SET question = 'web edit' WHERE id = ?" c])
          stale (-> (wire :flashcards {"id" c}) (assoc-in ["r" "question"] "desktop edit"))]
      (testing "the desktop edit is newer: it wins, the web edit goes to history"
        (let [result (upload! u base [(assoc-in stale ["r" "mtime"] (at 60000))])]
          (is (= "desktop edit" (:question (one db/ds "SELECT question FROM flashcards WHERE id = ?" c))))
          (is (= ["web edit"] (mapv :question (jdbc/execute! db/ds ["SELECT question FROM card_versions
                                                                     WHERE scope_type = 'card' AND scope_id = ?" c] opts))))
          (is (contains? (:corrections result) [:flashcards {"id" c}]))))
      (testing "an older concurrent desktop edit loses, and is the history row"
        (let [base2 (xmin)
              _ (jdbc/execute! db/ds ["UPDATE flashcards SET question = 'web again' WHERE id = ?" c])
              result (upload! u base2 [(-> stale (assoc-in ["r" "question"] "old desktop")
                                         (assoc-in ["r" "mtime"] (at -60000)))])]
          (is (= "web again" (:question (one db/ds "SELECT question FROM flashcards WHERE id = ?" c))))
          (is (some #{"old desktop"} (map :question (jdbc/execute! db/ds ["SELECT question FROM card_versions
                                                                           WHERE scope_id = ?" c] opts))))
          (is (contains? (:corrections result) [:flashcards {"id" c}])))))))

(deftest class-2-a-sequential-edit-wins-over-a-slow-clock
  (when (db-up?)
    (let [{u :user} (account!)
          t (topic! u :title "before")
          base (inc (xmin))
          row (-> (wire :topics {"id" t}) (assoc-in ["r" "title"] "after")
                (assoc-in ["r" "mtime"] "2000-01-01T00:00:00Z"))]
      (upload! u (+ base 1000000) [row])
      (is (= "after" (:title (one db/ds "SELECT title FROM topics WHERE id = ?" t)))
        "the desktop had seen the stored row, so its edit wins whatever its clock says"))))

(deftest class-2-a-partial-payload-keeps-other-columns
  (when (db-up?)
    (let [{u :user} (account!)
          t (topic! u :title "keep" :content "<p>keep</p>")
          row (-> (wire :topics {"id" t}) (update "r" select-keys ["id" "user_id" "title" "mtime" "usn"])
                (assoc-in ["r" "title"] "renamed"))]
      (upload! u (+ (xmin) 1000000) [row])
      (is (= {:title "renamed" :content "<p>keep</p>"}
             (one db/ds "SELECT title, content FROM topics WHERE id = ?" t)) "2.4.3"))))

;; ── Class 1 ────────────────────────────────────────────────────────

(deftest class-1-an-existing-key-is-never-overwritten
  (when (db-up?)
    (let [{u :user} (account!)
          t (topic! u)
          rep (:id (one db/ds "INSERT INTO topic_repetitions (topic_id, user_id, event_type, interval_days_before)
                               VALUES (?, ?, 'advance', 1) RETURNING id" t u))
          row (-> (wire :topic_repetitions {"id" rep}) (assoc-in ["r" "event_type"] "touch")
                (assoc-in ["r" "mtime"] (at 60000)))]
      (upload! u (+ (xmin) 1000000) [row])
      (is (= "advance" (:event_type (one db/ds "SELECT event_type FROM topic_repetitions WHERE id = ?" rep)))))))

;; ── Class 3 ────────────────────────────────────────────────────────

(deftest class-3-the-later-review-wins-whatever-the-mtime
  (when (db-up?)
    (let [{u :user} (account!)
          t (topic! u)
          c (card! t "q")
          _ (jdbc/execute! db/ds ["INSERT INTO card_schedules (flashcard_id, ord, reps, last_review)
                                   VALUES (?, 0, 5, now())" c])
          base (xmin)
          row (-> (wire :card_schedules {"flashcard_id" c "ord" 0})
                (assoc-in ["r" "reps"] 2)
                (assoc-in ["r" "last_review"] "2001-01-01T00:00:00Z")
                (assoc-in ["r" "mtime"] (at 60000)))]
      (jdbc/execute! db/ds ["UPDATE card_schedules SET reps = 6 WHERE flashcard_id = ?" c])
      (upload! u base [row])
      (is (= 6 (:reps (one db/ds "SELECT reps FROM card_schedules WHERE flashcard_id = ?" c)))
        "the server's later review stands against a newer edit of an older review"))))

;; ── Class 4 (2.9.1) ────────────────────────────────────────────────

(deftest class-4-a-duplicate-media-row-merges-and-references-follow
  (when (db-up?)
    (let [{u :user start :block-start} (account!)
          sha (sha64)
          server-media (:id (one db/ds "INSERT INTO media (user_id, kind, sha256, mime_type, byte_size, bytes)
                                        VALUES (?, 'image', ?, 'image/png', 1, '\\x00') RETURNING id" u sha))
          desktop-media (+ start 1)
          desktop-topic (+ start 2)
          media-row {"t" "media" "k" {"id" desktop-media}
                     "r" {"id" desktop-media "user_id" u "kind" "image" "sha256" sha "mime_type" "image/png"
                          "byte_size" 1 "mtime" (at 0) "usn" -1}
                     "b" {"bytes" {}}}
          topic-row {"t" "topics" "k" {"id" desktop-topic}
                     "r" {"id" desktop-topic "user_id" u "title" "with image" "kind" "basic"
                          "content" (str "<img src=\"/api/media/" desktop-media "\">") "mtime" (at 0) "usn" -1}}
          result (upload! u (xmin) [media-row topic-row])]
      (is (= {:media {desktop-media server-media}} (:remaps result)))
      (is (= (str "<img src=\"/api/media/" server-media "\">")
             (:content (one db/ds "SELECT content FROM topics WHERE id = ?" desktop-topic)))
        "a merged media row leaves no /api/media/<loser> in any HTML"))))

;; ── Class 5 ────────────────────────────────────────────────────────

(deftest class-5-a-colliding-mask-ordinal-is-reminted
  (when (db-up?)
    (let [{u :user start :block-start} (account!)
          t (topic! u)
          m (:id (one db/ds "INSERT INTO media (user_id, kind, sha256, mime_type, byte_size, bytes)
                             VALUES (?, 'image', ?, 'image/png', 1, '\\x00') RETURNING id" u (sha64)))
          g (:id (one db/ds "INSERT INTO occlusion_groups (anki_key, image_media_id, mode, geometry, next_ordinal)
                             VALUES ('k', ?, 'hide-all', '{}'::jsonb, 2) RETURNING id" m))
          _ (one db/ds "INSERT INTO flashcards (topic_id, root_topic_id, question, answer, kind, occlusion_group_id, mask_ordinal)
                        VALUES (?, ?, 'mask 1', 'a', 'occlusion', ?, 1) RETURNING id" t t g)
          card (+ start 7)
          row {"t" "flashcards" "k" {"id" card}
               "r" {"id" card "topic_id" t "root_topic_id" t "question" "desktop mask" "answer" "a" "kind" "occlusion"
                    "occlusion_group_id" g "mask_ordinal" 1 "mtime" (at 0) "usn" -1}}
          result (upload! u (xmin) [row])]
      (is (= 1 (:applied result)))
      (is (= 2 (:mask_ordinal (one db/ds "SELECT mask_ordinal FROM flashcards WHERE id = ?" card))))
      (is (contains? (:corrections result) [:flashcards {"id" card}])))))

;; ── Document summaries ─────────────────────────────────────────────

(deftest an-apply-keeps-document-summaries-right
  ;; Under the bypass the summary triggers only record their roots, and
  ;; with-apply-tx refreshes each root once before the commit.
  (when (db-up?)
    (let [{u :user start :block-start} (account!)
          root-a (+ start 10)
          root-b (+ start 11)
          kids (mapv #(+ start 100 %) (range 300))
          topic (fn [id parent]
                  {"t" "topics" "k" {"id" id}
                   "r" (cond-> {"id" id "user_id" u "title" (str id) "kind" "basic" "mtime" (at 0) "usn" -1}
                         parent (assoc "parent_id" parent))})
          card (fn [id t]
                 {"t" "flashcards" "k" {"id" id}
                  "r" {"id" id "topic_id" t "root_topic_id" root-a "question" (str "q" id) "answer" "a"
                       "kind" "basic" "mtime" (at 0) "usn" -1}})
          summary (fn [root] (one db/ds "SELECT total_items, total_cards FROM document_summaries
                                         WHERE root_topic_id = ?" root))]
      (upload! u (xmin) (concat [(topic root-a nil) (topic root-b nil)]
                          (map #(topic % root-a) kids)
                          (map-indexed (fn [i t] (card (+ start 1000 i) t)) (take 20 (rest kids)))))
      (is (= {:total_items 301 :total_cards 20} (summary root-a)) "the root and its 300 topics")
      (is (= {:total_items 1 :total_cards 0} (summary root-b)))
      (testing "a move refreshes the old root and the new one"
        (upload! u (+ (xmin) 1000000)
          [(-> (wire :topics {"id" (first kids)})
             (assoc-in ["r" "parent_id"] root-b)
             (assoc-in ["r" "mtime"] (at 60000)))])
        (is (= 300 (:total_items (summary root-a))))
        (is (= 2 (:total_items (summary root-b)))))
      (testing "a write outside a sync apply still refreshes at once"
        (jdbc/execute! db/ds ["INSERT INTO topics (user_id, parent_id, title) VALUES (?, ?, 'web')" u root-b])
        (is (= 3 (:total_items (summary root-b))))))))

;; ── Refusals ───────────────────────────────────────────────────────

(deftest an-edit-uploaded-after-a-delete-is-refused
  (when (db-up?)
    (let [{u :user} (account!)
          t (topic! u)
          row (wire :topics {"id" t})
          base (xmin)]
      (jdbc/execute! db/ds ["DELETE FROM topics WHERE id = ?" t])
      (is (= ["deleted-elsewhere"] (mapv #(get % "reason") (:rejected (upload! u base [row]))))
        "2.4.2"))))

(deftest a-card-under-a-deleted-topic-is-returned
  (when (db-up?)
    (let [{u :user start :block-start} (account!)
          gone (topic! u)
          _ (jdbc/execute! db/ds ["DELETE FROM topics WHERE id = ?" gone])
          card (+ start 3)
          result (upload! u (+ (xmin) 1000000)
                   [{"t" "flashcards" "k" {"id" card}
                     "r" {"id" card "topic_id" gone "root_topic_id" gone "question" "orphan" "answer" "a"
                          "kind" "basic" "mtime" (at 0) "usn" -1}}])]
      (is (= ["parent-deleted"] (mapv #(get % "reason") (:rejected result))) "2.4.4")
      (is (nil? (one db/ds "SELECT id FROM flashcards WHERE id = ?" card)) "nothing half-inserted"))))

(deftest another-accounts-rows-are-refused
  (when (db-up?)
    (let [{a :user start :block-start} (account!)
          {b :user} (account!)
          b-topic (topic! b :title "bob's")]
      (testing "editing it"
        (let [row (-> (wire :topics {"id" b-topic}) (assoc-in ["r" "user_id"] a) (assoc-in ["r" "title"] "mine"))]
          (is (= "not-yours" (get (first (:rejected (upload! a (+ (xmin) 1000000) [row]))) "reason")))
          (is (= "bob's" (:title (one db/ds "SELECT title FROM topics WHERE id = ?" b-topic))))))
      (testing "hanging a new row under it"
        (let [id (+ start 4)
              r (upload! a (xmin) [{"t" "topics" "k" {"id" id}
                                    "r" {"id" id "user_id" a "parent_id" b-topic "title" "x" "kind" "basic"
                                         "mtime" (at 0) "usn" -1}}])]
          (is (= "not-yours:parent_id" (get (first (:rejected r)) "reason")))))
      (testing "an id outside the account's blocks"
        (let [id (+ schema/client-id-floor 999999999999)
              r (upload! a (xmin) [{"t" "topics" "k" {"id" id}
                                    "r" {"id" id "user_id" a "title" "x" "kind" "basic" "mtime" (at 0) "usn" -1}}])]
          (is (= "unknown-id" (get (first (:rejected r)) "reason"))))))))

(deftest an-unknown-table-is-refused
  (when (db-up?)
    (let [{u :user} (account!)]
      (is (= "invalid:table" (get (first (:rejected (upload! u (xmin) [{"t" "users" "k" {"id" 1} "r" {"id" 1}}])))
                               "reason"))))))

;; ── Reading rows ───────────────────────────────────────────────────

(deftest a-row-reads-without-encoding-its-blob
  ;; to_jsonb renders bytea as hex, two characters per byte, and a jsonb string
  ;; holds at most 268435455 bytes. 135 MB of file_data is over that limit.
  (when (db-up?)
    (let [{u :user} (account!)
          t (topic! u)
          f (:id (one db/ds "INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
                             SELECT ?, decode(repeat('ab', 135000000), 'hex'), 135000000, 'application/pdf'
                             RETURNING id" t))]
      (try
        (let [row (wire :topic_files {"id" f})]
          (is (= {"file_data" {}} (get row "b")) "the blob is flagged, not sent")
          (is (not (contains? (get row "r") "file_data")))
          (is (= "application/pdf" (get-in row ["r" "mime_type"])))
          (testing "an upload that matches the stored row reads it too"
            (upload! u (+ (xmin) 1000000)
              [(-> row (assoc-in ["r" "mime_type"] "application/x-pdf")
                 (assoc "b" {"file_data" {"sha256" (sha64)}}))])
            (is (= "application/x-pdf" (:mime_type (one db/ds "SELECT mime_type FROM topic_files WHERE id = ?" f))))))
        (finally (jdbc/execute! db/ds ["DELETE FROM topic_files WHERE id = ?" f]))))))

(deftest a-wire-row-holds-every-sent-column-and-no-other
  (when (db-up?)
    (let [{u :user} (account!)
          t (topic! u :content "<p>c</p>")
          row (wire :topics {"id" t})]
      (is (= (:sent (apply/table-meta db/ds :topics)) (set (keys (get row "r")))))
      (is (= "<p>c</p>" (get-in row ["r" "content"]))))))

;; ── Desktop side (D8, 6.8) ─────────────────────────────────────────

(def ^:private desktop-ds
  (delay (jdbc/get-datasource {:dbtype "postgresql" :host (or (System/getenv "DB_HOST") "localhost")
                               :port (parse-long (or (System/getenv "DB_PORT") "5432"))
                               :dbname (or (System/getenv "DESKTOP_DB_NAME") "fm_desktop")
                               :user "cardmaker" :password (or (System/getenv "DB_PASSWORD") "dev")})))

(defn- desktop-up? []
  (try (= "desktop" (:p (one @desktop-ds "SELECT freememo_sync_peer() AS p"))) (catch Exception _ false)))

(deftest a-server-delete-moves-pending-local-work-to-the-conflict-report
  (when (desktop-up?)
    (let [d @desktop-ds
          u (:id (one d "INSERT INTO users (username) VALUES (?) RETURNING id" (str "desk-" (System/nanoTime))))
          start (+ schema/client-id-floor (* 1000 schema/block-size) (rand-int 1000000))
          _ (jdbc/execute! d ["INSERT INTO local_id_blocks VALUES (?, ?, ?) ON CONFLICT DO NOTHING"
                              start (+ start 1000) start])
          parent (:id (one d "INSERT INTO topics (user_id, title) VALUES (?, 'synced parent') RETURNING id" u))
          ;; Synced: a server usn, which only an apply may write on a desktop.
          _ (apply/with-apply-tx d (fn [tx] (jdbc/execute! tx ["UPDATE topics SET usn = 5 WHERE id = ?" parent])))
          child (:id (one d "INSERT INTO topics (user_id, parent_id, title) VALUES (?, ?, 'made offline') RETURNING id"
                       u parent))
          before (:n (one d "SELECT count(*) AS n FROM sync_conflicts"))]
      (apply/with-apply-tx d #(apply/apply-graves! % {:peer :desktop :user-id u}
                                [{"t" "topics" "k" {"id" parent}}]))
      (is (nil? (one d "SELECT id FROM topics WHERE id = ?" child)) "the delete wins")
      (is (= (inc before) (:n (one d "SELECT count(*) AS n FROM sync_conflicts"))))
      (is (= "made offline" (get (json/parse-string (str (:row_data (one d "SELECT row_data::text AS row_data FROM sync_conflicts
                                                                             ORDER BY id DESC LIMIT 1"))))
                              "title"))
        "6.8: the offline work is in the conflict report"))))

(deftest the-bypass-keeps-the-derived-triggers
  (when (desktop-up?)
    (let [d @desktop-ds
          u (:id (one d "INSERT INTO users (username) VALUES (?) RETURNING id" (str "desk-" (System/nanoTime))))
          id (+ schema/client-id-floor 5000000000 (rand-int 1000000))]
      (apply/with-apply-tx d
        #(apply/apply-rows! % {:peer :desktop :user-id u :blob-file (constantly nil)}
           [{"t" "topics" "k" {"id" id}
             "r" {"id" id "user_id" u "title" "from server" "kind" "pdf" "usn" 77 "mtime" "2026-01-01T00:00:00Z"}}]))
      (is (= {:usn 77} (one d "SELECT usn FROM topics WHERE id = ?" id)) "1.5: an applied row keeps the server's usn")
      (is (one d "SELECT root_topic_id FROM document_summaries WHERE root_topic_id = ?" id)
        "1.5.2: document_summaries still updates under the bypass"))))

(defn- desktop-id-block!
  "One unused id block on the desktop, as `ensure-id-blocks!` leaves a sync."
  [d]
  (let [start (+ schema/client-id-floor 8000000000 (* 1000 (rand-int 1000000)))]
    (jdbc/execute! d ["INSERT INTO local_id_blocks (block_start, block_end, next_id)
                       VALUES (?, ?, ?) ON CONFLICT DO NOTHING" start (+ start 1000) start])))

(deftest a-newer-pending-local-edit-stays-pending
  (when (desktop-up?)
    (let [d @desktop-ds
          u (:id (one d "INSERT INTO users (username) VALUES (?) RETURNING id" (str "desk-" (System/nanoTime))))
          id (+ schema/client-id-floor 6000000000 (rand-int 1000000))
          row (fn [title mtime] {"t" "topics" "k" {"id" id}
                                 "r" {"id" id "user_id" u "title" title "kind" "basic" "usn" 40 "mtime" mtime}})]
      ;; The loser's history row takes a local id.
      (desktop-id-block! d)
      (apply/with-apply-tx d #(apply/apply-rows! % {:peer :desktop :user-id u :blob-file (constantly nil)}
                                [(row "server v1" "2026-01-01T00:00:00Z")]))
      ;; A local edit, after the server's version.
      (jdbc/execute! d ["UPDATE topics SET title = 'local edit' WHERE id = ?" id])
      ;; An older server version arrives (it was written before the local edit).
      (apply/with-apply-tx d #(apply/apply-rows! % {:peer :desktop :user-id u :blob-file (constantly nil)}
                                [(row "server v2" "2026-01-02T00:00:00Z")]))
      (is (= {:title "local edit" :usn -1} (one d "SELECT title, usn FROM topics WHERE id = ?" id))
        "the local edit keeps its columns and stays pending, so the next sync uploads it")
      (is (= ["server v2"] (mapv :title (jdbc/execute! d ["SELECT title FROM topic_versions WHERE topic_id = ?" id] opts)))
        "D4: the older server version is the history row"))))

(deftest a-failed-history-write-is-reported-as-itself
  ;; The history row is written inside the row's savepoint. A failure there
  ;; once rolled back to the released savepoint, and the driver's
  ;; "Cannot reference a savepoint after it has been released" hid it.
  (when (desktop-up?)
    (let [d @desktop-ds
          u (:id (one d "INSERT INTO users (username) VALUES (?) RETURNING id" (str "desk-" (System/nanoTime))))
          id (+ schema/client-id-floor 6000000000 (rand-int 1000000))
          row (fn [title mtime] {"t" "topics" "k" {"id" id}
                                 "r" {"id" id "user_id" u "title" title "kind" "basic" "usn" 40 "mtime" mtime}})
          apply! #(apply/with-apply-tx d (fn [tx] (apply/apply-rows! tx {:peer :desktop :user-id u
                                                                          :blob-file (constantly nil)} [%])))]
      (apply! (row "server v1" "2026-01-01T00:00:00Z"))
      (jdbc/execute! d ["UPDATE topics SET title = 'local edit' WHERE id = ?" id])
      (let [result (with-redefs [apply/save-loser!
                                 (fn [& _] (throw (org.postgresql.util.PSQLException.
                                                    "history write failed" org.postgresql.util.PSQLState/UNKNOWN_STATE)))]
                     (apply! (row "server v2" "2026-01-02T00:00:00Z")))]
        (is (zero? (:applied result)))
        (is (re-find #"history write failed" (get (first (:rejected result)) "reason" ""))
          "the rejection names the real error")
        (is (= {:title "local edit" :usn -1} (one d "SELECT title, usn FROM topics WHERE id = ?" id))
          "the row's write rolled back with its history")))))
