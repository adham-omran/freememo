(ns freememo.flashcard-dedupe-test
  "DB-backed checks for the duplicate-flashcard repair that runs before the
   uniqueness indexes are built (plans/flashcard-dedupe-index-repair.md).

   Duplicates cannot exist while the indexes do, so every test runs inside ONE
   rollback-only transaction: it drops both indexes, seeds duplicates, runs the
   repair on that transaction, rebuilds the indexes, and rolls all of it back.
   The DROP INDEX holds an ACCESS EXCLUSIVE lock on flashcards until the
   rollback, so a dev server on the same database waits for about a second.

   The tests SKIP (not fail) when no database is reachable, matching
   `anki-settings-snapshot-test`. Run one against a scratch database with
   DB_PORT, as the CLAUDE.md Tests section shows."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]))

(def ^:private plain {:builder-fn rs/as-unqualified-maps})

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(def ^:private dedupe! @#'db/dedupe-flashcards-for-unique-indexes!)
(def ^:private index-ddl @#'db/flashcard-unique-index-ddl)

(defn- one [tx sql & params]
  (jdbc/execute-one! tx (into [sql] params) plain))

(defn- rows [tx sql & params]
  (jdbc/execute! tx (into [sql] params) plain))

(defn- new-user! [tx]
  (let [tag (str "dedupe-" (System/nanoTime))]
    (:id (one tx "INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
           tag (str tag "@example.test")))))

(defn- new-topic! [tx uid]
  (:id (one tx "INSERT INTO topics (user_id, title, kind) VALUES (?, 'dedupe doc', 'basic')
                RETURNING id" uid)))

(defn- card!
  "Insert one flashcard. `attrs` holds any flashcards columns."
  [tx attrs]
  (:id (apply one tx
         (str "INSERT INTO flashcards (" (str/join ", " (map name (keys attrs)))
           ") VALUES (" (str/join ", " (repeat (count attrs) "?")) ") RETURNING id")
         (vals attrs))))

(defn- basic! [tx topic q & {:as more}]
  (card! tx (merge {:topic_id topic :root_topic_id topic :kind "basic"
                    :question q :answer "a"} more)))

(defn- cloze! [tx topic c & {:as more}]
  (card! tx (merge {:topic_id topic :root_topic_id topic :kind "cloze"
                    :cloze c :answer ""} more)))

(defn- schedule! [tx card ord reps]
  (one tx "INSERT INTO card_schedules (flashcard_id, ord, reps, last_review)
           VALUES (?, ?, ?, now() - make_interval(days => ?::int))" card ord reps reps))

(defn- review! [tx uid card]
  (one tx "INSERT INTO kg_reviews (user_id, flashcard_id, ord, rating, state_before,
                                   state_after, reps_before)
           VALUES (?, ?, 0, 3, 1, 2, 0)" uid card))

(defn- anki-state! [tx uid note card-id reps]
  (one tx "INSERT INTO anki_card_states (user_id, anki_card_id, anki_note_id, reps,
                                         config_fingerprint)
           VALUES (?, ?, ?, ?, 'fp')" uid card-id note reps))

(defn- version! [tx root card]
  (one tx "INSERT INTO card_versions (root_topic_id, scope_type, scope_id, kind, question, answer)
           VALUES (?, 'card', ?, 'basic', 'old q', 'old a')" root card))

(defn- live? [tx id]
  (some? (one tx "SELECT 1 FROM flashcards WHERE id = ?" id)))

(defn- with-duplicates-allowed
  "Run `f` on a rollback-only transaction in which the uniqueness indexes do
   not exist, so the test can seed duplicates."
  [f]
  (jdbc/with-transaction [tx db/ds {:rollback-only true}]
    (doseq [[index-name _] index-ddl]
      (jdbc/execute! tx [(str "DROP INDEX IF EXISTS " index-name)]))
    (f tx)))

(defn- build-indexes! [tx]
  (doseq [[_ ddl] index-ddl] (jdbc/execute! tx [ddl])))

(deftest ^:db the-survivor-is-the-row-with-the-most-that-cannot-move
  (if-not (db-up?)
    (println "SKIP flashcard-dedupe: no Postgres reachable.")
    (with-duplicates-allowed
      (fn [tx]
        (let [uid (new-user! tx)
              topic (new-topic! tx uid)
              ;; Oldest, but it holds nothing.
              plain-card (basic! tx topic "Q1" :answer "first answer")
              ;; Holds reviews and history, but no Anki link.
              reviewed (basic! tx topic "Q1" :answer "second answer")
              ;; The Anki link wins over everything else.
              linked (basic! tx topic "Q1" :anki_note_id 111)]
          (schedule! tx reviewed 0 5)
          (review! tx uid reviewed)
          (review! tx uid reviewed)
          (version! tx topic reviewed)
          (let [result (dedupe! tx)]
            (testing "one survivor per key, and it is the linked row"
              (is (= 2 (:deleted result)))
              (is (live? tx linked))
              (is (not (live? tx plain-card)))
              (is (not (live? tx reviewed))))
            (testing "the loser's FSRS schedule and review log move to the survivor"
              (is (= [{:ord 0 :reps 5}]
                    (rows tx "SELECT ord, reps FROM card_schedules WHERE flashcard_id = ?" linked)))
              (is (= 2 (:n (one tx "SELECT count(*) AS n FROM kg_reviews WHERE flashcard_id = ?"
                             linked)))))
            (testing "each loser's different text is kept as a version of the survivor,
                      beside the loser's own history"
              (is (= #{"first answer" "second answer" "old a"}
                    (set (map :answer
                           (rows tx "SELECT answer FROM card_versions
                                     WHERE scope_type = 'card' AND scope_id = ?" linked)))))
              (is (zero? (:n (one tx "SELECT count(*) AS n FROM card_versions
                                      WHERE scope_type = 'card' AND scope_id IN (?, ?)"
                               plain-card reviewed)))))
            (testing "no tombstone: the only note is the survivor's"
              (is (zero? (:tombstones result))))
            (testing "both indexes build afterwards"
              (build-indexes! tx)
              (is (= 2 (:n (one tx "SELECT count(*) AS n FROM pg_indexes
                                    WHERE tablename = 'flashcards'
                                      AND indexname LIKE 'idx_flashcards_unique_%_digest'")))))))))))

(deftest ^:db schedules-merge-per-ord-and-cloze-groups-are-repaired
  (if-not (db-up?)
    (println "SKIP flashcard-dedupe: no Postgres reachable.")
    (with-duplicates-allowed
      (fn [tx]
        (let [uid (new-user! tx)
              topic (new-topic! tx uid)
              x (cloze! tx topic "{{c1::a}} {{c2::b}}")
              y (cloze! tx topic "{{c1::a}} {{c2::b}}")]
          ;; x reviewed ord 1 more, y reviewed ord 2 more. y has more in total,
          ;; so y survives, and ord 1's schedule comes from x.
          (schedule! tx x 1 3)
          (schedule! tx x 2 0)
          (schedule! tx y 1 1)
          (schedule! tx y 2 4)
          (let [result (dedupe! tx)]
            (is (= 1 (:deleted result)))
            (is (live? tx y))
            (is (= [{:ord 1 :reps 3} {:ord 2 :reps 4}]
                  (rows tx "SELECT ord, reps FROM card_schedules WHERE flashcard_id = ?
                            ORDER BY ord" y)))
            (is (= 2 (:schedules-dropped result)))
            (is (= 1 (:schedules-moved result)))
            (testing "identical content writes no version row"
              (is (zero? (:versions-written result))))))))))

(deftest ^:db a-loser-with-its-own-anki-note-leaves-a-tombstone
  (if-not (db-up?)
    (println "SKIP flashcard-dedupe: no Postgres reachable.")
    (with-duplicates-allowed
      (fn [tx]
        (let [uid (new-user! tx)
              topic (new-topic! tx uid)
              ;; More Anki reviews on note 202 than on note 201.
              rich (basic! tx topic "Q" :anki_note_id 202 :anki_baseline_digest (apply str (repeat 64 "b")))
              poor (basic! tx topic "Q" :anki_note_id 201 :anki_baseline_digest (apply str (repeat 64 "a")))
              ;; Two cards on ONE note: deleting either must not retire it.
              shared-a (basic! tx topic "S" :anki_note_id 301)
              shared-b (basic! tx topic "S" :anki_note_id 301)]
          (anki-state! tx uid 202 9002 10)
          (anki-state! tx uid 201 9001 2)
          (let [result (dedupe! tx)]
            (is (live? tx rich))
            (is (not (live? tx poor)))
            (is (= 1 (count (filter #(live? tx %) [shared-a shared-b]))))
            (testing "only the note no remaining card holds is tombstoned, with its baseline"
              (is (= 1 (:tombstones result)))
              (is (= [{:anki_note_id 201 :anki_baseline_digest (apply str (repeat 64 "a"))}]
                    (rows tx "SELECT anki_note_id, anki_baseline_digest FROM anki_note_tombstones
                              WHERE user_id = ?" uid))))))))))

(deftest ^:db rows-the-indexes-allow-are-never-touched
  (if-not (db-up?)
    (println "SKIP flashcard-dedupe: no Postgres reachable.")
    (with-duplicates-allowed
      (fn [tx]
        (let [uid (new-user! tx)
              t1 (new-topic! tx uid)
              t2 (new-topic! tx uid)
              ids [(basic! tx t1 "same")
                   ;; Another topic.
                   (basic! tx t2 "same")
                   ;; Another kind.
                   (card! tx {:topic_id t1 :root_topic_id t1 :kind "reverse" :question "same"})
                   ;; The same text in the cloze column.
                   (cloze! tx t1 "same")
                   ;; NULL question and cloze, as occlusion, score and
                   ;; overlapping cards store them. The index keeps each NULL
                   ;; distinct, so GROUP BY must not merge them.
                   (card! tx {:topic_id t1 :root_topic_id t1 :kind "occlusion"})
                   (card! tx {:topic_id t1 :root_topic_id t1 :kind "occlusion"})
                   ;; NULL topic_id, for the same reason.
                   (card! tx {:root_topic_id t1 :kind "basic" :question "orphan"})
                   (card! tx {:root_topic_id t1 :kind "basic" :question "orphan"})]]
          (is (= {:deleted 0} (dedupe! tx)))
          (is (every? #(live? tx %) ids))
          (build-indexes! tx))))))

(deftest ^:db a-second-run-changes-nothing
  (if-not (db-up?)
    (println "SKIP flashcard-dedupe: no Postgres reachable.")
    (with-duplicates-allowed
      (fn [tx]
        (let [uid (new-user! tx)
              topic (new-topic! tx uid)]
          (basic! tx topic "again")
          (basic! tx topic "again")
          (is (= 1 (:deleted (dedupe! tx))))
          (is (= {:deleted 0} (dedupe! tx))))))))

(deftest ^:db an-undo-restore-skips-a-card-the-indexes-refuse
  (if-not (db-up?)
    (println "SKIP flashcard-dedupe: no Postgres reachable.")
    (jdbc/with-transaction [tx db/ds {:rollback-only true}]
      (build-indexes! tx)
      (let [uid (new-user! tx)
            topic (new-topic! tx uid)
            live (basic! tx topic "kept")
            next-id #(:id (one tx "SELECT nextval('flashcards_id_seq') AS id"))
            snapshot-row (fn [q] {:id (next-id) :topic_id topic :root_topic_id topic
                                  :kind "basic" :question q :answer "restored"})
            ;; A re-created twin of a live card, and a pair that a snapshot
            ;; taken before the repair can hold.
            twin (snapshot-row "kept")
            pair-a (snapshot-row "pair")
            pair-b (snapshot-row "pair")]
        (testing "the restore does not throw on a digest clash"
          (is (some? (db/restore-flashcards-tx! tx [twin pair-a pair-b]))))
        (is (live? tx live))
        (is (not (live? tx (:id twin))))
        (is (= 1 (count (filter #(live? tx %) [(:id pair-a) (:id pair-b)]))))))))
