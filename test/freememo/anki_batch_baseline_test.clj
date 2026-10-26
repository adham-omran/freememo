(ns freememo.anki-batch-baseline-test
  "The local baseline a batch records is the digest of what it SENT
   (plans/anki-sync-lease-batches.md §4.3, item 3.9).

   `record-anki-writes!` used to re-read the local digest at record time, after
   the Anki round trip. An edit made during that round trip was then stamped as
   synced while Anki held the older text. The digests now come from
   `anki-write-bundles`, read before the cards it sends.

   Needs a real Postgres, because the property is about which row state the
   digest was read from. SKIPS when no database is reachable. Every test creates
   its own user."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]
            [freememo.anki-sync-server :as sync-server]
            [freememo.anki-reconcile-server :as recon]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- new-user! []
  (let [tag (str "anki-batch-" (System/nanoTime))]
    (:id (jdbc/execute-one! db/ds
           ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
            tag (str tag "@example.test")]
           {:builder-fn rs/as-unqualified-maps}))))

(defn- new-root! [uid]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, title, kind) VALUES (?, 'batch doc', 'basic')
           RETURNING id" uid]
         {:builder-fn rs/as-unqualified-maps})))

(defn- new-pushed-card!
  "A card that already has a note, so `anki-write-bundles` needs no deck."
  [root-id q note-id]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO flashcards (topic_id, root_topic_id, kind, question, answer,
                                   anki_note_id)
           VALUES (?, ?, 'basic', ?, 'a', ?) RETURNING id" root-id root-id q note-id]
         {:builder-fn rs/as-unqualified-maps})))

(defn- unsynced? [card-id]
  (:u (jdbc/execute-one! db/ds
        ["SELECT (local_current_digest IS DISTINCT FROM local_baseline_digest
                  OR anki_note_id IS NULL) AS u
            FROM flashcards WHERE id = ?" card-id]
        {:builder-fn rs/as-unqualified-maps})))

(defn- sync-batch!
  "One batch the way the driver runs it, with `between!` standing in for the
   Anki round trip between bundle build and record."
  [uid card-id note-id between!]
  (let [bundles (sync-server/anki-write-bundles uid [card-id])]
    (is (:success bundles) (pr-str bundles))
    (between!)
    (recon/record-anki-writes! uid
      [{:card-id card-id :note-id note-id :anki-digest "anki-side" :anki-mod 1}]
      (:local-digests bundles))))

(deftest ^:db a-batch-with-no-edit-reads-synced
  (if-not (db-up?)
    (println "SKIP anki-batch-baseline: no Postgres reachable.")
    (let [uid (new-user!)
          root (new-root! uid)
          card (new-pushed-card! root "q" 9001)]
      (is (:success (sync-batch! uid card 9001 (fn []))))
      (is (false? (unsynced? card))))))

(deftest ^:db an-edit-during-the-round-trip-stays-unsynced
  (if-not (db-up?)
    (println "SKIP anki-batch-baseline: no Postgres reachable.")
    (let [uid (new-user!)
          root (new-root! uid)
          card (new-pushed-card! root "q" 9002)]
      (testing "the edit lands after the bundle was built and before the record"
        (is (:success (sync-batch! uid card 9002
                        #(db/update-flashcard! uid card {:question "edited"})))))
      (is (true? (unsynced? card))
        "Anki holds the pre-edit text, so the card must still read unsynced"))))
