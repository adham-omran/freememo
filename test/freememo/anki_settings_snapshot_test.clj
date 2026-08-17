(ns freememo.anki-settings-snapshot-test
  "DB-backed checks for the Anki settings snapshot — the two columns that let a
   header or citation edit dirty a card (plans/anki-html-change-detection.md §4).

   These need a real Postgres because the properties under test ARE the SQL:
   `write-anki-settings-snapshot!` owns the ownership clause, and
   `topic-ids-for-source` is a recursive CTE whose whole point is where it stops.
   Neither can be exercised from a pure fixture.

   The DB tests SKIP (not fail) when no database is reachable, matching
   `fsrs-integration-test`, so the suite stays green in DB-less environments.

   Every test creates its own user, so nothing here depends on or disturbs the
   dev collection."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- new-user! []
  (let [tag (str "anki-snap-" (System/nanoTime))]
    (:id (jdbc/execute-one! db/ds
           ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
            tag (str tag "@example.test")]
           {:builder-fn rs/as-unqualified-maps}))))

(defn- new-root! [uid]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, title, kind) VALUES (?, 'snapshot doc', 'basic')
           RETURNING id" uid]
         {:builder-fn rs/as-unqualified-maps})))

(defn- new-child! [uid parent-id]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, parent_id, title, kind)
           VALUES (?, ?, 'snapshot extract', 'basic') RETURNING id" uid parent-id]
         {:builder-fn rs/as-unqualified-maps})))

(defn- new-source! [uid title]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO sources (user_id, csl_type, title) VALUES (?, 'webpage', ?)
           RETURNING id" uid title]
         {:builder-fn rs/as-unqualified-maps})))

(defn- new-card! [topic-id root-id q]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO flashcards (topic_id, root_topic_id, kind, question, answer)
           VALUES (?, ?, 'basic', ?, 'a') RETURNING id" topic-id root-id q]
         {:builder-fn rs/as-unqualified-maps})))

(defn- snapshot-of [card-id]
  (jdbc/execute-one! db/ds
    ["SELECT anki_header_text, anki_bibliography_html, local_current_digest
      FROM flashcards WHERE id = ?" card-id]
    {:builder-fn rs/as-unqualified-maps}))

(deftest ^:db nil-arguments-write-nothing
  (if-not (db-up?)
    (println "SKIP anki-settings-snapshot: no Postgres reachable.")
    (let [uid (new-user!)
          root (new-root! uid)
          card (new-card! root root "q")
          before (snapshot-of card)]
      (testing "a nil user-id must not widen the scope — HoneySQL renders
                [:= :user_id nil] as user_id IS NULL, which child topics match"
        (is (= 0 (db/set-anki-header-snapshot! nil root "H")))
        (is (= 0 (db/set-anki-bibliography-snapshot! nil 1 "B")))
        (is (= 0 (db/refresh-anki-settings-snapshot! nil [{:card-id card :header-text "H"}]))))
      (testing "a nil scope is a caller bug, not an empty-scope write"
        (is (= 0 (db/set-anki-header-snapshot! uid nil "H")))
        (is (= 0 (db/set-anki-bibliography-snapshot! uid nil "B"))))
      (testing "an empty entry list is a no-op, not an IN () syntax error"
        (is (= 0 (db/refresh-anki-settings-snapshot! uid [])))
        (is (= 0 (db/set-anki-bibliography-snapshot-for-topics! uid [] "B"))))
      (is (= before (snapshot-of card))))))

(deftest ^:db ownership-is-enforced-by-the-writer
  (if-not (db-up?)
    (println "SKIP anki-settings-snapshot: no Postgres reachable.")
    (let [owner (new-user!)
          other (new-user!)
          owner-root (new-root! owner)
          other-root (new-root! other)
          owner-card (new-card! owner-root owner-root "mine")
          other-card (new-card! other-root other-root "theirs")]
      (testing "a fan-out reaches only the caller's own cards"
        (is (= 1 (db/set-anki-header-snapshot! owner owner-root "H")))
        (is (= "H" (:anki_header_text (snapshot-of owner-card))))
        (is (nil? (:anki_header_text (snapshot-of other-card)))))
      (testing "another user's root id yields nothing, not that user's cards"
        (is (= 0 (db/set-anki-header-snapshot! owner other-root "STOLEN")))
        (is (nil? (:anki_header_text (snapshot-of other-card)))))
      (testing "the push refresh silently skips ids the user does not own —
                ownership is enforced here, not by trusting the caller"
        (is (= 1 (db/refresh-anki-settings-snapshot! owner
                   [{:card-id owner-card :header-text "P" :bibliography-html "B"}
                    {:card-id other-card :header-text "P" :bibliography-html "B"}])))
        (is (= "P" (:anki_header_text (snapshot-of owner-card))))
        (is (nil? (:anki_header_text (snapshot-of other-card))))))))

(deftest ^:db a-snapshot-write-moves-the-digest
  (if-not (db-up?)
    (println "SKIP anki-settings-snapshot: no Postgres reachable.")
    (let [uid (new-user!)
          root (new-root! uid)
          card (new-card! root root "q")]
      (db/restamp-card-digests! db/ds [card])
      (let [before (:local_current_digest (snapshot-of card))]
        (testing "the digest moves with the header — that IS how a settings edit
                  dirties a card, since the settings write touches no card row"
          (db/set-anki-header-snapshot! uid root "Chapter 1")
          (is (not= before (:local_current_digest (snapshot-of card)))))
        (testing "and returns to its old value when the header does"
          (db/set-anki-header-snapshot! uid root nil)
          (is (= before (:local_current_digest (snapshot-of card)))))
        (testing "writing one column leaves the other alone"
          (db/set-anki-header-snapshot! uid root "H")
          (db/refresh-anki-settings-snapshot! uid [{:card-id card :header-text "H"
                                                   :bibliography-html "B"}])
          (is (= "H" (:anki_header_text (snapshot-of card))))
          (is (= "B" (:anki_bibliography_html (snapshot-of card))))
          (db/set-anki-header-snapshot! uid root "H2")
          (is (= "B" (:anki_bibliography_html (snapshot-of card)))))))))

(deftest ^:db a-nearer-source-shadows-a-farther-one
  (if-not (db-up?)
    (println "SKIP anki-settings-snapshot: no Postgres reachable.")
    (let [uid (new-user!)
          root (new-root! uid)
          plain-child (new-child! uid root)
          own-source-child (new-child! uid root)
          root-src (new-source! uid "root citation")
          child-src (new-source! uid "child citation")]
      (jdbc/execute! db/ds ["UPDATE topics SET source_id = ? WHERE id = ?" root-src root])
      (jdbc/execute! db/ds ["UPDATE topics SET source_id = ? WHERE id = ?"
                            child-src own-source-child])
      (let [root-card (new-card! root root "root q")
            inherit-card (new-card! plain-child root "inherits")
            own-card (new-card! own-source-child root "has its own")]
        (testing "the inverse walk stops at a descendant carrying its own source"
          (let [reached (set (db/topic-ids-for-source root-src))]
            (is (contains? reached root))
            (is (contains? reached plain-child))
            (is (not (contains? reached own-source-child)))))
        (testing "it agrees with the forward walk on every topic it returns"
          (doseq [t (db/topic-ids-for-source root-src)]
            (is (= root-src (db/resolve-effective-source-id t)))))
        (testing "so the fan-out writes the root citation onto the inheriting
                  cards and NOT onto the card that cites something else"
          (is (= 2 (db/set-anki-bibliography-snapshot! uid root-src "ROOT")))
          (is (= "ROOT" (:anki_bibliography_html (snapshot-of root-card))))
          (is (= "ROOT" (:anki_bibliography_html (snapshot-of inherit-card))))
          (is (nil? (:anki_bibliography_html (snapshot-of own-card)))))
        (testing "and the nearer source reaches only its own subtree"
          (is (= 1 (db/set-anki-bibliography-snapshot! uid child-src "CHILD")))
          (is (= "CHILD" (:anki_bibliography_html (snapshot-of own-card))))
          (is (= "ROOT" (:anki_bibliography_html (snapshot-of root-card)))))))))
