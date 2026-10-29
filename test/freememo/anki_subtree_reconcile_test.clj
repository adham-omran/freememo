(ns freememo.anki-subtree-reconcile-test
  "A reconcile run scoped to one branch of the topic tree
   (plans/custom-context-menu.md section 5).

   The properties under test are the SQL ones: which cards a branch matches,
   what a branch run may write, and what it must leave alone. A pure fixture
   cannot reach them, so the DB tests need PostgreSQL and SKIP, not fail, when
   none is reachable (the `db-up?` guard the other DB suites use).

   Two roots, A and B, each with one child:

     A (card a)   A1 (card a1)       B (card b)   B1 (card b1)

   Every card is pushed (it holds a note id) and baselined. One tombstone stands
   for a card that was deleted. Every test creates its own user and deletes it
   afterwards, so nothing here touches the dev collection."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.activity-registry :as registry]
            [freememo.anki-digest :as digest]
            [freememo.anki-reconcile-server :as recon]
            [freememo.anki-subtree-request :as subtree]
            [freememo.db :as db]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

;; ---------------------------------------------------------------------------
;; The request seam. No database.
;; ---------------------------------------------------------------------------

(deftest a-request-made-before-the-driver-mounts-is-not-lost
  (reset! subtree/!pending [])
  (is (= :queued (subtree/request-subtree-sync! 41)))
  (subtree/request-subtree-sync! 42)
  (let [seen (atom [])
        unwatch! (subtree/watch! #(swap! seen conj %))]
    (try
      (is (= [41 42] @seen) "drained on subscribe, oldest first")
      (is (= [] @subtree/!pending))
      (subtree/request-subtree-sync! 43)
      (is (= [41 42 43] @seen) "a later request reaches the driver at once")
      (finally (unwatch!))))
  (testing "after unwatch the request waits instead of firing"
    (let [seen (atom [])]
      (subtree/request-subtree-sync! 44)
      (is (= [44] @subtree/!pending))
      (let [unwatch! (subtree/watch! #(swap! seen conj %))]
        (is (= [44] @seen))
        (unwatch!))))
  (reset! subtree/!pending []))

(deftest a-second-watch-replaces-the-first
  (reset! subtree/!pending [])
  (let [a (atom []) b (atom [])
        stop-a (subtree/watch! #(swap! a conj %))
        stop-b (subtree/watch! #(swap! b conj %))]
    (try
      (subtree/request-subtree-sync! 7)
      (is (= [] @a))
      (is (= [7] @b) "two live drivers would each start the run")
      (finally (stop-a) (stop-b))))
  (reset! subtree/!pending []))

(deftest a-request-must-name-a-topic
  (is (thrown? AssertionError (subtree/request-subtree-sync! nil)))
  (is (thrown? AssertionError (subtree/request-subtree-sync! "12")))
  (is (thrown? AssertionError (subtree/request-subtree-sync! (fn [])))))

(deftest the-activity-registry-and-the-client-share-one-work-key
  (let [wk (:work-key (registry/activity-type :anki-reconcile))]
    (is (= "collection" (wk)))
    (is (= "collection" (subtree/work-key nil)))
    (is (= "branch-12" (wk 12)))
    (is (= (wk 12) (subtree/work-key 12))))
  (is (= :topic (:subject (registry/activity-type :anki-reconcile)))
    "a branch run names its root as the subject")
  (is (= :server (:tier (registry/activity-type :anki-reconcile)))))

;; ---------------------------------------------------------------------------
;; A world with two roots. Needs PostgreSQL.
;; ---------------------------------------------------------------------------

(def ^:private plain {:builder-fn rs/as-unqualified-maps})
(def ^:private a-hex (apply str (repeat 64 "a")))

(defn- one [sql & params] (jdbc/execute-one! db/ds (into [sql] params) plain))
(defn- rows [sql & params] (jdbc/execute! db/ds (into [sql] params) plain))

(defn- new-user! []
  (let [tag (str "subtree-recon-" (System/nanoTime))]
    (:id (one "INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
           tag (str tag "@example.test")))))

(defn- new-topic! [uid parent-id title]
  (:id (one "INSERT INTO topics (user_id, parent_id, title, kind)
             VALUES (?, ?, ?, 'basic') RETURNING id" uid parent-id title)))

(defn- new-card!
  "A pushed card. `topic-id` is where it sits, `root-id` the top of its tree."
  [topic-id root-id note-id q]
  (:id (one "INSERT INTO flashcards (topic_id, root_topic_id, kind, question, answer,
                                     anki_note_id)
             VALUES (?, ?, 'basic', ?, 'ans', ?) RETURNING id"
         topic-id root-id q note-id)))

(defn- baseline!
  "Baseline `card-id` as if a run had just synced it: Anki side `a-hex`, local
   side the row's own digest."
  [uid card-id]
  (let [row (first (db/get-cards-for-reconcile uid [card-id]))]
    (db/set-card-baselines! uid [{:card-id card-id
                                  :anki-digest a-hex
                                  :local-digest (digest/local-digest row)}])))

(defn- card-state [card-id]
  (one "SELECT anki_note_id, anki_baseline_digest, local_baseline_digest,
               local_current_digest, baseline_digest_version, anki_conflict_at,
               anki_synced_at, question
          FROM flashcards WHERE id = ?" card-id))

(defn- tombstone-notes [uid]
  (set (map :anki_note_id (rows "SELECT anki_note_id FROM anki_note_tombstones
                                  WHERE user_id = ?" uid))))

(defn- pull-outcome
  "Plan real note-field pulls and retain their local digest and note identities."
  [{:keys [uid ca ca1 cb cb1 note tomb]} card-ids]
  (let [notes (into {} (map (fn [card-id]
                             [(note card-id)
                              {:fields {:Front {:value (:question (card-state card-id)) :order 0}
                                        :Back {:value "pulled answer" :order 1}}}]))
                card-ids)
        digests (merge (into {tomb a-hex} (map (fn [card-id] [(note card-id) a-hex]))
                         [ca ca1 cb cb1])
                  (update-vals notes #(digest/anki-digest (:fields %) "basic")))]
    (merge (recon/plan uid digests #{})
      {:digests digests :pull-notes notes})))

(defn- with-world
  "Build the two-root world, call `f` with its ids, delete the user after."
  [f]
  (let [uid (new-user!)
        base (* 1000000 (rem (System/nanoTime) 1000000))
        n (fn [i] (+ base i))
        a  (new-topic! uid nil "Root A")
        a1 (new-topic! uid a "A child")
        b  (new-topic! uid nil "Root B")
        b1 (new-topic! uid b "B child")
        ca  (new-card! a a (n 1) "qa")
        ca1 (new-card! a1 a (n 2) "qa1")
        cb  (new-card! b b (n 3) "qb")
        cb1 (new-card! b1 b (n 4) "qb1")
        tomb (n 5)]
    (try
      (doseq [c [ca ca1 cb cb1]] (baseline! uid c))
      (jdbc/execute! db/ds
        ["INSERT INTO anki_note_tombstones (user_id, anki_note_id, anki_baseline_digest)
          VALUES (?, ?, ?)" uid tomb a-hex])
      (f {:uid uid :a a :a1 a1 :b b :b1 b1
          :ca ca :ca1 ca1 :cb cb :cb1 cb1
          :note (fn [card] (:anki_note_id (card-state card)))
          :tomb tomb})
      (finally
        (try (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" uid])
             (catch Exception _ nil))))))

(defmacro ^:private with-db [[sym] & body]
  `(if-not (db-up?)
     (println "SKIP anki-subtree-reconcile: no Postgres reachable.")
     (with-world (fn [~sym] ~@body))))

(deftest ^:db a-branch-matches-the-cards-under-it-by-topic-not-by-root
  (with-db [w]
    (let [{:keys [uid a a1 b ca ca1 cb cb1 note tomb]} w]
      (testing "the whole collection, as before"
        (let [t (recon/scan-targets uid)]
          (is (= (set (map note [ca ca1 cb cb1])) (set (disj (set (:note-ids t)) tomb))))
          (is (contains? (set (:note-ids t)) tomb) "the full run scans tombstones")))
      (testing "a root reaches its child's card"
        (let [t (recon/scan-targets uid a)]
          (is (= #{(note ca) (note ca1)} (set (:note-ids t))))
          (is (= #{(note ca) (note ca1)} (set (keys (:kind-by-note t)))))))
      (testing "a MID-TREE root matches by topic_id. root_topic_id is the top of
                the tree, so it would match nothing here"
        (is (= #{(note ca1)} (set (:note-ids (recon/scan-targets uid a1)))))
        (is (= 1 (count (db/get-cards-for-reconcile uid nil a1)))))
      (testing "a branch run scans no tombstone"
        (is (not-any? #{tomb} (:note-ids (recon/scan-targets uid b)))))
      (testing "a nil root is the whole collection"
        (is (= (recon/scan-targets uid) (recon/scan-targets uid nil))))
      (testing "another user's branch yields nothing"
        (let [other (new-user!)]
          (try (is (= [] (:note-ids (recon/scan-targets other a))))
               (finally (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" other]))))))))

(deftest ^:db a-branch-plan-names-no-card-and-no-deletion-outside-the-root
  (with-db [w]
    (let [{:keys [uid a ca ca1 cb cb1 note tomb]} w
          ;; Edit one card in each root after its baseline, so both read :push.
          _ (jdbc/execute! db/ds ["UPDATE flashcards SET question = 'edited'
                                    WHERE id IN (?, ?)" ca cb])
          ;; Anki still holds every note as baselined, and the tombstone's note
          ;; unchanged, so a full run would delete it.
          digests (into {tomb a-hex} (map (fn [c] [(note c) a-hex])) [ca ca1 cb cb1])
          full (recon/plan uid digests #{})
          branch (recon/plan uid digests #{} a)]
      (testing "the full plan sees both roots and the tombstone"
        (is (= #{ca cb} (set (:write-card-ids full))))
        (is (= [tomb] (:note-ids-to-delete full))))
      (testing "the branch plan sees only A"
        (is (= [ca] (:write-card-ids branch)))
        (is (= {ca "basic"} (:kind-by-card branch)))
        (is (= [] (:note-ids-to-delete branch)))
        (is (= [] (:note-ids-to-drop branch)))
        (is (not (:aborted? branch))))
      (testing "a note Anki lost outside the root is not reported against the branch"
        (let [lost (recon/plan uid (dissoc digests (note cb) (note cb1)) #{(note cb) (note cb1)} a)]
          (is (= (:counts branch) (:counts lost))
            "the branch plan has no card that those notes could be absent for"))))))

(deftest ^:db a-branch-run-leaves-the-other-root-untouched
  (with-db [w]
    (let [{:keys [uid a ca ca1 cb cb1 note tomb]} w
          outcome (pull-outcome w [ca cb])]
      ;; B1 already carries a conflict flag from an earlier full run. A1 does too.
      (db/set-anki-conflicts! uid [ca1 cb1])
      (let [b-before  (mapv card-state [cb cb1])
            a1-before (card-state ca1)
            ca-before (card-state ca)
            result
            (recon/finish-run! uid
              ;; Include other-root IDs to exercise the finish boundary's scope gate.
              (assoc outcome
                :root-id a
                :backfill [{:card-id cb1 :note-id (note cb1)}]
                :conflict-card-ids [cb]
                :finished-note-ids [tomb]))]
        (is (:success result))
        (testing "no baseline, digest, note id, flag or timestamp of root B moved"
          (is (= b-before (mapv card-state [cb cb1]))))
        (testing "the tombstone stays for the next full run"
          (is (= #{tomb} (tombstone-notes uid))))
        (testing "A was written"
          (let [after (card-state ca)]
            (is (= (get (:digests outcome) (note ca)) (:anki_baseline_digest after)))
            (is (not= (:anki_baseline_digest ca-before) (:anki_baseline_digest after)))
            (is (= (:local_baseline_digest after) (:local_current_digest after)))))
        (testing "A's conflicts are replaced by the branch's list, and only A's"
          (is (nil? (:anki_conflict_at (card-state ca1)))
            "A1 stopped being a conflict, so its flag clears")
          (is (some? (:anki_conflict_at a1-before)))
          (is (some? (:anki_conflict_at (card-state cb1)))
            "B1's flag survives a run that never looked at B"))))))

(deftest ^:db a-full-run-still-covers-everything
  (with-db [w]
    (let [{:keys [uid ca ca1 cb cb1 note tomb]} w
          outcome (pull-outcome w [ca cb])]
      (db/set-anki-conflicts! uid [ca1 cb1])
      (let [result (recon/finish-run! uid
                     (assoc outcome :backfill [] :conflict-card-ids []
                       :finished-note-ids [tomb]))]
        (is (:success result))
        (is (= (get (:digests outcome) (note ca)) (:anki_baseline_digest (card-state ca))))
        (is (= (get (:digests outcome) (note cb)) (:anki_baseline_digest (card-state cb)))
          "no root id, no confinement")
        (is (nil? (:anki_conflict_at (card-state cb1))) "the whole conflict set is replaced")
        (is (nil? (:anki_conflict_at (card-state ca1))))
        (is (= #{} (tombstone-notes uid)))))))

(deftest ^:db the-write-gate-keeps-only-cards-under-the-root
  (with-db [w]
    (let [{:keys [uid a ca ca1 cb cb1]} w]
      (is (= #{ca ca1} (set (db/card-ids-in-subtree uid a [ca ca1 cb cb1]))))
      (is (= [] (db/card-ids-in-subtree uid a [])))
      (is (= [] (db/card-ids-in-subtree uid a [cb cb1])))
      (is (= [] (db/card-ids-in-subtree -1 a [ca])) "ownership still applies"))))
