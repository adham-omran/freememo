(ns freememo.anki-reconcile-write-cost-test
  "The reconcile's server writes are set-based, and a stop keeps what finished
   (plans/anki-reconcile-write-cost.md §1–§3).

   Needs a real Postgres: the properties are about statements, transactions and
   the document-summary triggers. SKIPS when no database is reachable. Every test
   creates its own user and deletes it after.

   The fixture installs the summary functions from the code under test, because
   `db.clj` never hot-reloads and the dev database may hold an older trigger."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.anki-digest :as digest]
            [freememo.anki-reconcile :as rec]
            [freememo.anki-reconcile-server :as recon]
            [freememo.db :as db]))

(def ^:private plain {:builder-fn rs/as-unqualified-maps})
(def ^:private a-hex (apply str (repeat 64 "a")))
(def ^:private b-hex (apply str (repeat 64 "b")))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(use-fixtures :once
  (fn [run]
    (when (db-up?) (#'db/setup-schema-document-summaries!))
    (run)))

(defn- one [sql & params] (jdbc/execute-one! db/ds (into [sql] params) plain))

(defn- new-user! []
  (let [tag (str "write-cost-" (System/nanoTime))]
    (:id (one "INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
           tag (str tag "@example.test")))))

(defn- new-root! [uid]
  (:id (one "INSERT INTO topics (user_id, title, kind) VALUES (?, 'doc', 'basic')
             RETURNING id" uid)))

(defn- new-cards!
  "`n` unpushed cards under `root-id`, in one statement, with their digests
   stamped as `insert-flashcards!` stamps them. Returns their ids in order."
  [root-id n]
  (let [ids (sort (mapv :id (jdbc/execute! db/ds
                              ["INSERT INTO flashcards (topic_id, root_topic_id, kind, question, answer)
                                SELECT ?, ?, 'basic', 'q' || g, 'a' FROM generate_series(1, ?) g
                                RETURNING id" root-id root-id n]
                              plain)))]
    (db/restamp-card-digests! db/ds ids)
    (vec ids)))

(defn- card [id]
  (one "SELECT anki_note_id, anki_baseline_digest, local_baseline_digest,
               local_current_digest, anki_baseline_mod, baseline_digest_version
          FROM flashcards WHERE id = ?" id))

(defn- with-user [f]
  (let [uid (new-user!)]
    (try (f uid)
      (finally (one "DELETE FROM users WHERE id = ?" uid)))))

(defn- update-statements
  "Run `f`, and count the `UPDATE flashcards AS f` statements it sent."
  [f]
  (let [n (atom 0)
        execute-one! jdbc/execute-one!]
    (with-redefs [jdbc/execute-one!
                  (fn [& args]
                    (when (some-> (second args) first (str/starts-with? "UPDATE flashcards AS f"))
                      (swap! n inc))
                    (apply execute-one! args))]
      (f))
    @n))

;; ---------------------------------------------------------------------------
;; §1 set-based writers
;; ---------------------------------------------------------------------------

(deftest ^:db note-ids-are-one-statement-and-owned
  (if-not (db-up?)
    (println "SKIP write-cost: no Postgres reachable.")
    (with-user
      (fn [uid]
        (with-user
          (fn [other]
            (let [ids (new-cards! (new-root! uid) 250)
                  foreign (first (new-cards! (new-root! other) 1))
                  pairs (conj (mapv (fn [id] [id (+ 7000000 id)]) ids)
                          [foreign 1])]
              (is (= 1 (update-statements #(db/set-anki-note-ids uid pairs))))
              (is (every? #(= (+ 7000000 %) (:anki_note_id (card %))) ids))
              (testing "the ownership clause holds"
                (is (nil? (:anki_note_id (card foreign))))))))))))

(deftest ^:db baselines-keep-the-per-row-semantics
  (if-not (db-up?)
    (println "SKIP write-cost: no Postgres reachable.")
    (with-user
      (fn [uid]
        (let [[c1 c2 c3 c4] (new-cards! (new-root! uid) 4)]
          (one "UPDATE flashcards SET anki_baseline_mod = 5 WHERE id IN (?, ?)" c2 c4)
          (testing "without restamp, the local digest is copied into current"
            (is (= 1 (update-statements
                       #(db/set-card-baselines! uid
                          [{:card-id c1 :anki-digest a-hex :local-digest b-hex :anki-mod 9}
                           {:card-id c2 :anki-digest a-hex :local-digest b-hex}]))))
            (is (= {:anki_baseline_digest a-hex :local_baseline_digest b-hex
                    :local_current_digest b-hex :anki_baseline_mod 9
                    :baseline_digest_version digest/digest-version}
                  (dissoc (card c1) :anki_note_id)))
            (is (= 5 (:anki_baseline_mod (card c2)))
              "an entry with no :anki-mod keeps the stored one"))
          (testing "with restamp, current is the row's own digest"
            (db/set-card-baselines! uid
              [{:card-id c3 :anki-digest a-hex :local-digest b-hex :anki-mod 3}
               {:card-id c4 :anki-digest a-hex :local-digest b-hex}]
              {:restamp-current? true})
            (let [row (first (db/get-cards-for-reconcile uid [c3]))]
              (is (= (digest/local-digest row) (:local_current_digest (card c3)))))
            (is (= b-hex (:local_baseline_digest (card c3))))
            (is (= 3 (:anki_baseline_mod (card c3))))
            (is (= 5 (:anki_baseline_mod (card c4)))))
          (testing "a repeated card id keeps the last entry, as the loop did"
            (db/set-card-baselines! uid
              [{:card-id c1 :anki-digest a-hex :local-digest a-hex}
               {:card-id c1 :anki-digest b-hex :local-digest b-hex}])
            (is (= b-hex (:anki_baseline_digest (card c1))))))))))

(deftest ^:db a-baseline-failure-keeps-the-note-ids
  (if-not (db-up?)
    (println "SKIP write-cost: no Postgres reachable.")
    (with-user
      (fn [uid]
        (let [ids (new-cards! (new-root! uid) 3)
              entries (mapv (fn [id] {:card-id id :note-id (+ 8000000 id)
                                      :anki-digest a-hex :anki-mod 1})
                        ids)
              result (with-redefs [db/set-card-baselines!
                                   (fn [& _] (throw (ex-info "baseline write failed" {})))]
                       (recon/record-anki-writes! uid entries
                         (zipmap ids (repeat b-hex))))]
          (is (false? (:success result)))
          (doseq [id ids
                  :let [row (card id)]]
            (is (= (+ 8000000 id) (:anki_note_id row)))
            (is (nil? (:anki_baseline_digest row)))
            (is (= :needs-baseline
                  (rec/classify {:anki-now a-hex
                                 :anki-baseline (:anki_baseline_digest row)
                                 :local-now b-hex
                                 :local-baseline (:local_baseline_digest row)
                                 :baseline-version (:baseline_digest_version row)})))))))))

;; ---------------------------------------------------------------------------
;; §2 card-only summary refresh
;; ---------------------------------------------------------------------------

(defn- summary [root-id]
  (one "SELECT total_items, done_items, total_cards, synced_cards
          FROM document_summaries WHERE root_topic_id = ?" root-id))

(defn- matches-full-refresh?
  "The trigger-maintained row for each root equals a fresh full refresh."
  [& root-ids]
  (let [maintained (mapv summary root-ids)]
    (one "SELECT freememo_refresh_document_summaries(?::bigint[])"
      (into-array Long (map long root-ids)))
    (= maintained (mapv summary root-ids))))

(deftest ^:db card-writes-keep-the-summary-exact
  (if-not (db-up?)
    (println "SKIP write-cost: no Postgres reachable.")
    (with-user
      (fn [uid]
        (let [r1 (new-root! uid)
              r2 (new-root! uid)
              ids (new-cards! r1 5)]
          (is (= 5 (:total_cards (summary r1))))
          (is (matches-full-refresh? r1 r2) "after insert")
          (db/set-anki-note-ids uid (mapv (fn [id] [id id]) (take 3 ids)))
          (db/set-card-baselines! uid
            (mapv (fn [id] {:card-id id :anki-digest a-hex :local-digest b-hex})
              (take 2 ids)))
          (is (= 2 (:synced_cards (summary r1))))
          (is (matches-full-refresh? r1 r2) "after update")
          (one "UPDATE flashcards SET root_topic_id = ?, topic_id = ? WHERE id = ?"
            r2 r2 (first ids))
          (is (= [4 1] (mapv (comp :total_cards summary) [r1 r2])))
          (is (matches-full-refresh? r1 r2) "after a root move")
          (one "DELETE FROM flashcards WHERE id = ?" (second ids))
          (is (= 3 (:total_cards (summary r1))))
          (is (matches-full-refresh? r1 r2) "after delete"))))))

(deftest ^:db a-card-write-skips-the-descendant-walk
  (if-not (db-up?)
    (println "SKIP write-cost: no Postgres reachable.")
    (with-user
      (fn [uid]
        (let [root (new-root! uid)]
          (one "INSERT INTO topics (user_id, parent_id, title, kind)
                SELECT ?, ?, 't' || g, 'basic' FROM generate_series(1, 50000) g"
            uid root)
          (let [[id] (new-cards! root 1)
                time-ms (fn []
                          (let [t0 (System/nanoTime)]
                            (one "UPDATE flashcards SET anki_synced_at = NOW() WHERE id = ?" id)
                            (/ (- (System/nanoTime) t0) 1e6)))
                ;; The median of nine runs, after one warm-up.
                ms (do (time-ms) (nth (sort (repeatedly 9 time-ms)) 4))]
            (println "card UPDATE on a 50001-topic root:" ms "ms")
            (is (<= ms 20.0))))))))

;; ---------------------------------------------------------------------------
;; §3 chunked finish phase
;; ---------------------------------------------------------------------------

(deftest ^:db a-failed-backfill-chunk-keeps-the-others
  (if-not (db-up?)
    (println "SKIP write-cost: no Postgres reachable.")
    (with-user
      (fn [uid]
        (let [ids (new-cards! (new-root! uid) 6)
              _ (db/set-anki-note-ids uid (mapv (fn [id] [id id]) ids))
              calls (atom 0)
              set-card-baselines! db/set-card-baselines!
              result (with-redefs [recon/finish-chunk-size 2
                                   db/set-card-baselines!
                                   ;; The 2-arity calls the 3-arity through the
                                   ;; var, so count the outer calls only.
                                   (fn [& args]
                                     (if (and (= 2 (count args)) (= 2 (swap! calls inc)))
                                       (throw (ex-info "chunk 2 failed" {}))
                                       (apply set-card-baselines! args)))]
                       (recon/backfill-baselines! uid
                         (mapv (fn [id] {:card-id id :anki-digest a-hex}) ids)))
              baselined (mapv #(some? (:anki_baseline_digest (card %))) ids)]
          (is (= [true true false false true true] baselined))
          (is (false? (:success result)))
          (is (= 4 (:count result)))
          (is (str/includes? (:error result) "chunk 2 failed")))))))
