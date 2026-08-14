(ns freememo.anki-tombstone-paths-test
  "Every path that hard-deletes a flashcard must record an Anki tombstone.

   Reads `db.clj` as TEXT rather than exercising the paths, for the reason
   `account_transfer_format_test` reads it as text: loading `freememo.db` builds
   its HikariCP pool at namespace load, so a test that required it could not run
   without PostgreSQL. This one runs with the database down.

   What it protects: `card-components/try-delete-anki-notes!` deletes the Anki
   note from the client on every local-delete path, and it is fire-and-forget.
   With Anki closed the note survives with nothing pointing at it — ten such
   orphans were counted in production. The tombstone is the durable record that
   lets the next reconcile finish the job, and it is only durable if every
   deleting path writes one.

   A new deleting path fails here until it either calls
   `db/record-anki-tombstones!` or is added to `exempt-fns` with a reason."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def ^:private db-source (slurp (io/file "src/freememo/db.clj")))

(def ^:private delete-patterns
  "How a flashcards delete is spelled in this file — raw SQL and HoneySQL both."
  [#"DELETE\s+FROM\s+flashcards" #":delete-from\s+:flashcards"])

(def ^:private exempt-fns
  "Deleting paths that owe no tombstone, each with the reason.
   An entry here is a claim that the cards it removes were never in Anki, or
   that something else already tombstoned them."
  {"restore-flashcards-tx!"
   "re-inserts cards, does not delete them — the ON CONFLICT arm reads as a
    delete to a text scan only"})

(defn- top-level-forms
  "[{:start :end}] for every top-level form in `src` — one whose opening paren
   sits in column 0. A form runs until the next one starts."
  [src]
  (let [idxs (loop [i 0 acc (if (str/starts-with? src "(") [0] [])]
               (if-let [j (str/index-of src "\n(" i)]
                 (recur (inc j) (conj acc (inc j)))
                 acc))]
    (mapv (fn [a b] {:start a :end (or b (count src))})
      idxs (concat (rest idxs) [nil]))))

(defn- top-level-defns
  "[{:name :body}] for every top-level `def`/`defn`/`defn-` in `src`.
   Metadata like `^:private` between the head and the name is skipped."
  [src]
  (into []
    (keep (fn [{:keys [start end]}]
            (let [body (subs src start end)]
              (when-let [m (re-find #"^\((?:defn-|defn|def)\s+(?:\^[^\s]+\s+)*([^\s\(\[\{]+)"
                             body)]
                {:name (second m) :body body}))))
    (top-level-forms src)))

(defn- deleting-fns
  "Top-level fns whose body contains a flashcards delete."
  []
  (let [forms (top-level-defns db-source)]
    (filterv (fn [{:keys [body]}]
               (some #(re-find % body) delete-patterns))
      forms)))

(deftest t1-there-is-at-least-one-deleting-path
  (testing "a scan that finds nothing would pass every assertion below vacuously"
    (is (pos? (count (deleting-fns)))
      "no flashcards delete found in db.clj — the pattern list has drifted")))

(deftest t2-every-flashcard-delete-path-tombstones
  (let [offenders (into []
                    (comp (remove #(contains? exempt-fns (:name %)))
                      (remove #(str/includes? (:body %) "record-anki-tombstones!"))
                      (map :name))
                    (deleting-fns))]
    (is (empty? offenders)
      (str "these db.clj fns delete flashcards without recording an Anki "
        "tombstone: " (pr-str offenders)
        ". Call db/record-anki-tombstones! in the same transaction, or add the "
        "fn to exempt-fns with a reason."))))

(deftest t3-exemptions-name-real-fns
  (testing "an exemption for a fn that no longer exists hides the next offender"
    (let [names (set (map :name (top-level-defns db-source)))]
      (doseq [n (keys exempt-fns)]
        (is (contains? names n) (str "exempt-fns names a missing fn: " n))))))

(deftest t4-the-recorder-and-its-subtree-reader-exist
  (testing "the two functions every deleting path depends on"
    (is (str/includes? db-source "(defn record-anki-tombstones!"))
    (is (str/includes? db-source "(defn subtree-synced-cards"))))

(deftest t5-staging-tombstones-because-it-destroys-the-notes
  (testing "stage-topic-for-deletion! hard-deletes cards AND its caller deletes
            the Anki notes, so it owes a tombstone like any other path"
    (let [f (first (filter #(= "stage-topic-for-deletion!" (:name %))
                     (top-level-defns db-source)))]
      (is (some? f) "stage-topic-for-deletion! not found")
      (is (str/includes? (:body f) "record-anki-tombstones!")))))

(deftest t6-the-cascade-path-reads-the-subtree-before-deleting
  (testing "delete-topic-for-user! removes topics, and the cards cascade — the
            note ids must be read while the rows still exist"
    (let [f (first (filter #(= "delete-topic-for-user!" (:name %))
                     (top-level-defns db-source)))
          body (:body f)]
      (is (some? f))
      (is (str/includes? body "subtree-synced-cards"))
      (is (< (str/index-of body "subtree-synced-cards")
            (str/index-of body ":delete-from :topics"))
        "the subtree must be read BEFORE the delete, or it reads nothing"))))
