(ns freememo.account-transfer-format-test
  "The archive format's contracts, and the schema-drift guard behind them.

   plans/user-data-export-import.md §5.1, §5.2. `every-table-has-a-disposition`
   is the test that matters most: a table added to `db.clj` with no entry in
   `entity-tables` or `excluded-tables` is user data silently absent from every
   export, and nothing else in the codebase would say so."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.account-transfer-format :as fmt]))

;; ── Manifest (§5.1.2, §5.1.3) ──────────────────────────────────────

(def ^:private valid-manifest
  {:format-version fmt/format-version
   :export-id "9f0c1f8e-0000-4000-8000-000000000000"
   :exported-at "2026-08-04T12:00:00Z"
   :source-instance "https://freememo.net"
   :counts {:topics 3}
   :tiers #{:video}})

(deftest validate-manifest-accepts-a-complete-manifest
  (is (= {:ok true} (fmt/validate-manifest valid-manifest))))

(deftest validate-manifest-names-every-missing-key
  (testing "one missing key appears in the message"
    (let [r (fmt/validate-manifest (dissoc valid-manifest :export-id))]
      (is (false? (:ok r)))
      (is (str/includes? (:error r) ":export-id"))))
  (testing "several missing keys all appear"
    (let [r (fmt/validate-manifest (dissoc valid-manifest :counts :tiers))]
      (is (false? (:ok r)))
      (is (str/includes? (:error r) ":counts"))
      (is (str/includes? (:error r) ":tiers")))))

(deftest validate-manifest-refuses-an-unsupported-version
  (let [r (fmt/validate-manifest (assoc valid-manifest :format-version 999))]
    (is (false? (:ok r)))
    (is (str/includes? (:error r) "999")
      "the message must name the version found, so the user knows which build wrote it")))

(deftest validate-manifest-never-throws-on-junk
  (doseq [junk [nil "manifest" 7 [] #{}]]
    (let [r (fmt/validate-manifest junk)]
      (is (false? (:ok r)) (str "junk input: " (pr-str junk)))
      (is (string? (:error r))))))

(deftest the-version-this-build-writes-is-a-version-it-reads
  ;; An export this build cannot re-import is a release blocker, and the two
  ;; constants live far enough apart to drift.
  (is (contains? fmt/supported-versions fmt/format-version)))

;; ── Entity order (§5.2.1) ──────────────────────────────────────────

(deftest entity-tables-is-a-set-of-41-in-a-fixed-order
  (is (= 41 (count fmt/entity-tables)))
  (is (= 41 (count (set fmt/entity-tables))) "no table may appear twice"))

(defn- before?
  "Whether `a` precedes `b` in `entity-tables`. Pre: both are members."
  [a b]
  (< (.indexOf ^java.util.List fmt/entity-tables a)
    (.indexOf ^java.util.List fmt/entity-tables b)))

(deftest entity-order-satisfies-every-foreign-key
  (testing "a referenced table precedes its referrer"
    (doseq [[referrer referenced]
            [[:topics :sources]
             ;; poem_packages.topic_id is NOT NULL and UNIQUE, so its row cannot
             ;; be inserted before the poem topic it belongs to.
             [:poem_packages :topics]
             [:neoreader_highlights :topics]
             [:topic_files :topics]
             [:topic_videos :topics]
             [:topic_code :topics]
             [:repo_selections :topics]
             [:video_transcripts :topics]
             [:video_segments :topics]
             [:topic_pins :topics]
             [:reading_positions :topics]
             [:topic_pins :media]
             [:occlusion_groups :media]
             [:score_groups :media]
             [:flashcards :topics]
             [:flashcards :occlusion_groups]
             [:flashcards :score_groups]
             [:card_schedules :flashcards]
             [:card_versions :topics]
             [:topic_repetitions :topics]
             [:study_schedules :topics]
             [:curriculum_blocks :curricula]
             [:curriculum_materials :curriculum_blocks]
             [:curriculum_materials :topics]
             [:curriculum_exam_forms :curriculum_blocks]
             [:curriculum_exam_forms :curriculum_materials]
             [:curriculum_exam_sittings :curriculum_exam_forms]
             [:curriculum_grade_revisions :curriculum_exam_sittings]
             [:curriculum_unlock_overrides :curricula]
             [:curriculum_unlock_overrides :curriculum_blocks]
             [:assistant_chats :topics]
             [:assistant_messages :assistant_chats]
             [:assistant_runs :assistant_chats]
             [:assistant_runs :assistant_messages]
             [:assistant_checkpoints :assistant_chats]
             [:assistant_checkpoints :assistant_runs]
             [:assistant_checkpoints :assistant_messages]
             [:assistant_evidence :assistant_runs]
             [:kg_facts :kg_entities]
             [:kg_facts :kg_predicates]
             [:kg_facts :topics]
             [:kg_question_facts :kg_questions]
             [:kg_question_facts :kg_facts]
             ;; Both of its topic columns are NOT NULL: graph_topic_id is the
             ;; document and page_topic_id the page inside it.
             [:kg_page_extractions :topics]
             ;; kg_reviews' CHECK requires exactly one of question_id /
             ;; flashcard_id (db.clj:1006), so it needs both tables present.
             [:kg_reviews :kg_questions]
             [:kg_reviews :flashcards]]]
      (is (before? referenced referrer)
        (str (name referrer) " references " (name referenced) " and must follow it"))))
  (testing "settings is last — its keys embed topic ids (§5.4.8)"
    (is (= :settings (last fmt/entity-tables)))))

;; ── Schema drift (§5.2.2) ──────────────────────────────────────────

(defn- tables-created-by-schema-setup
  "Every table `db.clj` creates, read from its source.

   Reading the source rather than a live database keeps this test runnable with
   no PostgreSQL. The `\\s*\\(` tail is load-bearing: db.clj:704 mentions the
   same SQL inside a comment, and without it the regex captures a word from
   that sentence."
  []
  (->> (slurp "src/freememo/db.clj")
    (re-seq #"CREATE TABLE IF NOT EXISTS (\w+)\s*\(")
    (map (comp keyword second))
    set))

(deftest every-table-has-a-disposition
  (let [created (tables-created-by-schema-setup)
        accounted (set/union (set fmt/entity-tables) (set (keys fmt/excluded-tables)))]
    (is (seq created) "the regex must find tables — a db.clj refactor can silence it")
    (is (empty? (set/difference created accounted))
      (str "a table exists with no export disposition: "
        (pr-str (set/difference created accounted))
        " — add it to entity-tables or excluded-tables"))
    (is (empty? (set/difference accounted created))
      (str "a table is listed but no longer exists: "
        (pr-str (set/difference accounted created))))))

(deftest exported-and-excluded-are-disjoint
  (is (empty? (set/intersection (set fmt/entity-tables) (set (keys fmt/excluded-tables))))))

(deftest every-exclusion-states-a-reason
  (doseq [[table reason] fmt/excluded-tables]
    (is (and (string? reason) (seq reason))
      (str (name table) " must say why it is excluded"))))

;; ── Export scope coverage ──────────────────────────────────────────

(deftest every-exported-table-has-a-scope
  (let [tables (set fmt/entity-tables)
        scoped (set (keys fmt/entity-scopes))]
    (is (empty? (set/difference tables scoped))
      (str "an exported table has no scope: " (pr-str (set/difference tables scoped))
        " — account-export/scope-of throws on it, which crashes the WebSocket on"
        " every Settings page load and fails every account export"))
    (is (empty? (set/difference scoped tables))
      (str "a scope names a table that is not exported: "
        (pr-str (set/difference scoped tables))))))

(deftest every-scope-declares-its-placeholder-count
  (doseq [[table {:keys [where params order]}] fmt/entity-scopes]
    (testing (name table)
      (is (= params (count (re-seq #"\?" where)))
        ":params must equal the number of ? placeholders — count-query binds the
         user id that many times, and a mismatch fails at request time")
      (is (and (string? order) (seq order))
        ":order must be non-empty — two exports of unchanged data must agree"))))

;; ── Entity paths (§5.1.4) ──────────────────────────────────────────

(deftest entity-path-round-trips-every-table
  (doseq [table fmt/entity-tables]
    (is (= table (fmt/table-of-entity-path (fmt/entity-path table))))))

(deftest entity-path-throws-on-a-table-it-does-not-export
  (is (thrown? clojure.lang.ExceptionInfo (fmt/entity-path :users)))
  (is (thrown? clojure.lang.ExceptionInfo (fmt/entity-path :nonesuch))))

(deftest table-of-entity-path-is-nil-for-anything-else
  (doseq [path [nil "" "manifest.edn" "entities/" "entities/users.ndjson"
                "entities/topics.edn" "topics.ndjson" "blobs/media/1"
                "entities/../secrets.ndjson"]]
    (is (nil? (fmt/table-of-entity-path path)) (str "path: " (pr-str path)))))

;; ── Blob paths (§5.1.5) ────────────────────────────────────────────

(deftest blob-path-round-trips-every-kind
  (testing "topic_files carries a role"
    (is (= {:kind :topic_files :source-id 42 :role "main"}
          (fmt/parse-blob-path (fmt/blob-path :topic_files 42 "main"))))
    (is (= {:kind :topic_files :source-id 42 :role "audio"}
          (fmt/parse-blob-path (fmt/blob-path :topic_files 42 "audio")))))
  (testing "media and topic_videos carry none"
    (is (= {:kind :media :source-id 7 :role nil}
          (fmt/parse-blob-path (fmt/blob-path :media 7 nil))))
    (is (= {:kind :topic_videos :source-id 7 :role nil}
          (fmt/parse-blob-path (fmt/blob-path :topic_videos 7 nil))))))

(deftest one-topic-can-own-two-topic-file-blobs
  ;; A score topic holds role='main' and role='audio' (db.clj:255-262). The path
  ;; must separate them, or an export overwrites one with the other.
  (is (not= (fmt/blob-path :topic_files 42 "main")
        (fmt/blob-path :topic_files 42 "audio"))))

(deftest blob-path-throws-on-a-value-it-cannot-round-trip
  (testing "unknown kind"
    (is (thrown? clojure.lang.ExceptionInfo (fmt/blob-path :flashcards 1 nil))))
  (testing "source id must be a positive integer"
    (doseq [bad [0 -1 nil "1" 1.5]]
      (is (thrown? clojure.lang.ExceptionInfo (fmt/blob-path :media bad nil))
        (str "source id: " (pr-str bad)))))
  (testing "a role that would escape the blob directory or parse back wrongly"
    (doseq [bad ["../../etc/passwd" "main/extra" "MAIN" "with.dot" "" nil]]
      (is (thrown? clojure.lang.ExceptionInfo (fmt/blob-path :topic_files 1 bad))
        (str "role: " (pr-str bad)))))
  (testing "a role on a kind that has none"
    (is (thrown? clojure.lang.ExceptionInfo (fmt/blob-path :media 1 "main")))
    (is (thrown? clojure.lang.ExceptionInfo (fmt/blob-path :topic_videos 1 "main")))))

(deftest parse-blob-path-is-nil-for-anything-else
  (doseq [path [nil "" "blobs/" "blobs/media" "blobs/media/0" "blobs/media/-1"
                "blobs/media/abc" "blobs/users/1" "blobs/media/1/extra"
                "blobs/topic_files/1" "blobs/topic_files/1.MAIN"
                "blobs/topic_files/0.main" "entities/topics.ndjson"
                "blobs/../manifest.edn"]]
    (is (nil? (fmt/parse-blob-path path)) (str "path: " (pr-str path)))))
