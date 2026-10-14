(ns freememo.question-map-test
  "The question map's server side against PostgreSQL: pins on a snapshot, the
   request tail, and applying a reply (plans/anchored-questions.md §6.1–§6.3).
   No model is called: the test user has no API key, so root linking degrades
   to unlinked roots, which is its documented failure path. Every test skips
   when the database is down."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.assistant :as assistant]
            [freememo.db :as db]
            [freememo.document-ref :as docref]
            [freememo.question-map :as qm]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})

(defn- create-user! []
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
          (str "question-map-server-test-" (random-uuid))
          (str (random-uuid) "@example.test")]
         opts)))

(defn- topic! [user-id kind parent-id repo-path]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, kind, title, parent_id, repo_path)
           VALUES (?, ?, ?, ?, ?) RETURNING id"
          user-id kind (str kind " test") parent-id repo-path]
         opts)))

(defn- random-sha []
  (apply str (repeatedly 40 #(rand-nth "0123456789abcdef"))))

(defn- markdown-file!
  "A commit with its own sha under `repo`, and `path` in it as a Markdown file.
   Post: the file's document ref."
  [user-id repo path source]
  (let [commit (topic! user-id "commit" repo "")
        file (topic! user-id "code" commit path)]
    (jdbc/execute! db/ds ["UPDATE topics SET commit_sha = ? WHERE id = ?" (random-sha) commit])
    (jdbc/execute! db/ds ["INSERT INTO topic_code (topic_id, source, byte_size, lang)
                           VALUES (?, ?, ?, 'markdown')"
                          file source (count source)])
    (docref/for-topic user-id file)))

(def v1 (str "# Ports\n\nA port declares shape, degree, Type and timing.\n\n"
          "## Callables\n\nCallable slots also carry a closure signature.\n"))
(def v2 (str "# Ports\n\nA port declares shape, degree, Type and timing.\n\n"
          "## Callables\n\nCallable slots also carry a closure type.\n"))

(defmacro ^:private with-user [sym & body]
  `(if-not (db-up?)
     (is true "PostgreSQL is unavailable; integration assertion skipped")
     (do (#'db/setup-schema-question-pins!)
         (#'db/setup-schema-walkthroughs!)
         (#'db/setup-schema-known-ideas!)
         (let [~sym (create-user!)]
           (try ~@body
             (finally (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" ~sym])))))))

(deftest pins-carry-forward-and-go-stale-per-snapshot
  (with-user u
    (let [repo (topic! u "repo" nil nil)
          old (markdown-file! u repo "docs/spec.md" v1)
          new (markdown-file! u repo "docs/spec.md" v2)]
      (is (:ok (qm/create-pin! u old {:quote "shape, degree" :anchor "ports"
                                      :question "How do they relate?"})))
      (is (:ok (qm/create-pin! u old {:quote "closure signature" :anchor "callables"})))
      (is (:error (qm/create-pin! u old {:quote "  "})) "a blank quote is refused")
      (let [old-view (qm/snapshot-view u old)
            new-view (qm/snapshot-view u new)
            ranged (fn [view] (mapv (comp some? :range) (:pins view)))]
        (testing "§6.1.2: one map, seen from both snapshots"
          (is (= (:map-id old-view) (:map-id new-view)))
          (is (= [1 2] (mapv :label (:pins new-view)))))
        (testing "§6.1.3: anchored on the old snapshot, unmatched on the new one"
          (is (= [true true] (ranged old-view)))
          (is (= [true false] (ranged new-view))))
        (is (= 2 (:open-count new-view)))))))

(deftest the-request-tail-carries-the-map-and-the-persona-on-an-ask
  (with-user u
    (let [repo (topic! u "repo" nil nil)
          file (markdown-file! u repo "a.md" v1)]
      (is (nil? (qm/request-tail u file false "PERSONA")) "no map, no tail")
      (qm/create-pin! u file {:quote "closure signature" :question "Relates to what?"})
      (is (nil? (qm/request-tail u file false "PERSONA"))
        "a map with no roots adds nothing to an ordinary turn")
      (let [{:keys [content map-id]} (qm/request-tail u file true "PERSONA")]
        (is (integer? map-id))
        (is (str/includes? content "PERSONA") "§6.3.1.2: the Ask teaches as the Tutor")
        (is (str/includes? content "[pin 1] \"closure signature\"\nRelates to what?"))
        (is (str/includes? content "This reply is a diagnosis")))
      (is (= {:map-id (qm/map-id-for-ref u file)
              :text (qm/diagnosis-user-message
                      (filterv #(= "open" (:status %))
                        (db/question-pins u (qm/map-id-for-ref u file))))}
            (qm/open-pins-message u file))))))

(defn- reply [block prose] (str "```freememo-map\n" block "\n```\n\n" prose))

(deftest apply-reply-writes-the-map-and-returns-the-prose
  (with-user u
    (let [repo (topic! u "repo" nil nil)
          file (markdown-file! u repo "a.md" v1)
          _ (qm/create-pin! u file {:quote "shape, degree"})
          _ (qm/create-pin! u file {:quote "closure signature"})
          map-id (qm/map-id-for-ref u file)
          run {:map-id map-id :diagnosis? true :run-uuid (str (random-uuid))
               :ref file}]
      (testing "an invalid block writes nothing and says why"
        (let [r (qm/apply-reply! u run (reply "{\"next\": \"Nowhere\"}" "Q?"))]
          (is (= "Q?" (:prose r)))
          (is (string? (:problem r)))
          (is (empty? (db/question-roots u map-id)))))
      (testing "a reply with no block, on a diagnosis, is a problem but keeps its text"
        (is (= {:prose "Just text." :problem "The tutor did not send a map."}
              (qm/apply-reply! u run "Just text."))))
      (testing "a valid diagnosis"
        (let [r (qm/apply-reply! u run
                  (reply (str "{\"roots\": [{\"label\": \"Signature axes\", \"in_document\": true}],"
                           " \"pins\": {\"1\": \"Signature axes\"}}")
                    "How many axes does §4.7 list?"))
              roots (db/question-roots u map-id)]
          (is (= {:prose "How many axes does §4.7 list?" :problem nil} r))
          (is (= #{"Signature axes" "Pin 2: closure signature"} (set (map :label roots)))
            "§6.3.3: the pin the block left out has a root of its own")
          (is (= "next" (:status (first (filter #(= "Signature axes" (:label %)) roots)))))
          (is (every? :root_id (db/question-pins u map-id))))))))

(deftest the-transcript-never-shows-the-block
  (let [view (fn [content]
               (assistant/transcript-message-view
                 {:assistant_messages/id 1 :assistant_messages/role "assistant"
                  :assistant_messages/content content}))]
    (testing "§6.3.2.1: mid-stream, an open block renders as an empty answer"
      (is (= "empty" (:assistant_messages/render-mode
                       (view "```freememo-map\n{\"roots\": [")))))
    (testing "a closed block leaves only the prose"
      (let [v (view (reply "{}" "Plain prose."))]
        (is (not (str/includes? (str (:assistant_messages/safe-html v)
                                  (:assistant_messages/content v))
                   "freememo-map")))))))

(deftest a-pin-on-code-anchors-by-its-exact-quote
  (testing "§6.3.7: a repeated quote resolves to the occurrence nearest the hint"
    (let [src "a = 1;\nb = 2;\na = 1;\n"]
      (is (= {:offsets [0 6]} (qm/locate-in-source src "a = 1;" nil)))
      (is (= {:offsets [14 20]} (qm/locate-in-source src "a = 1;" 13)))
      (is (nil? (qm/locate-in-source src "a  = 1;" 0)) "whitespace is not folded"))))

(deftest a-pin-made-at-a-commit-shows-on-the-snapshot-topic
  (with-user u
    (let [repo (topic! u "repo" nil nil)
          topic-ref (markdown-file! u repo "docs/spec.md" v1)
          ;; The same file at the same commit, named the way a walkthrough
          ;; canvas names it: no topic id.
          bare (dissoc topic-ref :topic-id)
          {:keys [ok]} (qm/create-pin! u bare {:quote "shape, degree" :anchor "ports"})]
      (is ok)
      (testing "§6.3.4: the pin stores its sha and no topic id"
        (is (= [nil (:sha topic-ref)]
              ((juxt :snapshot_topic_id :snapshot_sha)
               (jdbc/execute-one! db/ds ["SELECT snapshot_topic_id, snapshot_sha FROM question_pins
                                           WHERE id = ?" (:id ok)] opts)))))
      (testing "§6.3.6: the snapshot topic shows it"
        (is (some? (:range (first (:pins (qm/snapshot-view u topic-ref))))))))))

(deftest i-know-it-records-the-snapshot-it-was-said-on
  (with-user u
    (let [repo (topic! u "repo" nil nil)
          ref (markdown-file! u repo "docs/ir-spec.md" v1)
          _ (qm/create-pin! u ref {:quote "shape, degree"})
          map-id (qm/map-id-for-ref u ref)
          run {:map-id map-id :diagnosis? true :run-uuid (str (random-uuid)) :ref ref}
          _ (qm/apply-reply! u run (reply "{\"roots\": [{\"label\": \"Axes\", \"in_document\": true}]}" "Q?"))
          root (first (db/question-roots u map-id))]
      (is (:ok (qm/set-root-known! u (:id root) true ref)))
      (testing "§6.8.2 of plans/commit-range-walkthrough.md: the mark carries the file's commit"
        (is (= [repo (:sha ref)]
              ((juxt :as_of_repo_topic_id :as_of_sha)
               (first (db/known-marks-for-entities u [(:entity_id (first (db/question-roots u map-id)))])))))))))
