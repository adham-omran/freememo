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

(defn- markdown-file! [user-id repo path source]
  (let [commit (topic! user-id "commit" repo "")
        file (topic! user-id "code" commit path)]
    (jdbc/execute! db/ds ["INSERT INTO topic_code (topic_id, source, byte_size, lang)
                           VALUES (?, ?, ?, 'markdown')"
                          file source (count source)])
    file))

(def v1 (str "# Ports\n\nA port declares shape, degree, Type and timing.\n\n"
          "## Callables\n\nCallable slots also carry a closure signature.\n"))
(def v2 (str "# Ports\n\nA port declares shape, degree, Type and timing.\n\n"
          "## Callables\n\nCallable slots also carry a closure type.\n"))

(defmacro ^:private with-user [sym & body]
  `(if-not (db-up?)
     (is true "PostgreSQL is unavailable; integration assertion skipped")
     (do (#'db/setup-schema-question-pins!)
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
      (is (= {:map-id (qm/map-id-for-topic u file)
              :text (qm/diagnosis-user-message
                      (filterv #(= "open" (:status %))
                        (db/question-pins u (qm/map-id-for-topic u file))))}
            (qm/open-pins-message u file))))))

(defn- reply [block prose] (str "```freememo-map\n" block "\n```\n\n" prose))

(deftest apply-reply-writes-the-map-and-returns-the-prose
  (with-user u
    (let [repo (topic! u "repo" nil nil)
          file (markdown-file! u repo "a.md" v1)
          _ (qm/create-pin! u file {:quote "shape, degree"})
          _ (qm/create-pin! u file {:quote "closure signature"})
          map-id (qm/map-id-for-topic u file)
          run {:map-id map-id :diagnosis? true :run-uuid (str (random-uuid))
               :page-topic-id file}]
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
