(ns freememo.document-question-pins-db-test
  "Question maps keyed by a document topic, against PostgreSQL
   (plans/document-question-pins.md §1, §4.2, §7). Every test skips when the
   database is down."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.account-export :as export]
            [freememo.account-import]
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
          (str "document-pins-test-" (random-uuid))
          (str (random-uuid) "@example.test")]
         opts)))

(defn- topic!
  ([user-id kind parent-id] (topic! user-id kind parent-id nil))
  ([user-id kind parent-id page-number]
   (:id (jdbc/execute-one! db/ds
          ["INSERT INTO topics (user_id, kind, title, parent_id, page_number)
            VALUES (?, ?, ?, ?, ?) RETURNING id"
           user-id kind (str kind " test") parent-id page-number]
          opts))))

(defmacro ^:private with-users [syms & body]
  `(if-not (db-up?)
     (is true "PostgreSQL is unavailable; integration assertion skipped")
     (do (#'db/setup-schema-question-pins!)
         (#'db/setup-schema-known-ideas!)
         (#'db/setup-schema-pin-numbering!)
         (#'db/setup-schema-document-question-maps!)
         (let [~@(mapcat (fn [s] [s `(create-user!)]) syms)]
           (try ~@body
             (finally
               (jdbc/execute! db/ds (into [(str "DELETE FROM users WHERE id IN ("
                                             (str/join "," (repeat ~(count syms) "?"))
                                             ")")]
                                      ~(vec syms)))))))))

(defn- pdf! [user-id pages]
  (let [root (topic! user-id "pdf" nil)]
    {:root root
     :pages (into {} (map (fn [n] [n (topic! user-id "page" root n)])) pages)}))

(deftest the-key-rule
  (with-users [u other]
    (let [{:keys [root pages]} (pdf! u [1 2 3])
          extract (topic! u "basic" (pages 2))
          mine (topic! u "basic" nil)
          video (topic! u "video" nil)]
      (testing "7.1.1: a page and the merged view share the PDF root's map"
        (is (= [:topic root] (docref/doc-key (docref/for-topic u (pages 3)))))
        (is (= [:topic root] (docref/doc-key (docref/for-topic u root)))))
      (testing "7.1.2: an extract and a topic the user made key themselves"
        (is (= [:topic extract] (docref/doc-key (docref/for-topic u extract))))
        (is (= [:topic mine] (docref/doc-key (docref/for-topic u mine)))))
      (testing "a topic with no document editor has no ref"
        (is (nil? (docref/for-topic u video))))
      (testing "another user's topic has no ref"
        (is (nil? (docref/for-topic other (pages 1)))))
      (testing "a document ref is never read as a repository file"
        (let [ref (docref/for-topic u (pages 1))]
          (is (docref/document? ref))
          (is (not (docref/markdown? ref)))
          (is (nil? (docref/content u ref))))))))

(deftest one-key-per-map
  (with-users [u other]
    (let [doc (topic! u "web" nil)
          map-id (db/ensure-question-document-map! u doc)]
      (is (some? map-id))
      (is (= map-id (db/ensure-question-document-map! u doc)) "one map per document")
      (is (nil? (db/ensure-question-document-map! other doc)) "only the owner makes a map")
      (testing "1.1.2: a row with both keys, or neither, is refused"
        (is (thrown? Exception
              (jdbc/execute! db/ds ["INSERT INTO question_maps (user_id, topic_id, repo_topic_id, repo_path)
                                     VALUES (?, ?, ?, 'a.md')" u doc doc])))
        (is (thrown? Exception
              (jdbc/execute! db/ds ["INSERT INTO question_maps (user_id) VALUES (?)" u])))))))

(defn- seen-at [pin-id]
  (:tutor_seen_at (jdbc/execute-one! db/ds
                    ["SELECT tutor_seen_at FROM question_pins WHERE id = ?" pin-id] opts)))

(deftest pins-on-a-pdf
  (with-users [u]
    (let [{:keys [root pages]} (pdf! u [1 2 3])
          page-ref (docref/for-topic u (pages 3))
          merged-ref (docref/for-topic u root)
          on-page (:ok (qm/create-pin! u page-ref {:quote "  a passage  " :question "Why?"}))
          merged (:ok (qm/create-pin! u merged-ref {:quote "across" :anchor "fm-page-2"}))]
      (testing "the quote is trimmed, as on rendered Markdown"
        (is (= "a passage" (:quote on-page))))
      (testing "1.1.3, 2.4: the hint is the open page, or the page the marker names"
        (let [view (qm/snapshot-view u page-ref)
              by-label (into {} (map (juxt :label identity)) (:pins view))]
          (is (= (pages 3) (:snapshot_topic_id (by-label (:label on-page)))))
          (is (= (pages 2) (:snapshot_topic_id (by-label (:label merged)))))
          (is (= 2 (:hint_page (by-label (:label merged)))))
          (testing "1.2.3: the server locates nothing, and says so"
            (is (= :client (:locate view)))
            (is (every? nil? (map :range (:pins view)))))
          (testing "the merged view reads the same map"
            (is (= (:map-id view) (:map-id (qm/snapshot-view u merged-ref)))))))
      (testing "a finger on a document names the passage for the browser"
        (is (= {:quote "x" :anchor nil} (qm/locate-for-finger u page-ref nil "x" nil))))
      (testing "7.2: an Ask about one pin marks it seen and leaves it open"
        (is (nil? (seen-at (:id on-page))))
        (let [{:keys [text map-id]} (qm/pin-question-message u (:id on-page))]
          (is (= (:map-id (qm/snapshot-view u page-ref)) map-id))
          (is (str/includes? text "> a passage"))
          (is (str/ends-with? text "Why?")))
        (is (some? (seen-at (:id on-page))))
        (is (= "open" (:status (first (filter #(= (:id on-page) (:id %))
                                        (:pins (qm/snapshot-view u page-ref))))))))
      (testing "4.2: a pin with no question asks for an explanation"
        (is (str/ends-with? (:text (qm/pin-question-message u (:id merged)))
              "Explain this passage."))))))

(deftest another-users-pin-cannot-be-asked
  (with-users [u other]
    (let [doc (topic! u "web" nil)
          pin (:ok (qm/create-pin! u (docref/for-topic u doc) {:quote "q"}))]
      (is (:error (qm/pin-question-message other (:id pin))))
      (is (nil? (seen-at (:id pin)))))))

(deftest a-document-map-is-exported
  (with-users [u]
    (let [doc (topic! u "wikipedia" nil)]
      (qm/create-pin! u (docref/for-topic u doc) {:quote "q"})
      (testing "1.3.1: the export scope holds the map and its pin"
        (is (= 1 (:n (jdbc/execute-one! db/ds (#'export/count-query :question_maps u) opts))))
        (is (= 1 (:n (jdbc/execute-one! db/ds (#'export/count-query :question_pins u) opts)))))
      (testing "1.3.3: the import remaps the map's topic"
        (is (= :topics (get-in @#'freememo.account-import/fk-columns [:question_maps :topic_id])))))))
