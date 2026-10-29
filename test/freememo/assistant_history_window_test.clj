(ns freememo.assistant-history-window-test
  "Bounded transcript queries against PostgreSQL, including inherited forks."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [freememo.assistant :as assistant]
            [freememo.assistant-history :as history]
            [freememo.db :as db]
            [freememo.onboarding-progress :as onboarding]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- one [sql & params]
  (jdbc/execute-one! db/ds (into [sql] params) {:builder-fn rs/as-unqualified-maps}))

(defn- fixture! []
  (let [tag (str "audit-assistant-" (random-uuid))
        uid (:id (one "INSERT INTO users(username,email) VALUES (?,?) RETURNING id" tag (str tag "@example.test")))
        root (:id (one "INSERT INTO topics(user_id,kind,title) VALUES (?,'basic','History test') RETURNING id" uid))
        chat (db/create-assistant-chat! uid root "Owned" "socratic")]
    {:uid uid :root root :chat chat}))

(defn- seed-messages! [chat n]
  (jdbc/with-transaction [tx db/ds]
    (jdbc/execute! tx ["SET LOCAL session_replication_role = replica"])
    (jdbc/execute! tx
      ["INSERT INTO assistant_messages(chat_id,role,content)
         SELECT ?, CASE WHEN i % 2 = 0 THEN 'assistant' ELSE 'user' END, 'Message ' || i
           FROM generate_series(1, ?) i" chat n])
    (jdbc/execute! tx ["SET LOCAL session_replication_role = origin"])))

(defn- cleanup! [uid]
  (one "DELETE FROM users WHERE id = ?" uid))

(deftest recent-and-older-pages-have-fixed-size-and-no-gaps
  (doseq [n [100 1000 10000]]
    (let [{:keys [uid root chat]} (fixture!)]
      (try
        (seed-messages! chat n)
        (let [page (assistant/transcript-page* 0 uid chat nil)
              older (assistant/transcript-page* 0 uid chat (:cursor page))
              all-ids (mapv :assistant_messages/id (db/get-assistant-messages chat))]
          (is (= 50 (count (:rows page))))
          (is (= 50 (count (:rows older))))
          (is (= (vec (take-last 50 all-ids)) (mapv :assistant_messages/id (:rows page))))
          (is (= (vec (take-last 50 (drop-last 50 all-ids)))
                (mapv :assistant_messages/id (:rows older))))
          (is (empty? (:rows (assistant/transcript-page* 0 (inc uid) chat nil))))
          (is (= (:rows page) (assistant/transcript-rows page {:assistant_messages/id -1})))
          (is (= 1 (count (:rows (history/chat-page uid root "Owned" 0))))))
        (finally (cleanup! uid))))))

(deftest inherited-pages-preserve-the-existing-fork-timeline
  (let [{:keys [uid root chat]} (fixture!)]
    (try
      (seed-messages! chat 75)
      (let [through (:assistant_messages/id (nth (db/get-assistant-messages chat) 59))
            child (:id (one "INSERT INTO assistant_chats(user_id,root_topic_id,title,mode,parent_chat_id,fork_message_id,provider_session_id)
                             VALUES (?,?,'Child','socratic',?,?,?) RETURNING id"
                         uid root chat through (str (random-uuid))))]
        (seed-messages! child 23)
        (let [expected (assistant/messages* 0 uid child)
              page (assistant/transcript-page* 0 uid child nil)
              older (assistant/transcript-page* 0 uid child (:cursor page))
              actual (into (:rows older) (:rows page))]
          (is (= (mapv :assistant_messages/id expected) (mapv :assistant_messages/id actual)))
          (is (= (mapv :assistant_messages/inherited? expected)
                (mapv :assistant_messages/inherited? actual)))
          (is (false? (:more? older)))))
      (finally (cleanup! uid)))))

(deftest chat-and-document-search-reach-options-outside-the-first-page
  (let [{:keys [uid root chat]} (fixture!)]
    (try
      (jdbc/with-transaction [tx db/ds]
        (jdbc/execute! tx ["SET LOCAL session_replication_role = replica"])
        (jdbc/execute! tx
          ["INSERT INTO topics(user_id,kind,title) SELECT ?,'basic','Document ' || i FROM generate_series(1,30000) i" uid])
        (jdbc/execute! tx
          ["INSERT INTO assistant_chats(user_id,root_topic_id,title,mode,provider_session_id)
             SELECT ?,?,'Chat ' || i,'socratic',gen_random_uuid()::text FROM generate_series(1,1000) i" uid root])
        (jdbc/execute! tx ["SET LOCAL session_replication_role = origin"]))
      (is (= 50 (count (history/document-options uid root ""))))
      (is (= ["Document 30000"] (mapv :title (history/document-options uid root "Document 30000"))))
      (is (= 50 (count (:rows (history/chat-page uid root "" 0)))))
      (is (:more? (history/chat-page uid root "" 0)))
      (is (= ["Chat 1000"] (mapv :label (:rows (history/chat-page uid root "Chat 1000" 0)))))
      (is (= chat (:id (history/active-chat uid root chat))))
      (is (nil? (history/active-chat (inc uid) root chat)))
      (finally (cleanup! uid)))))

(deftest onboarding-requires-a-reviewable-card
  (let [{:keys [uid root]} (fixture!)]
    (try
      (is (false? (onboarding/reviewable-cards? uid)))
      (one "INSERT INTO flashcards(topic_id,root_topic_id,kind,question,answer)
             VALUES (?,?,'occlusion','Image','Answer') RETURNING id" root root)
      (is (false? (onboarding/reviewable-cards? uid)))
      (one "INSERT INTO flashcards(topic_id,root_topic_id,kind,question,answer)
             VALUES (?,?,'basic','Question','Answer') RETURNING id" root root)
      (is (true? (onboarding/reviewable-cards? uid)))
      (is (false? (onboarding/reviewable-cards? (inc uid))))
      (finally (cleanup! uid)))))

(deftest stream-reads-only-the-current-answer
  (let [{:keys [uid root chat]} (fixture!)
        original-execute jdbc/execute!
        queries (atom [])]
    (try
      (seed-messages! chat 1000)
      (let [run-id (str (random-uuid))
            session (:assistant_chats/provider_session_id (db/get-assistant-chat uid chat))
            created (db/create-assistant-answer-run!
                      chat "Question" "history-stream-client"
                      {:run-uuid run-id :provider-session-id session :requested-model "test"
                       :request-snapshot {} :source-snapshot [] :evidence []})
            stable (assistant/transcript-page* 0 uid chat nil)]
        (is (:created created))
        (with-redefs [jdbc/execute! (fn [& args]
                                    (let [result (apply original-execute args)]
                                      (swap! queries conj {:sql (first (second args)) :rows (count result)})
                                      result))]
          (dotimes [i 5]
            (db/append-assistant-run-text! run-id (str " token" i))
            (let [live (assistant/live-transcript-row* i uid chat)]
              (is (= (:assistant-message-id created) (:assistant_messages/id live)))
              (is (= 50 (count (assistant/transcript-rows stable live)))))))
        (let [reads (filter #(re-find #"SELECT" (:sql %)) @queries)]
          (is (= 10 (count reads)))
          (is (every? #(<= (:rows %) 1) reads))
          (is (not-any? #(re-find #"WITH RECURSIVE" (:sql %)) reads)))
        (let [terminal {:assistant_messages/id 1 :assistant_run/status "completed"
                        :assistant_messages/content "Final"}
              late {:assistant_messages/id 1 :assistant_run/status "streaming"
                    :assistant_messages/content "Partial"}]
          (is (= [terminal] (assistant/transcript-rows {:rows [terminal]} late)))))
      (finally (cleanup! uid)))))

(deftest active-run-summary-does-not-read-retained-snapshots
  (let [{:keys [uid chat]} (fixture!)
        execute-one! jdbc/execute-one!
        columns (atom [])]
    (try
      (let [run-id (str (random-uuid))
            session (:assistant_chats/provider_session_id (db/get-assistant-chat uid chat))]
        (db/create-assistant-answer-run! chat "Question" "summary-client"
          {:run-uuid run-id :provider-session-id session :requested-model "test"
           :request-snapshot {:retained (apply str (repeat 300000 "x"))}
           :source-snapshot [] :evidence []})
        (with-redefs [jdbc/execute-one!
                      (fn [connection statement & options]
                        (let [row (apply execute-one! connection statement options)]
                          (when (str/includes? (first statement) "FROM assistant_runs r")
                            (swap! columns conj (set (keys row))))
                          row))]
          (is (= {:run_uuid run-id :kind "answer" :status "queued"}
                (assistant/active-run* 0 uid chat))))
        (is (= [#{:run_uuid :kind :status}] @columns))
        (is (nil? (assistant/active-run* 0 (inc uid) chat))))
      (finally (cleanup! uid)))))

(deftest a-submitted-turn-keeps-the-latest-page-after-echo-retirement
  (require 'freememo.assistant-transcript)
  (let [echo (atom {:id "submitted" :text "Question"})
        page (atom {:chat 7 :before [2 300 0 150] :trail [nil]})]
    ((resolve 'freememo.assistant-transcript/retire-echo!) echo page 7 nil)
    (is (nil? @echo))
    (is (= {:chat 7 :before nil :trail []} @page))))

(deftest a-retained-transcript-page-cannot-belong-to-another-request
  (let [page {:scope [7 nil] :rows [{:assistant_messages/id 1}]
              :more? true :cursor [2 2 0 1]}]
    (is (= page (assistant/current-transcript-page page 7 nil)))
    (is (= {:rows [] :more? false :cursor nil :pending? true}
          (assistant/current-transcript-page page 8 nil)))
    (is (:pending? (assistant/current-transcript-page page 7 [2 2 0 1])))
    (is (= (:rows page) (:rows (assistant/current-transcript-page page 7 [2 2 0 1]))))
    (is (nil? (:cursor (assistant/current-transcript-page page 7 [2 2 0 1]))))))

(deftest exact-old-document-title-remains-reachable-among-newer-prefix-matches
  (let [{:keys [uid root]} (fixture!)]
    (try
      (one "INSERT INTO topics(user_id,kind,title) VALUES (?,'markdown','Reference-1') RETURNING id" uid)
      (jdbc/execute! db/ds
        ["INSERT INTO topics(user_id,kind,title)
           SELECT ?,'markdown','Reference-' || n FROM generate_series(100,199) n" uid])
      (let [defaults (history/document-options uid root "")
            matches (history/document-options uid root "Reference-1")]
        (is (not-any? #(= "Reference-1" (:title %)) defaults))
        (is (= 50 (count matches)))
        (is (= "Reference-1" (:title (first matches)))))
      (finally (cleanup! uid)))))

(deftest bounded-branch-navigation-always-includes-the-parent
  (let [{:keys [uid root chat]} (fixture!)]
    (try
      (seed-messages! chat 1)
      (let [parent-message (:id (one "SELECT max(id) AS id FROM assistant_messages WHERE chat_id=?" chat))
            child (:id (one "INSERT INTO assistant_chats(user_id,root_topic_id,title,mode,parent_chat_id,fork_message_id,provider_session_id)
                             VALUES (?,?,'Active child','socratic',?,?,?) RETURNING id"
                         uid root chat parent-message (str (random-uuid))))]
        (seed-messages! child 1)
        (let [child-message (:id (one "SELECT max(id) AS id FROM assistant_messages WHERE chat_id=?" child))]
          (jdbc/execute! db/ds
            ["INSERT INTO assistant_chats(user_id,root_topic_id,title,mode,parent_chat_id,fork_message_id,provider_session_id)
               SELECT ?,?,'Branch ' || n,'socratic',?,?,gen_random_uuid()::text
               FROM generate_series(1,100) n" uid root child child-message]))
        (let [options (history/branch-options uid root child)]
          (is (= 50 (count options)))
          (is (some #(= child (:id %)) options))
          (is (some #(= chat (:id %)) options))))
      (finally (cleanup! uid)))))
