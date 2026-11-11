(ns freememo.assistant-desktop-id-order-test
  "A chat that desktop sync uploaded holds rows with ids of 2^40 or more
   (`sync-schema/client-id-floor`). A turn the server adds later takes a
   sequence id below 2^40. Every reader must still show the turns in the order
   they were written: the stored transcript, the panel timeline, the model
   request, a fork's inherited rows and the token meter. Every test skips when
   the database is down."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.assistant :as assistant]
            [freememo.assistant-history :as history]
            [freememo.db :as db]
            [freememo.sync-schema :as sync-schema]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})

(defn- create-user! []
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
          (str "desktop-id-order-" (random-uuid)) (str (random-uuid) "@example.test")]
         opts)))

(defn- create-topic! [user-id]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, kind, title) VALUES (?, 'basic', ?) RETURNING id"
          user-id "Desktop id order test"]
         opts)))

(defn- insert-run!
  "An answer run for the turn pair, as either peer stores it."
  [id chat-id user-message-id answer-message-id input-tokens]
  (jdbc/execute! db/ds
    ["INSERT INTO assistant_runs (id, run_uuid, chat_id, kind, status, user_message_id,
                                  assistant_message_id, provider_session_id, input_tokens)
      VALUES (coalesce(?, nextval('assistant_runs_id_seq')), ?, ?, 'answer', 'completed',
              ?, ?, 'test', ?)"
     id (str (random-uuid)) chat-id user-message-id answer-message-id input-tokens]))

(defn- insert-desktop-chat!
  "A chat and one turn pair as a sync upload stores them: ids from a desktop
   block, timestamps from an hour ago. Returns the chat id."
  [user-id topic-id block-start]
  (jdbc/with-transaction [tx db/ds]
    (jdbc/execute! tx
      ["INSERT INTO assistant_chats (id, user_id, root_topic_id, title, mode,
                                     provider_session_id, created_at)
        VALUES (?, ?, ?, 'Desktop chat', 'socratic', ?, now() - interval '1 hour')"
       block-start user-id topic-id (str (random-uuid))])
    (jdbc/execute! tx
      ["INSERT INTO assistant_messages (id, chat_id, role, content, created_at)
        VALUES (?, ?, 'user', 'desktop question', now() - interval '1 hour'),
               (?, ?, 'assistant', 'desktop answer', now() - interval '59 minutes')"
       (+ block-start 1) block-start (+ block-start 2) block-start]))
  (insert-run! (+ block-start 3) block-start (+ block-start 1) (+ block-start 2) 100)
  block-start)

(defn- contents [rows]
  (mapv :assistant_messages/content rows))

(defn- timeline-contents
  "The message texts of the panel timeline, in its order. A fork event shows
   as :fork."
  [user-id chat-id]
  (let [events (history/transcript-index user-id chat-id)
        bodies (into {} (map (juxt :id :content))
                 (history/message-rows user-id (keep :message_id events)))]
    (into [] (keep (fn [{:keys [kind message_id]}]
                     (case (long kind) 0 (bodies message_id) 2 :fork nil)))
      events)))

(defn- request-contents [user-id chat-id]
  (mapv :content (:messages (#'assistant/request-transcript user-id
                              (db/get-assistant-chat user-id chat-id)))))

(deftest a-server-turn-after-desktop-turns-comes-last
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [user-id (create-user!)
          topic-id (create-topic! user-id)
          ;; A block per run, so a run that dies before cleanup leaves no clash.
          block-start (+ sync-schema/client-id-floor
                        (* sync-schema/block-size (+ 1000 (rand-int 1000000))))]
      (try
        (let [chat-id (insert-desktop-chat! user-id topic-id block-start)
              question (db/insert-assistant-message! chat-id "user" "server question" nil)
              answer (db/insert-assistant-message! chat-id "assistant" "server answer" nil)
              expected ["desktop question" "desktop answer" "server question" "server answer"]]
          (insert-run! nil chat-id question answer 200)
          (testing "the stored transcript is oldest first"
            (is (= expected (contents (db/get-assistant-messages chat-id)))))
          (testing "the panel timeline is oldest first"
            (is (= expected (timeline-contents user-id chat-id))))
          (testing "the model request is oldest first"
            (is (= expected (request-contents user-id chat-id))))
          (testing "the token meter reads the newest answer"
            (is (= 200 (:input_tokens (db/get-latest-assistant-answer-tokens user-id chat-id))))))
        (finally
          (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" user-id]))))))

(deftest a-server-fork-of-a-desktop-chat-inherits-its-turns
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [user-id (create-user!)
          topic-id (create-topic! user-id)
          block-start (+ sync-schema/client-id-floor
                        (* sync-schema/block-size (+ 1000 (rand-int 1000000))))]
      (try
        (let [parent-id (insert-desktop-chat! user-id topic-id block-start)
              ;; The fork is at the desktop answer, whose id is far above any
              ;; id the server gives the child's own turns.
              child-id (db/create-assistant-fork! user-id parent-id (+ block-start 2) "Fork")
              fork-question (db/insert-assistant-message! child-id "user" "fork question" nil)
              ;; A fork of the fork, at the child's own server turn. Its path
              ;; crosses two chats whose ids are on both sides of 2^40.
              grandchild-id (db/create-assistant-fork! user-id child-id fork-question "Fork 2")]
          (db/insert-assistant-message! grandchild-id "user" "second fork question" nil)
          (testing "the panel shows the inherited turns, the fork, then the own turn"
            (is (= ["desktop question" "desktop answer" :fork "fork question"]
                   (timeline-contents user-id child-id)))
            (is (= ["desktop question" "desktop answer" "fork question" :fork
                    "second fork question"]
                   (timeline-contents user-id grandchild-id))))
          (testing "the model request carries the inherited turns before the own turn"
            (is (= ["desktop question" "desktop answer" "fork question"]
                   (request-contents user-id child-id)))
            (is (= ["desktop question" "desktop answer" "fork question" "second fork question"]
                   (request-contents user-id grandchild-id)))))
        (finally
          (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" user-id]))))))
