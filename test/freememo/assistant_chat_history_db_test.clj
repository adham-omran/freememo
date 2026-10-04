(ns freememo.assistant-chat-history-db-test
  "Chat history contracts against PostgreSQL (plans/assistant-chat-history.md
   §7.3): the staged filter, rename, the delete stage and its undo, the purge,
   the failed first send, the boot cleanup, the prune exemption and the export
   scope. Every test skips when the database is down."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.account-export :as account-export]
            [freememo.assistant :as assistant]
            [freememo.db :as db]
            [freememo.settings :as settings]
            [freememo.undo :as undo]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})

(defn- create-user! []
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
          (str "chat-history-test-" (random-uuid))
          (str (random-uuid) "@example.test")]
         opts)))

(defn- create-topic! [user-id]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, kind, title) VALUES (?, 'basic', ?) RETURNING id"
          user-id "Chat history test"]
         opts)))

(defn- chat-with-run!
  "A chat holding one turn whose answer run is still active."
  [user-id topic-id]
  (let [chat-id (db/create-assistant-chat! user-id topic-id "A chat" "socratic")
        session-id (:assistant_chats/provider_session_id
                     (db/get-assistant-chat user-id chat-id))
        run-uuid (str (random-uuid))
        created (db/create-assistant-answer-run!
                  chat-id "Explain it" nil
                  {:run-uuid run-uuid :provider-session-id session-id
                   :requested-model "test-model"
                   :request-snapshot {:messages []} :source-snapshot []
                   :evidence []})]
    {:chat-id chat-id :run-uuid run-uuid :session-id session-id
     :assistant-message-id (:assistant-message-id created)}))

(defn- chat-row [chat-id]
  (jdbc/execute-one! db/ds ["SELECT * FROM assistant_chats WHERE id = ?" chat-id] opts))

(defmacro ^:private with-users
  "Bind `syms` to fresh users and delete them afterwards, or skip with no DB."
  [syms & body]
  `(if-not (db-up?)
     (is true "PostgreSQL is unavailable; integration assertion skipped")
     (let [~@(mapcat (fn [s] [s `(create-user!)]) syms)]
       (try ~@body
         (finally
           (jdbc/execute! db/ds (into [(str "DELETE FROM users WHERE id IN ("
                                         (clojure.string/join "," (repeat ~(count syms) "?"))
                                         ")")]
                                  [~@syms])))))))

(deftest a-staged-chat-is-invisible-to-every-chat-read
  (with-users [owner]
    (let [topic-id (create-topic! owner)
          {:keys [chat-id session-id]} (chat-with-run! owner topic-id)]
      (db/stop-assistant-run! owner chat-id)
      (is (:success (assistant/stage-chat-deletion! owner chat-id)))
      (testing "2.1.1: the ownership gate and the list"
        (is (nil? (db/get-assistant-chat owner chat-id)))
        (is (nil? (assistant/messages* nil owner chat-id)))
        (is (false? (:success (assistant/set-chat-mode! owner chat-id "general"))))
        (is (= "Chat not found."
               (:error (assistant/start-answer-run! owner chat-id nil "Hi" [] nil))))
        (is (empty? (db/get-assistant-chats owner topic-id)))
        (is (= [chat-id false] (assistant/chat-liveness* nil owner chat-id))))
      (testing "2.1.2: run creation locks only a live chat"
        (is (= {:created false :chat-missing? true}
               (db/create-assistant-answer-run!
                 chat-id "Again" nil
                 {:run-uuid (str (random-uuid)) :provider-session-id session-id
                  :requested-model "m" :request-snapshot {} :source-snapshot []
                  :evidence []})))
        (is (= {:created false :chat-missing? true}
               (db/create-assistant-compact-run!
                 chat-id {:run-uuid (str (random-uuid)) :provider-session-id session-id
                          :requested-model "m" :request-snapshot {}
                          :source-snapshot []})))))))

(deftest rename-is-trimmed-capped-and-owner-only
  (with-users [owner stranger]
    (let [topic-id (create-topic! owner)
          {:keys [chat-id]} (chat-with-run! owner topic-id)
          title #(:title (chat-row chat-id))]
      (is (:success (assistant/rename-chat! owner chat-id "  Sorting  ")))
      (is (= "Sorting" (title)))
      (testing "a blank title changes nothing"
        (is (= {:success true :changed? false} (assistant/rename-chat! owner chat-id "   ")))
        (is (= "Sorting" (title))))
      (testing "2.2.2: 300 characters are stored as 200"
        (assistant/rename-chat! owner chat-id (apply str (repeat 300 "x")))
        (is (= 200 (count (title)))))
      (testing "2.2.3: another user's rename changes no row"
        (is (false? (:success (assistant/rename-chat! stranger chat-id "Mine"))))
        (is (= 200 (count (title)))))
      (testing "2.3: pinning sets pinned_at and leaves updated_at alone"
        (let [before (:updated_at (chat-row chat-id))]
          (is (:success (assistant/set-chat-pinned! owner chat-id true)))
          (is (some? (:pinned_at (chat-row chat-id))))
          (is (= before (:updated_at (chat-row chat-id))))
          (is (some? (:pinned_ms (first (db/get-assistant-chats owner topic-id)))))
          (assistant/set-chat-pinned! owner chat-id false)
          (is (nil? (:pinned_at (chat-row chat-id))))))
      (testing "2.4 and 4.2: the opened chat is the seed"
        (is (nil? (assistant/last-opened-chat-id* owner topic-id)))
        (assistant/mark-chat-opened! owner chat-id)
        (is (= chat-id (assistant/last-opened-chat-id* owner topic-id)))))))

(deftest delete-stops-the-run-and-undo-restores-the-chat
  (with-users [owner]
    (let [topic-id (create-topic! owner)
          {:keys [chat-id run-uuid assistant-message-id]} (chat-with-run! owner topic-id)]
      (is (= "Partial" (db/append-assistant-run-text! run-uuid "Partial")))
      (testing "2.5.2: one transaction stops the run and stages the chat"
        (is (:success (assistant/stage-chat-deletion! owner chat-id)))
        (is (= "stopped" (:status (db/get-assistant-run owner run-uuid))))
        (is (nil? (db/append-assistant-run-text! run-uuid " late")))
        (is (= "Partial" (:content (db/get-assistant-message owner assistant-message-id))))
        (is (some? (:staged_delete_id (chat-row chat-id))))
        (is (false? (:success (assistant/stage-chat-deletion! owner chat-id)))
            "a second delete of the same chat finds nothing"))
      (testing "1.5: a full prune keeps the staged entry, so the chat stays hidden"
        (dotimes [_ 101]
          (db/insert-undo-entry! owner "remove-pin" "pin" [0] []))
        (is (some? (:staged_delete_id (chat-row chat-id)))))
      (testing "2.6: undo clears the marker; the stopped run stays stopped"
        (let [entry-id (:staged_delete_id (chat-row chat-id))]
          (is (:success (undo/undo-entry! owner entry-id)))
          (is (some? (db/get-assistant-chat owner chat-id)))
          (is (= 2 (count (db/get-assistant-messages chat-id))))
          (is (= "stopped" (:status (db/get-assistant-run owner run-uuid)))))))))

(deftest undo-after-the-cascade-fails-and-stays-live
  (with-users [owner]
    (let [topic-id (create-topic! owner)
          {:keys [chat-id]} (chat-with-run! owner topic-id)
          _ (db/stop-assistant-run! owner chat-id)
          _ (assistant/stage-chat-deletion! owner chat-id)
          entry-id (:staged_delete_id (chat-row chat-id))]
      (jdbc/execute! db/ds ["DELETE FROM topics WHERE id = ?" topic-id])
      (is (= {:success false :error "Undo failed"} (undo/undo-entry! owner entry-id)))
      (is (nil? (:undone_at (db/get-undo-entry entry-id)))))))

(deftest the-purge-removes-the-chat-and-every-child-row
  (with-users [owner]
    (let [topic-id (create-topic! owner)
          {:keys [chat-id]} (chat-with-run! owner topic-id)
          _ (assistant/stage-chat-deletion! owner chat-id)
          entry-id (:staged_delete_id (chat-row chat-id))
          count-of (fn [sql] (:n (jdbc/execute-one! db/ds [sql chat-id] opts)))]
      (jdbc/execute! db/ds ["UPDATE undo_log SET occurred_at = now() - interval '13 hours'
                              WHERE id = ?" entry-id])
      (is (pos? (db/purge-staged-assistant-chats!)))
      (is (zero? (count-of "SELECT count(*) n FROM assistant_chats WHERE id = ?")))
      (is (zero? (count-of "SELECT count(*) n FROM assistant_messages WHERE chat_id = ?")))
      (is (zero? (count-of "SELECT count(*) n FROM assistant_runs WHERE chat_id = ?")))
      (is (zero? (count-of "SELECT count(*) n FROM assistant_checkpoints WHERE chat_id = ?")))
      (is (zero? (count-of (str "SELECT count(*) n FROM assistant_evidence e"
                             " JOIN assistant_runs r ON r.id = e.run_id WHERE r.chat_id = ?"))))
      (is (nil? (db/get-undo-entry entry-id))))))

(deftest a-failed-first-send-leaves-no-chat
  (with-users [owner]
    (let [topic-id (create-topic! owner)
          chat-count #(count (jdbc/execute! db/ds
                               ["SELECT id FROM assistant_chats WHERE user_id = ?" owner]))]
      (testing "2.9.1: an error result"
        (with-redefs [settings/get-openrouter-api-key (constantly nil)]
          (let [result (assistant/ensure-and-start-answer-run!
                         owner topic-id nil nil "socratic" false "What is a heap?" [] "c1")]
            (is (= "OpenRouter API key not configured." (:error result)))
            (is (nil? (:chat-id result)))))
        (is (zero? (chat-count))))
      (testing "2.9.1: a throw"
        (with-redefs [assistant/start-answer-run!
                      (fn [& _] (throw (ex-info "boom" {})))]
          (is (thrown? Exception
                (assistant/ensure-and-start-answer-run!
                  owner topic-id nil nil "socratic" false "What is a heap?" [] "c2"))))
        (is (zero? (chat-count)))))))

(deftest boot-cleanup-deletes-only-messageless-chats
  (with-users [owner]
    (let [topic-id (create-topic! owner)
          empty-id (db/create-assistant-chat! owner topic-id "New chat" "socratic")
          {:keys [chat-id]} (chat-with-run! owner topic-id)]
      (db/delete-messageless-assistant-chats!)
      (is (nil? (chat-row empty-id)))
      (is (some? (chat-row chat-id))))))

(deftest the-export-leaves-a-staged-chat-behind
  (with-users [owner]
    (let [topic-id (create-topic! owner)
          live (chat-with-run! owner topic-id)
          staged (chat-with-run! owner topic-id)]
      (db/stop-assistant-run! owner (:chat-id staged))
      (assistant/stage-chat-deletion! owner (:chat-id staged))
      (let [counts (account-export/entity-counts db/ds owner)]
        (testing "6.1: every assistant table reads through the live chats only"
          (is (= 1 (:assistant_chats counts)))
          (is (= 2 (:assistant_messages counts)))
          (is (= 1 (:assistant_runs counts)))))
      (is (some? (:chat-id live))))))
