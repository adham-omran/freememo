(ns freememo.assistant-context-usage-test
  "The composer's context meter without a database
   (plans/assistant-context-meter-and-slash-menu.md §1). Every read is stubbed,
   except in the one query test, which skips when PostgreSQL is down."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.assistant :as assistant]
            [freememo.assistant-context-usage :as usage]
            [freememo.db :as db]
            [freememo.model-context :as model-context]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(deftest used-tokens-counts-the-request-and-the-visible-answer
  (is (= 102641 (usage/used-tokens {:input_tokens 101641 :output_tokens 1500
                                    :reasoning_tokens 500})))
  (testing "a missing output or reasoning count reads as 0"
    (is (= 10074 (usage/used-tokens {:input_tokens 10074})))
    (is (= 10100 (usage/used-tokens {:input_tokens 10074 :output_tokens 26
                                     :reasoning_tokens nil}))))
  (testing "no input count, no number"
    (is (nil? (usage/used-tokens {:output_tokens 40})))
    (is (nil? (usage/used-tokens nil)))))

(deftest count-text
  (is (= ["0" "999" "1k" "1k" "1.1k" "10.1k" "101.6k" "999.9k" "1M" "1M" "1.2M"]
        (mapv usage/count-text
          [0 999 1000 1049 1050 10074 101641 999949 999950 1048576 1200000]))))

(deftest usage-text
  (is (= "101.6k / 1M tokens" (usage/usage-text {:used 101641 :total 1048576})))
  (testing "an unknown total is left out"
    (is (= "10.1k tokens" (usage/usage-text {:used 10074 :total nil})))))

(deftest usage-text-uses-no-middle-dot
  (is (not (re-find #"·" (str (usage/usage-text {:used 1 :total 2})
                             usage/usage-tooltip)))))

(deftest context-usage*-reads-the-newest-answer-and-the-selected-model
  (let [!asked (atom nil)]
    (with-redefs [db/get-latest-assistant-answer-tokens
                  (fn [user-id chat-id]
                    (reset! !asked [user-id chat-id])
                    {:input_tokens 13349 :output_tokens 10 :reasoning_tokens 5})
                  assistant/model-entry
                  (fn [_ _] {:id "gemini-3.8-flash"
                             :openrouter-model "google/gemini-3.8-flash"})
                  model-context/context-length
                  {"google/gemini-3.8-flash" 1048576}]
      (is (= {:used 13354 :total 1048576} (assistant/context-usage* 0 8 133)))
      (is (= [8 133] @!asked))))
  (testing "nil before the chat has an answer with a count"
    (with-redefs [db/get-latest-assistant-answer-tokens (constantly nil)]
      (is (nil? (assistant/context-usage* 0 8 133)))))
  (testing "nil with no chat open, and no query"
    (with-redefs [db/get-latest-assistant-answer-tokens
                  (fn [& _] (throw (ex-info "queried" {})))]
      (is (nil? (assistant/context-usage* 0 8 nil)))))
  (testing "a total the catalog does not hold is nil"
    (with-redefs [db/get-latest-assistant-answer-tokens
                  (constantly {:input_tokens 100})
                  assistant/model-entry (fn [_ _] {:openrouter-model "x/unknown"})
                  model-context/context-length (constantly nil)]
      (is (= {:used 100 :total nil} (assistant/context-usage* 0 8 1))))))

;; ── The query, against PostgreSQL ──────────────────────────────────────────

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- finished-run!
  "Create a run with `create!`, record `tokens` on it, and complete it, so the
   chat can take the next run."
  [create! tokens]
  (let [run-uuid (str (random-uuid))]
    (is (:created (create! run-uuid)))
    (db/update-assistant-run-metadata! run-uuid tokens)
    (jdbc/execute! db/ds ["UPDATE assistant_runs SET status = 'completed' WHERE run_uuid = ?"
                          run-uuid])
    run-uuid))

(deftest the-newest-answer-run-with-a-count-answers-and-a-compact-run-never
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [user-id (:users/id (jdbc/execute-one! db/ds
                               ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                                (str "context-usage-test-" (random-uuid))
                                (str (random-uuid) "@example.test")]))
          other-id (:users/id (jdbc/execute-one! db/ds
                                ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                                 (str "context-usage-test-" (random-uuid))
                                 (str (random-uuid) "@example.test")]))]
      (try
        (let [topic-id (:topics/id (jdbc/execute-one! db/ds
                                     ["INSERT INTO topics (user_id, kind, title) VALUES (?, 'basic', 'Usage') RETURNING id"
                                      user-id]))
              chat-id (db/create-assistant-chat! user-id topic-id "Usage" "tutor")
              session (:provider_session_id
                        (jdbc/execute-one! db/ds
                          ["SELECT provider_session_id FROM assistant_chats WHERE id = ?" chat-id]
                          {:builder-fn rs/as-unqualified-maps}))
              run (fn [run-uuid] {:run-uuid run-uuid :provider-session-id session
                                  :requested-model "m" :request-snapshot {}
                                  :source-snapshot []})
              answer! (fn [tokens]
                        (finished-run!
                          #(db/create-assistant-answer-run! chat-id "Q" nil (run %))
                          tokens))]
          (is (nil? (db/get-latest-assistant-answer-tokens user-id chat-id))
            "no run yet")
          (answer! {:input-tokens 900 :output-tokens 50 :reasoning-tokens 10})
          (answer! {:input-tokens 1200 :output-tokens 80 :reasoning-tokens 20})
          (finished-run! #(db/create-assistant-compact-run! chat-id (run %))
            {:input-tokens 99 :output-tokens 9})
          (answer! {})
          (is (= {:input_tokens 1200 :output_tokens 80 :reasoning_tokens 20}
                (db/get-latest-assistant-answer-tokens user-id chat-id))
            "the compact run and the answer with no count are passed over")
          (is (nil? (db/get-latest-assistant-answer-tokens other-id chat-id))
            "another user reads nothing"))
        (finally
          (jdbc/execute! db/ds ["DELETE FROM users WHERE id IN (?, ?)" user-id other-id]))))))
