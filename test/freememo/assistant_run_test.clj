(ns freememo.assistant-run-test
  "Durable assistant-run transition and ownership contracts."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.assistant :as assistant]
            [freememo.assistant-evidence :as evidence]
            [freememo.assistant-panel :as panel]
            [freememo.assistant-panel-data :as panel-data]
            [freememo.card-modals :as card-modals]
            [freememo.db :as db]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- create-user! []
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
          (str "assistant-run-test-" (random-uuid))
          (str (random-uuid) "@example.test")]
         {:builder-fn rs/as-unqualified-maps})))

(defn- create-topic! [user-id]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, kind, title) VALUES (?, 'basic', ?) RETURNING id"
          user-id "Assistant run test"]
         {:builder-fn rs/as-unqualified-maps})))

(defn- answer-run [chat-id run-uuid session-id topic-id]
  (db/create-assistant-answer-run!
    chat-id "Explain it" "client-1"
    {:run-uuid run-uuid
     :provider-session-id session-id
     :requested-model "test-model"
     :request-snapshot {:model "provider/test" :messages []
                        :session_id session-id :stream true}
     :source-snapshot [{:citation-key "E1" :document-topic-id topic-id}]
     :evidence [{:citation-key "E1"
                 :document-topic-id topic-id
                 :document-title "Assistant run test"
                 :display-label "document-level evidence"}]}))

(def ^:private transcript-after-checkpoint
  #'assistant/transcript-messages-after-checkpoint)

(deftest stop-closes-text-but-keeps-late-metadata
  (let [owner (create-user!)
        stranger (create-user!)]
    (try
      (let [topic-id (create-topic! owner)
            chat-id (db/create-assistant-chat! owner topic-id "Test" "socratic")
            session-id (:assistant_chats/provider_session_id
                         (db/get-assistant-chat owner chat-id))
            run-uuid (str (random-uuid))
            created (answer-run chat-id run-uuid session-id topic-id)]
        (testing "web search defaults off and stays chat-owned"
          (is (false? (:assistant_chats/web_search
                        (db/get-assistant-chat owner chat-id))))
          (db/set-assistant-chat-web-search! owner chat-id true)
          (is (true? (:assistant_chats/web_search
                      (db/get-assistant-chat owner chat-id))))
          (db/set-assistant-chat-web-search! stranger chat-id false)
          (is (true? (:assistant_chats/web_search
                      (db/get-assistant-chat owner chat-id)))))

        (testing "one chat accepts one active operation"
          (is (:created created))
          (is (= {:created false :active-run-uuid run-uuid}
                (answer-run chat-id (str (random-uuid)) session-id topic-id)))
          (is (= 2 (count (db/get-assistant-messages chat-id)))))

        (testing "Stop preserves the last stored chunk and rejects later text"
          (is (= "Hello" (db/append-assistant-run-text! run-uuid "Hello")))
          (is (= "Hello " (db/append-assistant-run-text! run-uuid " ")))
          (is (:stopped (db/stop-assistant-run! owner chat-id)))
          (is (nil? (db/append-assistant-run-text! run-uuid "late")))
          (is (false? (db/complete-assistant-answer-run! run-uuid)))
          (is (= "Hello "
                (:content (db/get-assistant-message owner
                            (:assistant-message-id created))))))

        (testing "late provider facts update a stopped run"
          (db/update-assistant-run-metadata! run-uuid
            {:generation-id "gen-late" :input-tokens 12 :output-tokens 3
             :cost-usd 0.0015})
          (let [run (db/get-assistant-run owner run-uuid)]
            (is (= "stopped" (:status run)))
            (is (= "gen-late" (:generation_id run)))
            (is (= 12 (:input_tokens run)))
            (is (= 0.0015 (double (:cost_usd run)))))
          (is (nil? (db/get-assistant-run stranger run-uuid)))
          (is (empty? (db/get-assistant-run-evidence stranger run-uuid))))

        (testing "a compact run publishes one retained checkpoint"
          (let [message-ids (mapv :assistant_messages/id
                              (db/get-assistant-messages chat-id))
                compact-uuid (str (random-uuid))
                compact (db/create-assistant-compact-run!
                          chat-id {:run-uuid compact-uuid
                                   :provider-session-id session-id
                                   :requested-model "test-model"
                                   :request-snapshot {:messages []}
                                   :source-snapshot []})]
            (is (:created compact))
            (is (db/complete-assistant-compact-run! compact-uuid "Summary"
                  (first message-ids) (last message-ids)))
            (is (= "Summary" (:summary
                               (db/latest-assistant-checkpoint owner chat-id))))
            (let [checkpoint (db/latest-assistant-checkpoint owner chat-id)]
              (is (= [] (transcript-after-checkpoint chat-id checkpoint)))
              (db/insert-assistant-message! chat-id "user" "Later" nil)
              (db/insert-assistant-message! chat-id "assistant" "New answer" nil)
              (is (= [{:role "user" :content "Later"}
                      {:role "assistant" :content "New answer"}]
                    (transcript-after-checkpoint chat-id checkpoint))))
            (let [timeline (assistant/messages* nil owner chat-id)]
              (is (= 5 (count timeline)))
              (is (= "event" (:assistant_messages/role (nth timeline 2)))))))

        (testing "chat deletion cascades through all run records"
          (db/delete-assistant-chat! owner chat-id)
          (is (nil? (db/get-assistant-run owner run-uuid)))
          (is (empty? (db/get-assistant-checkpoints owner chat-id)))))
      (finally
        (jdbc/execute! db/ds ["DELETE FROM users WHERE id IN (?, ?)" owner stranger])))))

(deftest citations-render-only-retained-evidence
  (is (= "Claim [document-level evidence](/viewer/topic/7/3). Missing ."
        (assistant/render-evidence-citations
          "Claim [evidence:E1]. Missing [evidence:E9]."
          [{:citation_key "E1" :document_topic_id 7 :page_number 3
            :display_label "document-level evidence"}]))))

(deftest add-card-draft-transitions-keep-the-raw-fields
  (let [!kind (atom "basic")
        !primary (atom "Question")
        !answer (atom "Answer")
        !ol-question (atom "")
        !ol-items (atom "")
        !before (atom "3")
        !reveal (atom false)
        !revision (atom 0)]
    (card-modals/switch-add-card-kind!
      !kind !primary !answer !ol-question !ol-items "overlapping")
    (is (= "overlapping" @!kind))
    (is (= "Question" @!ol-question))
    (is (= "Answer" @!ol-items))

    (reset! !ol-question "Changed question")
    (reset! !ol-items "Changed answer")
    (card-modals/switch-add-card-kind!
      !kind !primary !answer !ol-question !ol-items "basic")
    (is (= "Changed question" @!primary))
    (is (= "Changed answer" @!answer))

    (card-modals/flip-basic-card-fields! !primary !answer !revision)
    (is (= "Changed answer" @!primary))
    (is (= "Changed question" @!answer))

    (card-modals/clear-add-card-draft!
      :front "basic" !primary !answer !ol-question !ol-items !before !reveal)
    (is (= "Changed answer" @!primary))
    (is (= "" @!answer))
    (is (= "1" @!before))
    (is (true? @!reveal))))

(deftest document-mentions-use-the-message-draft
  (is (= "front" (panel/mention-query "Compare this with @front")))
  (is (= "" (panel/mention-query "@")))
  (is (nil? (panel/mention-query "Use @front next")))
  (is (panel/active-document-mention? "Compare this with @front"))
  (is (not (panel/active-document-mention? "Compare this with front"))))

(deftest a-picked-reference-stays-in-the-message-text
  (testing "the mention the learner typed is completed, not deleted"
    (is (= "Compare this with @Frontend Design "
          (panel-data/draft-with-reference "Compare this with @front"
            "Frontend Design")))
    (is (= "@Frontend Design "
          (panel-data/draft-with-reference "@" "Frontend Design")))
    (is (= "No mention yet @Frontend Design "
          (panel-data/draft-with-reference "No mention yet " "Frontend Design"))))
  (testing "a title carrying regex replacement syntax is inserted verbatim"
    (is (= "read @Cost $1 \\ notes "
          (panel-data/draft-with-reference "read @c" "Cost $1 \\ notes"))))
  (testing "removing the chip removes the text that named it"
    (is (= "Compare this with "
          (panel-data/draft-without-reference
            "Compare this with @Frontend Design " "Frontend Design")))
    (is (= "@Zigzag is enough"
          (panel-data/draft-without-reference "@Zigzag is enough" "Zig"))
      "a mention must end at a word boundary before it is removed")
    (is (= "nothing to remove"
          (panel-data/draft-without-reference "nothing to remove" "Zig")))
    (is (= "" (panel-data/draft-without-reference "@Zig" "Zig")))))

(deftest document-mentions-filter-on-the-server
  (with-redefs [db/get-root-topics
                (fn [_]
                  [{:topics/id 9 :topics/title "Open document"}
                   {:topics/id 2 :topics/title "Frontend Design"}
                   {:topics/id 1 :topics/title "Backend Notes"}])]
    (is (= [{:id 2 :title "Frontend Design"}]
          (assistant/referenceable-docs "learner" 9 "FRONT")))
    (is (= [{:id 2 :title "Frontend Design"}
            {:id 1 :title "Backend Notes"}]
          (assistant/referenceable-docs "learner" 9 "")))))

(deftest citation-target-resolves-retained-evidence
  (let [source {:citation_key "E1" :document_topic_id 7 :page_number 3
                :display_label "Source excerpt 1"
                :quoted_text "Retained text"}
        same-target {:citation_key "E2" :document_topic_id 7 :page_number 3
                     :display_label "Source excerpt 2"
                     :quoted_text "Different retained text"}]
    (is (= "/viewer/topic/7/3" (evidence/citation-target source)))
    (is (= source (evidence/source-for-target [source] "/viewer/topic/7/3")))
    (is (= same-target
          (evidence/source-for-citation [source same-target]
            {:label "Source excerpt 2" :target "/viewer/topic/7/3"})))
    (is (nil? (evidence/source-for-target [source] "/viewer/topic/8")))
    (is (false? (evidence/other-document? source "7")))
    (is (true? (evidence/other-document? source 8)))))

(deftest failed-run-retry-reuses-the-exact-snapshot
  (let [started (atom nil)
        request {:model "provider/model" :messages [{:role "user" :content "Exact"}]}
        source {:citation-key "E1" :snapshot-content "Retained text"}]
    (with-redefs [db/get-assistant-run
                  (fn [_ _]
                    {:kind "answer" :status "failed" :chat_id 4
                     :user_message_id 8 :requested_model "model-id"
                     :request_snapshot request :source_snapshot [source]})
                  db/get-assistant-message (fn [_ _] {:content "Try this"})
                  db/get-assistant-run-evidence
                  (fn [_ _]
                    [{:citation_key "E1" :document_topic_id 7
                      :document_title "Doc"
                      :display_label "document-level evidence"}])
                  assistant/start-answer-run!
                  (fn [& args] (reset! started args) {:ok true :run-uuid "new"})]
      (is (:ok (assistant/retry-answer-run! 1 "failed" "client-2")))
      (is (= request (nth @started 7)))
      (is (= "failed" (nth @started 6)))
      (is (= "model-id" (nth @started 9)))
      (is (= "Retained text" (:snapshot-content (first (nth @started 8))))))))
