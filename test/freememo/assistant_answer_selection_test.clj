(ns freememo.assistant-answer-selection-test
  "Selection actions in assistant answers: annotations, side questions and
   cards from a selection. plans/assistant-answer-selection-actions.md §5.5.2.
   Runs with the database down: every read is stubbed."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.assistant :as assistant]
            [freememo.assistant-annotations :as annotations]
            [freememo.assistant-answer-selection :as selection]
            [freememo.cards :as cards]
            [freememo.cards-from-facts :as cff]
            [freememo.content-toolbar-helpers :as helpers]
            [freememo.cost-history :as cost-history]
            [freememo.db :as db]
            [freememo.settings :as settings]))

;; ── Annotation messages ────────────────────────────────────────────────────

(def ^:private two
  (annotations/wire-annotations
    [{:quote "  collection --> count\n can grow " :note "Why not fixed?" :start 1 :end 9}
     {:quote "steady ⊑ cont" :note "   "}]))

(deftest the-wire-form-keeps-only-quote-and-note
  (is (= [{:quote "collection --> count can grow" :note "Why not fixed?"}
          {:quote "steady ⊑ cont" :note nil}]
        two)))

(deftest the-user-message-numbers-each-segment-and-appends-the-draft
  (is (= (str "I have questions about these segments of your answers.\n\n"
           "[1] \"collection --> count can grow\"\nWhy not fixed?\n\n"
           "[2] \"steady ⊑ cont\"")
        (annotations/user-message two "  ")))
  (is (str/ends-with? (annotations/user-message two "And one more thing?")
        "[2] \"steady ⊑ cont\"\n\nAnd one more thing?")))

(deftest a-list-that-cannot-be-sent-names-why
  (is (nil? (annotations/problem two)))
  (is (re-find #"no quoted text" (annotations/problem [{:quote " " :note nil}])))
  (is (re-find #"At most" (annotations/problem (vec (repeat 21 {:quote "x"})))))
  (is (re-find #"quotes more"
        (annotations/problem [{:quote (apply str (repeat 4001 "x"))}]))))

;; ── Annotation state ───────────────────────────────────────────────────────

(deftest annotations-belong-to-the-chat-they-were-made-in
  (let [a {:id "a" :quote "one"}
        b {:id "b" :quote "two"}
        state (-> selection/empty-annotations
                (selection/add-annotation 5 a)
                (selection/add-annotation 5 b))]
    (is (= [a b] (selection/visible-annotations state 5)))
    (testing "another chat sees none"
      (is (= [] (selection/visible-annotations state 6))))
    (testing "adding in another chat starts a new list"
      (is (= [a] (selection/visible-annotations
                   (selection/add-annotation state 6 a) 6))))
    (testing "removing one keeps the rest"
      (is (= [b] (selection/visible-annotations
                   (selection/remove-annotation state "a") 5))))))

;; ── Requests ───────────────────────────────────────────────────────────────

(def ^:private answer-request #'assistant/answer-request)
(def ^:private side-request #'assistant/side-request)

(def ^:private chat
  {:assistant_chats/id 128 :assistant_chats/root_topic_id 77
   :assistant_chats/mode "general" :assistant_chats/provider_session_id "s-128"
   :assistant_chats/web_search false})

(defn- with-reads [f]
  (with-redefs [db/get-topic-for-user (fn [_ id] (when (= 77 id)
                                                   {:topics/id 77 :topics/kind "basic"
                                                    :topics/title "Notes"
                                                    :topics/content "<p>The source.</p>"}))
                db/get-topic (fn [id] (when (= 77 id)
                                        {:topics/id 77 :topics/kind "basic"
                                         :topics/title "Notes"
                                         :topics/content "<p>The source.</p>"}))
                db/get-ancestor-ids (fn [_] [77])
                db/get-setting (fn [& _] nil)
                db/get-kg-facts-context (fn [& _] [])
                db/get-fact-mastery (fn [& _] {})
                db/latest-assistant-checkpoint (fn [& _] nil)
                db/get-assistant-messages
                (fn [_] [{:assistant_messages/id 1 :assistant_messages/role "user"
                          :assistant_messages/content "What is shape?"}
                         {:assistant_messages/id 2 :assistant_messages/role "assistant"
                          :assistant_messages/content "Shape is how many items."}])]
    (f)))

(defn- messages [request] (get-in request [:provider-request :messages]))

(deftest a-side-question-starts-with-the-main-requests-head
  (with-reads
    (fn []
      (let [main (messages (answer-request 7 chat 77 "Next?" nil {:model "m"} false true))
            side (messages (side-request 7 chat 77 "how many items"
                             [{:role "user" :content "Why items?"}] {:model "m"} true))
            head (butlast main)]
        (is (= head (take (count head) side))
          "the side request repeats the main request up to the main user turn")
        (is (= {:role "user" :content "Why items?"} (last side)))
        (is (= "system" (:role (nth side (count head)))))
        (is (str/includes? (:content (nth side (count head))) "\"how many items\""))
        (is (= "s-128" (get-in (side-request 7 chat 77 "p"
                                 [{:role "user" :content "q"}] {:model "m"} true)
                         [:provider-request :session_id])))))))

(deftest annotation-instructions-sit-in-the-tail-only-when-annotated
  (with-reads
    (fn []
      (let [plain (messages (answer-request 7 chat 77 "Q" nil {:model "m"} false true false))
            annotated (messages (answer-request 7 chat 77 "Q" nil {:model "m"} false true true))
            instructions? #(and (= "system" (:role %))
                             (str/includes? (str (:content %)) "# Annotated segments"))]
        (is (not-any? instructions? plain))
        (is (instructions? (last (butlast annotated))))
        (is (= (butlast plain) (butlast (butlast annotated)))
          "the instructions add one message and change nothing before it")))))

(deftest submitting-annotations-sends-their-message
  (let [started (atom nil)]
    (with-redefs [assistant/ensure-and-start-answer-run!
                  (fn [& args] (reset! started (vec args)) {:ok true :chat-id 128})]
      (testing "a blank draft sends the annotation message, flagged"
        (assistant/submit-chat-input! 7 77 77 128 "general" false "" [] "c1"
          [{:quote "steady ⊑ cont" :note "What is steady?"}])
        (is (= (annotations/user-message
                 [{:quote "steady ⊑ cont" :note "What is steady?"}] "")
              (nth @started 6)))
        (is (true? (last @started))))
      (testing "/compact with annotations is a message, not the command"
        (is (= :answer (:command (assistant/submit-chat-input!
                                   7 77 77 128 "general" false "/compact" [] "c2"
                                   [{:quote "x" :note nil}])))))
      (testing "no chat, no annotations"
        (is (:error (assistant/submit-chat-input! 7 77 77 nil "general" false "" [] "c3"
                      [{:quote "x" :note nil}]))))
      (testing "a list that fails `problem` starts nothing"
        (reset! started nil)
        (is (:error (assistant/submit-chat-input! 7 77 77 128 "general" false "" [] "c4"
                      [{:quote " " :note nil}])))
        (is (nil? @started))))))

(deftest a-side-thread-sends-only-answered-pairs
  (is (= [{:role "user" :content "q1"} {:role "assistant" :content "a1"}]
        (#'assistant/side-history
         [{:id 1 :role "user" :content "q1"}
          {:id 2 :role "assistant" :content "a1" :status "completed"}
          {:id 3 :role "user" :content "q2"}
          {:id 4 :role "assistant" :content "" :status "failed"}]))))

(deftest a-closed-side-thread-takes-no-more-text
  (let [!thread (atom (assoc (assistant/new-side-thread "p" 2)
                        :turns [{:id "u" :role "user" :content "q"}
                                {:id "a" :role "assistant" :content "" :status "streaming"}]))]
    (assistant/close-side-thread! !thread)
    (#'assistant/update-side-answer! !thread "a" #(assoc % :content "late"))
    (is (= "" (:content (peek (:turns @!thread)))))))

;; ── Cards ──────────────────────────────────────────────────────────────────

(deftest selection-cards-are-a-toolbar-job
  (let [queued (atom nil)]
    (with-redefs [db/get-topic-for-user (fn [_ id] {:topics/id id :topics/kind "page"})
                  db/get-root-topic-id (fn [_] 300)
                  db/get-topic (fn [_] {:topics/id 300 :topics/kind "pdf"})
                  db/get-assistant-message
                  (fn [_ _] {:content "Shape is how many items [evidence:E1]."})
                  settings/get-context-enabled (fn [_] true)
                  settings/get-card-type (fn [_] "cloze")
                  settings/get-card-count (fn [_] 3)
                  helpers/enqueue-card-gen! (fn [item] (reset! queued item) :enqueued)]
      (is (:ok (assistant/selection->cards! 7 301 2 "<p>how <b>many</b> items</p>")))
      (is (= {:content "<p>how <b>many</b> items</p>"
              :context "Shape is how many items ."
              :card-type "cloze" :card-count 3 :user-id 7 :enc-key nil
              :topic-id 301 :root-topic-id 300
              :pre-prompt nil :pre-prompt-mode nil :success-toast? true}
            (dissoc @queued :id))
        "a page under a PDF root takes the PDF as its root, as the toolbar does"))))

(deftest selection-cards-follow-the-fact-rule
  (let [generated (atom nil)
        item {:content "<p>how many items</p>" :context "ctx" :card-type "basic"
              :card-count 2 :user-id 7 :topic-id 301 :root-topic-id 300}]
    (with-redefs [settings/get-model (fn [_] "m")
                  settings/get-learning-goal (fn [& _] nil)
                  cards/generate-basic-cards (fn [opts] (reset! generated opts)
                                               {:success true :cards []})
                  helpers/save-cards-for-topic (fn [& _] {:success true :ids []})]
      (testing "facts on, with matching approved facts: the facts are the content"
        (with-redefs [settings/effective-use-facts-for-card-generation (fn [_ _] true)
                      db/get-kg-facts (fn [& _] [{:id 1}])
                      cff/select-relevant-facts (fn [& _] {:success true :facts [{:id 1}]})
                      cff/render-facts (fn [_] "Shape has degree.")]
          (helpers/generate-and-save! item)
          (is (= "Shape has degree." (:content @generated)))
          (is (nil? (:context @generated)))))
      (testing "facts off: the selection is the content"
        (with-redefs [settings/effective-use-facts-for-card-generation (fn [_ _] false)]
          (helpers/generate-and-save! item)
          (is (= "<p>how many items</p>" (:content @generated)))
          (is (= "ctx" (:context @generated))))))))

(deftest the-side-question-charge-has-a-label
  (is (= "Assistant side question" (cost-history/endpoint-label "assistant.side"))))
