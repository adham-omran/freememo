(ns freememo.assistant-chat-forks-test
  "Chat forks without a database (plans/assistant-chat-forks.md §4.8.1): the
   slash parser, the inherited path, the request transcript, the branch step,
   the budget, the compaction planner and the history tree. Every read is
   stubbed."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.assistant :as assistant]
            [freememo.assistant-budget :as budget]
            [freememo.assistant-chat-history :as history]
            [freememo.assistant-chat-tree :as tree]
            [freememo.assistant-slash :as slash]
            [freememo.db :as db]
            [freememo.model-context :as model-context]))

;; ── §4.3.1: slash commands ─────────────────────────────────────────────────

(deftest slash-commands-parse-in-one-place
  (is (= {:command :compact} (slash/parse "  /compact ")))
  (is (= {:command :fork :question "what is A?"} (slash/parse "/fork  what is A? ")))
  (is (= {:command :fork :question "line one\nline two"}
        (slash/parse "/fork\nline one\nline two")))
  (is (= {:command :fork :question ""} (slash/parse "/fork")))
  (testing "a command must stand alone"
    (is (nil? (slash/parse "/forked")))
    (is (nil? (slash/parse "/compact now")))
    (is (nil? (slash/parse "please /fork this")))
    (is (nil? (slash/parse nil)))))

;; ── §4.2.1: the inherited path ─────────────────────────────────────────────
;;
;; Chat 1 is a root with messages 1u 2a 3u 4a. Chat 2 forks it at 4, with
;; 5u 6a 7u 8a. Chat 3 forks chat 2 at 6, with 9u 10a. Chat 4 forks chat 2 at
;; 2, a message chat 2 inherited from chat 1, with 11u 12a.

(def ^:private chats
  {1 {:id 1 :parent-id nil :fork-message-id nil :title "ABC"}
   2 {:id 2 :parent-id 1 :fork-message-id 4 :title "A"}
   3 {:id 3 :parent-id 2 :fork-message-id 6 :title "A1"}
   4 {:id 4 :parent-id 2 :fork-message-id 2 :title "A0"}})

(deftest the-inherited-path-runs-from-the-root-to-the-fork-point
  (is (= [] (tree/inherited-path chats 1)) "a root inherits nothing")
  (is (= [[1 4]] (tree/inherited-path chats 2)) "a child")
  (is (= [[1 4] [2 6]] (tree/inherited-path chats 3)) "a grandchild")
  (is (= [[1 2]] (tree/inherited-path chats 4))
    "a fork at a message inherited from the grandparent names no parent rows"))

;; ── §4.2.2: the request transcript ─────────────────────────────────────────

(def ^:private messages
  (into {}
    (map (fn [id] [id {:assistant_messages/id id
                       :assistant_messages/role (if (odd? id) "user" "assistant")
                       :assistant_messages/content (str "m" id)}]))
    (range 1 13)))

(def ^:private own-ids {1 [1 2 3 4] 2 [5 6 7 8] 3 [9 10] 4 [11 12]})

(defn- chat-row [id]
  (let [{:keys [parent-id fork-message-id]} (get chats id)]
    {:assistant_chats/id id :assistant_chats/root_topic_id 77
     :assistant_chats/parent_chat_id parent-id
     :assistant_chats/fork_message_id fork-message-id}))

(defn- with-tree
  "Call `f` with every transcript read stubbed. `checkpoints` maps
   [chat-id scope] to a checkpoint."
  [checkpoints f]
  (with-redefs [db/get-assistant-chat-lineage
                (fn [_ chat-id]
                  (loop [rows [] id chat-id]
                    (if-let [chat (get chats id)]
                      (recur (conj rows {:id id :parent_chat_id (:parent-id chat)
                                         :fork_message_id (:fork-message-id chat)
                                         :title (:title chat)})
                        (:parent-id chat))
                      rows)))
                db/get-assistant-messages
                (fn
                  ([chat-id] (mapv messages (own-ids chat-id)))
                  ([chat-id through] (mapv messages (filter #(<= % through) (own-ids chat-id)))))
                db/latest-assistant-checkpoint
                (fn [_ chat-id scope] (get checkpoints [chat-id scope]))]
    (f)))

(def ^:private request-transcript #'assistant/request-transcript)

(defn- turns
  "The provider turns of the messages `ids`."
  [ids]
  (mapv (fn [id]
          (let [message (messages id)]
            {:role (:assistant_messages/role message)
             :content (:assistant_messages/content message)}))
    ids))

(deftest a-request-reads-inherited-history-then-its-own
  (testing "a root chat sends its own rows, as before forks existed"
    (with-tree {}
      #(is (= {:head-messages [] :messages (turns [1 2 3 4])}
             (request-transcript 7 (chat-row 1))))))
  (testing "a child sends the inherited rows, then its own"
    (with-tree {}
      #(is (= {:head-messages [] :messages (turns [1 2 3 4 5 6 7 8])}
             (request-transcript 7 (chat-row 2))))))
  (testing "a grandchild reads both ancestors up to each fork point"
    (with-tree {}
      #(is (= (turns [1 2 3 4 5 6 9 10])
             (:messages (request-transcript 7 (chat-row 3)))))))
  (testing "a fork at an inherited message reads only up to it"
    (with-tree {}
      #(is (= (turns [1 2 11 12]) (:messages (request-transcript 7 (chat-row 4)))))))
  (testing "inherited checkpoint, inherited rows, own checkpoint, own rows"
    (with-tree {[3 "inherited"] {:summary "Inherited summary." :covered_through_message_id 5}
                [3 "own"] {:summary "Own summary." :covered_through_message_id 9}}
      #(is (= {:head-messages
               [{:role "system"
                 :content (str "# Checkpoint of the conversation this chat forked from\n\n"
                            "Inherited summary.")}]
               :messages (-> (turns [6])
                           (conj {:role "system"
                                  :content "# Conversation checkpoint\n\nOwn summary."})
                           (into (turns [10])))}
             (request-transcript 7 (chat-row 3)))))))

;; ── §4.4.1: which branches a request reads ─────────────────────────────────

(deftest branches-are-collected-depth-first-and-pruned-by-exclusion
  (let [nodes [{:id 2 :parent-id 1 :fork-message-id 4 :title "A"}
               {:id 5 :parent-id 1 :fork-message-id 4 :title "B" :excluded? true}
               {:id 6 :parent-id 5 :fork-message-id 30 :title "B1"}
               {:id 3 :parent-id 2 :fork-message-id 6 :title "A1"}
               {:id 7 :parent-id 1 :fork-message-id 2 :title "C"}
               {:id 9 :parent-id 99 :fork-message-id 1 :title "elsewhere"}]]
    (is (= [{:chat-id 7 :fork-message-id 2 :titles ["C"]}
            {:chat-id 2 :fork-message-id 4 :titles ["A"]}
            {:chat-id 3 :fork-message-id 6 :titles ["A" "A1"]}]
          (tree/included-branches nodes 1))
      "fork order, nested branches under their parent, excluded B prunes B1")
    (is (= [{:chat-id 3 :fork-message-id 6 :titles ["A1"]}]
          (tree/included-branches nodes 2))
      "a chat reads its descendants, never its siblings or ancestors")))

;; ── §4.4.2, §4.4.3: the branch step ────────────────────────────────────────

(def ^:private root-chat
  {:assistant_chats/id 1 :assistant_chats/root_topic_id 77
   :assistant_chats/mode "general" :assistant_chats/provider_session_id "s-1"
   :assistant_chats/web_search false})

(defn- with-branches [f]
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
                db/question-doc-ref (fn [& _] nil)
                db/latest-assistant-checkpoint
                (fn [_ chat-id _] (when (= 3 chat-id)
                                    {:summary "A1 so far." :covered_through_message_id 9}))
                db/get-assistant-chat-descendants
                (fn [_ _] [{:id 2 :parent_chat_id 1 :fork_message_id 4 :title "A"
                            :excluded false :activity_ms 10}
                           {:id 3 :parent_chat_id 2 :fork_message_id 6 :title "A1"
                            :excluded false :activity_ms 20}])
                db/get-assistant-message
                (fn [_ id] {:id id :content (str "Fork point " id " [evidence:E1].")})
                db/get-assistant-messages
                (fn [chat-id] (mapv messages (own-ids chat-id)))]
    (f)))

(def ^:private answer-request #'assistant/answer-request)
(def ^:private side-request #'assistant/side-request)

(deftest branches-follow-the-transcript-in-fork-order
  (with-branches
    (fn []
      (let [sent (get-in (answer-request 7 root-chat 77 "Combine them?" nil {:model "m"}
                           false true)
                   [:provider-request :messages])
            branch? #(str/starts-with? (str (:content %)) "# Branch ")
            [a a1] (filter branch? sent)
            last-own (.indexOf ^java.util.List sent {:role "assistant" :content "m4"})]
        (is (= 2 (count (filter branch? sent))))
        (is (str/starts-with? (:content a)
              "# Branch \"A\", forked at \"Fork point 4 .\""))
        (is (str/includes? (:content a) "Learner: m5\n\nAssistant: m6"))
        (is (str/starts-with? (:content a1)
              "# Branch \"A\" › \"A1\", forked at \"Fork point 6 .\""))
        (is (str/includes? (:content a1) "## Checkpoint\n\nA1 so far."))
        (is (str/includes? (:content a1) "Assistant: m10"))
        (is (not (str/includes? (:content a1) "Learner: m9"))
          "a branch sends only its rows after its checkpoint")
        (is (not (str/includes? (:content a) "m1"))
          "a branch never repeats what it inherited")
        (is (= (inc last-own) (.indexOf ^java.util.List sent a))
          "the first branch follows the last transcript message")
        (is (= {:role "user" :content "Combine them?"} (last sent)))))))

(deftest a-side-question-reads-no-branch
  (with-branches
    (fn []
      (let [sent (get-in (side-request 7 root-chat 77 "passage"
                           [{:role "user" :content "Why?"}] {:model "m"} true)
                   [:provider-request :messages])]
        (is (not-any? #(str/starts-with? (str (:content %)) "# Branch ") sent))))))

;; ── §4.5.2: the budget ─────────────────────────────────────────────────────

(deftest the-budget-is-ninety-percent-less-the-output-reserve
  (is (= 2 (budget/tokens 5)) "characters ÷ 4, rounded up")
  (is (= (- 115200 8000) (budget/budget-tokens 128000)))
  (is (nil? (budget/budget-tokens nil)) "an unknown length refuses nothing")
  (is (zero? (budget/budget-tokens 1000)) "never negative")
  (is (budget/fits? [{:content (apply str (repeat 400 "x"))}] 100))
  (is (not (budget/fits? [{:content (apply str (repeat 401 "x"))}] 100)))
  (is (budget/fits? [{:content (apply str (repeat 10000 "x"))}] nil))
  (is (= 3 (budget/request-tokens [{:content [{:type "text" :text "abcdefghij"}]}]))
    "a cached text part counts by its text"))

(deftest the-model-catalog-gives-the-top-providers-length
  (is (= {"a/one" 200000 "b/two" 64000}
        (model-context/catalog-lengths
          [{:id "a/one" :context_length 1000000 :top_provider {:context_length 200000}}
           {:id "b/two" :context_length 64000 :top_provider {:context_length nil}}
           {:id "c/three" :context_length nil}
           {:context_length 5}]))))

;; ── §4.5.3: the compaction planner ─────────────────────────────────────────

(deftest slices-take-the-oldest-rows-that-fit
  (is (= [[1 2] [3 3] [4 5]]
        (budget/slices [{:id 1 :chars 40} {:id 2 :chars 40} {:id 3 :chars 400}
                        {:id 4 :chars 4} {:id 5 :chars 4}]
          20))
    "a row over the budget is a slice of its own")
  (is (= [] (budget/slices [] 20))))

(deftest the-plan-compacts-stale-branches-then-inherited-then-own
  (is (= [{:target :branch :chat-id 9 :slices [[90 91]]}
          {:target :branch :chat-id 8 :slices [[80 80] [81 81]]}
          {:target :inherited :chat-id 3 :slices [[1 6]]}
          {:target :own :chat-id 3 :slices [[9 10]]}]
        (budget/compaction-plan
          {:branches [{:chat-id 8 :activity-ms 200 :rows [{:id 80 :chars 60} {:id 81 :chars 60}]}
                      {:chat-id 9 :activity-ms 100 :rows [{:id 90 :chars 8} {:id 91 :chars 8}]}
                      {:chat-id 7 :activity-ms 50 :rows []}]
           :inherited {:chat-id 3 :rows [{:id 1 :chars 8} {:id 6 :chars 8}]}
           :own {:chat-id 3 :rows [{:id 9 :chars 8} {:id 10 :chars 8}]}
           :slice-budget 20}))
    "stalest branch first, a branch with no rows is no target, slices fit 20 tokens")
  (is (= [{:target :own :chat-id 1 :slices [[1 4]]}]
        (budget/compaction-plan {:branches [] :inherited nil
                                 :own {:chat-id 1 :rows [{:id 1 :chars 4} {:id 4 :chars 4}]}
                                 :slice-budget 20}))
    "a root chat has no inherited target"))

(deftest a-compaction-request-ends-with-a-user-turn
  ;; Gemini answered a compaction that ended on the assistant's answer with
  ;; HTTP 400 (plans/assistant-context-meter-and-slash-menu.md §5).
  (let [chat {:assistant_chats/provider_session_id "s"}
        turns [{:role "user" :content "Q"} {:role "assistant" :content "A"}]
        request (assistant/compaction-request {:model "m"} chat {:summary "S"} turns)
        messages (:messages request)]
    (is (= "user" (:role (last messages))))
    (is (= ["system" "system" "user" "assistant" "user"] (mapv :role messages)))
    (is (= turns (subvec messages 2 4)) "the turns are summarized as they are")
    (is (re-find #"^Summarize this conversation" (:content (first messages)))
      "the instructions stay first, where the DB test's stub looks for them")
    (is (= "user" (:role (last (:messages (assistant/compaction-request
                                            {:model "m"} chat nil [])))))
      "with no prior and no turns too")))

;; ── §4.5.6: an overflow error ──────────────────────────────────────────────

(deftest overflow-errors-are-recognized
  (is (assistant/context-overflow-error?
        (ex-info "This endpoint's maximum context length is 131072 tokens." {:status 400})))
  (is (assistant/context-overflow-error? (ex-info "Payload" {:status 413})))
  (is (assistant/context-overflow-error? (ex-info "prompt is too long: 210000 tokens" {})))
  (is (not (assistant/context-overflow-error? (ex-info "Provider returned error" {:status 429}))))
  (is (not (assistant/context-overflow-error? nil))))

;; ── §4.6.1: the history tree ───────────────────────────────────────────────

(deftest history-nests-forks-under-their-parent
  (let [now 1000000000000
        chats [{:id 3 :label "A1" :parent-id 2 :fork-message-id 6 :activity-ms now}
               {:id 1 :label "ABC" :activity-ms (- now 1000)}
               {:id 2 :label "A" :parent-id 1 :fork-message-id 4 :activity-ms now}
               {:id 5 :label "B" :parent-id 1 :fork-message-id 4 :activity-ms now}
               {:id 9 :label "Other" :activity-ms (- now 2000)}]
        rows (fn [section] (mapv (juxt :id :depth) section))]
    (testing "a fork follows its parent, depth first, in fork order"
      (is (= [[1 0] [2 1] [3 2] [5 1] [9 0]]
            (rows (:today (history/history-sections chats "" now))))))
    (testing "a search for a fork shows the path to it (§4.6.4)"
      (is (= [[1 0] [2 1] [3 2]]
            (rows (:today (history/history-sections chats "a1" now))))))))
