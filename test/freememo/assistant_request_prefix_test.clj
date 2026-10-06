(ns freememo.assistant-request-prefix-test
  "A provider caches the start of a request that repeats the previous request.
   While the learner stays on one page, each assistant request must therefore
   start with the previous one, so the source text is in that shared start.
   See plans/assistant-request-prefix-cache.md. Runs with the database down:
   every read is stubbed."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.assistant :as assistant]
            [freememo.db :as db]))

(def ^:private answer-request #'assistant/answer-request)

(def ^:private topics
  {1 {:topics/id 1 :topics/kind "repo" :topics/title "grid"}
   42 {:topics/id 42 :topics/kind "code" :topics/title "ir-spec.md"
       :topics/repo_path "grid/docs/ir-spec.md" :topics/page_number 37
       :topics/content "<p>My note.</p>"}
   43 {:topics/id 43 :topics/kind "code" :topics/title "types.md"
       :topics/repo_path "grid/docs/types.md" :topics/page_number 38
       :topics/content ""}
   77 {:topics/id 77 :topics/kind "basic" :topics/title "Reading notes"
       :topics/content "<p>A referenced note.</p>"}})

(def ^:private code
  {42 {:source "# IR spec\n\n## Axes\n\nEvery slot has a signature.\n" :lang "markdown"}
   43 {:source "# Types\n\nA type is a set.\n" :lang "markdown"}})

(def ^:private facts
  [{:id 1 :page_number nil :subject_label "Slot" :predicate_label "has"
    :object_label "signature"}])

(def ^:private chat
  {:assistant_chats/id 128 :assistant_chats/root_topic_id 1
   :assistant_chats/mode "general" :assistant_chats/provider_session_id "s-128"
   :assistant_chats/web_search false})

(defn- with-chat
  "Call (f request! say!) with every read stubbed. `request!` renders the next
   request as [messages evidence], and `say!` adds a turn to the transcript.
   Requests carry a cache breakpoint unless `:breakpoint?` is false, because
   the default assistant model takes one."
  [{:keys [checkpoint breakpoint?] :or {breakpoint? true}} f]
  (let [!messages (atom [])
        request! (fn [page text refs]
                   (let [r (answer-request 7 chat page text refs {:model "m"} false
                             breakpoint?)]
                     [(get-in r [:provider-request :messages]) (:evidence r)]))
        say! (fn [role content]
               (swap! !messages conj {:assistant_messages/id (inc (count @!messages))
                                      :assistant_messages/role role
                                      :assistant_messages/content content}))]
    (with-redefs [db/get-topic-for-user (fn [_ id] (get topics id))
                  db/get-topic (fn [id] (get topics id))
                  db/get-ancestor-ids (fn [_] [1])
                  db/get-setting (fn [& _] nil)
                  db/get-topic-code (fn [_ id] (get code id))
                  db/get-kg-facts-context (fn [& _] facts)
                  db/get-fact-mastery (fn [& _] {})
                  db/latest-assistant-checkpoint (fn [& _] checkpoint)
                  db/question-doc-ref (fn [& _] nil)
                  db/get-assistant-messages (fn [_] @!messages)]
      (f request! say!))))

(defn- prefix? [shorter longer]
  (= shorter (take (count shorter) longer)))

(defn- text
  "The text of `message`, whether its content is a string or a list of parts."
  [message]
  (let [content (:content message)]
    (if (string? content) content (apply str (map :text content)))))

(defn- breakpoint? [message]
  (boolean (some :cache_control (when (sequential? (:content message))
                                  (:content message)))))

(deftest the-previous-request-starts-the-next-one-on-one-page
  (with-chat {}
    (fn [request! say!]
      (let [[first-turn] (request! 42 "First?" nil)]
        (say! "user" "First?")
        (say! "assistant" "An answer [evidence:E1].")
        (let [[second-turn] (request! 42 "Second?" nil)]
          (is (prefix? first-turn second-turn))
          (is (= {:role "user" :content "Second?"} (last second-turn))))))))

(deftest a-reference-is-the-only-part-the-next-turn-drops
  (with-chat {}
    (fn [request! say!]
      (let [[first-turn] (request! 42 "First?" [77])]
        (say! "user" "First?")
        (say! "assistant" "An answer.")
        (let [[second-turn] (request! 42 "Second?" nil)
              [reference question] (take-last 2 first-turn)]
          (is (str/includes? (text reference) "A referenced note."))
          (is (= {:role "user" :content "First?"} question))
          (is (prefix? (drop-last 2 first-turn) second-turn)
            "everything before the reference is shared")
          (is (not-any? #(str/includes? (text %) "A referenced note.") second-turn)
            "the reference is not sent again"))))))

(deftest a-new-page-changes-the-sources-and-what-follows
  (with-chat {}
    (fn [request! say!]
      (let [[first-turn] (request! 42 "First?" nil)]
        (say! "user" "First?")
        (say! "assistant" "An answer.")
        (let [[second-turn] (request! 43 "Second?" nil)
              first-source (fn [messages] (first (filter #(= "user" (:role %)) messages)))]
          (is (= (take 2 first-turn) (take 2 second-turn))
            "the instructions and the facts stay")
          (is (not= (first-source first-turn) (first-source second-turn)))
          (is (str/includes? (text (first-source second-turn)) "A type is a set.")))))))

(deftest the-request-runs-from-stable-to-per-turn
  (with-chat {:checkpoint {:summary "Earlier talk." :covered_through_message_id 0}}
    (fn [request! say!]
      (say! "user" "Before?")
      (say! "assistant" "Before.")
      (let [[messages] (request! 42 "Now?" [77])
            shape (mapv (fn [{:keys [role] :as message}]
                          [role (let [content (text message)]
                                  (cond
                                  (str/includes? content "# Evidence citations") :instructions
                                  (str/includes? content "Every slot has a signature.") :source
                                  (str/includes? content "My note.") :notes
                                  (str/includes? content "# Facts established") :facts
                                  (str/includes? content "# Conversation checkpoint") :checkpoint
                                  (str/includes? content "A referenced note.") :reference
                                  :else content))])
                    messages)]
        (is (= [["system" :instructions]
                ["system" :facts]
                ["system" :checkpoint]
                ["user" :source]
                ["user" :notes]
                ["user" "Before?"]
                ["assistant" "Before."]
                ["user" :reference]
                ["user" "Now?"]]
              shape))))))

(deftest the-facts-key-comes-first
  (with-chat {}
    (fn [request! _]
      (let [[without without-evidence] (request! 42 "Q?" nil)
            [with with-evidence] (request! 42 "Q?" [77])
            facts-message (fn [messages]
                            (some #(when (str/includes? (text %) "# Facts established") %)
                              messages))
            keys-of (fn [evidence] (mapv :citation-key evidence))
            facts-key (:citation-key (some #(when (= "document-level evidence"
                                                    (:display-label %)) %)
                                       with-evidence))]
        (is (= (facts-message without) (facts-message with))
          "a reference does not change the facts message")
        (is (= "E1" facts-key) "the facts key does not depend on any source")
        (is (str/includes? (text (facts-message with))
              (str "[evidence:" facts-key "]")))
        (testing "keys run facts, current topic, then references, with no gap"
          (is (= (keys-of with-evidence)
                (mapv #(str "E" %) (range 1 (inc (count with-evidence))))))
          (is (= (keys-of without-evidence)
                (take (count without-evidence) (keys-of with-evidence))))
          (is (= ["A referenced note."]
                (map :quoted-text (drop (count without-evidence) with-evidence)))))))))

(deftest the-breakpoint-ends-the-current-topic
  (testing "a breakpoint model marks the last current-topic source turn only"
    (with-chat {:checkpoint {:summary "Earlier talk." :covered_through_message_id 0}}
      (fn [request! say!]
        (say! "user" "Before?")
        (say! "assistant" "Before.")
        (let [[messages] (request! 42 "Now?" [77])
              marked (keep-indexed #(when (breakpoint? %2) %1) messages)]
          (is (= 1 (count marked)))
          (is (str/includes? (text (nth messages (first marked))) "My note.")
            "the notes are the last source turn of a repo file")
          (is (not-any? #(= "system" (:role %)) (drop (inc (first marked)) messages))
            "no system message comes after the breakpoint")
          (is (= [{:type "text" :text (text (nth messages (first marked)))
                   :cache_control {:type "ephemeral"}}]
                (:content (nth messages (first marked))))
            "the marked message is one text part"))
        (testing "the text is the same with and without a breakpoint"
          (let [[marked] (request! 42 "Now?" [77])]
            (with-chat {:checkpoint {:summary "Earlier talk." :covered_through_message_id 0}
                        :breakpoint? false}
              (fn [request-plain! say-plain!]
                (say-plain! "user" "Before?")
                (say-plain! "assistant" "Before.")
                (let [[plain] (request-plain! 42 "Now?" [77])]
                  (is (not-any? breakpoint? plain))
                  (is (= (map (juxt :role text) marked)
                        (map (juxt :role text) plain)))))))))))
  (testing "a topic with no source text carries no breakpoint"
    (with-redefs [db/get-topic-code (fn [& _] nil)]
      (let [r (with-redefs [db/get-topic-for-user (fn [_ id] (get topics id))
                            db/get-topic (fn [id] (assoc (get topics id) :topics/content ""))
                            db/get-ancestor-ids (fn [_] [1])
                            db/get-setting (fn [& _] nil)
                            db/get-kg-facts-context (fn [& _] [])
                            db/get-fact-mastery (fn [& _] {})
                            db/latest-assistant-checkpoint (fn [& _] nil)
                            db/question-doc-ref (fn [& _] nil)
                            db/get-assistant-messages (fn [_] [])]
                (answer-request 7 chat 42 "Q?" nil {:model "m"} false true))]
        (is (not-any? breakpoint? (get-in r [:provider-request :messages])))))))
