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
   request as [messages evidence], and `say!` adds a turn to the transcript."
  [{:keys [checkpoint]} f]
  (let [!messages (atom [])
        request! (fn [page text refs]
                   (let [r (answer-request 7 chat page text refs {:model "m"} false)]
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
                  db/get-assistant-messages (fn [_] @!messages)]
      (f request! say!))))

(defn- prefix? [shorter longer]
  (= shorter (take (count shorter) longer)))

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
          (is (str/includes? (:content reference) "A referenced note."))
          (is (= {:role "user" :content "First?"} question))
          (is (prefix? (drop-last 2 first-turn) second-turn)
            "everything before the reference is shared")
          (is (not-any? #(str/includes? (str (:content %)) "A referenced note.")
                second-turn)
            "the reference is not sent again"))))))

(deftest a-new-page-changes-the-sources-and-what-follows
  (with-chat {}
    (fn [request! say!]
      (let [[first-turn] (request! 42 "First?" nil)]
        (say! "user" "First?")
        (say! "assistant" "An answer.")
        (let [[second-turn] (request! 43 "Second?" nil)]
          (is (= (first first-turn) (first second-turn)) "the instructions stay")
          (is (not= (second first-turn) (second second-turn)))
          (is (str/includes? (:content (second second-turn)) "A type is a set.")))))))

(deftest the-request-runs-from-stable-to-per-turn
  (with-chat {:checkpoint {:summary "Earlier talk." :covered_through_message_id 0}}
    (fn [request! say!]
      (say! "user" "Before?")
      (say! "assistant" "Before.")
      (let [[messages] (request! 42 "Now?" [77])
            shape (mapv (fn [{:keys [role content]}]
                          [role (cond
                                  (str/includes? content "# Evidence citations") :instructions
                                  (str/includes? content "Every slot has a signature.") :source
                                  (str/includes? content "My note.") :notes
                                  (str/includes? content "# Facts established") :facts
                                  (str/includes? content "# Conversation checkpoint") :checkpoint
                                  (str/includes? content "A referenced note.") :reference
                                  :else content)])
                    messages)]
        (is (= [["system" :instructions]
                ["user" :source]
                ["user" :notes]
                ["system" :facts]
                ["system" :checkpoint]
                ["user" "Before?"]
                ["assistant" "Before."]
                ["user" :reference]
                ["user" "Now?"]]
              shape))))))

(deftest the-facts-key-comes-before-the-references
  (with-chat {}
    (fn [request! _]
      (let [[without without-evidence] (request! 42 "Q?" nil)
            [with with-evidence] (request! 42 "Q?" [77])
            facts-message (fn [messages]
                            (some #(when (str/includes? (:content %) "# Facts established") %)
                              messages))
            keys-of (fn [evidence] (mapv :citation-key evidence))
            facts-key (:citation-key (some #(when (= "document-level evidence"
                                                    (:display-label %)) %)
                                       with-evidence))]
        (is (= (facts-message without) (facts-message with))
          "a reference does not change the facts message")
        (is (str/includes? (:content (facts-message with))
              (str "[evidence:" facts-key "]")))
        (testing "keys run current topic, facts, then references, with no gap"
          (is (= (keys-of with-evidence)
                (mapv #(str "E" %) (range 1 (inc (count with-evidence))))))
          (is (= (keys-of without-evidence)
                (take (count without-evidence) (keys-of with-evidence))))
          (is (= ["A referenced note."]
                (map :quoted-text (drop (count without-evidence) with-evidence)))))))))
