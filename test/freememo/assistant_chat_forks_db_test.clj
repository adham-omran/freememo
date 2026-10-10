(ns freememo.assistant-chat-forks-db-test
  "Chat forks against PostgreSQL (plans/assistant-chat-forks.md §4.8). The
   provider is the one stubbed seam: `openrouter/chat-completion-stream!`
   answers from the request, so every run goes through the real run worker,
   the real preparation and the real checkpoints. Every test skips when the
   database is down."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.account-export :as account-export]
            [freememo.account-import :as account-import]
            [freememo.archive :as archive]
            [freememo.assistant :as assistant]
            [freememo.assistant-budget :as budget]
            [freememo.credits :as credits]
            [freememo.db :as db]
            [freememo.model-context :as model-context]
            [freememo.openrouter :as openrouter]
            [freememo.settings :as settings]
            [freememo.toasts :as toasts]
            [freememo.undo :as undo]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [java.io ByteArrayOutputStream File]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})

(defn- create-user! []
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
          (str "chat-forks-test-" (random-uuid)) (str (random-uuid) "@example.test")]
         opts)))

(defn- create-topic! [user-id]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, kind, title) VALUES (?, 'basic', ?) RETURNING id"
          user-id "Chat forks test"]
         opts)))

(defmacro ^:private with-users
  "Bind `syms` to fresh users and delete them afterwards, or skip with no DB."
  [syms & body]
  `(if-not (db-up?)
     (is true "PostgreSQL is unavailable; integration assertion skipped")
     (let [~@(mapcat (fn [s] [s `(create-user!)]) syms)]
       (try ~@body
         (finally
           (jdbc/execute! db/ds (into [(str "DELETE FROM users WHERE id IN ("
                                         (str/join "," (repeat ~(count syms) "?")) ")")]
                                  [~@syms])))))))

;; ── The provider seam ──────────────────────────────────────────────────────

(defn- request-kind [body]
  (if (str/starts-with? (str (:content (first (:messages body)))) "Summarize this conversation")
    :compact :answer))

(defn- last-user-text [body]
  (:content (last (filter #(= "user" (:role %)) (:messages body)))))

(defn- answer-text
  "What the stub answers: a long answer for a question that asks for one."
  [body]
  (if (str/includes? (str (last-user-text body)) "LONG")
    (apply str (repeat 40000 "x"))
    (str "Answer to " (last-user-text body))))

(defn- with-provider
  "Call `f` with the provider stubbed. `!requests` collects every request body
   in order. `fail-first-answer` is an exception the first answer request
   throws, or nil. `budget-tokens` replaces the budget of every request."
  [{:keys [!requests fail-first-answer budget-tokens]} f]
  (let [!failed? (atom false)]
    (with-redefs [settings/get-openrouter-api-key (fn [_] "test-key")
                  credits/check-cost-billed-balance! (fn [_] {:ok true})
                  model-context/context-length (fn [_] nil)
                  budget/budget-tokens (fn [_] budget-tokens)
                  openrouter/chat-completion-stream!
                  (fn [_ body _ on-delta on-metadata]
                    (swap! !requests conj body)
                    (when (and fail-first-answer (= :answer (request-kind body))
                            (compare-and-set! !failed? false true))
                      (throw fail-first-answer))
                    (let [text (if (= :compact (request-kind body))
                                 (str "SUMMARY of " (count (:messages body)) " messages")
                                 (answer-text body))]
                      (on-delta text)
                      (on-metadata {})
                      {:text text :metadata {} :headers {}}))]
      (f))))

(defn- settled?
  "True when the chat has no active run and no preparation."
  [user-id chat-id]
  (and (nil? (db/get-active-assistant-run user-id chat-id))
    (not (contains? @@#'assistant/!preparing chat-id))))

(defn- settle!
  "Wait until the chats have settled, or fail after 10 s."
  [user-id & chat-ids]
  (loop [n 0]
    (cond
      (every? #(settled? user-id %) chat-ids) true
      (> n 400) (is false (str "chats did not settle: " (vec chat-ids)))
      :else (do (Thread/sleep 25) (recur (inc n))))))

(defn- send!
  "Submit `draft` in `chat-id` (nil for a new chat) and wait for it to settle.
   Returns the submit result."
  ([user-id topic-id chat-id draft]
   (send! user-id topic-id chat-id draft nil))
  ([user-id topic-id chat-id draft fork]
   (let [result (assistant/submit-chat-input! user-id topic-id topic-id chat-id "general"
                  false draft [] (str (random-uuid)) [] fork)]
     (when-let [id (:chat-id result)] (settle! user-id id))
     (when chat-id (settle! user-id chat-id))
     result)))

(defn- chat-row [chat-id]
  (jdbc/execute-one! db/ds ["SELECT * FROM assistant_chats WHERE id = ?" chat-id] opts))

(defn- own-messages [chat-id]
  (mapv (fn [m] [(:assistant_messages/role m) (:assistant_messages/content m)])
    (db/get-assistant-messages chat-id)))

(defn- latest-run [user-id chat-id]
  (last (db/get-assistant-runs-for-chat user-id chat-id)))

(defn- branch-headers [request]
  (into []
    (comp (map :content) (filter #(str/starts-with? (str %) "# Branch "))
      (map #(first (str/split-lines %))))
    (:messages request)))

;; ── §4.3: creating a fork ──────────────────────────────────────────────────

(deftest a-fork-starts-with-one-question-and-the-parents-history
  (with-users [owner]
    (let [topic-id (create-topic! owner)
          !requests (atom [])]
      (with-provider {:!requests !requests}
        (fn []
          (let [{root :chat-id} (send! owner topic-id nil "Tell me about A, B and C")]
            (assistant/set-chat-mode! owner root "tutor")
            (testing "/fork asks for a question"
              (is (= {:error "Type a question after /fork." :chat-id root :command :fork}
                    (select-keys (send! owner topic-id root "/fork") [:error :chat-id :command]))))
            (testing "4.3.2: /fork makes a child with one turn and an answer"
              (let [r (send! owner topic-id root "/fork what is A?")
                    child (:chat-id r)
                    last-root-message (:assistant_messages/id
                                        (last (db/get-assistant-messages root)))]
                (is (:ok r))
                (is (= :fork (:command r)))
                (is (not= root child))
                (is (= root (:parent_chat_id (chat-row child))))
                (is (= last-root-message (:fork_message_id (chat-row child))))
                (is (= [["user" "what is A?"] ["assistant" "Answer to what is A?"]]
                      (own-messages child)))
                (is (= "tutor" (:mode (chat-row child))) "4.3.5: the mode is copied")
                (is (not= (:provider_session_id (chat-row root))
                      (:provider_session_id (chat-row child))))
                (is (some #(= {:role "user" :content "Tell me about A, B and C"} %)
                      (:messages (last @!requests)))
                  "the child's request carries the inherited turns")
                (testing "4.2.3: the child shows inherited rows, a divider, its own rows"
                  (let [rows (assistant/messages* nil owner child)]
                    (is (= [true true nil nil nil]
                          (mapv :assistant_messages/inherited? rows)))
                    (is (= "fork" (:assistant_event/type (nth rows 2))))
                    (is (= "Tell me about A, B and C"
                          (:assistant_messages/content (nth rows 2))))))))
            (testing "4.3.2: a failed start leaves no child"
              (let [before (count (db/get-assistant-chats owner topic-id))
                    r (with-redefs [settings/get-openrouter-api-key (fn [_] "")]
                        (send! owner topic-id root "/fork and B?"))]
                (is (= "OpenRouter API key not configured." (:error r)))
                (is (= root (:chat-id r)) "the parent stays open")
                (is (= before (count (db/get-assistant-chats owner topic-id))))))
            (testing "4.3.4: no fork at a message whose run is active"
              (let [session (:provider_session_id (chat-row root))
                    created (db/create-assistant-answer-run!
                              root "Pending" nil
                              {:run-uuid (str (random-uuid)) :provider-session-id session
                               :requested-model "m" :request-snapshot {} :source-snapshot []
                               :evidence []})]
                (is (= "Wait for the answer to finish."
                      (:error (assistant/submit-chat-input! owner topic-id topic-id root
                                "general" false "/fork too soon" [] "c" [] nil))))
                (is (= "Wait for the answer to finish."
                      (:error (assistant/fork-and-start-answer! owner root
                                (:assistant-message-id created) topic-id "Q" [] "c" nil))))
                (db/stop-assistant-run! owner root)))))))))

(deftest fork-here-at-an-inherited-message-makes-a-child-of-the-open-chat
  (with-users [owner]
    (let [topic-id (create-topic! owner)]
      (with-provider {:!requests (atom [])}
        (fn []
          (let [{root :chat-id} (send! owner topic-id nil "First question")
                first-answer (:assistant_messages/id (second (db/get-assistant-messages root)))
                _ (send! owner topic-id root "Second question")
                {a :chat-id} (send! owner topic-id root "/fork about A")
                r (send! owner topic-id a "Back to the first answer" {:message-id first-answer})
                child (:chat-id r)]
            (is (= :fork (:command r)))
            (is (= a (:parent_chat_id (chat-row child))) "D1b: the parent is the open chat")
            (is (= first-answer (:fork_message_id (chat-row child))))
            (is (= [["user" "First question"] ["assistant" "Answer to First question"]]
                  (->> (assistant/messages* nil owner child)
                    (filter :assistant_messages/inherited?)
                    (mapv (juxt :assistant_messages/role :assistant_messages/content))))
              "it inherits only up to the fork point, from the grandparent")
            (testing "a message outside the open chat cannot be forked at"
              (let [other (:chat-id (send! owner topic-id nil "Elsewhere"))
                    foreign (:assistant_messages/id (first (db/get-assistant-messages other)))]
                (is (= "That message is not in this chat."
                      (:error (send! owner topic-id a "Q" {:message-id foreign}))))))))))))

;; ── §4.4, §4.6.3: branches and exclusion ───────────────────────────────────

(deftest a-parent-reads-its-included-branches-in-fork-order
  (with-users [owner]
    (let [topic-id (create-topic! owner)
          !requests (atom [])]
      (with-provider {:!requests !requests}
        (fn []
          (let [{root :chat-id} (send! owner topic-id nil "ABC")
                {a :chat-id} (send! owner topic-id root "/fork about A")
                {a1 :chat-id} (send! owner topic-id a "/fork deeper in A")
                {b :chat-id} (send! owner topic-id root "/fork about B")]
            (send! owner topic-id root "Combine them")
            (is (= ["# Branch \"about A\", forked at \"Answer to ABC\""
                    "# Branch \"about A\" › \"deeper in A\", forked at \"Answer to about A\""
                    "# Branch \"about B\", forked at \"Answer to ABC\""]
                  (branch-headers (:request_snapshot (latest-run owner root)))))
            (is (empty? (branch-headers (:request_snapshot (latest-run owner a1))))
              "a leaf reads no branch")
            (send! owner topic-id a "More on A")
            (is (= ["# Branch \"deeper in A\", forked at \"Answer to about A\""]
                  (branch-headers (:request_snapshot (latest-run owner a))))
              "a branch reads its own branches, never its sibling B")
            (testing "4.6.3: excluding A drops A and A1 from the next request only"
              (let [before (:request_snapshot (latest-run owner root))]
                (is (:success (assistant/set-chat-excluded! owner a true)))
                (is (= before (:request_snapshot (latest-run owner root))))
                (send! owner topic-id root "Again")
                (is (= ["# Branch \"about B\", forked at \"Answer to ABC\""]
                      (branch-headers (:request_snapshot (latest-run owner root)))))
                (is (some? (db/get-assistant-chat owner a)) "an excluded chat stays openable")
                (is (= [a true] (assistant/chat-branch-state* nil owner a)))
                (is (:success (assistant/set-chat-excluded! owner a false)))
                (is (false? (:success (assistant/set-chat-excluded! owner root true)))
                  "a root cannot be excluded")))
            (is (= #{a b} (set (map :id (filter #(= root (:parent_chat_id %))
                                          (db/get-assistant-chats owner topic-id))))))))))))

;; ── §4.1.5: delete and undo ────────────────────────────────────────────────

(deftest deleting-a-chat-stages-its-forks-and-undo-restores-them
  (with-users [owner]
    (let [topic-id (create-topic! owner)
          !toasts (atom [])]
      (with-provider {:!requests (atom [])}
        (fn []
          (with-redefs [toasts/push! (fn [_ toast] (swap! !toasts conj toast))]
            (let [{root :chat-id} (send! owner topic-id nil "ABC")
                  {a :chat-id} (send! owner topic-id root "/fork A")
                  {a1 :chat-id} (send! owner topic-id a "/fork A1")
                  {b :chat-id} (send! owner topic-id root "/fork B")
                  {other :chat-id} (send! owner topic-id nil "Unrelated")]
              (is (:success (assistant/stage-chat-deletion! owner root)))
              (is (= "Chat and its 3 branches deleted. You can undo this for 12 hours."
                    (:message (last @!toasts))))
              (is (= [other] (mapv :id (db/get-assistant-chats owner topic-id)))
                "every descendant is hidden with the chat")
              (is (every? nil? (map #(db/get-assistant-chat owner %) [root a a1 b])))
              (is (:success (undo/undo-entry! owner (:staged_delete_id (chat-row root)))))
              (is (= #{root a a1 b other}
                    (set (map :id (db/get-assistant-chats owner topic-id))))))))))))

;; ── §4.5: overflow ─────────────────────────────────────────────────────────

(deftest an-oversized-request-compacts-the-stale-branch-first
  (with-users [owner]
    (let [topic-id (create-topic! owner)
          !requests (atom [])
          long-answer? #(> (count (str %)) 30000)]
      (with-provider {:!requests !requests :budget-tokens nil}
        (fn []
          (let [{root :chat-id} (send! owner topic-id nil "ABC")
                {a :chat-id} (send! owner topic-id root "/fork LONG one")
                _ (send! owner topic-id a "LONG two")
                _ (reset! !requests [])]
            (with-redefs [budget/budget-tokens (fn [_] 20000)]
              (let [r (send! owner topic-id root "Combine")]
                (is (:ok r))
                (is (:preparing? r) "4.5.4: the answer waits for its compactions")))
            (testing "4.5.3: the branch is compacted in slices of its oldest rows"
              (is (= [:compact :compact :answer] (mapv request-kind @!requests)))
              (let [checkpoints (db/get-assistant-checkpoints owner a)]
                (is (= 2 (count checkpoints)))
                (is (every? #(= root (:for_chat_id %)) checkpoints))
                (is (every? #(= "own" (:scope %)) checkpoints))
                (is (= (:assistant_messages/id (last (db/get-assistant-messages a)))
                      (:covered_through_message_id (last checkpoints))))))
            (testing "the answer then fits and completes"
              (let [run (latest-run owner root)]
                (is (= "completed" (:status run)))
                (is (= "Answer to Combine" (:response_text run)))
                (is (not-any? long-answer? (map :content (:messages (:request_snapshot run)))))))
            (testing "4.5.5: both transcripts show the compaction"
              (is (= 2 (count (filter #(= "compact" (:assistant_event/type %))
                                (assistant/messages* nil owner a)))))
              (let [rows (assistant/messages* nil owner root)]
                (is (= ["ABC" "Answer to ABC" "LONG one" "LONG one" "Combine" "Answer to Combine"]
                      (mapv #(or (:assistant_event/chat_title %) (:assistant_messages/content %))
                        rows))
                  "the rows name the branch and sit before the turn they were made for")))))))))

(deftest an-overflow-error-compacts-once-and-sends-a-new-request
  (with-users [owner]
    (let [topic-id (create-topic! owner)
          !requests (atom [])]
      (with-provider {:!requests !requests
                      :fail-first-answer nil}
        (fn []
          (let [{root :chat-id} (send! owner topic-id nil "ABC")
                _ (send! owner topic-id root "/fork about A")
                _ (reset! !requests [])]
            (with-provider {:!requests !requests
                            :fail-first-answer
                            (ex-info "This endpoint's maximum context length is 1000 tokens."
                              {:type :freememo.openrouter/api-error :status 400})}
              (fn [] (send! owner topic-id root "Combine")))
            (let [[failed retried] (take-last 2 (db/get-assistant-runs-for-chat owner root))]
              (is (= [:answer :compact :answer] (mapv request-kind @!requests))
                "4.5.6: one compaction, then one new request")
              (is (= "failed" (:status failed)))
              (is (= "completed" (:status retried)))
              (is (= (:run_uuid failed) (:retry_of_run_uuid retried)))
              (is (= "Answer to Combine" (:response_text retried))))))))))

(deftest a-request-too-large-after-every-compaction-fails-and-can-be-retried
  (with-users [owner]
    (let [topic-id (create-topic! owner)]
      (with-provider {:!requests (atom [])}
        (fn []
          (let [{root :chat-id} (send! owner topic-id nil "ABC")]
            (with-redefs [budget/budget-tokens (fn [_] 10)]
              (send! owner topic-id root "Too big")
              (let [run (latest-run owner root)]
                (is (= "failed" (:status run)))
                (is (= assistant/oversized-answer-error (:error_text run)))
                (testing "4.5.7: Retry builds the request again"
                  (let [r (assistant/retry-answer-run! owner (:run_uuid run) "retry-1")]
                    (is (:ok r))
                    (settle! owner root)
                    (is (= (:run_uuid run)
                          (:retry_of_run_uuid (latest-run owner root))))))))))))))

(deftest a-manual-compact-in-a-fork-covers-only-its-own-rows
  (with-users [owner]
    (let [topic-id (create-topic! owner)]
      (with-provider {:!requests (atom [])}
        (fn []
          (let [{root :chat-id} (send! owner topic-id nil "ABC")
                {a :chat-id} (send! owner topic-id root "/fork about A")
                r (send! owner topic-id a "/compact")]
            (is (:ok r))
            (let [checkpoint (db/latest-assistant-checkpoint owner a "own")]
              (is (some? checkpoint))
              (is (> (:covered_from_message_id checkpoint)
                    (:fork_message_id (chat-row a)))
                "4.5.8: the range starts after the fork point")
              (is (nil? (db/latest-assistant-checkpoint owner a "inherited"))))))))))

;; ── §4.1.4: account transfer ───────────────────────────────────────────────

(defn- delete-tree! [^File dir]
  (doseq [f (reverse (file-seq dir))] (.delete ^File f)))

(deftest a-fork-tree-survives-export-and-import
  (with-users [owner target]
    (let [topic-id (create-topic! owner)
          target-parent (create-topic! target)]
      (with-provider {:!requests (atom [])}
        (fn []
          (let [{root :chat-id} (send! owner topic-id nil "ABC")
                {a :chat-id} (send! owner topic-id root "/fork about A")
                _ (send! owner topic-id a "/fork deeper in A")
                out (ByteArrayOutputStream.)
                _ (account-export/write-archive! db/ds owner out "https://test")
                dir (archive/extract-to-temp-dir! (.toByteArray out) {:prefix "forks-test"})]
            (try
              (let [r (account-import/import-archive! db/ds target dir target-parent {})
                    chats (jdbc/execute! db/ds
                            ["SELECT id, title, parent_chat_id, fork_message_id
                                FROM assistant_chats WHERE user_id = ?" target]
                            opts)
                    by-title (into {} (map (juxt :title identity)) chats)
                    imported-a (by-title "about A")
                    imported-a1 (by-title "deeper in A")
                    message-chat (fn [id] (:chat_id (db/get-assistant-message target id)))]
                (is (:ok r))
                (is (= 3 (count chats)))
                (is (nil? (:parent_chat_id (by-title "ABC"))))
                (is (= (:id (by-title "ABC")) (:parent_chat_id imported-a)))
                (is (= (:id imported-a) (:parent_chat_id imported-a1)))
                (is (= (:id (by-title "ABC")) (message-chat (:fork_message_id imported-a))))
                (is (= (:id imported-a) (message-chat (:fork_message_id imported-a1))))
                (is (= [true true true true]
                      (->> (assistant/messages* nil target (:id imported-a1))
                        (filter :assistant_messages/inherited?)
                        (mapv :assistant_messages/inherited?)))
                  "the imported grandchild still inherits both ancestors"))
              (finally (delete-tree! dir)))))))))
