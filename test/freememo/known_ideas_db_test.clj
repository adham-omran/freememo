(ns freememo.known-ideas-db-test
  "Known ideas contracts against PostgreSQL (plans/known-ideas.md §1, §2.1,
   §2.4). Every test skips when the database is down."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [freememo.db :as db]
            [freememo.known-ideas :as ki]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})

(defn- create-user! []
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
          (str "known-ideas-test-" (random-uuid)) (str (random-uuid) "@example.test")]
         opts)))

(defn- topic! [user-id kind parent-id repo-path]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, kind, title, parent_id, repo_path)
           VALUES (?, ?, ?, ?, ?) RETURNING id"
          user-id kind (str kind " test") parent-id repo-path]
         opts)))

(defmacro ^:private with-users [syms & body]
  `(if-not (db-up?)
     (is true "PostgreSQL is unavailable; integration assertion skipped")
     (do (#'db/setup-schema-question-pins!)
         (#'db/setup-schema-known-ideas!)
         (let [~@(mapcat (fn [s] [s `(create-user!)]) syms)]
           (try ~@body
             (finally
               (jdbc/execute! db/ds (into [(str "DELETE FROM users WHERE id IN ("
                                             (clojure.string/join "," (repeat ~(count syms) "?"))
                                             ")")]
                                      ~(vec syms)))))))))

(defn- learned-idea!
  "A repository file with a map whose root `label` has a pin, a citation and
   an entity. Post: {:repo :file :map-id :root-id :entity}."
  [u label]
  (let [repo (topic! u "repo" nil nil)
        commit (topic! u "commit" repo "")
        file (topic! u "code" commit "docs/a.md")
        map-id (db/ensure-question-map! u repo "docs/a.md")
        entity (first (db/insert-kg-entities! u [label]))]
    (db/create-question-pin! u map-id {:quote "A port declares a signature"
                                       :snapshot-topic-id file :question "Why?"})
    (db/apply-question-map-plan! u map-id
      {:new-roots [{:label label :in-document? true :entity-id entity
                    :citations [{:quote "slots carry a closure" :anchor "ports"}]}]
       :pin-roots {1 label} :learned #{} :next label})
    {:repo repo :file file :map-id map-id :entity entity
     :root-id (:id (first (db/question-roots u map-id)))}))

(defn- proof-run! [u repo answer]
  (let [chat (db/create-assistant-chat! u repo "Proof" "tutor")
        message! (fn [role content]
                   (:id (jdbc/execute-one! db/ds ["INSERT INTO assistant_messages (chat_id, role, content)
                                                  VALUES (?, ?, ?) RETURNING id" chat role content]
                          opts)))
        msg (message! "user" answer)
        reply (message! "assistant" "Right.")
        uuid (str (random-uuid))]
    (jdbc/execute! db/ds ["INSERT INTO assistant_runs (run_uuid, chat_id, kind, status,
                                                       user_message_id, assistant_message_id,
                                                       provider_session_id)
                           VALUES (?, ?, 'answer', 'completed', ?, ?, 'test')" uuid chat msg reply])
    uuid))

(defn- basic-row [file repo q]
  {:topic_id file :root_topic_id repo :kind "basic" :question q :answer "a" :cloze nil})

(deftest a-tutor-mark-names-its-proof-run
  (with-users [u]
    (let [{:keys [repo map-id entity]} (learned-idea! u "Ports")
          first-run (proof-run! u repo "A port has four axes.")
          second-run (proof-run! u repo "Shape, degree, type and timing.")]
      (db/apply-question-map-plan! u map-id
        {:new-roots [] :pin-roots {} :learned #{"Ports"} :next nil :run-uuid first-run})
      (testing "§1.1: the mark names the run, and the page reads its user message"
        (is (= "A port has four axes." (get-in (db/known-idea u entity) [:proof :answer]))))
      (testing "a later confirmation names the later run"
        (db/apply-question-map-plan! u map-id
          {:new-roots [] :pin-roots {} :learned #{"Ports"} :next nil :run-uuid second-run})
        (is (= "Shape, degree, type and timing."
              (get-in (db/known-idea u entity) [:proof :answer])))))))

(deftest the-list-holds-each-users-own-ideas
  (with-users [u other]
    (let [{:keys [entity file repo]} (learned-idea! u "Ports")
          _ (learned-idea! other "Other idea")]
      (is (empty? (db/known-ideas u)) "§2 Terms: no mark, no known idea")
      (db/set-known-mark! u entity "self")
      (let [[idea :as ideas] (db/known-ideas u)]
        (is (= 1 (count ideas)) "§1.5.3: another user's idea never lists")
        (is (= ["self"] (:sources idea)))
        (is (= "docs/a.md" (:repo_path (first (:origin-files idea)))))
        (is (:material? idea))
        (is (= [0 nil false] ((juxt :card_count :next_check :due?) idea))))
      (testing "§2.4: cards made for the idea are linked in the insert's transaction"
        (let [ids (db/insert-flashcards-for-entity! u entity
                    [(basic-row file repo "q1") (basic-row file repo "q2")])]
          (is (= 2 (count ids)))
          (is (= 2 (:card_count (first (db/known-ideas u)))))
          (testing "§2.4.1: a skipped duplicate is linked to the card that holds its key"
            (let [plain (first (db/insert-flashcards! [(basic-row file repo "q3")]))]
              (is (empty? (db/insert-flashcards-for-entity! u entity [(basic-row file repo "q3")])))
              (is (= (set (conj ids plain)) (set (db/entity-card-ids u [entity]))))))
          (testing "another user's entity writes no card"
            (is (thrown? Exception (db/insert-flashcards-for-entity! other entity
                                     [(basic-row file repo "q4")])))
            (is (zero? (:count (jdbc/execute-one! db/ds
                                 ["SELECT count(*) FROM flashcards WHERE question = 'q4'"] opts)))))
          (testing "§1.4: a check draws every card, due or not, and only the owner's"
            (is (= (mapv (fn [id] [:card id 0]) (sort ids)) (db/draw-card-check u ids)))
            (is (empty? (db/draw-card-check other ids))))
          (testing "§1.2: deleting a card removes its link"
            (jdbc/execute! db/ds ["DELETE FROM flashcards WHERE id = ?" (first ids)])
            (is (not (contains? (set (db/entity-card-ids u [entity])) (first ids))))))))))

(deftest one-idea-is-its-owners-only
  (with-users [u other]
    (let [{:keys [entity]} (learned-idea! u "Ports")]
      (is (= "Ports" (:label (db/known-idea u entity))))
      (is (nil? (db/known-idea other entity)) "§1.6"))))

(deftest material-comes-from-quotes-and-pins
  (with-users [u other]
    (let [{:keys [entity file repo]} (learned-idea! u "Ports")
          m (db/idea-material u entity)]
      (testing "§2.1: each quote once, the pin questions, the newest pin's file"
        (is (= ["slots carry a closure" "A port declares a signature"] (:quotes m)))
        (is (= ["Why?"] (:questions m)))
        (is (= [file repo] ((juxt :topic-id :root-topic-id) m))))
      (is (nil? (db/idea-material other entity)))
      (testing "D6: an idea with no root has no material"
        (let [lone (first (db/insert-kg-entities! u ["Walkthrough idea"]))]
          (db/set-known-mark! u lone "tutor")
          (is (nil? (db/idea-material u lone)))
          (is (false? (:material? (first (filter #(= lone (:entity_id %)) (db/known-ideas u)))))))))))

(deftest not-learned-yet-and-undo
  (with-users [u other]
    (let [{:keys [entity map-id]} (learned-idea! u "Ports")]
      (db/apply-question-map-plan! u map-id
        {:new-roots [] :pin-roots {} :learned #{"Ports"} :next nil})
      (db/set-known-mark! u entity "self")
      (testing "§1.8: Undo my mark keeps the tutor's mark"
        (is (= 1 (db/delete-known-mark! u entity "self")))
        (is (= ["tutor"] (:sources (first (db/known-ideas u))))))
      (testing "§1.7: Not learned yet removes the marks and reopens the root"
        (is (nil? (db/unlearn-entity! other entity)))
        (is (= 1 (db/unlearn-entity! u entity)))
        (is (empty? (db/known-ideas u)))
        (let [root (first (db/question-roots u map-id))]
          (is (= "open" (:status root)))
          (is (nil? (:learned_at root))))))))

(deftest the-views-read-text-not-times
  (with-users [u]
    (let [{:keys [entity map-id]} (learned-idea! u "Ports")]
      (db/apply-question-map-plan! u map-id
        {:new-roots [] :pin-roots {} :learned #{"Ports"} :next nil})
      (testing "§3.2: a row says how the idea is known, where from, and when checked"
        (let [row (first (ki/list-rows u))]
          (is (str/starts-with? (:how row) "Tutor confirmed ("))
          (is (= "docs/a.md (repo test)" (:from row)))
          (is (= "Never checked" (:checked row)))))
      (testing "§5.3.3: the page lists only stored events, oldest first, dated as text"
        (let [page (ki/idea-page u entity)
              texts (map :text (:events page))]
          (is (= ["You pinned passage 1 in docs/a.md: Why?" "The tutor confirmed it in docs/a.md."]
                texts))
          (is (every? string? (map :date (:events page))))
          (is (:material? page)))))))

;; ── The idea trail (plans/idea-trail.md) ───────────────────────────

(defn- answer-run!
  "A turn sent through `create-assistant-answer-run!`, then completed.
   Post: {:id :uuid}."
  [chat question map-id]
  (let [uuid (str (random-uuid))
        {:keys [run-id]} (db/create-assistant-answer-run! chat question nil
                           {:run-uuid uuid :provider-session-id "test"
                            :request-snapshot {} :question-map-id map-id})]
    (jdbc/execute! db/ds ["UPDATE assistant_runs SET status = 'completed',
                                  completed_at = CURRENT_TIMESTAMP WHERE id = ?" run-id])
    {:id run-id :uuid uuid}))

(defn- old-run!
  "A completed turn as the app stored it before the idea trail: no map named.
   `created` and `completed` are timestamptz text. A `map-message?` run's
   request carried a question-map message, and `evidence-of` is the
   [document-topic-id title] of its one evidence row. Post: the run's id."
  [chat question created completed map-message? evidence-of]
  (let [message! (fn [role content]
                   (:id (jdbc/execute-one! db/ds ["INSERT INTO assistant_messages (chat_id, role, content)
                                                  VALUES (?, ?, ?) RETURNING id" chat role content]
                          opts)))
        snapshot (str "{\"messages\": [{\"role\": \"system\", \"content\": \""
                   (if map-message? "# Question map\\n\\n## Roots" "# Something else")
                   "\"}]}")
        id (:id (jdbc/execute-one! db/ds
                  ["INSERT INTO assistant_runs (run_uuid, chat_id, kind, status, user_message_id,
                                                assistant_message_id, provider_session_id,
                                                request_snapshot, created_at, completed_at)
                    VALUES (?, ?, 'answer', 'completed', ?, ?, 'test', CAST(? AS jsonb),
                            CAST(? AS timestamptz)::timestamp, CAST(? AS timestamptz)::timestamp)
                    RETURNING id"
                   (str (random-uuid)) chat (message! "user" question) (message! "assistant" "Reply.")
                   snapshot created completed]
                  opts))]
    (when-let [[doc title] evidence-of]
      (jdbc/execute! db/ds ["INSERT INTO assistant_evidence (run_id, citation_key, document_topic_id,
                                                             document_title, display_label)
                             VALUES (?, 'E1', ?, ?, ?)" id doc title title]))
    id))

(deftest a-trail-runs-from-the-creating-turn-to-the-proof
  (with-users [u other]
    (let [repo (topic! u "repo" nil nil)
          map-id (db/ensure-question-map! u repo "docs/a.md")
          map-b (db/ensure-question-map! u repo "docs/b.md")
          other-map (db/ensure-question-map! other (topic! other "repo" nil nil) "docs/a.md")
          entity (first (db/insert-kg-entities! u ["Ports"]))
          chat (db/create-assistant-chat! u repo "Ports chat" "tutor")
          ask (answer-run! chat "I pinned these passages." map-id)
          _ (db/apply-question-map-plan! u map-id
              {:new-roots [{:label "Ports" :in-document? true :entity-id entity :citations []}]
               :pin-roots {} :learned #{} :next "Ports" :run-uuid (:uuid ask)})
          middle (answer-run! chat "Is it the signature?" map-id)
          _ (answer-run! chat "Something about b." map-b)
          foreign (answer-run! chat "Not my map." other-map)
          proof (answer-run! chat "Shape, degree, type and timing." map-id)
          _ (db/apply-question-map-plan! u map-id
              {:new-roots [] :pin-roots {} :learned #{"Ports"} :next nil :run-uuid (:uuid proof)})
          _ (answer-run! chat "After the proof." map-id)]
      (testing "§3.1: a run names its map only when the chat's user owns it"
        (is (= map-id (:question_map_id (jdbc/execute-one! db/ds
                                          ["SELECT question_map_id FROM assistant_runs WHERE id = ?"
                                           (:id middle)] opts))))
        (is (nil? (:question_map_id (jdbc/execute-one! db/ds
                                      ["SELECT question_map_id FROM assistant_runs WHERE id = ?"
                                       (:id foreign)] opts)))))
      (testing "§3.1: a new root names the run that created it"
        (is (= (:id ask) (:created_run_id (jdbc/execute-one! db/ds
                                            ["SELECT created_run_id FROM question_roots WHERE map_id = ?"
                                             map-id] opts)))))
      (testing "§3.3: the trail holds the turns on the map, from the creating turn to the proof"
        (let [{:keys [turns inferred?]} (db/idea-trail u entity)]
          (is (= ["I pinned these passages." "Is it the signature?" "Shape, degree, type and timing."]
                (map :question turns)))
          (is (false? inferred?))
          (is (nil? (db/idea-trail other entity)))))
      (testing "the page shows the first turn, the count and the chat, as text"
        (let [{:keys [trail proof]} (ki/idea-page u entity)]
          (is (= "I pinned these passages." (:first-question trail)))
          (is (= 3 (:turn-count trail)))
          (is (= [{:chat_id chat :chat_title "Ports chat" :turns 3}] (:chats trail)))
          (is (string? (:first-date trail)))
          (is (false? (:inferred? proof)))))
      (testing "Open opens only a live chat of the user"
        (is (= repo (ki/open-chat! u chat)))
        (is (nil? (ki/open-chat! other chat)))))))

(deftest old-confirmations-get-an-inferred-trail
  (with-users [u]
    (let [repo (topic! u "repo" nil nil)
          map-id (db/ensure-question-map! u repo "docs/a.md")
          entity (first (db/insert-kg-entities! u ["Ports"]))
          chat (db/create-assistant-chat! u repo "Old chat" "tutor")
          file [repo "docs/a.md"]]
      (db/apply-question-map-plan! u map-id
        {:new-roots [{:label "Ports" :in-document? true :entity-id entity :citations []}]
         :pin-roots {} :learned #{} :next "Ports"})
      (db/apply-question-map-plan! u map-id
        {:new-roots [] :pin-roots {} :learned #{"Ports"} :next nil})
      (jdbc/execute! db/ds ["UPDATE question_roots SET created_at = '2026-10-06T10:00:00Z'
                              WHERE map_id = ?" map-id])
      (jdbc/execute! db/ds ["UPDATE known_marks SET created_at = '2026-10-06T11:00:00Z'
                              WHERE entity_id = ?" entity])
      (let [ask (old-run! chat "I pinned these passages." "2026-10-06T09:59:40Z"
                  "2026-10-06T10:00:05Z" true file)
            middle (old-run! chat "Is it the signature?" "2026-10-06T10:30:00Z"
                     "2026-10-06T10:30:20Z" true file)
            _ (old-run! chat "No map here." "2026-10-06T10:40:00Z" "2026-10-06T10:40:20Z" false file)
            proof (old-run! chat "Shape, degree, type and timing." "2026-10-06T10:59:40Z"
                    "2026-10-06T11:00:04Z" true file)]
        (jdbc/with-transaction [tx db/ds] (db/infer-idea-trails! tx [map-id]))
        (testing "§3.2: old runs on the map name it, and the run with no map message does not"
          (is (= #{ask middle proof}
                (set (map :id (jdbc/execute! db/ds ["SELECT id FROM assistant_runs WHERE question_map_id = ?"
                                                    map-id] opts))))))
        (testing "§3.2: the root and the mark name the runs that completed just after them"
          (is (= [ask true] ((juxt :created_run_id :created_run_inferred)
                             (jdbc/execute-one! db/ds ["SELECT created_run_id, created_run_inferred
                                                          FROM question_roots WHERE map_id = ?" map-id]
                               opts))))
          (is (= [proof true] ((juxt :proof_run_id :proof_run_inferred)
                               (jdbc/execute-one! db/ds ["SELECT proof_run_id, proof_run_inferred
                                                            FROM known_marks WHERE entity_id = ?" entity]
                                 opts)))))
        (testing "§3.3: the trail and the answer say they were inferred"
          (let [{:keys [turns inferred?]} (db/idea-trail u entity)]
            (is (= [ask middle proof] (map :run_id turns)))
            (is (true? inferred?)))
          (is (true? (get-in (db/known-idea u entity) [:proof :inferred?]))))
        (testing "a recorded confirmation clears the proof's inferred flag"
          (let [later (answer-run! chat "Again." map-id)]
            (db/apply-question-map-plan! u map-id
              {:new-roots [] :pin-roots {} :learned #{"Ports"} :next nil :run-uuid (:uuid later)})
            (is (false? (get-in (db/known-idea u entity) [:proof :inferred?])))))))))
