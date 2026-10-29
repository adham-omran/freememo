(ns freememo.anki-id-array-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [freememo.db :as db]
            [next.jdbc :as jdbc]))

(use-fixtures :each
  (fn [run]
    (jdbc/with-transaction [tx db/ds {:rollback-only true}]
      (with-redefs [db/ds tx] (run)))))

(defn- fixture []
  (let [user! #(-> (jdbc/execute-one! db/ds
                    ["INSERT INTO users(username) VALUES (?) RETURNING id"
                     (str "audit-id-array-" (random-uuid))]) :users/id)
        user-id (user!) other-user (user!)
        topic! (fn [uid parent]
                 (:topics/id (jdbc/execute-one! db/ds
                               ["INSERT INTO topics(user_id,title,parent_id) VALUES (?,'ID array',?) RETURNING id"
                                uid parent])))
        root (topic! user-id nil) child (topic! user-id root)
        outside (topic! user-id nil) foreign-root (topic! other-user nil)
        card! (fn [topic root-id]
                (:flashcards/id (jdbc/execute-one! db/ds
                                  ["INSERT INTO flashcards(topic_id,root_topic_id,kind,question,answer)
                                    VALUES (?,?,'basic','Question','Answer') RETURNING id" topic root-id])))
        inside (card! child root) sibling (card! outside outside)
        foreign (card! foreign-root foreign-root)]
    {:user user-id :root root :child child :inside inside :outside sibling :foreign foreign
     :ids (into [inside sibling foreign] (range -100000 0))}))

(defn- conflict-ids []
  (set (map :flashcards/id (jdbc/execute! db/ds
                            ["SELECT id FROM flashcards WHERE anki_conflict_at IS NOT NULL"])) ))

(deftest large-general-card-read-preserves-ownership
  (let [{:keys [user inside outside ids]} (fixture)]
    (is (= #{inside outside} (set (map :flashcards/id (db/get-flashcards-by-ids user ids)))))
    (is (= [] (db/get-flashcards-by-ids user [])))
    (is (= [] (db/get-flashcards-by-ids user nil)))))

(deftest large-reconcile-read-preserves-both-scopes
  (let [{:keys [user root child inside outside ids]} (fixture)]
    (is (= #{inside outside} (set (map :flashcards/id (db/get-cards-for-reconcile user ids)))))
    (is (= [inside] (mapv :flashcards/id (db/get-cards-for-reconcile user ids root))))
    (is (= [inside] (mapv :flashcards/id (db/get-cards-for-reconcile user nil child))))
    (is (= #{inside outside} (set (map :flashcards/id (db/get-cards-for-reconcile user nil)))))
    (is (= [] (db/get-cards-for-reconcile user [])))))

(deftest large-subtree-gate-excludes-outside-and-foreign-cards
  (let [{:keys [user root child inside ids]} (fixture)]
    (is (= [inside] (db/card-ids-in-subtree user root ids)))
    (is (= [inside] (db/card-ids-in-subtree user child ids)))
    (is (= [] (db/card-ids-in-subtree user nil ids)))
    (is (= [] (db/card-ids-in-subtree user root nil)))
    (is (= [] (db/card-ids-in-subtree user root [])))))

(deftest large-conflict-set-preserves-ownership-and-exact-membership
  (let [{:keys [user inside outside foreign ids]} (fixture)]
    (db/set-anki-conflicts! user ids)
    (is (= #{inside outside} (set (filter #{inside outside foreign} (conflict-ids)))))
    (db/set-anki-conflicts! user (into [inside foreign] (range -100000 0)))
    (is (= #{inside} (set (filter #{inside outside foreign} (conflict-ids)))))))

(deftest scoped-conflict-set-keeps-other-branches-unchanged
  (let [{:keys [user root inside outside foreign ids]} (fixture)]
    (db/set-anki-conflicts! user ids root)
    (is (= #{inside} (set (filter #{inside outside foreign} (conflict-ids)))))
    (db/set-anki-conflicts! user [outside])
    (db/set-anki-conflicts! user ids root)
    (is (= #{inside outside} (set (filter #{inside outside foreign} (conflict-ids)))))
    (db/set-anki-conflicts! user [] root)
    (is (= #{outside} (set (filter #{inside outside foreign} (conflict-ids)))))
    (db/set-anki-conflicts! user nil)
    (is (empty? (filter #{inside outside foreign} (conflict-ids))))))

(deftest scoped-conflict-set-cannot-mark-an-unscanned-branch
  (let [{:keys [user root inside outside foreign]} (fixture)]
    (db/set-anki-conflicts! user [inside outside foreign] root)
    (is (= #{inside} (set (filter #{inside outside foreign} (conflict-ids)))))))

(deftest large-conflict-clear-leaves-other-cards-and-users-unchanged
  (let [{:keys [user inside outside foreign]} (fixture)]
    (jdbc/execute! db/ds ["UPDATE flashcards SET anki_conflict_at=now() WHERE id=ANY(?)"
                         (long-array [inside outside foreign])])
    (db/clear-anki-conflicts! user [])
    (db/clear-anki-conflicts! user nil)
    (is (= #{inside outside foreign} (set (filter #{inside outside foreign} (conflict-ids)))))
    (db/clear-anki-conflicts! user (into [inside foreign] (range -100000 0)))
    (is (= #{outside foreign} (set (filter #{inside outside foreign} (conflict-ids)))))))
