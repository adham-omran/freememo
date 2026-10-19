(ns freememo.target-commands-test
  "plans/custom-context-menu.md §2: every queued target command has a server
   effect, and each effect checks ownership before it writes. The database
   tests print SKIP and pass when no PostgreSQL is reachable (the `db-up?`
   guard the other DB suites use)."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.commands :as commands]
            [freememo.db :as db]
            [freememo.optimistic :as opt]
            [freememo.target-commands]))

(deftest every-queued-target-command-has-a-server-effect
  (let [queued (for [[id entry] commands/registry
                     :when (and (:target entry) (= :queue (commands/exec-mode entry)))]
                 id)
        methods (set (keys (methods opt/run-command!)))]
    (is (seq queued))
    (doseq [id queued]
      (is (contains? methods id) (str id " has no run-command! method")))))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(def ^:private plain {:builder-fn rs/as-unqualified-maps})
(defn- one [sql & params] (jdbc/execute-one! db/ds (into [sql] params) plain))

(defn- new-user! []
  (let [tag (str "target-cmd-" (System/nanoTime))]
    (:id (one "INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
           tag (str tag "@example.test")))))

(defn- new-topic! [uid title]
  (:id (one "INSERT INTO topics (user_id, parent_id, title, kind)
             VALUES (?, NULL, ?, 'basic') RETURNING id" uid title)))

(defn- topic-row [id] (one "SELECT title, priority, staged_delete_id FROM topics WHERE id = ?" id))

(defn- run! [uid type payload]
  (opt/run-command! uid {:id (random-uuid) :type type :payload payload}))

(defn- target [id & {:as more}]
  (merge {:kind :topic :subjects [{:topic-id id}]} more))

(defmacro ^:private with-two-users [[owner other topic] & body]
  `(if-not (db-up?)
     (println "SKIP target-commands: no Postgres reachable.")
     (let [~owner (new-user!) ~other (new-user!) ~topic (new-topic! ~owner "Mine")]
       (try ~@body
            (finally
              (doseq [u# [~owner ~other]]
                (try (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" u#])
                     (catch Exception _# nil))))))))

(deftest ^:db rename-checks-the-owner
  (with-two-users [owner other topic]
    (testing "2.1.2: another user's topic is not renamed"
      (is (= :denied (db/rename-topic! other topic "Theirs")))
      (is (false? (:ok? (run! other :rename-topic (target topic :input "Theirs")))))
      (is (= "Mine" (:title (topic-row topic)))))
    (testing "the owner renames"
      (is (:ok? (run! owner :rename-topic (target topic :input "Renamed"))))
      (is (= "Renamed" (:title (topic-row topic)))))))

(deftest ^:db priority-is-clamped-to-its-range
  (with-two-users [owner _other topic]
    (testing "2.1.5: 150 saves as 100"
      (is (:ok? (run! owner :set-topic-priority (target topic :input 150))))
      (is (== 100 (:priority (topic-row topic)))))))

(deftest ^:db no-effect-writes-another-users-topic
  (with-two-users [owner other topic]
    (doseq [type [:mark-topic-done :dismiss :delete-topic :move-topic-to-top-level
                  :set-topic-priority]]
      (is (false? (:ok? (run! other type (target topic :input 1))))
        (str type " refuses a topic the user does not own")))
    (is (nil? (:staged_delete_id (topic-row topic))) "nothing was staged")
    (is (false? (:ok? (run! other :add-pin {:topic-id topic :media-id 1 :placement "front"}))))))
