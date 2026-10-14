(ns freememo.question-map-db-test
  "Question map contracts against PostgreSQL (plans/anchored-questions.md §6.1,
   §6.3, §6.4, plans/question-map-tab.md §4.4, §4.5, and
   plans/pin-numbering-reuse-and-tombstones.md). Every test skips when the
   database is down."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.db :as db]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})

(defn- create-user! []
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
          (str "question-map-test-" (random-uuid))
          (str (random-uuid) "@example.test")]
         opts)))

(defn- topic! [user-id kind parent-id repo-path]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, kind, title, parent_id, repo_path)
           VALUES (?, ?, ?, ?, ?) RETURNING id"
          user-id kind (str kind " test") parent-id repo-path]
         opts)))

(defn- repo-file!
  "A repo node, one commit under it, and `path` as a code file in that commit.
   Post: {:repo :commit :file}."
  [user-id path]
  (let [repo (topic! user-id "repo" nil nil)
        commit (topic! user-id "commit" repo "")
        file (topic! user-id "code" commit path)]
    {:repo repo :commit commit :file file}))

(defmacro ^:private with-users [syms & body]
  `(if-not (db-up?)
     (is true "PostgreSQL is unavailable; integration assertion skipped")
     (do (#'db/setup-schema-question-pins!)
         (#'db/setup-schema-known-ideas!)
         (#'db/setup-schema-pin-numbering!)
         (let [~@(mapcat (fn [s] [s `(create-user!)]) syms)]
           (try ~@body
             (finally
               (jdbc/execute! db/ds (into [(str "DELETE FROM users WHERE id IN ("
                                             (clojure.string/join "," (repeat ~(count syms) "?"))
                                             ")")]
                                      ~(vec syms)))))))))

(deftest the-document-key-is-the-repo-node-and-path
  (with-users [u other]
    (let [{:keys [repo commit file]} (repo-file! u "docs/ir-spec.md")
          next-commit (topic! u "commit" repo "")
          next-file (topic! u "code" next-commit "docs/ir-spec.md")]
      (is (= {:repo_topic_id repo :repo_path "docs/ir-spec.md"} (db/question-doc-key u file)))
      (testing "§6.1.2: two snapshots of one file share one key"
        (is (= (db/question-doc-key u file) (db/question-doc-key u next-file))))
      (is (nil? (db/question-doc-key other file)) "another user's file has no key")
      (is (nil? (db/question-doc-key u commit)) "a commit node is not a file"))))

(defn- entity! [user-id label]
  (first (db/insert-kg-entities! user-id [label])))

(defn- tombstone-count [map-id]
  (:n (jdbc/execute-one! db/ds
        ["SELECT count(*)::int AS n FROM question_pin_tombstones WHERE map_id = ?" map-id]
        opts)))

(deftest a-label-is-reused-until-the-tutor-sees-it
  (with-users [u other]
    (let [{:keys [repo file]} (repo-file! u "a.md")
          map-id (db/ensure-question-map! u repo "a.md")
          pin! #(db/create-question-pin! u map-id {:snapshot-topic-id file :quote %})
          [_p1 p2 _p3] (mapv pin! ["one" "two" "three"])]
      (is (= map-id (db/ensure-question-map! u repo "a.md")) "one map per key")
      (is (nil? (db/ensure-question-map! other repo "a.md")) "only the owner makes a map")
      (testing "§7.1: an unseen pin's label goes to the next pin"
        (is (= 1 (db/delete-question-pin! u (:id p2))))
        (is (zero? (tombstone-count map-id)) "§6.3: an unseen delete leaves nothing")
        (is (= 2 (:label (pin! "two again")))))
      (testing "§5.2: the tutor read marks every open pin seen"
        (is (= [1 2 3] (mapv :label (db/open-question-pins-for-tutor! u map-id))))
        (is (every? :tutor_seen_at (jdbc/execute! db/ds
                                     ["SELECT tutor_seen_at FROM question_pins WHERE map_id = ?" map-id]
                                     opts))))
      (testing "§7.1: a seen pin's label is never given out again"
        (let [two (first (filter #(= 2 (:label %)) (db/question-pins u map-id)))]
          (is (= 1 (db/delete-question-pin! u (:id two)))))
        (is (= [1 3] (mapv :label (db/question-pins u map-id))) "§6: live pins only")
        (is (= [{:label 2 :quote "two again" :question ""}]
               (db/question-pin-tombstones u map-id)) "§6.3")
        (is (= 4 (:label (pin! "four")))))
      (is (empty? (db/question-pin-tombstones other map-id)) "only the owner reads tombstones"))))

(deftest the-tutor-read-marks-open-pins-only
  (with-users [u]
    (let [{:keys [repo]} (repo-file! u "a.md")
          map-id (db/ensure-question-map! u repo "a.md")
          open (db/create-question-pin! u map-id {:quote "open"})
          closed (db/create-question-pin! u map-id {:quote "closed"})]
      (db/set-question-pin-status! u (:id closed) "closed")
      (is (= [(:id open)] (mapv :id (db/open-question-pins-for-tutor! u map-id))))
      (testing "a closed pin was never named, so its label is free after a delete"
        (db/delete-question-pin! u (:id closed))
        (is (zero? (tombstone-count map-id)))
        (is (= 2 (:label (db/create-question-pin! u map-id {:quote "again"}))))))))

(deftest concurrent-creates-get-distinct-labels
  (with-users [u]
    (let [{:keys [repo]} (repo-file! u "a.md")
          map-id (db/ensure-question-map! u repo "a.md")
          labels (->> (range 8)
                   (mapv (fn [i] (future (:label (db/create-question-pin! u map-id {:quote (str "q" i)})))))
                   (mapv deref))]
      (is (= (set (range 1 9)) (set labels)) "§7.1: one label each, none failed"))))

(deftest a-tombstone-counts-nowhere-but-the-deleted-group
  (with-users [u]
    (let [{:keys [repo file]} (repo-file! u "a.md")
          map-id (db/ensure-question-map! u repo "a.md")
          e (entity! u "Ports")
          pin (db/create-question-pin! u map-id {:snapshot-topic-id file :quote "a port"
                                                 :question "What is a port?"})
          material? (fn [] (:material? (first (filter #(= e (:entity_id %)) (db/known-ideas u)))))]
      (db/apply-question-map-plan! u map-id
        {:new-roots [{:label "Ports" :in-document? true :entity-id e}]
         :pin-roots {1 "Ports"} :learned #{} :next "Ports"})
      (db/set-known-mark! u e "self")
      (is (some? (db/idea-material u e)) "a live pin is material")
      (is (true? (material?)))
      (db/open-question-pins-for-tutor! u map-id)
      (is (= 1 (db/delete-question-pin! u (:id pin))))
      (is (= 1 (tombstone-count map-id)))
      (testing "§8.2: no card is made from a deleted quote"
        (is (nil? (db/idea-material u e))))
      (testing "§8.3: the list flag agrees"
        (is (false? (material?)))))))

(deftest the-migration-retires-legacy-gaps
  (with-users [u]
    (let [{:keys [repo]} (repo-file! u "a.md")
          map-id (db/ensure-question-map! u repo "a.md")
          other-map (db/ensure-question-map! u repo "b.md")
          legacy-pin! (fn [m label]
                        (jdbc/execute-one! db/ds
                          ["INSERT INTO question_pins (map_id, label, quote) VALUES (?, ?, 'q')"
                           m label]))]
      ;; The shape the old counter left: pins 1 and 2 deleted, 3 and 4 live.
      (jdbc/execute! db/ds ["UPDATE question_maps SET next_label = 5 WHERE id IN (?, ?)"
                            map-id other-map])
      (legacy-pin! map-id 3)
      (legacy-pin! map-id 4)
      (legacy-pin! other-map 4)
      (db/apply-question-map-plan! u other-map
        {:new-roots [{:label "Root" :in-document? true}] :pin-roots {} :learned #{} :next nil})
      (jdbc/with-transaction [tx db/ds]
        (db/retire-legacy-pin-labels! tx [map-id other-map]))
      (testing "§10.2: every gap below next_label is a tombstone with no quote"
        (is (= [{:label 1 :quote nil :question ""} {:label 2 :quote nil :question ""}]
               (db/question-pin-tombstones u map-id)))
        (is (= 5 (:label (db/create-question-pin! u map-id {:quote "new"})))))
      (testing "§10.1: pins are seen only in a map with a root"
        (let [seen (fn [m] (mapv :tutor_seen_at
                             (jdbc/execute! db/ds
                               ["SELECT tutor_seen_at FROM question_pins WHERE map_id = ? AND label < 5" m]
                               opts)))]
          (is (every? nil? (seen map-id)))
          (is (every? some? (seen other-map))))))))

(deftest ownership-guards-every-pin-write
  (with-users [u other]
    (let [{:keys [repo]} (repo-file! u "a.md")
          map-id (db/ensure-question-map! u repo "a.md")
          pin (db/create-question-pin! u map-id {:quote "q"})]
      (is (nil? (db/create-question-pin! other map-id {:quote "x"})))
      (is (zero? (db/update-question-pin-question! other (:id pin) "hijack")))
      (is (zero? (db/set-question-pin-status! other (:id pin) "closed")))
      (is (zero? (db/delete-question-pin! other (:id pin))))
      (is (empty? (db/question-pins other map-id)))
      (is (= 1 (db/set-question-pin-status! u (:id pin) "closed")))
      (is (= 1 (db/set-question-pin-status! u (:id pin) "open")) "§6.1.5: reopen"))))

(deftest a-plan-writes-roots-links-next-and-learned
  (with-users [u]
    (let [{:keys [repo]} (repo-file! u "a.md")
          map-id (db/ensure-question-map! u repo "a.md")
          _ (doseq [q ["shape, degree" "closure signature" "scalar <: fixed"]]
              (db/create-question-pin! u map-id {:quote q}))
          axes (entity! u "Signature axes")
          subtyping (entity! u "Subtyping")]
      (db/apply-question-map-plan! u map-id
        {:new-roots [{:label "Signature axes" :in-document? true
                      :citations [{:quote "A port declares" :anchor "47-ports"}]
                      :entity-id axes}
                     {:label "Subtyping" :in-document? false :citations []
                      :entity-id subtyping}]
         :pin-roots {1 "Signature axes" 2 "Signature axes" 3 "Subtyping"}
         :learned #{} :next "Signature axes"})
      (let [roots (db/question-roots u map-id)
            by-label (into {} (map (juxt :label identity)) roots)]
        (is (= "next" (:status (by-label "Signature axes"))))
        (is (= [{:quote "A port declares" :anchor "47-ports"}]
              (:citations (by-label "Signature axes"))))
        (is (every? :root_id (db/question-pins u map-id)) "§6.3.3: every pin placed"))
      (testing "§6.3.8: Teach me moves the one next root"
        (let [sub-id (:id (first (filter #(= "Subtyping" (:label %)) (db/question-roots u map-id))))]
          (is (db/set-question-root-next! u sub-id))
          (is (= {"Signature axes" "open" "Subtyping" "next"}
                (into {} (map (juxt :label :status)) (db/question-roots u map-id))))))
      (testing "§6.3.6 and §6.4.5: learned closes its pins and writes a tutor mark"
        (is (= {:closed-pins 2}
              (db/apply-question-map-plan! u map-id
                {:new-roots [] :pin-roots {} :learned #{"Signature axes"} :next nil})))
        (is (= ["closed" "closed" "open"] (mapv :status (db/question-pins u map-id))))
        (is (= ["tutor"] (:known_sources (first (filter #(= axes (:entity_id %))
                                                   (db/question-roots u map-id)))))))
      (testing "§6.3.4: a later plan keeps a learned root learned"
        (db/apply-question-map-plan! u map-id
          {:new-roots [{:label "Signature axes" :in-document? true :entity-id axes}]
           :pin-roots {} :learned #{} :next "Signature axes"})
        (is (= "learned" (:status (first (filter #(= "Signature axes" (:label %))
                                           (db/question-roots u map-id))))))))))

(deftest known-marks-are-global-and-removable
  (with-users [u other]
    (let [e (entity! u "Subtyping")]
      (is (db/set-known-mark! u e "self"))
      (is (db/set-known-mark! u e "self") "idempotent")
      (is (not (db/set-known-mark! other e "self")) "only the owner marks")
      (is (db/set-known-mark! u e "tutor"))
      (testing "§6.4.1: a root in another document sees the same mark"
        (let [{:keys [repo]} (repo-file! u "b.md")
              map-id (db/ensure-question-map! u repo "b.md")]
          (db/apply-question-map-plan! u map-id
            {:new-roots [{:label "Subtyping" :in-document? false :entity-id e}]
             :pin-roots {} :learned #{} :next nil})
          (is (= ["self" "tutor"] (:known_sources (first (db/question-roots u map-id)))))))
      (is (zero? (db/delete-known-marks! other e)))
      (is (= 2 (db/delete-known-marks! u e)) "§6.4.4: both sources go"))))

(deftest roots-outlive-their-entity
  (with-users [u]
    (let [{:keys [repo]} (repo-file! u "a.md")
          map-id (db/ensure-question-map! u repo "a.md")
          _ (db/create-question-pin! u map-id {:quote "q"})
          e (entity! u "Ports")]
      (db/set-known-mark! u e "self")
      (db/apply-question-map-plan! u map-id
        {:new-roots [{:label "Ports" :in-document? true :entity-id e}]
         :pin-roots {1 "Ports"} :learned #{} :next "Ports"})
      (jdbc/execute! db/ds ["DELETE FROM kg_entities WHERE id = ?" e])
      (testing "§6.4.2.4: the root stays, with no entity, and the pin keeps it"
        (let [root (first (db/question-roots u map-id))]
          (is (= "Ports" (:label root)))
          (is (nil? (:entity_id root)))
          (is (= (:id root) (:root_id (first (db/question-pins u map-id)))))
          (testing "a new entity is made from the root's label when needed"
            (let [e2 (db/ensure-question-root-entity! u (:id root))]
              (is (integer? e2))
              (is (= e2 (db/ensure-question-root-entity! u (:id root))))
              (is (true? (:from_diagnosis (jdbc/execute-one! db/ds
                                            ["SELECT from_diagnosis FROM kg_entities WHERE id = ?" e2]
                                            opts)))))))))))

(deftest diagnosis-entities-stay-out-of-the-knowledge-list
  (with-users [u]
    (let [_ (db/insert-kg-entities! u ["Diagnosed idea"] {:from-diagnosis? true})
          _ (entity! u "Extracted idea")
          labels (set (map :label (db/list-kg-entities u nil 100)))]
      (is (contains? labels "Extracted idea"))
      (is (not (contains? labels "Diagnosed idea")) "§6.4.2.3"))))

(defn- status-by-label [u map-id]
  (into {} (map (juxt :label :status)) (db/question-roots u map-id)))

(defn- three-roots!
  "A map with roots A (next), B and C (open), each with its own entity.
   Post: {:map-id :ids {label id} :entities {label entity-id}}."
  [u]
  (let [{:keys [repo]} (repo-file! u "a.md")
        map-id (db/ensure-question-map! u repo "a.md")
        entities (into {} (map (fn [l] [l (entity! u l)])) ["A" "B" "C"])]
    (db/apply-question-map-plan! u map-id
      {:new-roots (mapv (fn [l] {:label l :in-document? true :entity-id (entities l)})
                    ["A" "B" "C"])
       :pin-roots {} :learned #{} :next "A"})
    {:map-id map-id :entities entities
     :ids (into {} (map (juxt :label :id)) (db/question-roots u map-id))}))

(deftest i-know-this-on-the-current-root-moves-next-on
  (with-users [u]
    (let [{:keys [map-id ids entities]} (three-roots! u)]
      (db/set-known-mark! u (entities "B") "tutor")
      (testing "§4.4.5: next skips a root with a mark of either source"
        (is (db/mark-root-known! u (ids "A") (entities "A") nil))
        (is (= {"A" "open" "B" "open" "C" "next"} (status-by-label u map-id))))
      (testing "on an open root, the mark moves nothing"
        (is (db/mark-root-known! u (ids "B") (entities "B") nil))
        (is (= "next" ((status-by-label u map-id) "C"))))
      (testing "with no open root left, the map has no next root"
        (is (db/mark-root-known! u (ids "C") (entities "C") nil))
        (is (= #{"open"} (set (vals (status-by-label u map-id))))))
      (testing "another user's entity writes nothing"
        (with-users [other]
          (is (not (db/mark-root-known! other (ids "A") (entities "A") nil))))))))

(deftest undo-removes-the-self-mark-only
  (with-users [u other]
    (let [e (entity! u "Ports")]
      (db/set-known-mark! u e "self")
      (db/set-known-mark! u e "tutor")
      (is (zero? (db/delete-known-mark! other e "self")))
      (is (= 1 (db/delete-known-mark! u e "self")) "§4.4.6")
      (is (= ["tutor"] (mapv :source (jdbc/execute! db/ds
                                       ["SELECT source FROM known_marks WHERE entity_id = ?" e]
                                       opts)))))))

(deftest a-learned-root-keeps-learned
  (with-users [u]
    (let [{:keys [map-id ids]} (three-roots! u)
          learned-at #(:learned_at (jdbc/execute-one! db/ds
                                     ["SELECT learned_at FROM question_roots WHERE id = ?" (ids "B")]
                                     opts))]
      (is (nil? (learned-at)) "§4.5.3: no time until the tutor sets it learned")
      (db/apply-question-map-plan! u map-id
        {:new-roots [] :pin-roots {} :learned #{"B"} :next nil})
      (let [first-time (learned-at)]
        (is (some? first-time))
        (testing "§4.4.7: Teach me refuses a learned root and changes nothing"
          (is (nil? (db/set-question-root-next! u (ids "B"))))
          (is (= {"A" "next" "B" "learned" "C" "open"} (status-by-label u map-id))))
        (testing "§4.5.3: Teach me on another root and a second confirmation keep the time"
          (is (db/set-question-root-next! u (ids "C")))
          (db/apply-question-map-plan! u map-id
            {:new-roots [] :pin-roots {} :learned #{"B"} :next nil})
          (is (= first-time (learned-at))))))))
