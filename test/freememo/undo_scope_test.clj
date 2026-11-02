(ns freememo.undo-scope-test
  "plans/remove-toasts.md 1.8: Cmd-Z is scoped to the view, and undo-entry!
   reports its result. No test here needs a database:
   every db fn the code under test calls is replaced."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.activity :as activity]
   [freememo.commands :as commands]
   [freememo.db :as db]
   [freememo.logging :as log]
   [freememo.optimistic :as opt]
   [freememo.undo :as undo]
   [freememo.undo-scope :as scope]))

(def ^:private parents {7 70, 8 80})
(defn- parent-of [id] (get parents id))

(defn- entry [action-type & {:as m}]
  (merge {:id 1 :user_id 5 :action_type action-type :entity_type "x"
          :entity_refs [] :snapshot nil}
    m))

(deftest entry-subjects-cover-all-fourteen-action-types
  (testing "cards are shown by their topic's viewer and by Library cards"
    (doseq [t ["delete-card" "bulk-delete-cards"]]
      (is (= #{[:topic 3] [:topic 4] :library-cards}
            (scope/entry-subjects
              (entry t :snapshot {:cards [{:topic_id 3} {:topic_id 4}]}) parent-of)))
      (testing "the older flat-vector snapshot"
        (is (= #{[:topic 3] :library-cards}
              (scope/entry-subjects (entry t :snapshot [{:topic_id 3}]) parent-of))))))
  (is (= #{[:topic 9]}
        (scope/entry-subjects (entry "remove-pin" :snapshot [{:topic_id 9}]) parent-of)))
  (is (= #{:settings} (scope/entry-subjects (entry "reset-prompt") parent-of)))
  (testing "a deleted document is shown in Library and by its parent's viewer"
    (is (= #{:library-documents [:topic 70]}
          (scope/entry-subjects (entry "delete-document" :entity_refs [7]) parent-of))))
  (testing "a moved topic is shown by both parents"
    (is (= #{:library-documents [:topic 70] [:topic 11]}
          (scope/entry-subjects
            (entry "move-topic" :snapshot {:topic-id 7 :old-parent-id 11}) parent-of))))
  (is (= #{[:topic 12]}
        (scope/entry-subjects
          (entry "split-article" :snapshot {:source-topic-id 12}) parent-of)))
  (is (= #{:curriculum} (scope/entry-subjects (entry "delete-curriculum") parent-of)))
  (is (= #{:assistant}
        (scope/entry-subjects (entry "delete-assistant-chat") parent-of)))
  (doseq [t ["reject-question" "bulk-reject-questions"]]
    (is (= #{:knowledge-questions} (scope/entry-subjects (entry t) parent-of))))
  (doseq [t ["reject-fact" "bulk-reject-facts" "mark-duplicate-facts"]]
    (is (= #{:knowledge-facts} (scope/entry-subjects (entry t) parent-of))))
  (testing "an unknown type is shown nowhere"
    (is (= #{} (scope/entry-subjects (entry "nope") parent-of)))))

(deftest path-subjects-read-the-route
  (is (= #{[:topic 12]} (scope/path-subjects "/viewer/topic/12")))
  (is (= #{[:topic 12]} (scope/path-subjects "/viewer/topic/12/3")))
  (is (= #{:library-cards} (scope/path-subjects "/library/cards")))
  (is (= #{:library-documents} (scope/path-subjects "/library/documents")))
  (is (= #{:library-documents} (scope/path-subjects "/library")))
  (is (= #{:settings} (scope/path-subjects "/settings")))
  (is (= #{:curriculum} (scope/path-subjects "/curriculum/4")))
  (is (= #{:knowledge-questions} (scope/path-subjects "/knowledge/questions/3")))
  (is (= #{:knowledge-facts} (scope/path-subjects "/knowledge/facts/3")))
  (is (= #{} (scope/path-subjects "/learn")))
  (is (= #{} (scope/path-subjects nil))))

(deftest shown-subjects-add-panels
  (is (= #{[:topic 1] [:topic 2] :assistant}
        (scope/shown-subjects "/viewer/topic/1" [2 nil] true)))
  (is (= #{} (scope/shown-subjects "/learn" [] false))))

(deftest undo-newest-takes-the-newest-entry-the-view-shows
  (let [undone (atom [])
        doc-entry (entry "delete-document" :id 30 :entity_refs [7])
        card-entry (entry "delete-card" :id 20 :snapshot {:cards [{:topic_id 3}]})
        ;; newest first, as db/get-undo-entries returns them
        log-entries [doc-entry card-entry]]
    (with-redefs [db/get-undo-entries (fn [_] log-entries)
                  db/topic-parent-id parent-of
                  undo/undo-entry! (fn [_ id] (swap! undone conj id) {:ok? true})]
      (testing "a document deleted in Library, then Cmd-Z on Learn"
        (is (= {:ok? true :nothing? true} (undo/undo-newest! 5 (scope/shown-subjects "/learn" [] false))))
        (is (= [] @undone)))
      (testing "Cmd-Z in Library restores it"
        (undo/undo-newest! 5 (scope/shown-subjects "/library/documents" [] false))
        (is (= [30] @undone)))
      (testing "the newest entry is not shown, so the next matching one is taken"
        (reset! undone [])
        (undo/undo-newest! 5 [[:topic 3]])
        (is (= [20] @undone)))
      (testing "an absent :shown does nothing"
        (reset! undone [])
        (is (:nothing? (undo/undo-newest! 5 nil)))
        (is (= [] @undone))))))

(defn- with-undo-stubs [e within? f]
  (let [marked (atom [])]
    (with-redefs [db/get-undo-entry (fn [_] e)
                  db/undo-entry-within-window? (fn [_] within?)
                  db/mark-undone! (fn [id] (swap! marked conj id))
                  db/restore-card-rows! (fn [_] nil)
                  db/restore-flashcards! (fn [_] nil)
                  db/restore-staged-document! (fn [_ _] {:over-quota? true})
                  commands/bump-channels! (fn [& _] nil)
                  log/audit! (fn [& _] nil)]
      (f marked))))

(deftest undo-entry-returns-its-result
  (testing "a missing entry"
    (with-undo-stubs nil true
      (fn [_]
        (is (= {:ok? false :error "Action not found"}
              (select-keys (undo/undo-entry! 5 1) [:ok? :error]))))))
  (testing "another user's entry"
    (with-undo-stubs (entry "reset-prompt" :user_id 6) true
      (fn [_]
        (is (= "Action not found" (:error (undo/undo-entry! 5 1)))))))
  (testing "an entry past the window"
    (with-undo-stubs (entry "reset-prompt") false
      (fn [marked]
        (is (= {:ok? false :error "The 12-hour recovery window has ended."}
              (select-keys (undo/undo-entry! 5 1) [:ok? :error])))
        (is (= [] @marked)))))
  (testing "a restore that throws"
    (with-undo-stubs (entry "delete-card" :snapshot {:cards [{}]}) true
      (fn [_]
        (with-redefs [db/restore-card-rows! (fn [_] (throw (ex-info "boom" {})))]
          (is (= {:ok? false :error "Undo failed"}
                (select-keys (undo/undo-entry! 5 1) [:ok? :error])))))))
  (testing "an entry already undone is a quiet success"
    (with-undo-stubs (entry "reset-prompt" :undone_at "2026-01-01") true
      (fn [_]
        (is (= {:ok? true :nothing? true}
              (select-keys (undo/undo-entry! 5 1) [:ok? :nothing?]))))))
  (testing "a plain restore has no warning"
    (with-undo-stubs (entry "delete-card" :entity_type "flashcard"
                       :snapshot {:cards [{:topic_id 3}]}) true
      (fn [marked]
        (let [r (undo/undo-entry! 5 1)]
          (is (:ok? r))
          (is (nil? (:warning r)))
          (is (= [1] @marked))))))
  (testing "an Anki-synced card restores with a warning"
    (with-undo-stubs (entry "delete-card" :entity_type "flashcard"
                       :snapshot {:cards [{:topic_id 3 :anki_note_id 99}]}) true
      (fn [_]
        (let [r (undo/undo-entry! 5 1)]
          (is (:ok? r))
          (is (string? (:warning r)))))))
  (testing "a document over quota restores with a warning"
    (with-undo-stubs (entry "delete-document" :entity_type "document"
                       :entity_refs [7] :snapshot {:cards []}) true
      (fn [_]
        (let [r (undo/undo-entry! 5 1)]
          (is (:ok? r))
          (is (re-find #"storage quota" (:warning r))))))))

(deftest undo-newest-command-records-a-warning-and-returns-failure
  (let [recorded (atom [])]
    (with-redefs [activity/record-failure! (fn [_ m] (swap! recorded conj m) 1)]
      (testing "a partial restore is recorded, and the command still succeeds"
        (with-redefs [undo/undo-newest! (fn [_ _] {:ok? true :warning "Careful."})]
          (is (= {:ok? true}
                (opt/run-command! 5 {:type :undo-newest :payload {:shown [:settings]}})))
          (is (= [{:error "Careful." :what "Part of the undo"}] @recorded))))
      (testing "a failure is returned for the dispatcher, not recorded here"
        (reset! recorded [])
        (with-redefs [undo/undo-newest! (fn [_ _] {:ok? false :error "Undo failed"})]
          (is (= {:ok? false :error "Undo failed"}
                (opt/run-command! 5 {:type :undo-newest :payload {:shown [:settings]}})))
          (is (= [] @recorded))))
      (testing ":undo-entry returns the result map"
        (with-redefs [undo/undo-entry! (fn [_ _] {:ok? false :error "Action not found"})]
          (is (= {:ok? false :error "Action not found"}
                (opt/run-command! 5 {:type :undo-entry :payload {:id 1}}))))))))
