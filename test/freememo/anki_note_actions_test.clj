(ns freememo.anki-note-actions-test
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.anki-note-actions :as actions]))

(deftest note-state-drives-the-compact-toggle-controls
  (let [note {:noteId 42
              :tags ["marked"]
              :cards [10 11]
              :fields {:Remarks {:value "<p>Keep this.</p>"}}}
        partial (actions/note->state note [true false])]
    (testing "state keeps note and card state distinct"
      (is (:marked? partial))
      (is (= 2 (:card-count partial)))
      (is (= 1 (:suspended-count partial))))
    (testing "checkbox changes map to the matching AnkiConnect action"
      (is (= {:action "removeTags" :params {:notes [42] :tags "marked"}}
            (actions/note-action-request 42 partial :unmark nil)))
      (is (= {:action "suspend" :params {:cards [10 11]}}
            (actions/note-action-request 42 partial :suspend nil))))
    (testing "remarks save only after a change"
      (is (false? (actions/remarks-dirty? partial "<p>Keep this.</p>")))
      (is (actions/remarks-dirty? partial "<p>Changed.</p>")))))
