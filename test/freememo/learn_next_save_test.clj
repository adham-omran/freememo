(ns freememo.learn-next-save-test
  "Next saves the pending edit first
   (plans/learn-next-speed/issues/01-next-saves-pending-edit-first.md).

   The browser check is `playwright/learn-session-run.mjs`. These tests hold
   the order and the failure rule of the server step, and the test that picks
   the edit to save, with no database and no browser."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [freememo.activity :as activity]
   [freememo.editor-pane :as editor-pane]
   [freememo.learn-session :as learn-session]
   [freememo.rich-text-editor :as editor]))

(use-fixtures :each
  (fn [f]
    (let [dirty @editor/!dirty-html
          saved @editor/!saved-edit-id
          held @editor/!held-topic-id]
      (try (f)
           (finally
             (reset! editor/!dirty-html dirty)
             (reset! editor/!saved-edit-id saved)
             (reset! editor/!held-topic-id held))))))

(def ^:private scope {:user-id 7 :topic-id 41})
(def ^:private edit {:html "<p>edit</p>" :topic-id 41 :save-scope scope :edit-id "e1"})

(deftest pending-edit-is-the-edit-no-save-acknowledged
  (testing "no edit gives nil, so a Next with no edit sends no body"
    (reset! editor/!dirty-html nil)
    (is (nil? (learn-session/pending-edit 7))))
  (testing "an unsaved edit gives its body, its save scope and its id"
    (reset! editor/!dirty-html edit)
    (reset! editor/!saved-edit-id nil)
    (is (= {:html "<p>edit</p>" :save-scope scope :edit-id "e1"}
           (learn-session/pending-edit 7))))
  (testing "an edit whose auto-save landed gives nil"
    (reset! editor/!dirty-html edit)
    (reset! editor/!saved-edit-id "e1")
    (is (nil? (learn-session/pending-edit 7))))
  (testing "an edit by another user gives nil"
    (reset! editor/!dirty-html edit)
    (reset! editor/!saved-edit-id nil)
    (is (nil? (learn-session/pending-edit 8)))))

(deftest mark-edit-saved-never-moves-the-ack-back
  (testing "the editor still holds the saved edit: it is acknowledged"
    (reset! editor/!dirty-html edit)
    (reset! editor/!saved-edit-id nil)
    (is (= :marked (learn-session/mark-edit-saved! "e1")))
    (is (= "e1" @editor/!saved-edit-id)))
  (testing "a newer edit is in the editor: the auto-save owns it"
    (reset! editor/!dirty-html (assoc edit :edit-id "e2"))
    (reset! editor/!saved-edit-id nil)
    (learn-session/mark-edit-saved! "e1")
    (is (nil? @editor/!saved-edit-id)))
  (testing "no edit was saved: nothing changes"
    (reset! editor/!saved-edit-id "e0")
    (learn-session/mark-edit-saved! nil)
    (is (= "e0" @editor/!saved-edit-id))))

(defn- run-finish
  "Calls finish-topic!* with stubbed writes. Returns the result and the calls,
   in order."
  [pending {:keys [save-result schedule-throws?]}]
  (let [calls (atom [])]
    (with-redefs [editor-pane/save-editor-html!*
                  (fn [m] (swap! calls conj [:save m]) save-result)
                  activity/record-failure!
                  (fn [user-id m] (swap! calls conj [:failure user-id m]) 1)]
      (let [result (learn-session/finish-topic!* 7 41 pending "Next"
                     (fn []
                       (swap! calls conj [:schedule])
                       (when schedule-throws? (throw (ex-info "db down" {})))))]
        {:result result :calls @calls}))))

(deftest finish-topic-saves-before-it-schedules
  (testing "no pending edit: no save, then the schedule write"
    (let [{:keys [result calls]} (run-finish nil {})]
      (is (= [[:schedule]] calls))
      (is (= {:success true :saved-edit-id nil} result))))
  (testing "a pending edit: the save runs first, with the edit's own scope"
    (let [pending {:html "<p>edit</p>" :save-scope scope :edit-id "e1"}
          {:keys [result calls]} (run-finish pending {:save-result {:success true}})]
      (is (= [:save {:user-id 7 :topic-id 41 :html "<p>edit</p>"}] (first calls)))
      (is (= [:schedule] (second calls)))
      (is (= {:success true :saved-edit-id "e1"} result)))))

(deftest a-failed-save-stops-next-and-reaches-the-banner-strip
  (testing "the save fails: no schedule write, one failure row, the error back"
    (let [pending {:html "<p>edit</p>" :save-scope scope :edit-id "e1"}
          {:keys [result calls]} (run-finish pending {:save-result {:success false :error "refused"}})]
      (is (= [:save :failure] (mapv first calls)))
      (is (= [:failure 7 {:error "refused" :what "Save before Next" :subject-id 41}] (second calls)))
      (is (= {:success false :error "refused"} result))))
  (testing "a save failure with no text still gets a message"
    (let [pending {:html "<p>edit</p>" :save-scope scope :edit-id "e1"}
          {:keys [result]} (run-finish pending {:save-result {:success false :error nil}})]
      (is (= false (:success result)))
      (is (seq (:error result)))))
  (testing "the schedule write throws: a failure row and the error, never a throw"
    (let [{:keys [result calls]} (run-finish nil {:schedule-throws? true})]
      (is (= [:schedule :failure] (mapv first calls)))
      (is (= "Next" (:what (nth (second calls) 2))))
      (is (= {:success false :error "db down"} result)))))

;; plans/learn-next-speed/issues/10-edit-during-save-window.md
(deftest a-counted-tap-holds-the-editor-and-a-failure-releases-it
  (reset! editor/!held-topic-id nil)
  (testing "a tap that counts holds the editor for the tapped topic"
    (let [!gate (atom nil)]
      (is (some? (learn-session/tap-next! !gate 41 true 7 1000)))
      (is (= 41 @editor/!held-topic-id))))
  (testing "a tap that does not count leaves the hold as it is"
    (reset! editor/!held-topic-id nil)
    (let [!gate (atom {:id "g" :from 41 :at 1000})]
      (is (nil? (learn-session/tap-next! !gate 41 true 7 1050)))
      (is (nil? @editor/!held-topic-id))))
  (testing "a tap on a kind with no editor body holds nothing"
    (reset! editor/!held-topic-id nil)
    (let [!gate (atom nil)]
      (is (some? (learn-session/tap-next! !gate 41 false 7 1000)))
      (is (nil? @editor/!held-topic-id))))
  (testing "Postpone holds the editor only for a kind with an editor body"
    (reset! editor/!held-topic-id nil)
    (learn-session/hold-editor! 41 false)
    (is (nil? @editor/!held-topic-id))
    (learn-session/hold-editor! 41 true)
    (is (= 41 @editor/!held-topic-id)))
  (testing "a failed save releases the editor"
    (reset! editor/!held-topic-id 41)
    (is (= :released (learn-session/release-editor!)))
    (is (nil? @editor/!held-topic-id))))
