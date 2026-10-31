(ns freememo.activity-summary-test
  "What a finished Activity row says it did (plans/feedback-activity-biblio-youtube.md
   3.1). Needs PostgreSQL with the `activities.summary` column."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.activity :as activity]
            [freememo.db :as db]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- with-user! [test!]
  (let [user-id (:id (jdbc/execute-one! db/ds
                       ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                        (str "activity-summary-" (random-uuid))
                        (str (random-uuid) "@example.test")]
                       {:builder-fn rs/as-unqualified-maps}))]
    (try
      (test! user-id)
      (finally
        (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" user-id])))))

(defn- row [uid id]
  (some #(when (= id (:id %)) %) (activity/user-activities uid)))

(defn- start-sync! [uid]
  (activity/start! uid "anki-reconcile" "collection" {:phase "writing"}))

(deftest terminate-stores-the-summary-and-the-panel-reads-it
  (with-user!
    (fn [uid]
      (let [id (start-sync! uid)]
        (activity/terminate! uid id "success" {:summary "12 sent to Anki"})
        (is (= {:outcome "success" :error nil :summary "12 sent to Anki"}
              (select-keys (row uid id) [:outcome :error :summary])))))))

(deftest a-summary-is-bounded
  (with-user!
    (fn [uid]
      (let [id (start-sync! uid)]
        (activity/terminate! uid id "success"
          {:summary (apply str (repeat 600 "x"))})
        (is (= activity/summary-max-chars (count (:summary (row uid id)))))))))

(deftest a-cancelled-row-takes-its-summary-afterwards
  (with-user!
    (fn [uid]
      (let [floor (activity/newest-ended-row-id uid :anki-reconcile "collection")
            id (start-sync! uid)
            text "Sync cancelled. 3 of 9 cards sent to Anki."]
        (activity/terminate! uid id "cancelled" {})
        (is (= :recorded (activity/record-summary! uid :anki-reconcile "collection" floor text)))
        (is (= {:outcome "cancelled" :summary text}
              (select-keys (row uid id) [:outcome :summary])))
        (testing "a second record changes nothing"
          (is (= :unchanged (activity/record-summary! uid :anki-reconcile "collection" floor "other")))
          (is (= text (:summary (row uid id)))))))))

(deftest a-row-in-flight-is-left-to-terminate
  (with-user!
    (fn [uid]
      (let [floor (activity/newest-ended-row-id uid :anki-reconcile "collection")
            id (start-sync! uid)]
        (is (= :unchanged (activity/record-summary! uid :anki-reconcile "collection" floor "early")))
        (is (nil? (:summary (row uid id))))
        (is (nil? (:outcome (row uid id))))))))

(deftest a-run-with-no-row-of-its-own-touches-no-earlier-row
  (with-user!
    (fn [uid]
      (let [old (start-sync! uid)
            _ (activity/terminate! uid old "failure" {:error "boom"})
            floor (activity/newest-ended-row-id uid :anki-reconcile "collection")]
        (is (= old floor))
        (is (= :unchanged
              (activity/record-summary! uid :anki-reconcile "collection" floor "already in sync"))
          "the run ended before the reporter opened a row for it")
        (is (nil? (:summary (row uid old))))))))

(deftest a-row-in-flight-at-the-claim-belongs-to-the-new-run
  (with-user!
    (fn [uid]
      (let [carried (start-sync! uid)
            floor (activity/newest-ended-row-id uid :anki-reconcile "collection")
            text "Sync cancelled. 0 of 4 cards sent to Anki."]
        (is (< floor carried))
        (activity/terminate! uid carried "cancelled" {})
        (is (= :recorded
              (activity/record-summary! uid :anki-reconcile "collection" floor text)))
        (is (= text (:summary (row uid carried))))))))

(deftest only-the-newest-run-takes-a-summary
  (with-user!
    (fn [uid]
      (let [old (start-sync! uid)
            _ (activity/terminate! uid old "failure" {:error "boom"})
            floor (activity/newest-ended-row-id uid :anki-reconcile "collection")
            new (start-sync! uid)]
        (is (= :unchanged (activity/record-summary! uid :anki-reconcile "collection" floor "late"))
          "the newest run is still in flight, so the old one is not touched")
        (is (nil? (:summary (row uid old))))
        (is (nil? (:summary (row uid new))))))))
