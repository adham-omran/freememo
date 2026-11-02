(ns freememo.activity-failure-test
  "The failure state on `activities` (plans/remove-toasts.md 1.1): the three
   shapes `error-kind` reads, and the one-shot failure row the banner strip
   lists until the user acknowledges it.

   The row tests need PostgreSQL with the `seen_at`, `error_kind` and `retry`
   columns, and skip when the port refuses connections."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.activity :as activity]
            [freememo.desktop-link :as link]
            [freememo.db :as db]
            [freememo.user-state :as us]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

;; ---------------------------------------------------------------------------
;; error-kind. No database.
;; ---------------------------------------------------------------------------

(deftest error-kind-reads-a-string
  (is (= "needs-account" (activity/error-kind (str "Could not sync. " link/sign-in-message))))
  (is (= "insufficient-credits"
        (activity/error-kind "Out of credits. Top up in Settings to keep using AI features.")))
  (is (nil? (activity/error-kind "Network error")))
  (is (nil? (activity/error-kind nil))))

(deftest error-kind-reads-a-result-map
  (is (= "insufficient-credits" (activity/error-kind {:insufficient-credits true})))
  (is (= "insufficient-credits" (activity/error-kind {:error-type :insufficient-credits})))
  (is (= "insufficient-credits"
        (activity/error-kind {:error "Out of credits. Top up in Settings."})))
  (is (= "needs-account" (activity/error-kind {:error link/sign-in-message})))
  (is (nil? (activity/error-kind {:error "Network error"})))
  (is (nil? (activity/error-kind {:ok? false}))))

(deftest error-kind-reads-an-exception
  (is (= "insufficient-credits"
        (activity/error-kind
          (ex-info "Out of credits." {:type :freememo.cards/insufficient-credits}))))
  (testing "through a wrapping cause"
    (is (= "insufficient-credits"
          (activity/error-kind
            (RuntimeException. "wrapped"
              (ex-info "gate" {:type :freememo.kg-llm/insufficient-credits}))))))
  (is (= "needs-account"
        (activity/error-kind (ex-info link/sign-in-message {:type ::sign-in-required}))))
  (is (nil? (activity/error-kind (RuntimeException. "boom")))))

(deftest error-message-reads-every-shape
  (is (= "boom" (activity/error-message "boom")))
  (is (= "boom" (activity/error-message {:error "boom"})))
  (is (= "boom" (activity/error-message (RuntimeException. "boom"))))
  (is (nil? (activity/error-message "  ")))
  (is (nil? (activity/error-message nil))))

;; ---------------------------------------------------------------------------
;; Rows
;; ---------------------------------------------------------------------------

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT seen_at, error_kind, retry FROM activities LIMIT 1"])
       true
       (catch Exception _ false)))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})

(defn- make-user! []
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
          (str "activity-failure-" (random-uuid)) (str (random-uuid) "@example.test")]
         opts)))

(defn- with-users! [n test!]
  (let [ids (vec (repeatedly n make-user!))]
    (try
      (apply test! ids)
      (finally
        (doseq [id ids] (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" id]))))))

(defn- row [id]
  (jdbc/execute-one! db/ds ["SELECT * FROM activities WHERE id = ?" id] opts))

(defn- counter [uid] @(us/get-atom uid :activity-mutations))

(deftest record-failure-writes-one-unseen-failed-row
  (when (db-up?)
    (with-users!
      1
      (fn [uid]
        (let [topic (:id (jdbc/execute-one! db/ds
                           ["INSERT INTO topics (user_id, title, kind, content)
                             VALUES (?, 'T', 'basic', '') RETURNING id" uid] opts))
              before (counter uid)
              id (activity/record-failure! uid {:error "Delete failed"
                                                :what "Delete card"
                                                :subject-id topic})
              r (row id)]
          (is (= "action-failure" (:type r)))
          (is (= "failure" (:outcome r)))
          (is (= "Delete failed" (:error r)))
          (is (= "Delete card" (:phase r)))
          (is (= topic (:subject_id r)))
          (is (nil? (:seen_at r)))
          (is (some? (:terminated_at r)))
          (is (nil? (:error_kind r)))
          (is (= (inc before) (counter uid)) "bumped exactly once")
          (is (= 1 (count (activity/unseen-failures uid)))))))))

(deftest record-failure-derives-the-error-kind
  (when (db-up?)
    (with-users!
      1
      (fn [uid]
        (let [id (activity/record-failure! uid {:error {:error-type :insufficient-credits
                                                        :error "Out of credits."}
                                                :what "Generate cards"})]
          (is (= "insufficient-credits" (:error_kind (row id))))
          (is (= "insufficient-credits"
                (:error-kind (first (activity/unseen-failures uid))))))))))

(deftest record-failure-keeps-the-failure-when-the-subject-is-stale
  (when (db-up?)
    (with-users!
      2
      (fn [uid other]
        (let [foreign (:id (jdbc/execute-one! db/ds
                            ["INSERT INTO topics (user_id, title, kind, content)
                              VALUES (?, 'T', 'basic', '') RETURNING id" other] opts))
              id (activity/record-failure! uid {:error "boom" :subject-id foreign})]
          (is (nil? (:subject_id (row id)))
            "another user's topic is never stored as this user's subject"))))))

(deftest record-failure-requires-an-error
  (is (thrown? AssertionError (activity/record-failure! 1 {:error "  "})))
  (is (thrown? AssertionError (activity/record-failure! 1 {:error nil}))))

(deftest unseen-failures-lists-only-unseen-failed-rows
  (when (db-up?)
    (with-users!
      2
      (fn [uid other]
        (let [seen (activity/record-failure! uid {:error "seen"})
              unseen (activity/record-failure! uid {:error "unseen"})
              _ (activity/record-failure! other {:error "someone else's"})
              ok-run (activity/start! uid "anki-reconcile" "collection" {:phase "writing"})]
          (activity/terminate! uid ok-run "success" {:summary "done"})
          (activity/acknowledge! uid [seen])
          (is (= [unseen] (mapv :id (activity/unseen-failures uid)))
            "no seen failure, no success, no other user's failure"))))))

(deftest a-failed-run-closes-with-its-kind-and-reaches-the-strip
  (when (db-up?)
    (with-users!
      1
      (fn [uid]
        (let [id (activity/start! uid "anki-reconcile" "collection" {:phase "writing"})]
          (activity/terminate! uid id "failure" {:error link/sign-in-message})
          (is (= "needs-account" (:error_kind (row id))))
          (is (= [id] (mapv :id (activity/unseen-failures uid)))))))))

(deftest acknowledge-sets-seen-at-and-bumps-once
  (when (db-up?)
    (with-users!
      1
      (fn [uid]
        (let [a (activity/record-failure! uid {:error "a"})
              b (activity/record-failure! uid {:error "b"})
              before (counter uid)]
          (is (= :done (activity/acknowledge! uid [a b])))
          (is (some? (:seen_at (row a))))
          (is (some? (:seen_at (row b))))
          (is (empty? (activity/unseen-failures uid)))
          (is (= (inc before) (counter uid)) "one bump for two rows")
          (activity/acknowledge! uid [a b])
          (is (= (inc before) (counter uid)) "a second acknowledge changes nothing"))))))

(deftest acknowledge-of-another-users-id-is-a-no-op
  (when (db-up?)
    (with-users!
      2
      (fn [uid other]
        (let [theirs (activity/record-failure! other {:error "theirs"})
              before (counter other)]
          (activity/acknowledge! uid [theirs])
          (is (nil? (:seen_at (row theirs))))
          (is (= before (counter other)))
          (is (= 1 (count (activity/unseen-failures other)))))))))

(deftest retry-runs-the-stored-command-for-the-same-subject
  (when (db-up?)
    (with-users!
      1
      (fn [uid]
        (let [calls (atom [])
              topic 42]
          (activity/register-retry-handler! ::retry-probe
            (fn [user-id payload] (swap! calls conj [user-id payload]) {:ok? true}))
          (let [id (activity/record-failure! uid {:error "Distill failed"
                                                  :what "Distill"
                                                  :retry {:command ::retry-probe
                                                          :payload {:topic-id topic}}})]
            (testing "unseen-failures says a Retry exists and never sends the command"
              (let [f (first (activity/unseen-failures uid))]
                (is (true? (:retry? f)))
                (is (not (contains? f :retry)))))
            (is (map? (activity/retry! uid id)))
            (is (= [[uid {:topic-id topic}]] @calls))
            (is (some? (:seen_at (row id))) "Retry acknowledges the row")
            (testing "a second Retry of the same row does nothing"
              (is (= :gone (activity/retry! uid id)))
              (is (= 1 (count @calls))))))))))

(deftest retry-that-fails-records-a-new-failure-with-the-same-retry
  (when (db-up?)
    (with-users!
      1
      (fn [uid]
        (activity/register-retry-handler! ::retry-fails
          (fn [_ _] {:ok? false :error "still broken"}))
        (let [id (activity/record-failure! uid {:error "first"
                                                :retry {:command ::retry-fails :payload {}}})]
          (activity/retry! uid id)
          (let [unseen (activity/unseen-failures uid)]
            (is (= 1 (count unseen)))
            (is (= "still broken" (:error (first unseen))))
            (is (true? (:retry? (first unseen))))))))))

(deftest retry-of-another-users-id-does-nothing
  (when (db-up?)
    (with-users!
      2
      (fn [uid other]
        (let [calls (atom 0)]
          (activity/register-retry-handler! ::retry-foreign
            (fn [_ _] (swap! calls inc) {:ok? true}))
          (let [theirs (activity/record-failure! other {:error "theirs"
                                                        :retry {:command ::retry-foreign
                                                                :payload {}}})]
            (is (= :gone (activity/retry! uid theirs)))
            (is (zero? @calls))
            (is (nil? (:seen_at (row theirs))))))))))

(def ^:private nested-payload
  {:topic-id 1 :kind :pdf :nested {:k :v :xs [:a "b" 3]}})

(deftest retry-payload-round-trips-without-a-database
  (let [param (#'activity/retry-param {:command :set-page-done :payload nested-payload})]
    (is (= {:command :set-page-done :payload nested-payload}
          (#'activity/read-retry param)))
    (is (= :some.ns/cmd
          (:command (#'activity/read-retry
                      (#'activity/retry-param {:command :some.ns/cmd :payload {}})))))))

(deftest retry-replays-the-exact-payload
  (when (db-up?)
    (with-users!
      1
      (fn [uid]
        (let [got (atom nil)]
          (activity/register-retry-handler! ::retry-payload
            (fn [_ payload] (reset! got payload) {:ok? true}))
          (let [id (activity/record-failure! uid {:error "x"
                                                  :retry {:command ::retry-payload
                                                          :payload nested-payload}})]
            (activity/retry! uid id)
            (is (= nested-payload @got))))))))
