(ns freememo.command-failure-test
  "plans/remove-toasts.md 1.1.7 and 1.1.8: a queued command that fails returns
   {:ok? false :error string} and pushes nothing, and the dispatcher records one
   banner for it, except for the types that show the failure in place.
   No test here reaches PostgreSQL: each one replaces the database call that
   decides the outcome."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.activity :as activity]
            [freememo.bibliography-toolbar]
            [freememo.card-components]
            [freememo.card-modals]
            [freememo.cards :as cards]
            [freememo.db :as db]
            [freememo.occlusion :as occ]
            [freememo.optimistic :as opt]
            [freememo.pdf-toolbar]
            [freememo.score :as score]
            [freememo.staged-delete :as staged-delete]
            [freememo.target-commands]
            [freememo.topic-move :as topic-move]))

(def ^:private uid 7)

(defn- recording
  "Run `f` with `activity/record-failure!` replaced. Returns the calls."
  [f]
  (let [calls (atom [])]
    (with-redefs [activity/record-failure! (fn [user-id m] (swap! calls conj [user-id m]) 1)]
      (f))
    @calls))

(defn- failed? [r]
  (and (false? (:ok? r)) (string? (:error r)) (boolean (seq (:error r)))))

(deftest a-failed-set-page-done-records-one-banner
  (let [calls (recording
                #(with-redefs [db/set-page-done! (fn [& _] nil)]
                   (is (failed? (opt/run-command-and-record! uid
                                  {:id 1 :type :set-page-done
                                   :payload {:pdf-root-id 42 :page-number 3 :done? true}})))))]
    (is (= 1 (count calls)))
    (let [[user-id m] (first calls)]
      (is (= uid user-id))
      (is (= 42 (:subject-id m)))
      (is (= {:command :set-page-done
              :payload {:pdf-root-id 42 :page-number 3 :done? true}}
             (:retry m)))
      (is (seq (:what m)))
      (is (seq (:error m))))))

(deftest a-failed-delete-card-records-one-banner
  (let [calls (recording
                #(with-redefs [cards/delete-card-row! (fn [& _] {:success false :error "Card is locked"})]
                   (is (failed? (opt/run-command-and-record! uid
                                  {:id 2 :type :delete-card :payload {:member-ids [[5]]}})))))]
    (is (= 1 (count calls)))
    (is (= "Card is locked" (:error (second (first calls)))))))

(deftest a-thrown-command-records-one-banner
  (let [calls (recording
                #(with-redefs [db/set-page-done! (fn [& _] (throw (ex-info "disk full" {})))]
                   (is (= {:ok? false :error "disk full"}
                          (opt/run-command-and-record! uid
                            {:id 3 :type :set-page-done
                             :payload {:pdf-root-id 1 :page-number 1 :done? true}})))))]
    (is (= 1 (count calls)))))

(deftest a-failed-add-card-records-no-banner
  (let [calls (recording
                #(with-redefs [cards/save-cards (fn [& _] {:success false :error "No room"})]
                   (let [r (opt/run-command-and-record! uid
                             {:id 4 :type :add-card
                              :payload {:topic-id 1 :root-topic-id 1 :kind "basic"
                                        :card-data [{:q "a" :a "b"}]}})]
                     (is (failed? r))
                     (is (= "No room" (:error r))))))]
    (is (empty? calls))))

(deftest add-score-group-and-add-occlusion-keep-their-failure-in-place
  (let [calls (recording
                #(with-redefs [score/create-group! (fn [& _] {:success false})
                               occ/create-group! (fn [& _] {:success false :error ""})]
                   (is (failed? (opt/run-command-and-record! uid {:id 5 :type :add-score-group :payload {}})))
                   (is (failed? (opt/run-command-and-record! uid {:id 6 :type :add-occlusion :payload {}})))))]
    (is (empty? calls))))

(deftest a-success-records-nothing
  (is (empty? (recording
                #(with-redefs [db/set-page-done! (fn [& _] true)]
                   (is (:ok? (opt/run-command-and-record! uid
                               {:id 7 :type :set-page-done
                                :payload {:pdf-root-id 1 :page-number 1 :done? true}})))))))
  (is (empty? (recording #(opt/record-command-failure! uid {:type :x} {:ok? true})))))

(deftest every-queued-method-fails-with-an-error-string
  (let [owned {:topics/id 9 :topics/parent_id 3 :topics/kind "basic" :topics/content_text ""}
        subjects {:subjects [{:topic-id 9}]}
        run (fn [type payload] (opt/run-command! uid {:id 0 :type type :payload payload}))]
    (testing "unknown type"
      (is (failed? (run :no-such-command {}))))
    (testing "queue methods outside the target set"
      (with-redefs [cards/save-cards (fn [& _] {:success false})
                    cards/delete-card-row! (fn [& _] {:success false})
                    score/create-group! (fn [& _] {:success false})
                    occ/create-group! (fn [& _] {:success false})
                    db/set-page-done! (fn [& _] nil)
                    db/done-topic! (fn [& _] nil)
                    db/restore-topic! (fn [& _] nil)]
        (is (failed? (run :add-card {:topic-id 1 :root-topic-id 1 :kind "basic"
                                     :card-data [{:q "a" :a "b"}]})))
        (is (failed? (run :delete-card {:member-ids [[1]]})))
        (is (failed? (run :add-score-group {})))
        (is (failed? (run :add-occlusion {})))
        (is (failed? (run :set-page-done {:pdf-root-id 1 :page-number 1 :done? true})))
        (is (failed? (run :set-pdf-done {:pdf-root-id 1 :done? true})))
        (is (failed? (run :set-pdf-done {:pdf-root-id 1 :done? false})))))
    (testing "target methods on a topic the user does not own"
      (with-redefs [db/owns-topic? (fn [& _] false)
                    db/rename-topic! (fn [& _] :denied)]
        (doseq [type [:rename-topic :move-topic-to-top-level :set-topic-priority
                      :mark-topic-done :dismiss :undismiss :delete-topic
                      :generate-cards-for-topic :scan-pdf-page
                      :generate-cards-for-page :add-pin]]
          (is (failed? (run type (assoc subjects :input "x" :topic-id 9 :media-id 1)))
            (str type " must fail")))))
    (testing "target methods whose write is refused"
      (with-redefs [db/owns-topic? (fn [& _] true)
                    db/get-topic (fn [_] owned)
                    db/get-root-topic-id (fn [_] 9)
                    db/update-topic-priority! (fn [& _] :denied)
                    db/done-topic! (fn [& _] nil)
                    db/dismiss-topic! (fn [& _] nil)
                    db/undismiss-topic! (fn [& _] nil)
                    topic-move/move-topic! (fn [& _] nil)
                    staged-delete/stage-deletion! (fn [& _] nil)]
        (doseq [type [:move-topic-to-top-level :set-topic-priority :mark-topic-done
                      :dismiss :undismiss :delete-topic]]
          (is (failed? (run type (assoc subjects :input 5)))
            (str type " must fail")))
        (is (failed? (run :generate-cards-for-topic subjects))
          "a topic with no text has nothing to generate from")))
    (testing "pin cap"
      (with-redefs [db/owns-topic? (fn [& _] true)
                    db/get-topic (fn [_] owned)
                    db/set-pin! (fn [& _] (throw (ex-info "cap" {:reason :pin-cap-exceeded})))]
        (is (failed? (run :add-pin {:topic-id 9 :media-id 1 :placement "top"})))))
    (testing "a topic already at top level is a no-op success, not a refusal"
      (with-redefs [db/owns-topic? (fn [& _] true)
                    db/get-topic (fn [_] (dissoc owned :topics/parent_id))
                    topic-move/move-topic! (fn [& _] (throw (ex-info "must not run" {})))]
        (is (:ok? (run :move-topic-to-top-level subjects)))))))

(deftest in-place-types-are-the-three-pending-row-commands
  (is (= #{:add-card :add-score-group :add-occlusion} opt/in-place-failure-commands)))
