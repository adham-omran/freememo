(ns freememo.command-targets-test
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.command-targets :as targets]
            [freememo.commands :as commands]))

(def topic-entry {:target :topic :menu [1 1]})
(def many-entry {:target :card :arity :many :menu [9 1]})

(defn- topic-target [& subjects] {:kind :topic :subjects (vec subjects)})

(deftest applicable-matches-kind-and-arity
  (testing "the kind must match"
    (is (targets/applicable? topic-entry (topic-target {:topic-id 1})))
    (is (not (targets/applicable? topic-entry {:kind :card :subjects [{:card-id 1}]}))))
  (testing "a :one entry takes exactly one subject"
    (is (not (targets/applicable? topic-entry (topic-target {:topic-id 1} {:topic-id 2})))))
  (testing "a :many entry takes several"
    (is (targets/applicable? many-entry {:kind :card :subjects [{:card-id 1} {:card-id 2}]})))
  (testing "no subjects, no target and no :target never apply"
    (is (not (targets/applicable? topic-entry {:kind :topic :subjects []})))
    (is (not (targets/applicable? topic-entry nil)))
    (is (not (targets/applicable? {:label "x"} (topic-target {:topic-id 1}))))))

(defn- passes? [test-key subject]
  (targets/applicable? (assoc topic-entry :target-if #{test-key}) (topic-target subject)))

(deftest every-subject-test-has-a-passing-and-a-failing-subject
  (let [cases {:root               [{:root? true} {:root? false}]
               :not-root           [{:root? false} {:root? true}]
               :has-children       [{:has-children? true} {:has-children? false}]
               :dismissed          [{:dismissed? true} {:dismissed? false}]
               :not-dismissed      [{:dismissed? false} {:dismissed? true}]
               :not-done           [{:status "active"} {:status "done"}]
               :not-pdf-root       [{:root? false :topic-kind "page"} {:root? true :topic-kind "pdf"}]
               :deletable          [{:deletable? true} {:deletable? false}]
               :neoreader-syncable [{:neoreader-syncable? true} {:neoreader-syncable? false}]
               :has-topic          [{:topic-id 4} {}]
               :has-history        [{:history-scope [:card 4]} {}]
               :cross-origin-src   [{:src "https://e.org/a.png"} {:src "/api/media/12"}]
               :pin-room           [{:pin-count 1} {:pin-count 2}]
               :looping            [{:looping? true} {:looping? false}]
               :not-looping        [{:looping? false} {:looping? true}]
               :math-ready         [{:math-ready? true} {:math-ready? false}]
               :has-selection      [{:selection-text "a"} {:selection-text " "}]}]
    (is (= (set (keys targets/subject-tests)) (set (keys cases)))
      "every subject test has a case here")
    (doseq [[k [pass fail]] cases]
      (is (passes? k pass) (str k " passes " pass))
      (is (not (passes? k fail)) (str k " fails " fail)))))

(deftest an-absent-fact-hides-the-item
  (doseq [k [:root :not-root :dismissed :not-dismissed :not-done :not-pdf-root
             :deletable :looping :not-looping :math-ready]]
    (is (not (passes? k {})) (str k " hides on a subject that does not say"))))

(deftest a-pdf-of-unknown-position-offers-no-generate
  (is (not (passes? :not-pdf-root {:topic-kind "pdf"})))
  (is (passes? :not-pdf-root {:topic-kind "pdf" :root? false})))

(deftest a-data-uri-or-app-media-is-not-cross-origin
  (is (not (passes? :cross-origin-src {:src "data:image/png;base64,AAAA"}))))

(deftest disabled-reason-reads-enable-if
  (let [entry (assoc topic-entry :enable-if #{:pin-room})]
    (is (nil? (targets/disabled-reason entry (topic-target {:pin-count 0}))))
    (is (= "Max 2 pins. Remove one first."
          (targets/disabled-reason entry (topic-target {:pin-count 2}))))))

(deftest menu-items-are-ordered-by-group-then-rank
  (let [registry {:b {:label "B" :target :topic :menu [2 1]}
                  :a {:label "A" :menu-label "Short A" :target :topic :menu [1 2]}
                  :c {:label "C" :target :topic :menu [1 1] :danger? true}
                  :x {:label "X" :target :card :menu [0 0]}}]
    (is (= [{:id :c :label "C" :group 1 :danger? true :disabled-reason nil}
            {:id :a :label "Short A" :group 1 :danger? false :disabled-reason nil}
            {:id :b :label "B" :group 2 :danger? false :disabled-reason nil}]
          (targets/menu-items registry (topic-target {:topic-id 1}))))))

(deftest wire-payload-drops-the-surface
  (is (= {:kind :topic :subjects [{:topic-id 1}] :input "x"}
        (targets/wire-payload {:kind :topic :subjects [{:topic-id 1}] :input "x"
                               :surface {:quill :not-serializable}}))))

(deftest validate-entry-rejects-bad-target-keys
  (is (empty? (targets/validate-entry :ok {:target :topic :menu [1 1] :target-if #{:root}})))
  (is (seq (targets/validate-entry :bad-kind {:target :widget :menu [1 1]})))
  (is (seq (targets/validate-entry :no-target {:target-if #{:root}})))
  (is (seq (targets/validate-entry :bad-test {:target :topic :menu [1 1] :target-if #{:nope}})))
  (is (seq (targets/validate-entry :no-reason {:target :topic :menu [1 1] :enable-if #{:root}})))
  (is (seq (targets/validate-entry :no-menu {:target :topic})))
  (is (seq (targets/validate-entry :bad-arity {:target :topic :menu [1 1] :arity :two})))
  (is (seq (targets/validate-entry :bad-input {:target :topic :menu [1 1] :menu-input {:type :date}}))))

(deftest the-registry-target-entries-are-valid
  (is (empty? (commands/validate-registry))))

(deftest a-pdf-root-offers-no-generate
  (let [entry (commands/command :generate-cards-for-topic)]
    (is (not (targets/applicable? entry (topic-target {:topic-id 1 :root? true :topic-kind "pdf"}))))
    (is (targets/applicable? entry (topic-target {:topic-id 2 :root? false :topic-kind "page"})))
    (is (targets/applicable? entry (topic-target {:topic-id 3 :root? true :topic-kind "basic"})))))

(deftest library-row-conditions
  (testing "4.2.3.1: each former RowActionsMenu item keeps its condition"
    (let [root {:topic-id 1 :root? true :has-children? true :dismissed? false
                :neoreader-syncable? true}
          child {:topic-id 2 :root? false :has-children? false :dismissed? false
                 :neoreader-syncable? false}
          applies? (fn [id s] (targets/applicable? (commands/command id) (topic-target s)))]
      (is (applies? :move-topic-to-top-level child))
      (is (not (applies? :move-topic-to-top-level root)))
      (is (applies? :review-topic root))
      (is (not (applies? :review-topic child)))
      (is (applies? :sync-topic-with-neoreader root))
      (is (not (applies? :sync-topic-with-neoreader child)))
      (is (applies? :dismiss root))
      (is (not (applies? :dismiss (assoc root :dismissed? true))))
      (is (applies? :undismiss (assoc root :dismissed? true)))
      (is (not (applies? :undismiss root)))
      (is (applies? :open-document-options root))
      (is (not (applies? :open-document-options child))))))
