(ns freememo.generation-admission-callers-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.activity :as activity]
            [freememo.assistant :as assistant]
            [freememo.content-toolbar-helpers :as helpers]
            [freememo.db :as db]
            [freememo.generate-invoker :as invoker]
            [freememo.known-ideas :as ideas]
            [freememo.settings :as settings]
            [freememo.target-commands :as targets]))

(def refusal {:ok false :error "Queue full" :error-type :queue-full})

(deftest every-enqueue-caller-retains-the-admission-failure
  (let [failures (atom [])]
   (with-redefs [activity/record-failure! (fn [_ failure] (swap! failures conj failure) 1)
                helpers/enqueue-card-gen! (constantly refusal)
                helpers/build-gen-context* (constantly {:content "text" :context nil})
                db/get-topic-for-user (fn [_ id] {:topics/id id :topics/kind "basic"})
                db/get-root-topic-id (constantly 10)
                db/get-topic (constantly {:topics/id 10 :topics/kind "basic"})
                db/idea-material (constantly {:topic-id 11 :root-topic-id 10 :quotes ["quote"]})
                settings/get-context-enabled (constantly false)
                settings/get-context-pages (constantly 0)
                settings/get-card-type (constantly "basic")
                settings/get-card-count (constantly 3)]
    (is (= refusal (#'assistant/enqueue-assistant-cards! 1 11 "<p>text</p>" nil 3)))
    (is (= refusal (ideas/make-cards! 1 4)))
    (is (= refusal (invoker/enqueue-generation!* {:user-id 1 :topic-id 11})))
    (is (= [{:error "Queue full" :what "Generate cards" :subject-id 11}] @failures)
        "the Generate popover closes, so a refusal goes to the banner strip")
    (is (= {:ok? false :error "Queue full"}
           (#'targets/enqueue-generation! 1 {:topics/id 11 :topics/content_text "text"}))))))
