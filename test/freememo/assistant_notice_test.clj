(ns freememo.assistant-notice-test
  "Failures the assistant reports travel to the surface that owns
   them: a stored notice on the answer, and an error in the returned value.
   Every database and provider seam is stubbed, so this runs with no PostgreSQL."
  (:require [clojure.test :refer [deftest is]]
            [freememo.assistant :as assistant]
            [freememo.cards :as cards]
            [freememo.db :as db]
            [freememo.question-map :as question-map]
            [freememo.settings :as settings]))

(def ^:private apply-map! #'assistant/apply-question-map-reply!)

(deftest a-map-block-that-cannot-be-applied-stores-a-notice-on-the-answer
  (let [notices (atom [])]
    (with-redefs [question-map/apply-reply! (fn [& _] {:prose "Answer." :problem "Block was cut off."})
                  db/replace-assistant-run-text! (fn [& _] true)
                  db/set-assistant-run-notice! (fn [uuid text] (swap! notices conj [uuid text]) true)]
      (apply-map! 7 "run-1" {} "reply")
      (is (= [["run-1" "The question map was not updated. Block was cut off."]] @notices)))))

(deftest a-throwing-map-reader-stores-a-notice-and-does-not-throw
  (let [notices (atom [])]
    (with-redefs [question-map/apply-reply! (fn [& _] (throw (ex-info "boom" {})))
                  db/set-assistant-run-notice! (fn [uuid text] (swap! notices conj [uuid text]) true)]
      (apply-map! 7 "run-2" {} "reply")
      (is (= [["run-2" "The question map was not updated. Try asking again."]] @notices)))))

(deftest a-failed-capture-returns-its-error
  (with-redefs [settings/get-card-count (constantly 1)
                settings/get-model (constantly "m")
                cards/generate-basic-cards (fn [& _] {:success false :error "Model down."})]
    (let [res (assistant/reply->cards! 7 1 1 "A plain reply with some words in it.")]
      (is (false? (:success res)))
      (is (= "Model down." (:error res))))))

(deftest a-history-action-failure-is-returned
  (with-redefs [db/set-assistant-chat-pinned! (constantly false)]
    (let [res (assistant/apply-chat-history-action! 7 {:op :pin :chat-id 3 :pinned? true})]
      (is (false? (:success res)))
      (is (= "Chat not found." (:error res))))))
