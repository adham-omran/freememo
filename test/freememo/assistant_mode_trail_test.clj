(ns freememo.assistant-mode-trail-test
  "A mode change writes one :assistant trail entry that names the mode."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.assistant-mode-trail :as mode-trail]
            [freememo.interaction-trail :as trail]))

(defn- recorded [f]
  (let [!calls (atom [])]
    (with-redefs [trail/record! (fn [kind detail] (swap! !calls conj [kind detail]))]
      (f))
    @!calls))

(deftest mode-change-in-an-open-chat-names-chat-and-mode
  (is (= [[:assistant {:action :change-mode :chat-id 42 :mode "socratic"}]]
         (recorded #(mode-trail/record-mode-change! 42 "socratic")))))

(deftest mode-change-before-the-first-send-has-no-chat
  (testing "the entry still exists and its chat is nil"
    (is (= [[:assistant {:action :change-mode :chat-id nil :mode "general"}]]
           (recorded #(mode-trail/record-mode-change! nil "general"))))))
