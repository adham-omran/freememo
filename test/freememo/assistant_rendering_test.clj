(ns freememo.assistant-rendering-test
  "Assistant transcript rendering contracts that do not need a database."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [freememo.assistant :as assistant]
   [freememo.config :as config]
   [freememo.markdown :as markdown]))

(defn- assistant-message [status content]
  {:assistant_messages/id 7
   :assistant_messages/role "assistant"
   :assistant_messages/content content
   :assistant_messages/created_at (java.time.Instant/now)
   :assistant_run/uuid "run-7"
   :assistant_run/status status
   :assistant_run/evidence
   [{:citation_key "E1"
     :id 90
     :created_at (java.time.Instant/now)
     :document_topic_id 12
     :page_number 3
     :display_label "document-level evidence"}]})

(deftest active-answer-stays-a-stable-text-row
  (let [view (assistant/transcript-message-view
               (assistant-message "streaming" "**unfinished [evidence:E1]"))]
    (is (= "streaming-text" (:assistant_messages/render-mode view)))
    (is (= "**unfinished [evidence:E1]"
          (:assistant_messages/content view)))
    (is (not (contains? view :assistant_messages/safe-html)))
    (is (not (contains? view :assistant_messages/created_at)))
    (is (not (contains? (first (:assistant_run/evidence view)) :created_at)))
    (is (not (contains? (first (:assistant_run/evidence view)) :id)))))

(deftest terminal-answer-becomes-sanitized-html-once
  (with-redefs [config/assistant-rich-render? (constantly true)]
    (let [view (assistant/transcript-message-view
                 (assistant-message "completed"
                   "**Done.** [evidence:E1] <script>alert(1)</script>"))
          html (:assistant_messages/safe-html view)]
      (is (= "rich-html" (:assistant_messages/render-mode view)))
      (is (str/includes? html "<strong>Done.</strong>"))
      (is (str/includes? html "href=\"/viewer/topic/12/3\""))
      (is (not (str/includes? html "<script"))))))

(deftest rich-render-switch-and-size-limit-fall-back-to-text
  (testing "the deployment switch disables formatted answers"
    (with-redefs [config/assistant-rich-render? (constantly false)]
      (let [view (assistant/transcript-message-view
                   (assistant-message "completed" "Answer [evidence:E1]."))]
        (is (= "plain-text" (:assistant_messages/render-mode view)))
        (is (= "Answer document-level evidence."
              (:assistant_messages/content view)))
        (is (not (contains? view :assistant_messages/safe-html))))))
  (testing "large answers skip the formatted renderer"
    (with-redefs [config/assistant-rich-render? (constantly true)]
      (let [content (apply str (repeat (inc assistant/max-rich-answer-chars) "x"))
            view (assistant/transcript-message-view
                   (assistant-message "completed" content))]
        (is (= "plain-text" (:assistant_messages/render-mode view)))
        (is (= (count content) (count (:assistant_messages/content view))))))))

(deftest markdown-failure-falls-back-to-text
  (with-redefs [config/assistant-rich-render? (constantly true)
                markdown/markdown-with-math->html
                (fn [_] (throw (ex-info "parse failed" {})))]
    (let [view (assistant/transcript-message-view
                 (assistant-message "failed" "Partial [evidence:E1]"))]
      (is (= "plain-text" (:assistant_messages/render-mode view)))
      (is (= "Partial document-level evidence"
            (:assistant_messages/content view))))))
