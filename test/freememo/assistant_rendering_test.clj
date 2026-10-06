(ns freememo.assistant-rendering-test
  "Assistant transcript rendering contracts that do not need a database."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [freememo.assistant :as assistant]
   [freememo.config :as config]
   [freememo.markdown :as markdown]))

(def ^:private content-evidence
  #'assistant/content-evidence)

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

(deftest a-streaming-answer-renders-through-its-terminal-mode
  (testing "formatting arrives with the text, not on the last frame"
    (with-redefs [config/assistant-rich-render? (constantly true)]
      (let [streaming (assistant/transcript-message-view
                        (assistant-message "streaming" "**Partly.** [evidence:E1]"))
            terminal (assistant/transcript-message-view
                       (assistant-message "completed" "**Partly.** [evidence:E1]"))]
        (is (= "rich-html" (:assistant_messages/render-mode streaming)))
        (is (= (:assistant_messages/render-mode terminal)
              (:assistant_messages/render-mode streaming))
          "the renderer must not swap when the run settles")
        (is (= (:assistant_messages/safe-html terminal)
              (:assistant_messages/safe-html streaming)))
        (is (str/includes? (:assistant_messages/safe-html streaming)
              "<strong>Partly.</strong>")))))
  (testing "an unclosed construct still renders, and wire keys stay trimmed"
    (with-redefs [config/assistant-rich-render? (constantly true)]
      (let [view (assistant/transcript-message-view
                   (assistant-message "streaming" "**unfinished [evidence:E1]"))]
        (is (= "rich-html" (:assistant_messages/render-mode view)))
        (is (not (contains? view :assistant_messages/created_at)))
        (is (not (contains? (first (:assistant_run/evidence view)) :created_at)))
        (is (not (contains? (first (:assistant_run/evidence view)) :id)))))))

(deftest an-empty-answer-is-neither-formatted-nor-plain
  (is (= "empty" (:assistant_messages/render-mode
                   (assistant/transcript-message-view
                     (assistant-message "queued" ""))))))

(deftest the-echo-retires-against-the-rendered-rows
  (let [rows [{:assistant_messages/id 1 :assistant_messages/client_id "c-1"}
              {:assistant_messages/id 2 :assistant_messages/client_id nil}]]
    (is (assistant/message-client-id-present? rows "c-1"))
    (is (not (assistant/message-client-id-present? rows "c-2")))
    (is (not (assistant/message-client-id-present? rows nil))
      "a chat with no echo in flight never retires one")
    (is (not (assistant/message-client-id-present? [] "c-1"))
      "an empty transcript holds the echo until its row arrives")))

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

(deftest formatted-evidence-is-sanitized-for-the-browser
  (let [message (-> (assistant-message "streaming" "Working [evidence:E1]")
                  (assoc-in [:assistant_run/evidence 0 :quoted_html]
                    "<p onclick=\"steal()\">Retained<script>steal()</script></p>")
                  (assoc-in [:assistant_run/evidence 0 :quoted_text] "Retained"))
        [source] (:assistant_run/evidence (assistant/transcript-message-view message))]
    (is (= "<p>Retained</p>" (:quoted_html source)))
    (is (not (contains? source :quoted_text))
      "the evidence dialog shows the HTML when it exists, so the text stays home")
    (is (not (contains? source :created_at)))))

(deftest the-browser-gets-only-the-evidence-an-answer-cites
  (testing "a run retains every block it showed the model, and an answer links few"
    (let [block (fn [k] {:citation_key k :document_topic_id 12 :anchor (str "s-" k)
                         :display_label (str "Section " k)
                         :quoted_text (apply str (repeat 2000 "x"))
                         :quoted_html (str "<p>" (apply str (repeat 2000 "x")) "</p>")})
          message (assoc (assistant-message "completed"
                           "Scope [evidence:E2]. Types [evidence:E40].")
                    :assistant_run/evidence (mapv #(block (str "E" %)) (range 1 3001)))
          view (assistant/transcript-message-view message)]
      (is (= ["E2" "E40"] (mapv :citation_key (:assistant_run/evidence view))))
      (is (< (count (pr-str view)) 20000)
        "3000 retained blocks once crossed the wire with every answer")))
  (testing "a token still arriving cites nothing yet, and a half key matches no row"
    (let [message (assistant-message "streaming" "Scope [evidence:E")]
      (is (= [] (:assistant_run/evidence (assistant/transcript-message-view message))))))
  (testing "E1 is not cited by E12"
    (let [message (update (assistant-message "completed" "See [evidence:E12].")
                    :assistant_run/evidence conj
                    {:citation_key "E12" :document_topic_id 12
                     :display_label "twelve"})]
      (is (= ["E12"] (mapv :citation_key
                       (:assistant_run/evidence
                         (assistant/transcript-message-view message))))))))

(deftest old-html-in-the-text-field-becomes-safe-plain-text
  (let [message (assoc-in (assistant-message "streaming" "Working [evidence:E1]")
                  [:assistant_run/evidence 0 :quoted_text]
                  "<h2>Introduction</h2><p>Retained <strong>evidence</strong>.</p><li>cut")
        [source] (:assistant_run/evidence (assistant/transcript-message-view message))]
    (is (= "Introduction Retained evidence. cut" (:quoted_text source)))
    (is (nil? (:quoted_html source)))))

(deftest each-source-block-gets-distinct-retained-evidence
  (let [sources (content-evidence
                  "<h2 id=\"memory\">Memory</h2><p>Stack values.</p><p>Heap values.</p>"
                  {:document-topic-id 12
                   :page-topic-id 12
                   :document-title "Zig"
                   :source-revision "rev-1"}
                  4)]
    (is (= ["E4" "E5" "E6"] (mapv :citation-key sources)))
    (is (= ["Memory" "Stack values." "Heap values."]
          (mapv :quoted-text sources)))
    (is (= ["Memory, excerpt 1" "Memory, excerpt 2" "Memory, excerpt 3"]
          (mapv :display-label sources)))
    (is (= ["memory" "memory" "memory"] (mapv :anchor sources)))
    (is (every? :quoted-html sources))))

(deftest the-model-reads-decoded-text-while-the-citation-keeps-markup
  (let [[source] (content-evidence
                   "<p>Compare <code>?&lt;</code> with <code>?&gt;</code>.</p>"
                   {:document-topic-id 12 :page-topic-id 12
                    :document-title "Missionary" :source-revision "rev-1"}
                   1)]
    (is (= "Compare ?< with ?>." (:snapshot-content source)))
    (is (str/includes? (:quoted-html source) "<code>?&lt;</code>"))))

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
