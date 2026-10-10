(ns freememo.assistant-message-actions
  "Actions for one assistant answer."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.assistant-chat-tree :as tree]
   [freememo.icons :as icons]
   #?(:clj [freememo.assistant :as assistant])))

(defn fork-request
  "The pending fork that \"Fork here\" on `message` in chat `active` writes
   (plans/assistant-chat-forks.md §4.3.3)."
  [active message]
  {:chat-id active
   :message-id (:assistant_messages/id message)
   :excerpt (tree/excerpt (:assistant_messages/content message) 60)})

(e/defn AssistantMessageActions
  "Card, details, Fork here and Retry for one answer. An inherited answer has
   no Retry: its run belongs to an ancestor chat (§4.2.4). Fork here is off
   while any run streams, so it never forks at an unsettled answer (§4.3.4)."
  [message user-id page-topic-id root-topic-id sending? active !details-run !error
   !pending-fork]
  (e/client
    (let [status (:assistant_run/status message)
          content (:assistant_messages/content message)
          run-uuid (:assistant_run/uuid message)
          inherited? (:assistant_messages/inherited? message)]
      (when run-uuid
        (dom/div
          (dom/props {:class "assistant-msg-actions"})
          (when-not (str/blank? content)
            (dom/button
              (dom/props {:class "assistant-msg-card" :type "button"
                          :title "Generate a flashcard from this reply"
                          :aria-label "Generate a flashcard from this reply"
                          :disabled sending?})
              (let [click (dom/On "click" identity nil)
                    [generate-token _] (e/Token click)
                    generating? (some? generate-token)]
                (dom/props {:disabled (or sending? generating?)})
                (dom/text (if generating? "✦…" "✦"))
                (when generate-token
                  (case (e/server
                          (e/Offload
                            #(assistant/reply->cards!
                               user-id page-topic-id root-topic-id content)))
                    (generate-token))))))
          (dom/button
            (dom/props {:class "assistant-msg-card assistant-msg-details"
                        :type "button" :title "Answer details"
                        :aria-label "Answer details"})
            (icons/Icon :info :size 14)
            (dom/On "click" (fn [_] (reset! !details-run run-uuid)) nil))
          (dom/button
            (dom/props {:class "assistant-msg-card assistant-msg-fork" :type "button"
                        :title "Fork here: ask a new question in a branch of this chat"
                        :aria-label "Fork here"
                        :disabled (or sending? (str/blank? content))})
            (dom/text "⑂")
            (dom/On "click"
              (fn [_] (reset! !pending-fork (fork-request active message)))
              nil))
          (when (and (= "failed" status) (not inherited?))
            (dom/button
              (dom/props {:class "assistant-msg-card" :type "button"
                          :title "Retry answer" :aria-label "Retry answer"
                          :disabled sending?})
              (dom/text "↻")
              (let [retry-click (dom/On "click" identity nil)
                    [retry-token _] (e/Token retry-click)]
                (when retry-token
                  (let [result (e/server
                                 (e/Offload
                                   #(assistant/retry-answer-run!
                                      user-id run-uuid (str (random-uuid)))))]
                    (case result
                      (case (reset! !error
                              (when-not (:ok result) (:error result)))
                        (retry-token)))))))))))))
