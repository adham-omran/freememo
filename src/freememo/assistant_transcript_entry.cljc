(ns freememo.assistant-transcript-entry
  "One row in the assistant transcript."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.assistant-message-actions :as message-actions]
   [freememo.assistant-message-content :as message-content]))

(e/defn AssistantTranscriptEntry
  "One transcript row. An event row is a compaction or, in a fork, the
   divider between the inherited rows and the fork's own rows
   (plans/assistant-chat-forks.md §4.2.3)."
  [message user-id page-topic-id root-topic-id sending? active
   !details-run !evidence !error !pending-fork]
  (e/client
    (cond
      (= "fork" (:assistant_event/type message))
      (dom/div
        (dom/props {:class "assistant-fork-divider" :role "separator" :dir "auto"})
        (dom/text (str "Forked from “" (:assistant_messages/content message) "”")))

      (= "event" (:assistant_messages/role message))
      (dom/element "details"
        (dom/props {:class "assistant-compact-event"})
        (dom/element "summary"
          (dom/text (if-let [title (:assistant_event/chat_title message)]
                      (str "Context compacted in “" title "”")
                      "Context compacted")))
        (dom/p
          (dom/props {:class "assistant-compact-event__range"})
          (dom/text
            (str "Messages " (:assistant_event/covered_from message)
              "–" (:assistant_event/covered_through message))))
        (dom/div
          (dom/props {:class "assistant-compact-event__summary" :dir "auto"})
          (dom/text (:assistant_messages/content message))))

      (= "assistant" (:assistant_messages/role message))
      (dom/div
        (dom/props {:class (str "assistant-msg-group"
                             (when (:assistant_messages/inherited? message)
                               " assistant-msg-group--inherited"))})
        (message-content/AssistantAnswerContent message !evidence)
        (message-actions/AssistantMessageActions
          message user-id page-topic-id root-topic-id sending? active
          !details-run !error !pending-fork))

      :else
      (dom/div
        (dom/props
          {:class
           (str "assistant-msg assistant-msg--"
             (:assistant_messages/role message)
             (when (:assistant_messages/inherited? message)
               " assistant-msg--inherited"))})
        (dom/text (:assistant_messages/content message))))))
