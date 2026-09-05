(ns freememo.assistant-transcript-entry
  "One row in the assistant transcript."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.assistant-message-actions :as message-actions]
   [freememo.assistant-message-content :as message-content]))

(e/defn AssistantTranscriptEntry
  [message user-id page-topic-id root-topic-id sending?
   !details-run !evidence !error]
  (e/client
    (cond
      (= "event" (:assistant_messages/role message))
      (dom/element "details"
        (dom/props {:class "assistant-compact-event"})
        (dom/element "summary" (dom/text "Context compacted"))
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
        (dom/props {:class "assistant-msg-group"})
        (message-content/AssistantAnswerContent message !evidence)
        (message-actions/AssistantMessageActions
          message user-id page-topic-id root-topic-id sending?
          !details-run !error))

      :else
      (dom/div
        (dom/props
          {:class
           (str "assistant-msg assistant-msg--"
             (:assistant_messages/role message))})
        (dom/text (:assistant_messages/content message))))))
