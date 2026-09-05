(ns freememo.assistant-transcript
  "Transcript area for the assistant panel."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.assistant-panel-rows :as rows]
   [freememo.assistant-transcript-entry :as transcript-entry]
   #?(:clj [freememo.assistant :as assistant])
   #?(:clj [freememo.user-state :as us])))

(defn scroll-to-bottom! [node _dependency]
  #?(:cljs (set! (.-scrollTop node) (.-scrollHeight node))
     :clj nil))

(e/defn AssistantTranscript
  [{:keys [sending? echo active mode-entry active-run user-id page-topic-id
           root-topic-id]}
   !submit !active !refs !details-run !evidence !error]
  (e/client
    (dom/div
      (dom/props {:class "assistant-panel__transcript"})
      (let [assistant-rev
            (e/server (e/watch (us/get-atom user-id :assistant-mutations)))
            stream-rev
            (e/server (e/watch (us/get-atom user-id :assistant-stream)))
            message-rows
            (e/server
              (e/diff-by :assistant_messages/id
                (or (assistant/messages*
                      [assistant-rev stream-rev] user-id active)
                  [])))
            message-count (e/Count message-rows)]
        (if (zero? message-count)
          (when (and (not sending?) (nil? echo))
            (rows/EmptyChatStarters mode-entry !submit !active !refs))
          (e/for [message message-rows]
            (let [display-message
                  (e/server (assistant/transcript-message-view message))]
              (transcript-entry/AssistantTranscriptEntry
                display-message user-id page-topic-id root-topic-id sending?
                !details-run !evidence !error))))
        (when-let [echo-text (:text echo)]
          (dom/div
            (dom/props {:class "assistant-msg assistant-msg--user"})
            (dom/text echo-text)))
        (when (and sending? (= "compact" (:kind active-run)))
          (dom/div
            (dom/props {:class "assistant-panel__thinking"})
            (dom/text "Compacting context…")))
        (scroll-to-bottom! dom/node
          [message-count stream-rev sending? (:id echo)])))))
