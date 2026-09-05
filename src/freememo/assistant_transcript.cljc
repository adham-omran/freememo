(ns freememo.assistant-transcript
  "Transcript area for the assistant panel."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.assistant-panel-rows :as rows]
   [freememo.assistant-transcript-entry :as transcript-entry]))

(defn scroll-to-bottom! [node _dependency]
  #?(:cljs (set! (.-scrollTop node) (.-scrollHeight node))
     :clj nil))

(e/defn AssistantTranscript
  [{:keys [messages sending? echo mode-entry !submit !active !refs active-run
           user-id page-topic-id root-topic-id !details-run !evidence !error]}]
  (e/client
    (dom/div
      (dom/props {:class "assistant-panel__transcript"})
      (let [visible (vec messages)]
        (if (empty? visible)
          (when (and (not sending?) (nil? echo))
            (rows/EmptyChatStarters mode-entry !submit !active !refs))
          (e/for-by :assistant_messages/id [message visible]
            (transcript-entry/AssistantTranscriptEntry
              message user-id page-topic-id root-topic-id sending?
              !details-run !evidence !error))))
      (when-let [echo-text
                 (and echo
                   (not-any?
                     #(= (:id echo) (:assistant_messages/client_id %)) messages)
                   (:text echo))]
        (dom/div
          (dom/props {:class "assistant-msg assistant-msg--user"})
          (dom/text echo-text)))
      (when (and sending? (= "compact" (:kind active-run)))
        (dom/div
          (dom/props {:class "assistant-panel__thinking"})
          (dom/text "Compacting context…")))
      (scroll-to-bottom! dom/node
        [(count messages) sending? (:id echo)
         (some-> messages last :assistant_messages/content count)]))))
