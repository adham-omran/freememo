(ns freememo.assistant-mode-trail
  "Interaction-trail entry for a change of assistant mode.
   The logs can then show whether a user changed the mode before a send."
  (:require [freememo.interaction-trail :as trail]))

(defn record-mode-change!
  "Record one mode change in the interaction trail.
   Pre: chat-id is the open chat id, or nil before the first send.
   Post: one :assistant entry with :action :change-mode, :chat-id and :mode."
  [chat-id mode-id]
  (trail/record! :assistant {:action :change-mode :chat-id chat-id :mode mode-id}))
