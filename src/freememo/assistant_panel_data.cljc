(ns freememo.assistant-panel-data
  "Reactive data for the assistant panel."
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.assistant-modes :as modes]
   #?(:clj [freememo.assistant :as assistant])
   #?(:clj [freememo.user-state :as us])))

(defn mention-query
  "Return the unfinished document mention at the end of a draft."
  [draft]
  (some-> (re-find #"(?:^|\s)@([^\s@]*)$" (or draft "")) second))

(defn active-document-mention?
  "Return true only while a draft ends with an unfinished `@` mention.

   Pre: `draft` is the current composer text.
   Post: false removes the document menu.
   Invariant: deleting `@` makes this false."
  [draft]
  (some? (mention-query draft)))

(e/defn AssistantPanelData
  [user-id root-topic-id active details-run pending-mode echo-id]
  (e/client
    (let [assistant-rev
          (e/server (e/watch (us/get-atom user-id :assistant-mutations)))
          stream-rev
          (e/server (e/watch (us/get-atom user-id :assistant-stream)))
          chat-options
          (e/server
            (mapv
              (fn [chat]
                {:id (:assistant_chats/id chat)
                 :label (or (:assistant_chats/title chat) "Untitled")
                 :mode (:assistant_chats/mode chat)})
              (assistant/chats* assistant-rev user-id root-topic-id)))
          active-run
          (e/server
            (assistant/active-run* [assistant-rev stream-rev] user-id active))
          echo-persisted?
          (e/server
            (assistant/message-client-id-persisted?
              assistant-rev user-id active echo-id))
          details
          (e/server
            (assistant/run-details* assistant-rev user-id details-run))
          mode-entry
          (modes/resolve-mode
            (or (some
                  (fn [chat]
                    (when (= active (:id chat)) (:mode chat)))
                  chat-options)
              pending-mode))]
      {:chat-options chat-options
       :active-run active-run
       :details details
       :echo-persisted? echo-persisted?
       :sending? (some? active-run)
       :mode-entry mode-entry})))
