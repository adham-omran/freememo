(ns freememo.assistant-panel-data
  "Reactive data for the assistant panel."
  (:require
   [hyperfiddle.electric3 :as e]
   [clojure.string :as str]
   [freememo.assistant-modes :as modes]
   #?(:clj [freememo.assistant :as assistant])
   #?(:clj [freememo.markdown :as markdown])
   #?(:clj [freememo.user-state :as us])))

(defn mention-query
  "Return the unfinished document mention at the end of a draft."
  [draft]
  (some-> (re-find #"(?:^|\s)@([^\s@]*)$" (or draft "")) second))

(defn reference-options
  "Filter and cap document options for the current mention."
  [options query]
  (let [needle (str/lower-case (or query ""))]
    (into []
      (comp
        (filter #(str/includes? (str/lower-case (:label %)) needle))
        (take 8))
      options)))

(e/defn AssistantPanelData
  [user-id root-topic-id active details-run draft mention-index pending-mode]
  (e/client
    (let [assistant-rev
          (e/server (e/watch (us/get-atom user-id :assistant-mutations)))
          chat-options
          (e/server
            (mapv
              (fn [chat]
                {:id (:assistant_chats/id chat)
                 :label (or (:assistant_chats/title chat) "Untitled")
                 :mode (:assistant_chats/mode chat)})
              (assistant/chats* assistant-rev user-id root-topic-id)))
          docs
          (e/server
            (vec (assistant/referenceable-docs user-id root-topic-id)))
          doc-options
          (mapv (fn [document] {:id (:id document) :label (:title document)})
            docs)
          mention-options
          (reference-options doc-options (mention-query draft))
          mention-count (count mention-options)
          messages
          (e/server
            (mapv
              (fn [message]
                (if (= "assistant" (:assistant_messages/role message))
                  (assoc message :assistant_messages/content-html
                    (-> (assistant/render-evidence-citations
                          (:assistant_messages/content message)
                          (:assistant_run/evidence message))
                      markdown/unwrap-non-math-dollars
                      markdown/parse-markdown
                      markdown/dollar-math->tex))
                  message))
              (or (assistant/messages* assistant-rev user-id active) [])))
          active-run
          (e/server (assistant/active-run* assistant-rev user-id active))
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
       :documents-by-id (into {} (map (juxt :id identity)) docs)
       :mention-options mention-options
       :mention-count mention-count
       :active-mention-index
       (if (pos? mention-count) (mod mention-index mention-count) 0)
       :messages messages
       :active-run active-run
       :details details
       :sending? (some? active-run)
       :mode-entry mode-entry})))
