(ns freememo.assistant-panel
  "AI assistant panel content: chat picker, New Chat, transcript, and composer.

   Chats are per-document. Sending with no active chat creates one first. Each
   submitted turn creates a durable streamed run. Reading sources stay outside
   the transcript and remain available in the answer details.

   Reactivity: subscribes to :assistant-mutations. Stream workers publish at a
   bounded rate so the transcript can show durable partial text.

   Each chat carries its own mode. The mode select shows the open chat's mode.
   It stages the next chat's mode when no chat is open."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.assistant-composer :as composer]
   [freememo.assistant-modes :as modes]
   [freememo.assistant-panel-data :as panel-data]
   [freememo.assistant-panel-rows :as rows]
   [freememo.assistant-evidence :as evidence]
   [freememo.assistant-submit-effect :as submit-effect]
   [freememo.assistant-transcript :as transcript]
   [freememo.assistant-run-details :as run-details]))

(defn mention-query
  "Return the unfinished document mention at the end of a draft."
  [draft]
  (panel-data/mention-query draft))

(defn reference-options
  "Filter and cap document options for the current mention."
  [options query]
  (panel-data/reference-options options query))

(e/defn AssistantPanel [page-topic-id root-topic-id user-id]
  (e/client
    (let [!active (atom nil)
          active (e/watch !active)
          !draft (atom "")
          draft (e/watch !draft)
          !submit (atom nil)
          submit (e/watch !submit)
          [t _] (e/Token submit)
          !error (atom nil)
          error (e/watch !error)
          !details-run (atom nil)
          details-run (e/watch !details-run)
          !evidence (atom nil)
          ;; @-referenced documents queued for the next send (shown as chips);
          ;; their ids ride the submit payload and clear on a successful send.
          !refs (atom [])
          refs (e/watch !refs)
          !at-open? (atom false)
          at-open? (e/watch !at-open?)
          !picked (atom nil)
          picked (e/watch !picked)
          !mention-index (atom 0)
          mention-index (e/watch !mention-index)
          ;; Optimistic echo of the just-sent user turn: {:id :text} or nil.
          ;; Rendered as a user bubble until the persisted row carrying this :id
          ;; appears in the transcript (run creation stores :id as client_id and
          ;; bumps before the reply), so the echo and the real row never both show.
          !echo (atom nil)
          echo (e/watch !echo)
          ;; Mode staged for the next chat. The select writes it, and chat creation
          ;; consumes it. An open chat's persona comes off its own row instead.
          !pending-mode (atom modes/default-id)
          pending-mode (e/watch !pending-mode)
          data (panel-data/AssistantPanelData
                 user-id root-topic-id active details-run draft mention-index
                 pending-mode)
          chat-options (:chat-options data)
          by-id (:documents-by-id data)
          mention-options (:mention-options data)
          mention-count (:mention-count data)
          active-mention-index (:active-mention-index data)
          messages (:messages data)
          active-run (:active-run data)
          details (:details data)
          sending? (:sending? data)
          mode-entry (:mode-entry data)]

      (submit-effect/SubmitEffect
        {:submit submit :refs refs :user-id user-id
         :root-topic-id root-topic-id :page-topic-id page-topic-id}
        t !draft !refs !echo !active !error)

      ;; Add the selected document as a chip. Remove the mention query from the
      ;; message and close the menu.
      (let [[pt _] (e/Token picked)]
        (when pt
          (when-let [doc (get by-id picked)]
            (swap! !refs (fn [rs]
                           (if (some #(= (:id doc) (:id %)) rs)
                             rs
                             (conj rs {:id (:id doc) :title (:title doc)})))))
          (reset! !draft (str/replace draft #"@\S*$" ""))
          (reset! !mention-index 0)
          (reset! !at-open? false)
          (reset! !picked nil)
          (pt)))

      ;; Retire the optimistic echo after its durable user row arrives. This
      ;; also prevents a completed echo from appearing after a chat switch.
      (when (and echo
              (some #(= (:id echo) (:assistant_messages/client_id %)) messages))
        (reset! !echo nil))

      (dom/div
        (dom/props {:class "assistant-panel"})

        ;; Row 2: ＋ New + the chat picker.
        (rows/ChatRow user-id root-topic-id chat-options pending-mode
          !active !draft !error)
        (transcript/AssistantTranscript
          {:messages messages :sending? sending? :echo echo
           :mode-entry mode-entry :active-run active-run :user-id user-id
           :page-topic-id page-topic-id :root-topic-id root-topic-id}
          !submit !active !refs !details-run !evidence !error)

        (run-details/RunDetailsModal !details-run details)
        (evidence/EvidenceModal !evidence root-topic-id)

        (when error
          (dom/div
            (dom/props {:class "assistant-panel__error"})
            (dom/text error)))

        (composer/AssistantComposer
          {:at-open? at-open?
           :mention-options mention-options
           :active-mention-index active-mention-index
           :refs refs
           :mention-count mention-count
           :sending? sending?
           :mode-entry mode-entry
           :draft draft
           :user-id user-id
           :active active
           :root-topic-id root-topic-id}
          !picked !refs !mention-index !at-open? !draft !active !submit
          !pending-mode !error)))))
