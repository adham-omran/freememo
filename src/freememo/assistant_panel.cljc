(ns freememo.assistant-panel
  "AI assistant panel content: chat picker, New Chat, transcript, and composer.

   Chats are per-document. Sending with no active chat creates one first. Each
   submitted turn creates a durable streamed run. Reading sources stay outside
   the transcript and remain available in the answer details.

   Chat controls subscribe to :assistant-mutations. The transcript also watches
   :assistant-stream for bounded durable partial text.

   Each chat carries its own mode. The mode select shows the open chat's mode.
   It stages the next chat's mode when no chat is open."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.assistant-composer :as composer]
   [freememo.assistant-diagnostics :as diagnostics]
   [freememo.assistant-modes :as modes]
   [freememo.assistant-panel-data :as panel-data]
   [freememo.assistant-panel-rows :as rows]
   [freememo.assistant-evidence :as evidence]
   [freememo.assistant-submit-effect :as submit-effect]
   [freememo.assistant-transcript :as transcript]
   [freememo.assistant-run-details :as run-details]
   #?(:clj [freememo.assistant :as assistant])))

(defn mention-query
  "Return the unfinished document mention at the end of a draft."
  [draft]
  (panel-data/mention-query draft))

(defn active-document-mention?
  "Return true while the draft has an unfinished document mention."
  [draft]
  (panel-data/active-document-mention? draft))

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
                 user-id root-topic-id active details-run pending-mode (:id echo))
          chat-options (:chat-options data)
          mention-query (panel-data/mention-query draft)
          reference-documents
          (e/server (assistant/referenceable-docs
                      user-id root-topic-id mention-query))
          reference-document-count (e/server (count reference-documents))
          active-reference-index
          (if (pos? reference-document-count)
            (mod mention-index reference-document-count)
            0)
          mention-open?
          (and at-open? (panel-data/active-document-mention? draft))
          active-run (:active-run data)
          details (:details data)
          sending? (:sending? data)
          mode-entry (:mode-entry data)]

      (submit-effect/SubmitEffect
        {:submit submit :refs refs :user-id user-id
         :root-topic-id root-topic-id :page-topic-id page-topic-id}
        t !draft !refs !echo !active !error)

      ;; Add one selected document as a chip. The server list stays server-sited;
      ;; indexed lookup transfers only the selected document.
      (let [[pt _] (e/Token picked)]
        (when pt
          (let [document (e/server
                           (when (pos? reference-document-count)
                             (nth reference-documents
                               (mod picked reference-document-count) nil)))]
            (case document
              (if document
                (case (swap! !refs
                        (fn [references]
                          (if (some #(= (:id document) (:id %)) references)
                            references
                            (conj references {:id (:id document)
                                              :title (:title document)}))))
                  (case (reset! !draft (str/replace draft #"@\S*$" ""))
                    (case (reset! !mention-index 0)
                      (case (reset! !at-open? false)
                        (case (reset! !picked nil)
                          (pt))))))
                (case (reset! !picked nil)
                  (pt)))))))

      ;; Retire the optimistic echo after its durable user row arrives. This
      ;; also prevents a completed echo from appearing after a chat switch.
      (when (and echo (:echo-persisted? data))
        (reset! !echo nil))

      (case
        (diagnostics/replace-state!
          {:topic-id root-topic-id
           :page-topic-id page-topic-id
           :chat-id active
           :run-uuid (:run_uuid active-run)
           :run-kind (:kind active-run)
           :run-status (:status active-run)
           :render-phase (if sending? "streaming" "terminal")})
        (do
          (e/on-unmount #(diagnostics/replace-state! {}))
          (dom/div
        (dom/props {:class "assistant-panel"})

        ;; Row 2: ＋ New + the chat picker.
        (rows/ChatRow user-id root-topic-id chat-options pending-mode
          !active !draft !error)
        (transcript/AssistantTranscript
          {:sending? sending? :echo echo :active active
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
          {:at-open? mention-open?
           :mention-query mention-query
           :active-reference-index active-reference-index
           :reference-document-count reference-document-count
           :refs refs
           :sending? sending?
           :mode-entry mode-entry
           :draft draft
           :user-id user-id
           :active active
           :root-topic-id root-topic-id}
          reference-documents !picked !refs !mention-index !at-open? !draft !active !submit
          !pending-mode !error)))))))
