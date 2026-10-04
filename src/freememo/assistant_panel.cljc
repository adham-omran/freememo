(ns freememo.assistant-panel
  "AI assistant panel content: chat bar, history view, transcript, and composer.

   Chats are per-document. Sending with no active chat creates one with that
   first message. Each submitted turn creates a durable streamed run. Reading
   sources stay outside the transcript and remain available in the answer
   details.

   The active chat, the history view's open flag, the bar's rename flag and the
   chat-action queue belong to RightSidePanel, which outlives this component
   across a panel collapse and a poem tab switch. They arrive as positional
   parameters.

   Chat controls subscribe to :assistant-mutations. The transcript also watches
   :assistant-stream for bounded durable partial text.

   Each chat carries its own mode. The mode select shows the open chat's mode.
   It stages the next chat's mode when no chat is open."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.assistant-answer-selection :as answer-selection]
   [freememo.assistant-chat-history :as history]
   [freememo.assistant-composer :as composer]
   [freememo.assistant-modes :as modes]
   [freememo.assistant-panel-data :as panel-data]
   [freememo.assistant-panel-overlays :as overlays]
   [freememo.assistant-selection-cards :as selection-cards]
   [freememo.assistant-submit-effect :as submit-effect]
   [freememo.assistant-transcript :as transcript]
   #?(:clj [freememo.assistant :as assistant])))

(defn mention-query
  "Return the unfinished document mention at the end of a draft."
  [draft]
  (panel-data/mention-query draft))

(defn active-document-mention?
  "Return true while the draft has an unfinished document mention."
  [draft]
  (panel-data/active-document-mention? draft))

(e/defn AssistantPanel
  "Pre: `seeded?` is true once the last-chat seed has answered."
  [page-topic-id root-topic-id user-id seeded?
   !active !history-open? !renaming-active? !chat-action]
  (e/client
    (let [active (e/watch !active)
          !draft (atom "")
          draft (e/watch !draft)
          !submit (atom nil)
          submit (e/watch !submit)
          [t _] (e/Token submit)
          !error (atom nil)
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
          ;; Search starts off for a new chat. An open chat carries its own
          ;; stored preference through AssistantPanelData.
          !pending-web-search? (atom false)
          pending-web-search? (e/watch !pending-web-search?)
          ;; The answer selection menu's three outputs
          ;; (plans/assistant-answer-selection-actions.md §5). Annotations are
          ;; tagged with their chat, so a chat switch hides them.
          !annotations (atom answer-selection/empty-annotations)
          annotation-state (e/watch !annotations)
          annotations (answer-selection/visible-annotations annotation-state active)
          !selection-cards (atom nil)
          !side (atom nil)
          data (panel-data/AssistantPanelData
                 user-id root-topic-id active details-run pending-mode
                 pending-web-search?)
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
          active-run (:active-run data)
          details (:details data)
          sending? (:sending? data)
          mode-entry (:mode-entry data)
          web-search? (:web-search? data)
          web-search-supported? (:web-search-supported? data)]

      (submit-effect/SubmitEffect
        {:submit submit :refs refs :annotation-state annotation-state :user-id user-id
         :root-topic-id root-topic-id :page-topic-id page-topic-id}
        t !draft !refs !echo !active !error !annotations)

      (selection-cards/SelectionCardsEffect user-id page-topic-id !selection-cards !error)

      ;; Add one selected document as a chip. Its own e/defn, so this method
      ;; stays under the 64KB bytecode limit.
      (composer/ReferencePickEffect picked reference-documents reference-document-count
        draft !refs !draft !mention-index !at-open? !picked)

      (dom/div
        (dom/props {:class "assistant-panel"})

        (history/ChatBarWithHistory root-topic-id chat-options active
          (:active-title data)
          !active !history-open? !renaming-active? !chat-action !draft !error)
        (transcript/AssistantTranscript
          {:sending? sending? :echo echo :active active
           :mode-entry mode-entry :active-run active-run :user-id user-id
           :page-topic-id page-topic-id :root-topic-id root-topic-id
           :web-search? web-search? :seeded? seeded? :annotations annotations}
          !submit !active !refs !details-run !evidence !error !echo
          !annotations !selection-cards !side)

        (overlays/PanelOverlays details user-id root-topic-id page-topic-id
          !details-run !evidence !side !error)

        (composer/AssistantComposer
          {:at-open? at-open?
           :mention-query mention-query
           :active-reference-index active-reference-index
           :reference-document-count reference-document-count
           :refs refs
           :sending? sending?
           :mode-entry mode-entry
           :web-search? web-search?
           :web-search-supported? web-search-supported?
           :draft draft
           :user-id user-id
           :active active
           :root-topic-id root-topic-id
           :annotations annotations}
          reference-documents !picked !refs !mention-index !at-open? !draft !active !submit
          !pending-mode !pending-web-search? !error !annotations)

        ;; Diagnostics LAST, and wrapping nothing: see PanelDiagnostics.
        (overlays/PanelDiagnostics root-topic-id page-topic-id active active-run
          sending?)))))
