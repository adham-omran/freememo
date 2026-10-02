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
   [freememo.assistant-answer-selection :as answer-selection]
   [freememo.assistant-composer :as composer]
   [freememo.assistant-diagnostics :as diagnostics]
   [freememo.assistant-modes :as modes]
   [freememo.assistant-panel-data :as panel-data]
   [freememo.assistant-panel-rows :as rows]
   [freememo.assistant-evidence :as evidence]
   [freememo.assistant-selection-cards :as selection-cards]
   [freememo.assistant-side-question :as side-question]
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
          selection-card-job (e/watch !selection-cards)
          !side (atom nil)
          side (e/watch !side)
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

      (selection-cards/SelectionCardsEffect selection-card-job user-id page-topic-id !error)

      ;; Add one selected document as a chip. Its own e/defn, so this method
      ;; stays under the 64KB bytecode limit.
      (composer/ReferencePickEffect picked reference-documents reference-document-count
        draft !refs !draft !mention-index !at-open? !picked)

      (dom/div
        (dom/props {:class "assistant-panel"})

        ;; Row 2: ＋ New + the chat picker.
        (rows/ChatRow user-id root-topic-id chat-options pending-mode pending-web-search?
          !active !draft !error)
        (transcript/AssistantTranscript
          {:sending? sending? :echo echo :active active
           :mode-entry mode-entry :active-run active-run :user-id user-id
           :page-topic-id page-topic-id :root-topic-id root-topic-id
           :web-search? web-search? :annotations annotations}
          !submit !active !refs !details-run !evidence !error !echo
          !annotations !selection-cards !side)

        (run-details/RunDetailsModal !details-run details user-id)
        (evidence/EvidenceModal !evidence root-topic-id)
        (side-question/SideQuestionHost side user-id page-topic-id !side !evidence)

        (when error
          (dom/div
            (dom/props {:class "assistant-panel__error"})
            (dom/text error)))

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

        ;; Diagnostics LAST, and wrapping nothing. This used to be
        ;; `(case (replace-state! …) (do … (dom/div …)))`, which put the whole
        ;; panel inside a case branch keyed on a map that changes at exactly the
        ;; two moments a run starts and settles. Re-selecting the branch remounts
        ;; it, so both transitions tore down and rebuilt the panel: every
        ;; (e/server …) read went cold at once — the transcript to zero rows and
        ;; its empty-chat starters, the model select to zero options — while the
        ;; atoms in this `let` survived and put the same chat back a round trip
        ;; later. That was the blank on send and the blank on settle.
        ;;
        ;; A `case` is the right tool for SEQUENCING an effect before a token
        ;; spend. It is the wrong tool for merely keeping one alive: this is a
        ;; fire-and-forget write whose value nothing needs. As the last form of
        ;; the body its value IS the body's value, so it cannot be skipped, and
        ;; it gates no frame.
        (e/on-unmount #(diagnostics/replace-state! {}))
        (diagnostics/replace-state!
          {:topic-id root-topic-id
           :page-topic-id page-topic-id
           :chat-id active
           :run-uuid (:run_uuid active-run)
           :run-kind (:kind active-run)
           :run-status (:status active-run)
           :render-phase (if sending? "streaming" "terminal")})))))
