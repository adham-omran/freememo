(ns freememo.assistant-transcript
  "Transcript area for the assistant panel."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.assistant-answer-selection :as answer-selection]
   [freememo.assistant-panel-rows :as rows]
   [freememo.assistant-transcript-entry :as transcript-entry]
   [freememo.transcript-scroll :as transcript-scroll]
   #?(:clj [freememo.assistant :as assistant])
   #?(:clj [freememo.user-state :as us])))

(defn scroll-to-bottom!
  "Follow the latest message after layout when the reader permits it."
  [node _dependency]
  (transcript-scroll/follow! node))

(defn retire-echo!
  "Keep a submitted turn on the latest page after its durable row arrives."
  [!echo !page chat node]
  (reset! !page {:chat chat :before nil :trail []})
  (reset! !echo nil)
  (transcript-scroll/resume! node)
  nil)

(e/defn AssistantTranscript
  [{:keys [sending? echo active mode-entry web-search? active-run user-id page-topic-id
           root-topic-id seeded? annotations]}
   !submit !active !refs !details-run !evidence !error !echo
   !annotations !selection-cards !side !pending-fork]
  (e/client
    (let [!page (atom {:chat nil :before nil :trail []})
          stored-page (e/watch !page)
          page-state (if (and (= active (:chat stored-page)) (nil? echo)) stored-page
                       {:chat active :before nil :trail []})
          !following (atom true)
          following? (e/watch !following)]
      (dom/div
        (dom/props {:class "assistant-panel__transcript-shell"})
        (dom/div
          ;; The Chat tab's `aria-controls` names this id (plans/question-map-tab.md §4.1.3).
          (dom/props {:class "assistant-panel__transcript" :id "assistant-transcript"})
          (e/for-by identity [_chat [active]]
            (e/on-unmount
              (transcript-scroll/install! dom/node #(reset! !following %))))
          (let [assistant-rev
                (e/server (e/watch (us/get-atom user-id :assistant-mutations)))
                stream-rev
                (e/server (e/watch (us/get-atom user-id :assistant-stream)))
                page-result
                (e/server
                  (e/Offload #(assistant/transcript-page* assistant-rev user-id active (:before page-state))))
                page
                (e/server (assistant/current-transcript-page page-result active (:before page-state)))
                live-row
                (e/server
                  (e/Offload #(assistant/live-transcript-row* [assistant-rev stream-rev] user-id active)))
                messages (e/server (assistant/transcript-rows page live-row))
                ;; The VIEW is what gets diffed, so a row's shape and its content
                ;; cross in one frame. Diffing the raw rows and reading each view
                ;; through a nested (e/server …) made the content a second, demand
                ;; driven transfer: e/Count gave the client the row count a round
                ;; trip early, e/for mounted that many rows, and every one of them
                ;; was empty until its view arrived. That is the blank on send.
                message-rows
                (e/server
                  (e/diff-by :assistant_messages/id
                    messages))
                page-cursor (e/server (:cursor page))
                message-count (e/Count message-rows)
                echo-arrived?
                (e/server (assistant/message-client-id-present? messages (:id echo)))]
            ;; Retire the optimistic echo once the durable row it stands for is in
            ;; the rows above. Reading the rendered rows is what keeps the panel
            ;; from painting empty between the echo leaving and the row arriving.
            ;; It also stops a completed echo reappearing after a chat switch.
            (when (and echo echo-arrived?)
              (retire-echo! !echo !page active dom/node))
            ;; The answer selection menu, one per transcript (plans/assistant-
            ;; answer-selection-actions.md §5.1.4). Every argument is stable, so it
            ;; installs once and tears down with the transcript.
            (e/on-unmount
              (answer-selection/install! dom/node !active !annotations
                !selection-cards !side))
            (answer-selection/paint! dom/node annotations)
            ;; The rows live in their OWN element, mounted always, and no branch
            ;; chooses between them and the empty state. They used to be the else
            ;; branch of an `if` on `(zero? message-count)`, directly inside this div.
            ;; Opening a chat from the picker switched that branch from the starters
            ;; to every row at once, and the next send then mounted the echo after
            ;; them. Electric's mount point for this div lost count there: it
            ;; detached every row node while the row frames stayed mounted, so the
            ;; transcript stayed empty until a reload. Measured: 0 of 10 rows in the
            ;; DOM after the send, `e/Count` at 10, every removed node detached, and
            ;; no foreign node in the div. Which step inside Electric miscounts is
            ;; not identified. A chat whose rows arrived one send at a time never
            ;; tripped it. playwright/probe-assistant-evidence-payload.mjs drives it.
            ;;
            ;; Here each child of this div is at most one node. The loop's only
            ;; siblings are its own rows. `display: contents` keeps the rows as
            ;; flex items of the transcript.
            (dom/div
              (dom/props {:class "assistant-transcript__paging"})
              (dom/button
                (dom/props {:type "button" :class "btn btn-sm"
                            :disabled (not (e/server (:more? page)))})
                (dom/text "Older messages")
                (dom/On "click"
                  (fn [event]
                    (transcript-scroll/pause!
                      (.closest (.-currentTarget event) ".assistant-panel__transcript"))
                    (reset! !page {:chat active :before page-cursor
                                   :trail (conj (:trail page-state) (:before page-state))})) nil))
              (dom/button
                (dom/props {:type "button" :class "btn btn-sm"
                            :disabled (or (empty? (:trail page-state)) (e/server (:pending? page)))})
                (dom/text "Newer messages")
                (dom/On "click"
                  (fn [_]
                    (reset! !page {:chat active :before (peek (:trail page-state))
                                   :trail (pop (:trail page-state))})) nil))
              (when (seq (:trail page-state))
                (dom/button
                  (dom/props {:type "button" :class "btn btn-sm"})
                  (dom/text "Latest messages")
                  (dom/On "click"
                    (fn [event]
                      (reset! !page {:chat active :before nil :trail []})
                      (transcript-scroll/resume!
                        (.closest (.-currentTarget event) ".assistant-panel__transcript"))) nil))))
            (dom/div
              (dom/props {:class "assistant-panel__rows"})
              (e/for [display-message message-rows]
                (transcript-entry/AssistantTranscriptEntry
                  display-message user-id page-topic-id root-topic-id sending? active
                  !details-run !evidence !error !pending-fork)))
            (when (zero? message-count)
              (if (and (not sending?) (nil? echo))
                ;; Starters only for "no chat yet", and only once the last-chat
                ;; seed has answered. A chat always holds a message, so an active
                ;; chat with zero rows is one whose rows are still crossing — the
                ;; starters there would flash for a round trip on every chat open.
                (when (and seeded? (nil? active))
                  (rows/EmptyChatStarters mode-entry web-search? !submit !active !refs))
                ;; A send whose first rows have not crossed yet. The transcript must
                ;; render SOMETHING here: the starters are wrong mid-send, and
                ;; rendering nothing is the blank white panel this branch produced.
                (dom/div
                  (dom/props {:class "assistant-panel__thinking"})
                  (dom/text "Starting…"))))
            (when-let [echo-text (:text echo)]
              (dom/div
                (dom/props {:class "assistant-msg assistant-msg--user"})
                (dom/text echo-text)))
            (when (and sending? (= "compact" (:kind active-run)))
              (dom/div
                (dom/props {:class "assistant-panel__thinking"})
                (dom/text "Compacting context…")))
            ;; assistant-rev is in here because a run settles on that channel alone:
            ;; the stream channel stops, the row count does not move, and only the
            ;; mutation bump marks the last of the answer arriving.
            (scroll-to-bottom! dom/node
              [active page-cursor message-count stream-rev assistant-rev sending? (:id echo)])))
        (when-not following?
          (dom/button
            (dom/props {:type "button" :class "btn assistant-panel__latest"
                        :aria-controls "assistant-transcript"})
            (dom/text "Return to latest")
            (dom/On "click"
              (fn [event]
                (reset! !page {:chat active :before nil :trail []})
                (transcript-scroll/resume!
                  (.querySelector (.-parentElement (.-currentTarget event))
                    ".assistant-panel__transcript")))
              nil)))))))
