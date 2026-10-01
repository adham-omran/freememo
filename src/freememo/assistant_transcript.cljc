(ns freememo.assistant-transcript
  "Transcript area for the assistant panel."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.assistant-panel-rows :as rows]
   [freememo.assistant-transcript-entry :as transcript-entry]
   #?(:clj [freememo.assistant :as assistant])
   #?(:clj [freememo.user-state :as us])))

(defn scroll-to-bottom!
  "Pin the transcript to its newest content, after the browser lays that content out.

   The scroll runs in a requestAnimationFrame rather than inline. This fn is
   called during the same reactive propagation that patches the rows, so an
   inline `scrollHeight` read measures the transcript as it was BEFORE the new
   text landed — which is why a finished answer left the panel parked mid-answer
   while its last paragraphs sat below the fold. One frame later the layout is
   the one the learner is about to see.

   Pre:  `node` is the transcript's scroll container. `_dependency` is every
         value whose change should re-pin it, including the mutation revision a
         terminal answer bumps.
   Post: the container is scrolled to its own bottom. A node unmounted before
         the frame runs is written to harmlessly and never read again."
  [node _dependency]
  #?(:cljs (do (js/requestAnimationFrame
                 (fn [] (set! (.-scrollTop node) (.-scrollHeight node))))
               ;; The frame id is deliberately dropped: this call is the last
               ;; form of the transcript body, and returning a fresh integer per
               ;; scroll would feed the reactive graph a value that never settles.
               nil)
     :clj nil))

(e/defn AssistantTranscript
  [{:keys [sending? echo active mode-entry web-search? active-run user-id page-topic-id
           root-topic-id]}
   !submit !active !refs !details-run !evidence !error !echo]
  (e/client
    (dom/div
      (dom/props {:class "assistant-panel__transcript"})
      (let [assistant-rev
            (e/server (e/watch (us/get-atom user-id :assistant-mutations)))
            stream-rev
            (e/server (e/watch (us/get-atom user-id :assistant-stream)))
            ;; One read of the transcript, used for the rendered rows and for
            ;; the echo's retirement. Two server reads would land in two frames.
            messages
            (e/server
              (or (assistant/messages* [assistant-rev stream-rev] user-id active)
                []))
            ;; The VIEW is what gets diffed, so a row's shape and its content
            ;; cross in one frame. Diffing the raw rows and reading each view
            ;; through a nested (e/server …) made the content a second, demand
            ;; driven transfer: e/Count gave the client the row count a round
            ;; trip early, e/for mounted that many rows, and every one of them
            ;; was empty until its view arrived. That is the blank on send.
            message-rows
            (e/server
              (e/diff-by :assistant_messages/id
                (mapv assistant/transcript-message-view messages)))
            message-count (e/Count message-rows)
            echo-arrived?
            (e/server (assistant/message-client-id-present? messages (:id echo)))]
        ;; Retire the optimistic echo once the durable row it stands for is in
        ;; the rows above. Reading the rendered rows is what keeps the panel
        ;; from painting empty between the echo leaving and the row arriving.
        ;; It also stops a completed echo reappearing after a chat switch.
        (when (and echo echo-arrived?)
          (reset! !echo nil))
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
          (dom/props {:class "assistant-panel__rows"})
          (e/for [display-message message-rows]
            (transcript-entry/AssistantTranscriptEntry
              display-message user-id page-topic-id root-topic-id sending?
              !details-run !evidence !error)))
        (when (zero? message-count)
          (if (and (not sending?) (nil? echo))
            (rows/EmptyChatStarters mode-entry web-search? !submit !active !refs)
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
          [message-count stream-rev assistant-rev sending? (:id echo)])))))
