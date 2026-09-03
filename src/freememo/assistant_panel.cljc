(ns freememo.assistant-panel
  "AI assistant tab content for the right side panel: chat picker + New Chat,
   transcript, and composer.

   Chats are per-document. Sending with no active chat creates one first. Each
   submitted turn creates a durable streamed run. Reading sources stay outside
   the transcript and remain available in the answer details.

   Reactivity: subscribes to :assistant-mutations. Stream workers publish at a
   bounded rate so the transcript can show durable partial text.

   Persona: each chat carries its own mode (freememo.assistant-modes). The Mode
   pills therefore show the OPEN chat's persona, not a standing preference; with
   no chat open they stage `!pending-mode`, which rides chat creation. The chrome
   rows live in freememo.assistant-panel-rows — this ns keeps every
   `(e/server …)` form binding, so window rows and scalars cross the wire instead
   of whole collections."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [clojure.string :as str]
   [freememo.assistant-modes :as modes]
   [freememo.assistant-panel-rows :as rows]
   [freememo.assistant-run-details :as run-details]
   [freememo.math :as math]
   [freememo.typeahead :refer [Typeahead]]
   #?(:clj [freememo.assistant :as assistant])
   #?(:clj [freememo.markdown :as markdown])
   #?(:clj [freememo.user-state :as us])))

(defn- submit-edit
  "Snapshot the composer state into a one-shot submit token payload, including
   the current @-referenced document ids and the persona in force.

   `:mode` is frozen here rather than read during the send, so a pill tapped
   while the request is in flight cannot change which persona the new chat is
   created with. It is used only when `:chat` is nil — an existing chat keeps its
   own mode.
   Pre:  `mode-id` is a mode :id. Post: nil when the draft is blank."
  [draft-atom active-atom refs-atom mode-id]
  (let [text (str/trim @draft-atom)]
    (when-not (str/blank? text)
      {:id (str (random-uuid)) :text text :chat @active-atom
       :refs (mapv :id @refs-atom) :mode mode-id})))

(defn scroll-to-bottom!
  "CLJS-only: pin `node`'s scroll position to its bottom. CLJ no-op.
   `_dep` is a value Electric watches so the call re-fires when the transcript
   changes (message added, thinking toggled). Plain defn for signal parity."
  [node _dep]
  #?(:cljs (set! (.-scrollTop node) (.-scrollHeight node))
     :clj nil))

(e/defn AssistantTranscriptEntry
  [message user-id page-topic-id root-topic-id sending? !details-run !error]
  (e/client
    (cond
      (= "event" (:assistant_messages/role message))
      (dom/element "details"
        (dom/props {:class "assistant-compact-event"})
        (dom/element "summary" (dom/text "Context compacted"))
        (dom/p
          (dom/props {:class "assistant-compact-event__range"})
          (dom/text (str "Messages " (:assistant_event/covered_from message)
                      "–" (:assistant_event/covered_through message))))
        (dom/div
          (dom/props {:class "assistant-compact-event__summary" :dir "auto"})
          (dom/text (:assistant_messages/content message))))

      (= "assistant" (:assistant_messages/role message))
      (let [status (:assistant_run/status message)
            content (:assistant_messages/content message)
            run-uuid (:assistant_run/uuid message)]
        (dom/div
          (dom/props {:class "assistant-msg-group"})
          (dom/div
            (dom/props {:class "assistant-msg assistant-msg--assistant" :dir "auto"})
            (if (str/blank? content)
              (when-not (or (= "failed" status) (= "stopped" status))
                (dom/span
                  (dom/props {:class "assistant-msg-status"})
                  (dom/text "Thinking…")))
              (do
                (set! (.-innerHTML dom/node)
                  (or (:assistant_messages/content-html message) ""))
                (math/render-math! dom/node)))
            (when (= "stopped" status)
              (dom/div
                (dom/props {:class "assistant-msg-status"})
                (dom/text "Stopped")))
            (when (= "failed" status)
              (dom/div
                (dom/props {:class "assistant-msg-status assistant-msg-status--failed"})
                (dom/text (str "Failed"
                            (when-let [failure (:assistant_run/error message)]
                              (str ": " failure)))))))
          (when run-uuid
            (dom/div
              (dom/props {:class "assistant-msg-actions"})
              (when-not (str/blank? content)
                (dom/button
                  (dom/props {:class "assistant-msg-card" :type "button"
                              :title "Generate a flashcard from this reply"
                              :aria-label "Generate a flashcard from this reply"
                              :disabled sending?})
                  (let [click (dom/On "click" identity nil)
                        [generate-token _] (e/Token click)
                        generating? (some? generate-token)]
                    (dom/props {:disabled (or sending? generating?)})
                    (dom/text (if generating? "✦…" "✦"))
                    (when generate-token
                      (case (e/server (e/Offload
                                        #(assistant/reply->cards!
                                           user-id page-topic-id root-topic-id content)))
                        (generate-token))))))
              (dom/button
                (dom/props {:class "assistant-msg-card" :type "button"
                            :title "Answer details" :aria-label "Answer details"})
                (dom/text "[i]")
                (dom/On "click" (fn [_] (reset! !details-run run-uuid)) nil))
              (when (= "failed" status)
                (dom/button
                  (dom/props {:class "assistant-msg-card" :type "button"
                              :title "Retry answer" :aria-label "Retry answer"
                              :disabled sending?})
                  (dom/text "↻")
                  (let [retry-click (dom/On "click" identity nil)
                        [retry-token _] (e/Token retry-click)]
                    (when retry-token
                      (let [result (e/server
                                     (e/Offload
                                       #(assistant/retry-answer-run!
                                          user-id run-uuid (str (random-uuid)))))]
                        (case result
                          (case (reset! !error (when-not (:ok result) (:error result)))
                            (retry-token))))))))))))

      :else
      (dom/div
        (dom/props {:class (str "assistant-msg assistant-msg--"
                             (:assistant_messages/role message))})
        (dom/text (:assistant_messages/content message))))))

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
          ;; @-referenced documents queued for the next send (shown as chips);
          ;; their ids ride the submit payload and clear on a successful send.
          !refs (atom [])
          refs (e/watch !refs)
          !at-open? (atom false)
          at-open? (e/watch !at-open?)
          ;; Typeahead is id-keyed: both atoms hold a document id or nil, never
          ;; a title. Two documents may share a title; the id is what resolves.
          !pick-search (atom nil)
          !picked (atom nil)
          picked (e/watch !picked)
          ;; Optimistic echo of the just-sent user turn: {:id :text} or nil.
          ;; Rendered as a user bubble until the persisted row carrying this :id
          ;; appears in the transcript (run creation stores :id as client_id and
          ;; bumps before the reply), so the echo and the real row never both show.
          !echo (atom nil)
          echo (e/watch !echo)
          ;; Mode staged for the NEXT chat: the pills write it, chat creation
          ;; consumes it. An open chat's persona comes off its own row instead.
          !pending-mode (atom modes/default-id)
          pending-mode (e/watch !pending-mode)
          assistant-rev (e/server (e/watch (us/get-atom user-id :assistant-mutations)))
          ;; Server-sited so one vector crosses instead of a row at a time; the
          ;; chat Typeahead filters client-side, so it needs the whole list. :mode
          ;; rides along, which is how the pills learn the open chat's persona
          ;; without a second query.
          chat-options (e/server
                         (mapv (fn [c] {:id (:assistant_chats/id c)
                                        :label (or (:assistant_chats/title c) "Untitled")
                                        :mode (:assistant_chats/mode c)})
                           (assistant/chats* assistant-rev user-id root-topic-id)))
          ;; Other documents the learner can @-reference (current doc excluded).
          docs (e/server (vec (assistant/referenceable-docs user-id root-topic-id)))
          doc-options (mapv (fn [d] {:id (:id d) :label (:title d)}) docs)
          by-id (into {} (map (juxt :id identity)) docs)
          ;; Assistant replies are Markdown + dollar-delimited math; render to
          ;; HTML, then rewrite real math to `\(…\)`/`\[…\]` so the client (KaTeX)
          ;; needs no `$` delimiter and currency `$` never opens math. User rows
          ;; are the learner's own literal text — untouched, shown via dom/text.
          messages (e/server
                     (mapv (fn [m]
                             (if (= "assistant" (:assistant_messages/role m))
                               (assoc m :assistant_messages/content-html
                                 (-> (assistant/render-evidence-citations
                                       (:assistant_messages/content m)
                                       (:assistant_run/evidence m))
                                   markdown/unwrap-non-math-dollars
                                   markdown/parse-markdown
                                   markdown/dollar-math->tex))
                               m))
                       (or (assistant/messages* assistant-rev user-id active) [])))
          active-run (e/server (assistant/active-run* assistant-rev user-id active))
          details (e/server (assistant/run-details* assistant-rev user-id details-run))
          sending? (some? active-run)
          ;; The persona in force: the open chat's own mode, else the staged one.
          ;; The fallback also covers the gap right after ＋ New, before the
          ;; :assistant-mutations bump puts the new chat in chat-options — the
          ;; staged value is exactly what the row was created with, so the pills
          ;; do not flash the default. Chat ids are ints, so a nil `active`
          ;; matches nothing and falls through. resolve-mode is total, so a NULL
          ;; column (every chat predating it) reads as Socratic.
          mode-entry (modes/resolve-mode
                       (or (some (fn [c] (when (= active (:id c)) (:mode c))) chat-options)
                         pending-mode))]

      ;; Submit effect: ordinary text starts a streamed answer. `/compact` starts
      ;; a separate compaction run and never enters the transcript as user text.
      (when t
        (let [{:keys [id text chat mode] ref-ids :refs} submit
              refs-snapshot (e/snapshot refs)
              compact? (= "/compact" (str/trim text))]
          (if (str/blank? text)
            (t)
            (do
              (reset! !draft "")
              (reset! !refs [])
              (reset! !echo (when-not compact? {:id id :text text}))
              (let [r (e/server (e/Offload
                                  #(assistant/submit-chat-input!
                                     user-id root-topic-id page-topic-id chat mode
                                     text ref-ids id)))]
                (case r
                  (do
                    (reset! !active (:chat-id r))
                    (if (:ok r)
                      (reset! !error nil)
                      (do (reset! !error (:error r))
                          (reset! !draft text)
                          (reset! !refs refs-snapshot)
                          (reset! !echo nil)))
                    (t))))))))

      ;; @-reference commit: Typeahead wrote the chosen document's id to !picked.
      ;; Add the matching doc as a chip (dedup by id), strip the trailing `@token`
      ;; the trigger left in the draft, and close the picker.
      (let [[pt _] (e/Token picked)]
        (when pt
          (when-let [doc (get by-id picked)]
            (swap! !refs (fn [rs]
                           (if (some #(= (:id doc) (:id %)) rs)
                             rs
                             (conj rs {:id (:id doc) :title (:title doc)})))))
          (reset! !draft (str/replace draft #"@\S*$" ""))
          (reset! !pick-search nil)
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
        ;; Row 3: Mode pills. Shows the OPEN chat's persona; with no chat open a
        ;; tap only stages !pending-mode for the chat about to be created.
        (rows/ModeRow user-id active mode-entry !pending-mode)
        ;; Row 4: per-document assistant model. "" = use my global default.
        (rows/ModelRow user-id root-topic-id)

        ;; Transcript — every stored row is a real learner/assistant turn now
        ;; (reading context is injected transiently server-side, not persisted).
        (dom/div
          (dom/props {:class "assistant-panel__transcript"})
          (let [visible (vec messages)]
            (if (empty? visible)
              ;; Empty state: this persona's hint + starters — hidden once a send
              ;; is in flight (echo up or awaiting reply) so they don't sit beside
              ;; the echoed turn.
              (when (and (not sending?) (nil? echo))
                (rows/EmptyChatStarters mode-entry !submit !active !refs))
              (e/for-by :assistant_messages/id [m visible]
                (AssistantTranscriptEntry m user-id page-topic-id root-topic-id
                  sending? !details-run !error))))
          ;; Optimistic echo of the just-sent turn (user bubble). Rendered only
          ;; until the persisted row carrying this echo's id lands in the
          ;; transcript — a pure id correlation, so the echo and the real row
          ;; never both show and no reactive write is needed (the atom is cleared
          ;; at token completion).
          (when-let [e-text (and echo
                              (not-any? #(= (:id echo) (:assistant_messages/client_id %)) messages)
                              (:text echo))]
            (dom/div
              (dom/props {:class "assistant-msg assistant-msg--user"})
              (dom/text e-text)))
          (when (and sending? (= "compact" (:kind active-run)))
            (dom/div
              (dom/props {:class "assistant-panel__thinking"})
              (dom/text "Compacting context…")))
          ;; Scroll to bottom when a message lands, "Thinking…" toggles, or the
          ;; optimistic echo appears — follows the user's turn down, then the reply.
          (scroll-to-bottom! dom/node
            [(count messages) sending? (:id echo)
             (some-> messages last :assistant_messages/content count)]))

        (run-details/RunDetailsModal !details-run details)

        (when error
          (dom/div
            (dom/props {:class "assistant-panel__error"})
            (dom/text error)))

        ;; `@` picker: reuses Typeahead over other documents (titles shown, ids
        ;; written). Committing a pick is handled by the @-reference effect above
        ;; (writes the id to !picked).
        (when at-open?
          (dom/div
            (dom/props {:class "assistant-panel__at-popover"})
            (Typeahead !pick-search doc-options "Reference a document…" !picked true)))

        ;; Reference chips — one per queued @-document, removable.
        (when (seq refs)
          (dom/div
            (dom/props {:class "assistant-panel__refs"})
            (e/for-by :id [r refs]
              (dom/span
                (dom/props {:class "assistant-panel__ref-chip"})
                (dom/text (str "@" (:title r)))
                (dom/button
                  (dom/props {:class "assistant-panel__ref-remove" :type "button"
                              :title "Remove reference" :aria-label "Remove reference"})
                  (dom/text "×")
                  (let [ev (dom/On "click" identity nil)
                        [rt _] (e/Token ev)]
                    (when rt
                      (swap! !refs (fn [rs] (into [] (remove #(= (:id r) (:id %))) rs)))
                      (rt))))))))

        ;; Forms5 owns the composer and command buttons. The service acknowledges
        ;; local draft edits immediately and sequences server commands by token.
        (dom/div
          (dom/props {:class "assistant-panel__composer"})
          (dom/On "keydown"
            (fn [event]
              (cond
                (and (= (.-key event) "Enter")
                  (not (.-shiftKey event))
                  (not sending?))
                (do
                  (.preventDefault event)
                  (when-let [edit (submit-edit !draft !active !refs (:id mode-entry))]
                    (reset! !submit edit)))

                (= (.-key event) "@")
                (reset! !at-open? true)))
            nil)
          (e/for [[command-token command]
                  (e/amb
                    (forms/Input! :assistant/draft draft :as :textarea
                      :class "input assistant-panel__input" :dir "auto" :rows 2
                      :placeholder "Ask about this page… (@ to reference a document)"
                      :aria-label "Assistant message" :disabled sending?)
                    (if sending?
                      (forms/Button! [::stop]
                        :class "btn btn-secondary assistant-panel__send"
                        :label "Stop" :aria-label "Stop response")
                      (forms/Button! [::submit]
                        :class "btn btn-primary assistant-panel__send"
                        :label "↑" :aria-label "Send"
                        :disabled (str/blank? draft))))]
            (cond
              (map? command)
              (case (reset! !draft (:assistant/draft command))
                (command-token))

              (= [::submit] command)
              (case (when-let [edit (submit-edit !draft !active !refs (:id mode-entry))]
                      (reset! !submit edit))
                (command-token))

              (= [::stop] command)
              (let [result (e/server (e/Offload
                                       #(assistant/stop-active-run! user-id active)))]
                (case result
                  (case (reset! !error (when-not (:stopped result)
                                         "No active response to stop."))
                    (command-token)))))))))))
