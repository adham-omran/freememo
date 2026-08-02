(ns freememo.assistant-panel
  "AI assistant tab content for the right side panel: chat picker + New Chat,
   transcript, and composer.

   Chats are per-document (root-topic-id). Sending with no active chat creates
   one first. The learner's reading context is not stored in the transcript —
   it is injected transiently per send server-side, so the transcript shown here
   is exactly the learner/assistant turns.

   Reactivity: subscribes to :assistant-mutations (bumped on chat create + each
   message insert) so the list and transcript re-query. The learner's own turn
   appears before the reply because send! bumps right after persisting it.

   Persona: each chat carries its own mode (freememo.assistant-modes). The Mode
   pills therefore show the OPEN chat's persona, not a standing preference; with
   no chat open they stage `!pending-mode`, which rides chat creation. The chrome
   rows live in freememo.assistant-panel-rows — this ns keeps every
   `(e/server …)` form binding, so window rows and scalars cross the wire instead
   of whole collections."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.assistant-modes :as modes]
   [freememo.assistant-panel-rows :as rows]
   [freememo.typeahead :refer [Typeahead]]
   [freememo.viewport :as viewport]
   #?(:cljs [freememo.vendor-libs :as vendor])
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

(defn render-math!
  "CLJS-only: render KaTeX math (`\\(…\\)` inline, `\\[…\\]` display) in `node`.
   CLJ no-op. Call AFTER node's innerHTML is set.

   KaTeX is vendored without a `<script>` tag; `vendor-libs/ensure! :katex`
   returns the one shared load promise, so a message mounted before KaTeX
   arrives still renders the instant it does — no polling, and no bounded timer
   that could lose the race or leak. A failed load rejects, the `.then` never
   runs, and math stays literal (no crash, no hang).

   The client carries no `$` delimiter: `freememo.markdown/dollar-math->tex` has
   already rewritten real math to `\\(…\\)`/`\\[…\\]` server-side, so a currency
   `$` can never open math here. `throwOnError:false` shows a bad expression as
   source instead of throwing. Code/`pre` are skipped (KaTeX default ignoredTags).

   Plain defn so the reader conditional stays invisible to Electric's reactive
   compiler (CLJ/CLJS signal parity)."
  [node]
  #?(:cljs
     (.then (vendor/ensure! :katex)
       (fn [_]
         (js/renderMathInElement node
           #js {:delimiters #js [#js {:left "\\[" :right "\\]" :display true}
                                 #js {:left "\\(" :right "\\)" :display false}]
                :throwOnError false})))
     :clj nil))

(defn scroll-to-bottom!
  "CLJS-only: pin `node`'s scroll position to its bottom. CLJ no-op.
   `_dep` is a value Electric watches so the call re-fires when the transcript
   changes (message added, thinking toggled). Plain defn for signal parity."
  [node _dep]
  #?(:cljs (set! (.-scrollTop node) (.-scrollHeight node))
     :clj nil))

(defn focus!
  "CLJS-only: focus `node` when `armed?` is true. Called in the reactive body so
   Electric re-fires it only when `armed?` changes value — i.e. once when a send
   completes and the composer re-enables (armed? = (not sending?) rises), not on
   every frame, so it never fights the learner's own focus. Defers a tick so the
   `disabled` prop has cleared before focus. CLJ no-op; plain defn for parity."
  [node armed?]
  #?(:cljs (when (and node armed?)
             (js/setTimeout (fn [] (.focus node)) 0))
     :clj nil))

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
          ;; appears in the transcript (send! stores :id as the row's client_id and
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
                                 (-> (:assistant_messages/content m)
                                   markdown/unwrap-non-math-dollars
                                   markdown/parse-markdown
                                   markdown/dollar-math->tex))
                               m))
                       (or (assistant/messages* assistant-rev user-id active) [])))
          sending? (some? t)
          ;; The persona in force: the open chat's own mode, else the staged one.
          ;; The fallback also covers the gap right after ＋ New, before the
          ;; :assistant-mutations bump puts the new chat in chat-options — the
          ;; staged value is exactly what the row was created with, so the pills
          ;; do not flash the default. Chat ids are ints, so a nil `active`
          ;; matches nothing and falls through. resolve-mode is total, so a NULL
          ;; column (every chat predating it) reads as Socratic.
          mode-entry (modes/resolve-mode
                       (or (some (fn [c] (when (= active (:id c)) (:mode c))) chat-options)
                         pending-mode))
          ;; Touch devices: never steal focus into the composer — an autofocus
          ;; here pops the on-screen keyboard the moment the panel opens.
          coarse? (e/watch viewport/!coarse?)]

      ;; Send effect: fires when a submit payload is produced (Send / Enter /
      ;; suggested prompt). ensure-and-send! creates the chat when :chat is nil,
      ;; so the very first message auto-creates a chat carrying this page context.
      (when t
        (let [{:keys [id text chat mode] ref-ids :refs} submit
              refs-snapshot (e/snapshot refs)] ; frozen at mount for failure-restore
          (if (str/blank? text)
            (t)
            ;; Optimistic: clear composer + chips and echo the turn immediately.
            ;; All are restored below if the send fails, so nothing is lost.
            (do
              (reset! !draft "")
              (reset! !refs [])
              (reset! !echo {:id id :text text})
              (let [r (e/server (e/Offload
                                  #(assistant/ensure-and-send!
                                     user-id root-topic-id page-topic-id chat mode
                                     text ref-ids id)))]
                (case r
                  (do
                    (reset! !active (:chat-id r)) ; select the (possibly new) chat
                    (if (:ok r)
                      (reset! !error nil) ; chips consumed by this turn
                      (do (reset! !error (:error r))
                          (reset! !draft text)
                          (reset! !refs refs-snapshot)))
                    (reset! !echo nil) ; echo already stopped rendering on id correlation
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
                (if (= "assistant" (:assistant_messages/role m))
                  ;; Assistant: rendered Markdown HTML (math typeset) + a per-reply
                  ;; ✦ card-gen button. innerHTML is set on a dedicated child so it
                  ;; never clobbers the sibling button; dir="auto" keeps RTL right.
                  (dom/div
                    (dom/props {:class "assistant-msg-group"})
                    (dom/div
                      (dom/props {:class "assistant-msg assistant-msg--assistant" :dir "auto"})
                      (set! (.-innerHTML dom/node) (or (:assistant_messages/content-html m) ""))
                      (render-math! dom/node))
                    (dom/div
                      (dom/props {:class "assistant-msg-actions"})
                      (dom/button
                        (dom/props {:class "assistant-msg-card" :type "button"
                                    :title "Generate a flashcard from this reply"
                                    :aria-label "Generate a flashcard from this reply"})
                        ;; Per-bubble token (this row's frame): captures run
                        ;; concurrently across bubbles, disabling only their own.
                        (let [click (dom/On "click" identity nil)
                              [gt _] (e/Token click)
                              gen-active? (some? gt)]
                          (dom/props {:disabled gen-active?})
                          (dom/text (if gen-active? "✦…" "✦"))
                          (when gt
                            (let [content (:assistant_messages/content m)]
                              (case (e/server (e/Offload
                                                #(assistant/reply->cards!
                                                   user-id page-topic-id root-topic-id content)))
                                (gt))))))))
                  ;; User: literal text, no Markdown/math.
                  (dom/div
                    (dom/props {:class (str "assistant-msg assistant-msg--"
                                         (:assistant_messages/role m))})
                    (dom/text (:assistant_messages/content m)))))))
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
          (when sending?
            (dom/div
              (dom/props {:class "assistant-panel__thinking"})
              (dom/text "Thinking…")))
          ;; Scroll to bottom when a message lands, "Thinking…" toggles, or the
          ;; optimistic echo appears — follows the user's turn down, then the reply.
          (scroll-to-bottom! dom/node [(count messages) sending? (:id echo)]))

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

        ;; Composer: Enter sends, Shift+Enter newlines, `@` opens the doc picker.
        ;; The Send (↑) button sits beside the input, aligned to its bottom.
        (dom/div
          (dom/props {:class "assistant-panel__composer"})
          (dom/textarea
            (dom/props {:class "input assistant-panel__input" :dir "auto" :rows "2"
                        :placeholder "Ask about this page… (@ to reference a document)"
                        :value draft :disabled sending?})
            (let [v (dom/On "input" #(-> % .-target .-value) nil)]
              (when (some? v) (reset! !draft v)))
            (dom/On "keydown"
              (fn [e]
                (cond
                  (and (= (.-key e) "Enter") (not (.-shiftKey e)))
                  (do (.preventDefault e)
                    (when-let [ed (submit-edit !draft !active !refs (:id mode-entry))]
                      (reset! !submit ed)))
                  (= (.-key e) "@")
                  (reset! !at-open? true))) ; the `@` still types into the draft
              nil)
            ;; Return the caret to the composer once a reply lands (edge-fired).
            (case (focus! dom/node (and (not sending?) (not coarse?))) nil))
          (dom/div
            (dom/props {:class "assistant-panel__composer-actions"})
            (dom/button
              (dom/props {:class "btn btn-primary assistant-panel__send"
                          :type "button" :title "Send" :aria-label "Send"
                          :disabled (or sending? (str/blank? draft))})
              (dom/text "↑")
              (dom/On "click"
                (fn [_] (when-let [ed (submit-edit !draft !active !refs (:id mode-entry))]
                          (reset! !submit ed)))
                nil))))))))
