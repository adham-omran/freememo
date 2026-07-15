(ns freememo.assistant-panel
  "AI assistant tab content for the right side panel: chat picker + New Chat,
   transcript, and composer.

   Chats are per-document (root-topic-id). Sending with no active chat creates
   one first. The learner's reading context is not stored in the transcript —
   it is injected transiently per send server-side, so the transcript shown here
   is exactly the learner/assistant turns.

   Reactivity: subscribes to :assistant-mutations (bumped on chat create + each
   message insert) so the list and transcript re-query. The learner's own turn
   appears before the reply because send! bumps right after persisting it."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.typeahead :refer [Typeahead]]
   [freememo.viewport :as viewport]
   #?(:clj [freememo.assistant :as assistant])
   #?(:clj [freememo.markdown :as markdown])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.user-state :as us])))

(def ^:private suggested-prompts
  "First-message starters shown on an empty chat. The first is the learner's
   own seed; the rest cover prerequisites, plain-language recall and connection."
  ["What prerequisites do I need to understand this material?"
   "Explain the core idea of this page in simple terms."
   "What questions should I be able to answer after reading this?"
   "How does this connect to things I might already know?"])

(defn- submit-edit
  "Snapshot the composer state into a one-shot submit token payload, including
   the current @-referenced document ids."
  [draft-atom active-atom refs-atom]
  (let [text (str/trim @draft-atom)]
    (when-not (str/blank? text)
      {:id (str (random-uuid)) :text text :chat @active-atom
       :refs (mapv :id @refs-atom)})))

(defn render-math!
  "CLJS-only: render KaTeX math (`\\(…\\)` inline, `\\[…\\]` display) in `node`.
   CLJ no-op. Call AFTER node's innerHTML is set.

   KaTeX's auto-render script loads async from a CDN; `window.__katexReady`
   (defined in index.html) resolves with `renderMathInElement` once it is
   available. Chaining each message's render on that promise means a message
   mounted before KaTeX loads still renders the instant it arrives — no polling,
   and no bounded timer that could lose the race or leak. A blocked CDN simply
   leaves the promise pending, so math stays literal (no crash, no hang).

   The client carries no `$` delimiter: `freememo.markdown/dollar-math->tex` has
   already rewritten real math to `\\(…\\)`/`\\[…\\]` server-side, so a currency
   `$` can never open math here. `throwOnError:false` shows a bad expression as
   source instead of throwing. Code/`pre` are skipped (KaTeX default ignoredTags).

   Plain defn so the reader conditional stays invisible to Electric's reactive
   compiler (CLJ/CLJS signal parity)."
  [node]
  #?(:cljs
     (when-let [ready (.-__katexReady js/window)]
       (.then ready
         (fn [render]
           (render node
             #js {:delimiters #js [#js {:left "\\[" :right "\\]" :display true}
                                   #js {:left "\\(" :right "\\)" :display false}]
                  :throwOnError false}))))
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
          !pick-search (atom "")
          !picked (atom nil)
          picked (e/watch !picked)
          ;; Optimistic echo of the just-sent user turn: {:id :text} or nil.
          ;; Rendered as a user bubble until the persisted row carrying this :id
          ;; appears in the transcript (send! stores :id as the row's client_id and
          ;; bumps before the reply), so the echo and the real row never both show.
          !echo (atom nil)
          echo (e/watch !echo)
          assistant-rev (e/server (e/watch (us/get-atom user-id :assistant-mutations)))
          chats (e/server (vec (assistant/chats* assistant-rev user-id root-topic-id)))
          ;; Other documents the learner can @-reference (current doc excluded).
          docs (e/server (vec (assistant/referenceable-docs user-id root-topic-id)))
          doc-titles (mapv :title docs)
          by-title (into {} (map (juxt :title identity)) docs)
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
          ;; Touch devices: never steal focus into the composer — an autofocus
          ;; here pops the on-screen keyboard the moment the panel opens.
          coarse? (e/watch viewport/!coarse?)]

      ;; Send effect: fires when a submit payload is produced (Send / Enter /
      ;; suggested prompt). ensure-and-send! creates the chat when :chat is nil,
      ;; so the very first message auto-creates a chat carrying this page context.
      (when t
        (let [{:keys [id text chat] ref-ids :refs} submit
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
                                     user-id root-topic-id page-topic-id chat text ref-ids id)))]
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

      ;; @-reference commit: Typeahead wrote the chosen title to !picked. Add the
      ;; matching doc as a chip (dedup by id), strip the trailing `@token` the
      ;; trigger left in the draft, and close the picker.
      (let [[pt _] (e/Token picked)]
        (when pt
          (when-let [doc (get by-title picked)]
            (swap! !refs (fn [rs]
                           (if (some #(= (:id doc) (:id %)) rs)
                             rs
                             (conj rs {:id (:id doc) :title (:title doc)})))))
          (reset! !draft (str/replace draft #"@\S*$" ""))
          (reset! !pick-search "")
          (reset! !at-open? false)
          (reset! !picked nil)
          (pt)))

      (dom/div
        (dom/props {:class "assistant-panel"})

        ;; Controls: New Chat + chat picker + model badge.
        (dom/div
          (dom/props {:class "assistant-panel__bar"})
          (dom/button
            (dom/props {:class "btn btn-secondary assistant-panel__new"
                        :type "button" :title "Start a new chat about this page"})
            (dom/text "＋ New")
            (let [ev (dom/On "click" identity nil)
                  [nt _] (e/Token ev)]
              (when nt
                (let [cid (e/server (e/Offload
                                      #(assistant/start-chat! user-id root-topic-id)))]
                  (case cid
                    (do (reset! !active cid) (reset! !draft "") (reset! !error nil) (nt)))))))
          (dom/select
            (dom/props {:class "select assistant-panel__chats" :value (str active)})
            (dom/option (dom/props {:value ""}) (dom/text "Chats…"))
            (e/for-by :assistant_chats/id [c chats]
              (dom/option (dom/props {:value (str (:assistant_chats/id c))})
                (dom/text (or (:assistant_chats/title c) "Untitled"))))
            (let [v (dom/On "change" #(-> % .-target .-value) nil)]
              (when (some? v)
                (reset! !active (when (seq v) (js/parseInt v 10)))
                (reset! !error nil)))))
        ;; Per-document assistant model. "" = use my global default.
        (dom/div
          (dom/props {:class "assistant-panel__model"
                      :style {:display "flex" :align-items "center" :gap "8px"}})
          (dom/span (dom/text "Socratic tutor ·"))
          (let [current (e/server (settings/get-assistant-model-for user-id root-topic-id))
                default-id (e/server (settings/get-assistant-model user-id))
                choices (e/server (settings/card-model-choices))
                ;; Name the global default that "" resolves to, minus the
                ;; "Provider · " prefix (registry labels are "Google · Gemini 3 Flash").
                default-name (str/trim (last (str/split (get (into {} choices) default-id default-id) #"·")))
                options (into [["" (str "Use my default (" default-name ")")]] choices)
                !amodel (atom (e/snapshot (or current "")))
                amodel (e/watch !amodel)]
            (dom/select
              (dom/props {:class "select"})
              (e/for [[v label] (e/diff-by first options)]
                (dom/option (dom/props {:value v :selected (= v amodel)}) (dom/text label)))
              (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                    [mt _] (e/Token change-event)]
                (when (some? change-event)
                  (reset! !amodel change-event))
                (when mt
                  (let [r (e/server (e/Offload #(settings/save-assistant-model-for user-id root-topic-id change-event)))]
                    (case r
                      (if (:success r) (mt) (mt (:error r))))))))))

        ;; Transcript — every stored row is a real learner/assistant turn now
        ;; (reading context is injected transiently server-side, not persisted).
        (dom/div
          (dom/props {:class "assistant-panel__transcript"})
          (let [visible (vec messages)]
            (if (empty? visible)
              ;; Empty state: starters — hidden once a send is in flight (echo up
              ;; or awaiting reply) so they don't sit beside the echoed turn.
              (when (and (not sending?) (nil? echo))
                (dom/div
                  (dom/props {:class "assistant-panel__hint"})
                  (dom/text "Ask a question about this page and I'll help you think it through.")
                  (dom/div
                    (dom/props {:class "assistant-panel__suggestions"})
                    (e/for [p (e/diff-by identity suggested-prompts)]
                      (dom/button
                        (dom/props {:class "assistant-panel__suggestion" :type "button"})
                        (dom/text p)
                        (let [ev (dom/On "click" identity nil)
                              [st _] (e/Token ev)]
                          (when st
                            ;; Send immediately as the chat's first message.
                            (reset! !submit {:id (str (random-uuid)) :text p
                                             :chat @!active :refs (mapv :id @!refs)})
                            (st))))))))
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

        ;; `@` picker: reuses Typeahead over other documents' titles. Committing
        ;; a pick is handled by the @-reference effect above (writes to !picked).
        (when at-open?
          (dom/div
            (dom/props {:class "assistant-panel__at-popover"})
            (Typeahead !pick-search doc-titles "Reference a document…" !picked true)))

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
        ;; The Send (↑) button sits in a row beneath the input.
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
                    (when-let [ed (submit-edit !draft !active !refs)]
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
                (fn [_] (when-let [ed (submit-edit !draft !active !refs)]
                          (reset! !submit ed)))
                nil))))))))
