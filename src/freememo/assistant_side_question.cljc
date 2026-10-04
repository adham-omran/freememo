(ns freememo.assistant-side-question
  "The floating side-question panel: a thread about one passage of an answer.

   The panel is non-blocking and draggable by its title, in the
   card_compare.cljc shape: no backdrop, no `aria-modal`, no Tab trap, so the
   assistant panel behind it stays usable. Escape and ✕ close it.

   The thread lives in one server atom that this panel's frame owns
   (`assistant/new-side-thread`). Closing the panel unmounts the frame, which
   marks the thread closed: a run in flight is still charged, and writes nothing
   more. Nothing is stored. Only one panel exists at a time, because the host
   keys the panel by the request's :key.

   See plans/assistant-answer-selection-actions.md §5.3."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [clojure.string :as str]
   [freememo.assistant-message-content :as message-content]
   [freememo.assistant-transcript :as transcript]
   [freememo.modal-shell :as modal]
   #?(:clj [freememo.assistant :as assistant])))

(defn- replace-side-draft!
  "Clear the side panel's question field, in the atom and on screen. Forms5 owns
   the textarea's value, as in `assistant-composer/replace-composer-draft!`.
   Post: `@!draft` and the visible value are both empty."
  [!draft]
  #?(:cljs (when-let [input (.querySelector js/document ".assistant-side-question__input")]
             (set! (.-value input) ""))
     :clj nil)
  (reset! !draft ""))

(defn- submit-on-enter!
  "Enter without Shift clicks the panel's enabled primary button."
  [e]
  #?(:cljs (when (and (= "Enter" (.-key e)) (not (.-shiftKey e)))
             (when-let [button (.querySelector (.-currentTarget e)
                                 "button.btn-primary:not([disabled])")]
               (.preventDefault e)
               (.click button)))
     :clj nil))

(defn- debug-log!
  "Temporary diagnostics for the side-panel Generate cards command."
  [& parts]
  #?(:cljs (apply js/console.log "[side-debug]" parts) :clj nil))

(e/defn SideQuestionPanel
  "One side question about `side` = {:chat-id :message-id :passage}.
   `!side` closes the panel when reset to nil. `!evidence` opens the panel's
   evidence dialog, as a main answer's citation does."
  [side user-id page-topic-id !side !evidence]
  (e/client
    (dom/div
      (dom/props {:class "assistant-side-question" :role "dialog"
                  :aria-label "Side question"})
      (dom/On "pointerdown" (fn [e] (modal/drag-modal-by-title! e)) nil)
      (dom/On "keydown"
        (fn [e]
          (when (= "Escape" (.-key e))
            (.stopPropagation e)
            (reset! !side nil)))
        nil)
      (modal/FocusReturn)
      (let [!thread (e/server (atom (assistant/new-side-thread
                                      (:passage side) (:message-id side))))
            thread (e/server (e/watch !thread))
            rows (e/server (e/diff-by :assistant_messages/id
                             (assistant/side-thread-rows thread)))
            row-count (e/Count rows)
            newest-size (e/server (count (str (:content (peek (:turns thread))))))
            !draft (atom "")
            draft (e/watch !draft)
            !error (atom nil)
            error (e/watch !error)]
        (e/server (e/on-unmount #(assistant/close-side-thread! !thread)))
        (dom/div
          (dom/props {:class "assistant-side-question__header"})
          (dom/h3 (dom/text "Side question"))
          (dom/button
            (dom/props {:class "assistant-side-question__close" :type "button"
                        :title "Close" :aria-label "Close side question"})
            (dom/text "✕")
            (dom/On "click" (fn [_] (reset! !side nil)) nil)))
        (dom/blockquote
          (dom/props {:class "assistant-side-question__passage" :dir "auto"})
          (dom/text (:passage side)))
        (dom/div
          (dom/props {:class "assistant-side-question__thread"})
          ;; The loop in its own element, mounted always (CLAUDE.md, "An e/for
          ;; Gets an Element of Its Own").
          (dom/div
            (dom/props {:class "assistant-panel__rows"})
            (e/for [row rows]
              (if (= "assistant" (:assistant_messages/role row))
                (message-content/AssistantAnswerContent row !evidence)
                (dom/div
                  (dom/props {:class "assistant-msg assistant-msg--user"})
                  (dom/text (:assistant_messages/content row))))))
          (transcript/scroll-to-bottom! dom/node [row-count newest-size]))
        (dom/div
          (dom/props {:class "assistant-panel__error"
                      :style {:display (if error "block" "none")}})
          (dom/text (or error "")))
        (dom/div
          (dom/props {:class "assistant-side-question__composer"})
          (dom/On "keydown" submit-on-enter! nil)
          (let [commands
                (e/amb
                  (forms/Input! :side/draft draft :as :textarea
                    :class "input assistant-side-question__input" :dir "auto" :rows 2
                    :autofocus true
                    :placeholder "Ask about this passage…"
                    :aria-label "Side question")
                  ;; A dom/div's value is its LAST form, so the two buttons
                  ;; are joined in an `e/amb` of their own. As plain siblings,
                  ;; Generate cards was dropped: its click took a token that
                  ;; no branch ever spent, and the button stayed busy.
                  (dom/div
                    (dom/props {:class "assistant-side-question__buttons"})
                    (e/amb
                      (forms/Button! [::cards]
                        :class "btn btn-sm btn-secondary"
                        :label "Generate cards"
                        :disabled (zero? row-count))
                      (forms/Button! [::ask]
                        :class "btn btn-sm btn-primary"
                        :label "Ask"
                        :disabled (str/blank? draft)))))]
            (e/for [[command-token command] (e/diff-by first (e/as-vec commands))]
              (cond
                (map? command)
                (case (reset! !draft (:side/draft command))
                  (command-token))

                (= [::ask] command)
                ;; Snapshot: the thunk must not restart when the learner types
                ;; on while the call runs, or the question would be asked twice.
                (let [question (e/snapshot (str/trim draft))
                      result (e/server
                               (e/Offload
                                 #(assistant/ask-side-question!
                                    !thread user-id (:chat-id side) page-topic-id
                                    question)))]
                  (case result
                    (if (:ok result)
                      (case (replace-side-draft! !draft)
                        (case (reset! !error nil)
                          (command-token)))
                      (case (reset! !error (:error result))
                        (command-token)))))

                (= [::cards] command)
                (case (debug-log! "side cards branch mounted" (pr-str command))
                  (let [result (e/server
                                 (e/Offload
                                   #(assistant/side-thread->cards!
                                      user-id page-topic-id @!thread)))]
                    (case result
                      (case (reset! !error (:error result))
                        (command-token)))))))))))))

(e/defn SideQuestionHost
  "The side-question panel for `side`, or nothing while `side` is nil. Keyed by
   (:key side), so a new Ask aside replaces the open panel and its thread.
   The host element is always mounted and holds only the panel."
  [side user-id page-topic-id !side !evidence]
  (e/client
    (dom/div
      (dom/props {:class "assistant-side-question-host"})
      (e/for-by identity [_k (if side [(:key side)] [])]
        (SideQuestionPanel side user-id page-topic-id !side !evidence)))))
