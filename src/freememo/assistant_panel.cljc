(ns freememo.assistant-panel
  "AI assistant tab content for the right side panel: chat picker + New Chat,
   transcript, and composer.

   Chats are per-document (root-topic-id). Sending with no active chat creates
   one first, whose message #1 is the current page context (server-resolved).
   Message #1 is hidden from the transcript — it is machinery, not conversation.

   Reactivity: subscribes to :assistant-mutations (bumped on chat create + each
   message insert) so the list and transcript re-query. The learner's own turn
   appears before the reply because send! bumps right after persisting it."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   #?(:clj [freememo.assistant :as assistant])
   #?(:clj [freememo.markdown :as markdown])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.user-state :as us])))

(defn- submit-edit
  "Snapshot the composer state into a one-shot submit token payload."
  [draft-atom active-atom]
  (let [text (str/trim @draft-atom)]
    (when-not (str/blank? text)
      {:id (str (random-uuid)) :text text :chat @active-atom})))

(defn typeset!
  "CLJS-only: render MathJax math ($…$ inline, $$…$$ display) in `node`. CLJ no-op.
   Call AFTER node's innerHTML is set; re-typeset re-reads the source delimiters
   (which the fresh innerHTML restored), so re-renders never nest math.

   MathJax loads async from a CDN and Electric mounts each message frame once
   (work-skipping), so a one-shot typeset silently no-ops for any message already
   on screen while the script is still loading — the common case when an existing
   chat is opened on page load. Retry on a short timer until MathJax is ready,
   bounded (~5s) so a blocked CDN can't leak a forever-timer.

   Plain defn so the reader conditional stays invisible to Electric's reactive
   compiler (CLJ/CLJS signal parity)."
  [node]
  #?(:cljs
     (letfn [(attempt [tries]
               (let [mj (.-MathJax js/window)]
                 (cond
                   (and mj (fn? (.-typesetPromise mj))) (.typesetPromise mj #js [node])
                   (pos? tries) (js/setTimeout #(attempt (dec tries)) 100)
                   :else nil)))]
       (attempt 50))
     :clj nil))

(defn scroll-to-bottom!
  "CLJS-only: pin `node`'s scroll position to its bottom. CLJ no-op.
   `_dep` is a value Electric watches so the call re-fires when the transcript
   changes (message added, thinking toggled). Plain defn for signal parity."
  [node _dep]
  #?(:cljs (set! (.-scrollTop node) (.-scrollHeight node))
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
          assistant-rev (e/server (e/watch (us/get-atom user-id :assistant-mutations)))
          chats (e/server (vec (assistant/chats* assistant-rev user-id root-topic-id)))
          ;; Assistant replies are Markdown + `$$…$$` display math; render them
          ;; as HTML (client injects + MathJax typesets). User rows are the
          ;; learner's own literal text — left untouched, shown via dom/text.
          messages (e/server
                     (mapv (fn [m]
                             (if (= "assistant" (:assistant_messages/role m))
                               (assoc m :assistant_messages/content-html
                                 (markdown/parse-markdown (:assistant_messages/content m)))
                               m))
                       (or (assistant/messages* assistant-rev user-id active) [])))
          sending? (some? t)]

      ;; Send effect: fires when a submit payload is produced (Send / Enter).
      ;; ensure-and-send! creates the chat when :chat is nil, so the very first
      ;; message auto-creates a chat carrying this page's context.
      (when t
        (let [{:keys [text chat]} submit]
          (if (str/blank? text)
            (t)
            ;; Clear the composer immediately — the text is already snapshot in
            ;; `submit`. Restored below if the send fails, so nothing is lost.
            (do
              (reset! !draft "")
              (let [r (e/server (e/Offload
                                  #(assistant/ensure-and-send!
                                     user-id root-topic-id page-topic-id chat text)))]
                (case r
                  (do
                    (reset! !active (:chat-id r)) ; select the (possibly new) chat
                    (if (:ok r)
                      (reset! !error nil)
                      (do (reset! !error (:error r))
                          (reset! !draft text)))
                    (t))))))))

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
                                      #(assistant/start-chat! user-id root-topic-id page-topic-id)))]
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

        ;; Transcript (message #1, the page context, is skipped via rest).
        (dom/div
          (dom/props {:class "assistant-panel__transcript"})
          (let [visible (vec (rest messages))]
            (if (empty? visible)
              (dom/div
                (dom/props {:class "assistant-panel__hint"})
                (dom/text "Ask a question about this page and I'll help you think it through."))
              (e/for-by :assistant_messages/id [m visible]
                (if (= "assistant" (:assistant_messages/role m))
                  ;; Assistant: inject rendered Markdown HTML, then typeset math.
                  ;; dir="auto" keeps RTL replies right-aligned.
                  (dom/div
                    (dom/props {:class "assistant-msg assistant-msg--assistant" :dir "auto"})
                    (set! (.-innerHTML dom/node) (or (:assistant_messages/content-html m) ""))
                    (typeset! dom/node))
                  ;; User: literal text, no Markdown/math.
                  (dom/div
                    (dom/props {:class (str "assistant-msg assistant-msg--"
                                         (:assistant_messages/role m))})
                    (dom/text (:assistant_messages/content m)))))))
          (when sending?
            (dom/div
              (dom/props {:class "assistant-panel__thinking"})
              (dom/text "Thinking…")))
          ;; Scroll to bottom when a message lands or "Thinking…" toggles —
          ;; follows the user's turn down and then the reply.
          (scroll-to-bottom! dom/node [(count messages) sending?]))

        (when error
          (dom/div
            (dom/props {:class "assistant-panel__error"})
            (dom/text error)))

        ;; Composer: Enter sends, Shift+Enter newlines, button sends.
        (dom/div
          (dom/props {:class "assistant-panel__composer"})
          (dom/textarea
            (dom/props {:class "input assistant-panel__input" :dir "auto" :rows "2"
                        :placeholder "Ask about this page…"
                        :value draft :disabled sending?})
            (let [v (dom/On "input" #(-> % .-target .-value) nil)]
              (when (some? v) (reset! !draft v)))
            (dom/On "keydown"
              (fn [e]
                (when (and (= (.-key e) "Enter") (not (.-shiftKey e)))
                  (.preventDefault e)
                  (when-let [ed (submit-edit !draft !active)]
                    (reset! !submit ed))))
              nil))
          (dom/button
            (dom/props {:class "btn btn-primary assistant-panel__send"
                        :type "button" :disabled (or sending? (str/blank? draft))})
            (dom/text "Send")
            (dom/On "click"
              (fn [_] (when-let [ed (submit-edit !draft !active)]
                        (reset! !submit ed)))
              nil)))))))
