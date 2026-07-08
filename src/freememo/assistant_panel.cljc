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
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.card-models :as card-models])
   #?(:clj [freememo.user-state :as us])))

(defn- submit-edit
  "Snapshot the composer state into a one-shot submit token payload."
  [draft-atom active-atom]
  (let [text (str/trim @draft-atom)]
    (when-not (str/blank? text)
      {:id (str (random-uuid)) :text text :chat @active-atom})))

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
          messages (e/server (vec (or (assistant/messages* assistant-rev user-id active) [])))
          model-label (e/server (:label (card-models/resolve-model
                                          (settings/get-assistant-model user-id))))
          sending? (some? t)]

      ;; Send effect: fires when a submit payload is produced (Send / Enter).
      ;; ensure-and-send! creates the chat when :chat is nil, so the very first
      ;; message auto-creates a chat carrying this page's context.
      (when t
        (let [{:keys [text chat]} submit]
          (if (str/blank? text)
            (t)
            (let [r (e/server (e/Offload
                                #(assistant/ensure-and-send!
                                   user-id root-topic-id page-topic-id chat text)))]
              (case r
                (do
                  (reset! !active (:chat-id r)) ; select the (possibly new) chat
                  (if (:ok r)
                    (do (reset! !draft "") (reset! !error nil))
                    (reset! !error (:error r)))
                  (t)))))))

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
        (dom/div
          (dom/props {:class "assistant-panel__model"})
          (dom/text (str "Socratic tutor · " (or model-label "AI"))))

        ;; Transcript (message #1, the page context, is skipped via rest).
        (dom/div
          (dom/props {:class "assistant-panel__transcript"})
          (let [visible (vec (rest messages))]
            (if (empty? visible)
              (dom/div
                (dom/props {:class "assistant-panel__hint"})
                (dom/text "Ask a question about this page and I'll help you think it through."))
              (e/for-by :assistant_messages/id [m visible]
                (dom/div
                  (dom/props {:class (str "assistant-msg assistant-msg--"
                                       (:assistant_messages/role m))})
                  (dom/text (:assistant_messages/content m))))))
          (when sending?
            (dom/div
              (dom/props {:class "assistant-panel__thinking"})
              (dom/text "Thinking…"))))

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
