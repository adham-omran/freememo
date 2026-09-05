(ns freememo.assistant-panel-rows
  "Assistant controls, the document mention menu, and empty-chat starters.

   Split out of freememo.assistant-panel so each `e/defn` gets its own JVM method
   and the panel stays under the 64KB bytecode limit — the same reason
   toolbar_generate_dropdown / toolbar_sync_dropdown live in their own files. The
   parent keeps every `(e/server …)` form binding, because moving one into an
   `e/defn` would materialize its whole value at the call site (see CLAUDE.md,
   \"e/defn Returns Materialize at the Call Site\"). Server-sited values arrive
   here as positional arguments, which does not force a transfer.

   Atoms arrive as positional parameters, never inside a map: Electric serializes
   a map as data and an atom is not serializable."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.assistant-modes :as modes]
   [freememo.interaction-trail :as trail]
   [freememo.typeahead :refer [Typeahead]]
   #?(:clj [freememo.assistant :as assistant])
   #?(:clj [freememo.settings :as settings])))

(e/defn ChatRow
  "`＋ New` plus the chat picker.

   The picker is a `Typeahead` over `chat-options`, which is id-keyed on the chat
   id — the same atom (`!active`) the send effect and `＋ New` write, so an
   outside write updates the displayed title. Emptying the field and leaving
   clears the selection: that is the widget's documented postcondition, and it
   matches the `Chats…` option the `<select>` carried before.

   `pending-mode` seeds a chat created by `＋ New`, so changing the mode and then
   `＋ New` gives you that persona rather than silently reverting to the default.

   Pre:  `chat-options` is a vec of {:id chat-id :label title}; `pending-mode` is
         a mode :id.
   Post: `!active` holds a chat id or nil, never a title."
  [user-id root-topic-id chat-options pending-mode !active !draft !error]
  (e/client
    (let [!picked (atom nil)
          picked (e/watch !picked)]
      (dom/div
        (dom/props {:class "assistant-panel__bar"})
        (dom/button
          (dom/props {:class "btn btn-secondary assistant-panel__new"
                      :type "button" :title "Start a new chat about this page"})
          (dom/text "＋ New")
          (let [ev (dom/On "click" identity nil)
                [nt _] (e/Token ev)]
            (when nt
              (case (trail/record! :assistant
                      {:action :new-chat :topic-id root-topic-id})
                (let [cid (e/server (e/Offload
                                      #(assistant/start-chat! user-id root-topic-id pending-mode)))]
                  (case cid
                    (do (reset! !active cid) (reset! !draft "") (reset! !error nil) (nt))))))))
        (dom/div
          (dom/props {:class "assistant-panel__chats"})
          (Typeahead !active chat-options "Search chats…" !picked false))
        ;; A different chat is a different transcript, so a stale error must not
        ;; survive the switch. Keyed on the commit atom, not on !active, so it
        ;; fires once per deliberate pick.
        (let [[pt _] (e/Token picked)]
          (when pt
            (case (trail/record! :assistant
                    {:action :select-chat :chat-id picked
                     :topic-id root-topic-id})
              (case (reset! !error nil)
                (pt)))))))))

(e/defn ModeRow
  "Select the mode for the open chat or the next new chat."
  [user-id active mode-entry !pending-mode]
  (e/client
    (dom/label
      (dom/props {:class "assistant-panel__footer-field"})
      (dom/span (dom/text "Mode"))
      (dom/select
        (dom/props {:class "assistant-panel__select"
                    :value (:id mode-entry)
                    :aria-label "Assistant mode"})
        (e/for [mode (e/diff-by :id modes/registry)]
          (dom/option
            (dom/props {:value (:id mode)
                        :selected (= (:id mode) (:id mode-entry))})
            (dom/text (:label mode))))
        (let [change (dom/On "change"
                       (fn [event]
                         (let [mode-id (-> event .-target .-value)]
                           (reset! !pending-mode mode-id)
                           mode-id))
                       nil)
              [token _] (e/Token change)]
          (dom/props {:disabled (some? token) :aria-busy (some? token)})
          (when token
            (if (nil? active)
              (token)
              (let [result (e/server (e/Offload
                                      #(assistant/set-chat-mode!
                                         user-id active change)))]
                (case result
                  (if (:success result)
                    (token)
                    (token (:error result)))))))
          )))))

(e/defn ModelRow
  "Select the model for this document. An empty value uses the global default."
  [user-id root-topic-id]
  (e/client
    (let [current (e/server (settings/get-assistant-model-for user-id root-topic-id))
          default-id (e/server (settings/get-assistant-model user-id))
          choices (e/server (settings/card-model-choices))
          ;; Name the global default that "" resolves to, minus the
          ;; "Provider · " prefix (registry labels are "Google · Gemini 3 Flash").
          default-name (str/trim (last (str/split (get (into {} choices) default-id default-id) #"·")))
          options (into [{:id "" :label (str "Use my default (" default-name ")")}]
                    (map (fn [[v label]] {:id v :label label}))
                    choices)
          !amodel (atom (e/snapshot (or current "")))
          amodel (e/watch !amodel)
          selected (or amodel "")]
      (dom/label
        (dom/props {:class "assistant-panel__footer-field assistant-panel__footer-field--model"})
        (dom/span (dom/text "Model"))
        (dom/select
          (dom/props {:class "assistant-panel__select"
                      :value selected :aria-label "Assistant model"})
          (e/for [option (e/diff-by :id options)]
            (dom/option
              (dom/props {:value (:id option)
                          :selected (= (:id option) selected)})
              (dom/text (:label option))))
          (let [change (dom/On "change"
                         (fn [event]
                           (let [model-id (-> event .-target .-value)]
                             (reset! !amodel model-id)
                             model-id))
                         nil)
                [token _] (e/Token change)]
            (dom/props {:disabled (some? token) :aria-busy (some? token)})
            (when token
              (let [result (e/server (e/Offload
                                      #(settings/save-assistant-model-for
                                         user-id root-topic-id change)))]
                (case result
                  (if (:success result)
                    (token)
                    (token (:error result))))))
            ))))))

(e/defn ReferenceMenu
  "Show document matches above the composer. The message box keeps focus and
   remains visible."
  [options selected-index !picked]
  (e/client
    (dom/div
      (dom/props {:class "assistant-panel__mention-menu" :role "listbox"
                  :aria-label "Reference a document"})
      (if (seq options)
        (e/for [option (e/diff-by :id
                         (mapv (fn [index entry]
                                 (assoc entry :menu-index index))
                           (range) options))]
          (let [selected? (= selected-index (:menu-index option))]
            (dom/button
              (dom/props {:class (str "assistant-panel__mention-option"
                                   (when selected? " assistant-panel__mention-option--selected"))
                          :type "button" :role "option"
                          :aria-selected (str selected?)})
              (dom/text (:label option))
              (dom/On "click" (fn [_] (reset! !picked (:id option))) nil))))
        (dom/div
          (dom/props {:class "assistant-panel__mention-empty"})
          (dom/text "No matching documents."))))))

(e/defn EmptyChatStarters
  "Empty-chat hint plus this persona's four first-message starters.

   Both come from the mode registry, so changing the mode on an empty chat swaps
   the whole block. A tap sends immediately as the chat's first message, which
   creates the chat in the staged mode.

   Pre:  `mode-entry` is a registry entry carrying :hint and :starters.
   Post: writing !submit is the only effect; the send itself belongs to the
         parent's send token."
  [mode-entry !submit !active !refs]
  (e/client
    (dom/div
      (dom/props {:class "assistant-panel__hint"})
      (dom/text (:hint mode-entry))
      (dom/div
        (dom/props {:class "assistant-panel__suggestions"})
        (e/for [p (e/diff-by identity (:starters mode-entry))]
          (dom/button
            (dom/props {:class "assistant-panel__suggestion" :type "button"})
            (dom/text p)
            (let [ev (dom/On "click" identity nil)
                  [st _] (e/Token ev)]
              (when st
                (reset! !submit {:id (str (random-uuid)) :text p
                                 :chat @!active :refs (mapv :id @!refs)
                                 :mode (:id mode-entry)})
                (st)))))))))
