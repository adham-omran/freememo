(ns freememo.assistant-panel-rows
  "Chrome rows of the assistant panel: the chat row, the Mode pill row, the model
   row, and the empty-chat starters.

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

   `pending-mode` seeds a chat created by `＋ New`, so tapping a pill and then
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
              (let [cid (e/server (e/Offload
                                    #(assistant/start-chat! user-id root-topic-id pending-mode)))]
                (case cid
                  (do (reset! !active cid) (reset! !draft "") (reset! !error nil) (nt)))))))
        (dom/div
          (dom/props {:class "assistant-panel__chats"})
          (Typeahead !active chat-options "Search chats…" !picked false))
        ;; A different chat is a different transcript, so a stale error must not
        ;; survive the switch. Keyed on the commit atom, not on !active, so it
        ;; fires once per deliberate pick.
        (let [[pt _] (e/Token picked)]
          (when pt
            (reset! !error nil)
            (pt)))))))

(e/defn ModeRow
  "The `Mode` label and one pill per persona.

   With a chat open a pill tap is a real mutation on that chat's row — mode is
   per chat, so the pills show the open chat's persona rather than a standing
   preference. With no chat open there is nothing to mutate yet, so the tap only
   stages `!pending-mode`, which rides the first send (ensure-and-send! →
   start-chat!) or `＋ New`.

   The click atom is created HERE, not passed in: an `e/Token` input that arrives
   through a parameter map re-fires on every unrelated change to that map.

   Pre:  `mode-entry` is a registry entry (from modes/resolve-mode, never nil);
         `active` is a chat id or nil.
   Post: exactly one pill carries the active class."
  [user-id active mode-entry !pending-mode]
  (e/client
    (let [!click (atom nil)
          click (e/watch !click)
          [t ?error] (e/Token click)]
      (dom/div
        (dom/props {:class "assistant-panel__modes"})
        (dom/span
          (dom/props {:class "assistant-panel__modes-label"})
          (dom/text "Mode"))
        (dom/div
          (dom/props {:class "assistant-panel__mode-pills" :role "tablist"})
          (e/for [m (e/diff-by :id modes/registry)]
            (let [mid (:id m)
                  on? (= mid (:id mode-entry))]
              (dom/button
                (dom/props {:class (str "side-panel__tab"
                                    (when on? " side-panel__tab--active"))
                            :role "tab" :aria-selected (str on?) :type "button"
                            :title (str (:label m) " mode")})
                (dom/text (:label m))
                ;; Staging every tap keeps `＋ New` and the first send in step
                ;; with the pills whether or not a chat is open.
                (dom/On "click"
                  (fn [_] (reset! !pending-mode mid) (reset! !click mid))
                  nil)))))
        (when t
          (if (nil? active)
            (t) ; nothing persisted yet — the staged value rides chat creation
            (let [r (e/server (e/Offload #(assistant/set-chat-mode! user-id active click)))]
              (case r
                (if (:success r) (t) (t (:error r)))))))
        (when ?error
          (dom/div
            (dom/props {:class "assistant-panel__error"})
            (dom/text ?error)))))))

(e/defn ModelRow
  "Per-document assistant model picker. `\"\"` = use my global default.

   A `Typeahead` rather than a `<select>` so 12 entries stay searchable. The
   widget writes `!amodel` only on a definitive pick, so the save fires once per
   selection instead of once per keystroke.

   Pre:  root-topic-id is the open document.
   Post: the saved value is `\"\"` or a card-models :id — save-assistant-model-for
         rejects anything else."
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
          !picked (atom nil)
          picked (e/watch !picked)]
      (dom/div
        (dom/props {:class "assistant-panel__model"})
        (Typeahead !amodel options "Search models…" !picked false)
        ;; Typeahead clears !amodel to nil when the field is emptied and blurred,
        ;; but it does not write the commit atom. Mirror that one transition, or
        ;; the field reads as cleared while the old per-document model stays in
        ;; force. "" and nil already mean the same thing to
        ;; settings/effective-assistant-model.
        (when (nil? amodel) (reset! !picked ""))
        (let [[mt _] (e/Token picked)]
          (when mt
            (let [r (e/server (e/Offload
                                #(settings/save-assistant-model-for
                                   user-id root-topic-id picked)))]
              (case r
                (if (:success r) (mt) (mt (:error r)))))))))))

(e/defn EmptyChatStarters
  "Empty-chat hint plus this persona's four first-message starters.

   Both come from the mode registry, so switching pills on an empty chat swaps
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
