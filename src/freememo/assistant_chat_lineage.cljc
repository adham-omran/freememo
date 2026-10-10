(ns freememo.assistant-chat-lineage
  "The fork row under the assistant chat bar (plans/assistant-chat-forks.md
   §4.6.2, §4.6.3).

   For a fork it shows the path of ancestors, each a link that opens it, and
   the control that excludes the fork from its parent's answers. For a chat
   with forks it lists them. A chat with neither shows nothing.

   Its own namespace because AssistantPanel sits at the 64KB method limit; see
   freememo.assistant-panel-rows. It reads only the chat list the panel
   already has, so it makes no server read of its own."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.assistant-chat-tree :as tree]))

(e/defn ChatLineage
  "Pre: `chat-options` is the panel's chat list, each with :id, :label,
   :parent-id, :fork-message-id and :excluded?. `open!` takes a chat id and
   opens that chat. `set-excluded!` takes a chat id and a boolean. Both are
   plain client fns.
   Post: writes nothing itself. Every effect goes through the two fns."
  [chat-options active open! set-excluded!]
  (e/client
    (let [by-id (into {} (map (juxt :id identity)) chat-options)
          chat (get by-id active)
          ancestors (tree/lineage by-id active)
          children (get (tree/children-index chat-options) active)
          excluded? (:excluded? chat)]
      (when (and chat (or (seq ancestors) (seq children)))
        (dom/nav
          (dom/props {:class "assistant-lineage" :aria-label "Chat branches"})
          (when (seq ancestors)
            (dom/div
              (dom/props {:class "assistant-lineage__row"})
              ;; Each loop has an element of its own, so its rows are never
              ;; counted together with their siblings (CLAUDE.md, "An e/for
              ;; Gets an Element of Its Own"). `display: contents` keeps them
              ;; in the row's flex layout.
              (dom/span
                (dom/props {:class "assistant-lineage__loop"})
                (e/for-by :id [ancestor ancestors]
                  (dom/span
                    (dom/props {:class "assistant-lineage__step"})
                    (dom/button
                      (dom/props {:type "button" :class "assistant-lineage__link"
                                  :data-tooltip "Open this chat"})
                      (dom/text (:label ancestor))
                      (dom/On "click" (fn [_] (open! (:id ancestor))) nil))
                    (dom/span
                      (dom/props {:class "assistant-lineage__sep" :aria-hidden "true"})
                      (dom/text "›")))))
              (dom/span
                (dom/props {:class (str "assistant-lineage__here"
                                     (when excluded? " assistant-lineage__here--excluded"))})
                (dom/text (:label chat)))
              (dom/button
                (dom/props {:type "button" :class "btn btn-secondary assistant-lineage__exclude"
                            :aria-pressed (str (boolean excluded?))
                            :data-tooltip (if excluded?
                                            "The parent chat does not read this chat"
                                            "The parent chat reads this chat in its answers")})
                (dom/text (if excluded? "Include in parent" "Exclude from parent"))
                (dom/On "click" (fn [_] (set-excluded! active (not excluded?))) nil))))
          (when (seq children)
            (dom/div
              (dom/props {:class "assistant-lineage__row"})
              (dom/span
                (dom/props {:class "assistant-lineage__label"})
                (dom/text "Branches:"))
              (dom/span
                (dom/props {:class "assistant-lineage__loop"})
                (e/for-by :id [child children]
                  (dom/button
                    (dom/props {:type "button"
                                :class (str "assistant-lineage__link"
                                         (when (:excluded? child)
                                           " assistant-lineage__link--excluded"))
                                :data-tooltip (if (:excluded? child)
                                                "Excluded: this chat does not read it"
                                                "Open this branch")})
                    (dom/text (:label child))
                    (dom/On "click" (fn [_] (open! (:id child))) nil)))))))))))
