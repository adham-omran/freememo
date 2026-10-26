(ns freememo.card-action-buttons
  "The compact Extract and Add new card pair, for surfaces that have no
   selection popover: phone reading mode, the code pane and a read-only
   reader such as a Google Doc snapshot (plans/toolbar-card-actions-fold.md
   §3.2).

   Both buttons dispatch through the command bus and publish no invoker:
   freememo.card-actions-host owns :extract and :add-new."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.commands :as commands]
   [freememo.command-bus :as bus]
   [freememo.tooltip :as tooltip]))

(e/defn CardActionButtons []
  (e/client
    (dom/div
      (dom/props {:class "toolbar-group"})
      (dom/button
        ;; A ghost button: no toolbar button here is the filled accent.
        (dom/props {:class "btn btn-sm btn-secondary" :aria-label "Extract"})
        (tooltip/Tooltip! (str "Extract selected text as a topic ("
                            (commands/display-chord :extract) ")"))
        (icons/Icon :scissors :size 16)
        (dom/span (dom/props {:class "icon-label icon-label--tier-2"}) (dom/text "Extract"))
        (dom/On "click" (fn [_] (bus/dispatch! :extract)) nil))
      (dom/button
        ;; The chord belongs in the tooltip, not the accessible name.
        (dom/props {:class "btn btn-sm btn-secondary" :aria-label "Add new card"})
        (tooltip/Tooltip! (str "Add new card (" (commands/display-chord :add-new) ")"))
        (icons/Icon :plus :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text "Add new card"))
        (dom/On "click" (fn [_] (bus/dispatch! :add-new)) nil)))))
