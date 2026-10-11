(ns freememo.assistant-slash-menu
  "The composer's menu of slash commands, which `/` at the start of the draft
   opens (plans/assistant-context-meter-and-slash-menu.md §2). The commands
   come from `freememo.assistant-slash/commands`, so nothing here names one."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.assistant-slash :as slash]))

(defn menu-state
  "What the menu shows for `draft`.
   Pre:  `dismissed` is the draft Escape closed the menu on, or nil once the
         draft has changed since (the composer's input handler clears it). `index`
         is the selection counter, any integer. `at-open?` is true while the
         `@` menu is open.
   Post: {:open? :matches :selected}. :open? is false while the `@` menu is
         open, so the two menus never show together. :selected is an index
         into :matches, or nil when there is none."
  [draft dismissed index at-open?]
  (let [matches (slash/suggestions draft)
        open? (boolean (and (seq matches) (not= draft dismissed) (not at-open?)))]
    {:open? open?
     :matches (or matches [])
     :selected (when (seq matches) (mod index (count matches)))}))

(defn enter-picks?
  "True when Enter on `draft` must write the selected command into the draft
   rather than send. Enter sends once the draft already is the command's
   completion, so `/compact` and Enter sends at once."
  [draft command]
  (not= draft (:completion command)))

(e/defn SlashMenu
  "Show the commands that match the draft, above the composer.
   Pre:  `pick!` is a plain fn of one command that writes its completion into
         the draft.
   Invariant: the list is always mounted and only hidden, in an element of its
   own, for the reason `ReferenceMenu` records."
  [{:keys [open? matches selected]} pick!]
  (e/client
    (dom/ul
      (dom/props {:class (str "assistant-panel__mention-menu assistant-panel__slash-menu"
                           (when-not open? " assistant-panel__mention-menu--closed"))
                  :role "listbox" :aria-label "Slash commands"
                  :aria-hidden (str (not open?))})
      (e/for [[i command] (e/diff-by (comp :text second) (map-indexed vector matches))]
        (let [selected? (= selected i)]
          (dom/li
            (dom/button
              (dom/props {:class (str "assistant-panel__mention-option assistant-panel__slash-option"
                                   (when selected? " assistant-panel__mention-option--selected"))
                          :type "button" :role "option"
                          :aria-selected (str selected?)})
              (dom/span
                (dom/props {:class "assistant-panel__slash-usage"})
                (dom/text (:usage command)))
              (dom/span
                (dom/props {:class "assistant-panel__slash-description"})
                (dom/text (:description command)))
              ;; mousedown, not click, and no default: the textarea keeps focus
              ;; and the caret, as in ReferenceMenu.
              (dom/On "mousedown"
                (fn [event]
                  (.preventDefault event)
                  (pick! command))
                nil))))))))
