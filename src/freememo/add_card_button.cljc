(ns freememo.add-card-button
  "Add-new-card toolbar button + modal trigger. Extracted from
   content_toolbar_actions so each e/defn stays under the JVM 64KB limit."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.selected-passage :as selected-passage]
   [freememo.card-modals :refer [AddCardModal]]
   [freememo.command-bus :as bus]
   [freememo.commands :as commands]
   [freememo.tooltip :as tooltip]))

(e/defn AddCardButton [user-id topic-id root-topic-id card-type]
  (e/client
    (let [!show-add (atom false)
          show-add (e/watch !show-add)
          !card-kind (atom card-type)
          ;; AddCardModal's prefill map. Only :primary is ever set here — a
          ;; captured selection seeds the front field, and there is nothing to
          ;; put in the back. Create copy… is what fills the rest.
          !prefill (atom {})]
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-item"
                    :style {:font-weight "500"}
                    ;; The chord belongs in the tooltip, not the accessible
                    ;; name — so :aria-label is set here rather than via
                    ;; Tooltip!'s :aria? (which would mirror the chord text).
                    :aria-label "Add new card"})
        (tooltip/Tooltip! (str "Add new card (" (commands/display-chord :add-new) ")"))
        (icons/Icon :plus :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text "Add new card"))
        (let [node dom/node]
          (bus/publish-invoker! :add-new (fn [] (.click node)))
          (e/on-unmount (fn [] (bus/retract-invoker! :add-new))))
        (dom/On "click"
          (fn [_]
            ;; Gold is Generate's mark: "a card was made from this passage".
            ;; Gated on the SAME condition as the capture below, so the
            ;; highlight is present iff the selection reached the modal —
            ;; a whitespace-only range leaves neither. A code-pane selection
            ;; is taken first and marks nothing (freememo.selected-passage).
            (let [passage (selected-passage/current)
                  taken? (not (str/blank? (:text passage)))]
              (when taken?
                (selected-passage/mark! passage :color "var(--color-highlight-gold)"))
              (reset! !prefill {:primary (if taken? (:html passage) "")}))
            (reset! !show-add true))
          nil))
      (when show-add
        (AddCardModal (fn [] (reset! !show-add false))
          !card-kind !prefill topic-id root-topic-id user-id nil)))))
