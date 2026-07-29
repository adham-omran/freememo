(ns freememo.add-card-button
  "Add-new-card toolbar button + modal trigger. Extracted from
   content_toolbar_actions so each e/defn stays under the JVM 64KB limit."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.rich-text-editor :as editor]
   [freememo.card-modals :refer [AddCardModal]]
   [freememo.command-bus :as bus]
   [freememo.tooltip :as tooltip]))

(e/defn AddCardButton [user-id topic-id root-topic-id card-type mod-key]
  (e/client
    (let [!show-add (atom false)
          show-add (e/watch !show-add)
          !card-kind (atom card-type)
          !captured-selection (atom "")]
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-item"
                    :style {:font-weight "500"}
                    ;; The chord belongs in the tooltip, not the accessible
                    ;; name — so :aria-label is set here rather than via
                    ;; Tooltip!'s :aria? (which would mirror the chord text).
                    :aria-label "Add new card"})
        (tooltip/Tooltip! (str "Add new card (" mod-key "+Shift+A)"))
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
            ;; a whitespace-only range leaves neither.
            (let [{:keys [html text index length]} (editor/get-selection-html!)
                  taken? (not (str/blank? text))]
              (when taken?
                (editor/highlight-range! index length
                  :color "var(--color-highlight-gold)"))
              (reset! !captured-selection (if taken? html "")))
            (reset! !show-add true))
          nil))
      (when show-add
        (AddCardModal !show-add !card-kind !captured-selection topic-id root-topic-id user-id)))))
