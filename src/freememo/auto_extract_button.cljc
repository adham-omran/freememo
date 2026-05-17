(ns freememo.auto-extract-button
  "Auto-extract toolbar button — visual only placeholder for triggering an
   automatic content extraction pass. No click handler, no e/Token, no server
   call yet."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]))

(e/defn AutoExtractButton []
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-secondary"
                  :aria-label "Auto-extract (future feature)"
                  :data-tooltip "Future Feature"
                  :disabled true})
      (icons/Icon :scan-text :size 16)
      (dom/span (dom/props {:class "icon-label"}) (dom/text "Auto-extract")))))
