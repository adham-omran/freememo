(ns freememo.merged-window-navigation
  "Loaded-range status for continuous Focus reading."
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [freememo.doc-context :as dctx]
            [freememo.merged-window :as window]))

(e/defn MergedWindowNavigation []
  (e/client
    (when dctx/merged?
      (let [blocked (e/watch window/!blocked-range)
            blocked (when (= dctx/merged-root-id (:root-id blocked)) blocked)
            [start end] (or (:range blocked) dctx/merged-range)]
        (dom/span
          (dom/props {:class "toolbar-group focus-reader-pages"
                      :role "status" :aria-live "polite"})
          (dom/text (str "Pages " start "–" end " of " dctx/total
                        ". Scroll to load adjacent pages. Undo covers loaded pages."))
          (when blocked
            (dom/span
              (dom/props {:class "focus-reader-range-note"})
              (dom/text "Use the page field to load earlier pages after editing above a page marker."))))))))
