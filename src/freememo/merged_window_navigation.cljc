(ns freememo.merged-window-navigation
  "Navigation between bounded Focus reader page ranges."
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [freememo.doc-context :as dctx]))

(e/defn MergedWindowNavigation []
  (e/client
    (when dctx/merged?
      (let [[start end] dctx/merged-range
            total dctx/total
            on-page-change! dctx/on-page-change!]
        (when (and start end (> total (- (inc end) start)))
          (dom/div
            (dom/props {:class "toolbar-group focus-reader-pages"
                        :aria-label "Focus reader page range"})
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary"
                          :disabled (<= start 1) :aria-label "Previous page range"})
              (dom/text "Previous pages")
              (dom/On "click" (fn [_] (on-page-change! (max 1 (dec start)))) nil))
            (dom/span
              (dom/props {:aria-live "polite"})
              (dom/text (str "Pages " start "–" end " of " total)))
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary"
                          :disabled (>= end total) :aria-label "Next page range"})
              (dom/text "Next pages")
              (dom/On "click" (fn [_] (on-page-change! (min total (inc end)))) nil))))))))
