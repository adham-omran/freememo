(ns freememo.score-rect-modal
  "Score notation-rect snapshot modal: the current PDF page rendered to a
   canvas (score-pdf/snapshot-scale) with a Konva rect editor over it, plus an
   in-modal page switcher so one card's phrase can span systems ACROSS pages.

   Rects accumulate continuously into the ambient dctx/!score-pages atom
   ({page {:width :height :rects [...]}}), so page switches and modal
   close/reopen never lose them; the score toolbar reads the same atom for
   its pending-card state. Zero-rect pages are pruned at submit time
   (score-toolbar), not here — an entry must keep its snapshot dims while the
   user redraws."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.modal-shell :as modal]
   #?(:cljs [freememo.score-pdf :as score-pdf])
   #?(:cljs [freememo.score-rect-editor :as rect-editor])))

;; ---------------------------------------------------------------------------
;; Platform wrappers — reader conditionals live in plain defns, never in the
;; e/defn reactive bodies (CLJ/CLJS frame signal-count rule).
;; ---------------------------------------------------------------------------

(defn page-count* []
  #?(:cljs (or (score-pdf/page-count) 1)
     :clj 1))

(defn load-page-editor!
  "Async: render `page` → mount the Konva rect editor with the page's already
   drawn rects; record the snapshot dims on the page entry. `zoom` scales the
   display only. The host may unmount while the render is in flight — guarded
   via isConnected."
  [!handle container page zoom !score-pages]
  #?(:cljs
     (do (-> (score-pdf/render-page page)
           (.then
             (fn [canvas]
               (when (.-isConnected container)
                 (swap! !score-pages update page
                   (fn [entry]
                     (merge {:rects []} entry
                       {:width (.-width canvas) :height (.-height canvas)})))
                 (reset! !handle
                   (rect-editor/init!
                     {:container container
                      :canvas canvas
                      :zoom zoom
                      :rects (get-in @!score-pages [page :rects])
                      :on-change (fn [rects]
                                   (swap! !score-pages assoc-in [page :rects]
                                     (vec rects)))})))))
           (.catch (fn [e] (js/console.error "[score-rect-modal] page render failed:" e))))
       nil)
     :clj nil))

(defn destroy-page-editor! [!handle]
  #?(:cljs (do (rect-editor/destroy! @!handle)
             (reset! !handle nil)
             nil)
     :clj nil))

;; ---------------------------------------------------------------------------

(e/defn ScoreRectModal
  "Mounts while @dctx/!score-modal-open? is true."
  []
  (e/client
    (when (e/watch dctx/!score-modal-open?)
      (let [!score-pages dctx/!score-pages
            !page (atom (or dctx/current-page 1))
            page (e/watch !page)
            !zoom (atom 1)
            zoom (e/watch !zoom)
            total (page-count*)
            score-pages (e/watch !score-pages)
            page-rects (count (get-in score-pages [page :rects]))
            total-rects (transduce (map (comp count :rects)) + 0 (vals score-pages))
            close! (fn [] (reset! dctx/!score-modal-open? false))]
        (dom/div
          (dom/props {:class "modal-backdrop" :tabindex "-1" :autofocus true})
          (modal/ModalEscape close! "Score regions")
          (dom/On "click" (fn [e]
                            (when (= (.-target e) (.-currentTarget e)) (close!)))
            nil)
          (dom/div
            (dom/props {:class "modal-content"
                        :style {:width "820px" :max-width "95vw"}})
            (dom/h3
              (dom/props {:style {:margin "0 0 12px 0" :font-size "16px"}})
              (dom/text "Select notation"))
            (dom/div
              (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                                  :margin-bottom "var(--sp-2)"}})
              (dom/text "Drag on the page to draw a rectangle around the phrase. A card may combine rectangles from several pages — switch pages below; drawn rectangles are kept."))
            ;; Page switcher
            (dom/div
              (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-2)"
                                  :margin-bottom "var(--sp-2)"}})
              (dom/button
                (dom/props {:class "btn btn-sm btn-secondary" :title "Previous page"
                            :disabled (<= page 1)})
                (dom/text "◀")
                (dom/On "click" (fn [_] (swap! !page #(max 1 (dec %)))) nil))
              (dom/span
                (dom/props {:style {:font-size "13px"}})
                (dom/text (str "Page " page " of " total)))
              (dom/button
                (dom/props {:class "btn btn-sm btn-secondary" :title "Next page"
                            :disabled (>= page total)})
                (dom/text "▶")
                (dom/On "click" (fn [_] (swap! !page #(min total (inc %)))) nil))
              (dom/button
                (dom/props {:class "btn btn-sm btn-secondary" :title "Zoom out"
                            :style {:margin-left "var(--sp-2)"}
                            :disabled (<= zoom 1)})
                (dom/text "−")
                (dom/On "click" (fn [_] (swap! !zoom #(max 1 (/ % 1.5)))) nil))
              (dom/button
                (dom/props {:class "btn btn-sm btn-secondary" :title "Zoom in"
                            :disabled (>= zoom 6)})
                (dom/text "+")
                (dom/On "click" (fn [_] (swap! !zoom #(min 6 (* % 1.5)))) nil))
              (dom/span
                (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                                    :margin-left "auto"}})
                (dom/text (str page-rects " on this page · " total-rects " total"))))
            ;; Editor host — keyed by page × zoom so page switches AND zoom
            ;; changes remount the Konva stage; drawn rects live in
            ;; !score-pages, so remounts never lose them.
            (e/for-by identity [[p z] [[page zoom]]]
              (dom/div
                (dom/props {:class "score-rect-host"
                            :style {:min-height "200px" :max-height "58vh"
                                    :overflow "auto" :background "var(--color-bg-subtle)"
                                    :border "1px solid var(--color-border)"
                                    :border-radius "var(--radius-sm)"}})
                (let [host dom/node
                      !handle (atom nil)]
                  (load-page-editor! !handle host p z !score-pages)
                  (e/on-unmount (fn [] (destroy-page-editor! !handle))))))
            ;; Footer
            (dom/div
              (dom/props {:style {:display "flex" :justify-content "flex-end"
                                  :margin-top "var(--sp-3)"}})
              (dom/button
                (dom/props {:class "btn btn-primary" :style {:font-weight "600"}})
                (dom/text "Done")
                (dom/On "click" (fn [_] (close!)) nil)))))))))
