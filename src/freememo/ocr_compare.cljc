(ns freememo.ocr-compare
  "3-way OCR compare: run every allowed OCR model on the current page, show the
   results side-by-side, and let the user adopt one (optionally remembering it as
   the per-document model). Distinct from copy-text's A/B compare, which compares
   text-extraction engines, not OCR models. Each candidate is a real billed OCR
   call, so opening the modal spends credits per model."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.commands :as commands]
   [freememo.command-bus :as bus]
   [freememo.loading :as loading]
   [freememo.ocr-models :as ocr-models]
   #?(:clj [freememo.ocr :as ocr])
   #?(:clj [freememo.page-ocr :as page])
   #?(:clj [freememo.settings :as settings])))

(defn ocr-preview!*
  "Run one model's OCR on a page WITHOUT saving (bills the call). Server-only."
  [user-id root model-id page dpi]
  #?(:clj (page/ocr-preview-with-model user-id root model-id page dpi) :cljs nil))

(defn commit-and-remember!*
  "Save the chosen OCR text to the page; when `remember?`, also persist `model-id`
   as the per-document OCR model. Bumps :refresh (and :settings-refresh when
   remembering). Returns the save result. Server-only.
   Pre: html is sanitized OCR output. Post: page content updated; per-doc model
   set iff remember? and the save succeeded."
  [user-id root page html model-id remember?]
  #?(:clj (let [r (page/save-page-html-impl root page html)]
            (when (:success r)
              (commands/bump! user-id :compare-ocr)
              (when remember?
                (settings/save-ocr-model user-id root model-id)
                (commands/bump! user-id :set-setting)))
            r)
     :cljs nil))

(defn start-drag!
  "Begin dragging the floating modal: follow the pointer on document and update
   `!pos` until mouseup. cljs-only (no-op on clj)."
  [!pos e]
  #?(:cljs
     (let [sx (.-clientX e) sy (.-clientY e)
           {ox :x oy :y} @!pos
           mv (fn [ev] (reset! !pos {:x (+ ox (- (.-clientX ev) sx))
                                     :y (+ oy (- (.-clientY ev) sy))}))
           !up (atom nil)]
       (reset! !up (fn [_]
                     (.removeEventListener js/document "mousemove" mv)
                     (.removeEventListener js/document "mouseup" @!up)))
       (.addEventListener js/document "mousemove" mv)
       (.addEventListener js/document "mouseup" @!up)
       (.preventDefault e))
     :clj nil))

(e/defn OcrResultColumn
  "One model's labelled result panel. `result` is the preview map or nil (running).
   `dir=\"auto\"` lets the browser flip to RTL for Arabic OCR output."
  [model-id result]
  (e/client
    (let [label (or (:label (ocr-models/resolve-model model-id)) model-id)]
      (dom/div
        (dom/props {:style {:flex "1" :min-width "240px" :display "flex" :flex-direction "column"
                            :border "1px solid var(--color-border)" :border-radius "var(--radius-md)"
                            :overflow "hidden"}})
        (dom/div
          (dom/props {:style {:padding "6px 10px" :font-size "12px" :font-weight "600"
                              :background "var(--color-bg-subtle)"
                              :border-bottom "1px solid var(--color-border)"}})
          (dom/text label))
        (dom/div
          (dom/props {:dir "auto"
                      :style {:padding "10px" :overflow "auto" :max-height "55vh" :font-size "13px"
                              :text-align "start"}})
          (cond
            (nil? result)
            (dom/div
              (dom/props {:style {:display "flex" :align-items "center" :justify-content "center"
                                  :gap "8px" :padding "28px 8px" :color "var(--color-text-secondary)"}})
              (dom/span (dom/props {:class "spinner"}))
              (dom/text "Running…"))
            (:success result) (set! (.-innerHTML dom/node) (:text result))
            :else (dom/span (dom/props {:style {:color "var(--color-text-hint)"}})
                    (dom/text (or (:error result) "Failed")))))))))

(e/defn UseOcrButton
  "Adopt this model's text for the page; honors the shared Remember checkbox.
   Closes the modal on success."
  [user-id root page model-id result !remember !open]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-primary" :disabled (not (:success result))
                  :style {:width "100%"}})
      (dom/text "Use this")
      (let [click (dom/On "click" (fn [_] @!remember) nil)
            [t _] (e/Token click)]
        (when t
          (e/on-unmount #(reset! !open nil))
          (case (e/server (e/Offload #(commit-and-remember!* user-id root page (:text result) model-id click)))
            (t)))))))

(e/defn OcrCompareModal
  "Floating, draggable, non-blocking compare panel: runs every model in
   `model-ids` on `page` (each a billed OCR call), shows results side-by-side,
   and adopts the picked one. No backdrop — the page stays visible and
   interactive so the user can compare against it; drag the header to move,
   resize from the corner."
  [user-id root-topic-id page model-ids dpi !open]
  (e/client
    (let [!remember (atom false)
          !pos (atom {:x 60 :y 60})
          pos (e/watch !pos)]
      (dom/div
        (dom/props {:class "modal-content"
                    :style {:position "fixed" :left (str (:x pos) "px") :top (str (:y pos) "px")
                            :width "min(1100px, 96vw)" :max-height "85vh" :z-index 1000
                            :display "flex" :flex-direction "column" :gap "10px"
                            :padding "12px 16px" :resize "both" :overflow "auto"}})
        ;; Draggable header
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :justify-content "space-between"
                              :cursor "move" :user-select "none" :gap "12px"
                              :padding-bottom "8px" :border-bottom "1px solid var(--color-border)"}})
          (dom/On "mousedown" (fn [e] (start-drag! !pos e)) nil)
          (dom/h3 (dom/props {:style {:margin "0" :font-size "15px" :font-weight "500"}})
            (dom/text (str "Compare OCR — page " page)))
          (dom/span (dom/props {:style {:font-size "11px" :color "var(--color-text-hint)"}})
            (dom/text "drag to move")))
        (dom/div
          (dom/props {:style {:display "flex" :gap "12px" :align-items "stretch" :overflow-x "auto"}})
          (e/for [mid (e/diff-by identity model-ids)]
            (dom/div
              (dom/props {:style {:flex "1" :min-width "260px" :display "flex" :flex-direction "column" :gap "8px"}})
              (loading/WithLoading
                (e/fn [] (e/server (e/Offload #(ocr-preview!* user-id root-topic-id mid page dpi))))
                (e/fn [res]
                  (e/amb
                    (OcrResultColumn mid res)
                    (UseOcrButton user-id root-topic-id page mid res !remember !open)))
                (e/fn [] (OcrResultColumn mid nil))))))
        (dom/label
          (dom/props {:style {:display "flex" :align-items "center" :gap "6px" :font-size "13px"}})
          (dom/input
            (dom/props {:type "checkbox"})
            (dom/On "change" (fn [e] (reset! !remember (-> e .-target .-checked))) nil))
          (dom/text "Remember the picked model for this PDF"))
        (dom/div
          (dom/props {:style {:display "flex" :justify-content "flex-end"}})
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"})
            (dom/text "Close")
            (dom/On "click" (fn [_] (reset! !open nil)) nil)))))))

(e/defn OcrCompareButton
  "Toolbar button that opens the OCR compare modal for the current page."
  [user-id root-topic-id page-number dpi]
  (e/client
    (let [!open (atom nil)
          open (e/watch !open)
          model-ids (e/server (ocr/allowed-ocr-model-ids))]
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary"
                    :aria-label "Compare OCR models"
                    :data-tooltip "Run every OCR model on this page and compare (uses credits)"})
        (icons/Icon :scan-text :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text "Compare OCR"))
        (let [node dom/node]
          (bus/publish-invoker! :compare-ocr (fn [] (.click node)))
          (e/on-unmount (fn [] (bus/retract-invoker! :compare-ocr))))
        (dom/On "click" (fn [_] (reset! !open true)) nil))
      (when open
        (OcrCompareModal user-id root-topic-id page-number model-ids dpi !open)))))
