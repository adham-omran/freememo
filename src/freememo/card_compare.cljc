(ns freememo.card-compare
  "Compare card models: pick a subset of card-generation models, generate the
   same content with each, show the candidate card-sets side-by-side, and keep
   one set. Mirrors freememo.ocr-compare (the OCR-model compare), but for card
   generation. Candidates are held client-side and nothing is persisted until
   the user clicks \"Use this\" — each model run is a real billed generation, so
   comparing N models spends credits N times."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.doc-context :as dctx]
   [freememo.rich-text-editor :as editor]
   [freememo.card-models :as card-models]
   [freememo.command-bus :as bus]
   [freememo.loading :as loading]
   [freememo.icons :as icons]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.cards :as cards])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.commands :as commands])
   #?(:clj [freememo.content-toolbar-helpers :as helpers])))

(defn run-card-compare!*
  "Build content/context (same as the Generate path) and fan out one unsaved
   generation per model. Server-only. Returns a vector of
   {:model-id id :result {:success ..}}; nothing is persisted."
  [ctx model-ids]
  #?(:clj (let [{:keys [content context]} (helpers/build-gen-context* ctx)
                opts (-> (select-keys ctx [:card-type :card-count :user-id :enc-key
                                           :topic-id :root-topic-id :pre-prompt])
                       (assoc :content content :context context))]
            (cards/compare-card-gen opts model-ids))
     :cljs nil))

(defn commit-and-maybe-default!*
  "Save the chosen model's card set; when `set-default?`, also persist `model-id`
   as the user's card-generation default. Bumps :card-mutations (via :generate)
   and :set-setting. Server-only. Returns the save result."
  [user-id topic-id root-topic-id card-type cards model-id set-default?]
  #?(:clj (let [r (cards/commit-card-set! user-id topic-id root-topic-id card-type cards)]
            (when (and (:success r) set-default?)
              (settings/save-model user-id model-id)
              (commands/bump! user-id :set-setting))
            r)
     :cljs nil))

(defn start-drag!
  "Begin dragging the floating modal: follow the pointer and update `!pos` until
   mouseup. cljs-only (no-op on clj). Mirrors ocr-compare/start-drag!."
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

(e/defn CardHtml
  "Render one card field's HTML. `dir=\"auto\"` flips to RTL for Arabic content."
  [html]
  (e/client
    (dom/div
      (dom/props {:dir "auto" :style {:font-size "13px" :line-height "1.4" :text-align "start"}})
      (set! (.-innerHTML dom/node) (or html "")))))

(e/defn CardResultColumn
  "One model's labelled candidate set: header (label + card count), then each
   generated card (front/back or cloze/back), or the error on failure."
  [model-id result]
  (e/client
    (let [label (or (:label (card-models/resolve-model model-id)) model-id)
          ok? (:success result)
          cost-credits (:cost-credits result)]
      (dom/div
        (dom/props {:style {:flex "1" :min-width "240px" :display "flex" :flex-direction "column"
                            :border "1px solid var(--color-border)" :border-radius "var(--radius-md)"
                            :overflow "hidden"}})
        (dom/div
          (dom/props {:style {:padding "6px 10px" :font-size "12px" :font-weight "600"
                              :background "var(--color-bg-subtle)"
                              :border-bottom "1px solid var(--color-border)"
                              :display "flex" :justify-content "space-between" :gap "8px"}})
          (dom/span (dom/text label))
          (when ok?
            (dom/span
              (dom/props {:style {:font-weight "400" :color "var(--color-text-hint)"
                                  :display "flex" :gap "8px"}})
              (dom/span (dom/text (str (count (:cards result)) " cards")))
              ;; Credits omitted when nil — credits disabled (self-host) or a
              ;; debit failure; the figure is retry-inclusive (see tooltip).
              (when cost-credits
                (dom/span (tooltip/Tooltip! "Includes retries")
                  (dom/text (str cost-credits " credits")))))))
        (dom/div
          (dom/props {:style {:padding "8px" :overflow "auto" :max-height "55vh"
                              :display "flex" :flex-direction "column" :gap "8px"}})
          (if ok?
            (e/for [card (e/diff-by :i (map-indexed (fn [i c] (assoc c :i i)) (:cards result)))]
              (dom/div
                (dom/props {:style {:border "1px solid var(--color-bg-hover)" :border-radius "var(--radius-sm)"
                                    :padding "6px 8px" :display "flex" :flex-direction "column" :gap "4px"}})
                (CardHtml (or (:q card) (:c card)))
                (when (:a card)
                  (dom/div (dom/props {:style {:border-top "1px dashed var(--color-bg-hover)" :padding-top "4px"
                                               :color "var(--color-text-secondary)"}})
                    (CardHtml (:a card))))))
            (dom/span (dom/props {:style {:color "var(--color-text-hint)"}})
              (dom/text (or (:error result) "Failed")))))))))

(e/defn UseCardsButton
  "Persist this model's set for the topic; honors the shared set-default
   checkbox. Closes the modal on success."
  [ctx model-id result !set-default !open]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-primary" :disabled (not (:success result))
                  :style {:width "100%"}})
      (dom/text "Use this")
      (let [click (dom/On "click" (fn [_] @!set-default) nil)
            [t _] (e/Token click)]
        (when t
          (e/on-unmount #(reset! !open nil))
          (case (e/server (e/Offload #(commit-and-maybe-default!*
                                        (:user-id ctx) (:topic-id ctx) (:root-topic-id ctx)
                                        (:card-type ctx) (:cards result) model-id click)))
            (t)))))))

(e/defn CardCompareModal
  "Floating, draggable, non-blocking compare panel. Phase 1: pick ≥2 card models.
   Phase 2: each selected model generates the same content (billed), results show
   side-by-side, and \"Use this\" keeps one set. No backdrop — the editor stays
   visible."
  [ctx model-ids !open]
  (e/client
    (let [!selected (atom #{}) selected (e/watch !selected)
          !run (atom nil) run (e/watch !run)
          !set-default (atom false)
          !done (atom false) done (e/watch !done)
          !pos (atom {:x 60 :y 60}) pos (e/watch !pos)]
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
            (dom/text "Compare card models"))
          (dom/span (dom/props {:style {:font-size "11px" :color "var(--color-text-hint)"}})
            (dom/text "drag to move")))

        (if (nil? run)
          ;; Phase 1 — model subset selection (≥2 required)
          (dom/div
            (dom/props {:style {:display "flex" :flex-direction "column" :gap "10px"}})
            (dom/div (dom/props {:class "hint"})
              (dom/text "Pick 2 or more models. Each runs a real generation and is billed."))
            (dom/div
              (dom/props {:style {:display "flex" :flex-direction "column" :gap "6px"}})
              (e/for [mid (e/diff-by identity model-ids)]
                (dom/label
                  (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                                      :font-size "13px" :cursor "pointer"}})
                  (dom/input
                    (dom/props {:type "checkbox"})
                    (dom/On "change"
                      (fn [e] (if (-> e .-target .-checked)
                                (swap! !selected conj mid)
                                (swap! !selected disj mid)))
                      nil))
                  (dom/text (or (:label (card-models/resolve-model mid)) mid)))))
            (dom/div
              (dom/props {:style {:display "flex" :justify-content "flex-end"}})
              (dom/button
                (dom/props {:class "btn btn-sm btn-primary" :disabled (< (count selected) 2)})
                (dom/text (str "Compare (" (count selected) ")"))
                (dom/On "click" (fn [_] (reset! !run {:models (vec selected)})) nil))))

          ;; Phase 2 — run every selected model and show candidate sets
          (dom/div
            (dom/props {:style {:display "flex" :flex-direction "column" :gap "10px"}})
            (dom/div
              (dom/props {:style {:display "flex" :gap "12px" :align-items "stretch" :overflow-x "auto"}})
              (loading/WithLoading
                (e/fn [] (e/server (e/Offload #(run-card-compare!* ctx (:models run)))))
                (e/fn [results]
                  ;; Results resolved → enable the set-default checkbox. `case`
                  ;; forces the reactive write (WithLatestLoading idiom).
                  (case results (reset! !done true))
                  (e/for [{:keys [model-id result]} (e/diff-by :model-id results)]
                    (dom/div
                      (dom/props {:style {:flex "1" :min-width "260px" :display "flex"
                                          :flex-direction "column" :gap "8px"}})
                      (e/amb
                        (CardResultColumn model-id result)
                        (UseCardsButton ctx model-id result !set-default !open)))))
                (e/fn [] (loading/Spinner "Generating with each model…"))))
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "6px" :font-size "13px"}})
              (dom/input
                (dom/props {:type "checkbox" :disabled (not done)})
                (dom/On "change" (fn [e] (reset! !set-default (-> e .-target .-checked))) nil))
              (dom/text "Set the picked model as my card-gen default"))))

        (dom/div
          (dom/props {:style {:display "flex" :justify-content "flex-end"}})
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"})
            (dom/text "Close")
            (dom/On "click" (fn [_] (reset! !open nil)) nil)))))))

(e/defn CardCompareButton
  "Toolbar button that opens the card-model compare modal for the current
   content/selection. Reads doc-context like ToolbarGenerate; captures the
   editor selection at click time so the compared content matches Generate."
  []
  (e/client
    (let [user-id dctx/user-id enc-key dctx/enc-key topic-id dctx/topic-id
          root-topic-id dctx/root-topic-id page-number dctx/page-number
          content-text dctx/content-text context-mode dctx/context-mode
          use-context dctx/use-context context-window dctx/context-window
          card-type dctx/card-type card-count-val dctx/card-count-val
          !open (atom nil) open (e/watch !open)
          model-ids (e/server (settings/card-model-ids))]
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary"
                    :aria-label "Compare card models"})
        (tooltip/Tooltip! "Generate with several models and compare (uses credits per model)")
        (icons/Icon :sparkles :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text "Compare models"))
        (let [node dom/node]
          (bus/publish-invoker! :compare-card-gen (fn [] (.click node)))
          (e/on-unmount (fn [] (bus/retract-invoker! :compare-card-gen))))
        (dom/On "click"
          (fn [_]
            (let [sel (editor/get-selection-html!)]
              (when sel
                (editor/highlight-range! (:index sel) (:length sel) :color "var(--color-highlight-gold)"))
              (reset! !open {:selection-html (when (and sel (not (str/blank? (:text sel))))
                                               (:html sel))})))
          nil))
      (when open
        (CardCompareModal
          {:selection-html (:selection-html open)
           :content-text content-text :context-mode context-mode :use-context use-context
           :topic-id topic-id :root-topic-id root-topic-id :page-number page-number
           :context-window context-window :card-type card-type :card-count card-count-val
           :user-id user-id :enc-key enc-key :pre-prompt nil}
          model-ids !open)))))
