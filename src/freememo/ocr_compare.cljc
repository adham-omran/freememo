(ns freememo.ocr-compare
  "OCR compare: pick which OCR models to run on the current page, show their
   results side-by-side, and let the user adopt one (optionally remembering it as
   the per-document model). Distinct from copy-text's A/B compare, which compares
   text-extraction engines, not OCR models. Each candidate is a real billed OCR
   call, so a run spends credits per SELECTED model — opening the modal spends
   nothing, because the picker gates the fan-out."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.commands :as commands]
   [freememo.command-bus :as bus]
   [freememo.loading :as loading]
   [freememo.math :as math]
   [freememo.ocr-models :as ocr-models]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.ocr :as ocr])
   #?(:clj [freememo.page-ocr :as page])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.user-state :as us])))

(defn compare-ocr-page!*
  "OCR the page with each model the user selected in the compare modal, WITHOUT
   saving (bills every call). The page is rendered once server-side and shared
   across models (C); the fan-out is concurrency-bounded (B). Server-only. Returns
   a vector of {:model-id id :result {:success ..}} in `model-ids` order, so the
   caller controls column order; the heavy render never reaches the client."
  [user-id root model-ids page dpi]
  #?(:clj (page/compare-ocr-page user-id root model-ids page dpi) :cljs nil))

(defn effective-ocr-model-id*
  "The OCR model :id a normal Scan Page on `root` would use right now — the id the
   compare picker pre-selects. `_settings-refresh` exists only to create the
   reactive dependency: adopting a model with Remember bumps :set-setting (see
   commit-and-remember!*), so without it the next open would pre-select the model
   the user just replaced. Server-only.
   Post: an :id present in (ocr/allowed-ocr-model-ids) — effective-ocr-model
   clamps every precedence branch to that list, so callers need no membership
   guard against the picker's own options."
  [_settings-refresh user-id root]
  #?(:clj (:id (ocr/effective-ocr-model user-id root))
     :cljs nil))

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
            (:success result) (math/set-html! dom/node (:text result))
            :else (dom/span (dom/props {:style {:color "var(--color-text-hint)"}})
                    (dom/text (or (:error result) "Failed")))))))))

(e/defn UseOcrButton
  "Adopt this model's text for the page; honors the shared Remember checkbox.
   Closes the modal on success; on failure surfaces the error and leaves the
   modal open so the user isn't left wondering whether the save happened."
  [user-id root page model-id result !remember !open]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-primary" :disabled (not (:success result))
                  :style {:width "100%"}})
      (dom/text "Use this")
      (let [click (dom/On "click" (fn [_] @!remember) nil)
            [t ?error] (e/Token click)]
        (when ?error
          (dom/div (dom/props {:style {:color "var(--color-danger-text)" :font-size "12px" :margin-top "4px"}})
            (dom/text "Error: " ?error)))
        (when t
          (let [r (e/server (e/Offload #(commit-and-remember!* user-id root page (:text result) model-id click)))]
            (if (:success r)
              (do (reset! !open nil) (t))
              (t (:error r)))))))))

(e/defn OcrCompareModal
  "Floating, draggable, non-blocking compare panel. Phase 1 picks which of
   `model-ids` to run, with `effective-model-id` checked to start. Phase 2 runs
   only those (each a billed OCR call), shows the results side-by-side, and adopts
   the picked one. No backdrop — the page stays visible and interactive so the user
   can compare against it; drag the header to move, resize from the corner.

   ONE model is a legal selection, and such a run is NOT a second Scan Page:
   compare never writes, so the text reaches the page only through \"Use this\"
   (commit-and-remember!*).

   \"Change models\" returns to phase 1 with the ticks intact. The selection outlives
   the phase, the run's results do not — a paid run is lost by going back, by Close,
   and by any frame remount (WS reconnect, hot reload), because nothing here is
   persisted outside the frame by design.

   Pre:  `effective-model-id` ∈ `model-ids` (effective-ocr-model-id*'s Post).
   Post: no OCR call is issued until the phase-1 button is clicked."
  [user-id root-topic-id page model-ids effective-model-id dpi !open]
  (e/client
    ;; initial-model-id is snapshotted and then used for BOTH the seed and the
    ;; " (current)" label, so the two cannot disagree. No nil guard: the Post of
    ;; effective-ocr-model-id* is a real id, and a broken Post should surface as a
    ;; failed column naming the model, not as a silently empty picker.
    (let [initial-model-id (e/snapshot effective-model-id)
          !selected (atom #{initial-model-id})
          selected (e/watch !selected)
          !run (atom nil) run (e/watch !run)
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
        (if (nil? run)
          ;; Phase 1 — which models to run. Nothing is billed here; the modal used
          ;; to fan out over every allowed model on open, which spent credits nine
          ;; times for a page the user may want two opinions on.
          ;; ONE ordered vector feeds the disable guard, the count in the label and
          ;; the run itself, so the button cannot promise a number the run does not
          ;; honour. Ordered by model-ids — the order the checkboxes render in — so
          ;; the columns match what the user ticked. NOT ocr-models/registry order:
          ;; in credits mode allowed-ocr-model-ids returns the config allow-list,
          ;; whose order differs from the catalog's.
          (let [models-to-run (filterv selected model-ids)]
            (dom/div
              (dom/props {:style {:display "flex" :flex-direction "column" :gap "10px"}})
              (dom/div (dom/props {:class "hint"})
                (dom/text "Pick one or more models. Each runs a real OCR call and is billed."))
              (dom/div
                (dom/props {:style {:display "flex" :flex-direction "column" :gap "6px"}})
                ;; Deliberately not Forms5: this list holds no token, makes no
                ;; e/server call and mutates nothing — it is local state gating a
                ;; later call, outside the "any input that goes to the server" rule.
                ;; A variable-length set also fits Checkbox!'s fixed field map badly.
                (e/for [mid (e/server (e/diff-by identity model-ids))]
                  (dom/label
                    (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                                        :font-size "13px" :cursor "pointer"}})
                    (dom/input
                      (dom/props {:type "checkbox" :checked (contains? selected mid)})
                      (dom/On "change"
                        (fn [e] (if (-> e .-target .-checked)
                                  (swap! !selected conj mid)
                                  (swap! !selected disj mid)))
                        nil))
                    ;; " (current)" says WHY a box starts checked, so it compares
                    ;; against the SNAPSHOT. Reading the live effective-model-id
                    ;; would let the suffix move to a model whose box did not.
                    (dom/text (or (:label (ocr-models/resolve-model mid)) mid)
                      (if (= mid initial-model-id) " (current)" "")))))
              (dom/div
                (dom/props {:style {:display "flex" :justify-content "flex-end"}})
                (dom/button
                  (dom/props {:class "btn btn-sm btn-primary" :disabled (empty? models-to-run)})
                  ;; The count is the only place the user sees how many billed calls
                  ;; the click buys, so it stays in the label at every size.
                  (dom/text (if (= 1 (count models-to-run))
                              "Run (1)"
                              (str "Compare (" (count models-to-run) ")")))
                  (dom/On "click" (fn [_] (reset! !run {:models models-to-run})) nil)))))

          ;; Phase 2 — run the selected models and show their results. !remember is
          ;; scoped here because only this branch reads it, so going back to the
          ;; picker starts the next run with the choice unmade.
          (let [!remember (atom false)]
            (dom/div
              (dom/props {:style {:display "flex" :flex-direction "column" :gap "10px"}})
              (dom/div
                (dom/props {:style {:display "flex" :gap "12px" :align-items "stretch" :overflow-x "auto"}})
                ;; C: render the page once server-side and OCR each selected model
                ;; from it (the render payload stays on the server); columns appear
                ;; when the batch resolves. Fan-out is concurrency-bounded
                ;; server-side (B).
                (loading/WithLoading
                  (e/fn [] (e/server (e/Offload #(compare-ocr-page!* user-id root-topic-id (:models run) page dpi))))
                  (e/fn [results]
                    (e/for [{:keys [model-id result]} (e/diff-by :model-id results)]
                      (dom/div
                        (dom/props {:style {:flex "1" :min-width "260px" :display "flex" :flex-direction "column" :gap "8px"}})
                        (e/amb
                          (OcrResultColumn model-id result)
                          (UseOcrButton user-id root-topic-id page model-id result !remember !open)))))
                  (e/fn [] (loading/Spinner "Running OCR…"))))
              (dom/div
                (dom/props {:style {:display "flex" :align-items "center" :gap "12px"
                                    :justify-content "space-between"}})
                ;; Lives in phase 2, not beside the picker: "the picked model" means
                ;; the adopted result, and rendering it during selection collides
                ;; with the other pick the user is making.
                (dom/label
                  (dom/props {:style {:display "flex" :align-items "center" :gap "6px" :font-size "13px"}})
                  (dom/input
                    (dom/props {:type "checkbox"})
                    (dom/On "change" (fn [e] (reset! !remember (-> e .-target .-checked))) nil))
                  (dom/text "Remember the picked model for this PDF"))
                (dom/button
                  (dom/props {:class "btn btn-sm btn-secondary"})
                  (dom/text "Change models")
                  ;; Abandons this run's results — its credits are already spent,
                  ;; exactly as Close does today. !selected outlives the branch, so
                  ;; the picker returns with the previous ticks and adding one more
                  ;; model does not force reselecting the rest.
                  (dom/On "click" (fn [_] (reset! !run nil)) nil))))))
        (dom/div
          (dom/props {:style {:display "flex" :justify-content "flex-end"}})
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"})
            (dom/text "Close")
            (dom/On "click" (fn [_] (reset! !open nil)) nil)))))))

(e/defn OcrCompareButton
  "Toolbar button that opens the OCR compare modal for the current page.

   Reads the effective OCR model HERE rather than in the modal, and that placement
   is load-bearing: this button mounts with the toolbar, so the read resolves long
   before any click, while a read inside the modal would race its own mount. An
   unresolved e/server value is an empty e/amb, not nil, so a let binding feeding
   the modal's atom seed would blank the modal body until it arrived."
  [user-id root-topic-id page-number dpi]
  (e/client
    (let [!open (atom nil)
          open (e/watch !open)
          model-ids (e/server (ocr/allowed-ocr-model-ids))
          ;; :settings-refresh is broader than this read needs — every setting write
          ;; bumps it, including DPI and prompts — and it is still the right channel:
          ;; no narrower one exists, the read is two get-settings, and an unrelated
          ;; write returns the same id, so work-skipping means nothing crosses the
          ;; wire. Dropping the watch instead would leave a Remember adoption
          ;; pre-selecting the model it just replaced.
          settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
          effective-model-id (e/server (effective-ocr-model-id* settings-refresh user-id root-topic-id))]
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary"
                    :aria-label "Compare OCR models"})
        (tooltip/Tooltip! "Compare OCR models on this page (uses credits per model)")
        (icons/Icon :scan-text :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text "Compare OCR"))
        (let [node dom/node]
          (bus/publish-invoker! :compare-ocr (fn [] (.click node)))
          (e/on-unmount (fn [] (bus/retract-invoker! :compare-ocr))))
        (dom/On "click" (fn [_] (reset! !open true)) nil))
      (when open
        (OcrCompareModal user-id root-topic-id page-number model-ids effective-model-id dpi !open)))))
