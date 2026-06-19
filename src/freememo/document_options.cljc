(ns freememo.document-options
  "\"Document options\" — per-content processing preferences (Forms5).
   PDF: quick-text-extraction Style (Client = PDF.js / Remote = PDFBox) + a
   disabled Custom Prompt (future). Other kinds: only the disabled Custom Prompt.
   Style persists per-PDF via copy-text/save-style!* (bumps :settings-refresh so
   Copy-text re-reads)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.icons :as icons]
   [freememo.copy-text :as copy]
   [freememo.modal-shell :as modal]
   #?(:clj [freememo.user-state :as us])))

(def ^:private style-options [["ask"    "Ask every time (compare A vs B)"]
                              ["client" "Client (PDF.js, in-browser)"]
                              ["remote" "Remote (PDFBox, server)"]])

(e/defn StyleSelect
  "A1-fallback select (Forms5 has no tracked select). Atom is the truth; the
   Form's :Parse reads (e/watch !style)."
  [!style]
  (let [current (e/watch !style)]
    (dom/div
      (dom/props {:style {:margin-bottom "var(--sp-3)"}})
      (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500" :font-size "13px"}})
        (dom/text "Quick text extraction"))
      (dom/select
        (dom/props {:class "input" :style {:width "100%"}})
        (dom/On "change" (fn [e] (reset! !style (-> e .-target .-value))) nil)
        (e/for [[v label] (e/diff-by first style-options)]
          (dom/option (dom/props {:value v :selected (= v current)}) (dom/text label)))))
    (e/amb)))

(e/defn CustomPromptField
  "Disabled Custom Prompt placeholder — future feature, no typing."
  []
  (dom/div
    (dom/props {:style {:margin-bottom "var(--sp-3)"}})
    (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500" :font-size "13px"}})
      (dom/text "Custom Prompt"))
    (dom/textarea
      (dom/props {:class "input" :style {:width "100%" :min-height "72px" :resize "vertical"}
                  :disabled true :placeholder "Coming soon — a custom card-generation prompt for this item."}))
    (e/amb)))

(e/defn DocumentOptionsDialog
  "PDF: Forms5 Style select + disabled Custom Prompt + Submit (saves style)."
  [user-id root-topic-id current-style !open]
  (let [!style (atom (e/snapshot (or current-style "ask")))
        commits (forms/Form! {}
                  (e/fn Fields [_fields]
                    (e/amb
                      (StyleSelect !style)
                      (CustomPromptField)
                      (dom/div
                        (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
                        (e/amb
                          (forms/SubmitButton! :label "Save" :class "btn btn-primary" :style {:order "1"})
                          (dom/button
                            (dom/props {:class "btn btn-secondary" :type "button"})
                            (dom/text "Cancel")
                            (dom/On "click" (fn [_] (reset! !open false)) nil))))))
                  :Parse (e/fn [_merged _tempid]
                           [`SaveStyle (e/watch !style)])
                  :type :command
                  :show-buttons false)]
    (e/for [[token cmd] (e/diff-by first (e/as-vec commits))]
      (let [style (nth cmd 1)
            r (e/server (e/Offload #(copy/save-style!* user-id root-topic-id style)))]
        (when (some? r)
          (if (:success r) (do (reset! !open false) (token)) (token (:error r))))))))

(e/defn DocumentOptionsModal [user-id topic-id is-pdf? root-topic-id !open]
  (e/client
    (when (e/watch !open)
      (dom/div
        (dom/props {:class "modal-backdrop" :tabindex "-1" :autofocus true})
        (modal/ModalEscape (fn [] (reset! !open false)))
        (dom/On "click" (fn [e] (when (= (.-target e) (.-currentTarget e)) (reset! !open false))) nil)
        (dom/div
          (dom/props {:class "modal-content"
                      :style {:width "min(520px, 95vw)" :max-height "85vh" :overflow-y "auto"
                              :display "flex" :flex-direction "column" :padding "16px 20px"}})
          (dom/On "click" (fn [e] (.stopPropagation e)) nil)
          (dom/h3 (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
            (dom/text "Document options"))
          (if is-pdf?
            (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
                  current (e/server (copy/get-extract-style* settings-refresh user-id root-topic-id))]
              (DocumentOptionsDialog user-id root-topic-id current !open))
            ;; non-PDF: only the disabled Custom Prompt; nothing to save.
            (e/amb
              (CustomPromptField)
              (dom/div
                (dom/props {:style {:display "flex" :justify-content "flex-end"}})
                (dom/button
                  (dom/props {:class "btn btn-primary" :type "button" :disabled true})
                  (dom/text "Save"))))))))))

(e/defn DocumentOptionsButton [user-id topic-id is-pdf? root-topic-id]
  (e/client
    (let [!open (atom false)]
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary"
                    :aria-label "Document options"
                    :data-tooltip "Per-document options"})
        (icons/Icon :settings :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text "Document options"))
        (dom/On "click" (fn [_] (reset! !open true)) nil))
      (DocumentOptionsModal user-id topic-id is-pdf? root-topic-id !open))))
