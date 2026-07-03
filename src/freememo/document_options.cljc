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
   [freememo.ocr-models :as ocr-models]
   [freememo.modal-shell :as modal]
   [freememo.priority-control :refer [PriorityControl get-topic-priority*]]
   [freememo.bibliography-button :as bib-btn]
   [freememo.commands :as commands]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])))

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

(e/defn OcrModelSelect
  "Per-document OCR model for \"Scan Page\". \"\" = use my global default.
   Atom is the truth; the Form's :Parse reads (e/watch !model)."
  [!model]
  (let [current (e/watch !model)
        options (into [["" "Use my default"]]
                  (map (juxt :id :label) ocr-models/registry))]
    (dom/div
      (dom/props {:style {:margin-bottom "var(--sp-3)"}})
      (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500" :font-size "13px"}})
        (dom/text "OCR model (Scan Page)"))
      (dom/select
        (dom/props {:class "input" :style {:width "100%"}})
        (dom/On "change" (fn [e] (reset! !model (-> e .-target .-value))) nil)
        (e/for [[v label] (e/diff-by first options)]
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

;; ---------------------------------------------------------------------------
;; Bibliography section — item-scoped Edit / Refetch / Push-to-children
;; ---------------------------------------------------------------------------

(defn push-biblio-to-descendants!*
  "Server: push the item's effective bibliography into every descendant extract
   (overwrite-all), then bump :refresh so citations re-read.
   Pre:  user-id, topic-id non-nil.
   Post: {:ok true :count n} | {:ok false :error kw}."
  [user-id topic-id]
  #?(:clj (let [r (db/push-bibliography-to-descendants! user-id topic-id)]
            (when (:ok r) (commands/bump! user-id :push-biblio))
            r)
     :cljs nil))

(defn has-effective-source?*
  "Server: true iff the topic resolves to any bibliography source (own or
   inherited). `_refresh` is a reactive dep so the gate re-reads after edits."
  [_refresh user-id topic-id]
  #?(:clj (boolean (and user-id topic-id (db/resolve-effective-source-id topic-id)))
     :cljs false))

(e/defn PushToChildrenButton
  "Copy this item's effective bibliography into every descendant extract
   (overwrite-all), after a confirm. Disabled when the item has no effective
   source or while a roundtrip is in flight; flashes green on success, shows
   the error text on failure. Mirrors BibliographyButton's token pattern."
  [user-id bib-topic-id enabled?]
  (e/client
    (dom/button
      (let [!flash-success (atom false)
            flash-success? (e/watch !flash-success)
            click-event (dom/On "click"
                          (fn [e]
                            (when #?(:cljs (js/confirm "Overwrite the bibliography of all descendant extracts with this item's bibliography? This cannot be undone.")
                                     :clj true)
                              e))
                          nil)
            [t ?error] (e/Token click-event)]
        (dom/props {:class "btn btn-sm btn-secondary" :type "button"
                    :aria-label "Push bibliography to children"
                    :data-tooltip "Copy this bibliography to all descendant extracts"
                    :disabled (or (not enabled?) (some? t))
                    :aria-busy (some? t)
                    :style (when flash-success?
                             {:background "var(--color-success-light)"
                              :border-color "var(--color-success)"
                              :color "var(--color-success-dark)"
                              :transition "background-color 0.2s ease, border-color 0.2s ease, color 0.2s ease"})})
        (icons/Icon :download :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text "Push to children"))
        (when ?error
          (dom/div
            (dom/props {:style {:color "var(--color-danger)" :font-size "11px" :margin-top "4px"}})
            (dom/text ?error)))
        (when t
          (let [result (e/server (e/Offload #(push-biblio-to-descendants!* user-id bib-topic-id)))]
            (when (some? result)
              (if (:ok result)
                (do (e/on-unmount
                      (fn []
                        (reset! !flash-success true)
                        #?(:cljs (js/setTimeout #(reset! !flash-success false) 1200))))
                    (t))
                (t (str "Failed: " (name (or (:error result) :error))))))))))))

(e/defn BibliographySection
  "Item-scoped bibliography ops. Edit opens the bibliography modal mounted in
   TopicPage (closes this options modal first, avoiding modal stacking);
   Refetch reuses BibliographyButton (own-source gate); Push-to-children copies
   this item's bibliography down the subtree.
   Pre: bib-topic-id is the item's bibliography target — for PDF pages this is
   the document root, since pages own no bibliography."
  [user-id bib-topic-id !open !show-bib show-edit?]
  (e/client
    (let [refresh     (e/server (e/watch (us/get-atom user-id :refresh)))
          has-source? (e/server (bib-btn/has-source?* refresh user-id bib-topic-id))
          eff-source? (e/server (has-effective-source?* refresh user-id bib-topic-id))]
      (dom/div
        (dom/props {:style {:margin-bottom "var(--sp-3)"}})
        (dom/label
          (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500" :font-size "13px"}})
          (dom/text "Bibliography"))
        (dom/div
          (dom/props {:style {:display "flex" :flex-wrap "wrap" :gap "var(--sp-2)"}})
          ;; Edit opens the bibliography modal mounted in the viewer (TopicPage);
          ;; gated off on surfaces that don't mount it (e.g. the library tree).
          (when show-edit?
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :type "button"
                          :aria-label "Edit bibliography" :data-tooltip "Edit bibliography"})
              (icons/Icon :book-open :size 16)
              (dom/span (dom/props {:class "icon-label"}) (dom/text "Edit"))
              (dom/On "click" (fn [_] (reset! !open false) (reset! !show-bib true)) nil)))
          (bib-btn/BibliographyButton user-id bib-topic-id has-source? nil)
          (PushToChildrenButton user-id bib-topic-id eff-source?))))))

(e/defn DocumentOptionsDialog
  "PDF: Forms5 Style select + OCR-model select + disabled Custom Prompt + Submit
   (saves both the quick-extract style and the per-document OCR model)."
  [user-id root-topic-id current-style current-ocr-model !open]
  (let [!style (atom (e/snapshot (or current-style "ask")))
        !ocr-model (atom (e/snapshot (or current-ocr-model "")))
        commits (forms/Form! {}
                  (e/fn Fields [_fields]
                    (e/amb
                      (StyleSelect !style)
                      (OcrModelSelect !ocr-model)
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
                           [`SaveDocOptions {:style (e/watch !style)
                                             :ocr-model (e/watch !ocr-model)}])
                  :type :command
                  :show-buttons false)]
    (e/for [[token cmd] (e/diff-by first (e/as-vec commits))]
      (let [{:keys [style ocr-model]} (nth cmd 1)
            r (e/server (e/Offload #(do (copy/save-style!* user-id root-topic-id style)
                                        (settings/save-ocr-model user-id root-topic-id ocr-model)
                                        {:success true})))]
        (when (some? r)
          (if (:success r) (do (reset! !open false) (token)) (token (:error r))))))))

(e/defn DocumentOptionsModal [user-id bib-topic-id is-pdf? root-topic-id priority-topic-id !open !show-bib show-edit?]
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

          ;; Review priority (C5) — applies to all kinds; the stepper autosaves
          ;; on change via update-priority!*, independent of the form's Save.
          (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))]
            (dom/div
              (dom/props {:style {:margin-bottom "var(--sp-3)"}})
              (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500" :font-size "13px"}})
                (dom/text "Review priority"))
              (PriorityControl user-id priority-topic-id
                (e/server (get-topic-priority* refresh priority-topic-id)))))

          ;; Bibliography — item-scoped Edit / Refetch / Push-to-children.
          (BibliographySection user-id bib-topic-id !open !show-bib show-edit?)

          (if is-pdf?
            (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
                  current (e/server (copy/get-extract-style* settings-refresh user-id root-topic-id))
                  current-ocr (e/server (do settings-refresh (settings/get-ocr-model user-id root-topic-id)))]
              (DocumentOptionsDialog user-id root-topic-id current current-ocr !open))
            ;; non-PDF: only the disabled Custom Prompt; nothing to save.
            (e/amb
              (CustomPromptField)
              (dom/div
                (dom/props {:style {:display "flex" :justify-content "flex-end"}})
                (dom/button
                  (dom/props {:class "btn btn-primary" :type "button" :disabled true})
                  (dom/text "Save"))))))))))

;; show-edit?    — render BibliographySection's Edit button (needs the viewer's
;;                 bibliography modal); false on surfaces without it.
;; trigger-class — CSS class for the trigger button; lets a caller (e.g. the
;;                 library row menu) restyle it as a dropdown row. Defaults to
;;                 the toolbar's secondary-button look.
(e/defn DocumentOptionsButton [user-id bib-topic-id is-pdf? root-topic-id priority-topic-id !show-bib show-edit? trigger-class]
  (e/client
    (let [!open (atom false)]
      (dom/button
        (dom/props {:class (or trigger-class "btn btn-sm btn-secondary")
                    :aria-label "Document options"
                    :data-tooltip "Per-document options"})
        (icons/Icon :settings :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text "Document options"))
        ;; stop propagation so a click doesn't reach an ancestor row handler
        ;; (the library tree row navigates on click); inert in the toolbar.
        (dom/On "click" (fn [e] (.stopPropagation e) (reset! !open true)) nil))
      (DocumentOptionsModal user-id bib-topic-id is-pdf? root-topic-id priority-topic-id !open !show-bib show-edit?))))
