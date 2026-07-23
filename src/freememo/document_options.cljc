(ns freememo.document-options
  "\"Document options\" — per-content processing preferences (Forms5).
   PDF: quick-text-extraction Style (Client = PDF.js / Remote = PDFBox), per-document
   OCR model, and a per-item Custom Prompt. Other kinds: only the Custom Prompt.
   Style/OCR persist per-PDF (keyed by root-topic-id); the Custom Prompt persists
   per-item (keyed by item-topic-id) via settings/save-custom-prompt and is appended
   to the effective system prompt at card-generation time, inherited nearest-first
   down the topic tree. Style persists via copy-text/save-style!* (bumps
   :settings-refresh so Copy-text re-reads)."
  (:require
   [clojure.string :as str]
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
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])))

(def ^:private style-options [["ask"    "Ask every time (compare A vs B)"]
                              ["client" "Client (PDF.js, in-browser)"]
                              ["remote" "Remote (PDFBox, server)"]])

;; A1-fallback: Forms5 has no tracked select.
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

;; A1-fallback: Forms5 has no tracked select.
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

;; A1-fallback: Forms5 has no tracked select.
(e/defn DocCardModelSelect
  "Per-document card-generation model. \"\" = use my global default. Document-scoped
   (keyed by root-topic-id), applies to every kind. Autosaves on change — sits in
   the modal's shared section beside the priority stepper, which also autosaves,
   so it is not part of the PDF/non-PDF Forms."
  [user-id root-topic-id]
  (e/client
    (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
          current (e/server (do settings-refresh (e/Offload #(settings/get-card-model user-id root-topic-id))))
          default-id (e/server (e/Offload #(settings/get-model user-id)))
          choices (e/server (settings/card-model-choices))
          ;; Name the global default that "" resolves to, minus the
          ;; "Provider · " prefix (registry labels are "OpenAI · GPT-5.1").
          default-name (str/trim (last (str/split (get (into {} choices) default-id default-id) #"·")))
          options (into [["" (str "Use my default (" default-name ")")]] choices)
          !model (atom (e/snapshot (or current "")))
          model (e/watch !model)]
      (dom/div
        (dom/props {:style {:margin-bottom "var(--sp-3)"}})
        (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500" :font-size "13px"}})
          (dom/text "Card generation model"))
        (dom/select
          (dom/props {:class "input" :style {:width "100%"}})
          (e/for [[v label] (e/diff-by first options)]
            (dom/option (dom/props {:value v :selected (= v model)}) (dom/text label)))
          (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                [t _] (e/Token change-event)]
            (dom/props {:disabled (some? t) :aria-busy (some? t)})
            (when (some? change-event)
              (reset! !model change-event))
            (when t
              (let [r (e/server (e/Offload #(settings/save-card-model user-id root-topic-id change-event)))]
                (case r
                  (if (:success r) (t) (t (:error r))))))))))))

(e/defn DocLearningGoalField
  "Per-document learning goal (root-topic-id): the learner's reason for studying
   this document. Grounds the Socratic assistant's questions and card generation.
   Document-scoped, autosaves on blur (textarea 'change') like DocCardModelSelect,
   so it sits outside the PDF/non-PDF Forms. Blank = no goal."
  [user-id root-topic-id]
  (e/client
    (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
          current (e/server (do settings-refresh (e/Offload #(settings/get-learning-goal user-id root-topic-id))))
          !goal (atom (e/snapshot (or current "")))
          goal (e/watch !goal)]
      (dom/div
        (dom/props {:style {:margin-bottom "var(--sp-3)"}})
        (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500" :font-size "13px"}})
          (dom/text "Learning goal"))
        (dom/textarea
          (dom/props {:class "input"
                      :style {:width "100%" :min-height "72px" :resize "vertical"}
                      :maxlength 2000
                      :placeholder "Why are you studying this? Grounds the assistant's questions and card generation."
                      :value goal})
          (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                [t _] (e/Token change-event)]
            (dom/props {:aria-busy (some? t)})
            (when (some? change-event)
              (reset! !goal change-event))
            (when t
              (let [r (e/server (e/Offload #(settings/save-learning-goal user-id root-topic-id change-event)))]
                (case r
                  (if (:success r) (t) (t (:error r))))))))))))

(e/defn CustomPromptField
  "Editable per-item Custom Prompt, appended to the effective system prompt for
   this item and everything under it. Tracked Forms5 field (Input! :as :textarea)
   — contributes its edit into the surrounding Form!'s e/amb; :Parse reads it back
   from the merged :prompt key. Blank = inherit the nearest ancestor's (or the
   global) prompt."
  [prompt]
  (dom/div
    (dom/props {:style {:margin-bottom "var(--sp-3)"}})
    (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500" :font-size "13px"}})
      (dom/text "Custom Prompt"))
    (forms/Input! :prompt prompt :as :textarea
      :class "input" :style {:width "100%" :min-height "72px" :resize "vertical"}
      :maxlength 5000
      :placeholder "Extra card-generation instructions for this item (and anything under it).")))

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
                            (when (js/confirm "Overwrite the bibliography of all descendant extracts with this item's bibliography? This cannot be undone.")
                              e))
                          nil)
            [t ?error] (e/Token click-event)]
        (dom/props {:class "btn btn-sm btn-secondary" :type "button"
                    :aria-label "Push bibliography to children"
                    :disabled (or (not enabled?) (some? t))
                    :aria-busy (some? t)
                    :style (when flash-success?
                             {:background "var(--color-success-light)"
                              :border-color "var(--color-success)"
                              :color "var(--color-success-dark)"
                              :transition "background-color 0.2s ease, border-color 0.2s ease, color 0.2s ease"})})
        (tooltip/Tooltip! "Copy this bibliography to all descendant extracts")
        (icons/Icon :download :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text "Push to children"))
        (when ?error
          (dom/div
            (dom/props {:style {:color "var(--color-danger-text)" :font-size "11px" :margin-top "4px"}})
            (dom/text ?error)))
        (when t
          (let [result (e/server (e/Offload #(push-biblio-to-descendants!* user-id bib-topic-id)))]
            (when (some? result)
              (if (:ok result)
                (do (e/on-unmount
                      (fn []
                        (reset! !flash-success true)
                        (js/setTimeout #(reset! !flash-success false) 1200)))
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
          has-source? (e/server (e/Offload #(bib-btn/has-source?* refresh user-id bib-topic-id)))
          eff-source? (e/server (e/Offload #(has-effective-source?* refresh user-id bib-topic-id)))]
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
              (dom/props {:class "btn btn-sm btn-secondary" :type "button"})
              (tooltip/Tooltip! "Edit bibliography" :aria? true)
              (icons/Icon :book-open :size 16)
              (dom/span (dom/props {:class "icon-label"}) (dom/text "Edit"))
              (dom/On "click" (fn [_] (reset! !open false) (reset! !show-bib true)) nil)))
          (bib-btn/BibliographyButton user-id bib-topic-id has-source? nil)
          (PushToChildrenButton user-id bib-topic-id eff-source?))))))

(e/defn DocumentOptionsDialog
  "PDF: Forms5 Style select + OCR-model select + per-item Custom Prompt + Submit.
   Style and OCR model are document-scoped (keyed by root-topic-id); the prompt is
   item-scoped (keyed by item-topic-id), tracked via Input! and read back from the
   merged form fields. A save that fails the prompt length check surfaces the
   error and leaves style/OCR untouched."
  [user-id root-topic-id item-topic-id current-style current-ocr-model current-prompt !open]
  (let [!style (atom (e/snapshot (or current-style "ask")))
        !ocr-model (atom (e/snapshot (or current-ocr-model "")))
        commits (forms/Form! {:prompt (or current-prompt "")}
                  (e/fn Fields [{:keys [prompt]}]
                    (e/amb
                      (StyleSelect !style)
                      (OcrModelSelect !ocr-model)
                      (CustomPromptField prompt)
                      (dom/div
                        (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
                        (e/amb
                          (forms/SubmitButton! :label "Save" :class "btn btn-primary" :style {:order "1"})
                          (dom/button
                            (dom/props {:class "btn btn-secondary" :type "button"})
                            (dom/text "Cancel")
                            (dom/On "click" (fn [_] (reset! !open false)) nil))))))
                  :Parse (e/fn [merged _tempid]
                           [`SaveDocOptions {:style (e/watch !style)
                                             :ocr-model (e/watch !ocr-model)
                                             :prompt (:prompt merged)}])
                  :type :command
                  :show-buttons false)]
    (e/for [[token cmd] (e/diff-by first (e/as-vec commits))]
      (let [{:keys [style ocr-model prompt]} (nth cmd 1)
            r (e/server (e/Offload #(let [pr (settings/save-custom-prompt user-id item-topic-id prompt)]
                                      (if-not (:success pr)
                                        pr
                                        (let [sr (copy/save-style!* user-id root-topic-id style)]
                                          (if-not (:success sr)
                                            sr
                                            (settings/save-ocr-model user-id root-topic-id ocr-model)))))))]
        (when (some? r)
          (if (:success r) (do (reset! !open false) (token)) (token (:error r))))))))

(e/defn CustomPromptDialog
  "Non-PDF: a Forms5 form with just the per-item Custom Prompt (tracked Input!)
   + Save."
  [user-id item-topic-id current-prompt !open]
  (let [commits (forms/Form! {:prompt (or current-prompt "")}
                  (e/fn Fields [{:keys [prompt]}]
                    (e/amb
                      (CustomPromptField prompt)
                      (dom/div
                        (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
                        (e/amb
                          (forms/SubmitButton! :label "Save" :class "btn btn-primary" :style {:order "1"})
                          (dom/button
                            (dom/props {:class "btn btn-secondary" :type "button"})
                            (dom/text "Cancel")
                            (dom/On "click" (fn [_] (reset! !open false)) nil))))))
                  :Parse (e/fn [merged _tempid]
                           [`SaveCustomPrompt {:prompt (:prompt merged)}])
                  :type :command
                  :show-buttons false)]
    (e/for [[token cmd] (e/diff-by first (e/as-vec commits))]
      (let [{:keys [prompt]} (nth cmd 1)
            r (e/server (e/Offload #(settings/save-custom-prompt user-id item-topic-id prompt)))]
        (when (some? r)
          (if (:success r) (do (reset! !open false) (token)) (token (:error r))))))))

(e/defn DismissButton
  "Toggle Dismiss for the whole document (root-topic-id + its subtree): remove it
   from / restore it to the Learning Queue. Reads the current state reactively so
   the label flips after the roundtrip. The dismiss-vs-undismiss intent is
   captured at click time so a mid-flight :refresh can't flip which mutation runs."
  [user-id root-topic-id]
  (e/client
    (let [refresh    (e/server (e/watch (us/get-atom user-id :refresh)))
          dismissed? (e/server (do refresh (e/Offload #(db/topic-dismissed? root-topic-id))))
          !click     (atom nil)
          click      (e/watch !click)]
      (dom/div
        (dom/props {:style {:margin-bottom "var(--sp-3)"}})
        (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500" :font-size "13px"}})
          (dom/text "Learning"))
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary" :type "button"
                      :aria-label (if dismissed? "Undismiss" "Dismiss")})
          (tooltip/Tooltip! (if dismissed?
                              "Restore this document and its contents to the Learning Queue"
                              "Remove this document and its contents from the Learning Queue"))
          (icons/Icon (if dismissed? :rotate-ccw :x) :size 16)
          (dom/span (dom/props {:class "icon-label"})
            (dom/text (if dismissed? "Undismiss" "Dismiss")))
          (dom/On "click" (fn [_] (reset! !click {:dismiss? (not dismissed?) :n (str (random-uuid))})) nil))
        (let [[t _] (e/Token click)]
          (when t
            (let [to-dismiss? (:dismiss? click)]
              (case (e/server (e/Offload #(do (if to-dismiss?
                                                (db/dismiss-topic! user-id root-topic-id)
                                                (db/undismiss-topic! user-id root-topic-id))
                                              :ok)))
                (case (e/server (commands/bump! user-id (if to-dismiss? :dismiss :undismiss)))
                  (t))))))))))

(e/defn DocumentOptionsModal [user-id bib-topic-id is-pdf? root-topic-id item-topic-id priority-topic-id !open !show-bib show-edit?]
  (e/client
    (when (e/watch !open)
      (dom/div
        (dom/props {:class "modal-backdrop" :tabindex "-1"})
        (modal/ModalEscape (fn [] (reset! !open false)) "Document options")
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
                (e/server (e/Offload #(get-topic-priority* refresh priority-topic-id))))))

          ;; Learning — Dismiss/Undismiss the whole document. Viewer only:
          ;; show-edit? is true only from the viewer toolbar; the library row
          ;; ⋯-menu has its own Dismiss item, so gating here avoids a duplicate.
          (when show-edit?
            (DismissButton user-id root-topic-id))

          ;; Bibliography — item-scoped Edit / Refetch / Push-to-children.
          (BibliographySection user-id bib-topic-id !open !show-bib show-edit?)

          ;; Card-generation model — document-scoped (root-topic-id), all kinds.
          (DocCardModelSelect user-id root-topic-id)

          ;; Learning goal — document-scoped (root-topic-id); grounds the
          ;; assistant and card generation.
          (DocLearningGoalField user-id root-topic-id)

          (if is-pdf?
            (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
                  current (e/server (e/Offload #(copy/get-extract-style* settings-refresh user-id root-topic-id)))
                  current-ocr (e/server (do settings-refresh (e/Offload #(settings/get-ocr-model user-id root-topic-id))))
                  current-prompt (e/server (do settings-refresh (e/Offload #(settings/get-custom-prompt user-id item-topic-id))))]
              (DocumentOptionsDialog user-id root-topic-id item-topic-id current current-ocr current-prompt !open))
            ;; non-PDF: per-item Custom Prompt only, in its own Forms5 form.
            (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
                  current-prompt (e/server (do settings-refresh (e/Offload #(settings/get-custom-prompt user-id item-topic-id))))]
              (CustomPromptDialog user-id item-topic-id current-prompt !open))))))))

;; item-topic-id — the topic the Custom Prompt attaches to: the viewed page/extract
;;                 in the toolbar, the row's id in the library. Distinct from
;;                 root-topic-id (document-scoped style/OCR); equals the enqueue
;;                 :topic-id so the edited prompt is the one generation resolves.
;; show-edit?    — render BibliographySection's Edit button (needs the viewer's
;;                 bibliography modal); false on surfaces without it.
;; trigger-class — CSS class for the trigger button; lets a caller (e.g. the
;;                 library row menu) restyle it as a dropdown row. Defaults to
;;                 the toolbar's secondary-button look.
(e/defn DocumentOptionsButton [user-id bib-topic-id is-pdf? root-topic-id item-topic-id priority-topic-id !show-bib show-edit? trigger-class]
  (e/client
    (let [!open (atom false)]
      (dom/button
        (dom/props {:class (or trigger-class "btn btn-sm btn-secondary")
                    :aria-label "Document options"})
        (tooltip/Tooltip! "Per-document options")
        (icons/Icon :settings :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text "Document options"))
        ;; stop propagation so a click doesn't reach an ancestor row handler
        ;; (the library tree row navigates on click); inert in the toolbar.
        (dom/On "click" (fn [e] (.stopPropagation e) (reset! !open true)) nil))
      (DocumentOptionsModal user-id bib-topic-id is-pdf? root-topic-id item-topic-id priority-topic-id !open !show-bib show-edit?))))
