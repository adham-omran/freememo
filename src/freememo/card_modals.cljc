(ns freememo.card-modals
  "Extracted modal components for ocr-page — kept separate to stay below JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [clojure.string :as str]
   [freememo.typeahead :refer [Typeahead]]
   [freememo.quill-field :refer [QuillField flush-syntax-tokens!]]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.cards :as cards])
   #?(:clj [freememo.html-cleaner :as cleaner])
   #?(:clj [freememo.settings :as settings])))

(defn sanitize-card-field
  "Sanitize a card field's HTML server-side. Uses clean-html (the user-trust
   variant) — user-pasted <img> tags survive (pin-aware authoring may use them)."
  [html]
  #?(:clj (when html (cleaner/clean-html html))
     :cljs html))

;; Browser download helper
(defn trigger-download! [filename content]
  #?(:cljs
     (let [blob (js/Blob. #js [content] #js {:type "text/csv"})
           url (.createObjectURL js/URL blob)
           a (.createElement js/document "a")]
       (set! (.-href a) url)
       (set! (.-download a) filename)
       (.click a)
       (.revokeObjectURL js/URL url))))

(defn validate-cloze
  "Validate cloze MARKER structure, not raw brace counts — card answers may
   contain literal { } from code (e.g. a trailing }} that closes two Java
   blocks). Counts {{cN::}} openers and how many are closed by a later }}
   (non-greedy), then checks the numbering runs 1..max."
  [text]
  #?(:cljs
     (let [text     (or text "")
           opens    (count (re-seq #"\{\{c\d+::" text))
           closed   (count (re-seq #"\{\{c\d+::[\s\S]*?\}\}" text))
           nums     (set (map #(js/parseInt (second %) 10) (re-seq #"\{\{c(\d+)::" text)))
           max-n    (if (seq nums) (apply max nums) 0)
           expected (set (range 1 (inc max-n)))]
       (cond
         (zero? opens)
         "No cloze deletion: select text and press the {+} button to add {{c1::...}}"
         (not= opens closed)
         "Unclosed cloze: a {{cN::...}} is missing its closing }}"
         (not= nums expected)
         (str "Non-sequential cloze numbers: found " (sort nums) ", expected 1 to " max-n)
         :else nil))
     :clj nil))

;; Export modal — Forms5 (Form! + Input!/Checkbox!).
;; Scope and Card Type render via A1-fallback selects (atom-backed); their
;; values are merged into the parsed command at commit time. Pattern mirrors
;; bibliography_form.cljc CslTypeSelect.
(def ^:private export-scope-options
  [["Current Page" "Current Page"]
   ["Entire Doc" "Entire Document"]])

(def ^:private export-kind-options
  [["Both" "Both (Basic + Cloze)"]
   ["Basic" "Basic Only"]
   ["Cloze" "Cloze Only"]])

(e/defn ExportScopeSelect [!scope]
  (let [current (e/watch !scope)]
    (dom/div
      (dom/props {:style {:margin-bottom "var(--sp-4)"}})
      (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500"}})
        (dom/text "Scope:"))
      (dom/select
        (dom/props {:class "select" :style {:width "100%"}})
        (dom/On "change" (fn [e] (reset! !scope (-> e .-target .-value))) nil)
        (e/for [[v label] (e/diff-by first export-scope-options)]
          (dom/option
            (dom/props {:value v :selected (= v current)})
            (dom/text label))))))
  (e/amb))

(e/defn ExportKindSelect [!kind]
  (let [current (e/watch !kind)]
    (dom/div
      (dom/props {:style {:margin-bottom "var(--sp-6)"}})
      (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500"}})
        (dom/text "Card Type:"))
      (dom/select
        (dom/props {:class "select" :style {:width "100%"}})
        (dom/On "change" (fn [e] (reset! !kind (-> e .-target .-value))) nil)
        (e/for [[v label] (e/diff-by first export-kind-options)]
          (dom/option
            (dom/props {:value v :selected (= v current)})
            (dom/text label))))))
  (e/amb))

(e/defn ExportCancelButton [!show-export]
  (dom/button
    (dom/props {:class "btn btn-secondary" :type "button"})
    (dom/text "Cancel")
    (dom/On "click" (fn [_] (reset! !show-export false)) nil))
  (e/amb))

;; !show-export: atom bool  topic-id: current topic  root-topic-id: root topic  user-id: int
(e/defn ExportModal [!show-export topic-id root-topic-id user-id]
  (e/client
    ;; e/snapshot seeds A1 atoms once at mount — mirrors bibliography_form.cljc:296-297.
    (let [!scope (atom (e/snapshot "Current Page"))
          !kind (atom (e/snapshot "Both"))]
      (dom/div
        (dom/props {:class "modal-backdrop" :tabindex "-1"})
        (dom/On "click" (fn [_] (reset! !show-export false)) nil)
        (dom/On "keydown"
          (fn [e]
            #?(:cljs
               (when (= (.-key e) "Escape")
                 (reset! !show-export false))))
          nil)
        (dom/div
          (dom/props {:class "modal-content modal-sm"})
          (dom/On "click" (fn [e] (.stopPropagation e)) nil)
          (dom/h3 (dom/props {:style {:margin-top "0" :margin-bottom "20px"}})
            (dom/text "Export Cards"))
          (let [commits (forms/Form! {:use-header? false :header-text ""}
                          (e/fn Fields [{:keys [use-header? header-text]}]
                            (e/amb
                              (dom/div
                                (dom/props {:style {:margin-bottom "var(--sp-4)"}})
                                (dom/label
                                  (dom/props {:style {:display "flex" :align-items "center"
                                                      :gap "var(--sp-2)" :margin-bottom "var(--sp-2)"}})
                                  (forms/Checkbox! :use-header? (boolean use-header?))
                                  (dom/text "Add custom header to each card"))
                                (e/When use-header?
                                  (forms/Input! :header-text (or header-text "")
                                    :placeholder "e.g., Chapter 5: Accounting"
                                    :class "input input-full"
                                    :style {:padding "var(--sp-2)"})))

                              (ExportScopeSelect !scope)
                              (ExportKindSelect !kind)

                              (dom/div
                                (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "var(--sp-3)"}})
                                (e/amb
                                  (forms/SubmitButton! :label "Export"
                                    :class "btn btn-primary"
                                    :style {:font-weight "600" :order "1"})
                                  (ExportCancelButton !show-export)))))
                          :Parse (e/fn [fields _tempid]
                                   [`Export-cards
                                    {:topic-id topic-id
                                     :root-topic-id root-topic-id
                                     :user-id user-id
                                     :scope (e/watch !scope)
                                     :kind (e/watch !kind)
                                     :header-text (when (:use-header? fields)
                                                    (:header-text fields))}])
                          :type :command
                          :show-buttons false)]
            (e/for [[token cmd] (e/diff-by first (e/as-vec commits))]
              (let [opts (nth cmd 1)
                    kind (:kind opts)
                    results (if (= "Both" kind)
                              (e/server
                                (e/Offload
                                  #(mapv (fn [k] (cards/export-cards-csv (assoc opts :kind k)))
                                     ["basic" "cloze"])))
                              (e/server
                                (e/Offload
                                  #(vector (cards/export-cards-csv
                                             (assoc opts :kind (str/lower-case kind)))))))]
                (when (some? results)
                  (let [successes (filter :success results)]
                    (if (seq successes)
                      (do (doseq [r successes]
                            (trigger-download! (:filename r) (:csv r)))
                        (reset! !show-export false)
                        (token))
                      (token (or (some :error results) "Export failed")))))))))))))

;; Pre-prompt dialog
;; state map keys: :!show :!prompt-submit :!pre-prompt :!prompt-history
;;                 :!history-save-trigger :captured-selection :prompt-dialog-kind
(e/defn PromptDialog [state]
  (e/client
    (let [{:keys [!show !prompt-submit !pre-prompt !prompt-history
                  !history-save-trigger captured-selection prompt-dialog-kind]} state
          pre-prompt-value (e/watch !pre-prompt)
          !local-prompt (atom pre-prompt-value)
          local-prompt (e/watch !local-prompt)
          !primary-btn (atom nil)]
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "transparent" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"
                            :pointer-events "none"}
                    :tabindex "-1"})
        (dom/On "keydown"
          (fn [e]
            #?(:cljs
               (cond
                 (= (.-key e) "Escape")
                 (reset! !show false)
                 (and (= (.-key e) "Enter") (or (.-metaKey e) (.-ctrlKey e)))
                 (when-let [btn @!primary-btn]
                   (.preventDefault e)
                   (.click btn)))))
          nil)
        (dom/div
          (dom/props {:style {:background "var(--color-bg-card)" :border-radius "var(--radius-lg)" :padding "var(--sp-6)"
                              :width "500px" :max-width "90%" :box-shadow "0 4px 20px rgba(0,0,0,0.25)"
                              :pointer-events "auto"}})
          (dom/On "pointerdown"
            (fn [e]
              #?(:cljs
                 (let [inner (.-currentTarget e)
                       h3 (.querySelector inner "h3")]
                   (when (and h3 (or (= (.-target e) h3)
                                   (.contains h3 (.-target e))))
                     (.preventDefault e)
                     (let [rect (.getBoundingClientRect inner)
                           sx (.-clientX e)
                           sy (.-clientY e)
                           px (.-left rect)
                           py (.-top rect)
                           move-fn (fn [me]
                                     (set! (.-position (.-style inner)) "fixed")
                                     (set! (.-left (.-style inner))
                                       (str (+ px (- (.-clientX me) sx)) "px"))
                                     (set! (.-top (.-style inner))
                                       (str (+ py (- (.-clientY me) sy)) "px"))
                                     (set! (.-margin (.-style inner)) "0"))
                           up-fn (fn self [ue]
                                   (.releasePointerCapture inner (.-pointerId ue))
                                   (.removeEventListener inner "pointermove" move-fn)
                                   (.removeEventListener inner "pointerup" self))]
                       (.setPointerCapture inner (.-pointerId e))
                       (.addEventListener inner "pointermove" move-fn)
                       (.addEventListener inner "pointerup" up-fn))))))
            nil)
          (dom/h3
            (dom/props {:style {:margin-top "0" :cursor "move" :user-select "none"
                                :padding-bottom "var(--sp-2)" :margin-bottom "var(--sp-3)"
                                :border-bottom "1px solid var(--color-border)" :font-size "18px"}})
            (dom/text "Generate " (if (= prompt-dialog-kind "basic") "Basic" "Cloze") " Cards"))
          (dom/div
            (dom/props {:style {:margin-bottom "20px"}})
            (dom/label
              (dom/props {:style {:display "block" :margin-bottom "var(--sp-2)" :font-size "14px"}})
              (dom/text "Pre-prompt (will be added to the system prompt):"))
            (let [prompt-history (e/watch !prompt-history)]
              (Typeahead !local-prompt (take 20 prompt-history) "e.g., Focus on accounting terminology..." nil true)))
          (dom/div
            (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "var(--sp-3)"}})
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:order "1"}})
              (reset! !primary-btn dom/node)
              (dom/text "Generate")
              (dom/On "click"
                (fn [_]
                  (reset! !pre-prompt local-prompt)
                  (when (seq local-prompt)
                    (let [new-history (->> (cons local-prompt @!prompt-history)
                                        (distinct)
                                        (take 50)
                                        (vec))]
                      (reset! !prompt-history new-history)
                      (reset! !history-save-trigger new-history)))
                  (reset! !prompt-submit {:selection captured-selection
                                          :pre-prompt local-prompt
                                          :kind prompt-dialog-kind})
                  (reset! !show false))
                nil))
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil))))))))

;; Edit card modal
(e/defn EditCardModal [!editing-card user-id]
  (e/client
    (let [editing-card (e/watch !editing-card)
          card-id (:id editing-card)
          kind (:kind editing-card)
          init-q (or (:question editing-card) "")
          init-a (or (:answer editing-card) "")
          init-c (or (:cloze editing-card) "")
          !question (atom init-q)
          !answer (atom init-a)
          !cloze (atom init-c)
          !q-editor (atom nil)
          !a-editor (atom nil)
          !c-editor (atom nil)
          question (e/watch !question)
          answer (e/watch !answer)
          cloze (e/watch !cloze)
          card-font-sz (e/server (settings/get-card-font-size user-id))
          modal-font (str (or card-font-sz 14) "px")
          !primary-btn (atom nil)]
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "transparent" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"
                            :pointer-events "none"}
                    :tabindex "-1"})
        (dom/On "keydown"
          (fn [e]
            #?(:cljs
               (cond
                 (= (.-key e) "Escape")
                 (reset! !editing-card nil)
                 (and (= (.-key e) "Enter") (or (.-metaKey e) (.-ctrlKey e)))
                 (when-let [btn @!primary-btn]
                   (.preventDefault e)
                   (.click btn)))))
          nil)
        (dom/div
          (dom/props {:class "card-modal-inner"
                      :style {:background "var(--color-bg-card)" :border-radius "var(--radius-lg)" :padding "var(--sp-6)"
                              :box-shadow "0 4px 20px rgba(0,0,0,0.25)"
                              :pointer-events "auto"}})
          (dom/On "pointerdown"
            (fn [e]
              #?(:cljs
                 (let [inner (.-currentTarget e)
                       h3 (.querySelector inner "h3")]
                   (when (and h3 (or (= (.-target e) h3)
                                   (.contains h3 (.-target e))))
                     (.preventDefault e)
                     (let [rect (.getBoundingClientRect inner)
                           sx (.-clientX e)
                           sy (.-clientY e)
                           px (.-left rect)
                           py (.-top rect)
                           move-fn (fn [me]
                                     (set! (.-position (.-style inner)) "fixed")
                                     (set! (.-left (.-style inner))
                                       (str (+ px (- (.-clientX me) sx)) "px"))
                                     (set! (.-top (.-style inner))
                                       (str (+ py (- (.-clientY me) sy)) "px"))
                                     (set! (.-margin (.-style inner)) "0"))
                           up-fn (fn self [ue]
                                   (.releasePointerCapture inner (.-pointerId ue))
                                   (.removeEventListener inner "pointermove" move-fn)
                                   (.removeEventListener inner "pointerup" self))]
                       (.setPointerCapture inner (.-pointerId e))
                       (.addEventListener inner "pointermove" move-fn)
                       (.addEventListener inner "pointerup" up-fn))))))
            nil)
          (dom/h3 (dom/props {:style {:margin-top "0" :cursor "move" :user-select "none"
                                      :padding-bottom "var(--sp-2)" :margin-bottom "var(--sp-3)"
                                      :border-bottom "1px solid var(--color-border)"}})
            (dom/text "Edit " (if (= kind "basic") "Basic" "Cloze") " Card"))
          (if (= kind "basic")
            (dom/div
              (dom/label (dom/text "Question:"))
              (dom/div
                (dom/props {:style {:margin-bottom "var(--sp-3)" :font-size modal-font}})
                (QuillField {:value-string init-q
                             :on-change (fn [html] (reset! !question html))
                             :placeholder "Question..."
                             :field-key [:edit-q card-id]
                             :!editor-atom !q-editor}))
              (dom/label (dom/text "Answer:"))
              (dom/div
                (dom/props {:style {:font-size modal-font}})
                (QuillField {:value-string init-a
                             :on-change (fn [html] (reset! !answer html))
                             :placeholder "Answer..."
                             :field-key [:edit-a card-id]
                             :!editor-atom !a-editor})))
            (dom/div
              (dom/label (dom/text "Cloze:"))
              (dom/div
                (dom/props {:style {:margin-bottom "var(--sp-3)" :font-size modal-font}})
                (QuillField {:value-string init-c
                             :on-change (fn [html] (reset! !cloze html))
                             :placeholder "Cloze text with {{c1::deletion}}..."
                             :field-key [:edit-c card-id]
                             :!editor-atom !c-editor
                             :cloze? true}))
              (dom/label (dom/text "Back Extra:"))
              (dom/div
                (dom/props {:style {:font-size modal-font}})
                (QuillField {:value-string init-a
                             :on-change (fn [html] (reset! !answer html))
                             :placeholder "Optional back extra..."
                             :field-key [:edit-be card-id]
                             :!editor-atom !a-editor}))))
          (dom/div
            (dom/props {:style {:display "flex" :justify-content "flex-end" :align-items "center" :gap "var(--sp-2)" :margin-top "var(--sp-4)"}})
            (let [click-event
                  (dom/button
                    (dom/props {:class "btn btn-primary" :style {:order "1"}})
                    (reset! !primary-btn dom/node)
                    (dom/text "Save")
                    ;; Force the syntax module to flush its hljs-* spans into the
                    ;; DOM, then read the post-flush innerHTML and reset! the
                    ;; matching atom directly. Quill 2.0.3's syntax module does
                    ;; NOT fire any observable text-change for its formatAt pass,
                    ;; so on-change cannot deliver the tokenized HTML; we MUST
                    ;; capture it via the flush return value here. dom/On is the
                    ;; button's last form, so the button returns the click event
                    ;; for the e/Token below — letting the error render beside the
                    ;; buttons (not inside this blue one).
                    (dom/On "click"
                      (fn [e]
                        (when-let [html (flush-syntax-tokens! @!q-editor)]
                          (reset! !question html))
                        (when-let [html (flush-syntax-tokens! @!a-editor)]
                          (reset! !answer html))
                        (when-let [html (flush-syntax-tokens! @!c-editor)]
                          (reset! !cloze html))
                        e)
                      nil))
                  [t ?error] (e/Token click-event)]
              (when ?error
                (dom/div (dom/props {:style {:order "-1" :margin-right "auto" :color "var(--color-danger)" :font-size "12px"}})
                  (dom/text "Error: " ?error)))
              (when t
                (let [validation-error (when (= kind "cloze") (validate-cloze cloze))]
                  (if validation-error
                    (t validation-error)
                    (let [clean-q (e/server (sanitize-card-field question))
                          clean-a (e/server (sanitize-card-field answer))
                          clean-c (e/server (sanitize-card-field cloze))
                          fields (if (= kind "basic")
                                   {:question clean-q :answer clean-a}
                                   (cond-> {:cloze clean-c}
                                     (and clean-a (not (str/blank? clean-a)))
                                     (assoc :answer clean-a)))
                          result (e/server (cards/update-card card-id fields))]
                      (if (:success result)
                        (do (e/on-unmount #(reset! !editing-card nil))
                            (case (e/server (swap! (us/get-atom user-id :card-mutations) inc))
                              (t)))
                        (t (:error result))))))))
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !editing-card nil)) nil))))))))


(defn insert-flashcards-safe! [rows]
  #?(:clj (try
            (db/insert-flashcards! rows)
            {:success true}
            (catch Exception e
              {:success false :error (.getMessage e)}))
     :cljs nil))

;; Add card modal — uses topic-id and root-topic-id instead of doc-id + page-number
(e/defn AddCardModal [!show-add !card-kind !captured-selection topic-id root-topic-id user-id]
  (e/client
    (let [initial-kind @!card-kind
          initial-text (or @!captured-selection "")
          prefill? (not (str/blank? initial-text))
          init-q (if (and prefill? (= initial-kind "basic")) initial-text "")
          init-c (if (and prefill? (= initial-kind "cloze")) initial-text "")
          kind (e/watch !card-kind)
          !question (atom init-q)
          !answer (atom "")
          !cloze (atom init-c)
          !q-editor (atom nil)
          !a-editor (atom nil)
          !c-editor (atom nil)
          question (e/watch !question)
          answer (e/watch !answer)
          cloze (e/watch !cloze)
          card-font-sz (e/server (settings/get-card-font-size user-id))
          modal-font (str (or card-font-sz 14) "px")
          !primary-btn (atom nil)]
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "transparent" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"
                            :pointer-events "none"}
                    :tabindex "-1"})
        (dom/On "keydown"
          (fn [e]
            #?(:cljs
               (cond
                 (= (.-key e) "Escape")
                 (reset! !show-add false)
                 (and (= (.-key e) "Enter") (or (.-metaKey e) (.-ctrlKey e)))
                 (when-let [btn @!primary-btn]
                   (.preventDefault e)
                   (.click btn)))))
          nil)
        (dom/div
          (dom/props {:class "card-modal-inner"
                      :style {:background "var(--color-bg-card)" :border-radius "var(--radius-lg)" :padding "var(--sp-6)"
                              :box-shadow "0 4px 20px rgba(0,0,0,0.25)"
                              :pointer-events "auto"}})
          (dom/On "pointerdown"
            (fn [e]
              #?(:cljs
                 (let [inner (.-currentTarget e)
                       h3 (.querySelector inner "h3")]
                   (when (and h3 (or (= (.-target e) h3)
                                   (.contains h3 (.-target e))))
                     (.preventDefault e)
                     (let [rect (.getBoundingClientRect inner)
                           sx (.-clientX e)
                           sy (.-clientY e)
                           px (.-left rect)
                           py (.-top rect)
                           move-fn (fn [me]
                                     (set! (.-position (.-style inner)) "fixed")
                                     (set! (.-left (.-style inner))
                                       (str (+ px (- (.-clientX me) sx)) "px"))
                                     (set! (.-top (.-style inner))
                                       (str (+ py (- (.-clientY me) sy)) "px"))
                                     (set! (.-margin (.-style inner)) "0"))
                           up-fn (fn self [ue]
                                   (.releasePointerCapture inner (.-pointerId ue))
                                   (.removeEventListener inner "pointermove" move-fn)
                                   (.removeEventListener inner "pointerup" self))]
                       (.setPointerCapture inner (.-pointerId e))
                       (.addEventListener inner "pointermove" move-fn)
                       (.addEventListener inner "pointerup" up-fn))))))
            nil)
          (dom/h3 (dom/props {:style {:margin-top "0" :cursor "move" :user-select "none"
                                      :padding-bottom "var(--sp-2)" :margin-bottom "var(--sp-3)"
                                      :border-bottom "1px solid var(--color-border)"}})
            (dom/text "Add Card"))
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-3)" :margin-bottom "var(--sp-4)"}})
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-1)" :font-size "14px" :cursor "pointer"}})
              (dom/input
                (dom/props {:type "radio" :name "add-card-kind" :value "basic"
                            :checked (= kind "basic")})
                (dom/On "change" (fn [_] (reset! !card-kind "basic")) nil))
              (dom/text "Basic"))
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-1)" :font-size "14px" :cursor "pointer"}})
              (dom/input
                (dom/props {:type "radio" :name "add-card-kind" :value "cloze"
                            :checked (= kind "cloze")})
                (dom/On "change" (fn [_] (reset! !card-kind "cloze")) nil))
              (dom/text "Cloze")))
          (if (= kind "basic")
            (dom/div
              (dom/label (dom/text "Question:"))
              (dom/div
                (dom/props {:style {:margin-bottom "var(--sp-3)" :font-size modal-font}})
                (QuillField {:value-string init-q
                             :on-change (fn [html] (reset! !question html))
                             :placeholder "Question..."
                             :field-key [:add-q]
                             :!editor-atom !q-editor}))
              (dom/label (dom/text "Answer:"))
              (dom/div
                (dom/props {:style {:font-size modal-font}})
                (QuillField {:value-string ""
                             :on-change (fn [html] (reset! !answer html))
                             :placeholder "Answer..."
                             :field-key [:add-a]
                             :!editor-atom !a-editor})))
            (dom/div
              (dom/label (dom/text "Cloze:"))
              (dom/div
                (dom/props {:style {:margin-bottom "var(--sp-3)" :font-size modal-font}})
                (QuillField {:value-string init-c
                             :on-change (fn [html] (reset! !cloze html))
                             :placeholder "Cloze text with {{c1::deletion}}..."
                             :field-key [:add-c]
                             :!editor-atom !c-editor
                             :cloze? true}))
              (dom/label (dom/text "Back Extra:"))
              (dom/div
                (dom/props {:style {:font-size modal-font}})
                (QuillField {:value-string ""
                             :on-change (fn [html] (reset! !answer html))
                             :placeholder "Optional back extra..."
                             :field-key [:add-be]
                             :!editor-atom !a-editor}))))
          (dom/div
            (dom/props {:style {:display "flex" :justify-content "flex-end" :align-items "center" :gap "var(--sp-2)" :margin-top "var(--sp-4)"}})
            (let [click-event
                  (dom/button
                    (dom/props {:class "btn btn-primary" :style {:border-radius "var(--radius-sm)" :order "1"}})
                    (reset! !primary-btn dom/node)
                    (dom/text "Save")
                    ;; Mirror the EditCardModal Save: force-flush hljs spans and
                    ;; capture the post-flush innerHTML via the return value.
                    ;; The Quill 2.0.3 syntax module does not fire text-change for
                    ;; its formatAt pass, so on-change cannot deliver the
                    ;; tokenized HTML — we MUST reset! atoms directly here.
                    ;; dom/On is the button's last form → button returns the click.
                    (dom/On "click"
                      (fn [e]
                        (when-let [html (flush-syntax-tokens! @!q-editor)]
                          (reset! !question html))
                        (when-let [html (flush-syntax-tokens! @!a-editor)]
                          (reset! !answer html))
                        (when-let [html (flush-syntax-tokens! @!c-editor)]
                          (reset! !cloze html))
                        e)
                      nil))
                  [t ?error] (e/Token click-event)]
              (when ?error
                (dom/div (dom/props {:style {:order "-1" :margin-right "auto" :color "var(--color-danger)" :font-size "12px"}})
                  (dom/text "Error: " ?error)))
              (when t
                (let [validation-error (when (= kind "cloze") (validate-cloze cloze))]
                  (if validation-error
                    (t validation-error)
                    (let [clean-q (e/server (sanitize-card-field question))
                          clean-a (e/server (sanitize-card-field answer))
                          clean-c (e/server (sanitize-card-field cloze))
                          card-data (if (= kind "basic")
                                      [{:q clean-q :a clean-a}]
                                      [(cond-> {:c clean-c}
                                         (and clean-a (not (str/blank? clean-a)))
                                         (assoc :a clean-a))])
                          ;; Route through cards/save-cards so bake-card-html
                          ;; appends pinned <img> tags before insert (P3d).
                          result (e/server (cards/save-cards topic-id root-topic-id kind card-data))]
                      (if (:success result)
                        (do (e/on-unmount #(reset! !show-add false))
                            (case (e/server (swap! (us/get-atom user-id :card-mutations) inc))
                              (t)))
                        (t (:error result))))))))
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show-add false)) nil))))))))

