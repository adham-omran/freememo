(ns freememo.card-modals
  "Extracted modal components for ocr-page — kept separate to stay below JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.typeahead :refer [Typeahead]]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.cards :as cards])
   #?(:clj [freememo.settings :as settings])))

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

;; Export modal
;; !show-export: atom bool  topic-id: current topic  root-topic-id: root topic  user-id: int
(e/defn ExportModal [!show-export topic-id root-topic-id user-id]
  (e/client
    (let [!export-scope (atom "Current Page")
          export-scope (e/watch !export-scope)
          !export-kind (atom "Both")
          export-kind (e/watch !export-kind)
          !use-header (atom false)
          use-header (e/watch !use-header)
          !header-text (atom "")
          header-text (e/watch !header-text)
          !primary-btn (atom nil)]
      (dom/div
        (dom/props {:class "modal-backdrop" :tabindex "-1"})
        (dom/On "click" (fn [_] (reset! !show-export false)) nil)
        (dom/On "keydown"
          (fn [e]
            #?(:cljs
               (cond
                 (= (.-key e) "Escape")
                 (reset! !show-export false)
                 (and (= (.-key e) "Enter") (or (.-metaKey e) (.-ctrlKey e)))
                 (when-let [btn @!primary-btn]
                   (.preventDefault e)
                   (.click btn)))))
          nil)
        (dom/div
          (dom/props {:class "modal-content modal-sm"})
          (dom/On "click" (fn [e] (.stopPropagation e)) nil)
          (dom/h3 (dom/props {:style {:margin-top "0" :margin-bottom "20px"}})
            (dom/text "Export Cards"))
          ;; Custom header checkbox
          (dom/div
            (dom/props {:style {:margin-bottom "var(--sp-4)"}})
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-2)" :margin-bottom "var(--sp-2)"}})
              (dom/input (dom/props {:type "checkbox" :checked use-header})
                (let [v (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)]
                  (when (some? v) (reset! !use-header v))))
              (dom/text "Add custom header to each card"))
            (when use-header
              (dom/input
                (dom/props {:type "text" :value header-text :placeholder "e.g., Chapter 5: Accounting"
                            :class "input input-full" :style {:padding "var(--sp-2)"}})
                (let [v (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                  (when (some? v) (reset! !header-text v))))))
          ;; Scope select
          (dom/div
            (dom/props {:style {:margin-bottom "var(--sp-4)"}})
            (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500"}})
              (dom/text "Scope:"))
            (dom/select
              (dom/props {:class "select" :style {:width "100%"}})
              (dom/option (dom/props {:value "Current Page"}) (dom/text "Current Page"))
              (dom/option (dom/props {:value "Entire Doc"}) (dom/text "Entire Document"))
              (let [v (dom/On "change" (fn [e] (-> e .-target .-value)) nil)]
                (when (some? v) (reset! !export-scope v)))))
          ;; Card type select
          (dom/div
            (dom/props {:style {:margin-bottom "var(--sp-6)"}})
            (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500"}})
              (dom/text "Card Type:"))
            (dom/select
              (dom/props {:class "select" :style {:width "100%"}})
              (dom/option (dom/props {:value "Both"}) (dom/text "Both (Basic + Cloze)"))
              (dom/option (dom/props {:value "Basic"}) (dom/text "Basic Only"))
              (dom/option (dom/props {:value "Cloze"}) (dom/text "Cloze Only"))
              (let [v (dom/On "change" (fn [e] (-> e .-target .-value)) nil)]
                (when (some? v) (reset! !export-kind v)))))
          ;; Buttons
          (dom/div
            (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "var(--sp-3)"}})
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:order "1"}})
              (reset! !primary-btn dom/node)
              (dom/text "Export")
              (let [click-event (dom/On "click" identity nil)
                    [?token ?error] (e/Token click-event)]
                (dom/props {:disabled (some? ?token)
                            :class "btn btn-primary"})
                (when ?error
                  (dom/div (dom/props {:style {:color "var(--color-danger)" :font-size "12px" :margin-top "var(--sp-2)"}})
                    (dom/text "Error: " ?error)))
                (when-some [token ?token]
                  (let [export-opts {:topic-id topic-id
                                     :root-topic-id root-topic-id
                                     :scope export-scope
                                     :header-text (when use-header header-text)
                                     :user-id user-id}]
                    (if (= export-kind "Both")
                      (let [basic-result (e/server (cards/export-cards-csv (assoc export-opts :kind "basic")))
                            cloze-result (e/server (cards/export-cards-csv (assoc export-opts :kind "cloze")))]
                        (let [any-success? (or (:success basic-result) (:success cloze-result))]
                          (when (:success basic-result)
                            (trigger-download! (:filename basic-result) (:csv basic-result)))
                          (when (:success cloze-result)
                            (trigger-download! (:filename cloze-result) (:csv cloze-result)))
                          (if any-success?
                            (do (reset! !show-export false) (token))
                            (token (str "Export failed: " (or (:error basic-result) (:error cloze-result)))))))
                      (let [export-result (e/server (cards/export-cards-csv
                                                      (assoc export-opts :kind (str/lower-case export-kind))))]
                        (if (:success export-result)
                          (do
                            (trigger-download! (:filename export-result) (:csv export-result))
                            (reset! !show-export false)
                            (token))
                          (token (:error export-result)))))))))
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show-export false)) nil))))))))

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
          (dom/props {:style {:background "var(--color-bg-card)" :border-radius "var(--radius-lg)" :padding "var(--sp-6)"
                              :width "460px" :box-shadow "0 4px 20px rgba(0,0,0,0.25)"
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
              (dom/textarea
                (dom/props {:dir "auto" :value question :rows 3
                            :style {:width "100%" :padding "var(--sp-2)" :margin-bottom "var(--sp-3)"
                                    :font-family "system-ui, -apple-system, sans-serif" :font-size modal-font}})
                (let [focus-node dom/node]
                  (js/setTimeout (fn [] (.focus focus-node)) 50))
                (let [ev (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                  (when (some? ev) (reset! !question ev))))
              (dom/label (dom/text "Answer:"))
              (dom/textarea
                (dom/props {:dir "auto" :value answer :rows 3
                            :style {:width "100%" :padding "var(--sp-2)"
                                    :font-family "system-ui, -apple-system, sans-serif" :font-size modal-font}})
                (let [ev (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                  (when (some? ev) (reset! !answer ev)))))
            (dom/div
              (dom/label (dom/text "Cloze:"))
              (dom/textarea
                (dom/props {:dir "auto" :value cloze :rows 4
                            :style {:width "100%" :padding "var(--sp-2)"
                                    :font-family "system-ui, -apple-system, sans-serif" :font-size modal-font}})
                (let [focus-node dom/node]
                  (js/setTimeout (fn [] (.focus focus-node)) 50))
                (let [ev (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                  (when (some? ev) (reset! !cloze ev))))))
          (dom/div
            (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "var(--sp-2)" :margin-top "var(--sp-4)"}})
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:order "1"}})
              (reset! !primary-btn dom/node)
              (dom/text "Save")
              (let [click-event (dom/On "click" identity nil)
                    [?token ?error] (e/Token click-event)]
                (when-some [token ?token]
                  (let [fields (if (= kind "basic")
                                 {:question question :answer answer}
                                 {:cloze cloze})
                        result (e/server (cards/update-card card-id fields))]
                    (if (:success result)
                      (do
                        (e/server (swap! (us/get-atom user-id :card-mutations) inc))
                        (reset! !editing-card nil)
                        (token))
                      (token (:error result)))))))
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
(e/defn AddCardModal [!show-add card-type topic-id root-topic-id source-reference user-id]
  (e/client
    (let [!kind (atom card-type)
          kind (e/watch !kind)
          !question (atom "")
          !answer (atom "")
          !cloze (atom "")
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
          (dom/props {:style {:background "var(--color-bg-card)" :border-radius "var(--radius-lg)" :padding "var(--sp-6)"
                              :width "460px" :box-shadow "0 4px 20px rgba(0,0,0,0.25)"
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
                (dom/On "change" (fn [_] (reset! !kind "basic")) nil))
              (dom/text "Basic"))
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-1)" :font-size "14px" :cursor "pointer"}})
              (dom/input
                (dom/props {:type "radio" :name "add-card-kind" :value "cloze"
                            :checked (= kind "cloze")})
                (dom/On "change" (fn [_] (reset! !kind "cloze")) nil))
              (dom/text "Cloze")))
          (if (= kind "basic")
            (dom/div
              (dom/label (dom/text "Question:"))
              (dom/textarea
                (dom/props {:dir "auto" :value question :rows 3
                            :style {:width "100%" :padding "var(--sp-2)" :margin-bottom "var(--sp-3)"
                                    :font-family "system-ui, -apple-system, sans-serif" :font-size modal-font}})
                (let [focus-node dom/node]
                  (js/setTimeout (fn [] (.focus focus-node)) 50))
                (let [ev (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                  (when (some? ev) (reset! !question ev))))
              (dom/label (dom/text "Answer:"))
              (dom/textarea
                (dom/props {:dir "auto" :value answer :rows 3
                            :style {:width "100%" :padding "var(--sp-2)"
                                    :font-family "system-ui, -apple-system, sans-serif" :font-size modal-font}})
                (let [ev (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                  (when (some? ev) (reset! !answer ev)))))
            (dom/div
              (dom/label (dom/text "Cloze:"))
              (dom/textarea
                (dom/props {:dir "auto" :value cloze :rows 4
                            :style {:width "100%" :padding "var(--sp-2)"
                                    :font-family "system-ui, -apple-system, sans-serif" :font-size modal-font}})
                (let [focus-node dom/node]
                  (js/setTimeout (fn [] (.focus focus-node)) 50))
                (let [ev (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                  (when (some? ev) (reset! !cloze ev))))))
          (dom/div
            (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "var(--sp-2)" :margin-top "var(--sp-4)"}})
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:border-radius "var(--radius-sm)" :order "1"}})
              (reset! !primary-btn dom/node)
              (dom/text "Save")
              (let [click-event (dom/On "click" identity nil)
                    [?token ?error] (e/Token click-event)]
                (when ?error
                  (dom/div (dom/props {:style {:color "var(--color-danger)" :font-size "12px" :margin-top "var(--sp-2)"}})
                    (dom/text "Error: " ?error)))
                (when-some [token ?token]
                  (let [card-data (if (= kind "basic")
                                    [{:q question :a answer}]
                                    [{:c cloze}])
                        rows (e/server
                               (mapv (fn [card]
                                       (cond-> {:topic_id topic-id
                                                :root_topic_id root-topic-id
                                                :kind kind}
                                         (= kind "basic") (assoc :question (:q card) :answer (:a card))
                                         (= kind "cloze") (assoc :cloze (:c card))
                                         source-reference (assoc :source_reference source-reference)))
                                 card-data))
                        result (e/server (insert-flashcards-safe! rows))]
                    (if (:success result)
                      (do
                        (e/server (swap! (us/get-atom user-id :card-mutations) inc))
                        (reset! !show-add false)
                        (token))
                      (token (:error result)))))))
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show-add false)) nil))))))))

