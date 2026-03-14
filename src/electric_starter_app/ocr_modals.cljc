(ns electric-starter-app.ocr-modals
  "Extracted modal components for ocr-page — kept separate to stay below JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [electric-starter-app.components :refer [Typeahead]]
   #?(:clj [electric-starter-app.cards :as cards])))

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
;; !show-export: atom bool  selected-doc: int  current-pdf-page: int  user-id: int
(e/defn ExportModal [!show-export selected-doc current-pdf-page user-id]
  (e/client
    (let [!export-scope (atom "Current Page")
          export-scope (e/watch !export-scope)
          !export-kind (atom "Both")
          export-kind (e/watch !export-kind)
          !use-header (atom false)
          use-header (e/watch !use-header)
          !header-text (atom "")
          header-text (e/watch !header-text)]
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "rgba(0,0,0,0.5)" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"}
                    :tabindex "-1"})
        (dom/On "click" (fn [_] (reset! !show-export false)) nil)
        (dom/On "keydown"
          (fn [e]
            #?(:cljs
               (when (= (.-key e) "Escape")
                 (reset! !show-export false))))
          nil)
        (dom/div
          (dom/props {:style {:background "white" :border-radius "8px" :padding "24px"
                              :width "400px" :box-shadow "0 4px 6px rgba(0,0,0,0.1)"}})
          (dom/On "click" (fn [e] (.stopPropagation e)) nil)
          (dom/h3 (dom/props {:style {:margin-top "0" :margin-bottom "20px"}})
            (dom/text "Export Cards"))
          ;; Custom header checkbox
          (dom/div
            (dom/props {:style {:margin-bottom "16px"}})
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "8px" :margin-bottom "8px"}})
              (dom/input (dom/props {:type "checkbox" :checked use-header})
                (let [v (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)]
                  (when (some? v) (reset! !use-header v))))
              (dom/text "Add custom header to each card"))
            (when use-header
              (dom/input
                (dom/props {:type "text" :value header-text :placeholder "e.g., Chapter 5: Accounting"
                            :style {:width "100%" :padding "8px" :border "1px solid #ccc"
                                    :border-radius "4px" :font-size "14px"}})
                (let [v (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                  (when (some? v) (reset! !header-text v))))))
          ;; Scope select
          (dom/div
            (dom/props {:style {:margin-bottom "16px"}})
            (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500"}})
              (dom/text "Scope:"))
            (dom/select
              (dom/props {:style {:width "100%" :padding "8px" :border "1px solid #ccc"
                                  :border-radius "4px" :font-size "14px"}})
              (dom/option (dom/props {:value "Current Page"}) (dom/text "Current Page"))
              (dom/option (dom/props {:value "Entire Doc"}) (dom/text "Entire Document"))
              (let [v (dom/On "change" (fn [e] (-> e .-target .-value)) nil)]
                (when (some? v) (reset! !export-scope v)))))
          ;; Card type select
          (dom/div
            (dom/props {:style {:margin-bottom "24px"}})
            (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500"}})
              (dom/text "Card Type:"))
            (dom/select
              (dom/props {:style {:width "100%" :padding "8px" :border "1px solid #ccc"
                                  :border-radius "4px" :font-size "14px"}})
              (dom/option (dom/props {:value "Both"}) (dom/text "Both (Basic + Cloze)"))
              (dom/option (dom/props {:value "Basic"}) (dom/text "Basic Only"))
              (dom/option (dom/props {:value "Cloze"}) (dom/text "Cloze Only"))
              (let [v (dom/On "change" (fn [e] (-> e .-target .-value)) nil)]
                (when (some? v) (reset! !export-kind v)))))
          ;; Buttons
          (dom/div
            (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "12px"}})
            (dom/button
              (dom/props {:style {:padding "8px 16px" :background "#f8f9fa" :color "#333"
                                  :border "1px solid #ccc" :border-radius "4px" :cursor "pointer" :font-size "14px"}})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show-export false)) nil))
            (dom/button
              (dom/props {:style {:padding "8px 16px" :background "#2563eb" :color "white" :border "none"
                                  :border-radius "4px" :cursor "pointer" :font-size "14px" :font-weight "500"}})
              (dom/text "Export")
              (let [click-event (dom/On "click" identity nil)
                    [?token ?error] (e/Token click-event)]
                (dom/props {:disabled (some? ?token)
                            :style {:padding "8px 16px"
                                    :background (if (some? ?token) "#999" "#2563eb")
                                    :color "white" :border "none" :border-radius "4px"
                                    :cursor (if (some? ?token) "not-allowed" "pointer")
                                    :font-size "14px" :font-weight "500"}})
                (when ?error
                  (dom/div (dom/props {:style {:color "red" :font-size "12px" :margin-top "8px"}})
                    (dom/text "Error: " ?error)))
                (when-some [token ?token]
                  (let [export-opts {:document-id selected-doc
                                     :page-number (when (= export-scope "Current Page") current-pdf-page)
                                     :header-text (when use-header header-text)
                                     :user-id user-id}]
                    (if (= export-kind "Both")
                      (let [basic-result (e/server (cards/export-cards-csv (assoc export-opts :kind "basic")))
                            cloze-result (e/server (cards/export-cards-csv (assoc export-opts :kind "cloze")))]
                        (let [any-success? (or (:success basic-result) (:success cloze-result))]
                          #?(:cljs
                             (do
                               (when (:success basic-result)
                                 (trigger-download! (:filename basic-result) (:csv basic-result)))
                               (when (:success cloze-result)
                                 (trigger-download! (:filename cloze-result) (:csv cloze-result)))))
                          (if any-success?
                            (do (reset! !show-export false) (token))
                            (token (str "Export failed: " (or (:error basic-result) (:error cloze-result)))))))
                      (let [export-result (e/server (cards/export-cards-csv
                                                      (assoc export-opts :kind (str/lower-case export-kind))))]
                        (if (:success export-result)
                          (do
                            #?(:cljs (trigger-download! (:filename export-result) (:csv export-result)))
                            (reset! !show-export false)
                            (token))
                          (token (:error export-result)))))))))))))))

;; Pre-prompt dialog
;; state map keys: :!show :!prompt-gen-state :!pre-prompt :!prompt-history
;;                 :!history-save-trigger :captured-selection :prompt-dialog-kind
(e/defn PromptDialog [state]
  (e/client
    (let [{:keys [!show !prompt-gen-state !pre-prompt !prompt-history
                  !history-save-trigger captured-selection prompt-dialog-kind]} state
          pre-prompt-value (e/watch !pre-prompt)
          !local-prompt (atom pre-prompt-value)
          local-prompt (e/watch !local-prompt)]
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "rgba(0,0,0,0.5)" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"}
                    :tabindex "-1"})
        (dom/On "click" (fn [_] (reset! !show false)) nil)
        (dom/On "keydown"
          (fn [e]
            #?(:cljs
               (when (= (.-key e) "Escape")
                 (reset! !show false))))
          nil)
        (dom/div
          (dom/props {:style {:background "white" :border-radius "8px" :padding "24px"
                              :width "600px" :max-width "90%" :box-shadow "0 4px 6px rgba(0,0,0,0.1)"}})
          (dom/On "click" (fn [e] (.stopPropagation e)) nil)
          (dom/h3
            (dom/props {:style {:margin-top "0" :margin-bottom "20px" :font-size "18px"}})
            (dom/text "Generate " (if (= prompt-dialog-kind "basic") "Basic" "Cloze") " Cards"))
          (dom/div
            (dom/props {:style {:margin-bottom "20px"}})
            (dom/label
              (dom/props {:style {:display "block" :margin-bottom "8px" :font-size "14px"}})
              (dom/text "Pre-prompt (will be added to the system prompt):"))
            (let [prompt-history (e/watch !prompt-history)]
              (Typeahead !local-prompt (take 20 prompt-history) "e.g., Focus on accounting terminology..." nil)))
          (dom/div
            (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "12px"}})
            (dom/button
              (dom/props {:style {:padding "8px 16px" :background "#f8f9fa" :color "#333"
                                  :border "1px solid #ccc" :border-radius "4px" :cursor "pointer"
                                  :font-size "15px"}})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil))
            (dom/button
              (dom/props {:style {:padding "8px 16px" :background "#2563eb"
                                  :color "white" :border "none" :border-radius "4px"
                                  :cursor "pointer" :font-size "15px" :font-weight "500"}})
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
                  (swap! !prompt-gen-state update :queue conj
                    {:id        (str (random-uuid))
                     :selection captured-selection
                     :pre-prompt local-prompt
                     :kind      prompt-dialog-kind})
                  (reset! !show false))
                nil))))))))

;; Edit card modal
(e/defn EditCardModal [!editing-card !refresh]
  (e/client
    (let [editing-card (e/watch !editing-card)
          _ (println "MODAL editing-card:" (pr-str editing-card))
          card-id (:id editing-card)
          kind (:kind editing-card)
          init-q (or (:question editing-card) "")
          init-a (or (:answer editing-card) "")
          init-c (or (:cloze editing-card) "")
          _ (println "MODAL init values q:" (pr-str init-q) "a:" (pr-str init-a) "c:" (pr-str init-c))
          !question (atom init-q)
          !answer (atom init-a)
          !cloze (atom init-c)
          question (e/watch !question)
          answer (e/watch !answer)
          cloze (e/watch !cloze)
          _ (println "MODAL watched values q:" (pr-str question) "a:" (pr-str answer) "c:" (pr-str cloze))]
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "transparent" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"
                            :pointer-events "none"}
                    :tabindex "-1"})
        (dom/On "keydown"
          (fn [e]
            #?(:cljs
               (when (= (.-key e) "Escape")
                 (reset! !editing-card nil))))
          nil)
        (dom/div
          (dom/props {:style {:background "white" :border-radius "8px" :padding "24px"
                              :width "460px" :box-shadow "0 4px 20px rgba(0,0,0,0.25)"
                              :pointer-events "auto"}})
          (dom/On "pointerdown"
            (fn [e]
              #?(:cljs
                 (let [inner  (.-currentTarget e)
                       h3     (.querySelector inner "h3")]
                   (when (and h3 (or (= (.-target e) h3)
                                     (.contains h3 (.-target e))))
                     (.preventDefault e)
                     (let [rect    (.getBoundingClientRect inner)
                           sx      (.-clientX e)
                           sy      (.-clientY e)
                           px      (.-left rect)
                           py      (.-top rect)
                           move-fn (fn [me]
                                     (set! (.-position (.-style inner)) "fixed")
                                     (set! (.-left (.-style inner))
                                           (str (+ px (- (.-clientX me) sx)) "px"))
                                     (set! (.-top (.-style inner))
                                           (str (+ py (- (.-clientY me) sy)) "px"))
                                     (set! (.-margin (.-style inner)) "0"))
                           up-fn   (fn self [ue]
                                     (.releasePointerCapture inner (.-pointerId ue))
                                     (.removeEventListener inner "pointermove" move-fn)
                                     (.removeEventListener inner "pointerup" self))]
                       (.setPointerCapture inner (.-pointerId e))
                       (.addEventListener inner "pointermove" move-fn)
                       (.addEventListener inner "pointerup" up-fn))))))
            nil)
          (dom/h3 (dom/props {:style {:margin-top "0" :cursor "move" :user-select "none"
                                      :padding-bottom "8px" :margin-bottom "12px"
                                      :border-bottom "1px solid #ddd"}})
            (dom/text "Edit " (if (= kind "basic") "Basic" "Cloze") " Card"))
          (if (= kind "basic")
            (dom/div
              (dom/label (dom/text "Question:"))
              (dom/textarea
                (dom/props {:value question :rows 3
                            :style {:width "100%" :padding "8px" :margin-bottom "12px"
                                    :font-family "system-ui, -apple-system, sans-serif" :font-size "14px"}})
                (let [ev (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                  (when (some? ev) (reset! !question ev))))
              (dom/label (dom/text "Answer:"))
              (dom/textarea
                (dom/props {:value answer :rows 3
                            :style {:width "100%" :padding "8px"
                                    :font-family "system-ui, -apple-system, sans-serif" :font-size "14px"}})
                (let [ev (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                  (when (some? ev) (reset! !answer ev)))))
            (dom/div
              (dom/label (dom/text "Cloze:"))
              (dom/textarea
                (dom/props {:value cloze :rows 4
                            :style {:width "100%" :padding "8px"
                                    :font-family "system-ui, -apple-system, sans-serif" :font-size "14px"}})
                (let [ev (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                  (when (some? ev) (reset! !cloze ev))))))
          (dom/div
            (dom/props {:style {:display "flex" :gap "8px" :margin-top "16px"}})
            (dom/button
              (dom/props {:style {:padding "8px 16px"}})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !editing-card nil)) nil))
            (dom/button
              (dom/props {:style {:padding "8px 16px" :background "#2563eb" :color "white" :border "none"}})
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
                        (e/server (swap! !refresh inc))
                        (reset! !editing-card nil)
                        (token))
                      (token (:error result)))))))))))))

;; Add card modal
(e/defn AddCardModal [!show-add card-type doc-id page-number !refresh content-item-id source-reference]
  (e/client
    (let [!kind (atom card-type)
          kind (e/watch !kind)
          !question (atom "")
          !answer (atom "")
          !cloze (atom "")
          question (e/watch !question)
          answer (e/watch !answer)
          cloze (e/watch !cloze)]
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "transparent" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"
                            :pointer-events "none"}
                    :tabindex "-1"})
        (dom/On "keydown"
          (fn [e]
            #?(:cljs
               (when (= (.-key e) "Escape")
                 (reset! !show-add false))))
          nil)
        (dom/div
          (dom/props {:style {:background "white" :border-radius "8px" :padding "24px"
                              :width "460px" :box-shadow "0 4px 20px rgba(0,0,0,0.25)"
                              :pointer-events "auto"}})
          (dom/On "pointerdown"
            (fn [e]
              #?(:cljs
                 (let [inner  (.-currentTarget e)
                       h3     (.querySelector inner "h3")]
                   (when (and h3 (or (= (.-target e) h3)
                                     (.contains h3 (.-target e))))
                     (.preventDefault e)
                     (let [rect    (.getBoundingClientRect inner)
                           sx      (.-clientX e)
                           sy      (.-clientY e)
                           px      (.-left rect)
                           py      (.-top rect)
                           move-fn (fn [me]
                                     (set! (.-position (.-style inner)) "fixed")
                                     (set! (.-left (.-style inner))
                                           (str (+ px (- (.-clientX me) sx)) "px"))
                                     (set! (.-top (.-style inner))
                                           (str (+ py (- (.-clientY me) sy)) "px"))
                                     (set! (.-margin (.-style inner)) "0"))
                           up-fn   (fn self [ue]
                                     (.releasePointerCapture inner (.-pointerId ue))
                                     (.removeEventListener inner "pointermove" move-fn)
                                     (.removeEventListener inner "pointerup" self))]
                       (.setPointerCapture inner (.-pointerId e))
                       (.addEventListener inner "pointermove" move-fn)
                       (.addEventListener inner "pointerup" up-fn))))))
            nil)
          (dom/h3 (dom/props {:style {:margin-top "0" :cursor "move" :user-select "none"
                                      :padding-bottom "8px" :margin-bottom "12px"
                                      :border-bottom "1px solid #ddd"}})
            (dom/text "Add Card"))
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center" :gap "12px" :margin-bottom "16px"}})
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "14px" :cursor "pointer"}})
              (dom/input
                (dom/props {:type "radio" :name "add-card-kind" :value "basic"
                            :checked (= kind "basic")})
                (dom/On "change" (fn [_] (reset! !kind "basic")) nil))
              (dom/text "Basic"))
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "14px" :cursor "pointer"}})
              (dom/input
                (dom/props {:type "radio" :name "add-card-kind" :value "cloze"
                            :checked (= kind "cloze")})
                (dom/On "change" (fn [_] (reset! !kind "cloze")) nil))
              (dom/text "Cloze")))
          (if (= kind "basic")
            (dom/div
              (dom/label (dom/text "Question:"))
              (dom/textarea
                (dom/props {:value question :rows 3
                            :style {:width "100%" :padding "8px" :margin-bottom "12px"
                                    :font-family "system-ui, -apple-system, sans-serif" :font-size "14px"}})
                (let [ev (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                  (when (some? ev) (reset! !question ev))))
              (dom/label (dom/text "Answer:"))
              (dom/textarea
                (dom/props {:value answer :rows 3
                            :style {:width "100%" :padding "8px"
                                    :font-family "system-ui, -apple-system, sans-serif" :font-size "14px"}})
                (let [ev (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                  (when (some? ev) (reset! !answer ev)))))
            (dom/div
              (dom/label (dom/text "Cloze:"))
              (dom/textarea
                (dom/props {:value cloze :rows 4
                            :style {:width "100%" :padding "8px"
                                    :font-family "system-ui, -apple-system, sans-serif" :font-size "14px"}})
                (let [ev (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                  (when (some? ev) (reset! !cloze ev))))))
          (dom/div
            (dom/props {:style {:display "flex" :gap "8px" :margin-top "16px"}})
            (dom/button
              (dom/props {:style {:padding "8px 16px"}})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show-add false)) nil))
            (dom/button
              (dom/props {:style {:padding "8px 16px" :background "#2563eb" :color "white" :border "none"
                                  :border-radius "4px"}})
              (dom/text "Save")
              (let [click-event (dom/On "click" identity nil)
                    [?token ?error] (e/Token click-event)]
                (when ?error
                  (dom/div (dom/props {:style {:color "red" :font-size "12px" :margin-top "8px"}})
                    (dom/text "Error: " ?error)))
                (when-some [token ?token]
                  (let [fields (if (= kind "basic")
                                {:question question :answer answer}
                                {:cloze cloze})
                        result (e/server (cards/add-card doc-id page-number kind fields content-item-id source-reference))]
                    (if (:success result)
                      (do
                        (e/server (swap! !refresh inc))
                        (reset! !show-add false)
                        (token))
                      (token (:error result)))))))))))))
