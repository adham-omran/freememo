(ns electric-starter-app.anki-sync-form
  "Extracted form UI components for Anki sync — kept in a separate namespace
   to stay below the JVM 64KB method limit imposed by Electric v3's e/defn macro."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(e/defn AnkiSyncModelSelect
  "Reusable model dropdown with field mapping hint."
  [label !model models field-hint]
  (e/client
    (let [model (e/watch !model)]
      (dom/div
        (dom/props {:style {:margin-bottom "12px"}})
        (dom/label (dom/props {:style {:font-weight "600" :font-size "13px" :display "block" :margin-bottom "4px"}})
          (dom/text label))
        (dom/select
          (dom/props {:style {:padding "4px 8px" :border "1px solid #ccc" :border-radius "4px"
                              :font-size "14px" :width "100%"}
                      :value (or model "")})
          (e/for [m (e/diff-by {} models)]
            (dom/option (dom/props {:value m}) (dom/text m)))
          (let [v (dom/On "change" (fn [e] (-> e .-target .-value)) nil)]
            (when (some? v) (reset! !model v))))
        (when (seq field-hint)
          (dom/div
            (dom/props {:style {:font-size "12px" :color "#666" :margin-top "4px"}})
            (dom/text field-hint)))))))

(e/defn AnkiSyncOptions
  "Custom header checkbox/input + allow-duplicates checkbox."
  [!allow-dupes !use-header !header-text]
  (e/client
    ;; Custom header
    (dom/div
      (dom/props {:style {:margin-bottom "12px"}})
      (dom/label
        (dom/props {:style {:display "flex" :align-items "center" :gap "8px" :font-size "13px" :margin-bottom "8px"}})
        (dom/input
          (dom/props {:type "checkbox" :checked (e/watch !use-header)})
          (let [v (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)]
            (when (some? v)
              (reset! !use-header v)
              (when-not v (reset! !header-text "")))))
        (dom/text "Add custom header to each card"))
      (when (e/watch !use-header)
        (dom/input
          (dom/props {:type "text"
                      :value (e/watch !header-text)
                      :placeholder "e.g., Chapter 5: Accounting"
                      :style {:width "100%" :padding "8px" :border "1px solid #ccc"
                              :border-radius "4px" :font-size "14px"}})
          (let [v (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
            (when (some? v) (reset! !header-text v))))))
    ;; Allow duplicates
    (dom/div
      (dom/props {:style {:margin-bottom "16px"}})
      (dom/label
        (dom/props {:style {:display "flex" :align-items "center" :gap "6px" :font-size "13px"}})
        (dom/input
          (dom/props {:type "checkbox" :checked (e/watch !allow-dupes)})
          (let [v (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)]
            (when (some? v) (reset! !allow-dupes v))))
        (dom/text "Allow duplicates")))))

(e/defn AnkiSyncForm
  "The connected-state form: scope, deck, model selection, field mapping, custom header."
  [!scope decks !selected-deck models
   !basic-model basic-fields !cloze-model cloze-fields !allow-dupes !use-header !header-text]
  (e/client
    (let [scope (e/watch !scope)
          selected-deck (e/watch !selected-deck)]

      ;; Scope
      (dom/div
        (dom/props {:style {:margin-bottom "12px"}})
        (dom/label (dom/props {:style {:font-weight "600" :font-size "13px" :display "block" :margin-bottom "4px"}})
          (dom/text "Scope"))
        (dom/select
          (dom/props {:style {:padding "4px 8px" :border "1px solid #ccc" :border-radius "4px" :font-size "14px"}
                      :value scope})
          (dom/option (dom/props {:value "Current Page"}) (dom/text "Current Page"))
          (dom/option (dom/props {:value "Entire Doc"}) (dom/text "Entire Document"))
          (let [v (dom/On "change" (fn [e] (-> e .-target .-value)) nil)]
            (when (some? v) (reset! !scope v)))))

      ;; Deck
      (dom/div
        (dom/props {:style {:margin-bottom "12px"}})
        (dom/label (dom/props {:style {:font-weight "600" :font-size "13px" :display "block" :margin-bottom "4px"}})
          (dom/text "Deck"))
        (dom/select
          (dom/props {:style {:padding "4px 8px" :border "1px solid #ccc" :border-radius "4px"
                              :font-size "14px" :width "100%"}
                      :value (or selected-deck "")})
          (e/for [d (e/diff-by {} decks)]
            (dom/option (dom/props {:value d}) (dom/text d)))
          (let [v (dom/On "change" (fn [e] (-> e .-target .-value)) nil)]
            (when (some? v) (reset! !selected-deck v)))))

      ;; Note Type selectors
      (AnkiSyncModelSelect "Note Type (Basic)" !basic-model models
        (when (seq basic-fields)
          (str "question \u2192 " (first basic-fields) ", answer \u2192 " (second basic-fields))))
      (AnkiSyncModelSelect "Note Type (Cloze)" !cloze-model models
        (when (seq cloze-fields)
          (str "cloze \u2192 " (first cloze-fields))))

      ;; Options
      (AnkiSyncOptions !allow-dupes !use-header !header-text))))

(e/defn AnkiSyncStatus
  "Sync status display and action buttons."
  [sync-phase sync-result sync-error !show-modal !sync-phase !sync-result !sync-error !push-pairs !pull-updates]
  (e/client
    ;; Status display
    (when sync-phase
      (dom/div
        (dom/props {:style {:margin-bottom "16px" :padding "12px" :background "#f8f9fa"
                            :border-radius "4px" :font-size "13px"}})
        (cond
          (= sync-phase :pushing)   (dom/text "Pushing cards to Anki...")
          (= sync-phase :pulling)   (dom/text "Pulling edits from Anki...")
          (= sync-phase :recording) (dom/text "Saving to database...")
          (= sync-phase :error)
          (dom/div
            (dom/props {:style {:color "#dc3545"}})
            (dom/text (str "Error: " (or sync-error "Unknown error"))))
          (= sync-phase :done)
          (let [r sync-result
                pairs (or (:pairs r) [])
                updated-count (or (:updated r) 0)
                added-count (max 0 (- (count pairs) updated-count))
                skipped (or (:skipped r) [])
                pull-upds (or (:updates r) [])]
            (dom/div
              (dom/props {:style {:color "#28a745"}})
              (dom/text
                (str "Done! "
                  (cond
                    (seq pull-upds) (str (count pull-upds) " cards updated from Anki")
                    (or (pos? added-count) (pos? updated-count))
                    (str (when (pos? added-count) (str added-count " added"))
                         (when (and (pos? added-count) (pos? updated-count)) ", ")
                         (when (pos? updated-count) (str updated-count " updated"))
                         (when (seq skipped) (str ", " (count skipped) " skipped")))
                    :else "No changes"))))))))

    ;; Action buttons
    (dom/div
      (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "8px" :margin-top "8px"}})
      (dom/button
        (dom/props {:style {:padding "8px 16px" :background "#f8f9fa" :color "#333"
                            :border "1px solid #ccc" :border-radius "4px" :cursor "pointer" :font-size "14px"}})
        (dom/text (if (= sync-phase :done) "Close" "Cancel"))
        (dom/On "click" (fn [_]
                          (reset! !show-modal false)
                          (reset! !sync-phase nil)
                          (reset! !sync-result nil)
                          (reset! !sync-error nil)
                          (reset! !push-pairs nil)
                          (reset! !pull-updates nil))
          nil))
      (when-not (#{:pushing :pulling :recording} sync-phase)
        (dom/button
          (dom/props {:style {:padding "8px 16px" :background "#17a2b8" :color "white" :border "none"
                              :border-radius "4px" :cursor "pointer" :font-size "14px"}})
          (dom/text "Pull from Anki")
          (dom/On "click"
            (fn [_]
              (reset! !sync-phase :pulling)
              (reset! !sync-result nil)
              (reset! !sync-error nil)
              (reset! !pull-updates nil)
              (reset! !push-pairs nil))
            nil)))
      (when-not (#{:pushing :pulling :recording} sync-phase)
        (dom/button
          (dom/props {:style {:padding "8px 16px" :background "#28a745" :color "white" :border "none"
                              :border-radius "4px" :cursor "pointer" :font-size "14px" :font-weight "500"}})
          (dom/text "Push to Anki")
          (dom/On "click"
            (fn [_]
              (reset! !sync-phase :pushing)
              (reset! !sync-result nil)
              (reset! !sync-error nil)
              (reset! !push-pairs nil)
              (reset! !pull-updates nil))
            nil))))))
