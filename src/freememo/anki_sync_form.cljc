(ns freememo.anki-sync-form
  "Extracted form UI components for Anki sync — kept in a separate namespace
   to stay below the JVM 64KB method limit imposed by Electric v3's e/defn macro."
  (:require
   [clojure.string :as string]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.typeahead :refer [Typeahead]]
   #?(:clj [freememo.settings :as settings])))

(e/defn AnkiSyncModelSelect
  "Reusable model typeahead with field mapping hint.
   fields = :loading | [] | vector of field names. format-hint = fn from non-empty vector to string."
  [label !model models fields format-hint]
  (e/client
    (dom/div
      (dom/props {:style {:margin-bottom "var(--sp-3)"}})
      (dom/label (dom/props {:style {:font-weight "600" :font-size "14px" :display "block" :margin-bottom "4px"}})
        (dom/text label))
      (Typeahead !model models "Start typing..." nil nil)
      (if (= fields :loading)
        (dom/div
          (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)" :margin-top "var(--sp-1)"}})
          (dom/span (dom/props {:class "spinner"}))
          (dom/text "Loading fields..."))
        (when (and (coll? fields) (seq fields))
          (dom/div
            (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)" :margin-top "var(--sp-1)"}})
            (dom/text (format-hint fields))))))))

(e/defn TagInput
  "Multi-tag input with chip display and autocomplete from all-tags."
  [!tags all-tags]
  (e/client
    (let [tags (e/watch !tags)
          !search (atom "")
          search (e/watch !search)
          !focused (atom false)
          focused (e/watch !focused)
          !active-idx (atom -1)
          active-idx (e/watch !active-idx)
          filtered (when (and focused (seq search))
                     (->> all-tags
                       (filter (fn [t]
                                 (and (string/includes?
                                        (string/lower-case t)
                                        (string/lower-case search))
                                   (not (some #{t} tags)))))
                       (take 5)
                       vec))
          add-tag! (fn [t]
                     (when-not (some #{t} @!tags)
                       (swap! !tags conj t))
                     (reset! !search "")
                     (reset! !active-idx -1))]
      (dom/div
        (dom/props {:style {:position "relative"}})
        ;; Input row with chips
        (dom/div
          (dom/props {:style {:display "flex" :flex-wrap "wrap" :align-items "center"
                              :gap "var(--sp-1)" :padding "var(--sp-1) 6px"
                              :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"
                              :font-size "14px" :min-height "30px" :background "var(--color-bg-card)"}})
          ;; Chips
          (e/for [t (e/diff-by {} tags)]
            (dom/span
              (dom/props {:style {:display "inline-flex" :align-items "center" :gap "3px"
                                  :background "var(--color-border)" :border-radius "3px"
                                  :padding "1px 5px" :font-size "13px"}})
              (dom/text t)
              (dom/button
                (dom/props {:type "button"
                            :style {:background "none" :border "none" :cursor "pointer"
                                    :padding "0" :font-size "13px" :line-height "1"
                                    :color "var(--color-text-secondary)"}})
                (dom/text "\u00d7")
                (dom/On "click" (fn [_] (swap! !tags (fn [ts] (vec (remove #{t} ts))))) nil))))
          ;; Text input
          (dom/input
            (dom/props {:type "text"
                        :value search
                        :placeholder (if (seq tags) "" "Add tags...")
                        :style {:border "none" :outline "none" :font-size "14px"
                                :flex "1" :min-width "80px" :padding "2px"}})
            (dom/On "focus" (fn [_] (reset! !focused true)
                              (reset! !active-idx -1)) nil)
            (dom/On "blur"
              (fn [_]
                (reset! !focused false)
                (reset! !active-idx -1)
                (let [val (string/trim @!search)]
                  (when (seq val)
                    (when-not (some #{val} @!tags)
                      (swap! !tags conj val))
                    (reset! !search ""))))
              nil)
            (let [v (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
              (when (some? v)
                (reset! !search v)
                (reset! !active-idx -1)))
            (dom/On "keydown"
              (fn [e]
                (let [key (.-key e)
                      val (string/trim search)
                      n (count filtered)]
                  (cond
                    (= key "ArrowDown")
                    (do (.preventDefault e)
                      (reset! !active-idx (mod (inc active-idx) n)))
                    (= key "ArrowUp")
                    (do (.preventDefault e)
                      (reset! !active-idx (mod (dec active-idx) n)))
                    (and (= key "Enter") (>= active-idx 0))
                    (do (.preventDefault e)
                      (add-tag! (nth filtered active-idx)))
                    (and (= key " ") (seq val))
                    (do (.preventDefault e)
                      (add-tag! val))
                    (and (= key "Enter") (seq val))
                    (do (.preventDefault e)
                      (add-tag! val))
                    (and (= key "Escape") (seq filtered))
                    (do (.preventDefault e)
                      (reset! !active-idx -1)
                      (reset! !focused false))
                    (and (= key "Backspace") (empty? search) (seq tags))
                    (swap! !tags (fn [ts] (vec (butlast ts)))))))
              nil)))
        ;; Dropdown
        (when (seq filtered)
          (dom/div
            (dom/props {:style {:position "absolute" :top "100%" :left "0" :right "0"
                                :background "var(--color-bg-card)" :border "1px solid var(--color-border)"
                                :border-radius "var(--radius-sm)" :z-index "100"
                                :box-shadow "0 2px 4px rgba(0,0,0,0.15)"}})
            (e/for [[i t] (e/diff-by {} (map-indexed vector filtered))]
              (dom/div
                (dom/props {:style {:padding "5px 8px" :cursor "pointer" :font-size "14px"
                                    :background (cond
                                                  (= i active-idx) "var(--color-highlight)"
                                                  (odd? i) "var(--color-bg-subtle)"
                                                  :else "var(--color-bg-card)")}})
                (dom/text t)
                (dom/On "mousemove" (fn [_] (reset! !active-idx i)) nil)
                (dom/On "mousedown"
                  (fn [e]
                    (.preventDefault e)
                    (add-tag! t))
                  nil)))))))))

(e/defn AnkiSyncOptions
  "Custom header checkbox/input + allow-duplicates checkbox + tags.
   conn = {:!all-tags ...}  form = {:!allow-dupes :!use-header :!header-text :!use-tags :!tags ...}"
  [conn form]
  (e/client
    (let [all-tags (e/watch (:!all-tags conn))]
      ;; Custom header
      (dom/div
        (dom/props {:style {:margin-bottom "var(--sp-3)"}})
        (dom/label
          (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-2)" :font-size "14px" :margin-bottom "var(--sp-2)"}})
          (dom/input
            (dom/props {:type "checkbox" :checked (e/watch (:!use-header form))})
            (let [v (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)]
              (when (some? v)
                (reset! (:!use-header form) v)
                (when-not v (reset! (:!header-text form) "")))))
          (dom/text "Add custom header to each card"))
        (when (e/watch (:!use-header form))
          (dom/input
            (dom/props {:type "text"
                        :value (e/watch (:!header-text form))
                        :placeholder "e.g., Chapter 5: Accounting"
                        :class "input input-full" :style {:font-size "15px"}})
            (let [v (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
              (when (some? v) (reset! (:!header-text form) v))))))
      ;; Allow duplicates
      (dom/div
        (dom/props {:style {:margin-bottom "var(--sp-3)"}})
        (dom/label
          (dom/props {:style {:display "flex" :align-items "center" :gap "6px" :font-size "14px"}})
          (dom/input
            (dom/props {:type "checkbox" :checked (e/watch (:!allow-dupes form))})
            (let [v (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)]
              (when (some? v) (reset! (:!allow-dupes form) v))))
          (dom/text "Allow duplicates")))
      ;; Tags
      (dom/div
        (dom/props {:style {:margin-bottom "var(--sp-4)"}})
        (dom/label
          (dom/props {:style {:display "flex" :align-items "center" :gap "6px" :font-size "14px" :margin-bottom "6px"}})
          (dom/input
            (dom/props {:type "checkbox" :checked (e/watch (:!use-tags form))})
            (let [v (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)]
              (when (some? v) (reset! (:!use-tags form) v))))
          (dom/text "Tags"))
        (when (e/watch (:!use-tags form))
          (TagInput (:!tags form) all-tags))))))

(e/defn AnkiSyncForm
  "The connected-state form: scope, deck, model selection, field mapping, custom header, tags.
   conn = {:!decks :!models :!selected-deck :!basic-model :!cloze-model :!all-tags ...}
   form = {:!scope :!basic-fields :!cloze-fields ...}"
  [user-id conn form]
  (e/client
    (let [scope (e/watch (:!scope form))
          decks (e/watch (:!decks conn))
          models (e/watch (:!models conn))
          basic-fields (e/watch (:!basic-fields form))
          cloze-fields (e/watch (:!cloze-fields form))]

      ;; Scope
      (dom/div
        (dom/props {:style {:margin-bottom "var(--sp-3)"}})
        (dom/label (dom/props {:style {:font-weight "600" :font-size "14px" :display "block" :margin-bottom "4px"}})
          (dom/text "Scope"))
        (dom/select
          (dom/props {:class "select" :style {:font-size "15px"}
                      :value scope})
          (dom/option (dom/props {:value "Current Page"}) (dom/text "Current Page"))
          (dom/option (dom/props {:value "Entire Doc"}) (dom/text "Entire Document"))
          (let [v (dom/On "change" (fn [e] (-> e .-target .-value)) nil)]
            (when (some? v) (reset! (:!scope form) v)))))

      ;; Deck
      (dom/div
        (dom/props {:style {:margin-bottom "var(--sp-3)"}})
        (dom/label (dom/props {:style {:font-weight "600" :font-size "14px" :display "block" :margin-bottom "4px"}})
          (dom/text "Deck"))
        (Typeahead (:!selected-deck conn) decks "Start typing deck name..." nil nil))

      ;; Note Type selectors
      (AnkiSyncModelSelect "Note Type (Basic)" (:!basic-model conn) models basic-fields
        (fn [fs] (str "question \u2192 " (first fs) ", answer \u2192 " (second fs))))
      (AnkiSyncModelSelect "Note Type (Cloze)" (:!cloze-model conn) models cloze-fields
        (fn [fs] (str "cloze \u2192 " (first fs))))

      ;; Options
      (AnkiSyncOptions conn form))))

(e/defn AnkiSyncStatus
  "Sync status display and Push/Cancel buttons. Pull lives in the toolbar.
   sync = {:!phase :!result :!error :!push-pairs :!push-btn}
   conn = {:!selected-deck :!basic-model :!cloze-model ...} — for the push gate."
  [sync !show-modal conn]
  (e/client
    (let [sync-phase (e/watch (:!phase sync))
          sync-result (e/watch (:!result sync))
          sync-error (e/watch (:!error sync))
          can-push? (helpers/pushable? (e/watch (:!selected-deck conn))
                      (e/watch (:!basic-model conn))
                      (e/watch (:!cloze-model conn)))
          {:keys [!phase !result !error !push-pairs !push-btn]} sync]

      ;; Auto-close after a brief "Pushed to Anki" confirmation. Token-guarded
      ;; so the timer schedules exactly once on the :done transition.
      (when (= sync-phase :done)
        (let [[t _] (e/Token :anki-autoclose)]
          (when t (helpers/schedule-close! !show-modal 1500))))
      ;; Status display
      (when sync-phase
        (dom/div
          (dom/props {:style {:margin-bottom "var(--sp-4)" :padding "var(--sp-3)" :background "var(--color-bg-subtle)"
                              :border-radius "var(--radius-sm)" :font-size "14px"}})
          (cond
            (= sync-phase :pushing) (dom/text "Pushing cards to Anki...")
            (= sync-phase :recording) (dom/text "Saving to database...")
            (= sync-phase :error)
            (dom/div
              (dom/props {:style {:color "var(--color-danger)"}})
              (dom/text (str "Error: " (or sync-error "Unknown error"))))
            (= sync-phase :done)
            (dom/div
              (dom/props {:style {:color "var(--color-success)"}})
              (dom/text "Pushed to Anki")))))

      ;; Action buttons
      (dom/div
        (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "var(--sp-2)" :margin-top "var(--sp-2)"}})
        (dom/button
          (dom/props {:class "btn btn-secondary" :style {:font-size "15px"}})

          (dom/text (if (= sync-phase :done) "Close" "Cancel"))
          (dom/On "click" (fn [_]
                            (reset! !show-modal false)
                            (reset! !phase nil)
                            (reset! !result nil)
                            (reset! !error nil)
                            (reset! !push-pairs nil))
            nil))
        (when-not (#{:pushing :recording} sync-phase)
          (dom/button
            (dom/props {:class "btn btn-primary" :style {:font-size "15px"}
                        :disabled (not can-push?)})
            (dom/text "Push to Anki")
            (reset! !push-btn dom/node)
            (e/on-unmount (fn [] (reset! !push-btn nil)))
            (dom/On "click"
              (fn [_]
                (reset! !phase :pushing)
                (reset! !result nil)
                (reset! !error nil)
                (reset! !push-pairs nil))
              nil)))))))
