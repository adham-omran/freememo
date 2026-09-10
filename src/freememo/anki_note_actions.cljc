(ns freememo.anki-note-actions
  "Live Anki-note controls shared by normal card editor modals."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.quill-field :refer [QuillField]]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.client-state :as cs]))

(defn refresh-anki-overlay!
  "Ask every mounted card-list overlay in this browser session to re-read Anki."
  []
  (swap! (cs/get-atom :anki-overlay-refresh) inc)
  nil)

(defn note->state
  "Convert one AnkiConnect notesInfo value into editor display state."
  [note suspended-flags]
  (let [card-ids (vec (:cards note))]
    {:phase :ready
     :note-id (:noteId note)
     :tags (set (:tags note))
     :card-ids card-ids
     :suspended-count (count (filter true? suspended-flags))
     :remarks-html (or (get-in note [:fields :Remarks :value]) "")}))

(defn load-note-state!
  "Load the current note fields, tags, and card suspension state from Anki."
  [note-id !state]
  #?(:cljs
     (do
       (reset! !state {:phase :loading})
       ((helpers/anki-call! "notesInfo" {:notes [note-id]})
        (fn [notes]
          (let [note (first (js->clj notes :keywordize-keys true))]
            (if (and (map? note) (:noteId note))
              (let [card-ids (vec (:cards note))]
                ((helpers/anki-call! "areSuspended" {:cards card-ids})
                 (fn [suspended]
                   (reset! !state (note->state note (vec (js->clj suspended)))))
                 (fn [error]
                   (reset! !state {:phase :error :message (str error)}))))
              (reset! !state {:phase :absent}))))
        (fn [error]
          (reset! !state {:phase :error :message (str error)})))
       nil)
     :clj nil))

(defn run-note-action!
  "Run one Anki note mutation, then refresh both this editor and list overlays."
  [note-id state action remarks-html !state]
  #?(:cljs
     (let [params (case action
                    :unmark {:notes [note-id] :tags "marked"}
                    :unsuspend {:cards (:card-ids state)}
                    :save-remarks {:note {:id note-id
                                          :fields {"Remarks" (or remarks-html "")}}})]
       (reset! !state {:phase :saving})
       ((helpers/anki-call! (case action
                              :unmark "removeTags"
                              :unsuspend "unsuspend"
                              :save-remarks "updateNoteFields")
                            params)
        (fn [_]
          (refresh-anki-overlay!)
          (load-note-state! note-id !state))
        (fn [error]
          (reset! !state (assoc state :phase :ready :message (str error)))))
       nil)
     :clj nil))

(e/defn AnkiNoteActions
  "Render and mutate one pushed card's Anki note.

   Pre: `note-id` identifies the note the editor's card was pushed as.
   Post: successful actions re-read Anki before presenting the next state."
  [note-id]
  (e/client
    (when note-id
      (e/for-by identity [_note-key [note-id]]
        (let [!state (atom nil)
              state (e/watch !state)]
          (when (nil? state)
            (load-note-state! note-id !state))
          (cond
            (= :loading (:phase state))
            (dom/p
              (dom/props {:class "card-anki-state-panel" :role "status"})
              (dom/text "Checking Anki…"))

            (= :absent (:phase state))
            (dom/p
              (dom/props {:class "card-anki-state-panel"})
              (dom/text "This Anki note no longer exists."))

            (= :error (:phase state))
            (dom/p
              (dom/props {:class "card-anki-state-panel"
                          :style {:color "var(--color-danger-text)"}})
              (dom/text (str "Could not reach Anki. " (:message state))))

            (contains? #{:ready :saving} (:phase state))
            (let [saving? (= :saving (:phase state))]
              (dom/section
                (dom/props {:class "card-anki-state-panel" :aria-label "Edit Anki state"})
                (dom/div
                  (dom/props {:class "card-anki-state-heading"})
                  (dom/span
                    (dom/props {:class "card-anki-state-label"})
                    (dom/text "Anki actions"))
                  (when (:message state)
                    (dom/span
                      (dom/props {:style {:color "var(--color-danger-text)" :font-size "12px"}})
                      (dom/text (:message state)))))
                (when (contains? (:tags state) "marked")
                  (dom/button
                    (dom/props {:class "btn btn-sm btn-secondary" :disabled saving?})
                    (dom/text "Unmark")
                    (dom/On "click"
                      (fn [_] (run-note-action! note-id state :unmark nil !state)) nil)))
                (when (pos? (:suspended-count state))
                  (dom/button
                    (dom/props {:class "btn btn-sm btn-secondary" :disabled saving?
                                :style {:margin-left "8px"}})
                    (dom/text "Unsuspend all")
                    (dom/On "click"
                      (fn [_] (run-note-action! note-id state :unsuspend nil !state)) nil)))
                (e/for-by identity [_remarks-key [[note-id (:remarks-html state)]]]
                  (let [!remarks-html (atom (:remarks-html state))
                        remarks-html (e/watch !remarks-html)]
                    (dom/div
                      (dom/props {:style {:margin-top "12px"}})
                      (dom/label (dom/text "Remarks:"))
                      (QuillField (:remarks-html state)
                        (fn [html] (reset! !remarks-html html))
                        "Remarks…" [:anki-remarks note-id] nil nil nil)
                      (dom/button
                        (dom/props {:class "btn btn-sm btn-secondary" :disabled saving?
                                    :style {:margin-top "8px"}})
                        (dom/text "Save remarks")
                        (dom/On "click"
                          (fn [_]
                            (run-note-action! note-id state :save-remarks remarks-html !state))
                          nil)))))))))))))
