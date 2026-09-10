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
  (let [card-ids (vec (:cards note))
        tags (set (:tags note))
        card-count (count card-ids)
        suspended-count (count (filter true? suspended-flags))]
    {:phase :ready
     :note-id (:noteId note)
     :tags tags
     :marked? (contains? tags "marked")
     :card-ids card-ids
     :card-count card-count
     :suspended-count suspended-count
     :remarks-html (or (get-in note [:fields :Remarks :value]) "")}))

(defn note-action-request
  "Return the AnkiConnect request for one editor state change."
  [note-id state action remarks-html]
  (case action
    :mark {:action "addTags" :params {:notes [note-id] :tags "marked"}}
    :unmark {:action "removeTags" :params {:notes [note-id] :tags "marked"}}
    :suspend {:action "suspend" :params {:cards (:card-ids state)}}
    :unsuspend {:action "unsuspend" :params {:cards (:card-ids state)}}
    :save-remarks {:action "updateNoteFields"
                   :params {:note {:id note-id
                                   :fields {"Remarks" (or remarks-html "")}}}}))

(defn remarks-dirty?
  "True when editor remarks differ from the latest Anki note value."
  [state remarks-html]
  (not= (:remarks-html state) remarks-html))

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
     (let [{:keys [action params]} (note-action-request note-id state action remarks-html)]
       (reset! !state {:phase :saving})
       ((helpers/anki-call! action params)
        (fn [_]
          (refresh-anki-overlay!)
          (load-note-state! note-id !state))
        (fn [error]
          (reset! !state (assoc state :phase :ready :message (str error)))))
       nil)
     :clj nil))

(defn save-remarks!
  "Save changed remarks through the editor's main Save action."
  [note-id !state !remarks-html]
  #?(:cljs
     (let [state @!state
           remarks-html @!remarks-html]
       (when (and (= :ready (:phase state))
                  (remarks-dirty? state remarks-html))
         (run-note-action! note-id state :save-remarks remarks-html !state)))
     :clj nil)
  nil)

(e/defn AnkiNoteActions
  "Render compact controls for one pushed card's Anki note.

   `!save-remarks` holds the current remarks save function. The card editor
   invokes it after its card write succeeds."
  [note-id !save-remarks]
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
              (dom/props {:class "card-anki-action-status" :role "status"})
              (dom/text "Checking Anki…"))

            (= :absent (:phase state))
            (dom/p
              (dom/props {:class "card-anki-action-status"})
              (dom/text "This Anki note no longer exists."))

            (= :error (:phase state))
            (dom/p
              (dom/props {:class "card-anki-action-status card-anki-action-status--error"})
              (dom/text (str "Could not reach Anki. " (:message state))))

            (contains? #{:ready :saving} (:phase state))
            (let [saving? (= :saving (:phase state))
                  marked? (:marked? state)
                  suspended? (pos? (:suspended-count state))
                  suspended-label (if (pos? (:suspended-count state))
                                    (str "Suspended " (:suspended-count state)
                                      "/" (:card-count state))
                                    "Suspended")]
              (dom/section
                (dom/props {:class "card-anki-actions" :aria-label "Anki state"})
                (dom/div
                  (dom/props {:class "card-anki-toggles"})
                  (dom/label
                    (dom/props {:class "card-anki-toggle"})
                    (dom/input
                      (dom/props {:type "checkbox" :checked marked? :disabled saving?})
                      (dom/On "change"
                        (fn [event]
                          (run-note-action! note-id state
                            (if (-> event .-target .-checked) :mark :unmark)
                            nil !state))
                        nil))
                    (dom/text "Marked"))
                  (dom/label
                    (dom/props {:class "card-anki-toggle"})
                    (dom/input
                      (dom/props {:type "checkbox" :checked suspended?
                                  :disabled (or saving? (zero? (:card-count state)))})
                      (dom/On "change"
                        (fn [event]
                          (run-note-action! note-id state
                            (if (-> event .-target .-checked) :suspend :unsuspend)
                            nil !state))
                        nil))
                    (dom/text suspended-label))
                  (when saving?
                    (dom/span
                      (dom/props {:class "card-anki-action-message" :role "status"})
                      (dom/text "Saving Anki…")))
                  (when (:message state)
                    (dom/span
                      (dom/props {:class "card-anki-action-message card-anki-action-message--error"})
                      (dom/text (:message state)))))
                (e/for-by identity [_remarks-key [[note-id (:remarks-html state)]]]
                  (let [!remarks-html (atom (:remarks-html state))]
                    (dom/div
                      (dom/props {:class "card-anki-remarks"})
                      (dom/label
                        (dom/props {:class "card-anki-remarks-label"})
                        (dom/text "Remarks"))
                      (dom/div
                        (dom/props {:class "card-anki-remarks-editor"})
                        (QuillField (:remarks-html state)
                          (fn [html]
                            (reset! !remarks-html html)
                            (reset! !save-remarks
                              (fn [] (save-remarks! note-id !state !remarks-html))))
                          "Remarks…" [:anki-remarks note-id] nil nil nil)))))))))))))
