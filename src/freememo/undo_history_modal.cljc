(ns freememo.undo-history-modal
  "Undo/Actions UI: a modal listing the user's recent reversible actions
   (12h / 100 cap, newest first). Each row reverses one action via the
   :undo-entry command; Cmd-Shift-Z dispatches :undo-newest through the
   registry-driven keyboard layer. Rows whose target was touched by a newer
   action are shown as superseded and disabled (the hybrid ordering model —
   keyboard pops the newest, the list undoes selectively).
   Reverse logic lives in freememo.undo; this file is the surface."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.icons :as icons]
   [freememo.command-bus :as bus]
   [freememo.modal-shell :as modal]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.db :as db])
   ;; Loads the :undo-newest / :undo-entry run-command! methods server-side.
   #?(:clj [freememo.undo :as undo])
   #?(:clj [freememo.user-state :as us])))

;; ---------------------------------------------------------------------------
;; Server bridges
;; ---------------------------------------------------------------------------

#?(:clj
   (defn- action-label [action-type refs]
     (case action-type
       "delete-card"        "Deleted a card"
       "bulk-delete-cards"  (str "Deleted " (count refs) " cards")
       "remove-pin"         "Removed a pin"
       "reset-prompt"       "Reset a prompt"
       "delete-document"    "Deleted a document"
       "Action")))

#?(:clj
   (defn- format-ts [ts]
     (when ts
       (.format (java.time.format.DateTimeFormatter/ofPattern "MMM d, HH:mm")
         (.atZone (.toInstant ts) (java.time.ZoneId/systemDefault))))))

#?(:clj
   (defn- entry-source-ids
     "Topic ids whose titles source this entry: the root topics of deleted cards,
      or the deleted document itself. nil for non-card/doc actions (no source)."
     [e]
     (case (:entity_type e)
       "flashcard" (vec (distinct (keep :root_topic_id (:snapshot e))))
       "document"  (vec (:entity_refs e))
       nil)))

#?(:clj
   (defn- entry-card-count [e]
     (case (:entity_type e)
       "flashcard" (count (:snapshot e))
       "document"  (count (:cards (:snapshot e)))
       nil)))

#?(:clj
   (defn- source-cell
     "Source string for a row: first document title (+N more for additional
      distinct documents) followed by the card count. nil when no source."
     [titles src-ids cnt]
     (when (seq src-ids)
       (let [names (distinct (map #(get titles % "—") src-ids))
             extra (dec (count names))
             head  (str (first names) (when (pos? extra) (str " +" extra " more")))]
         (if cnt (str head " · " cnt " card" (when (not= 1 cnt) "s")) head)))))

#?(:clj
   (defn- card-cell
     "Plain-text prompt of an entry's first snapshot card — cloze text or
      question, tags stripped, clamped to 80 chars, with a '+N more' suffix for
      multi-card entries. nil for entries that snapshot no card (pins, prompts,
      topic moves, kg rejects)."
     [e]
     (let [cards (case (:entity_type e)
                   "flashcard" (:snapshot e)
                   "document"  (:cards (:snapshot e))
                   nil)]
       (when-let [c (first cards)]
         (let [cloze   (:cloze c)
               prompt  (if (seq cloze) cloze (:question c))
               text    (-> (or prompt "") (str/replace #"<[^>]+>" "") str/trim)
               clamped (if (> (count text) 80) (str (subs text 0 80) "…") text)
               extra   (dec (count cards))]
           (when (seq clamped)
             (if (pos? extra) (str clamped " +" extra " more") clamped)))))))

#?(:clj
   (defn- annotate-views
     "Project DB entries (newest-first) to slim, wire-friendly view rows:
      label, time, superseded flag, and a source string (deleted cards' document
      or the deleted document) with card count. All source titles resolve in one
      staged-inclusive query."
     [entries]
     (let [titles (db/get-topic-titles (into #{} (mapcat entry-source-ids) entries))]
       (:rows
        (reduce
          (fn [{:keys [seen rows]} e]
            (let [et   (:entity_type e)
                  refs (set (:entity_refs e))
                  sup? (boolean (some (get seen et #{}) refs))]
              {:seen (update seen et (fnil into #{}) refs)
               :rows (conj rows {:id (:id e)
                                 :label (action-label (:action_type e) (:entity_refs e))
                                 :time-str (format-ts (:occurred_at e))
                                 :card-text (card-cell e)
                                 :source (source-cell titles (entry-source-ids e) (entry-card-count e))
                                 :superseded? sup?})}))
          {:seen {} :rows []}
          entries)))))

(defn get-undo-views* [_refresh user-id]
  #?(:clj (annotate-views (db/get-undo-entries user-id))
     :cljs []))

;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
;; Components
;; ---------------------------------------------------------------------------

(e/defn ActionsNavButton
  "Toolbar pill button that toggles the Actions/undo-history modal.
   Mirrors modal-open state via .is-open / aria-pressed."
  [!open?]
  (e/client
    (let [open? (e/watch !open?)]
      (dom/button
        (dom/props {:class (str "nav-action-btn" (when open? " is-open"))
                    :aria-label "Actions"
                    :aria-pressed (str (boolean open?))})
        (icons/Icon :rotate-ccw :size 16)
        (dom/span (dom/props {:class "nav-tab-label"}) (dom/text "Actions"))
        (dom/On "click" (fn [_] (swap! !open? not)) nil)))))

(e/defn UndoRow [row user-id]
  (e/client
    (dom/tr
      (dom/props {:style {:display "contents"}})
      (dom/td
        (dom/props {:style {:padding "8px 12px" :color "var(--color-text-secondary)"
                            :border-bottom "0.5px solid var(--color-border-light)"}})
        (dom/text (or (:time-str row) "")))
      (dom/td
        (dom/props {:style {:padding "8px 12px"
                            :border-bottom "0.5px solid var(--color-border-light)"}})
        (dom/text (:label row)))
      (dom/td
        (dom/props {:style {:padding "8px 12px" :color "var(--color-text-secondary)"
                            :word-break "break-word"
                            :border-bottom "0.5px solid var(--color-border-light)"}})
        (dom/text (or (:card-text row) "")))
      (dom/td
        (dom/props {:style {:padding "8px 12px" :color "var(--color-text-secondary)"
                            :word-break "break-word"
                            :border-bottom "0.5px solid var(--color-border-light)"}})
        (dom/text (or (:source row) "")))
      (dom/td
        (dom/props {:style {:padding "8px 12px" :text-align "right"
                            :border-bottom "0.5px solid var(--color-border-light)"}})
        (if (:superseded? row)
          (dom/span
            (dom/props {:style {:font-size "12px" :color "var(--color-text-hint)"}})
            (tooltip/Tooltip! "A newer action changed the same item")
            (dom/text "Superseded"))
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"})
            (dom/text "Undo")
            ;; Fire-and-forget through the command queue: undo-entry! is
            ;; idempotent, and the row re-renders when :undo-mutations bumps.
            (dom/On "click" (fn [_] (bus/dispatch! :undo-entry {:id (:id row)})) nil)))))))

(e/defn UndoHistoryModal
  "Modal listing reversible actions; refreshes on :undo-mutations."
  [user-id !open?]
  (e/client
    (when (e/watch !open?)
      (let [refresh (e/server (e/watch (us/get-atom user-id :undo-mutations)))
            rows (e/server (get-undo-views* refresh user-id))
            n (count rows)]
        (dom/div
          (dom/props {:class "modal-backdrop"})
          (dom/On "click" (fn [_] (reset! !open? false)) nil)
          (modal/ModalEscape (fn [] (reset! !open? false)) "Actions history")
          (dom/div
            (dom/props {:class "modal-content"
                        :style {:width "min(680px, 95vw)" :max-height "85vh"
                                :overflow "hidden" :display "flex"
                                :flex-direction "column" :padding "0"}})
            (dom/On "click" (fn [e] (.stopPropagation e)) nil)
            ;; Header
            (dom/div
              (dom/props {:style {:padding "16px 20px"
                                  :border-bottom "0.5px solid var(--color-border-light)"
                                  :display "flex" :align-items "center"
                                  :justify-content "space-between" :gap "12px"}})
              (dom/h3
                (dom/props {:style {:margin "0" :font-size "16px" :font-weight "500"}})
                (dom/text (str "Actions · " n " undoable")))
              (dom/button
                (dom/props {:aria-label "Close"
                            :style {:width "28px" :height "28px" :display "inline-flex"
                                    :align-items "center" :justify-content "center"
                                    :background "transparent" :cursor "pointer"
                                    :border "0.5px solid var(--color-border)"
                                    :border-radius "var(--radius-md)" :padding "0"
                                    :color "var(--color-text-secondary)"}})
                (icons/Icon :x :size 14)
                (dom/On "click" (fn [_] (reset! !open? false)) nil)))
            ;; Body
            (dom/div
              (dom/props {:style {:padding "12px 20px" :overflow "auto" :flex "1 1 auto"}})
              (if (pos? n)
                (dom/table
                  (dom/props {:style {:width "100%" :display "grid"
                                      :grid-template-columns "110px 1fr 1fr 1.4fr 100px"
                                      :font-size "13px"}})
                  (e/for [row (e/diff-by :id rows)]
                    (UndoRow row user-id)))
                (dom/div
                  (dom/props {:style {:text-align "center" :padding "32px 12px"
                                      :color "var(--color-text-secondary)" :font-size "13px"}})
                  (dom/text "Nothing to undo. Deletes from the last 12 hours show up here."))))))))))
