(ns freememo.undo-history-modal
  "Undo/Actions UI: a modal listing the user's recent reversible actions
   (12h / 100 cap, newest first) plus a hidden trigger for the Cmd-Shift-Z
   global-undo shortcut. Each row reverses one action; rows whose target was
   touched by a newer action are shown as superseded and disabled (the hybrid
   ordering model — keyboard pops the newest, the list undoes selectively).
   Reverse logic lives in freememo.undo; this file is the surface."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.keyboard :as keyboard]
   #?(:clj [freememo.db :as db])
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
                                 :source (source-cell titles (entry-source-ids e) (entry-card-count e))
                                 :superseded? sup?})}))
          {:seen {} :rows []}
          entries)))))

(defn get-undo-views* [_refresh user-id]
  #?(:clj (annotate-views (db/get-undo-entries user-id))
     :cljs []))

(defn undo-entry!* [user-id entry-id]
  #?(:clj (do (undo/undo-entry! user-id entry-id) nil)
     :cljs nil))

(defn undo-newest!* [user-id]
  #?(:clj (do (undo/undo-newest! user-id) nil)
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Escape-to-close — plain defn so addEventListener attaches once per open
;; (Electric re-evaluates let-bindings unpredictably). Returns a cleanup fn.
;; ---------------------------------------------------------------------------

(defn install-escape-listener! [!open?]
  #?(:cljs
     (let [on-key (fn [e] (when (= (.-key e) "Escape") (reset! !open? false)))]
       (.addEventListener js/document "keydown" on-key)
       (fn [] (.removeEventListener js/document "keydown" on-key)))
     :clj (fn [] nil)))

;; ---------------------------------------------------------------------------
;; Components
;; ---------------------------------------------------------------------------

(e/defn UndoNewestTrigger
  "Invisible button bound to keyboard/!undo-newest-btn-ref. The Cmd-Shift-Z
   handler clicks it; clicking undoes the newest live action."
  [user-id]
  (e/client
    (dom/button
      (dom/props {:style {:display "none"} :aria-hidden "true" :tabindex "-1"})
      (reset! keyboard/!undo-newest-btn-ref dom/node)
      (e/on-unmount (fn [] (reset! keyboard/!undo-newest-btn-ref nil)))
      (let [click (dom/On "click" identity nil)
            [t _] (e/Token click)]
        (when t
          (case (e/server (undo-newest!* user-id)) (t)))))))

(e/defn ActionsNavButton
  "Toolbar pill button that toggles the Actions/undo-history modal.
   Mirrors modal-open state via .is-open / aria-pressed."
  [!open?]
  (e/client
    (let [open? (e/watch !open?)]
      (dom/button
        (dom/props {:class (str "nav-action-btn" (when open? " is-open"))
                    :aria-label "Actions"
                    :aria-pressed (str (boolean open?))
                    :data-tooltip "Undo history (⇧⌘Z to undo)"})
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
        (dom/text (or (:source row) "")))
      (dom/td
        (dom/props {:style {:padding "8px 12px" :text-align "right"
                            :border-bottom "0.5px solid var(--color-border-light)"}})
        (if (:superseded? row)
          (dom/span
            (dom/props {:style {:font-size "12px" :color "var(--color-text-hint)"}
                        :data-tooltip "A newer action changed the same item"})
            (dom/text "Superseded"))
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"})
            (dom/text "Undo")
            (let [click (dom/On "click" identity nil)
                  [t _] (e/Token click)]
              (dom/props {:disabled (some? t)})
              (when t
                (case (e/server (undo-entry!* user-id (:id row))) (t))))))))))

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
          (let [cleanup (install-escape-listener! !open?)]
            (e/on-unmount cleanup))
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
                                      :grid-template-columns "110px 1fr 1.4fr 100px"
                                      :font-size "13px"}})
                  (e/for [row (e/diff-by :id rows)]
                    (UndoRow row user-id)))
                (dom/div
                  (dom/props {:style {:text-align "center" :padding "32px 12px"
                                      :color "var(--color-text-secondary)" :font-size "13px"}})
                  (dom/text "Nothing to undo. Deletes from the last 12 hours show up here."))))))))))
