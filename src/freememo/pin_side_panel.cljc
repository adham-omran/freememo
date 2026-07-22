(ns freememo.pin-side-panel
  "Pinned-image content for the right side panel's Pins tab.
   Each pin renders a thumbnail with an F/B placement badge and a × remove button.
   Badge click cycles placement front↔back. Remove button deletes the pin row.

   Reactivity: subscribes to :pin-mutations (bumped by pin insert/remove/update)
   so the list re-queries without touching the wider :refresh channel.

   The collapsible/resizable panel chrome that hosts this body lives in
   freememo.right-side-panel (shared with the AI assistant tab)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.commands :as commands]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.logging :as log])
   #?(:clj [freememo.toasts :as toasts])
   #?(:clj [freememo.user-state :as us])))

;; ---------------------------------------------------------------------------
;; Server helpers
;; ---------------------------------------------------------------------------

(defn get-pins-for-topic*
  "Server-side query: returns pin rows for topic-id ordered by ord.
   `_pin-rev` is the watched :pin-mutations value — bumping it forces a re-query."
  [_pin-rev topic-id]
  #?(:clj
     (when topic-id
       (db/get-pins topic-id))
     :cljs nil))

(defn remove-pin!*
  "Delete a topic_pins row by id, snapshot it for undo with an Undo toast,
   then bump :pin-mutations on behalf of the caller."
  [user-id pin-id]
  #?(:clj
     (let [removed (db/remove-pin! pin-id)]
       (when removed
         (log/audit! {:id ::remove-pin :user-id user-id :action :delete
                      :entity :pin :entity-id pin-id})
         (let [undo-id (db/insert-undo-entry! user-id "remove-pin" "pin"
                         [pin-id] [removed])]
           (toasts/push! user-id {:level :success
                                  :message "Pin removed"
                                  :dedup? false
                                  :actions [{:label "Undo" :undo-id undo-id}]})))
       (commands/bump! user-id :remove-pin)
       :ok)
     :cljs nil))

(defn toggle-pin-placement!*
  "Cycle placement: front → back, back → front. Bumps :pin-mutations."
  [user-id pin-id current-placement]
  #?(:clj
     (let [new-placement (if (= current-placement "front") "back" "front")]
       (db/update-pin-placement! pin-id new-placement)
       (log/audit! {:id ::toggle-pin-placement :user-id user-id :action :update
                    :entity :pin :entity-id pin-id})
       (commands/bump! user-id :toggle-pin-placement)
       :ok)
     :cljs nil))

;; ---------------------------------------------------------------------------
;; PinThumbnail — renders one pin with badge + remove
;; ---------------------------------------------------------------------------

(e/defn PinThumbnail [user-id pin]
  (e/client
    (let [pin-id (:topic_pins/id pin)
          media-id (:topic_pins/media_id pin)
          placement (:topic_pins/placement pin)
          badge-text (if (= placement "front") "F" "B")]
      (dom/div
        (dom/props {:class "pin-thumb"})

        ;; Image
        (dom/img
          (dom/props {:src (str "/api/media/" media-id)
                      :class "pin-thumb__img"
                      :alt (str (if (= placement "front") "Front" "Back") " pin")}))

        ;; F/B badge — click cycles placement.
        ;; Snapshot `placement` at click time inside the `dom/On` callback —
        ;; do NOT close over the reactive `placement` signal in the Offload
        ;; thunk. The server mutation bumps :pin-mutations, which re-queries
        ;; pins and changes `placement`; if the Offload closure read the
        ;; reactive signal, latest-wins would restart it with the new
        ;; placement, toggling again forever.
        (dom/button
          (dom/props {:class "pin-thumb__badge"})
          (dom/text badge-text)
          (let [click-ev (dom/On "click"
                           (fn [_] {:id (str (random-uuid)) :placement placement})
                           nil)
                [t _] (e/Token click-ev)]
            (when t
              (case (e/server (e/Offload #(toggle-pin-placement!* user-id pin-id (:placement click-ev)))) (t)))))

        ;; × remove button
        (dom/button
          (dom/props {:class "pin-thumb__remove"
                      :aria-label "Remove pin"})
          (icons/Icon :x :size 14 :title "Remove pin")
          (let [click-ev (dom/On "click" identity nil)
                [t _] (e/Token click-ev)]
            (when t
              (case (e/server (e/Offload #(remove-pin!* user-id pin-id))) (t)))))))))

;; ---------------------------------------------------------------------------
;; PinsBody — the Pins tab content (scrollable thumbnail list)
;; ---------------------------------------------------------------------------

(e/defn PinsBody
  "Scrollable pin-thumbnail list for `topic-id`. Re-queries on :pin-mutations.
   Rendered inside the shared right-side-panel body; owns no panel chrome."
  [topic-id user-id]
  (e/client
    (dom/div
      (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"
                          :display "flex" :flex-direction "column"
                          :gap "8px" :padding "8px"}})
      (if (nil? topic-id)
        nil
        (let [pin-rev (e/server (e/watch (us/get-atom user-id :pin-mutations)))
              pins (e/server (vec (get-pins-for-topic* pin-rev topic-id)))]
          (e/for-by :topic_pins/id [pin pins]
            (PinThumbnail user-id pin)))))))
