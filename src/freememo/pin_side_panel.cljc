(ns freememo.pin-side-panel
  "Right-side collapsible panel showing pinned images for the current topic.
   Each pin renders a thumbnail with an F/B placement badge and a × remove button.
   Badge click cycles placement front↔back. Remove button deletes the pin row.

   Reactivity: subscribes to :pin-mutations (bumped by pin insert/remove/update)
   so the panel re-queries without touching the wider :refresh channel.

   Open/collapsed state is persisted per-topic via freememo.settings."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])
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
  "Delete a topic_pins row by id, then bump :pin-mutations on behalf of the caller."
  [user-id pin-id]
  #?(:clj
     (do (db/remove-pin! pin-id)
       (swap! (us/get-atom user-id :pin-mutations) inc)
       :ok)
     :cljs nil))

(defn toggle-pin-placement!*
  "Cycle placement: front → back, back → front. Bumps :pin-mutations."
  [user-id pin-id current-placement]
  #?(:clj
     (let [new-placement (if (= current-placement "front") "back" "front")]
       (db/update-pin-placement! pin-id new-placement)
       (swap! (us/get-atom user-id :pin-mutations) inc)
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

        ;; F/B badge — click cycles placement
        (dom/button
          (dom/props {:class "pin-thumb__badge"})
          (dom/text badge-text)
          (let [click-ev (dom/On "click" identity nil)
                [?token _] (e/Token click-ev)]
            (when-some [token ?token]
              (let [result (e/server (toggle-pin-placement!* user-id pin-id placement))]
                (when result
                  (token))))))

        ;; × remove button
        (dom/button
          (dom/props {:class "pin-thumb__remove"})
          (dom/text "×")
          (let [click-ev (dom/On "click" identity nil)
                [?token _] (e/Token click-ev)]
            (when-some [token ?token]
              (let [result (e/server (remove-pin!* user-id pin-id))]
                (when result
                  (token))))))))))

;; ---------------------------------------------------------------------------
;; PinSidePanel — exported component
;; ---------------------------------------------------------------------------

(e/defn PinSidePanel [topic-id root-topic-id user-id]
  (e/client
    ;; Frame isolation — remounts only when root-topic-id changes (i.e. when
    ;; navigating between documents). Page navigation within one document
    ;; updates topic-id reactively without remount, so !open? state and the
    ;; DOM subtree persist across page scrolls. Pin data still queries by
    ;; topic-id since pins are per-topic.
    (e/for-by identity [_k [root-topic-id]]
      (let [initial-open?   (e/server (settings/get-pins-open user-id root-topic-id))
            !open?          (atom initial-open?)
            open?           (e/watch !open?)
            !save           (atom nil)
            save-val        (e/watch !save)
            [?save-token _] (e/Token save-val)]

        (when-some [token ?save-token]
          (e/server (settings/save-pins-open user-id root-topic-id save-val))
          (token))

        (dom/div
          (dom/props {:class (str "pin-side-panel"
                               (when-not open? " pin-side-panel--collapsed"))})

          ;; Header row — toggle (always visible) + title (only when open).
          ;; CSS flips this row's direction so the toggle anchors to the
          ;; panel's right (outer) edge, mirroring the left pane.
          (dom/div
            (dom/props {:class "side-panel__header"})
            (dom/button
              (dom/props {:class "side-panel__toggle"})
              (dom/text "☰")
              (dom/On "click"
                (fn [_]
                  (let [next-open? (not @!open?)]
                    (reset! !open? next-open?)
                    (reset! !save next-open?)))
                nil))
            (when open?
              (dom/span
                (dom/props {:class "side-panel__title"})
                (dom/text "Pins"))))

          (when open?
            (dom/div
              (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"
                                  :display "flex" :flex-direction "column"
                                  :gap "8px" :padding "8px"}})

              ;; Thumbnail list (re-queries on :pin-mutations bump)
              (if (nil? topic-id)
                nil
                (let [pin-rev (e/server (e/watch (us/get-atom user-id :pin-mutations)))
                      pins (e/server (vec (get-pins-for-topic* pin-rev topic-id)))]
                  (e/for-by :topic_pins/id [pin pins]
                    (PinThumbnail user-id pin)))))))))))
