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
   [freememo.icons :as icons]
   [freememo.util :as util]
   [freememo.viewport :as viewport]
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
                      :aria-label "Remove pin"
                      :data-tooltip "Remove pin"})
          (icons/Icon :x :size 14 :title "Remove pin")
          (let [click-ev (dom/On "click" identity nil)
                [t _] (e/Token click-ev)]
            (when t
              (case (e/server (e/Offload #(remove-pin!* user-id pin-id))) (t)))))))))

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
      (let [phone?          (e/watch viewport/!phone?)
            persisted-open? (e/server (settings/get-pins-open user-id root-topic-id))
            ;; Phone defaults to collapsed; desktop persisted pref preserved.
            initial-open?   (and (not phone?) persisted-open?)
            !open? (atom initial-open?)
            open? (e/watch !open?)
            !save (atom nil)
            save-val (e/watch !save)
            [t _] (e/Token save-val)
            persisted-width (e/server (settings/get-pins-width user-id root-topic-id))
            !width-px (atom persisted-width)
            width-px (e/watch !width-px)
            !width-save (atom nil)
            width-save (e/watch !width-save)
            [tw _] (e/Token width-save)]

        (when t
          (let [r (e/server (e/Offload #(settings/save-pins-open user-id root-topic-id save-val)))]
            (case r
              (if (:success r) (t) (t (:error r))))))

        (when tw
          (let [r (e/server (e/Offload #(settings/save-pins-width user-id root-topic-id width-save)))]
            (case r
              (if (:success r) (tw) (tw (:error r))))))

        (dom/div
          (dom/props {:class (str "pin-side-panel"
                               (when-not open? " pin-side-panel--collapsed"))
                      :style (merge {:position "relative"}
                               (when open? {:width (str width-px "px")}))})

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

          ;; Resize handle on the inner (left) edge; only when open.
          (when open?
            (dom/div
              (dom/props {:class "side-panel__resize side-panel__resize--left"
                          :title "Drag to resize"})
              (dom/On "pointerdown"
                (fn [e]
                  (util/start-drag-px! e !width-px
                    {:min 120
                     :max (max 120 (util/panel-resize-max (.-currentTarget e) :before 320))
                     :invert? true
                     :on-commit #(reset! !width-save %)}))
                nil)))

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
