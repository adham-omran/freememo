(ns freememo.right-side-panel
  "Right-side collapsible/resizable panel with two tabs: Pins and AI Assistant.

   Owns the panel chrome — collapse toggle, resize handle, and per-document
   open/width persistence (reusing the pins_open_/pins_width_ settings the pins
   panel used before it gained tabs). The active tab is persisted per document
   as assistant_tab_<root-id>. Tab bodies are rendered by freememo.pin-side-panel
   (PinsBody) and freememo.assistant-panel (AssistantPanel).

   Frame isolation: the subtree remounts only when root-topic-id changes (i.e.
   navigating between documents), so open/width/tab/active-chat state persists
   across page scrolls within one document but resets across documents."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.pin-side-panel :refer [PinsBody]]
   [freememo.assistant-panel :refer [AssistantPanel]]
   [freememo.util :as util]
   [freememo.viewport :as viewport]
   #?(:clj [freememo.settings :as settings])))

(e/defn RightSidePanel [page-topic-id root-topic-id user-id]
  (e/client
    (e/for-by identity [_k [root-topic-id]]
      (let [phone?          (e/watch viewport/!phone?)
            persisted-open? (e/server (settings/get-pins-open user-id root-topic-id))
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
            [tw _] (e/Token width-save)
            persisted-tab (e/server (settings/get-assistant-tab user-id root-topic-id))
            !tab (atom persisted-tab)
            tab (e/watch !tab)
            !tab-save (atom nil)
            tab-save (e/watch !tab-save)
            [tt _] (e/Token tab-save)]

        (when t
          (let [r (e/server (e/Offload #(settings/save-pins-open user-id root-topic-id save-val)))]
            (case r
              (if (:success r) (t) (t (:error r))))))

        (when tw
          (let [r (e/server (e/Offload #(settings/save-pins-width user-id root-topic-id width-save)))]
            (case r
              (if (:success r) (tw) (tw (:error r))))))

        (when tt
          (let [r (e/server (e/Offload #(settings/save-assistant-tab user-id root-topic-id tab-save)))]
            (case r (tt))))

        (dom/div
          (dom/props {:class (str "pin-side-panel"
                               (when-not open? " pin-side-panel--collapsed"))
                      :style (merge {:position "relative"}
                               (when open? {:width (str width-px "px")}))})

          ;; Header: toggle (always visible, anchored right via CSS row-reverse)
          ;; + tab switch (only when open).
          (dom/div
            (dom/props {:class "side-panel__header"})
            (dom/button
              (dom/props {:class "side-panel__toggle"
                          :aria-label "Toggle side panel"
                          :aria-expanded (str (boolean open?))})
              (dom/text "☰")
              (dom/On "click"
                (fn [_]
                  (let [next-open? (not @!open?)]
                    (reset! !open? next-open?)
                    (reset! !save next-open?)))
                nil))
            (when open?
              (dom/div
                (dom/props {:class "side-panel__tabs" :role "tablist"})
                (dom/button
                  (dom/props {:class (str "side-panel__tab"
                                       (when (= tab "pins") " side-panel__tab--active"))
                              :role "tab" :aria-selected (str (= tab "pins"))})
                  (dom/text "Pins")
                  (dom/On "click"
                    (fn [_] (reset! !tab "pins") (reset! !tab-save "pins")) nil))
                (dom/button
                  (dom/props {:class (str "side-panel__tab"
                                       (when (= tab "assistant") " side-panel__tab--active"))
                              :role "tab" :aria-selected (str (= tab "assistant"))})
                  (dom/text "Assistant")
                  (dom/On "click"
                    (fn [_] (reset! !tab "assistant") (reset! !tab-save "assistant")) nil)))))

          ;; Resize handle on the inner (left) edge; only when open.
          (when open?
            (dom/div
              (dom/props {:class "side-panel__resize side-panel__resize--left"
                          :title "Drag to resize"
                          :role "separator" :aria-orientation "vertical"
                          :aria-label "Resize side panel" :tabindex "0"
                          :aria-valuenow (str (int (or width-px 0)))})
              (dom/On "pointerdown"
                (fn [e]
                  (util/start-drag-px! e !width-px
                    {:min 180
                     :max (max 180 (util/panel-resize-max (.-currentTarget e) :before 320))
                     :invert? true
                     :on-commit #(reset! !width-save %)}))
                nil)
              (dom/On "keydown"
                (fn [e]
                  (util/key-resize-px! e !width-px
                    {:min 180
                     :max (max 180 (util/panel-resize-max (.-currentTarget e) :before 320))
                     :invert? true
                     :on-commit #(reset! !width-save %)}))
                nil)))

          ;; Active tab body.
          (when open?
            (if (= tab "assistant")
              (AssistantPanel page-topic-id root-topic-id user-id)
              (PinsBody page-topic-id user-id))))))))
