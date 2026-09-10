(ns freememo.right-side-panel
  "Right-side AI assistant panel.

   The panel can collapse and resize. It keeps the former right-panel settings
   keys so an existing layout does not reset after Pins moves to the left panel.

   The subtree remounts only when root-topic-id changes. Local chat and panel
   state therefore stays stable during page navigation within one document."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.hierarchy-side-panel :refer [SidePanelShell]]
   [freememo.assistant-panel :refer [AssistantPanel]]
   [freememo.command-bus :as bus]
   #?(:clj [freememo.settings :as settings])))

(e/defn RightSidePanel [page-topic-id root-topic-id user-id]
  (e/client
    (e/for-by identity [_k [root-topic-id]]
      (let [persisted-open? (e/server (settings/get-assistant-open user-id root-topic-id))
            ;; A command-set state must restore at every Viewer viewport.
            initial-open?   persisted-open?
            !open? (atom initial-open?)
            open? (e/watch !open?)
            !save (atom nil)
            save-val (e/watch !save)
            [t _] (e/Token save-val)
            persisted-width (e/server (settings/get-assistant-width user-id root-topic-id))
            !width-px (atom persisted-width)
            width-px (e/watch !width-px)
            !width-save (atom nil)
            width-save (e/watch !width-save)
            [tw _] (e/Token width-save)
            toggle-assistant-pane!
            (e/snapshot
              (fn []
                (let [next-open? (not @!open?)]
                  (reset! !open? next-open?)
                  (reset! !save next-open?))))]

        (bus/publish-ctx! {:toggle-assistant-pane! toggle-assistant-pane!})
        (e/on-unmount #(bus/retract-ctx! [:toggle-assistant-pane!]))

        (when t
          (let [r (e/server (e/Offload #(settings/save-assistant-open user-id root-topic-id save-val)))]
            (case r
              (if (:success r) (t) (t (:error r))))))

        (when tw
          (let [r (e/server (e/Offload #(settings/save-assistant-width user-id root-topic-id width-save)))]
            (case r
              (if (:success r) (tw) (tw (:error r))))))

        (SidePanelShell
          {:side :left
           :class "assistant-side-panel"
           :toggle-aria-label "Toggle assistant panel"
           :resize-aria-label "Resize assistant panel"
           :open? open? :!open? !open? :!save !save
           :width-px width-px :!width-px !width-px :!width-save !width-save}
          (e/fn []
            (dom/span
              (dom/props {:class "side-panel__title"})
              (dom/text "Assistant")))
          (e/fn []
            (AssistantPanel page-topic-id root-topic-id user-id)))))))
