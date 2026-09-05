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
   [freememo.viewport :as viewport]
   #?(:clj [freememo.settings :as settings])))

(e/defn RightSidePanel [page-topic-id root-topic-id user-id]
  (e/client
    (e/for-by identity [_k [root-topic-id]]
      (let [phone?          (e/watch viewport/!phone?)
            compact?        (e/watch viewport/!compact?)
            persisted-open? (e/server (settings/get-assistant-open user-id root-topic-id))
            ;; Phone and portrait-tablet (compact) default to collapsed regardless
            ;; of the persisted desktop pref — the toggle still opens it manually.
            initial-open?   (and (not phone?) (not compact?) persisted-open?)
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
            [tw _] (e/Token width-save)]

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
