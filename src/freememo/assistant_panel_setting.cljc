(ns freememo.assistant-panel-setting
  "Assistant layout setting for the AI Features page."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.commands :as commands]
   #?(:clj [freememo.settings :as settings])))

(e/defn AssistantPanelLayoutField [user-id]
  (e/client
    (let [saved-value (e/server (settings/get-assistant-full-height user-id))
          !layout (atom (if saved-value "full" "content"))
          layout (e/watch !layout)]
      (dom/div
        (dom/props {:class "field"})
        (dom/label
          (dom/props {:class "label"})
          (dom/text "Assistant panel height"))
        (dom/select
          (dom/props {:class "select" :value layout})
          (dom/option
            (dom/props {:value "content" :selected (= layout "content")})
            (dom/text "Content area"))
          (dom/option
            (dom/props {:value "full" :selected (= layout "full")})
            (dom/text "Full workspace"))
          (let [change (dom/On "change" #(-> % .-target .-value) nil)
                [token ?error] (e/Token change)]
            (dom/props {:disabled (some? token)
                        :aria-busy (some? token)
                        :aria-invalid (some? ?error)})
            (when (some? change)
              (reset! !layout change))
            (when token
              (let [result (e/server
                             (e/Offload
                               #(settings/save-assistant-full-height
                                  user-id (= change "full"))))]
                (case result
                  (if (:success result)
                    (case (e/server (commands/bump! user-id :set-setting))
                      (token))
                    (token (:error result))))))))
        (dom/div
          (dom/props {:class "hint"})
          (dom/text "Full workspace extends the assistant beside the card table in the Viewer."))))))
