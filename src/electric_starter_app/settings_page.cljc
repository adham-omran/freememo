(ns electric-starter-app.settings-page
  "Settings page UI component."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]
    #?(:clj [electric-starter-app.settings :as settings])))


(e/defn SettingsPage [user-id username enc-key]
  (e/client
    ;; Logout section
    (dom/div
      (dom/props {:style {:display "flex" :align-items "center" :justify-content "space-between"
                          :padding "12px 0" :border-bottom "1px solid #e0e0e0" :margin-bottom "16px"}})
      (dom/span
        (dom/props {:style {:font-size "14px" :color "#666"}})
        (dom/text "Logged in as ")
        (dom/strong (dom/text username)))
      (dom/form
        (dom/props {:action "/api/logout" :method "post" :style {:margin "0"}})
        (dom/button
          (dom/props {:type "submit"
                      :style {:padding "6px 16px" :background "#dc3545" :color "white"
                              :border "none" :border-radius "4px" :cursor "pointer"
                              :font-size "13px"}})
          (dom/text "Logout"))))

    ;; Load initial values from server
    (let [!key-status-refresh (atom 0)
          key-status-refresh (e/watch !key-status-refresh)
          api-key-status (e/server
                           (do key-status-refresh
                               (settings/get-openai-api-key-status user-id enc-key)))
          api-key-source (:source api-key-status)
          api-key-status-text (case api-key-source
                                :user "Configured"
                                :shared "Using demo key"
                                "Not configured")
          api-key-button-text (if (= api-key-source :user)
                                "Update OpenAI API Key"
                                "Set OpenAI API Key")
          !show-key-modal (atom false)
          show-key-modal (e/watch !show-key-modal)
          !draft-key (atom "")
          draft-key (e/watch !draft-key)
          !key-save-error (atom nil)
          key-save-error (e/watch !key-save-error)
          server-reasoning (e/server (settings/get-reasoning user-id))
          !reasoning (atom server-reasoning)
          reasoning (e/watch !reasoning)
          server-verbosity (e/server (settings/get-verbosity user-id))
          !verbosity (atom server-verbosity)
          verbosity (e/watch !verbosity)]

      (dom/div
        (dom/h2 (dom/text "Settings"))

        (dom/div
          (dom/props {:style {:margin-bottom "20px"}})
          (dom/label (dom/text "OpenAI API Key:"))
          (dom/br)
          (dom/span
            (dom/props {:style {:display "inline-block" :margin "8px 12px 0 0"
                                :font-size "14px" :color "#666"}})
            (dom/text api-key-status-text))
          (dom/button
            (dom/props {:type "button"
                        :style {:padding "8px 14px"
                                :background "#0d6efd"
                                :color "white"
                                :border "none"
                                :border-radius "4px"
                                :cursor "pointer"
                                :font-size "13px"}})
            (dom/text api-key-button-text)
            (dom/On "click"
              (fn [_]
                (reset! !draft-key "")
                (reset! !key-save-error nil)
                (reset! !show-key-modal true))
              nil)))

        (when show-key-modal
          (dom/div
            (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                                :background "rgba(0,0,0,0.5)" :display "flex" :align-items "center"
                                :justify-content "center" :z-index "1000"}
                        :tabindex "-1"})
            (dom/On "click" (fn [_] (reset! !show-key-modal false)) nil)
            (dom/On "keydown"
              (fn [e]
                #?(:cljs
                   (when (= (.-key e) "Escape")
                     (reset! !show-key-modal false))))
              nil)
            (dom/div
              (dom/props {:style {:background "white" :border-radius "8px" :padding "24px"
                                  :width "420px" :max-width "90%"
                                  :box-shadow "0 4px 6px rgba(0,0,0,0.1)"}})
              (dom/On "click" (fn [e] (.stopPropagation e)) nil)
              (dom/h3
                (dom/props {:style {:margin-top "0" :margin-bottom "16px"}})
                (dom/text "Save OpenAI API Key"))
              (dom/p
                (dom/props {:style {:margin-top "0" :margin-bottom "12px" :font-size "13px" :color "#666"}})
                (dom/text "Enter your key and click Save. Saving an empty value clears your stored key."))
              (dom/input
                (dom/props {:type "password"
                            :value draft-key
                            :placeholder "sk-..."
                            :style {:width "100%"
                                    :padding "8px"
                                    :border "1px solid #ccc"
                                    :border-radius "4px"
                                    :font-size "14px"}})
                (let [input-event (dom/On "input" #(-> % .-target .-value) nil)]
                  (when (some? input-event)
                    (reset! !draft-key input-event))))
              (when key-save-error
                (dom/div
                  (dom/props {:style {:margin-top "10px" :font-size "13px" :color "#dc3545"}})
                  (dom/text key-save-error)))
              (dom/div
                (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "12px"
                                    :margin-top "16px"}})
                (dom/button
                  (dom/props {:type "button"
                              :style {:padding "8px 16px" :background "#f8f9fa" :color "#333"
                                      :border "1px solid #ccc" :border-radius "4px"
                                      :cursor "pointer" :font-size "14px"}})
                  (dom/text "Cancel")
                  (dom/On "click" (fn [_] (reset! !show-key-modal false)) nil))
                (dom/button
                  (let [click-event (dom/On "click" identity nil)
                        [?token _] (e/Token click-event)]
                    (dom/props {:type "button"
                                :disabled (some? ?token)
                                :style {:padding "8px 16px"
                                        :background (if (some? ?token) "#999" "#28a745")
                                        :color "white"
                                        :border "none"
                                        :border-radius "4px"
                                        :cursor (if (some? ?token) "not-allowed" "pointer")
                                        :font-size "14px"
                                        :font-weight "500"}})
                    (dom/text "Save")
                    (when-some [token ?token]
                      (let [result (e/server (settings/save-openai-api-key user-id draft-key enc-key))]
                        (if (:success result)
                          (do
                            (reset! !draft-key "")
                            (reset! !key-save-error nil)
                            (swap! !key-status-refresh inc)
                            (reset! !show-key-modal false)
                            (token))
                          (let [err-msg (or (:error result) "Failed to save API key")]
                            (reset! !key-save-error err-msg)
                            (token err-msg)))))))))))

        ;; Reasoning selector
        (dom/div
          (dom/props {:style {:margin-top "20px"}})
          (dom/label (dom/text "Reasoning Effort:"))
          (dom/br)
          (dom/select
            (dom/props {:value reasoning})
            (dom/option (dom/props {:value "none"}) (dom/text "None"))
            (dom/option (dom/props {:value "minimal"}) (dom/text "Minimal"))
            (dom/option (dom/props {:value "low"}) (dom/text "Low"))
            (dom/option (dom/props {:value "medium"}) (dom/text "Medium"))
            (dom/option (dom/props {:value "high"}) (dom/text "High"))
            (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                  [?token ?error] (e/Token change-event)]
              (when (some? change-event)
                (reset! !reasoning change-event))
              (when-some [token ?token]
                (e/server (settings/save-reasoning user-id change-event))
                (token)))))

        ;; Verbosity selector
        (dom/div
          (dom/props {:style {:margin-top "20px"}})
          (dom/label (dom/text "Verbosity:"))
          (dom/br)
          (dom/select
            (dom/props {:value verbosity})
            (dom/option (dom/props {:value "low"}) (dom/text "Low"))
            (dom/option (dom/props {:value "medium"}) (dom/text "Medium"))
            (dom/option (dom/props {:value "high"}) (dom/text "High"))
            (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                  [?token ?error] (e/Token change-event)]
              (when (some? change-event)
                (reset! !verbosity change-event))
              (when-some [token ?token]
                (e/server (settings/save-verbosity user-id change-event))
                (token)))))))))
