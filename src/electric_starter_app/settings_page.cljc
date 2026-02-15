(ns electric-starter-app.settings-page
  "Settings page UI component."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]
    #?(:clj [electric-starter-app.settings :as settings])))


(e/defn SettingsPage [user-id username]
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

    ;; Load initial value from server
    (let [server-key (e/server (settings/get-openai-api-key user-id))
          !api-key (atom server-key)
          api-key (e/watch !api-key)
          server-reasoning (e/server (settings/get-reasoning user-id))
          !reasoning (atom server-reasoning)
          reasoning (e/watch !reasoning)
          server-verbosity (e/server (settings/get-verbosity user-id))
          !verbosity (atom server-verbosity)
          verbosity (e/watch !verbosity)]

      (dom/div
        (dom/h2 (dom/text "Settings"))

        (dom/div
          (dom/label (dom/text "OpenAI API Key:"))
          (dom/br)
          (dom/input
            (dom/props {:type "password" :value api-key})
            (let [input-event (dom/On "change" #(-> % .-target .-value) nil)
                  [?token ?error] (e/Token input-event)]
              (when (some? input-event)
                (reset! !api-key input-event))
              (when-some [token ?token]
                (e/server (settings/save-openai-api-key user-id input-event))
                (token)))))

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
                (token)))))

))))
