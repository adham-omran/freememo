(ns electric-starter-app.settings-page
  "Settings page UI component."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]
    #?(:clj [electric-starter-app.settings :as settings])))

(e/defn Input [v]
  (dom/input
    (dom/props {:type "password" :value v})
    (dom/On "input" #(-> % .-target .-value) v)))

(e/defn SaveButton []
  (dom/button
    (dom/props {:type "button"})
    (dom/text "Save")
    (let [click-event (dom/On "click" identity nil)
          [?token ?error] (e/Token click-event)]
      (when ?error
        (dom/text " ")
        (dom/span
          (dom/props {:style {:color "red"}})
          (dom/text ?error)))
      (dom/props {:disabled (some? ?token)})
      ?token)))

(e/defn SettingsPage []
  (e/client
    ;; Load initial value from server
    (let [server-key (e/server (settings/get-openai-api-key))
          !api-key (atom server-key)
          api-key (e/watch !api-key)
          server-reasoning (e/server (settings/get-reasoning))
          !reasoning (atom server-reasoning)
          reasoning (e/watch !reasoning)
          server-verbosity (e/server (settings/get-verbosity))
          !verbosity (atom server-verbosity)
          verbosity (e/watch !verbosity)
          !save-success (atom nil)
          save-success (e/watch !save-success)]

      (dom/div
        (dom/h2 (dom/text "Settings"))

        (dom/div
          (dom/label (dom/text "OpenAI API Key:"))
          (dom/br)
          (reset! !api-key (Input api-key)))

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
            (reset! !reasoning (dom/On "change" #(-> % .-target .-value) reasoning))))

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
            (reset! !verbosity (dom/On "change" #(-> % .-target .-value) verbosity))))

        (dom/div
          (dom/props {:style {:margin-top "20px"}})
          (when-some [token (SaveButton)]
            (let [result-api-key (e/server (settings/save-openai-api-key api-key))
                  result-reasoning (e/server (settings/save-reasoning reasoning))
                  result-verbosity (e/server (settings/save-verbosity verbosity))
                  all-success (and (:success result-api-key)
                                   (:success result-reasoning)
                                   (:success result-verbosity))
                  error (or (:error result-api-key)
                            (:error result-reasoning)
                            (:error result-verbosity))]
              (if all-success
                (do
                  (reset! !save-success true)
                  #?(:cljs (js/setTimeout #(reset! !save-success nil) 3000))
                  (token))
                (token error)))))

        (when save-success
          (dom/div
            (dom/props {:style {:margin-top "10px" :color "green"}})
            (dom/text "✓ Saved successfully")))))))
