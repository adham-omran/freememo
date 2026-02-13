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
          !save-success (atom nil)
          save-success (e/watch !save-success)]

      (dom/div
        (dom/h2 (dom/text "Settings"))

        (dom/div
          (dom/label (dom/text "OpenAI API Key:"))
          (dom/br)
          (reset! !api-key (Input api-key)))

        (dom/div
          (when-some [token (SaveButton)]
            (let [result (e/server (settings/save-openai-api-key api-key))]
              (if (:success result)
                (do
                  (reset! !save-success true)
                  #?(:cljs (js/setTimeout #(reset! !save-success nil) 3000))
                  (token))
                (token (:error result))))))

        (when save-success
          (dom/div
            (dom/props {:style {:margin-top "10px" :color "green"}})
            (dom/text "✓ Saved successfully")))))))
