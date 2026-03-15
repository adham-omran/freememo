(ns electric-starter-app.settings-page
  "Settings page UI component."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [electric-starter-app.settings :as settings])))

;; Shared styles
(def section-style
  {:background "#fff" :border "1px solid #e5e7eb" :border-radius "8px"
   :padding "20px" :margin-bottom "16px"})

(def section-title-style
  {:font-size "15px" :font-weight "600" :color "#111" :margin "0 0 16px 0"})

(def field-style
  {:margin-bottom "16px"})

(def label-style
  {:display "block" :font-size "13px" :font-weight "500" :color "#374151" :margin-bottom "6px"})

(def hint-style
  {:font-size "12px" :color "#9ca3af" :margin-top "4px"})

(def select-style
  {:padding "7px 10px" :border "1px solid #d1d5db" :border-radius "6px"
   :font-size "14px" :background "#fff" :color "#111" :min-width "160px"})

(def badge-configured {:display "inline-block" :padding "2px 10px" :border-radius "12px"
                       :font-size "12px" :font-weight "500"
                       :background "#dcfce7" :color "#166534"})

(def badge-demo {:display "inline-block" :padding "2px 10px" :border-radius "12px"
                 :font-size "12px" :font-weight "500"
                 :background "#fef9c3" :color "#854d0e"})

(def badge-none {:display "inline-block" :padding "2px 10px" :border-radius "12px"
                 :font-size "12px" :font-weight "500"
                 :background "#fee2e2" :color "#991b1b"})

(e/defn SettingsPage [user-id username enc-key !settings-refresh]
  (e/client
    (dom/div
      (dom/props {:style {:padding "24px" :max-width "600px" :margin "0 auto"}})

      (dom/h2 (dom/props {:style {:font-size "20px" :font-weight "600" :margin "0 0 20px 0"}})
        (dom/text "Settings"))

      ;; ── Account section ──
      (dom/div
        (dom/props {:style section-style})
        (dom/h3 (dom/props {:style section-title-style}) (dom/text "Account"))
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :justify-content "space-between"}})
          (dom/span
            (dom/props {:style {:font-size "14px" :color "#374151"}})
            (dom/text "Logged in as ")
            (dom/strong (dom/text username)))
          (dom/form
            (dom/props {:action "/api/logout" :method "post" :style {:margin "0"}})
            (dom/button
              (dom/props {:type "submit"
                          :style {:padding "6px 14px" :background "#fff" :color "#dc3545"
                                  :border "1px solid #dc3545" :border-radius "6px" :cursor "pointer"
                                  :font-size "13px" :font-weight "500"}})
              (dom/text "Logout")))))

      ;; ── AI Features section ──
      (let [server-llm-enabled (e/server (settings/get-llm-enabled user-id))
            !llm-enabled (atom server-llm-enabled)
            llm-enabled (e/watch !llm-enabled)
            !key-status-refresh (atom 0)
            key-status-refresh (e/watch !key-status-refresh)
            api-key-status (e/server
                             (do key-status-refresh
                               (settings/get-openai-api-key-status user-id enc-key)))
            api-key-source (:source api-key-status)
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
          (dom/props {:style section-style})
          (dom/h3 (dom/props {:style section-title-style}) (dom/text "AI Features"))

          ;; LLM toggle
          (dom/div
            (dom/props {:style field-style})
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "10px" :cursor "pointer"}})
              (dom/input
                (dom/props {:type "checkbox" :checked llm-enabled
                            :style {:width "18px" :height "18px" :accent-color "#2563eb"}})
                (let [change-event (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)
                      [?token ?error] (e/Token change-event)]
                  (when (some? change-event)
                    (reset! !llm-enabled change-event))
                  (when-some [token ?token]
                    (e/server (settings/save-llm-enabled user-id change-event))
                    (e/server (swap! !settings-refresh inc))
                    (token))))
              (dom/div
                (dom/span
                  (dom/props {:style {:font-size "14px" :font-weight "500" :color "#111"}})
                  (dom/text "Enable LLM features"))
                (dom/div
                  (dom/props {:style hint-style})
                  (dom/text "OCR text extraction and flashcard generation. Requires an OpenAI API key.")))))

          (when llm-enabled
            ;; API Key
            (dom/div
              (dom/props {:style (assoc field-style :padding "14px" :background "#f9fafb"
                                        :border-radius "6px" :border "1px solid #f3f4f6")})
              (dom/div
                (dom/props {:style {:display "flex" :align-items "center" :justify-content "space-between"
                                    :margin-bottom "4px"}})
                (dom/span
                  (dom/props {:style {:font-size "13px" :font-weight "500" :color "#374151"}})
                  (dom/text "OpenAI API Key"))
                (dom/span
                  (dom/props {:style (case api-key-source
                                       :user badge-configured
                                       :shared badge-demo
                                       badge-none)})
                  (dom/text (case api-key-source
                              :user "Configured"
                              :shared "Demo key"
                              "Not set"))))
              (dom/button
                (dom/props {:type "button"
                            :style {:margin-top "8px" :padding "6px 14px" :background "#fff"
                                    :color "#2563eb" :border "1px solid #2563eb" :border-radius "6px"
                                    :cursor "pointer" :font-size "13px" :font-weight "500"}})
                (dom/text (if (= api-key-source :user) "Update Key" "Set API Key"))
                (dom/On "click"
                  (fn [_]
                    (reset! !draft-key "")
                    (reset! !key-save-error nil)
                    (reset! !show-key-modal true))
                  nil)))

            ;; API Key Modal
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
                  (dom/props {:style {:background "white" :border-radius "10px" :padding "28px"
                                      :width "420px" :max-width "90%"
                                      :box-shadow "0 8px 30px rgba(0,0,0,0.12)"}})
                  (dom/On "click" (fn [e] (.stopPropagation e)) nil)
                  (dom/h3
                    (dom/props {:style {:margin-top "0" :margin-bottom "4px" :font-size "17px"}})
                    (dom/text "OpenAI API Key"))
                  (dom/p
                    (dom/props {:style {:margin-top "0" :margin-bottom "16px" :font-size "13px" :color "#6b7280"}})
                    (dom/text "Enter your key and click Save. Saving an empty value clears your stored key."))
                  (dom/input
                    (dom/props {:type "password"
                                :value draft-key
                                :placeholder "sk-..."
                                :style {:width "100%" :padding "10px 12px" :border "1px solid #d1d5db"
                                        :border-radius "6px" :font-size "14px" :box-sizing "border-box"}})
                    (let [input-event (dom/On "input" #(-> % .-target .-value) nil)]
                      (when (some? input-event)
                        (reset! !draft-key input-event))))
                  (when key-save-error
                    (dom/div
                      (dom/props {:style {:margin-top "10px" :font-size "13px" :color "#dc3545"
                                          :padding "8px 10px" :background "#fef2f2" :border-radius "4px"}})
                      (dom/text key-save-error)))
                  (dom/div
                    (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "10px"
                                        :margin-top "20px"}})
                    (dom/button
                      (dom/props {:type "button"
                                  :style {:padding "8px 18px" :background "#fff" :color "#374151"
                                          :border "1px solid #d1d5db" :border-radius "6px"
                                          :cursor "pointer" :font-size "14px"}})
                      (dom/text "Cancel")
                      (dom/On "click" (fn [_] (reset! !show-key-modal false)) nil))
                    (dom/button
                      (let [click-event (dom/On "click" identity nil)
                            [?token _] (e/Token click-event)]
                        (dom/props {:type "button"
                                    :disabled (some? ?token)
                                    :style {:padding "8px 18px"
                                            :background (if (some? ?token) "#93c5fd" "#2563eb")
                                            :color "white" :border "none" :border-radius "6px"
                                            :cursor (if (some? ?token) "not-allowed" "pointer")
                                            :font-size "14px" :font-weight "500"}})
                        (dom/text (if (some? ?token) "Saving..." "Save"))
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

            ;; Reasoning
            (dom/div
              (dom/props {:style field-style})
              (dom/label (dom/props {:style label-style}) (dom/text "Reasoning Effort"))
              (dom/select
                (dom/props {:value reasoning :style select-style})
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
                    (token))))
              (dom/div (dom/props {:style hint-style})
                (dom/text "Higher = better quality but slower and more expensive")))

            ;; Verbosity
            (dom/div
              (dom/props {:style field-style})
              (dom/label (dom/props {:style label-style}) (dom/text "Verbosity"))
              (dom/select
                (dom/props {:value verbosity :style select-style})
                (dom/option (dom/props {:value "low"}) (dom/text "Low"))
                (dom/option (dom/props {:value "medium"}) (dom/text "Medium"))
                (dom/option (dom/props {:value "high"}) (dom/text "High"))
                (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                      [?token ?error] (e/Token change-event)]
                  (when (some? change-event)
                    (reset! !verbosity change-event))
                  (when-some [token ?token]
                    (e/server (settings/save-verbosity user-id change-event))
                    (token))))
              (dom/div (dom/props {:style hint-style})
                (dom/text "Controls detail level of generated flashcards"))))))

      ;; ── Anki Sync section ──
      (let [server-source-mode (e/server (settings/get-source-display-mode user-id))
            !source-mode (atom server-source-mode)
            source-mode (e/watch !source-mode)]
        (dom/div
          (dom/props {:style section-style})
          (dom/h3 (dom/props {:style section-title-style}) (dom/text "Anki Sync"))
          (dom/div
            (dom/props {:style field-style})
            (dom/label (dom/props {:style label-style}) (dom/text "Source Display Mode"))
            (dom/div
              (dom/props {:style {:display "flex" :flex-direction "column" :gap "10px" :margin-top "4px"}})
              (dom/label
                (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                (dom/input
                  (dom/props {:type "radio" :name "source-display-mode" :value "append"
                              :checked (= source-mode "append")
                              :style {:margin-top "3px"}})
                  (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                        [?token ?error] (e/Token change-event)]
                    (when (some? change-event)
                      (reset! !source-mode change-event))
                    (when-some [token ?token]
                      (e/server (settings/save-source-display-mode user-id change-event))
                      (e/server (swap! !settings-refresh inc))
                      (token))))
                (dom/div
                  (dom/span (dom/props {:style {:font-size "14px" :color "#111"}})
                    (dom/text "Append to card"))
                  (dom/div (dom/props {:style hint-style})
                    (dom/text "Source text appended to card content during Anki sync"))))
              (dom/label
                (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                (dom/input
                  (dom/props {:type "radio" :name "source-display-mode" :value "field"
                              :checked (= source-mode "field")
                              :style {:margin-top "3px"}})
                  (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                        [?token ?error] (e/Token change-event)]
                    (when (some? change-event)
                      (reset! !source-mode change-event))
                    (when-some [token ?token]
                      (e/server (settings/save-source-display-mode user-id change-event))
                      (e/server (swap! !settings-refresh inc))
                      (token))))
                (dom/div
                  (dom/span (dom/props {:style {:font-size "14px" :color "#111"}})
                    (dom/text "Separate field"))
                  (dom/div (dom/props {:style hint-style})
                    (dom/text "Source sent as a separate \"Source\" Anki field")))))))))))
