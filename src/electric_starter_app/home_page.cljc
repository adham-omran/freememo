(ns electric-starter-app.home-page
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]
    #?(:clj [electric-starter-app.settings :as settings])))

(defn get-api-key-status* [user-id enc-key]
  #?(:clj (settings/get-openai-api-key-status user-id enc-key)
     :cljs nil))

(e/defn HomePage [navigate! user-id enc-key]
  (e/client
    (let [api-status (e/server (get-api-key-status* user-id enc-key))
          configured? (:configured? api-status)]
      (dom/div
        (dom/props {:style {:padding "40px" :max-width "700px" :margin "0 auto"}})

        (dom/h1
          (dom/props {:style {:font-size "2.5rem" :margin-bottom "8px"}})
          (dom/text "FreeMemo"))

        (dom/p
          (dom/props {:style {:font-size "1.2rem" :color "#555" :margin-bottom "32px"}})
          (dom/text "Turn documents into flashcards with AI"))

        ;; Early iteration notice
        (dom/div
          (dom/props {:style {:background "#fff8e1" :border "1px solid #ffe082" :border-radius "8px"
                              :padding "16px" :margin-bottom "32px"}})
          (dom/p
            (dom/props {:style {:margin "0" :color "#795548"}})
            (dom/text "Early iteration — the vision is to become an Incremental Reading companion to Anki, similar in features to SuperMemo.")))

        ;; API key status
        (dom/div
          (dom/props {:style {:padding "12px 16px" :border-radius "8px" :margin-bottom "24px"
                              :background (if configured? "#f0fdf4" "#fefce8")
                              :border (str "1px solid " (if configured? "#bbf7d0" "#fde68a"))}})
          (dom/span
            (dom/props {:style {:font-size "14px" :color (if configured? "#166534" "#854d0e")}})
            (dom/text (if configured?
                        "OpenAI API key configured — AI features are ready."
                        "Set up your OpenAI API key in Settings to enable AI features.")))
          (when-not configured?
            (dom/button
              (dom/props {:style {:margin-left "12px" :padding "4px 12px" :background "#2563eb"
                                  :color "white" :border "none" :border-radius "4px"
                                  :cursor "pointer" :font-size "13px"}})
              (dom/text "Go to Settings")
              (dom/On "click" (fn [_] (navigate! :settings)) nil))))

        ;; Workflow steps
        (dom/h2
          (dom/props {:style {:margin-bottom "16px"}})
          (dom/text "How it works"))

        (dom/ol
          (dom/props {:style {:padding-left "20px" :line-height "2"}})
          (dom/li (dom/text "Import content (PDF, web article, or URL)"))
          (dom/li (dom/text "AI extracts and structures the text"))
          (dom/li (dom/text "Generate flashcards from your content"))
          (dom/li (dom/text "Review with spaced repetition or export to Anki")))

        ;; Get Started button
        (dom/button
          (dom/props {:style {:margin-top "32px" :padding "12px 32px" :background "#2563eb"
                              :color "white" :border "none" :border-radius "6px"
                              :cursor "pointer" :font-size "16px" :font-weight "600"}})
          (dom/text "Get Started →")
          (dom/On "click" (fn [_] (navigate! :pdf)) nil))))))
