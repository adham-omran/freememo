(ns electric-starter-app.home-page
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]))

(e/defn HomePage [navigate!]
  (e/client
    (dom/div
      (dom/props {:style {:padding "40px" :max-width "700px" :margin "0 auto"}})

      (dom/h1
        (dom/props {:style {:font-size "2.5rem" :margin-bottom "8px"}})
        (dom/text "FreeMemo"))

      (dom/p
        (dom/props {:style {:font-size "1.2rem" :color "#555" :margin-bottom "32px"}})
        (dom/text "Turn your PDFs into flashcards with AI"))

      ;; Early iteration notice
      (dom/div
        (dom/props {:style {:background "#fff8e1" :border "1px solid #ffe082" :border-radius "8px"
                            :padding "16px" :margin-bottom "32px"}})
        (dom/p
          (dom/props {:style {:margin "0" :color "#795548"}})
          (dom/text "Early iteration — the vision is to become an Incremental Reading companion to Anki, similar in features to SuperMemo.")))

      ;; Workflow steps
      (dom/h2
        (dom/props {:style {:margin-bottom "16px"}})
        (dom/text "How it works"))

      (dom/ol
        (dom/props {:style {:padding-left "20px" :line-height "2"}})
        (dom/li (dom/text "Upload a PDF"))
        (dom/li (dom/text "AI extracts text with OCR"))
        (dom/li (dom/text "AI generates flashcards"))
        (dom/li (dom/text "Export to Anki")))

      ;; Get Started button
      (dom/button
        (dom/props {:style {:margin-top "32px" :padding "12px 32px" :background "#2563eb"
                            :color "white" :border "none" :border-radius "6px"
                            :cursor "pointer" :font-size "16px" :font-weight "600"}})
        (dom/text "Get Started →")
        (dom/On "click" (fn [_] (navigate! :pdf)) nil)))))
