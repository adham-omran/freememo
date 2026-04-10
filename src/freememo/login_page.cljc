(ns freememo.login-page
  "Login/signup full-page gate."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(e/defn FeatureItem [icon-char title description]
  (e/client
    (dom/div
      (dom/props {:style {:display "flex" :align-items "flex-start" :gap "14px" :margin-bottom "24px"}})
      (dom/div
        (dom/props {:style {:flex-shrink "0" :width "40px" :height "40px" :border-radius "10px"
                            :background "rgba(59,130,246,0.1)" :display "flex" :align-items "center"
                            :justify-content "center" :font-size "18px" :color "var(--color-primary)"
                            :font-weight "700"}})
        (dom/text icon-char))
      (dom/div
        (dom/div
          (dom/props {:style {:font-weight "600" :font-size "15px" :color "var(--color-text-primary)"
                              :margin-bottom "2px"}})
          (dom/text title))
        (dom/div
          (dom/props {:style {:font-size "13.5px" :color "var(--color-text-secondary)" :line-height "1.45"}})
          (dom/text description))))))

(e/defn LeftPanel []
  (e/client
    (dom/div
      (dom/props {:class "login-left"})

      ;; Branding
      (dom/h1
        (dom/props {:style {:font-size "38px" :font-weight "800" :color "var(--color-text-primary)"
                            :margin "0 0 10px" :letter-spacing "-0.5px"}})
        (dom/text "FreeMemo"))
      (dom/p
        (dom/props {:style {:font-size "18px" :color "var(--color-text-secondary)" :margin "0 0 44px"
                            :line-height "1.5" :max-width "420px"}})
        (dom/text "Incremental reading with AI flashcards"))

      ;; Features
      (dom/div
        (dom/props {:style {:max-width "440px"}})
        (FeatureItem "1" "Import your reading"
          "Bring in PDFs, EPUBs, and web articles -- all in one place.")
        (FeatureItem "2" "AI-generated flashcards"
          "Key concepts are extracted automatically and turned into review cards.")
        (FeatureItem "3" "Anki sync"
          "Push cards directly to your Anki collection -- no manual export needed."))

      ;; Pricing note
      (dom/div
        (dom/props {:style {:margin-top "32px" :padding "12px 14px" :background "rgba(59,130,246,0.06)"
                            :border-radius "var(--radius-md)" :font-size "13px" :line-height "1.5"
                            :color "var(--color-text-secondary)" :max-width "440px"}})
        (dom/text "Reading, review, and Anki sync are free. AI features (OCR + card generation) use OpenAI \u2014 bring your own API key, pay only for what you use."))

      )))


(e/defn RightPanel [auth-error]
  (e/client
    (dom/div
      (dom/props {:class "login-right"})
      (dom/div
        (dom/props {:style {:background "var(--color-bg-card)" :padding "40px" :border-radius "var(--radius-lg)"
                            :box-shadow "0 8px 30px rgba(0,0,0,0.12)" :width "100%" :max-width "360px"}})
        (dom/h1
          (dom/props {:style {:text-align "center" :margin-bottom "24px" :color "var(--color-text-primary)"}})

          (dom/text "FreeMemo"))

        ;; Error message
        (when auth-error
          (dom/div
            (dom/props {:style {:background "var(--color-bg-subtle)" :color "var(--color-danger)" :padding "10px" :border-radius "var(--radius-sm)"
                                :margin-bottom "var(--sp-4)" :font-size "14px"}})
            (dom/text auth-error)))

        ;; Google sign-in
        (dom/a
          (dom/props {:href "/auth/google"
                      :class "btn btn-primary" :style {:display "block" :text-align "center" :padding "12px 16px"
                                                       :text-decoration "none" :font-size "16px"}})
          (dom/text "Sign in with Google"))))))

(e/defn LoginPage [auth-error]
  (e/client
    (dom/div
      (dom/props {:class "login-layout"})
      (LeftPanel)
      (RightPanel auth-error))))
