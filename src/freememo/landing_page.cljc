(ns freememo.landing-page
  "Public landing page shown to unauthenticated visitors. Single-hero stealth
   layout; sign-in lives in the top-right header."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(e/defn Header []
  (e/client
    (dom/header
      (dom/props {:class "landing-header"})
      (dom/span
        (dom/props {:class "landing-wordmark"})
        (dom/text "FreeMemo"))
      (dom/a
        (dom/props {:href "/auth/google"
                    :class "btn btn-primary landing-signin"})
        (dom/text "Sign in")))))

(e/defn Hero [auth-error]
  (e/client
    (dom/main
      (dom/props {:class "landing-hero"})
      (when auth-error
        (dom/div
          (dom/props {:class "landing-error"})
          (dom/text auth-error)))
      (dom/h1
        (dom/props {:class "landing-headline"})
        (dom/text "Read, Extract, Remember."))
      (dom/p
        (dom/props {:class "landing-sub"})
        (dom/text "A tool for turning reading into spaced repetition."))
      (dom/p
        (dom/props {:class "landing-features"})
        (dom/text "Import → Extract → Review → Sync"))
      (dom/img
        (dom/props {:class "landing-screenshot"
                    :src "/freememo/landing-hero.png"
                    :alt "FreeMemo workspace: PDF, extracted text, generated flashcards"}))
      (dom/p
        (dom/props {:class "landing-disclaimer"})
        (dom/text "Pre-production. No uptime guarantee, no data guarantee, no SLA. Sign up, demo it — expect to hear from us.")))))

(e/defn LandingPage [auth-error]
  (e/client
    (dom/div
      (dom/props {:class "landing-layout"})
      (Header)
      (Hero auth-error))))
