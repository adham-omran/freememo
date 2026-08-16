(ns freememo.landing-page
  "Public landing page for unauthenticated visitors. Multi-section page:
   hero → three-loop diagram → objection rebuttal → feature grid → FAQ →
   early-user pact → footer.

   The page's core claim is that FreeMemo runs three review systems, one per
   kind of knowledge — reading on a priority queue, questions on FSRS-6, cards
   on Anki's own scheduler. Every string here must trace to a shipped
   capability; see plans/reposition-landing-copy-three-loops.md.

   **The words live in `freememo.landing-copy`, not here.** This namespace owns
   structure and markup only. `freememo.crawler-page` renders the same data to
   static HTML for crawlers, which cannot run this page at all — Googlebot's
   renderer has no WebSocket support, so a crawler sees an empty body without it.
   Three blocks are the exception and keep their strings inline, because each
   composes one sentence from fragments around inline markup: the source-paragraph
   demo, the four label-and-sentence pairs under it, and the contact line. The
   crawler renderer carries its own equivalent prose for those three.
   See plans/crawler-rendering-and-well-known-files.md."
  (:require
   [freememo.landing-copy :as lc]
   [freememo.legal-copy :as legal]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(defn show-google?
  "Whether to surface Google sign-in controls for the given AUTH_MODE keyword."
  [auth-mode]
  (or (= auth-mode :google) (= auth-mode :both)))

(defn show-password?
  "Whether to surface the username/password form for the given AUTH_MODE keyword."
  [auth-mode]
  (or (= auth-mode :password) (= auth-mode :both)))

(e/defn LoginForm [variant]
  ;; Native POST to /login: the session cookie is set on the HTTP response by
  ;; Ring middleware and cannot be set over Electric's WebSocket (G-2). Browser
  ;; submission navigates; no e/Token. Documented Forms5 exception.
  (e/client
    (dom/form
      (dom/props {:method "post" :action "/login"
                  :class (str "landing-login-form landing-login-" (name variant))})
      ;; Visible labels (WCAG 3.3.2) — placeholder alone vanishes on entry.
      ;; Ids carry the variant: the form mounts twice on the landing page.
      (let [user-id (str "login-user-" (name variant))
            pass-id (str "login-password-" (name variant))]
        (dom/label
          (dom/props {:for user-id :class "landing-login-label"})
          (dom/text "Username"))
        (dom/input
          (dom/props {:id user-id :type "text" :name "user" :placeholder "Username"
                      :class "input" :autocomplete "username" :required true}))
        (dom/label
          (dom/props {:for pass-id :class "landing-login-label"})
          (dom/text "Password"))
        (dom/input
          (dom/props {:id pass-id :type "password" :name "password" :placeholder "Password"
                      :class "input" :autocomplete "current-password" :required true})))
      (dom/button
        (dom/props {:type "submit" :class "btn btn-primary landing-cta-primary"})
        (dom/text "Sign in")))))

(e/defn Header [auth-mode]
  (e/client
    (dom/header
      (dom/props {:class "landing-header"})
      (dom/span
        (dom/props {:class "landing-wordmark"})
        (dom/text "FreeMemo"))
      (when (show-google? auth-mode)
        (dom/a
          (dom/props {:href "/auth/google"
                      :class "btn btn-primary landing-signin"})
          (dom/text "Sign in"))))))

(e/defn Hero [auth-error auth-mode]
  (e/client
    (dom/section
      (dom/props {:class "landing-hero"})
      (when auth-error
        (dom/div
          (dom/props {:class "landing-error"})
          (dom/text auth-error)))
      (dom/h1
        (dom/props {:class "landing-headline"})
        (dom/text (:hero/headline lc/copy)))
      (dom/p
        (dom/props {:class "landing-sub"})
        (dom/text (:hero/sub lc/copy)))
      (when (show-password? auth-mode)
        (LoginForm :hero))
      (dom/div
        (dom/props {:class "landing-cta-row"})
        (when (show-google? auth-mode)
          (dom/a
            (dom/props {:href "/auth/google"
                        :class "btn btn-primary landing-cta-primary"})
            (dom/text "Get early access →")))
        (dom/a
          (dom/props {:href "#how-it-works"
                      :class "landing-cta-secondary"})
          (dom/text "How it works ↓"))))))

(e/defn ThreeLoops []
  ;; Carries id="how-it-works" — Hero's secondary CTA anchors to it. Renaming
  ;; the id here requires updating that href.
  (e/client
    (dom/section
      (dom/props {:id "how-it-works" :class "landing-loops"})
      (dom/h2
        (dom/props {:class "landing-section-title"})
        (dom/text (:loops/title lc/copy)))
      (dom/p
        (dom/props {:class "landing-section-lead"})
        (dom/text (:loops/lead lc/copy)))
      (dom/div
        (dom/props {:class "landing-loop-grid"})
        (e/for-by first [l lc/loops]
          (let [[n nm sched steps] l]
            (dom/div
              (dom/props {:class "landing-loop-card"})
              (dom/span (dom/props {:class "landing-loop-num"}) (dom/text n))
              (dom/h3 (dom/props {:class "landing-loop-name"}) (dom/text nm))
              (dom/p (dom/props {:class "landing-loop-sched"}) (dom/text sched))
              (dom/ul
                (dom/props {:class "landing-loop-list"})
                (e/for-by identity [s steps]
                  (dom/li (dom/text s)))))))))))

(e/defn Objection []
  (e/client
    (dom/section
      (dom/props {:class "landing-objection"})
      (dom/h2
        (dom/props {:class "landing-section-title"})
        (dom/text (:objection/title lc/copy)))
      (dom/p
        (dom/props {:class "landing-section-lead"})
        (dom/text (:objection/lead lc/copy)))
      (dom/div
        (dom/props {:class "landing-demo"})
        (dom/div
          (dom/props {:class "landing-demo-source"})
          (dom/h4 (dom/props {:class "landing-demo-label"}) (dom/text "Source paragraph"))
          (dom/p
            (dom/text "Mitochondria are membrane-bound organelles found in most eukaryotic cells. They generate most of the cell's supply of ATP through oxidative phosphorylation, a process that takes place across the inner mitochondrial membrane.")))
        (dom/div
          (dom/props {:class "landing-demo-cards"})
          (dom/h4 (dom/props {:class "landing-demo-label"}) (dom/text "What it becomes"))
          (dom/article
            (dom/props {:class "demo-card"})
            (dom/span (dom/props {:class "demo-card-tag"}) (dom/text "Basic → Anki"))
            (dom/p (dom/props {:class "demo-card-q"}) (dom/text "What process do mitochondria use to generate the cell's ATP?"))
            (dom/p (dom/props {:class "demo-card-a"}) (dom/text "Oxidative phosphorylation, across the inner mitochondrial membrane.")))
          (dom/article
            (dom/props {:class "demo-card"})
            (dom/span (dom/props {:class "demo-card-tag"}) (dom/text "Cloze → Anki"))
            (dom/p
              (dom/props {:class "demo-card-cloze"})
              (dom/text "Mitochondria generate ATP through ")
              (dom/span (dom/props {:class "demo-cloze-mark"}) (dom/text "{{c1::oxidative phosphorylation}}"))
              (dom/text ", across the ")
              (dom/span (dom/props {:class "demo-cloze-mark"}) (dom/text "{{c2::inner mitochondrial membrane}}"))
              (dom/text ".")))
          (dom/article
            (dom/props {:class "demo-card"})
            (dom/span (dom/props {:class "demo-card-tag"}) (dom/text "Quiz → stays here"))
            (dom/p (dom/props {:class "demo-card-q"}) (dom/text "Where in the mitochondrion does ATP synthesis take place, and by what process?"))
            (dom/p (dom/props {:class "demo-card-a"}) (dom/text "◐ Partial — you named oxidative phosphorylation but not the inner mitochondrial membrane. Graded on your prose, not a self-rating.")))))
      (dom/ul
        (dom/props {:class "landing-objection-list"})
        (dom/li
          (dom/strong (dom/text "Page-level context. "))
          (dom/text "Generation looks at one page at a time, not the whole book, so cards stay tied to a specific paragraph instead of mushing chapters together."))
        (dom/li
          (dom/strong (dom/text "Human review before sync. "))
          (dom/text "Cards land in an editable table. Edit, reject, or rewrite before anything reaches Anki."))
        (dom/li
          (dom/strong (dom/text "Cloze precision. "))
          (dom/text "The model is prompted to emit valid Anki cloze syntax, with each deletion a single concept — not a half-sentence."))
        (dom/li
          (dom/strong (dom/text "Source traceability. "))
          (dom/text "Every card carries a link back to its source page. If a card looks wrong, find the paragraph it came from in two clicks."))))))

(e/defn Features []
  (e/client
    (dom/section
      (dom/props {:class "landing-features-section"})
      (dom/h2
        (dom/props {:class "landing-section-title"})
        (dom/text (:features/title lc/copy)))
      (dom/div
        (dom/props {:class "landing-feature-grid"})
        (e/for-by first [f lc/features]
          (let [[title desc] f]
            (dom/div
              (dom/props {:class "landing-feature-tile"})
              (dom/h3 (dom/props {:class "landing-feature-title"}) (dom/text title))
              (dom/p (dom/props {:class "landing-feature-desc"}) (dom/text desc)))))))))

(e/defn FAQ []
  (e/client
    (dom/section
      (dom/props {:class "landing-faq"})
      (dom/h2
        (dom/props {:class "landing-section-title"})
        (dom/text (:faq/title lc/copy)))
      (dom/div
        (dom/props {:class "landing-faq-list"})
        (e/for-by first [item lc/faq-items]
          (let [[q a] item]
            (dom/div
              (dom/props {:class "landing-faq-item"})
              (dom/h3 (dom/props {:class "landing-faq-q"}) (dom/text q))
              (dom/p (dom/props {:class "landing-faq-a"})
                (if (string? a)
                  (dom/text a)
                  (e/for-by :t [frag a]
                    (if (:href frag)
                      (dom/a (dom/props {:href (:href frag) :target "_blank" :rel "noopener"})
                        (dom/text (:t frag)))
                      (dom/text (:t frag)))))))))))))

(e/defn EarlyUserPact []
  (e/client
    (dom/section
      (dom/props {:class "landing-pact"})
      (dom/div
        (dom/props {:class "landing-pact-card"})
        (dom/h2
          (dom/props {:class "landing-pact-title"})
          (dom/text (:pact/title lc/copy)))
        (dom/div
          (dom/props {:class "landing-pact-grid"})
          (dom/div
            (dom/h4 (dom/props {:class "landing-pact-h"}) (dom/text (:pact/benefits-heading lc/copy)))
            (dom/ul
              (e/for-by identity [b (:pact/benefits lc/copy)]
                (dom/li (dom/text b)))))
          (dom/div
            (dom/h4 (dom/props {:class "landing-pact-h"}) (dom/text (:pact/limits-heading lc/copy)))
            (dom/ul
              (e/for-by identity [l (:pact/limits lc/copy)]
                (dom/li (dom/text l))))))
        (dom/p
          (dom/props {:class "landing-pact-contact"})
          (dom/text "Questions, bug reports, or feature ideas: ")
          (dom/a
            (dom/props {:href "mailto:contact@adham-omran.com"})
            (dom/text "contact@adham-omran.com")))))))

(e/defn Footer [auth-mode]
  (e/client
    (dom/footer
      (dom/props {:class "landing-footer"})
      (dom/span (dom/props {:class "landing-footer-brand"}) (dom/text "FreeMemo"))
      (dom/nav
        (dom/props {:class "landing-footer-nav"})
        (dom/a (dom/props {:href "mailto:contact@adham-omran.com"}) (dom/text "Contact"))
        (dom/a (dom/props {:href "https://github.com/adham-omran/freememo"
                           :target "_blank" :rel "noopener"})
          (dom/text "Source"))
        (dom/a (dom/props {:href legal/privacy-path}) (dom/text "Privacy"))
        (when (show-google? auth-mode)
          (dom/a (dom/props {:href "/auth/google"}) (dom/text "Sign in")))))))

(e/defn LandingPage [auth-error auth-mode]
  (e/client
    (dom/div
      (dom/props {:class "landing-layout"})
      (Header auth-mode)
      (dom/main
        (dom/props {:class "landing-main"})
        (Hero auth-error auth-mode)
        (ThreeLoops)
        (Objection)
        (Features)
        (FAQ)
        (EarlyUserPact))
      (Footer auth-mode))))
