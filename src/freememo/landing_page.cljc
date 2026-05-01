(ns freememo.landing-page
  "Public landing page for unauthenticated visitors. Multi-section page:
   hero → annotated screenshot → 4-step flow → objection rebuttal →
   feature grid → FAQ → early-user pact → footer."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(def callouts
  [["1" "PDF viewer with selectable text"        {:top "50%" :left "22%"}]
   ["2" "Generated cards from the current page" {:top "77%" :left "61%"}]
   ["3" "Cloze syntax with {{c1::deletions}}"   {:top "90%" :left "22%"}]
   ["4" "One-click Anki sync"                   {:top "77%" :left "91%"}]])

(def flow-steps
  [["1" "Import"  "Drop in a PDF, EPUB, or paste a URL."]
   ["2" "Extract" "OCR each page, then generate Q&A or cloze cards from real paragraphs."]
   ["3" "Review"  "Edit every card before it syncs. Reject the bad ones."]
   ["4" "Sync"    "One click pushes approved cards into your Anki collection."]])

(def features
  [["PDF + EPUB import" "Upload books, papers, and articles. Web URLs and Wikipedia work too."]
   ["Basic and Cloze cards" "Pick the format that fits the material. Mix both inside one source."]
   ["Page-anchored context" "Generation reads one page at a time, so cards stay specific instead of vague."]
   ["Prompt customization" "Swap in your own extraction prompt per card type if the defaults aren't sharp enough."]
   ["Anki sync" "One-click push to your local Anki via AnkiConnect. Updates flow back too."]
   ["Search across your library" "Find a card by its source paragraph, not just its front."]])

(def faq-items
  [["What does it cost?"
    "The app is free. Card generation and OCR uses the OpenAI API at the moment; you bring your own API key and pay OpenAI directly for the calls you make. Reading, reviewing, editing, and Anki sync never touch a paid API."]
   ["Where does my reading go?"
    "PDFs, EPUBs, and extracted text live on the FreeMemo server, tied to your account. Cards stay in your browser until you push them to your own Anki collection."]
   #_["What if FreeMemo shuts down?"
    "You can export every card as Anki-compatible CSV at any time, and your Anki collection lives on your machine regardless. Worst case: you lose the reading view — your cards survive."]
   ["Will my cards work in regular Anki?"
    "Yes. Basic cards map to Anki's Basic note type. Cloze cards use Anki's standard {{c1::}} syntax. Sync runs over AnkiConnect, so any standard Anki setup with the AnkiConnect plugin works."]
   ["What can I import?"
    "PDFs, EPUBs, and web articles via URL. Wikipedia is supported as a special case with cleaner extraction."]
   ["Can I run it offline or self-host?"
    "Self-host, yes — the codebase is runnable locally (Postgres + JVM), so your reading and cards can live entirely on your own machine. Fully offline, no — OCR and card generation call the OpenAI API, so those steps need an internet connection regardless of where the server runs."]
   ["What does \"pre-production\" actually mean for me?"
    "You're an early user. That means direct access to the team and your feedback shapes the product. The flip side: no SLA, no uptime guarantee, no backup warranty. Export your cards regularly if they matter to you."]])

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
    (dom/section
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
        (dom/text "Turn PDFs, electronic books, and articles into Anki cards you'll actually remember."))
      (dom/div
        (dom/props {:class "landing-cta-row"})
        (dom/a
          (dom/props {:href "/auth/google"
                      :class "btn btn-primary landing-cta-primary"})
          (dom/text "Get early access →"))
        (dom/a
          (dom/props {:href "#how-it-works"
                      :class "landing-cta-secondary"})
          (dom/text "How it works ↓"))))))

(e/defn ProductShot []
  (e/client
    (dom/section
      (dom/props {:class "landing-product"})
      (dom/figure
        (dom/props {:class "landing-product-figure"})
        (dom/div
          (dom/props {:class "landing-product-frame"})
          (dom/img
            (dom/props {:class "landing-screenshot"
                        :src "/freememo/landing-hero.png"
                        :alt "FreeMemo workspace: PDF, extracted text, and generated flashcards"}))
          (e/for-by first [c callouts]
            (let [[n _label pos] c]
              (dom/span
                (dom/props {:class "landing-pin"
                            :style pos})
                (dom/text n)))))
        (dom/ol
          (dom/props {:class "landing-legend"})
          (e/for-by first [c callouts]
            (let [[n label _pos] c]
              (dom/li
                (dom/props {:class "landing-legend-item"})
                (dom/span (dom/props {:class "landing-legend-num"}) (dom/text n))
                (dom/span (dom/text label))))))))))

(e/defn HowItWorks []
  (e/client
    (dom/section
      (dom/props {:id "how-it-works" :class "landing-flow"})
      (dom/h2
        (dom/props {:class "landing-section-title"})
        (dom/text "How it works"))
      (dom/div
        (dom/props {:class "landing-flow-grid"})
        (e/for-by first [step flow-steps]
          (let [[n title desc] step]
            (dom/div
              (dom/props {:class "landing-flow-step"})
              (dom/span (dom/props {:class "landing-step-num"}) (dom/text n))
              (dom/h3 (dom/props {:class "landing-step-title"}) (dom/text title))
              (dom/p (dom/props {:class "landing-step-desc"}) (dom/text desc)))))))))

(e/defn Objection []
  (e/client
    (dom/section
      (dom/props {:class "landing-objection"})
      (dom/h2
        (dom/props {:class "landing-section-title"})
        (dom/text "Why not just let an LLM dump cards into Anki?"))
      (dom/p
        (dom/props {:class "landing-section-lead"})
        (dom/text "Because the dump is usually noise. Generic prompts on whole books produce vague trivia, repeated facts, and cards no one remembers a week later. The fix is structural: generation runs at page granularity, every card is human-reviewed before it syncs, and the source paragraph stays attached so you can audit what came from where."))
      (dom/div
        (dom/props {:class "landing-demo"})
        (dom/div
          (dom/props {:class "landing-demo-source"})
          (dom/h4 (dom/props {:class "landing-demo-label"}) (dom/text "Source paragraph"))
          (dom/p
            (dom/text "Mitochondria are membrane-bound organelles found in most eukaryotic cells. They generate most of the cell's supply of ATP through oxidative phosphorylation, a process that takes place across the inner mitochondrial membrane.")))
        (dom/div
          (dom/props {:class "landing-demo-cards"})
          (dom/h4 (dom/props {:class "landing-demo-label"}) (dom/text "Generated cards"))
          (dom/article
            (dom/props {:class "demo-card"})
            (dom/span (dom/props {:class "demo-card-tag"}) (dom/text "Basic"))
            (dom/p (dom/props {:class "demo-card-q"}) (dom/text "What process do mitochondria use to generate the cell's ATP?"))
            (dom/p (dom/props {:class "demo-card-a"}) (dom/text "Oxidative phosphorylation, across the inner mitochondrial membrane.")))
          (dom/article
            (dom/props {:class "demo-card"})
            (dom/span (dom/props {:class "demo-card-tag"}) (dom/text "Cloze"))
            (dom/p
              (dom/props {:class "demo-card-cloze"})
              (dom/text "Mitochondria generate ATP through ")
              (dom/span (dom/props {:class "demo-cloze-mark"}) (dom/text "{{c1::oxidative phosphorylation}}"))
              (dom/text ", across the ")
              (dom/span (dom/props {:class "demo-cloze-mark"}) (dom/text "{{c2::inner mitochondrial membrane}}"))
              (dom/text ".")))))
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
        (dom/text "What's in the box"))
      (dom/div
        (dom/props {:class "landing-feature-grid"})
        (e/for-by first [f features]
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
        (dom/text "Questions"))
      (dom/div
        (dom/props {:class "landing-faq-list"})
        (e/for-by first [item faq-items]
          (let [[q a] item]
            (dom/div
              (dom/props {:class "landing-faq-item"})
              (dom/h3 (dom/props {:class "landing-faq-q"}) (dom/text q))
              (dom/p (dom/props {:class "landing-faq-a"}) (dom/text a)))))))))

(e/defn EarlyUserPact []
  (e/client
    (dom/section
      (dom/props {:class "landing-pact"})
      (dom/div
        (dom/props {:class "landing-pact-card"})
        (dom/h2
          (dom/props {:class "landing-pact-title"})
          (dom/text "The early-user pact"))
        (dom/div
          (dom/props {:class "landing-pact-grid"})
          (dom/div
            (dom/h4 (dom/props {:class "landing-pact-h"}) (dom/text "What you get"))
            (dom/ul
              (dom/li (dom/text "Direct line to the team. Bug reports and feature requests land on the developer's screen, not in a queue."))
              (dom/li (dom/text "Your feedback ships into the product on a scale of days, not quarters."))
              (dom/li (dom/text "Free use of the app while it's pre-production."))))
          (dom/div
            (dom/h4 (dom/props {:class "landing-pact-h"}) (dom/text "What we don't promise"))
            (dom/ul
              (dom/li (dom/text "No SLA. The server might be down for an hour."))
              (dom/li (dom/text "No data warranty. Export your cards to Anki regularly."))
              (dom/li (dom/text "No long-term price guarantee. Today's free won't be forever.")))))
        (dom/p
          (dom/props {:class "landing-pact-contact"})
          (dom/text "Questions, bug reports, or feature ideas: ")
          (dom/a
            (dom/props {:href "mailto:contact@adham-omran.com"})
            (dom/text "contact@adham-omran.com")))))))

(e/defn Footer []
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
        (dom/a (dom/props {:href "/auth/google"}) (dom/text "Sign in"))))))

(e/defn LandingPage [auth-error]
  (e/client
    (dom/div
      (dom/props {:class "landing-layout"})
      (Header)
      (dom/main
        (dom/props {:class "landing-main"})
        (Hero auth-error)
        (ProductShot)
        (HowItWorks)
        (Objection)
        (Features)
        (FAQ)
        (EarlyUserPact))
      (Footer))))
