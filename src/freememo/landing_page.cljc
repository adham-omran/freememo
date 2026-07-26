(ns freememo.landing-page
  "Public landing page for unauthenticated visitors. Multi-section page:
   hero → three-loop diagram → objection rebuttal → feature grid → FAQ →
   early-user pact → footer.

   The page's core claim is that FreeMemo runs three review systems, one per
   kind of knowledge — reading on a priority queue, questions on FSRS-6, cards
   on Anki's own scheduler. Every string here must trace to a shipped
   capability; see plans/reposition-landing-copy-three-loops.md."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(def loops
  "The three review systems, in the order material moves through them.
   Each entry is [number name scheduler steps]. `scheduler` names the algorithm
   that owns that loop's queue — the evidence the whole page rests on."
  [["1" "Read"
    "Your own priority queue"
    ["Import a PDF, an EPUB, an article, a score, a codebase, or photos you take."
     "Read a page at a time. Select a passage and extract it into its own topic."
     "A Socratic tutor asks about the page you're on instead of answering for you."
     "Due date decides what's in today's queue; priority decides the order."]]
   ["2" "Quiz"
    "FSRS-6, the modern spaced-repetition scheduler"
    ["Distill a document into a graph of facts, curated by exception rather than one by one."
     "Every fact becomes an atomic question, generated for you."
     "Answer in prose. The model grades it ✓ Correct, ◐ Partial, or ✗ Incorrect and names the facts you missed."
     "Or sit a timed exam: forward-only, graded at the end."]]
   ["3" "Sync"
    "Anki's scheduler, once the cards land"
    ["Generate basic, cloze, or overlapping-cloze cards from the page you just read."
     "Mask an image for occlusion, or pair a bar of notation with the audio that plays it."
     "Edit or reject every card before it leaves."
     "One click pushes them to your own Anki collection. Edits you make there pull back, with a diff."]]])

(def features
  [["Import what you actually read"
    "PDFs, EPUBs, HTML, Markdown, web articles, and pasted text. Wikipedia gets its own cleaner extraction."]
   ["Zotero, without the export dance"
    "Pull a PDF and its citation metadata straight from your local library. Zotero data goes to your browser directly — never through our server."]
   ["Live Documents"
    "A PDF you keep adding to. Take or upload photos, rotate and crop each one, append them as pages. HEIC converts automatically."]
   ["Codebases as course material"
    "Upload a Clojure repo as a .zip. FreeMemo analyzes the sources into a topic tree and a fact graph you can quiz against. Clojure-only for now; the code is read, never run."]
   ["Sheet music and its recording"
    "Score import pairs a notation PDF with an audio file. Drag on the waveform, box the matching bars, get audio ↔ notation cards."]
   ["Spoken material"
    "Import audio and transcribe it into text you can read, extract from, and turn into cards like anything else."]
   ["Compare models before trusting one"
    "Run the same page through two card models, or two OCR models, side by side — then keep the better set."]
   ["Find the paragraph, not just the card"
    "Full-text search across every topic, with card text filtered separately in the Library. Each card links back to the page it came from."]
   ["Built to be driven"
    "A Cmd/Ctrl-K palette reaches every action, shortcuts cover scan, extract, generate, and push, and an action history undoes what you didn't mean."]])

(def faq-items
  [["What does it cost?"
    "The app is free. The steps that call a model spend credits: OCR, card generation, transcription, fact distillation, and grading a quiz answer. Reading, extracting, editing, reviewing, and Anki sync never touch a paid API. Credits are hosted, or you set your own OpenRouter key when self-hosting."]
   ["Where does my reading go?"
    "PDFs, EPUBs, extracted text, and your cards all live on the FreeMemo server, tied to your account. Anki sync pushes copies into your local collection over AnkiConnect. Zotero imports are the exception: they travel from Zotero straight to your browser and never reach our server."]
   ["Will my cards work in regular Anki?"
    "Yes. FreeMemo installs and owns its own note types — FreeMemo Basic, FreeMemo Cloze, FreeMemo Overlapping Cloze, FreeMemo IO for image occlusion, and FreeMemo Score — so there is nothing for you to map. Cloze cards use Anki's standard {{c1::}} syntax. Sync runs over AnkiConnect, so any standard Anki with the AnkiConnect plugin works."]
   ["Do quiz questions go to Anki too?"
    "No, and that's the point. Cards go to Anki and Anki schedules them. Quiz questions stay in FreeMemo on their own FSRS-6 schedule, because grading a prose answer needs the model. Your reading queue is FreeMemo's as well. Three loops, three schedules."]
   ["What can I import?"
    "PDFs, EPUBs, HTML and Markdown files, web articles by URL, Wikipedia (with cleaner extraction), pasted text, photos from your camera or library, audio for transcription, sheet music paired with a recording, PDFs from your Zotero library, and Clojure code repositories as a .zip."]
   ["Can I run it offline or self-host?"
    [{:t "Self-host, yes — the "}
     {:t "codebase" :href "https://github.com/adham-omran/freememo"}
     {:t " is runnable locally (Postgres + JVM), so your reading and cards can live entirely on your own machine. Fully offline, no — OCR, card generation, transcription, distillation, and quiz grading all call the OpenRouter API, so those steps need an internet connection regardless of where the server runs."}]]
   ["What does \"pre-production\" actually mean for me?"
    "You're an early user. That means direct access to the team and your feedback shapes the product. The flip side: no SLA, no uptime guarantee, no backup warranty. Export your cards regularly if they matter to you."]])

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
        (dom/text "Read it once. Three systems make sure it sticks."))
      (dom/p
        (dom/props {:class "landing-sub"})
        (dom/text "Papers, books, scores, codebases, whiteboard photos. Read them incrementally, quiz yourself on what you extracted, push the cards to your own Anki."))
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
        (dom/text "Three loops, three schedules"))
      (dom/p
        (dom/props {:class "landing-section-lead"})
        (dom/text "A flashcard app has one queue. FreeMemo has three, because reading, questions, and cards do not decay at the same rate and should not be scheduled by the same algorithm."))
      (dom/div
        (dom/props {:class "landing-loop-grid"})
        (e/for-by first [l loops]
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
        (dom/text "Why not just have a model generate the whole deck?"))
      (dom/p
        (dom/props {:class "landing-section-lead"})
        (dom/text "Because the dump is usually noise. Generic prompts on whole books produce vague trivia, repeated facts, and cards no one remembers a week later. The fix is structural: generation runs at page granularity, every card is human-reviewed before it syncs, and the source paragraph stays attached so you can audit what came from where. The same paragraph also feeds the quiz loop, where you answer in prose instead of recognising a front."))
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
