(ns freememo.landing-copy
  "Every word of the public landing page, as data, on both peers.

   Two renderers read this namespace. `freememo.landing-page` renders it through
   Electric for a visitor, and `freememo.crawler-page` renders it to static HTML
   for a crawler. One home per sentence is the point: a crawler that reads
   different words than a visitor sees is what search engines call cloaking.

   ## This namespace MUST stay free of requires

   `freememo.crawler-page` is plain Clojure on the server boot path, and
   `freememo.landing-page` pulls in Electric. A require here would drag Electric's
   compiler into the server's boot, which `dev/load-app-namespaces!` exists to keep
   off the main thread (see CLAUDE.md, dev JVM stack). Pure data needs nothing, so
   this costs nothing.

   ## What is NOT here

   Three blocks stay inline in `freememo.landing-page`, and
   `freememo.crawler-page` carries its own equivalent prose for them: the
   source-paragraph demo with its cloze-marked spans, the four label-and-sentence
   pairs under it, and the contact line with its mailto link. Each composes one
   sentence out of several fragments around inline markup, which no single string
   can hold. Interface chrome is also absent — button labels, form labels and the
   navigation live with the components that render them, because a crawler has no
   use for them.

   See plans/crawler-rendering-and-well-known-files.md.")

(def copy
  "Headings and running prose, keyed by the section that shows them.

   Both renderers read these keys. A key added here is invisible to a crawler
   until `freememo.crawler-page` reads it too — the structure lives in each
   renderer, and only the words live here. `copy-keys-are-read-by-both-renderers`
   is the guard against that drift."
  {:hero/headline "Read it once. Three systems make sure it sticks."
   :hero/sub "Papers, books, scores, codebases, whiteboard photos. Read them incrementally, quiz yourself on what you extracted, push the cards to your own Anki."

   :loops/title "Three loops, three schedules"
   :loops/lead "A flashcard app has one queue. FreeMemo has three, because reading, questions, and cards do not decay at the same rate and should not be scheduled by the same algorithm."

   :objection/title "Why not just have a model generate the whole deck?"
   :objection/lead "Because the dump is usually noise. Generic prompts on whole books produce vague trivia, repeated facts, and cards no one remembers a week later. The fix is structural: generation runs at page granularity, every card is human-reviewed before it syncs, and the source paragraph stays attached so you can audit what came from where. The same paragraph also feeds the quiz loop, where you answer in prose instead of recognising a front."

   :features/title "What's in the box"

   :faq/title "Questions"

   :pact/title "The early-user pact"
   :pact/benefits-heading "What you get"
   :pact/benefits ["Direct line to the team. Bug reports and feature requests land on the developer's screen, not in a queue."
                   "Your feedback ships into the product on a scale of days, not quarters."
                   "Free use of the app while it's pre-production."]
   :pact/limits-heading "What we don't promise"
   :pact/limits ["No SLA. The server might be down for an hour."
                 "No data warranty. Export your cards to Anki regularly."
                 "No long-term price guarantee. Today's free won't be forever."]})

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
     "Or sit a Timed Quiz: forward-only, graded at the end."]]
   ["3" "Sync"
    "Anki's scheduler, once the cards land"
    ["Generate basic, cloze, or overlapping-cloze cards from the page you just read."
     "Mask an image for occlusion, or pair a bar of notation with the audio that plays it."
     "Edit or reject every card before it leaves."
     "One click pushes them to your own Anki collection. Edits you make there pull back, with a diff."]]])

(def features
  "Feature tiles, each [title description]."
  [["Import what you actually read"
    "PDFs, EPUBs, HTML, Markdown, web articles, and pasted text. Wikipedia gets its own cleaner extraction."]
   ["Zotero, without the export dance"
    "Pull a PDF and its citation metadata straight from your local library. Your browser talks to Zotero directly — we never connect to your library, and only the item you pick gets imported."]
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
  "Questions and answers, each [question answer].

   An answer is a string, or a vector of parts when it carries a link. A part is
   `{:t text}`, optionally with `:href`. Both renderers must handle both shapes —
   a renderer that assumes a string drops the answer that has a link."
  [["What does it cost?"
    "The app is free. The steps that call a model spend credits: OCR, card generation, transcription, fact distillation, and grading a quiz answer. Reading, extracting, editing, reviewing, and Anki sync never touch a paid API. Credits are hosted, or you set your own OpenRouter key when self-hosting."]
   ["Where does my reading go?"
    "PDFs, EPUBs, extracted text, and your cards all live on the FreeMemo server, tied to your account — a Zotero import included, once you pick it. Anki sync pushes copies into your local collection over AnkiConnect, and edits you make there pull back. We never connect to Zotero or Anki ourselves: your browser does that over a local connection, and we only ever see the items you chose."]
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
