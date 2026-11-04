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
   off the main thread (see .claude/rules/dev-jvm.md, dev JVM stack). Pure data needs nothing, so
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
  {:hero/headline "Turn what you read into practice you return to."
   :hero/sub "Bring a paper, a book, a score, a codebase, or a whiteboard photo. Read a little, keep the useful passages, and practice with questions and cards. Sync to your own Anki when you want to."

   :loops/title "Read, practice, and revisit"
   :loops/lead "Your reading queue brings documents back when they are due. Quiz gives you spaced practice with questions and basic or cloze cards. Anki sync is optional, with a separate schedule in your own collection."

   :objection/title "Why not just have a model generate the whole deck?"
   :objection/lead "Start with the passage that matters to you. Generate from that passage, then review, edit, or reject the cards before you sync. Each card links back to its source page so you can check what it came from. You can also turn the material into questions and explain your answer in your own words."

   :features/title "What's in the box"

   :faq/title "Questions"

   ;; Each item is [heading text]. The text is a string or a vector of
   ;; `{:t :href}` parts, the same shape as an FAQ answer, so an item can carry a
   ;; link. Every claim here is a commitment to paying customers: see
   ;; plans/paying-customer-commitments.md before you add or soften one.
   :commitments/title "What you can count on"
   :commitments/items [["Support from the developer"
                        "Your email goes to the developer, not to a queue. You get a reply within 3 working days, or within 1 working day if you made a purchase in the last 12 months. Working days are Saturday to Wednesday."]
                       ["Pay for what you use"
                        "There is no subscription. Hosted AI and stored files spend credits. We announce a price change at least 7 days before it starts, in the changelog and in the update email. Credits you already bought keep their balance."]]})

(def loops
  "Three parts of the study workflow. Each entry is
   [number name scheduler steps]. Anki sync is optional."
  [["1" "Read"
    "Your own priority queue"
    ["Import a PDF, an EPUB, an article, a score, a codebase, or photos you take."
     "Read a page at a time. Select a passage and extract it into its own topic."
     "A Socratic tutor asks about the page you're on instead of answering for you."
     "Due date decides what's in today's queue; priority decides the order."]]
   ["2" "Quiz"
    "Spaced practice, paced with FSRS-6"
    ["Turn supported facts from your document into focused questions. Some facts need clearer evidence before they can become questions."
     "Review questions and basic or cloze cards in FreeMemo."
     "Answer in prose. The model grades it ✓ Correct, ◐ Partial, or ✗ Incorrect and names the facts you missed."
     "Or sit a Timed Quiz: forward-only, graded at the end."]]
   ["3" "Sync, if you want to"
    "Your Anki collection has its own schedule"
    ["Generate basic, cloze, or overlapping-cloze cards from the page you just read."
     "Mask an image for occlusion, or pair a bar of notation with the audio that plays it."
     "Review and edit cards before you sync."
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
    "Choose a Clojure repository from GitHub, GitLab, or Forgejo, or upload a .zip. FreeMemo analyzes the sources into a topic tree and a fact graph you can quiz against. Clojure-only for now; the code is read, never run."]
   ["Sheet music and its recording"
    "Score import pairs a notation PDF with an audio file. Drag on the waveform, box the matching bars, get audio ↔ notation cards."]
   ["Spoken material"
    "Import audio and transcribe it into text you can read, extract from, and turn into cards like anything else."]
   ["Compare models before trusting one"
    "Run the same page through two card models, or two OCR models, side by side — then keep the better set."]
   ["Find the paragraph, not just the card"
    "Full-text search across every topic, with card text filtered separately in the Library. Each card links back to the page it came from."]
   ["Built to be driven"
    "Use the Cmd/Ctrl-K palette and shortcuts to scan, extract, generate, and push. Action history can undo supported edits and deletions. Topic deletion has a 12-hour undo window; entity merges cannot be undone."]])

(def faq-items
  "Questions and answers, each [question answer].

   An answer is a string, or a vector of parts when it carries a link. A part is
   `{:t text}`, optionally with `:href`. Both renderers must handle both shapes —
   a renderer that assumes a string drops the answer that has a link."
  [["What does it cost?"
    "There is no subscription. Hosted AI calls spend credits, including OCR, card generation, transcription, fact distillation, and answer grading. When hosted storage metering is enabled, stored files also spend credits over time, even with AI off. Settings shows the storage rate and credit balance. With storage metering enabled, a zero balance pauses video playback and starts a grace period. Video files can be removed after that period; transcripts, extracts, and cards stay. Self-hosting uses your own OpenRouter key and storage configuration."]
   ["Where does my reading go?"
    "In the hosted app, your documents, extracted text, and cards live on the FreeMemo server, tied to your account. A Zotero import goes there once you pick it. Anki sync pushes copies into your local collection over AnkiConnect, and edits you make there pull back. Your browser connects to Zotero and Anki locally. Self-hosted and desktop local libraries use the storage you run."]
   ["Will my cards work in regular Anki?"
    "Yes. FreeMemo installs and owns its own note types — FreeMemo Basic, FreeMemo Cloze, FreeMemo Overlapping Cloze, FreeMemo IO for image occlusion, and FreeMemo Score — so there is nothing for you to map. Cloze cards use Anki's standard {{c1::}} syntax. Sync runs over AnkiConnect, so any standard Anki with the AnkiConnect plugin works."]
   ["Do quiz questions go to Anki too?"
    "Quiz questions stay in FreeMemo. Quiz also reviews basic and cloze cards on a FreeMemo schedule. You can optionally sync cards to Anki, where Anki schedules them separately. A review in one app does not mean a review in the other."]
   ["What can I import?"
    "Documents include PDFs, EPUBs, HTML, Markdown, web articles, Wikipedia, Google Docs, and built Sphinx sites. You can also paste text, photograph pages, transcribe audio or video playlists, and pair sheet music with a recording. Other paths include Zotero PDFs, SuperMemo exports, poem packages, FreeMemo account archives, and Clojure repositories from GitHub, GitLab, Forgejo, or a .zip file."]
   ["Can I run it offline or self-host?"
    [{:t "Self-host, yes — the "}
     {:t "codebase" :href "https://github.com/adham-omran/freememo"}
     {:t " runs locally with Postgres and a JVM, so your reading and cards can live on your own machine. The desktop app also has a local mode without sign-in or sync. That mode is still in testing. AI features such as OCR, generation, transcription, and grading need an internet connection. Desktop local mode hides AI features until you sign in."}]]])
