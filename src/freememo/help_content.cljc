(ns freememo.help-content
  "Help's content and the lookups over it, as plain data and plain fns.

   The Help page renders it and the command palette searches it, so it
   requires no Electric namespace — the palette must not require a page
   (plans/searchable-help.md 1.1). Text is resolved through `resolved-text`,
   the one place chord tokens become the chord a user sees, so display and
   search can never disagree about what a step says."
  (:require
   [clojure.string :as str]
   [freememo.commands :as commands]))

(def sections
  "Every Help section, in reading order.

   `:slug` is hand-written and never derived from `:title`, so renaming a
   title breaks no /help/<slug> link. A step's `:commands` holds the registry
   ids that step documents; `undocumented` holds the rest, and
   help_content_test asserts the two cover the registry between them."
  [{:slug "getting-around"
    :title "Getting around"
    :intro "The sidebar at the left holds every place in FreeMemo, in the order you use them: import, read, review, plan, explore."
    :steps [{:label "Open the sidebar"
             :detail "Rest the pointer on the sidebar, or press Tab to move into it. It opens over the page and closes when you leave it. Open Sidebar in the command palette puts focus in it, and Esc closes it."
             :commands #{:open-nav-rail}}
            {:label "Keep it open ({toggle-nav-rail})"
             :detail "Keep open, at the foot of the sidebar, docks it beside the page. In a window 900px wide or less it opens over the page instead. On a phone, ☰ opens it."
             :commands #{:toggle-nav-rail}}
            {:label "Continue"
             :detail "Continue opens the last document you opened in the Viewer. A learn session and a subset review do not change it: Continue returns to the document you chose to read, and a learn session picks its items from a queue."
             :commands #{:go-continue}}
            {:label "Learn's count"
             :detail "The number on Learn is how many topics are due, the same count Home shows."
             :commands #{}}]}
   {:slug "importing-content"
    :title "Importing content"
    :intro "Get PDFs, EPUBs, articles, and photos in. Everything starts on the Import tab."
    :steps [{:label "Upload a file"
             :detail "Import → Upload. Drop a PDF, EPUB, HTML, Markdown, or code-repo (.zip) file (or click to browse); PDFs, EPUBs, and code repos ask \"Import as …\" first. Drag-and-drop onto the modal works too."
             :commands #{:import-upload :import-file}}
            {:label "Import from a link"
             :detail "Import → Link, paste a web or Wikipedia URL, and Fetch. A URL that resolves to a PDF or EPUB is offered for import."
             :commands #{:import-link}}
            {:label "Import from Zotero"
             :detail "Import → Zotero opens your library; filter and click an item to pull its PDF. First turn it on in Settings → Zotero (Enable Zotero import, then Test Connection) with the FreeMemo for Zotero plugin installed."
             :commands #{:import-zotero}}
            {:label "Live Document"
             :detail "Import → Live Document makes a PDF you keep adding to. Tap ＋ to upload or take photos, rotate and crop each in the editor, then Done to append them as pages; HEIC photos convert automatically."
             :commands #{:create-live-doc :append-images}}]}
   {:slug "studying-a-pdf"
    :title "Studying a PDF"
    :intro "Turn a PDF into studied, card-ready material, one page at a time."
    :steps [{:label "Import it"
             :detail "Import tab → Upload (PDF or EPUB) or Link (a web or Wikipedia URL). Drag-and-drop works too."
             :commands #{}}
            {:label "Read & navigate"
             :detail "Open it from Library, or from Continue in the sidebar. The page arrows or the page box move you; the zoom menu fits width or whole page."
             :commands #{:prev-page :next-page}}
            {:label "Extract the text ({scan})"
             :detail "Scan Page runs OCR on the current page into clean, editable HTML. Copy-text pulls the PDF's own text layer when it has one."
             :commands #{:scan :copy-text}}
            {:label "Edit & split ({extract})"
             :detail "Fix the text in the editor. Select a passage and Extract turns it into a child topic linked back to the page."
             :commands #{:extract}}
            {:label "Mark done ({done})"
             :detail "Mark a page Done to drop it from your review queue while keeping the content."
             :commands #{:done}}]}
   {:slug "boox-neoreader-highlights"
    :title "Boox NeoReader highlights"
    :intro "Bring the highlights and notes you made on a Boox e-reader into the EPUB or PDF you read them in. Google Drive carries the file; nothing is written back to it or to the device."
    :steps [{:label "Export on the device"
             :detail "In NeoReader, export the book's annotations as HTML. The file lands in onyx / your device name / Read in your Google Drive."
             :commands #{}}
            {:label "Extract the text first (PDF only)"
             :detail "A PDF's pages hold no text until you Scan Page or Copy text on them, and a page with no text can hold no highlight. Copy text matches your Boox export most closely, because both read the PDF's own text layer."
             :commands #{}}
            {:label "Start the sync"
             :detail "Library → Documents → the document's ⋯ menu → Sync with NeoReader. The first run asks Google for read access to one file, then opens Google's own file picker."
             :commands #{:neoreader-sync}}
            {:label "Pick the export"
             :detail "Go to onyx → your device → Read and pick the book's HTML file. FreeMemo remembers it, so every later sync is one click with no picker."
             :commands #{}}
            {:label "Read the result"
             :detail "Each highlighted passage is tinted orange in its chapter or page. A passage you annotated also carries a [1] marker, and that chapter or page ends with a numbered USER NOTES list."
             :commands #{}}
            {:label "Check what did not place"
             :detail "A highlight FreeMemo cannot find in the text is listed under UNPLACED HIGHLIGHTS — on the document itself for an EPUB, on page 1 for a PDF. An unscanned page is the usual cause on a PDF; editing a sentence after you highlighted it is the usual cause on either."
             :commands #{}}
            {:label "Sync again any time"
             :detail "Every sync rebuilds from the export. A highlight you deleted on the device loses its mark, and your own edits inside a USER NOTES list do not survive."
             :commands #{}}]}
   {:slug "ai-assistant"
    :title "Asking the AI assistant"
    :intro "Work a page through with an assistant that either answers you or questions you — your choice."
    :steps [{:label "Open it"
             :detail "Open the Assistant panel on the right. New Chat starts a saved conversation about the current document."
             :commands #{:toggle-assistant-pane}}
            {:label "Set the panel height"
             :detail "Settings → AI Features → Assistant panel height. Content area stops above the card table. Full workspace extends beside it."
             :commands #{}}
            {:label "Pick a mode"
             :detail "Use the Mode dropdown below the composer. General answers directly. Tutor asks guiding questions. Socratic uses a fixed learning structure. Each chat keeps its mode."
             :commands #{}}
            {:label "Ask about the page"
             :detail "Type a question about what you're reading. Every mode is grounded in the page in front of you. Markdown and math render inline."
             :commands #{}}
            {:label "Reference another document"
             :detail "Type @ in the composer and pick a document. The menu stays above the composer, and the selected document becomes a removable chip."
             :commands #{}}
            {:label "Check source evidence"
             :detail "Select an evidence link to read its retained excerpt. Evidence from another document includes Open document in new tab."
             :commands #{}}
            {:label "Pick its model"
             :detail "Use the Model dropdown below the composer to set the model for this document. Leave it on your default otherwise."
             :commands #{}}
            {:label "Manage an answer"
             :detail "Answers stream into the saved chat. Stop an active answer. Answer details shows its request, timing, provider, and sources. Failed answers offer Retry."
             :commands #{}}]}
   {:slug "reading-schedule"
    :title "Planning a reading schedule"
    :intro "Plan a document page range to a target date, with reading and Anki review work in one forecast."
    :steps [{:label "Create a document schedule"
             :detail "Schedule → New schedule. Pick a document, target date, page range, and daily review capacity. A blank last page means the document end."
             :commands #{}}
            {:label "Read the dashboard"
             :detail "Curriculum schedules and document schedules appear separately. A curriculum card shows its remaining study minutes. Select a document schedule to open its forecast."
             :commands #{}}
            {:label "Follow the reading pill"
             :detail "The Viewer toolbar shows today's page quota and deadline for the schedule that covers the current page. Hover to see your pace."
             :commands #{}}
            {:label "Inspect the plan"
             :detail "Study plan is the default view. It lists daily pages and reviews through card maturation. Switch to Pages for each page's state."
             :commands #{}}
            {:label "Handle a past target"
             :detail "After the target passes, the report shows the overdue time, page coverage, and expected card maturation date. You can move the target."
             :commands #{}}
            {:label "Archive or restore"
             :detail "Archive removes a schedule from active work and its daily load. Open Archived in the schedule list to restore it."
             :commands #{}}]}
   {:slug "building-a-curriculum"
    :title "Building a curriculum"
    :intro "Arrange imported material into an ordered study path with blocks, schedule controls, and progress gates."
    :steps [{:label "Create it"
             :detail "Curriculum → New curriculum. Name it and set a target date, daily study minutes, study days, or a useful combination."
             :commands #{}}
            {:label "Add blocks and materials"
             :detail "Add blocks in study order. In an unlocked block, Add material searches all imported sources. Use the type filters to narrow the list."
             :commands #{}}
            {:label "Order the work"
             :detail "Use the arrow controls to move blocks and materials. Edit a material's minute estimate when the default is wrong."
             :commands #{}}
            {:label "Set the focus"
             :detail "Make one available block the focus. Later blocks unlock after you pass the prior block exam. The override can skip that exam."
             :commands #{}}
            {:label "Track readiness"
             :detail "Mark a material studied, build its Anki cards, and pass its exam. The curriculum schedule shows the next tasks and estimated finish."
             :commands #{}}
            {:label "Edit or delete"
             :detail "Edit schedule controls from the curriculum header. Use the pencil to rename a block. Delete curriculum hides its blocks and exams for 12 hours. Undo restores them. Imported documents stay."
             :commands #{:curriculum-change :rename-block :delete-curriculum}}]}
   {:slug "curriculum-exams"
    :title "Taking curriculum exams"
    :intro "Generate evidence-bound material and block exams, save answer drafts, and keep a reviewable grade history."
    :steps [{:label "Generate an exam"
             :detail "Select Generate exam on a material or block. Generation shows progress and survives page changes. Use Activity to monitor or cancel it."
             :commands #{}}
            {:label "Review and approve"
             :detail "The exam route shows the blueprint, evidence, and questions. Use Edit to change a draft. Approve freezes the version used by later sittings."
             :commands #{}}
            {:label "Take or resume it"
             :detail "Take exam opens five answer fields. Draft answers save when you leave a field. Reloading the sitting route restores the same exam and elapsed time."
             :commands #{:save-exam-answer-draft}}
            {:label "Grade it"
             :detail "Submit freezes the answers and requests an AI grade. Use Regrade with AI when needed. A failed result can create a targeted retry exam."
             :commands #{}}
            {:label "Cancel or archive"
             :detail "Cancel this sitting closes an unsubmitted attempt without a grade. Archive removes a cancelled sitting from the list. Submitted sittings remain in history."
             :commands #{:cancel-exam-sitting :archive-exam-sitting}}
            {:label "Understand credits"
             :detail "Paid actions show a credit notice before use. Completed forms and grades show their recorded charge. AI Costs lists every returned model charge."
             :commands #{}}]}
   {:slug "making-a-deck"
    :title "Making a deck"
    :intro "Generate flashcards from the page you're reading, then curate them."
    :steps [{:label "Generate ({generate})"
             :detail "Pick Basic or Cloze, a card count, and whether to include previous-page context, then generate from the current page. If the document has approved knowledge-graph facts, cards are built from those facts instead of the raw text."
             :commands #{:generate}}
            {:label "Generate with a prompt"
             :detail "Add a custom instruction (e.g. \"focus on definitions\") — recent prompts autocomplete."
             :commands #{:gen-prompt}}
            {:label "Compare models"
             :detail "Run the same content through two or more card models side by side and keep the set you prefer; each model is a separate, billed generation."
             :commands #{:compare-card-gen}}
            {:label "Curate"
             :detail "Add a card from a text selection ({add-new}) — the passage is marked in the editor — edit any card's question/answer or cloze, or delete the weak ones."
             :commands #{:add-new :add-card :edit-card :delete-card}}]}
   {:slug "score-cards"
    :title "Sheet-music (Score) cards"
    :intro "Turn a score PDF and a recording into audio ↔ notation cards."
    :steps [{:label "Create it"
             :detail "Import → Score, drop the sheet-music PDF and the recording, then Import Score."
             :commands #{:import-score}}
            {:label "Select the audio"
             :detail "Drag directly on the waveform strip to mark the passage you want to hear."
             :commands #{}}
            {:label "Mark the notation"
             :detail "Click Notation, drag a rectangle over the matching bars on the page, then Done."
             :commands #{}}
            {:label "Add the card"
             :detail "Add card ▾ → Audio front, Sheet front, or Both."
             :commands #{:add-score-group}}]}
   {:slug "image-occlusion-cards"
    :title "Image-occlusion cards"
    :intro "Hide parts of an image and quiz yourself on what's under the mask."
    :steps [{:label "Open the editor"
             :detail "Right-click an image in the topic editor → Image Occlusion…"
             :commands #{}}
            {:label "Draw masks"
             :detail "In the Masks Editor tab, pick Draw (R) and drag on the image to add a mask. Pick Select (S) to adjust: click a mask, drag to move, handles to resize, Delete to remove."
             :commands #{}}
            {:label "Group masks (optional)"
             :detail "With Select (S), Shift-click or drag a box over several masks, then Group (G) — grouped masks are revealed together as ONE card. Ungroup (U) splits them again. The footer shows \"N masks → M cards\"."
             :commands #{}}
            {:label "Add fields (optional)"
             :detail "The Fields tab holds Header, Footer, Remarks, Sources, and two Extra fields."
             :commands #{}}
            {:label "Create the cards"
             :detail "Hide One, Guess One makes one card per mask group; Hide All, Guess One hides them all on every card. Push to Anki as image-occlusion notes."
             :commands #{:add-occlusion}}]}
   {:slug "syncing-to-anki"
    :title "Syncing to Anki"
    :intro "Push curated cards into your own Anki collection over AnkiConnect."
    :steps [{:label "Set up AnkiConnect"
             :detail "Settings → Anki Sync walks you through it: install Anki and the AnkiConnect add-on, press Connect, and choose Yes when Anki asks about FreeMemo."
             :commands #{}}
            {:label "Sync ({anki-sync})"
             :detail "Choose the deck, tags and a custom header, then sync. FreeMemo owns the Basic and Cloze note types, so there's nothing to map."
             :commands #{:anki-sync}}
            {:label "Sync now ({quick-sync})"
             :detail "The same run with your saved settings, in the background — no modal."
             :commands #{:quick-sync}}
            {:label "What a run does"
             :detail "Each card moves whichever way it changed: edited here it goes to Anki, edited in Anki it comes back, deleted on one side it goes from the other. There is no direction to pick."
             :commands #{}}
            {:label "Conflicts"
             :detail "A card edited on BOTH sides is left untouched and reported. Filter Library › Cards by Conflict, then choose Keep FreeMemo or Keep Anki."
             :commands #{}}]}
   {:slug "reviewing"
    :title "Reviewing (Learn)"
    :intro "Study what's due on a spaced-repetition schedule."
    :steps [{:label "Start Learning"
             :detail "The Learn tab opens the day's due queue as a single-file session."
             :commands #{:start-learn-session}}
            {:label "Order"
             :detail "The due date decides membership; priority (0 = highest) orders the queue; ties shuffle stably for the day."
             :commands #{}}
            {:label "Advance or postpone"
             :detail "Move to the next item, or push a topic's due date out by a number of days."
             :commands #{:advance-topic :postpone-topic}}
            {:label "Track"
             :detail "The Learn dashboard shows due/studied counts and streaks; Repetition History shows a topic's past reviews."
             :commands #{}}]}
   {:slug "knowledge-graph"
    :title "Building a knowledge graph"
    :intro "Distill a document into a fact graph you can curate and quiz against."
    :steps [{:label "Distill"
             :detail "Knowledge → Documents → Distill on a document. It runs in the background with a spinner and a Cancel; Re-distill re-runs it. Facts land approved — you curate by exception, not one by one."
             :commands #{:distill}}
            {:label "Generate questions"
             :detail "Questions turns each fact into an atomic question; both actions are also in the {toggle-palette} palette."
             :commands #{:generate-questions}}
            {:label "Fix the facts"
             :detail "Facts (N) opens the fact table — edit or relink an object, or × to reject a fact."
             :commands #{}}
            {:label "Curate entities"
             :detail "Entities: Rename one, Merge… two that name the same thing (irreversible), or Synthesize questions for one."
             :commands #{}}
            {:label "Curate questions"
             :detail "Questions: click a row to edit its question and reference answer, or × to reject it."
             :commands #{}}]}
   {:slug "learning-a-codebase"
    :title "Learning a codebase"
    :intro "Turn a code repository into a topic tree and a fact graph you can browse and quiz. v1 analyzes Clojure sources (.clj/.cljc/.cljs)."
    :steps [{:label "Zip the source"
             :detail "Make a .zip of the repo. `git archive --format=zip -o repo.zip HEAD` is cleanest — tracked files only, no .git or build output. Only Clojure files are analyzed; anything else is ignored."
             :commands #{}}
            {:label "Upload it"
             :detail "Import → Upload, pick the .zip, and confirm \"Import as Code Repository\". Drag-and-drop works too."
             :commands #{:import-code}}
            {:label "What you get"
             :detail "A root topic named after the repo, one child topic per source file (source shown as a syntax-highlighted code block), and code facts distilled in the background — a toast reports when they're ready."
             :commands #{}}
            {:label "Browse & quiz"
             :detail "Facts (defined-in, calls, requires, arglists, docstrings) land under Knowledge → Facts for the repo. Generate questions and quiz against them like any other graph."
             :commands #{}}]}
   {:slug "quiz"
    :title "Testing yourself — Quiz & Timed Quiz"
    :intro "Test recall against the fact graph, graded by the model."
    :steps [{:label "Set it up"
             :detail "Quiz tab → toggle Quiz or Timed Quiz, check the source documents, set Questions (and Minutes for a timed quiz), then Start."
             :commands #{:start-quiz :start-exam}}
            {:label "Quiz"
             :detail "Untimed. Submit an answer for an instant verdict — ✓ Correct, ◐ Partial, or ✗ Incorrect — with an explanation and the facts you missed."
             :commands #{}}
            {:label "Timed Quiz"
             :detail "Timed and forward-only. Answers are saved as you go and graded only when you submit, or when the timer runs out."
             :commands #{}}
            {:label "Review past sessions"
             :detail "History lists your previous quizzes and timed quizzes with scores."
             :commands #{:quiz-history}}]}
   {:slug "finding-and-organizing"
    :title "Finding & organizing"
    :intro "Search, browse, reshape, and review your growing library."
    :steps [{:label "Search"
             :detail "The Search tab finds topics by their text, Fuzzy or Exact. Card text is filtered separately in Library → Cards."
             :commands #{:search}}
            {:label "Browse the Library"
             :detail "Library toggles between Documents (the knowledge tree) and Cards; Expand All / Collapse All opens the tree. Each document's Done and Synced columns show its progress."
             :commands #{}}
            {:label "Use the Library on a phone"
             :detail "Filters stack above the list, document progress uses a compact row, and the row menu provides a large action target."
             :commands #{}}
            {:label "Re-nest topics"
             :detail "Use Hierarchy, the default left-panel tab. Drag a row by its ⠿ grip to a new parent. Switch to Pins for saved images. Press Esc to cancel a move."
             :commands #{:move-topic :show-hierarchy :show-pins}}
            {:label "Review a subset"
             :detail "In Library → Documents, a document's ⋯ menu → Review studies just that topic's children."
             :commands #{:open-subset-review}}]}
   {:slug "keyboard"
    :title "Control FreeMemo with the keyboard"
    :intro "Use focus keys, the command palette, and contextual controls to operate FreeMemo without a pointer."
    :steps [{:label "Start with focus"
             :detail "Press Tab to show the Skip to content link. Press Enter to bypass the sidebar."
             :commands #{}}
            {:label "Move between controls"
             :detail "Press Tab to move forward. Press Shift+Tab to move backward. A visible ring shows the focused control."
             :commands #{}}
            {:label "Activate controls"
             :detail "Press Enter or Space to activate a focused button, row, result, card, review, exam, transcript segment, or heatmap day."
             :commands #{}}
            {:label "Open any tab"
             :detail "Open {toggle-palette}, type “Go to”, select a destination with the arrow keys, and press Enter."
             :commands #{:go-home :go-learn :go-schedule :go-library :go-knowledge :go-quiz :go-search :go-import :go-settings :go-curriculum :go-help :go-graph}}
            {:label "Use the command palette ({toggle-palette})"
             :detail "Type to filter actions. Use ↑ and ↓ or Ctrl+n and Ctrl+p. Press Enter to run an action. Press Escape to close."
             :commands #{:toggle-palette}}
            {:label "Read documents"
             :detail "{prev-page} and {next-page} change pages. {scan} scans a page. {done} marks it done."
             :commands #{}}
            {:label "Make cards"
             :detail "{extract} extracts selected text. {generate} generates cards. {add-new} opens a new card. {undo-newest} undoes the latest action."
             :commands #{:undo-newest}}
            {:label "Sync Anki"
             :detail "{anki-sync} opens Anki sync. {quick-sync} runs sync with the saved settings."
             :commands #{}}
            {:label "Use tables and lists"
             :detail "Focus a row and press Enter or Space. Focus a sortable header and press Enter or Space."
             :commands #{}}
            {:label "Use the topic tree"
             :detail "Focus an arrow and press Enter or Space to expand it. Focus the row to open the topic."
             :commands #{}}
            {:label "Move a topic"
             :detail "Activate its ⠿ grip. Tab to a destination grip and activate it. Press Escape to cancel."
             :commands #{}}
            {:label "Use the graph"
             :detail "Open Filters and use Find a concept. Type a name, use the arrow keys, and press Enter."
             :commands #{}}
            {:label "Use rich-text editors"
             :detail "Press Tab to indent a line and Shift+Tab to outdent it. Press Escape, then Tab, to leave the editor. In a dialog, press Escape a second time to close it."
             :commands #{}}
            {:label "Use dialogs"
             :detail "Press Tab or Shift+Tab to move inside a dialog. Press Escape to close it and restore focus."
             :commands #{}}
            {:label "Resize panes"
             :detail "Tab to a separator. Use the arrow keys to resize its adjacent panes."
             :commands #{}}
            {:label "Spatial editors"
             :detail "Score regions and image-occlusion masks require precise pointer input. All other documented workflows support keyboard control."
             :commands #{}}]}])

(def tab->slug
  "Routed tab → the Help section that explains it, for the rail's Help for
   this page button (plans/searchable-help.md 1.5, 5.1). A tab with no
   section maps to `[:none reason]`. help_content_test asserts every key of
   `page-title-format/tab-labels` is here, so a new tab cannot go unmapped."
  {:home       "getting-around"
   :viewer     "studying-a-pdf"
   :library    "finding-and-organizing"
   :learn      "reviewing"
   :schedule   "reading-schedule"
   :curriculum "building-a-curriculum"
   :quiz       "quiz"
   :search     "finding-and-organizing"
   :knowledge  "knowledge-graph"
   :import     "importing-content"
   :graph      [:none "No Graph section yet."]
   :help       [:none "This is the Help page."]
   :settings   [:none "No Settings section yet."]
   :credits    [:none "No Credits section yet."]})

(def undocumented
  "Registry ids no step documents, each with its reason. `:pending` means Help
   owes the command a step. help_content_test caps the number of `:pending`
   entries, and the cap only goes down (plans/searchable-help.md 6.4)."
  {:credits-granted           "Bookkeeping after a payment. No user action runs it."
   :debit-credits             "Bookkeeping after a charge. No user action runs it."
   :open-topic                "Opens a topic from a link. It is not a step a user takes."
   :save-video-position       "Saved on pause and on leaving. No user action runs it."
   :set-setting               "The write behind every Settings control. The Settings page is its UI."
   :add-pin                   :pending
   :apply-code-language       :pending
   :bulk-delete-cards         :pending
   :bulk-reject-facts         :pending
   :card-history              :pending
   :compare-ocr               :pending
   :copy-all                  :pending
   :copy-card                 :pending
   :delete-document           :pending
   :dismiss                   :pending
   :extract-video             :pending
   :find-next-marked-card     :pending
   :find-next-remarked-card   :pending
   :find-next-suspended-card  :pending
   :flag-question             :pending
   :import-account            :pending
   :import-audio              :pending
   :import-document           :pending
   :import-paste              :pending
   :import-poem               :pending
   :import-supermemo          :pending
   :import-video              :pending
   :insert-audio              :pending
   :new-topic                 :pending
   :open-activity             :pending
   :open-undo-history         :pending
   :parse-questions           :pending
   :process-video             :pending
   :push-biblio               :pending
   :refetch-biblio            :pending
   :remove-pin                :pending
   :rename-topic              :pending
   :reset-dedup-thresholds    :pending
   :restore                   :pending
   :save-biblio               :pending
   :set-page-done             :pending
   :set-pdf-done              :pending
   :show-getting-started      :pending
   :show-poem-panel           :pending
   :suspend-question          :pending
   :sweep-duplicates          :pending
   :sweep-duplicates-all      :pending
   :toggle-card-table-view    :pending
   :toggle-focus-reader       :pending
   :toggle-left-pane          :pending
   :toggle-pin-placement      :pending
   :transcribe                :pending
   :undismiss                 :pending
   :undo-entry                :pending
   :update-occlusion          :pending
   :update-score-group        :pending})

;; ── Lookups ────────────────────────────────────────────────────────────────

(def ^:private binds
  "command-id → registry bind string, for chord-token resolution."
  (into {} (commands/bindings)))

(defn resolve-chords
  "Replace `{command-id}` tokens in `s` with the platform display chord
   (commands/display-chord) — \"⌘S\" on macOS, \"Ctrl+S\" elsewhere.
   Unknown or unbound ids stay as literal text."
  [s]
  (str/replace s #"\{([a-z-]+)\}"
    (fn [[whole id]]
      (or (commands/display-chord (keyword id) (get binds (keyword id)))
          whole))))

(defn resolved-text
  "The text a user reads for a step: label and detail, chord tokens resolved.
   Display and search both read this (plans/searchable-help.md 1.4)."
  [{:keys [label detail]}]
  (str (resolve-chords label) " " (resolve-chords detail)))

(defn section-by-slug
  "The section whose `:slug` is `slug`, or nil."
  [slug]
  (some #(when (= slug (:slug %)) %) sections))

(defn slug-for-tab
  "The section slug for routed tab `tab`, or nil when the tab has no section.
   A tab missing from `tab->slug` also gives nil; the test makes that a failure."
  [tab]
  (let [v (get tab->slug tab)]
    (when (string? v) v)))

(def ^:private command->slug
  "Registry id → the slug of the FIRST section, in reading order, with a step
   that documents it. The keyboard section repeats ids the specific sections
   own, and it comes last, so the specific section wins."
  (reduce (fn [m {:keys [slug steps]}]
            (reduce (fn [m id] (if (contains? m id) m (assoc m id slug)))
              m (mapcat :commands steps)))
    {} sections))

(defn slug-for-command
  "The slug of the section that documents registry id `id`, or nil."
  [id]
  (get command->slug id))

(defn- contains-ci?
  "True when `text` contains `query`, ignoring case. Pre: query is not blank."
  [text query]
  (str/includes? (str/lower-case text) (str/lower-case query)))

(defn filter-sections
  "The sections a Help-page query leaves, in reading order
   (plans/searchable-help.md 3.1, 3.2).

   Matching is a case-insensitive substring test on resolved text — never
   command-palette/match-score, whose subsequence tier matches almost any long
   detail. A section whose title or intro matches keeps every step. Otherwise
   it keeps the steps that match, and a section with none left is dropped.

   Post: a blank query returns `sections` unchanged."
  [query]
  (let [q (str/trim (or query ""))]
    (if (str/blank? q)
      sections
      (keep (fn [{:keys [title intro steps] :as section}]
              (if (or (contains-ci? title q) (contains-ci? (resolve-chords intro) q))
                section
                (let [hits (filterv #(contains-ci? (resolved-text %) q) steps)]
                  (when (seq hits) (assoc section :steps hits)))))
        sections))))

(defn step-label-hit?
  "True when some step label of `section`, resolved, contains `query`.
   The palette's second tier of help-row matching (D10). Pre: query is not
   blank — an empty query matches every title already."
  [section query]
  (boolean (some #(contains-ci? (resolve-chords (:label %)) query) (:steps section))))
