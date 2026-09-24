(ns freememo.help-page
  "Static Help tab: FreeMemo's workflows, hand-authored.
   Pure client-side prose — no server fetch, no markdown runtime. Content is
   data-driven so a new workflow is a map, not more markup."
  (:require
   [clojure.string :as str]
   [freememo.commands :as commands]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(def binds
  "command-id → registry bind string, for shortcut-token resolution."
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

(def workflows
  "Comprehensive tour of FreeMemo, section by section: getting around, importing, reading,
   assistant chat, schedules, curricula, exams, card-making, Anki sync,
   review, the knowledge graph, quizzing, finding/organizing, and shortcuts.
   Each step's label names the real control (with its shortcut where one exists)."
  [{:title "Getting around"
    :intro "The sidebar at the left holds every place in FreeMemo, in the order you use them: import, read, review, plan, explore."
    :steps [["Open the sidebar" "Rest the pointer on the sidebar, or press Tab to move into it. It opens over the page and closes when you leave it. Open Sidebar in the command palette puts focus in it, and Esc closes it."]
            ["Keep it open ({toggle-nav-rail})" "Keep open, at the foot of the sidebar, docks it beside the page. In a window 900px wide or less it opens over the page instead. On a phone, ☰ opens it."]
            ["Continue" "Continue opens the last document you opened in the Viewer. A learn session and a subset review do not change it: Continue returns to the document you chose to read, and a learn session picks its items from a queue."]
            ["Learn's count" "The number on Learn is how many topics are due, the same count Home shows."]]}
   {:title "Importing content"
    :intro "Get PDFs, EPUBs, articles, and photos in. Everything starts on the Import tab."
    :steps [["Upload a file" "Import → Upload. Drop a PDF, EPUB, HTML, Markdown, or code-repo (.zip) file (or click to browse); PDFs, EPUBs, and code repos ask \"Import as …\" first. Drag-and-drop onto the modal works too."]
            ["Import from a link" "Import → Link, paste a web or Wikipedia URL, and Fetch. A URL that resolves to a PDF or EPUB is offered for import."]
            ["Import from Zotero" "Import → Zotero opens your library; filter and click an item to pull its PDF. First turn it on in Settings → Zotero (Enable Zotero import, then Test Connection) with the FreeMemo for Zotero plugin installed."]
            ["Live Document" "Import → Live Document makes a PDF you keep adding to. Tap ＋ to upload or take photos, rotate and crop each in the editor, then Done to append them as pages; HEIC photos convert automatically."]]}
   {:title "Studying a PDF"
    :intro "Turn a PDF into studied, card-ready material, one page at a time."
    :steps [["Import it" "Import tab → Upload (PDF or EPUB) or Link (a web or Wikipedia URL). Drag-and-drop works too."]
            ["Read & navigate" "Open it from Library, or from Continue in the sidebar. The page arrows or the page box move you; the zoom menu fits width or whole page."]
            ["Extract the text ({scan})" "Scan Page runs OCR on the current page into clean, editable HTML. Copy-text pulls the PDF's own text layer when it has one."]
            ["Edit & split ({extract})" "Fix the text in the editor. Select a passage and Extract turns it into a child topic linked back to the page."]
            ["Mark done ({done})" "Mark a page Done to drop it from your review queue while keeping the content."]]}
   {:title "Boox NeoReader highlights"
    :intro "Bring the highlights and notes you made on a Boox e-reader into the EPUB or PDF you read them in. Google Drive carries the file; nothing is written back to it or to the device."
    :steps [["Export on the device" "In NeoReader, export the book's annotations as HTML. The file lands in onyx / your device name / Read in your Google Drive."]
            ["Extract the text first (PDF only)" "A PDF's pages hold no text until you Scan Page or Copy text on them, and a page with no text can hold no highlight. Copy text matches your Boox export most closely, because both read the PDF's own text layer."]
            ["Start the sync" "Library → Documents → the document's ⋯ menu → Sync with NeoReader. The first run asks Google for read access to one file, then opens Google's own file picker."]
            ["Pick the export" "Go to onyx → your device → Read and pick the book's HTML file. FreeMemo remembers it, so every later sync is one click with no picker."]
            ["Read the result" "Each highlighted passage is tinted orange in its chapter or page. A passage you annotated also carries a [1] marker, and that chapter or page ends with a numbered USER NOTES list."]
            ["Check what did not place" "A highlight FreeMemo cannot find in the text is listed under UNPLACED HIGHLIGHTS — on the document itself for an EPUB, on page 1 for a PDF. An unscanned page is the usual cause on a PDF; editing a sentence after you highlighted it is the usual cause on either."]
            ["Sync again any time" "Every sync rebuilds from the export. A highlight you deleted on the device loses its mark, and your own edits inside a USER NOTES list do not survive."]]}
   {:title "Asking the AI assistant"
    :intro "Work a page through with an assistant that either answers you or questions you — your choice."
    :steps [["Open it" "Open the Assistant panel on the right. New Chat starts a saved conversation about the current document."]
            ["Set the panel height" "Settings → AI Features → Assistant panel height. Content area stops above the card table. Full workspace extends beside it."]
            ["Pick a mode" "Use the Mode dropdown below the composer. General answers directly. Tutor asks guiding questions. Socratic uses a fixed learning structure. Each chat keeps its mode."]
            ["Ask about the page" "Type a question about what you're reading. Every mode is grounded in the page in front of you. Markdown and math render inline."]
            ["Reference another document" "Type @ in the composer and pick a document. The menu stays above the composer, and the selected document becomes a removable chip."]
            ["Check source evidence" "Select an evidence link to read its retained excerpt. Evidence from another document includes Open document in new tab."]
            ["Pick its model" "Use the Model dropdown below the composer to set the model for this document. Leave it on your default otherwise."]
            ["Manage an answer" "Answers stream into the saved chat. Stop an active answer. Answer details shows its request, timing, provider, and sources. Failed answers offer Retry."]]}
   {:title "Planning a reading schedule"
    :intro "Plan a document page range to a target date, with reading and Anki review work in one forecast."
    :steps [["Create a document schedule" "Schedule → New schedule. Pick a document, target date, page range, and daily review capacity. A blank last page means the document end."]
            ["Read the dashboard" "Curriculum schedules and document schedules appear separately. A curriculum card shows its remaining study minutes. Select a document schedule to open its forecast."]
            ["Follow the reading pill" "The Viewer toolbar shows today's page quota and deadline for the schedule that covers the current page. Hover to see your pace."]
            ["Inspect the plan" "Study plan is the default view. It lists daily pages and reviews through card maturation. Switch to Pages for each page's state."]
            ["Handle a past target" "After the target passes, the report shows the overdue time, page coverage, and expected card maturation date. You can move the target."]
            ["Archive or restore" "Archive removes a schedule from active work and its daily load. Open Archived in the schedule list to restore it."]]}
   {:title "Building a curriculum"
    :intro "Arrange imported material into an ordered study path with blocks, schedule controls, and progress gates."
    :steps [["Create it" "Curriculum → New curriculum. Name it and set a target date, daily study minutes, study days, or a useful combination."]
            ["Add blocks and materials" "Add blocks in study order. In an unlocked block, Add material searches all imported sources. Use the type filters to narrow the list."]
            ["Order the work" "Use the arrow controls to move blocks and materials. Edit a material's minute estimate when the default is wrong."]
            ["Set the focus" "Make one available block the focus. Later blocks unlock after you pass the prior block exam. The override can skip that exam."]
            ["Track readiness" "Mark a material studied, build its Anki cards, and pass its exam. The curriculum schedule shows the next tasks and estimated finish."]
            ["Edit or delete" "Edit schedule controls from the curriculum header. Use the pencil to rename a block. Delete curriculum hides its blocks and exams for 12 hours. Undo restores them. Imported documents stay."]]}
   {:title "Taking curriculum exams"
    :intro "Generate evidence-bound material and block exams, save answer drafts, and keep a reviewable grade history."
    :steps [["Generate an exam" "Select Generate exam on a material or block. Generation shows progress and survives page changes. Use Activity to monitor or cancel it."]
            ["Review and approve" "The exam route shows the blueprint, evidence, and questions. Use Edit to change a draft. Approve freezes the version used by later sittings."]
            ["Take or resume it" "Take exam opens five answer fields. Draft answers save when you leave a field. Reloading the sitting route restores the same exam and elapsed time."]
            ["Grade it" "Submit freezes the answers and requests an AI grade. Use Regrade with AI when needed. A failed result can create a targeted retry exam."]
            ["Cancel or archive" "Cancel this sitting closes an unsubmitted attempt without a grade. Archive removes a cancelled sitting from the list. Submitted sittings remain in history."]
            ["Understand credits" "Paid actions show a credit notice before use. Completed forms and grades show their recorded charge. AI Costs lists every returned model charge."]]}
   {:title "Making a deck"
    :intro "Generate flashcards from the page you're reading, then curate them."
    :steps [["Generate ({generate})" "Pick Basic or Cloze, a card count, and whether to include previous-page context, then generate from the current page. If the document has approved knowledge-graph facts, cards are built from those facts instead of the raw text."]
            ["Generate with a prompt" "Add a custom instruction (e.g. \"focus on definitions\") — recent prompts autocomplete."]
            ["Compare models" "Run the same content through two or more card models side by side and keep the set you prefer; each model is a separate, billed generation."]
            ["Curate" "Add a card from a text selection ({add-new}) — the passage is marked in the editor — edit any card's question/answer or cloze, or delete the weak ones."]
            ["Export" "Export to Anki-compatible CSV by scope (current page or whole document) and kind (Basic, Cloze, or both)."]]}
   {:title "Sheet-music (Score) cards"
    :intro "Turn a score PDF and a recording into audio ↔ notation cards."
    :steps [["Create it" "Import → Score, drop the sheet-music PDF and the recording, then Import Score."]
            ["Select the audio" "Drag directly on the waveform strip to mark the passage you want to hear."]
            ["Mark the notation" "Click Notation, drag a rectangle over the matching bars on the page, then Done."]
            ["Add the card" "Add card ▾ → Audio front, Sheet front, or Both."]]}
   {:title "Image-occlusion cards"
    :intro "Hide parts of an image and quiz yourself on what's under the mask."
    :steps [["Open the editor" "Right-click an image in the topic editor → Image Occlusion…"]
            ["Draw masks" "In the Masks Editor tab, pick Draw (R) and drag on the image to add a mask. Pick Select (S) to adjust: click a mask, drag to move, handles to resize, Delete to remove."]
            ["Group masks (optional)" "With Select (S), Shift-click or drag a box over several masks, then Group (G) — grouped masks are revealed together as ONE card. Ungroup (U) splits them again. The footer shows \"N masks → M cards\"."]
            ["Add fields (optional)" "The Fields tab holds Header, Footer, Remarks, Sources, and two Extra fields."]
            ["Create the cards" "Hide One, Guess One makes one card per mask group; Hide All, Guess One hides them all on every card. Push to Anki as image-occlusion notes."]]}
   {:title "Syncing to Anki"
    :intro "Push curated cards into your own Anki collection over AnkiConnect."
    :steps [["Sync ({anki-sync})" "Choose the deck, tags and a custom header, then sync. FreeMemo owns the Basic and Cloze note types, so there's nothing to map."]
            ["Sync now ({quick-sync})" "The same run with your saved settings, in the background — no modal."]
            ["What a run does" "Each card moves whichever way it changed: edited here it goes to Anki, edited in Anki it comes back, deleted on one side it goes from the other. There is no direction to pick."]
            ["Conflicts" "A card edited on BOTH sides is left untouched and reported. Filter Library › Cards by Conflict, then choose Keep FreeMemo or Keep Anki."]]}
   {:title "Reviewing (Learn)"
    :intro "Study what's due on a spaced-repetition schedule."
    :steps [["Start Learning" "The Learn tab opens the day's due queue as a single-file session."]
            ["Order" "The due date decides membership; priority (0 = highest) orders the queue; ties shuffle stably for the day."]
            ["Advance or postpone" "Move to the next item, or push a topic's due date out by a number of days."]
            ["Track" "The Learn dashboard shows due/studied counts and streaks; Repetition History shows a topic's past reviews."]]}
   {:title "Building a knowledge graph"
    :intro "Distill a document into a fact graph you can curate and quiz against."
    :steps [["Distill" "Knowledge → Documents → Distill on a document. It runs in the background with a spinner and a Cancel; Re-distill re-runs it. Facts land approved — you curate by exception, not one by one."]
            ["Generate questions" "Questions turns each fact into an atomic question; both actions are also in the {toggle-palette} palette."]
            ["Fix the facts" "Facts (N) opens the fact table — edit or relink an object, or × to reject a fact."]
            ["Curate entities" "Entities: Rename one, Merge… two that name the same thing (irreversible), or Synthesize questions for one."]
            ["Curate questions" "Questions: click a row to edit its question and reference answer, or × to reject it."]]}
   {:title "Learning a codebase"
    :intro "Turn a code repository into a topic tree and a fact graph you can browse and quiz. v1 analyzes Clojure sources (.clj/.cljc/.cljs)."
    :steps [["Zip the source" "Make a .zip of the repo. `git archive --format=zip -o repo.zip HEAD` is cleanest — tracked files only, no .git or build output. Only Clojure files are analyzed; anything else is ignored."]
            ["Upload it" "Import → Upload, pick the .zip, and confirm \"Import as Code Repository\". Drag-and-drop works too."]
            ["What you get" "A root topic named after the repo, one child topic per source file (source shown as a syntax-highlighted code block), and code facts distilled in the background — a toast reports when they're ready."]
            ["Browse & quiz" "Facts (defined-in, calls, requires, arglists, docstrings) land under Knowledge → Facts for the repo. Generate questions and quiz against them like any other graph."]]}
   {:title "Testing yourself — Quiz & Timed Quiz"
    :intro "Test recall against the fact graph, graded by the model."
    :steps [["Set it up" "Quiz tab → toggle Quiz or Timed Quiz, check the source documents, set Questions (and Minutes for a timed quiz), then Start."]
            ["Quiz" "Untimed. Submit an answer for an instant verdict — ✓ Correct, ◐ Partial, or ✗ Incorrect — with an explanation and the facts you missed."]
            ["Timed Quiz" "Timed and forward-only. Answers are saved as you go and graded only when you submit, or when the timer runs out."]
            ["Review past sessions" "History lists your previous quizzes and timed quizzes with scores."]]}
   {:title "Finding & organizing"
    :intro "Search, browse, reshape, and review your growing library."
    :steps [["Search" "The Search tab finds topics by their text, Fuzzy or Exact. Card text is filtered separately in Library → Cards."]
            ["Browse the Library" "Library toggles between Documents (the knowledge tree) and Cards; Expand All / Collapse All opens the tree. Each document's Done and Synced columns show its progress."]
            ["Use the Library on a phone" "Filters stack above the list, document progress uses a compact row, and the row menu provides a large action target."]
            ["Re-nest topics" "Use Hierarchy, the default left-panel tab. Drag a row by its ⠿ grip to a new parent. Switch to Pins for saved images. Press Esc to cancel a move."]
            ["Review a subset" "In Library → Documents, a document's ⋯ menu → Review studies just that topic's children."]]}
   {:title "Control FreeMemo with the keyboard"
    :intro "Use focus keys, the command palette, and contextual controls to operate FreeMemo without a pointer."
    :steps [["Start with focus" "Press Tab to show the Skip to content link. Press Enter to bypass the sidebar."]
            ["Move between controls" "Press Tab to move forward. Press Shift+Tab to move backward. A visible ring shows the focused control."]
            ["Activate controls" "Press Enter or Space to activate a focused button, row, result, card, review, exam, transcript segment, or heatmap day."]
            ["Open any tab" "Open {toggle-palette}, type “Go to”, select a destination with the arrow keys, and press Enter."]
            ["Use the command palette ({toggle-palette})" "Type to filter actions. Use ↑ and ↓ or Ctrl+n and Ctrl+p. Press Enter to run an action. Press Escape to close."]
            ["Read documents" "{prev-page} and {next-page} change pages. {scan} scans a page. {done} marks it done."]
            ["Make cards" "{extract} extracts selected text. {generate} generates cards. {add-new} opens a new card. {undo-newest} undoes the latest action."]
            ["Sync Anki" "{anki-sync} opens Anki sync. {quick-sync} runs sync with the saved settings."]
            ["Use tables and lists" "Focus a row and press Enter or Space. Focus a sortable header and press Enter or Space."]
            ["Use the topic tree" "Focus an arrow and press Enter or Space to expand it. Focus the row to open the topic."]
            ["Move a topic" "Activate its ⠿ grip. Tab to a destination grip and activate it. Press Escape to cancel."]
            ["Use the graph" "Open Filters and use Find a concept. Type a name, use the arrow keys, and press Enter."]
            ["Use rich-text editors" "Press Tab or Shift+Tab to leave an editor. Press Escape to remove focus from it."]
            ["Use dialogs" "Press Tab or Shift+Tab to move inside a dialog. Press Escape to close it and restore focus."]
            ["Resize panes" "Tab to a separator. Use the arrow keys to resize its adjacent panes."]
            ["Spatial editors" "Score regions and image-occlusion masks require precise pointer input. All other documented workflows support keyboard control."]]}])

(e/defn HelpPage []
  (e/client
    (dom/div
      (dom/props {:style {:max-width "820px" :margin "0 auto" :padding "32px 24px"}})
      (dom/h1
        (dom/props {:style {:font-size "26px" :font-weight "700" :margin-bottom "6px"
                            :color "var(--color-text-primary)"}})
        (dom/text "Help"))
      (dom/p
        (dom/props {:style {:font-size "15px" :color "var(--color-text-secondary)"
                            :margin-bottom "28px"}})
        (dom/text "FreeMemo runs three review systems over what you read: a priority queue for reading, FSRS-6 for quiz questions, and your own Anki for cards. Here's how to use it, feature by feature."))
      (e/for-by :title [wf workflows]
        (dom/section
          (dom/props {:style {:margin-bottom "28px"}})
          (dom/h2
            (dom/props {:style {:font-size "18px" :font-weight "600" :margin-bottom "4px"
                                :color "var(--color-text-primary)"}})
            (dom/text (:title wf)))
          (dom/p
            (dom/props {:style {:font-size "14px" :color "var(--color-text-secondary)"
                                :margin-bottom "10px"}})
            (dom/text (resolve-chords (:intro wf))))
          (dom/ol
            (dom/props {:style {:padding-left "20px" :display "flex"
                                :flex-direction "column" :gap "8px"}})
            (e/for-by first [step (:steps wf)]
              (let [[label detail] step]
                (dom/li
                  (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"
                                      :line-height "1.5"}})
                  (dom/strong (dom/text (resolve-chords label)))
                  (dom/text " — ")
                  (dom/text (resolve-chords detail)))))))))))
