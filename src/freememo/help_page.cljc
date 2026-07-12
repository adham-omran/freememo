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
   (commands/display-chord) — \"⌘⇧S\" on macOS, \"Ctrl+Shift+S\" elsewhere.
   Unknown or unbound ids stay as literal text."
  [s]
  (str/replace s #"\{([a-z-]+)\}"
    (fn [[whole id]]
      (or (commands/display-chord (keyword id) (get binds (keyword id)))
          whole))))

(def workflows
  "Comprehensive tour of FreeMemo, section by section: importing, reading,
   card-making (basic, score, image-occlusion), Anki sync, review, the
   knowledge graph, quizzing, finding/organizing, and shortcuts.
   Each step's label names the real control (with its shortcut where one exists)."
  [{:title "Importing content"
    :intro "Get PDFs, EPUBs, articles, and photos in. Everything starts on the Import tab."
    :steps [["Upload a file" "Import → Upload. Drop a PDF, EPUB, HTML, or Markdown file (or click to browse); PDFs and EPUBs ask \"Import as …\" first. Drag-and-drop onto the modal works too."]
            ["Import from a link" "Import → Link, paste a web or Wikipedia URL, and Fetch. A URL that resolves to a PDF or EPUB is offered for import."]
            ["Import from Zotero" "Import → Zotero opens your library; filter and click an item to pull its PDF. First turn it on in Settings → Zotero (Enable Zotero import, then Test Connection) with the FreeMemo for Zotero plugin installed."]
            ["Live Document" "Import → Live Document makes a PDF you keep adding to. In its viewer use Upload images or Take photo, then Add pages; HEIC photos convert automatically."]]}
   {:title "Studying a PDF"
    :intro "Turn a PDF into studied, card-ready material, one page at a time."
    :steps [["Import it" "Import tab → Upload (PDF or EPUB) or Link (a web or Wikipedia URL). Drag-and-drop works too."]
            ["Read & navigate" "Open it from Library or the Viewer. The page arrows or the page box move you; the zoom menu fits width or whole page."]
            ["Extract the text ({scan})" "Scan Page runs OCR on the current page into clean, editable HTML. Copy-text pulls the PDF's own text layer when it has one."]
            ["Edit & split ({extract})" "Fix the text in the editor. Select a passage and Extract turns it into a child topic linked back to the page."]
            ["Mark done ({done})" "Mark a page Done to drop it from your review queue while keeping the content."]]}
   {:title "Asking the AI tutor"
    :intro "Think a page through with a Socratic assistant that asks rather than answers."
    :steps [["Open it" "Right side panel → AI Assistant tab. New Chat starts a conversation scoped to the current document; your chats and transcripts are saved."]
            ["Ask about the page" "Type a question about what you're reading. The tutor replies with questions to guide you, grounded in the page — it won't just hand you the answer. Markdown and math render inline."]
            ["Pick its model" "The panel's model selector sets the tutor's model for this document; leave it on your default otherwise."]]}
   {:title "Making a deck"
    :intro "Generate flashcards from the page you're reading, then curate them."
    :steps [["Generate ({generate})" "Pick Basic or Cloze, a card count, and whether to include previous-page context, then generate from the current page. If the document has approved knowledge-graph facts, cards are built from those facts instead of the raw text."]
            ["Generate with a prompt" "Add a custom instruction (e.g. \"focus on definitions\") — recent prompts autocomplete."]
            ["Compare models" "Run the same content through two or more card models side by side and keep the set you prefer; each model is a separate, billed generation."]
            ["Curate" "Add a card from a text selection, edit any card's question/answer or cloze, or delete the weak ones."]
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
            ["Draw masks" "In the Masks Editor tab, drag on the image to add a mask; click one to select it, drag or use its handles to adjust, Delete to remove."]
            ["Add fields (optional)" "The Fields tab holds Header, Footer, Remarks, Sources, and two Extra fields."]
            ["Create the cards" "Hide One, Guess One makes one card per mask; Hide All, Guess One hides them all on every card. Push to Anki as image-occlusion notes."]]}
   {:title "Syncing to Anki"
    :intro "Push curated cards into your own Anki collection over AnkiConnect."
    :steps [["Push ({anki-sync})" "Choose scope (this topic, its subtree, or the whole document), deck, tags, and a custom header, then push. FreeMemo owns the Basic and Cloze note types, so there's nothing to map."]
            ["Quick Sync ({quick-sync})" "Re-push with your last-used settings in the background — no modal."]
            ["Pull" "Bring edits made in Anki back into FreeMemo; a diff shows what changed."]]}
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
   {:title "Testing yourself — Quiz & Exam"
    :intro "Test recall against the fact graph, graded by the model."
    :steps [["Set it up" "Quiz tab → toggle Quiz or Exam, check the source documents, set Questions (and Minutes for an exam), then Start."]
            ["Quiz" "Untimed. Submit an answer for an instant verdict — ✓ Correct, ◐ Partial, or ✗ Incorrect — with an explanation and the facts you missed."]
            ["Exam" "Timed and forward-only. Answers are saved as you go and graded only when you Submit exam, or when the timer runs out."]
            ["Review past sessions" "History lists your previous quizzes and exams with scores."]]}
   {:title "Finding & organizing"
    :intro "Search, browse, reshape, and review your growing library."
    :steps [["Search" "The Search tab finds topics by their text, Fuzzy or Exact. Card text is filtered separately in Library → Cards."]
            ["Browse the Library" "Library toggles between Documents (the knowledge tree) and Cards; Expand All / Collapse All opens the tree. Each document's Done and Synced columns show its progress."]
            ["Re-nest topics" "Drag a row by its ⠿ grip onto a new parent, or click the grip to arm a move and click the destination (Esc cancels). Works in the Library tree and the Viewer's Hierarchy (☰) panel."]
            ["Review a subset" "In Library → Documents, a document's ⋯ menu → Review studies just that topic's children."]]}
   {:title "Keyboard & command palette"
    :intro "Reach any action without hunting for its button."
    :steps [["Command palette ({toggle-palette})" "Open the palette, type to fuzzy-search every action — including ones with no shortcut, like Distill facts or Start quiz — and press Enter to run it."]
            ["While reading" "{scan} Scan page · {extract} Extract selection · {generate} Generate cards · {done} Mark done."]
            ["Anki" "{anki-sync} Push to Anki · {quick-sync} Quick push."]
            ["Undo" "{undo-newest} undoes your last action."]]}])

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
        (dom/text "FreeMemo turns your reading into Anki cards. Here's how to use it, feature by feature."))
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
