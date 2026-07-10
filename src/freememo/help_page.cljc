(ns freememo.help-page
  "Static Help tab: FreeMemo's core workflows, hand-authored.
   Pure client-side prose — no server fetch, no markdown runtime. Content is
   data-driven so a new workflow is a map, not more markup."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(def workflows
  "The v1 core funnel: Import → Read/mark PDF → Make deck → Review/Anki.
   Each step's label names the real control (with its shortcut where one exists)."
  [{:title "Studying a PDF"
    :intro "Turn a PDF into studied, card-ready material, one page at a time."
    :steps [["Import it" "Import tab → Upload (PDF or EPUB) or Link (a web or Wikipedia URL). Drag-and-drop works too."]
            ["Read & navigate" "Open it from Library or the Viewer. The page arrows or the page box move you; the zoom menu fits width or whole page."]
            ["Extract the text (⌘⇧S)" "Scan Page runs OCR on the current page into clean, editable HTML. Copy-text pulls the PDF's own text layer when it has one."]
            ["Edit & split (⌘⇧E)" "Fix the text in the editor. Select a passage and Extract turns it into a child topic linked back to the page."]
            ["Mark done (⌘⇧D)" "Mark a page Done to drop it from your review queue while keeping the content."]]}
   {:title "Asking the AI tutor"
    :intro "Think a page through with a Socratic assistant that asks rather than answers."
    :steps [["Open it" "Right side panel → AI Assistant tab. New Chat starts a conversation scoped to the current document; your chats and transcripts are saved."]
            ["Ask about the page" "Type a question about what you're reading. The tutor replies with questions to guide you, grounded in the page — it won't just hand you the answer. Markdown and math render inline."]
            ["Pick its model" "The panel's model selector sets the tutor's model for this document; leave it on your default otherwise."]]}
   {:title "Making a deck"
    :intro "Generate flashcards from the page you're reading, then curate them."
    :steps [["Generate (⌘⇧G)" "Pick Basic or Cloze, a card count, and whether to include previous-page context, then generate from the current page. If the document has approved knowledge-graph facts, cards are built from those facts instead of the raw text."]
            ["Generate with a prompt" "Add a custom instruction (e.g. \"focus on definitions\") — recent prompts autocomplete."]
            ["Compare models" "Run the same content through two or more card models side by side and keep the set you prefer; each model is a separate, billed generation."]
            ["Curate" "Add a card from a text selection, edit any card's question/answer or cloze, or delete the weak ones."]
            ["Export" "Export to Anki-compatible CSV by scope (current page or whole document) and kind (Basic, Cloze, or both)."]]}
   {:title "Syncing to Anki"
    :intro "Push curated cards into your own Anki collection over AnkiConnect."
    :steps [["Push (⌘⇧X)" "Choose scope (this topic, its subtree, or the whole document), deck, tags, and a custom header, then push. FreeMemo owns the Basic and Cloze note types, so there's nothing to map."]
            ["Quick Sync (⌘⌥⇧X)" "Re-push with your last-used settings in the background — no modal."]
            ["Pull" "Bring edits made in Anki back into FreeMemo; a diff shows what changed."]]}
   {:title "Reviewing (Learn)"
    :intro "Study what's due on a spaced-repetition schedule."
    :steps [["Start Learning" "The Learn tab opens the day's due queue as a single-file session."]
            ["Order" "The due date decides membership; priority (0 = highest) orders the queue; ties shuffle stably for the day."]
            ["Advance or postpone" "Move to the next item, or push a topic's due date out by a number of days."]
            ["Track" "The Learn dashboard shows due/studied counts and streaks; Repetition History shows a topic's past reviews."]]}])

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
        (dom/text "FreeMemo turns your reading into Anki cards. Here are the core workflows."))
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
            (dom/text (:intro wf)))
          (dom/ol
            (dom/props {:style {:padding-left "20px" :display "flex"
                                :flex-direction "column" :gap "8px"}})
            (e/for-by first [step (:steps wf)]
              (let [[label detail] step]
                (dom/li
                  (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"
                                      :line-height "1.5"}})
                  (dom/strong (dom/text label))
                  (dom/text " — ")
                  (dom/text detail))))))))))
