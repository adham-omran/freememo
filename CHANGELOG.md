# Changelog

<!--
Format contract (see freememo.changelog):
- Each release is one `## <version>` block, newest first.
- Up to three subsections per release: `### For users`, `### Known issues`,
  `### Technical`.
- The email/Discord broadcast sends `For users` + `Known issues` ONLY.
  `### Technical` never leaves the repo — put developer-facing notes there.
-->

## v20261002-b0e8697

### For users

- **AI settings now use levels, and every account starts on Best.** Settings →
  AI Features gives each of five tasks a level: Flashcards, Page scanning,
  Reading assistant, Quiz grading and Knowledge graph. The levels are Economy
  ($), Balanced ($$), Best ($$$) and Custom.
  - **Three views show the same settings.** Simple sets one level for every
    task. By task sets a level per task. Advanced shows every setting, grouped
    by task, as two-column rows. An edit in Advanced makes that task Custom.
  - **Best runs GPT-6 Sol for cards, the assistant, quiz grading and most
    knowledge-graph steps.** It scans with Gemini 3.8 Flash at 300 DPI. On card
    generation, Best costs about 20 times as much as Economy.
  - **This update moves every existing account to Best once.** A model,
    reasoning or DPI you chose before stays stored, but a named level does not
    read it. Pick Custom or edit in Advanced to use your own values again.
  - **A document no longer picks its own model.** Document options loses its
    card and OCR model pickers, and the assistant panel loses its model picker.
    The level decides the model in every document.
  - **Duplicate detection moved into Advanced**, under Knowledge graph.
- **You can import a code repository from GitHub, GitLab or Forgejo.** The Code
  card on the Import page, or `Import code…` in the palette, opens the form. You
  pick a repository and a ref, then the directories and file extensions to keep.
  - **Each file becomes a topic in the Learn queue.** It opens in a read-only
    code pane beside your notes. `Copy to notes` inserts the selected code, and
    Generate reads the selection in the pane first.
  - **A Markdown file shows formatted, with a `Source` / `Rendered` toggle.**
    Links to other files in the same import work. A selection in the rendered
    view can become an extract.
  - **The import shows its phase, and you can cancel it.** `Close` lets the
    import continue on the server and shows a toast when it ends. The Activity
    monitor can cancel it too.
  - **`Update` on the repository adds the newest commit beside the old ones.**
  - **Settings → Git Forges stores your access tokens**, encrypted.
  - **Search reads code, and has a Code filter.** The picker can search
    directories and extensions.
- **`Toggle Editor pane` hides the notes editor beside a PDF, a video or a code
  file.** FreeMemo remembers the choice for each document. Add card and Extract
  still work. They read the selection from the pane.
- **A sidebar on the left replaces the top tab bar.** It shows icons when it is
  closed and opens when you point at it. `Keep open` (`⌘.` or `Ctrl+.`) docks
  it. On a phone, a menu button in a slim top bar opens it.
  - **`Continue` opens the last document you read.** The palette command `Go to
    Viewer` is now `Continue reading`, and `Go to Knowledge` is now `Go to
    Facts`.
  - **Learn shows the number of due items.** The closed sidebar shows `99+` for
    more than 99.
- **Home has a `Get started` checklist of six steps.** It reads your existing
  data, so it shows the progress you already made. `Show me` points at the
  control for each step. `Dismiss` hides the checklist, and `Show getting
  started` in the palette brings it back.
  - **Settings → Anki Sync shows how to connect AnkiConnect.** `Connect` asks
    Anki to allow this site. If Anki never asks, a section shows the line to add
    to AnkiConnect's config, with a copy button.
- **Help is searchable and opens beside the page.** `Help for this page` in the
  sidebar opens the section for the page you are on, in a modal. In a document,
  it picks the section for that kind of document.
  - **The palette lists matching Help sections after the commands.**
    `Shift+Enter` or the `?` on a row opens the Help for that command.
  - **Each import source has its own Help section, 14 in total.** Three wrong
    steps in the old import section are corrected.
- **The assistant reads more of what you mention.**
  - **An `@` mention of a PDF or an EPUB now sends its text.** A PDF sends the
    pages around the one you last read. An EPUB sends the chapters around the
    first chapter you have not marked done. Before, both sent nothing.
  - **The assistant reads the source of a repository file.** In the `@` menu, a
    file shows as `<repository>/<path>`.
  - **The model reads plain text, not HTML.** Code in an answer no longer shows
    `&lt;` for `<`, and a long code block keeps its lines.
  - **Answers on Gemini 3.8 Flash and Gemini 3.5 Flash-Lite cost less.** The
    request now lets the provider cache your sources between turns. In two
    measured turns, the first one cost $0.0021 instead of about $0.0096.
  - **Answer details lists thousands of sources without slowing the page.** A
    `Show` button opens the excerpt below the list.
- **You can put an MP3 clip inside your text.** Drag it in, paste it, or use
  `Insert audio` in the format menu or the palette. It plays in the document, in
  extracts and on cards. Right-click a clip to turn Loop on or off. Drag the
  grip on a clip to move it to another line. In Anki, the clip plays with the
  Replay button.
- **The editors understand Markdown as you type or paste.** `#` makes a heading,
  `- ` a bullet, `1. ` a numbered list, `> ` a quote, and ```` ``` ```` a code
  block. `**bold**`, `*italic*`, `~~strike~~`, `` `code` `` and `[label](url)`
  work inline.
- **Tab indents in the document editor.** `Shift+Tab` outdents. Press `Esc`,
  then `Tab`, to leave the editor. In a card field, Tab still moves to the next
  field. The new `Increase indent` and `Decrease indent` buttons in the format
  menu indent there.
  - **Lists are easier to read.** Items have space between them, and the bullet
    is larger. Cards and Anki notes use the same list style.
  - **`⌘Y` (`Ctrl+Y`) redoes.** `⌘⇧Z` still works.
  - **Closing a modal puts the caret back where it was.** It used to jump to the
    start of the document.
- **Card changes.**
  - **`⌘⇧Enter` (`Ctrl+Shift+Enter`) clicks `Add & Keep`.** Both Add buttons show
    their shortcut in a tooltip. Freeze gains `Freeze Both`.
  - **Copy card can change the card type.**
  - **The × on a card row hides the row at once.** A failed delete puts it back.
  - **A PDF page's card table lists the cards of its extracts too.** Before, a
    page showed `No cards yet` while its extract held cards.
  - **You can choose whether card generation uses approved facts.** Settings has
    a default, and Document options can override it for one document.
- **Duplicate cards are removed once, and FreeMemo blocks new ones again.** Card
  dedupe had not worked since 2026-08-03. For each set of duplicates, FreeMemo
  keeps the card linked to Anki or with the most reviews. That card gets the
  schedules, reviews and history of the others. If a removed card had other
  text, History keeps it as a version.
- **Focus reader works on Topic, EPUB, Web, Wikipedia and Markdown items.** It
  worked on PDFs only before.
- **A text topic opens where you stopped reading.** Before, every text topic
  except a PDF opened at the top. FreeMemo now keeps your place in the notes
  editor, the poem reader, and both views of a code file.
  - **Your place survives a topic switch, a reload and another device.** It is
    saved one second after you stop scrolling, and when you leave the topic.
  - **An edit above your place does not move it to the wrong paragraph.**
  - **A link to a heading still opens at that heading.**
- **A PDF reopens at the fit you last chose**, `Fit page` or `Fit width`.
- **Document options saves each field as you change it.** The modal has only a
  `Done` button now, and it uses the same groups and rows as Settings. Its
  buttons show their labels at every width.
  - **`Done` waits for a save in progress.** A failed save keeps the modal open
    and shows the error.
  - **Two labels changed.** `Push to children` is now `Copy to extracts`, and
    `Facts for card generation` is now `Use approved facts`.
  - **Text typed during a save is no longer lost.** Before, an older save could
    finish last and replace the newer text.
- **Deleting a curriculum can be undone for 12 hours.** The toast has an Undo
  button, and `⌘Z` works too. Exam credits already spent stay spent.
- **You can import a poem package and read it.** The first package is معلقة
  امرئ القيس. Tap a word to read its gloss and its إعراب, with the source of
  each. Buttons in the panel make cards and extracts from a verse.
- **Settings is easier to scan.** Each tab has one card, with groups inside it.
  A new `Usage` tab holds Credits, Storage and AI costs.
  - **AI costs shows token counts for new charges.** It fits its card, and its
    empty message shows when there are no rows.
  - **A prompt you edited shows a chip.** The chip says when the default prompt
    changed after your edit. `View default` shows the new default.
  - **Settings → Appearance has `Code Block Font Size`.**
- **Page scans store math as `\(TeX\)`.** A scan used to store subscripts as
  HTML, which the editor could not show as a formula.
- **A done item is easy to see.** Its toggle is solid green, and the toolbar row
  gets a green tint. A score now has a Done button, and `⌘D` marks it done.
- **The browser tab names what you have open**, as `<name> | FreeMemo`.
- **Anki cards center code blocks and tables.** A code block is as wide as its
  longest line. A line wider than the card scrolls instead of wrapping.
- **The toolbar's Anki Sync is one button, not a menu with one item.**
- **The palette finds `Import document…` and `Import code…`.**
- **Lists, results and table rows work with the keyboard.** Enter or Space opens
  them, and sortable headers say their sort order.
- **Library shows better on a phone.** Side panels cover the document instead of
  squeezing it, and the card table columns line up.
- **Input borders are darker**, at 3:1 contrast or more in both themes. Buttons
  and the assistant's Mode select now use the app font.
- **The Library type filter lists every document kind.**
- **Model changes.**
  - **New: GPT-6 Sol, GPT-6 Luna and Ling 3.0 Flash.** Ling 3.0 Flash is the
    cheapest card model.
  - **Retired from the card picker:** GPT-5.1, GPT-5.6 Luna, Terra and Sol,
    Gemini 3 Flash, Gemini 3.6 Flash and Claude Haiku 4.5. The same GPT and
    Gemini models leave the scanning picker. A saved choice moves to GPT-6 Sol,
    GPT-6 Luna, Gemini 3.8 Flash or Claude Sonnet 5.
  - **GLM 5.3 Flash goes to its fastest provider, not its cheapest.** The
    cheapest one was often slow.
- **Removed: CSV export.** Its button, its palette command and its menu item are
  gone.
- Bug fix: **the page Wayl opens after a payment crashed for a pending order.**
  It now shows the order, and a pending order shows `Payment confirmed` when it
  completes.
- Bug fix: **moving through Learn items could freeze the tab.** No crash
  happened in 150 test advances after the fix.
- Bug fix: **a server crash froze the tab with no message.** The connection-lost
  banner now shows within a few milliseconds.
- Bug fix: **a very long review interval or Postpone ended the session.** An
  interval now stops at 100 years.
- Bug fix: **the assistant transcript could go blank after a send** and stay
  blank until a reload.
- Bug fix: **a citation that named several sources showed as raw text.** Each
  source now gets its own link, in old answers too.
- Bug fix: **a link import did not show in the Activity monitor.**
- Bug fix: **in dark mode, pointing at a node in Graph hid its label.**
- Bug fix: **importing a document again after you deleted it could open the
  deleted copy.** The new import gets a numbered title, such as `Title (2)`.
- Bug fix: **a self-hosted server could not start on an empty database.**

### Known issues

- **Best costs more than the level many accounts used before.** This update set
  every account to Best. Choose Balanced or Economy in Settings → AI Features to
  spend less.
- **A model you chose for one document is no longer used.** The level decides.
- **Gemini 3.8 Flash has not been tested on Arabic scans.** The earlier Arabic
  test chose Gemini 3 Flash, which this update retires.
- **The GLM 5.3 Flash routing change has not run against the live service.** It
  also gives up a temporary discount on the cheapest provider.
- **The duplicate repair can delete Anki notes.** When FreeMemo removes a
  duplicate, your next Anki sync deletes its note if no other card holds it. A
  note you edited in Anki shows as a conflict instead.
- **Editing a card so that it equals another card shows a database error.**
- **Audio clips must be MP3.** Loop does not reach Anki. You cannot seek inside
  a clip, and a removed clip still uses storage.
- **A repository import cannot change its selection afterward.** An import makes
  at most 20,000 topics. A file over 1 MiB, or a binary file, becomes an empty
  topic.
- **An `@` mention of a score sends nothing now.** The `@` menu no longer offers
  scores.
- **Pages you scanned before this update keep their HTML math.** If you saved
  your own OCR prompt, it does not get the new math rule.
- **AI cost rows from before this update still show 0 tokens.**
- **`Extract a passage` in Get started can read not done** if all your extracts
  came before this update.
- **To leave the document editor with the keyboard, press `Esc` and then
  `Tab`.** Tab alone indents.
- **A poem imported before the package rebuild keeps its old text.** Delete it
  and import it again.
- **Video, audio and score topics do not remember a reading position.** A PDF
  remembers its page, as before.
- **After you go from one video to another, the old transcript shows for a
  moment.** It stays until the new transcript loads.

### Technical

- Five new tables. `poem_packages` holds one poem's JSONB package.
  `topic_code` holds a repository file's source, and `repo_selections` holds an
  import's forge, ref and selection. `app_migrations` holds the keys of
  one-time data migrations, for `db/run-once!`. `reading_positions` holds one
  row per topic and surface.
- A reading position is an anchor `{i, fp, frac}`: a block index, a text
  fingerprint, and the fraction of the block above the top edge. The browser
  sends it with `sendBeacon` to `POST /api/reading-position`, only after the
  reader's own input. A restore runs once per content write, against its own
  topic only, and re-applies for up to 3 s while images settle.
  `freememo.reading-anchor` holds the pure part.
- New columns: `topics.repo_path`, `commit_sha`, `repo_url` and `is_extract`,
  `curricula.staged_delete_id` (FK to `undo_log`), and
  `upload_sessions.forge_source`. `undo_log`'s CHECK constraints accept
  `delete-curriculum`. Staged deletes no longer count against the 100-entry cap.
- Account export covers 41 tables, up from 37: `poem_packages`, `topic_code`,
  `repo_selections` and `reading_positions`. `app_migrations` is excluded. A
  staged curriculum is left out. New settings prefixes that embed a topic id:
  `focus_reader_`, `pdf_fit_`, `code_view_`, `editor_hidden_` and
  `right_panel_tab_`.
- `db/move-everyone-to-best-level!` runs once, under the `app_migrations`
  marker. The second deploy color waits on the first one's key and skips.
- `setup-schema-flashcard-uniqueness!` is now the last schema step. In one
  transaction it removes duplicates, moves their schedules, reviews, history and
  Anki tombstones to the survivor, and builds the two `md5` indexes.
  `plans/flashcard-dedupe-audit.sql` is a read-only count to run before deploy.
- `idx_topics_source_content_digest` moved to `setup-schema-sources!`, after the
  column it indexes. On an empty database the boot threw before.
- `db/structural-kinds` (`page`, `repo`, `commit`, `dir`) keeps structure-only
  topics out of every review query. Topic kinds go from 12 to 17, and
  `freememo.topic-kinds/all` is the first single list of them.
- New env vars: `FORGE_ALLOW_PRIVATE_HOSTS` (production sets it in
  `docker-compose.truenas.yml`) and `SLOW_READ_LOG_MS` (default 250). The forge
  token key comes from `ENC_KEY_SECRET` and the user id.
- New dependency `flexmark-ext-footnotes` 0.64.8. Vendored Amiri 5.3.0 and
  Floating UI 1.8.0. Run `clj -X:build vendor/fetch!` once.
- Every AI debit row records prompt, cached, completion and reasoning tokens. A
  0 means the provider reported zero, and NULL means it reported nothing.
- The assistant request puts the current topic's sources before the transcript.
  The last source turn carries `cache_control` on the two Gemini models, and
  `/compact` starts a new cache. Claude Sonnet 5 is excluded, because Anthropic
  bills a cache write at 1.25 times the input price.
- The transcript sends only the evidence rows an answer cites. On a 238 KB repo
  file, the largest message in a send went from 702 KB to 2.9 KB, and opening
  the chat went from 3.5 MB to 17 KB. `run-details*` no longer sends
  `:source_snapshot`, which was 1.1 MB on the measured run.
- `freememo.electric-process-failure` logs `::process-failed` with the stack,
  the user id and the path. `client-boot/boot!` raises the banner on close code
  1011 or 1012. `DocumentRoot` drains the ids the server reads back.
  `bugs/20260926-electric-input-diff-corruption/` holds a one-file reproduction
  of the `:diff-corruption` for upstream.
- `get-flashcards-for-subtree` is one recursive CTE, not one parameter per
  topic.
- Document options saves each field through one service. The server numbers
  each value as it arrives and writes it under a per-field lock, and it skips a
  write older than the newest one stored. A cancelled `e/Offload` cannot stop a
  JDBC write that has started, so the lock is what keeps the newest text. The
  Escape probe lost text in 1 of 10 runs before, and kept it in 20 of 20 after.
  `forms-ext/Select!` is a tracked `<select>` with the `Input!` contract.
  `document-options-targets` holds the one rule for which topics the modal, the
  toolbar and History act on.
- The subset review queue, the undo history, the video transcript and both
  history-modal reads now run in `e/Offload`, and each one logs its time. The
  subset queue reads eight columns instead of the whole row.
- Production logs are in Loki on the TrueNAS box. `scripts/prod_logs.py` reads
  them. `broadcast-changelog.sh` now runs `freememo.changelog` in a container on
  `BROADCAST_HOST` and needs no Clojure CLI.
- `spec/` holds a portable FreeMemo specification, version 0.1. Publish-clean
  excludes it. `extensions/benchmark-card-generation/` is a card generation
  benchmark that spends the operator's own OpenRouter credits.
- `CLAUDE.md` gains patterns for an `e/input` flow with no value at mount, an
  `e/for` that shares a branch slot, an overlay that adds no height, and a PDF
  viewer that never swaps in place. It also gains the tab-title and nav-rail
  obligations and a Production logs section.
- These were not driven in a browser before release: the signed-in sidebar
  check, the rendered Markdown pane, the repository import progress and picker
  search, the done-state checks, Markdown shortcuts, and the live check of audio
  clips.

## v20260910-9781008

### For users

- **Curriculum is a new tab for a study program you build from your own
  documents.** You put imported material into ordered blocks. A long-form exam
  decides when a block is complete, and Schedule plans your focus block first.
  - **An exam asks five essay questions, and the AI grades your answers against
    passages from your material.** The grader must cite a passage for every
    deduction. A failed block exam can produce a targeted retry that stresses
    the outcomes you missed.
  - **A material can have its own exam too.** Its score guides your study and
    unlocks nothing. You can also unlock the next block without a pass, and the
    control says what it skips.
  - **Each exam has its own page, and a reload keeps your place.** An answer
    saves when you leave its box. The timer comes from the server, so a reload
    20 minutes in still reads 20 minutes.
  - **Exam generation takes minutes, so the server owns it.** A refresh does not
    stop it, the Activity monitor can cancel it, and a deploy waits for it.
  - **Every paid exam action states its cost.** Generate, Regenerate, Targeted
    retry and Regrade each carry a credit note. After a run, the page shows what
    FreeMemo charged, and AI Costs lists both exam operations.
  - **You can cancel a sitting you started by mistake, then archive it.** A
    cancelled sitting takes no answers and gets no grade. A submitted sitting
    stays on the list, because a grade can already cite its answers.
  - **A curriculum can be deleted and a block renamed.** Material search now
    reaches every document you imported, and the kind row only narrows it.
  - **The timed exam on the Quiz tab is now called Timed Quiz.** It works as
    before. Its palette entry is `Start timed quiz…`.
- **Focus reader shows a PDF as one document you read from top to bottom.** It
  hides the PDF pane and centers the text in a column about 720px wide. Every
  page joins into one scroll, with a `Page N` rule at each boundary.
  - **Turn it on from the PDF toolbar or the palette.** FreeMemo remembers the
    choice for each document.
  - **An edit still belongs to its page.** A save splits the text back into the
    page rows, so cards, extracts, search and `p. N` links on Anki notes stay
    with their page.
  - **Your page survives a deep link, a reload and leaving the mode.** Page
    turns inside the mode are saved, so the document reopens where you stopped.
  - **The format menu can restore a page boundary you deleted.** It restores the
    lowest missing boundary. It never creates a new page.
- **`Previous page` and `Next page` are palette commands with shortcuts.** Use
  `⌘⇞` and `⌘⇟` on macOS, or `Alt+PgUp` and `Alt+PgDn` elsewhere.
- **Scan Page keeps paragraphs whole.** It used to break a paragraph at every
  printed line, so a page of four paragraphs could store forty. A word
  hyphenated across two lines now joins again, so search can find it.
- **The assistant streams its answers, and `Stop` ends one early.** Text appears
  as the model writes it. A partial answer stays after a reload.
  - **Each answer keeps the sources it used.** A citation opens a formatted
    preview of its passage, with the cited text highlighted. `Answer details`
    shows the model, the provider, token use, cost and the context the answer
    read.
  - **`Retry answer` sends the exact request of a failed answer again.** The
    `/compact` command summarizes a long chat for later answers. The original
    messages stay visible.
  - **Web Search works with every assistant model.** You turn it on per chat,
    and it starts off. Links from the web show apart from your document sources.
  - **General mode answers from general knowledge when your material has no
    answer.**
  - **The viewer layout changed.** The assistant has the right rail. Hierarchy
    and Pins share the left pane, and Hierarchy opens first. A new Settings
    option, `Full workspace`, extends the assistant beside the card table.
  - **An `@` reference stays in the message you send.** The reference menu
    searches all your documents instead of showing eight matches.
- **The knowledge graph starts from a search.** You filter by document and
  relation, open a concept's neighborhood in rings by distance, and read its
  facts in a docked panel. Long relation and fact lists render only the rows on
  screen.
- **Library, Learn and Home draw their layout at once and fill each part as its
  data arrives.** Library sorts by Done and Synced without recomputing progress
  on each visit. It loads a document's children only when you expand it.
- **Library is easier to use on a phone.** Filters get fixed space, a document's
  state reads as one compact line, and the row menu is a 44px touch target.
- **Schedule changes.**
  - **The schedule pill in the reading view names the schedule for the page you
    are on.** A PDF split into a finished half and a live half used to show the
    finished one.
  - **A schedule past its target date shows a report instead of a warning.** It
    says how long ago the target passed, what the schedule reached, and when
    every carded page reaches your retention target. `Archive` sits in that
    report.
  - **Home tells you how many pages each schedule wants today.** The book name
    links to the first unfinished page in that schedule's range.
  - **`Tag range cards` adds one Anki tag to every pushed note in a schedule's
    page range.** It reports the notes it tagged and the unpushed cards it
    skipped.
  - **The detail pane has one scroll bar, not two.**
- **Anki review state is visible in FreeMemo.** Card rows in the viewer and in
  Library show when a note is marked, suspended or carries Remarks. FreeMemo
  reads that state live from Anki. Three palette commands find the next marked,
  suspended or remarked card.
  - **The editor of a pushed card can change that state.** Marked and Suspended
    are toggles, and an edit to Remarks saves with the card. Occlusion and score
    cards do not get these controls.
- **PDF page completion responds at once.** The done control and the page count
  change when you click. A failed save puts them back and shows an error.
- **Add Card gains `Add & Keep` and `Freeze`.** `Add & Keep` adds the card and
  keeps the modal open for the next one. `Freeze` keeps the front or the back
  across cards. Card kind is now a dropdown, and a Basic card has a flip button.
  - **`⌘\` (`Ctrl+\`) clears formatting from the selected text.**
  - **A Basic card switched to Overlapping and back keeps its question and
    answer.**
- **The palette lists every command that applies, not the first 12.** New viewer
  commands show and hide panes: `Toggle Assistant pane`, `Toggle Left pane`,
  `Show Hierarchy`, `Show Pins` and `Toggle Card Table View`. The viewer keeps
  that pane state after a reload.
- **`Parse questions` in the format menu turns selected lines into Basic cards
  with no AI call.** Each line must read `Question? Answer`, with one question
  mark. One bad line rejects the whole selection, so you never get a partial
  set. The lines that made cards are highlighted afterward.
- **`Apply language to all code…` sets one language on every code block in a
  document.** It also rewrites the code blocks in that document's cards. The
  picker shows its progress while it works.
- **Code blocks can highlight Zig and C.**
- **A title keeps its slashes, backslashes and `\(TeX\)` delimiters.** "Foo /
  Bar" used to become "Foo Bar". A downloaded PDF also keeps the title's own
  characters in its filename, non-Latin scripts included.
- **New models: Gemini 3.8 Flash and GLM 5.3 Flash.** Both appear wherever you
  pick a model, OCR included. Gemini 3.8 Flash costs the same as 3.6 and scores
  higher, but it is slower. Gemini 3.5 Flash is retired, because 3.6 is
  cheaper, faster and scores higher.
- **The Help page covers Schedule, Curriculum, exams and the new assistant
  controls.**
- **The Zotero plugin works with Zotero 10.** Install version 0.1.1 from the link
  in Settings. It also stops listing child items and items in the trash.
- Bug fix: **AnkiMobile could not find image occlusion masks.** Anki 26.05
  lowercases media filenames, and FreeMemo kept uppercase names in the note.
  Both sides now use lowercase.
- Bug fix: **a slow card generation could save its cards and still report a
  timeout.** The result of the work now decides the outcome.
- Bug fix: **saving a card could scroll the document away from where you were
  reading.**
- Bug fix: **reasoning effort `None` made every call fail on Gemini 3.6 Flash and
  Gemini 3.5 Flash-Lite.** Those models cannot turn reasoning off. FreeMemo now
  sends the provider default for them, and for the two new models with the same
  limit.

### Known issues

- **`Stop` does not stop the model call.** The server reads the rest of the
  reply and records its cost. You keep only the text that arrived before you
  clicked.
- **The assistant keeps a record of every answer until you delete its chat.**
  The record holds the context the answer read, its sources and the provider's
  data. Deleting the chat removes all of it. Your credit ledger entries stay.
- **With Web Search on, every message in that chat runs a web search first.** A
  search can add to the cost of the message. A model with no search of its own
  uses OpenRouter's fallback search engine.
- **Pages you scanned before this update keep their line breaks.** Only a new
  scan applies the new rule, and a scan costs credits. If you edited the OCR
  prompt in Settings, press Reset to get the new default. The Mistral OCR model
  reads no prompt, so it did not change. No billed scan has checked this change
  yet.
- **Focus reader is slow on very large PDFs.** Typing costs about 46ms per
  keystroke at 883 pages. Leaving the mode holds the page for about two seconds
  while the PDF viewer builds again.
- **Deleting a `Page N` rule moves that page's text into the page above.** The
  page itself stays, empty, until you restore its boundary from the format
  menu. Focus reader cannot add a page.
- **`Apply language to all code...` cannot be undone as one step.** `⌘Z` does
  not reach it. Every card it rewrites becomes unsynced, and the card history
  keeps the earlier version of each card. It has not run against the 707-block
  document that froze the tab before the fix.
- **An occlusion card synced before this update keeps its old mask names in
  Anki.** This update does not mark those cards for sync. Contact me if
  AnkiMobile still reports missing masks.
- **A Gemini 3.5 Flash selection now runs Gemini 3.6 Flash, with no message.**
  The stored choice is not rewritten. Old AI Costs rows keep the 3.5 name.
- **Reasoning effort `None` has no effect on four models.** Gemini 3.6 Flash,
  Gemini 3.8 Flash, Gemini 3.5 Flash-Lite and GLM 5.3 Flash always reason, and
  reasoning tokens are billed.
- **GLM 5.3 Flash has no fixed price.** OpenRouter sends each call to the
  cheapest provider at or under 0.15 USD in and 0.50 USD out per million tokens.
  The launch price came from a discount that ended on 2026-09-09.
- **A title the old rule changed stays changed.** The removed characters are
  gone. An imported code repository keeps its `a > b` titles until you import it
  again.
- **Zig highlighting follows Zig 0.8.** It still colors `async` and `await`, and
  it does not know `addrspace`, `opaque` or `anyopaque`.

### Technical

- Eleven new tables. `curricula`, `curriculum_blocks`, `curriculum_materials`,
  `curriculum_exam_forms`, `curriculum_exam_sittings`,
  `curriculum_grade_revisions` and `curriculum_unlock_overrides` hold the
  curriculum. `assistant_runs`, `assistant_checkpoints` and `assistant_evidence`
  hold one row per assistant turn, compaction checkpoint and retained excerpt.
  `document_summaries` holds Library progress per root.
- New columns: `assistant_chats.web_search`,
  `assistant_chats.provider_session_id` (NOT NULL, unique),
  `assistant_evidence.quoted_html`, and `curriculum_exam_sittings.draft_answers`,
  `cancelled_at` and `archived_at`. A sitting is a sequence of event stamps, not
  a status column. `idx_topic_repetitions_user_event_day` bounds the streak
  query.
- Statement-level triggers on `topics` and `flashcards` keep
  `document_summaries` current, and `setup-schema-document-summaries!` rebuilds
  every root once at boot. The Done and Synced sorts read it instead of running
  recursive aggregates on navigation.
- Account export covers 37 tables, up from 27: the seven curriculum tables and
  the three assistant-run tables.
- `freememo.page-merge` owns the merge and the split of a PDF's pages, and
  `merged-page-bridge` owns scroll and restore. A marker is
  `<h6 id="fm-page-N">`, and no marker reaches a stored row. A split writes only
  the pages whose slice changed. `dctx/editor-doc-id` names the document the
  editor holds, apart from the page's `topic-id`.
- A focus-reader toggle wraps four branch swaps in `e/Reconcile`, which took
  leaving the mode from 4487ms to 2070ms of long tasks. `DocumentBody` still
  unmounts once per toggle, because `DocumentViewState` returns the body inside a
  map. `plans/focus-reader-toggle-cost.md` records the fix.
- The default OCR prompt drops `<br>` as a general element and adds a reflow
  rule and a hyphen rule. Only the `:chat` lane sends it.
- Every structured LLM lane asks for JSON, not EDN. `freememo.llm-json` replaces
  `llm-edn`, and `structured_llm_contract_test` pins each lane's contract. An EDN
  reply could reach a lazy JSON fallback whose Jackson error escaped the retry
  boundary.
- `openrouter/model-fields` turns a registry entry into `:model` and
  `:provider`, so an entry can set its own routing. `kg-llm/chat!` takes those
  fields. The `::call` signal logs the provider that served each call, and
  `api-error` keeps `Retry-After` and the `X-RateLimit-*` headers in ex-data.
- A retired registry entry keeps its id, its label and its successor, and
  `effective-id` maps a stored id onto the successor at six read sites.
  `:reasoning-mandatory?` drops `reasoning_effort` for entries that refuse
  `none`.
- Web Search sends `openrouter:web_search` with engine `auto` and
  `tool_choice "required"`. Run metadata records `:web-search-requests`.
- The assistant panel is split across `assistant-composer`, `-transcript`,
  `-evidence`, `-run-details`, `-panel-data` and more namespaces.
  `:assistant-rich-render?` (`ASSISTANT_RICH_RENDER`) is an operator switch that
  shows completed answers as plain text.
- `POST /api/client-reactor-failure` receives a beacon when the client reactor
  dies. It carries bounded run identifiers and a content-free interaction trail.
- Exam form generation is an Activity monitor type. `prod.cljc` installs a
  shutdown hook that waits for accepted exam work up to its 20-minute timeout.
  The reporter now closes only activities its own process observed.
- Titles pass `input-check/sanitize-title`, which strips control characters
  only. `input-check/content-disposition` writes a folded ASCII `filename` and
  an RFC 8187 `filename*`.
- Logs go out as one-line JSON, from the container and the rotated files, so
  Loki keeps each event whole. Email alerts stay readable.
- A blank Google OAuth credential now reads as absent. The boot check emits an
  `:error` signal routed to email instead of a warning in `logs/app.log`, and
  `auth-url` has a precondition.
- `deploy.sh` pushes deploy state to a Pushgateway in two job groups. A failed
  push never fails a deploy.
- Vendored `highlightjs-zig` 1.0.2 in its own directory. No `deps.edn` change.
- `/curriculum` is the fourteenth tab. Its exam routes dispatch inside
  `CurriculumPage`, so they do not move the Routes count.
- New Playwright probes drive focus reader, merged reading, toggle cost, the
  schedule end state and the curriculum page. One more drives an assistant run
  with only the provider stream stubbed.
- `CLAUDE.md` gains six patterns, each from a defect that shipped. They cover
  capitalized `e/fn` parameters, an `e/Offload` that reads what it writes,
  identity keys for a reorderable `e/for`, and a `case` that remounts its
  branch. They also cover an absent props map, and a nested `(e/server …)`
  inside `e/for`. `AGENTS.md` carries the same guidance for other coding agents.

## v20260819-11689b6

### For users

- **An Activity monitor shows every long-running operation, and can stop most of
  them.** Page scans, card generation, imports, transcription, fact extraction,
  account export and import, and Anki sync all report into one panel with a
  phase, a progress figure and a start time. Before this, each one sat behind a
  modal spinner, and a refresh mid-operation had unknown consequences — you could
  not tell whether the work continued, died, or half-finished.
  - **A row opens the moment you click.** Discovery used to run on a 20-second
    tick, so work that finished inside one tick was never seen running at all.
    Three card generations on one page reached the panel as a single row already
    reading `Done`.
  - **Cancel reaches six operations**, a running card generation and a running
    page scan included.
  - **Cancelling a page scan now leaves your page alone.** It used to set the row
    to `cancelled` and then let the scan finish and overwrite the page you had
    just abandoned. A second row appeared beside the first and closed as a
    success.
  - **A cancelled run reads as cancelled, not as a failure.** Cancelling a scan
    raised the toast `Scan cancelled` as an error and recorded a scan error on
    the page.
- **The quiz tab answers "how is this question doing" and "how much did I
  study".** A per-item panel opens from a review, from a Knowledge question row
  and from a Library card row. It carries the review record, the scheduler
  numbers, a forgetting curve and the time each turn took.
  - **A footer states today's reviews, the minutes spent, the per-item average
    and your streak.** The `Reviewed today` tile is gone, because the footer says
    the same number with its context.
  - **Time per turn is recorded from now on.** Reviews taken before this update
    carry no timing, and every figure states how many rows it counted rather than
    averaging a missing value as zero.
  - **Both history lists scroll like the Knowledge and Library tables.** They
    were the last unwindowed tables in the app.
  - **Cmd/Ctrl+Enter presses whichever button is primary**, so one chord serves
    Submit and Next. Keys `1` to `4` rate a revealed card.
  - Bug fix: **your review streak was reported as 1 day, whatever its real
    length.**
- **Fact extraction skips pages it has already read, and resumes where it
  stopped.** Distillation re-sent every page to the model on every run, and it
  inserted nothing until the last page returned. A cancel or the 20-minute
  timeout threw away every paid call, and a document past roughly 300 pages
  could never finish inside that timeout at all.
  - **`Cancel` is now `Pause`,** because it discards nothing.
  - **Only pages whose text changed are read again.** Editing a page brings it
    back into the next run.
  - **Knowledge → Documents gains a `Pages` column** reading done against total,
    plus any pages the run gave up on.
- **Compare OCR asks which models to run.** Opening it used to fan out over every
  allowed model, which is nine billed calls, serialized into about five waves
  before the first column appeared. The modal now opens on a checkbox list with
  your document's own model already ticked, and issues no call until you confirm.
- **Every link in the editor now does something, and they all do the same
  thing.** A Wikipedia link offered an Import popover, a FreeMemo topic link
  navigated, and every other link did nothing at all — a click inside an editor
  does not follow an address. On one account 833 of 1046 external links were
  inert.
  - **An external link offers Import and View.** The import runs in the
    background and reports as a toast, so the reader is not held while a page is
    fetched.
  - **A link to a heading in the same document scrolls to it.** Headings now keep
    their anchors through storage and editing, which no imported document could
    do before.
  - **Clicking a link to a page you already have can open your copy.** A new
    Appearance checkbox controls that.
- Bug fix: **an imported table renders as the table it was.** A Wikipedia
  article stored two tables and the editor drew one: a maintenance banner and an
  infobox merged into a single grid whose first row was four cells wide and whose
  other nine rows were one cell wide, with two rows the article does not have.
  Nested tables, captions, header cells, hidden rows, merged columns and lists
  inside a cell are all lowered into a shape the editor can hold.
- Bug fix: **the format menu no longer covers the text you selected.** It
  anchored to the selection's first line and preferred the space below it, so
  every selection of more than one line put the menu inside the selection.
- Bug fix: **Anki sync no longer freezes the tab.** Since the Activity monitor
  landed, every sync killed the page: the tab froze with nothing on screen, the
  server logged a clean disconnect, and no sync run ever reached an outcome.
- Bug fix: **a client failure now says so on screen.** Every client crash
  reported one wrong message and threw the real one away, so a frozen tab was
  all you saw. A banner now appears for a crash and for a server that goes away.
- Bug fix: **a SuperMemo citation named the wrong article.** `reference.dat`
  stores reference R at index R-1, so every imported element cited the article
  after its own, and an element citing the last reference got none. New imports
  are correct. I have a repair for collections already imported, rehearsed
  against a copy of production — see Known issues.
- **A schedule links to the document it plans.** The sidebar card, the home card
  and the detail heading all reach it. The tab used to be a dead end for the book
  it schedules.
- **The schedule's `Pages` table says which date it describes.** Every row is the
  projection at the horizon, never today, and four rows reading `known` against a
  `Known now` tile of `0 / 23` had nothing on the table to explain them.
- **The Zotero plugin moved to `extensions/zotero`.** The download link in
  Settings moved with it. A link you saved by hand no longer resolves.
- **A change to formatting now syncs.** Styling a word as `code`, bold, italic or
  any other inline format counts as a change, in both directions. Before this,
  sync compared the visible text only, so a formatting-only edit was invisible —
  the card table said `1 unsynced` while the run said `already in sync`, and
  nothing could clear it.
- **Changing a document's Anki header or its bibliography marks its cards for
  sync.** Those two live outside the card, so nothing used to notice they had
  changed and the notes kept the old header or citation for good.
- **Every unsynced count and badge now answers one question.** The card table, the
  Library badges, the document tree's Unsynced filter and the Status tab all read
  the same rule the sync run reads, so they cannot disagree.
- **An edit made in Anki keeps its formatting when it comes back.** A pull used to
  overwrite the local card with plain text, discarding the formatting on both
  sides.
- **A pulled edit keeps your images.** Pulling a card that holds an image used to
  drop the image.
- **A card you exported to CSV no longer shows as synced to Anki.** It shows as
  unsynced, which is what it is — exporting is not pushing. The Library already
  said `unpushed` for those cards while the document card table said `synced`; the
  two now agree.
- **The conflict diff shows formatting differences.** It compared visible text, so
  a conflict caused by formatting alone appeared as two identical texts under a
  removed/added legend, with nothing to choose between. When the two sides really
  are identical it now says so.
- **`Keep FreeMemo` and `Keep Anki` are greyed out unless a conflicted card is
  selected.** They used to look available on any selection and then do nothing but
  print a small note beside themselves.
- **Your first sync after this update rebuilds its records instead of resending
  your collection.** Change detection reads formatting now, so no record written
  by the old version matches. FreeMemo notices that and rebuilds the records from
  what both sides currently hold — nothing is sent to Anki and no card is reported
  as changed.
- **A Chrome extension captures the page you are reading.** FreeMemo could only
  import a page the server could fetch itself, which excludes most pages worth
  keeping: documentation behind a login, an application that draws itself in the
  browser, one paragraph in the middle of an article. Reddit, Amazon, Zillow and
  IMDb refuse a server fetch outright.
  - **Four things can be captured** — the page as rendered, the current
    selection, a full-page screenshot wrapped in a PDF, and a PDF already open
    in Chrome's viewer, uploaded as the file it is.
  - **The popup closes at once and the capture continues in the background.** A
    capture can take tens of seconds.
  - **A second capture of the same URL is a new topic when the content differs.**
    One URL legitimately gives a selection, a re-read and a later revision, so
    the duplicate check compares content rather than the address.
  - **Settings holds the install steps** in a new browser-extension section.

### Known issues

- **Cancelling a model call does not refund it.** The request goes through an
  HTTP client that ignores an interrupt, so the call runs to completion and the
  credits are spent. What cancelling saves is the result: a scanned page is not
  overwritten and generated cards are not inserted. That is the part still
  available once you have clicked.
- **Cancel a page or a card and immediately run it again, and the abandoned run
  may still save.** Each owner clears its cancel marker when new work for the
  same page arrives. You asked for that content twice, so the outcome is an extra
  result rather than a wrong one. Accepted rather than closed.
- **An operation that reports failure only as a toast is recorded as a success in
  Activity history.** The row closes clean and the toast is the only record. URL
  import records its error; the others do not yet.
- **A Compare OCR run is lost by `Close` and by `Change models`.** Nothing
  persists the columns outside the open modal, so a paid comparison cannot be
  reopened. The same gap applies to card compare.
- **A page that returns no facts is not read again until you edit it**, and a page
  whose extraction fails twice on unchanged text is given up on and reported per
  document. Both are deliberate: before this, every such page cost money on every
  run. `Forget extractions` is the way back for a whole document.
- **A capture records only what the page had drawn.** On one GitHub issue that
  was 26 of 404 comments, because the page draws comments as you scroll. A long
  capture is also absent from the default fuzzy search, which ranks the 200
  shortest matches. Exact search still finds it.
- **Chrome stops an idle extension worker after 30 seconds.** Each step of a
  capture resets that timer, so a normal capture is safe. A single request that
  takes longer than 30 seconds to answer at all is not.
- **A document imported before this update has no heading anchors.** Its links to
  its own headings cannot land anywhere until you import it again.
- **A merged table row keeps its height, not its span.** `rowspan` is dropped
  rather than expanded, which no article I sampled needs. Two adjacent tables you
  author inside FreeMemo still merge into one.
- **The link affordance and the quiz panel have not been driven in a browser.**
  Every Clojure test passes and both compilers are clean, which covers the
  classification and the sanitizer and nothing of the interface. Tell me what
  breaks.
- **`Open my copy instead` takes effect on the next editor.** Changing a setting
  does not reach an editor that is already open. Leaving the Settings page is
  enough.
- **A card you edited in FreeMemo but never synced still needs one decision.**
  Three such cards existed across all accounts when this shipped. They wait in
  Library → Cards; `Keep FreeMemo` sends your edit to Anki.

### Technical

- Three new tables. `activities` is one row per operation, with a unique partial
  index on in-flight work per user and type. `kg_page_extractions` records a
  page's content digest, attempt count and extraction version.
  `kg_staged_triples` holds extracted triples until linking runs.
  `kg_reviews.duration_ms` records the turn time and is NULL for every earlier
  review, with no backfill.
- `freememo.activity`, `activity-registry`, `activity-reporter`,
  `activity-panel` and `activity-client` are the monitor. No owner namespace
  calls `start!` or `terminate!` — the reporter DISCOVERS work by reading the
  per-user atoms owners already publish, so an owner joins by adding a registry
  entry. `us/work-started!` opens the row on the click path instead of on the
  20-second tick.
- `freememo.table-lowering` owns every table an importer stores, and
  `rewrite-for-quill!` calls it for every caller. Two table steps existed behind
  `:browser-dom?` and served the capture extension alone for their whole life,
  while the Wikipedia path called no lowering at all.
- `freememo.page-pipeline` holds the shared import HTML transform that was
  private to `wikipedia` and welded to its fetch. Output for the fetch path is
  byte-identical to the pre-move code.
- `clean-html` allow-lists `id` on `h1`-`h6` and `p` only, and `quill-field`
  registers a BLOCK-scope Parchment attributor so the editor keeps it.
  `clean-html-fragment` is the same sanitizer minus `id`, and six callers take
  it — content cut out of a document must not carry its parent's anchors. A test
  asserts the two differ in nothing but `id`.
- `freememo.client-boot` owns both boot continuations for all four entrypoints.
  `e/boot-client` returns a two-arity fn, so wrapping its thunk and calling it
  with no arguments passed `undefined` for both callbacks and discarded every
  real error message.
- `freememo.selection-placement` and `freememo.turn-timing` are `.cljc` because
  `test/` is Clojure only. A function inside a `.cljs` file cannot be unit tested
  at all.
- `freememo.link-target` classifies what a click can offer, and
  `editor-links.cljs` owns the DOM half. Citation plumbing gets no affordance:
  it is 93.6% of the corpus's 149,457 fragment links and points into reference
  lists no importer carried in.
- The `:small` upload cap moves from 10 MiB to 50 MiB. Eleven of twelve measured
  pages encode under 10 MiB, but one single-page specification needs 12.87 MiB on
  text alone, and the worst page reaches about 37.5 MiB with its images inlined.
- Both day-scoped review queries use a half-open range rather than
  `reviewed_at::date = ?::date`. The cast is not sargable, so the index served
  only the `user_id` half.
- Capture de-duplication indexes `md5(content_text)`, never the text: a btree
  row cannot exceed about 2704 bytes.
- `playwright/probe-wiki-tables.mjs` loads stored HTML into a real Quill and
  asserts the table count, row-id distinctness and convergence. Every table
  defect happened during Quill's Delta conversion, so the Clojure suite could not
  have caught any of them. `probe-format-menu-placement.mjs` samples
  `elementFromPoint` across the selection's own client rects.
- `extensions/` holds both companion clients. The Zotero plugin moved from the
  repository root, and the install URL, its download filename and two comments
  moved with it.
- `flashcards` gains `local_current_digest`, `anki_header_text` and
  `anki_bibliography_html`. The first is the one staleness predicate
  (`db/unsynced-sql` and `card-components/unsynced?` are its two restatements);
  the other two are a snapshot of the push settings, so a settings edit can move
  a digest.
- `anki-fields/strip-push-decorations` splits the decoration half out of
  `strip-html`. The reconcile digests and the pull comparison use it; the
  cross-side overlay and display still use `strip-html`.
- Pulled HTML now passes `clean-html` (`anki-sync-server/sanitized-pull-update`).
  Pull was the only unsanitized HTML path into `flashcards`.
- `flashcards.baseline_digest_version` records which generation of the digest
  functions wrote a card's baselines. `anki-reconcile/classify` reads a mismatch
  as `:needs-baseline`, so a future change to `local-parts` or `anki-parts` needs
  only a bump of `anki-digest/digest-version` to re-baseline every account on its
  next run. Version 2 shipped without this and reported 1459 of 1528 cards on one
  account as conflicts; the recovery was a manual baseline reset.
- `plans/anki-html-change-detection.md` holds the spec.
- No new dependencies.
- `CLAUDE.md` gains "A Missionary Canceller Cannot Stop an LLM Call",
  "Long-running work owes the Activity monitor a decision", "The Interop Call Is
  What Transfers", "The Client Entrypoint Owes Both Continuations" and the Quill
  table-merge section. Each one records a defect that shipped.

## v20260814-1e4cf45

### For users

- **Anki sync is now one run that decides the direction per card.** Push and
  Pull are gone. The toolbar action reads `Sync with Anki` and the quick action
  reads `Sync with Anki now`.
  - **Each side keeps its own record of what it held at the last sync.** FreeMemo
    can now tell which side moved. A push used to overwrite an edit you made in
    Anki, and you had to remember which side you had edited and press the two
    buttons in the right order.
  - **One run covers every card you ever synced**, not only the document you have
    open. A card you deleted in a document nobody has open still owes its Anki
    note a deletion.
  - **Per card the run pushes, pulls, creates, deletes on either side, or reports
    a conflict.** A card edited on both sides applies nothing and waits for you.
  - **Conflicts are settled in Library → Cards.** The origin filter gains
    `conflict`, and `Keep FreeMemo` / `Keep Anki` resolve the selected rows.
  - **A delete made while Anki was closed is retried on the next run.** Ten such
    orphan notes were counted on production.
  - **Score and overlapping cards take part in change detection for the first
    time.**
  - **A run stops itself when Anki reports most of your notes missing.** The old
    overlay deleted every local card it could not find, which wiped cards when
    you opened the wrong Anki profile.
  - **The toast counts what the run wrote, not what it decided to write.** A
    card counts as sent only after FreeMemo reads the note back from Anki, so a
    run cannot claim a card Anki refused.
  - **A run that fails now says why.** Per-card errors from Anki reach the
    toast, and a run that cannot assemble its work ends with the reason instead
    of a clean-looking report over nothing.
  - **A sync with new cards and no deck set refuses before it writes**, and
    names the document. Choose a deck in `Sync with Anki…` and run it again.
- Bug fix: **a push no longer clears your Anki tags.** AnkiConnect replaces a
  note's whole tag list whenever a request carries tags, so every push spent the
  Marked status of every card it touched, along with `leech` and every tag you
  added by hand. With the Tags checkbox off it cleared them all. Tags now merge.
  Measured on a live collection: 869 of 1335 FreeMemo Basic notes carried no tag
  at all.
- **A new Schedule tab plans a PDF to a deadline.** Pick a document and a page
  range, state a target date, and get a dated table with pages to read and cards
  to review, one row per day.
  - **The tab is a plan, not a queue.** It writes nothing to Anki and nothing to
    your learn queue. You follow it by hand, the way you would follow a training
    programme.
  - **A page counts as known when its Anki cards clear your deck's retention
    target and reach a projected interval of at least 21 days.** The browser
    reads your review history from Anki and the server replays it through FSRS
    with your own weights. Every Anki action is read-only.
  - **Today's row is a quota that counts down.** Mark pages done and it falls to
    `done`, then to `N ahead`. It used to divide the whole remainder by the days
    left, so marking three pages moved it from 3 to 2.
  - **The plan leaves the cards time to mature.** Reading is spread over the days
    up to the coverage deadline rather than up to the target, so the days a card
    needs to settle are visible instead of implied. The days past that deadline
    carry their review load and an em dash.
  - **A `Covered` tile counts the pages you marked done.** A page you had read
    and carded used to appear in no figure on the tab, so half a document read as
    `Known now 0 / 23`.
  - **The home view is one card per schedule** — pace, covered pages, pages due
    today, and that schedule's own Anki status. It was a line of placeholder
    text.
  - **An `Edit` button revises a schedule**, and the infeasibility panel's target
    date carries an `Apply` control that writes it.
  - **The reading view shows a second badge with pace and today's quota**, such
    as `today 3 pages · 14d`. The full verdict is in the tooltip.
  - **`Go to Schedule` is in the command palette.** The tab shipped without one.
  - Bug fix: **marking a page done now moves every schedule surface at once.**
    The badge, the Today tile, plan row 0 and the home view all read stale until
    a reload.
  - Bug fix: **a document whose cards you gathered into a filtered deck no longer
    reports `FSRS is off in Anki`.** FreeMemo now finds the deck preset that
    governs the cards, and reads the review history per card rather than per
    deck. Eleven cards sitting outside the named deck used to contribute no
    reviews at all.
  - Bug fix: **applying the suggested target date now clears the warning that
    offered it.** The warning measured against the whole page range while the
    date search measured against the pages a projection can reach, so no date
    could ever close it.
  - Bug fix: **pages with no cards no longer read as an impossible target.** The
    panel used to say the target was out of reach and, in the same box, that the
    plan below schedules the reading that reaches those pages. Both cannot hold.
- **Create a copy of a card instead of retyping it.** Text cards get
  `Create copy…` in the edit modal, between History and Cancel. It commits the
  card you are editing, closes, and opens the add modal filled from what you had
  on screen. Occlusion and score editors get `Duplicate` instead, because those
  write the copy on click.
  - The copy modal refuses to save while the copy still equals its source.
  - Bug fix: **a save where every card was already present used to report
    `Card added` and add nothing.** Generate and Add reached that path too.
- **The image occlusion editor can add text labels.** An image with no labels
  often cannot be occluded usefully, because what you want to recall is not in
  the pixels. A label is drawn under the masks, so a mask over it hides it on the
  question and the answer reveals it. This needs no Anki model or template
  change, so notes you already pushed are untouched.
  - Bug fix: **a click with the Text tool now creates a label.** The tool shipped
    unusable. The label and its editor were gone before a keystroke could reach
    them, and the letters you typed switched tools instead.
  - **The editor gains a `Delete` button.** Deleting a mask was on the Delete key
    alone.
- **Both card tables show one row per authoring group.** A 7-mask image with no
  grouping filled 7 rows. Deleting the row deletes every card it stands for, and
  that delete is now undoable. Counts state rows and cards separately.
  - Clicking an occlusion row used to open the editor with no way to tell which
    mask the row stood for. That edit path never worked.
- **Occlusion notes get a Links field**, at the same place as the other four
  FreeMemo note types. An occlusion card made from a PDF page carried no
  citation, no page locator and no link back into FreeMemo. FreeMemo also puts
  the fields of all five of its note types into a fixed order, so a note type you
  already had gets the field in the same place a new one does.
- **A card made from a PDF page carries its page number into Anki again**, on the
  citation line: `Compilers: principles, techniques, & tools — (2007) — p. 5`.
  The page number was lost when Source and Bibliography became one Links field.
- **The keyboard chords lost a modifier.** Marking a page done is a constant
  action in a reading session and cost three keys. Mark done is `⌘D`, Extract is
  `⌘E`, Scan page is `⌘S`, Generate cards is `⌘G`, and Undo is `⌘Z`. Add card
  (`⌘⇧A`), Sync with Anki (`⌘⇧X`), Sync now (`⌘⇧⌥X`) and the command palette
  (`⌘K`) are unchanged. On Windows and Linux read `Ctrl` for `⌘`.
  A chord now takes the browser's key only on the screen where the action
  belongs. `⌘Z` yields to any text field or editor you are typing in. The
  Settings shortcuts table is read from the registry, so it can no longer go
  stale — it listed six of the nine bound commands.
- **Two chords insert a cloze in the card editor.** `⌘⇧C` inserts the next cloze
  number and `⌘⇧⌥C` reuses the current highest. The `{+}` and `{=}` buttons
  appear only on a selection, so a collapsed caret had no way to make a cloze.
- **Knowledge marks facts that restate a fact you already have.** Re-distilling a
  document produces reworded copies, and the database indexes cannot see them.
  Measured on production, those indexes caught 4 of 49 duplicate pairs by hand
  count. The detector runs after each distillation, and on demand over one
  document or all of them.
  - **A marked fact takes a new `duplicate` status, not `rejected`.** Your
    judgement and a detector's guess stay tellable apart. Nothing is deleted, and
    a mark is reversible per fact and through undo.
  - **The Facts view gains a status filter**, which is the only place a withdrawn
    fact is listed. Rejected facts had been unreachable the same way.
  - **Settings → Duplicate detection holds the three thresholds and an on/off
    switch.**
- **A large EPUB imports.** The upload takes the same chunked route video and
  archives already use, the parser reads the book without holding it in memory,
  and the zip-bomb guard measures a ratio rather than a fixed size. A 596 MB book
  imports and deletes cleanly.
- **A Sphinx documentation build imports as a topic tree.** Drop the zip in, and
  the directory layout becomes the topic tree. Images become media and internal
  links navigate inside FreeMemo. Verified against the full Blender 5.2 manual:
  2356 pages, 13 559 links rewritten, 3138 images stored.
- Bug fix: **an imported documentation page no longer flattens into a wall of
  paragraphs.** Quill holds no element for a definition list, a horizontal rule,
  a figure or a keyboard key, and it dropped each one on load. Worse, the editor
  wrote the flattened page back, so one keystroke destroyed the structure for
  good. 1787 web pages and 968 SuperMemo pages were exposed.
- Bug fix: **the Learn page no longer says `Nothing due — you're all caught up.`
  when topics are waiting.** A single document could abort the queue query for a
  whole account. Three users hit it, and one of them lost all 2512 due topics.
  The page now shows the error instead of an empty queue.
- Bug fix: **the navigation bar is reachable on a 320px phone.** The ⋮ trigger
  laid out past the right edge of the strip, where it was invisible but still
  took every click aimed at Settings. The seven tabs behind that menu had no way
  in, and nothing recovered from it.
- Bug fix: **an upload under a stale sign-in now answers `401` and signs you
  out.** It failed with `500 Upload could not be started`, which sent you to
  inspect a file that was never at fault.
- Bug fix: **math scanned from a page renders in the editor.** A model that
  answers with `$…$` or `\[…\]` produced text that rendered on cards and sat as
  literal characters in the editor — one document reading two ways.
- **The Library and Knowledge tables are faster and steadier.** Both cap at ten
  rows of scroll container, so 15 rows mount whatever your window height instead
  of about 44. All four Knowledge views now share the Cards table's scroll, its
  empty state and its cell styling. The Facts view loaded slowly and jittered
  when the pointer crossed a row.

### Known issues

- **FreeMemo can no longer remove a tag from Anki.** A tag you clear in the sync
  form stays on the notes that already carry it. Additions propagate, removals do
  not. That asymmetry is the price of not deleting your own tags.
- **A score or overlapping card cannot receive a pull.** FreeMemo has no way to
  read those kinds back from Anki, so a run reports them as conflicts rather than
  dropping them without saying so.
- **An Anki push of a copied occlusion group has not been attempted.** The score
  `Duplicate` button also has no browser test, because no score document exists
  to drive one.
- **The Schedule tab needs Anki with FSRS on and an optimized deck preset.**
  Without it a schedule reports its status per row and shows no forecast. If your
  cards sit in a filtered deck, empty it first.
- **The feasibility warning measures against the daily review capacity you
  declared, not against your measured rate.** No read-only Anki action reports a
  collection's review rate.
- **Every page you marked done before this release reads as `not today`.** The
  first day after the deploy starts today's quota from a plain count rather than
  from `done`.
- **Pace and today's quota are measured against different lines and need not
  agree.** Pace runs from the coverage baseline to the coverage deadline. The
  quota divides today's allocation. The pace verdict now sits in the tooltip
  alone for that reason.
- **Every schedule that predates this release reads `On plan` on the day it
  deploys.** Coverage at creation time is unrecoverable, so each schedule takes a
  fresh baseline and whatever lag its old line had measured is forgiven.
- **Two of the four infeasibility levers are marked `(not editable yet)`.**
  Retention target and page exclusions are real ways out of an impossible plan,
  so they are named rather than hidden.
- **`p. N` in an Anki note is the physical PDF page index.** There is no page
  label or offset model, so a book with front matter disagrees with its own
  numbering. A document nested inside a PDF page also pairs its own citation with
  the host PDF's page.
- **An occlusion label never becomes a card**, and it does not wrap — only your
  own line breaks split a line. The label's vertical alignment on a real Anki
  card is unverified. If a label sits wrong on your card, contact me.
- **Roughly one duplicate-fact mark in ten is wrong.** The detector scored 0.90
  precision against 234 hand-labelled pairs. Every mark is reversible. The
  thresholds were fitted against four documents from two users and will need
  moving.
- **A first distillation marks nothing**, and facts from the same run are never
  compared with each other. The detector compares new facts against facts the
  document already had.
- **Documentation you imported before this release stays flat.** A backfill
  cannot restore tags the sanitizer already deleted, so those trees need a
  re-import. The first edit of a freshly imported page also rewrites the stored
  row once, as Quill normalizes it. That is a one-time change and stable
  afterwards.
- **A Sphinx import drops in-page anchors.** A link to a heading inside a page
  lands on the page. Nested definition lists lose one indent step.
- **A book with several thousand images can still exhaust memory on import.** The
  bound is the image count, not the file size. A 596 MB book imports inside a 1 GB
  heap.
- **The EPUB import has not been driven end to end through the browser.** The
  server side is verified against PostgreSQL, including the upload, the finalize
  and the delete.
- **The first server start after this release may take longer.** One table gains
  a generated column, and adding it rewrites the table. That cost is unmeasured
  against a production-sized table.
- **A deleted account still reaches an empty app on any page that reads no
  quota.** The sign-out happens on the next upload.
- **`⌘D` still bookmarks the page outside a document.** The chord belongs to the
  reading view, so every other screen leaves the key to the browser.

### Technical

- **Six namespaces carry Anki reconcile.** `freememo.anki-reconcile` holds
  `classify`, which answers "did Anki change?" and "did FreeMemo change?" from
  four digests and nothing else. `anki-reconcile-client` is the browser half,
  `anki-reconcile-server` the server half, `anki-reconcile-panels` the UI, and
  `anki-digest` and `anki-fields` the shared vocabulary. `anki_pull_button.cljc`
  is deleted.
  - **The interface says Sync and the code says reconcile, on purpose.** A sync
    is what the user asks for. A reconciliation is how the direction is decided.
    Every string a user reads carries the first word. Namespaces, functions and
    docstrings keep the second.
  - **`get-bulk-push-bundles` is renamed `anki-write-bundles` and no longer
    filters on `anki_note_id`.** It was written for the bulk Push-updates
    button, which could only update, so an unpushed card was noise to it. To a
    sync those rows ARE the `:create` set. It also omitted `:deck` and
    `:allow-dupes`, both of which `addNote` needs, so every create would have
    gone to `deckName nil`. It now refuses up front when a bundle has creates
    and no deck, naming the document.
  - **`report-line` and `outcome-text` are separate functions.** The first says
    what a run DECIDED and only a dry run shows it. The second counts
    `finish-run!`'s steps, and `:writes` counts cards whose note was written and
    read back. Reporting the plan after a real run is how a run that created
    nothing came to say "46 created".
  - **`do-anki-push!`'s per-card errors are reported rather than collected and
    dropped**, and a failed bundle build ends the run instead of passing a nil
    `:bundles` onward into an empty write.
  - `flashcards` gains `anki_baseline_digest`, `local_baseline_digest`,
    `anki_baseline_mod` and `anki_conflict_at`. `anki_baseline_mod` is recorded
    and read by nothing, so a future `notesModTime` fast path is a data question
    rather than a migration.
  - New table `anki_note_tombstones`. Every path that hard-deletes a synced card
    writes one, and a run retries the deletion from it.
  - Measured on the production account beforehand: 58 of 1477 cards carried an
    Anki `mod` newer than their sync stamp, and none of them had a content
    difference. Tag writes move it.
  - `build-update-note` owns the `tags` key for both update actions. The same
    bare `:tags` expression inlined at two call sites is how the tag wipe reached
    both of them. Omitting the key looks safe and is not — AnkiConnect reads
    `note.get('tags', [])` and assigns the result.
- **Seven namespaces carry the Schedule tab**, plus three tables.
  `study_schedules` is user intent and is exported. `anki_card_states` and
  `anki_deck_configs` cache what Anki already knows and are excluded from account
  transfer.
  - Projection branches on all four ratings at every review, weighted by your
    measured rating distribution, and prunes below 1e-4 probability. A 365-day
    horizon over 879 cards runs in 1.2 s inside `e/Offload`.
  - The maturation lead is derived per schedule and memoized on the scheduler
    map. It is 11 days under the canonical deck config and 8 under the FSRS-6
    defaults, and it is not monotonic in the retention target — 15 days at 0.85
    against 8 at 0.90. The search is therefore a linear scan.
  - `topics.done_at` is what makes a daily quota possible. All three writers of
    `topics.status` own it. It is deliberately not backfilled, because NULL reads
    as "not today".
  - `study_schedules.start_covered` and `covered_baseline_at` put pace on the
    coverage axis. Copying `start_known` instead was measured and rejected — over
    six live rows it understated four baselines, two of them by 32 pages.
  - `:done` and `:restore` now declare `:schedule-mutations`. The three plan
    readers watched that channel and nothing bumped it, so Electric skipped the
    work with no dep change and no unmount.
  - `revlog!` reads through `getReviewsOfCards`, batched at 999: 137 cards in
    30 ms and 12 KB, against 365 ms for one deck-wide sweep. That removed
    `normalize-card-reviews`, `db/anki-state-high-water` and the incremental
    replay, so every replay starts fresh and the write no longer depends on
    order.
- **`freememo.doc-html` is the seam between imported markup and Quill.** It
  rewrites a definition term to a bold paragraph, a definition body to
  `ql-indent-1`, a `kbd` to inline `code` and a Sphinx admonition to a
  blockquote. It runs before `clean-html`, because the sanitizer strips the
  classes that identify the constructs.
  - `hr` needs a `BlockEmbed` blot, and that blot is a production-only hazard.
    `blotName` is in no Closure extern, so writing it with `set!` on a
    dot-property is a write `:advanced` believes nothing reads. Closure deleted
    the assignment, the bundle compiled with zero warnings, and every `<hr>`
    dropped in production. `aset` with a string key survives, and
    `clj -X:build bundle-check/verify!` now asserts it against the built
    artifact.
  - Both importers parsed with Jsoup pretty-print left on, so `clean-html` kept
    the re-indentation as text nodes. `.ql-editor` is `white-space: pre-wrap`, so
    one page measured 30180px tall against 9292px collapsed.
- **`freememo.sphinx-import` walks a build into a tree.** Ids are drawn from the
  sequence before anything is written, because a page's links point at topics not
  yet inserted. Nodes are ordered by tree depth, not by path segment count.
  Detection requires `.buildinfo`, `objects.inv` and `searchindex.js` together.
  - A stored `href` cannot navigate for two independent reasons: Chrome does not
    follow an anchor click inside a `contenteditable`, and router5 attaches its
    click handler per node inside `router/Link`. A listener on the Quill root is
    the only route.
- **EPUB uploads move to the chunked transport**, with chunk, abort and status
  shared through `freememo.upload-http` and five new `/api/epub` routes. Bytes
  land in a new `topic_epubs` table as a large object, so finalize hands the
  session's OID to the topic instead of copying. The parse reads through a
  commons-compress `ZipFile` with random access rather than forcing a
  `{path -> byte[]}` map.
  - `ceilings*` reported `upload_max_bytes` while the middleware enforced
    `request-max-bytes`. In production the first is unlimited and the second is
    100 MB, so the client check was off and the chip row showed no limit.
    `STORAGE_REQUEST_MAX_BYTES` is now in the compose allowlist.
- **`topics.content_preview` is a STORED generated column holding
  `left(content_text || '', 400)`.** The concatenation forces a full detoast, so
  PostgreSQL takes no byte slice and cannot end one inside a multi-byte
  character. `get-learning-queue` now returns `{:rows … :error …}` and never
  throws, because an exception there crosses `e/server` and closes the WebSocket.
- **`freememo.kg-dedup` holds the duplicate detector**, the best of 21 scored
  against 234 hand-labelled pairs at precision 0.90 and recall 0.78. Three
  branches, each catching what the others miss. Trigram similarity comes from
  `pg_trgm`, because the thresholds were fitted against it. `fuzzystrmatch` and
  `unaccent` are installed as groundwork for a planned embedding tier and feed
  nothing today. `kg_facts.status` and `undo_log.action_type` both widen their
  CHECK.
- **A repo-wide siting audit moved 33 blocking database reads into `e/Offload`**,
  across 10 namespaces. Each one ran on its session's reactive thread and stalled
  the whole graph for the length of the query. `loading/WithLatestServerLoading`
  is new and additive: it calls the thunk inside `e/server` and latches a
  readiness boolean, so a large collection stays on the server.
  - `content-card-table` read `:success` and `:error` in client scope from a map
    holding every card of the document. The Anki pull path sent every card with
    bibliography HTML and group geometry attached, then dropped the unpushed rows
    in the browser.
- **`freememo.tape-table` owns the header, the scroll container, the row window
  and the empty state for all four Knowledge views.** Cell layout moved from
  merged inline `:style` maps to role classes — a row cost about 46 reactive prop
  nodes against 4 for a Cards row. Both Library tables and the Knowledge tables
  cap the container at ten rows with an overquery factor of 1.5.
- **`freememo.card-rows` owns the collapsed-row model.** `:row-key` is the
  selection unit, `:member-ids` is the delete, push and pull unit, and `:members`
  never crosses the wire. Collapse runs before filtering, or a row would delete a
  subset of its own group.
- **`freememo.interaction-trail` records one `::interaction` signal** with
  `:kind` as the discriminator: `:command`, `:nav`, `:pdf-page` and `:search`.
  `command-bus/dispatch!` is instrumented rather than its 18 call sites, and a
  command carries `:source #{:button :keyboard :palette}`, which is what makes
  chord adoption measurable. Card bodies and search text stay out of the log
  deliberately — ids, counts and enums answer the question without them.
- **`applies-here?` is split out of `available?` in the command bus.** The first
  asks whether a key is the app's on this screen, the second adds whether
  anything is mounted to serve it. Suppressing on the second hands `⌘S` to
  Chrome's Save Page sheet on every non-PDF topic, because `:scan`'s only
  publisher sits behind `(when is-pdf? …)`.
  - Closure defaults `alwaysPreventDefault_` to true and calls `preventDefault`
    before it dispatches the shortcut event, so every registered chord ate its
    browser default on every tab. Only a literal `false` return suppresses.
  - `commands/display-chord` gains a 1-arity that reads the bind from the
    registry. Seven labels built their chord text by hand and four went stale the
    moment these chords shortened.
  - `command-bus` keys published invokers by publisher. `:copy-card` has three
    publishers and two of them can mount together.
- **Every Quill instance installs
  `quill-field/install-resize-release-on-focus-out!`.** `quill-resize-module`
  owns `document` keydown in the capture phase while it holds a selected image,
  and calls `quill.setSelection` from it. A right-click on an image arms it, so
  any text surface on the page loses its first keystroke and cannot defend
  itself.
- **Signal ids: `::interaction` is new.** `::client-error` now carries a real
  `:session-id` — the forwarder read the var inside `e/server`, which is a
  server-side read of a browser-only value, so every entry ever recorded reported
  nil. `session-id` moved to `freememo.client-state`.
- **`freememo.markdown` holds the OCR math conversions**, and `math/display->inline`
  rewrites a `\[…\]` region to `\(…\)`. Normalization happens where both OCR
  lanes pass, not in the prompt, because the plugin lane never receives one.
- **Deps and images:** `org.postgresql/postgresql` `42.7.3` → `42.7.13`, and the
  postgres image pinned from `16-alpine` to `16.14-alpine`. 16.14 carries 11 CVE
  fixes. `deploy.sh` runs `up -d postgres` every deploy, and an unchanged tag
  pulls nothing, so the minor only moves when that line moves. The driver audit
  is recorded in `plans/postgres-16-14-and-jdbc-42-7-13-bump.md` for the next
  bump to reuse. `ontop` carries its own copy of the driver jar, bumped to match.
- **The build stage sets `JAVA_TOOL_OPTIONS="-Xmx3g"`.** Nothing set a heap
  before, so the JVM took a quarter of whatever the host had. Three deploys died
  with `OutOfMemoryError` inside `uberjar` on the 7.9 GB box while the same
  commits built fine on a 16 GB machine. Raising this without raising the box's
  headroom risks the OOM killer choosing the live container.
- **`deploy.sh` tees its whole output to `logs/deploy/<utc>-<commit>.log`** and
  points `logs/deploy/latest` at it. A failed deploy used to leave nothing on the
  box to read. Capture opens before the lock, so a rejected concurrent run is
  recorded too. `test-deploy-transcript.sh` drives the real script against a stub
  docker in a temporary git repo, 20 checks, no network.
- **Coverage counts moved:** Routes 12 → 13, Nav destinations 9 → 10, Registry
  commands 82 → 86, HTTP routes 36 → 41. `coverage-counts.spec.mjs` derives each
  from `src/` and fails until `docs/post-deploy-checklist.md` matches.
- **`freememo.scroll` gains a container cap and `compute-overquery` accepts a
  fractional factor.** Mounting fewer rows than the container shows paints a
  blank band, so the cap is on `:max-height`, never on the window.

## v20260806-5467b1f

### For users

- **Boox NeoReader highlights now sync into the book you read them in.** A Boox
  e-reader writes its reading notes to Google Drive as one HTML file per book.
  FreeMemo reads that file and paints the highlights into the matching EPUB or
  PDF document.
  - **Start it from Library → Documents → the document's ⋯ menu → Sync with
    NeoReader.** The row action appears on EPUB and PDF documents only, and on
    the document itself rather than on a chapter or a page.
  - **The first run asks Google for read access to one file, then opens Google's
    own file picker.** Go to `onyx` → your device → `Read` and pick the book's
    HTML file. FreeMemo remembers it, so every later sync is one click with no
    picker. FreeMemo reads that one file and nothing else. It writes nothing back
    to Drive and nothing back to the device.
  - **Each highlighted passage is tinted orange in its chapter or page.** A
    passage you annotated also carries a `[1]` marker, and that chapter or page
    ends with a numbered `USER NOTES` list.
  - **A toast reports the counts after every sync** — how many highlights were
    marked, how many needed a text search to place, how many matched in part
    only, and how many did not place at all.
  - **A highlight FreeMemo cannot find in the text is listed under `UNPLACED
    HIGHLIGHTS`.** For an EPUB that list sits on the document. For a PDF it sits
    on page 1. Editing a sentence after you highlighted it is the usual cause on
    either kind.
  - **A PDF needs its text first.** A page holds no text until you run Scan Page
    or Copy text on it, and a page with no text can hold no highlight. Copy text
    matches your export most closely, because both read the PDF's own text layer.
  - **Every sync rebuilds from the export.** A highlight you deleted on the
    device loses its mark, and your own edits inside a `USER NOTES` list do not
    survive.
  - Help → **Boox NeoReader highlights** holds the same seven steps.
- **Card generation can now take your prompt literally.** Generate with Prompt
  sent your text to the model under "Use the following pattern for your
  questions", so the model read it as a pattern rather than as a question. The
  creative reading you saw was an explicit instruction, and no setting could ask
  for the literal reading instead.
  - **Three positions: Guide, Clarify and Verbatim.** Guide is the previous
    behavior and stays the default. Clarify makes your prompt the question,
    rephrased only for clarity. Verbatim makes your prompt the question, word for
    word.
  - **The default lives in Settings → AI features → Pre-prompt Strictness.** The
    Generate with Prompt dialog carries the same control, and its choice governs
    that one generation without changing your default.
  - **Clarify and Verbatim generate a single card**, and the dialog says so
    before you submit. Your saved card count still governs plain Generate. Cloze
    carries no question field, so both strict modes bound it to the prompt's
    topic instead.
  - Bug fix: the Generate with Prompt dialog titled itself "Generate Cloze Cards"
    for overlapping cards.
- Bug fix: **every call that selected DeepSeek V4 Flash failed.** The model id
  sent to the provider named an alias that does not exist, and the provider
  rejected it with HTTP 400. One model registry serves card generation, all six
  knowledge-graph steps, the assistant, the card-compare modal and answer
  grading, so the failure reached all of them. The id now names a dated snapshot.
  Your saved model selection keeps resolving, and the price is unchanged.
- Bug fix: **an AI failure now tells you what the provider said.** Every failing
  call reported its status code and nothing else, so the invalid model id above
  reached you as a bare "OpenRouter HTTP 400".
- **The ads are gone.** They ran for about 35 hours. The ad network closed the
  publisher account, so the whole path came out of the code — the loader, the
  unit on Home, the seller file, and the per-request check that decided whether
  to withhold an ad from a paying customer. No page requests anything from an ad
  vendor now.
- Bug fix: **the Settings export card named a card that no longer exists.** It
  told you to drop the archive on the Upload card. It now says to drop the `.zip`
  anywhere on the Import page, which is what the page has taken since the last
  release.
- Bug fix: **the nav works on a phone.** At a 320px window the ⋮ menu could not
  be opened, and seven of the twelve tabs were behind it. The bar needed 417px
  and had 320, so the ⋮ was pushed out of sight while its click target stayed on
  top of the Settings button. Below 600px the wordmark is now the `FM` mark and
  Actions moves into the ⋮ menu, which frees 120px, and the ⋮ itself no longer
  sits in the part of the bar that gets cut off. The bar now needs 297px.
  Nothing changes above 600px.

### Known issues

- **A NeoReader sync rewrites the content of every chapter or page it touches.**
  The editor in view re-initializes, and the synced quotes enter global search.
- **Your own typing inside a `USER NOTES` list is discarded.** The next sync
  rebuilds that section in full.
- **One export file binds to one document.** A second device's highlights on the
  same book stay absent.
- **Placement follows a reading-order cursor, and that cursor is a chain.** One
  wrong placement can push every later highlight off.
- **A book with 200 highlights has no overview.** Nothing lists the marks and
  nothing is clickable, so the only way to reach a note is to read past it.
- **A sync strips an orange tint you applied by hand**, because a mark carries no
  identity of its own. It also deletes a highlight whose whole text is a
  bracketed number such as `[3]`, because that is how it finds its own markers. A
  `[3]` the book carries in plain text is untouched.
- **A cancelled picker and a blocked Google request reach the browser console
  only.** Both look like nothing happening.
- **On a PDF, a page whose text came from Scan Page may not match your export.**
  Scan Page re-transcribes the page image. Copy text reads the PDF's own text
  layer, which is the source the device reads.
- **On a PDF, a highlight that crosses a page break marks only its leading
  part**, and the toast counts it as matched in part.
- **The `UNPLACED HIGHLIGHTS` list sits on page 1 of a PDF, and a PDF opens on
  the page you left it at.** After your first visit the list is one navigation
  away. A later scan of page 1 overwrites the list, and the next sync writes it
  again, so the state repairs itself. Cards you generate from page 1 in between
  would carry the list's text.
- **An account export carries the Drive file binding, and it is useless on the
  target instance.** That instance holds no grant for the file, so the first sync
  there asks you to pick it again.
- **The PDF sync is not verified against a real Boox PDF export.** The EPUB path
  is verified end to end. If a PDF sync misplaces your highlights, contact me.
- **Pre-prompt strictness governs the question wording and the card count only.**
  It makes no statement about where the answer comes from.

### Technical

- **Six namespaces carry the NeoReader sync.** `freememo.neoreader-export`
  decodes the Boox HTML. `freememo.quote-anchor` places each quote and writes the
  marks. `freememo.neoreader-sync` runs one sync. `freememo.google-drive` holds
  the Drive authorization and the download. `freememo.neoreader-picker` is the
  browser half. `freememo.neoreader-kinds` holds the accepted kind set, which the
  Library row action and both HTTP routes read, so a button cannot appear over a
  route that 404s.
  - **The export carries no anchor** — no CFI, no spine index, no character
    offset, only a chapter heading and the quoted text. The text is also damaged:
    lost spaces at emphasis boundaries, spliced paragraphs, curly quotes. Every
    quote is found again by search against a normalized form, and a reading-order
    cursor breaks ties.
  - **`document-sections` is filtered to children carrying a `page_number`.** An
    extract made from a root's editor was in the match set before that, so a copy
    of a passage was a second place a highlight could land. A kind filter cannot
    separate the two — an EPUB chapter and an extract are both kind `basic`.
  - **`strip-sync-content` erases everything an earlier sync wrote, and that
    erasure is load-bearing.** The unplaced list holds quotes truncated at 300
    characters, which is exactly the shape the matcher looks for. Without the
    strip, a highlight that failed to place would find its own listing on the next
    sync and report itself placed.
  - **`neoreader_highlights` is table 24**, with `quote_hash` as a generated
    `md5` column. The unique index reads the digest, never the text, for the
    B-tree row-size reason the flashcard indexes record.
  - **Drive authorization is a second OAuth round trip on the same redirect URI
    as sign-in.** The flow marker travels in `state`, the only channel Google
    returns verbatim. Sign-in still asks for `openid email profile` alone, so no
    user grants Drive access at signup. The scope is `drive.file`, which is
    non-sensitive and needs no verification review.
  - **`setAppId` is required under `drive.file`, and it takes the Cloud project
    number rather than the client id.** Without it a pick creates no per-file
    grant, and every later `files.get` answers 404 naming the file. That defect
    shipped once and cost a production debugging round.
  - The Picker is the second third-party script this app loads. It carries no
    `<script>` tag in either shell and loads only when a user clicks Sync.
  - New command registry entry `:neoreader-sync`, invalidating `:refresh` and
    `:settings-refresh`.
- **`entity-scopes` moved from `freememo.account-export` into
  `freememo.account-transfer-format`.** The new table reached `entity-tables`
  with no scope, and a missing scope makes `scope-of` throw inside the reactive
  transfer that renders the Settings export card. Every Settings page load then
  closed the WebSocket with code 1011, and `/api/account/export` returned 500.
  The map now sits in a namespace that needs no database, so two new guards run
  with PostgreSQL down: every exported table has a scope, and each scope's
  declared placeholder count matches the `?` count in its `WHERE`.
- **`openrouter/api-error` is the single owner of the error mapping.** clj-http
  parses a JSON body only for unexceptional statuses, so an error body arrived as
  a String and `(get-in "…" [:error :message])` returned nil. The OCR lane held a
  byte-identical copy of that expression. `api-error` decodes the body, falls
  back to the raw text of a non-JSON page truncated at 500 characters, and
  carries `:status` in ex-data. `post!` logs the provider text on a failing
  status, and the OCR error copy dispatches on `:status` instead of matching the
  message text.
- **The four pre-prompt wrappers moved into `resources/prompts/`.** The guide
  wrapper holds the lines `build-prompt` inlined before, so its output stays
  byte-identical. A missing wrapper file fails the generation rather than
  degrading to no wrapper at all. `freememo.pre-prompt-mode` owns the enum,
  `normalize` and `single-card?`. The server forces the card count from that one
  predicate and the dialog discloses "Generates 1 card" from the same one, so the
  promise and the count cannot drift. `freememo.segmented` holds the segmented
  control the Settings field and the dialog share.
- **The twenty Playwright probe scripts are replaced by a checklist suite.** None
  of the probes could run. Each signed in as a user that does not exist, opened
  two topics that are absent, and launched a system Chrome that is not installed.
  The suite now takes one test per row of `docs/post-deploy-checklist.md`, and
  `playwright/manifest.json` accounts for all 106 rows. A row is implemented or
  excluded with a reason, and the manifest test fails when a row falls out of
  both. Twelve rows are written: ten pass and two skip for stated reasons. Row
  0.7 found the 320px nav defect fixed above, and now also asserts that no tab
  overhangs the strip that clips it and that the ⋮ takes its own clicks.
  - **Every browser test hashes `/freememo/index.css` against the file on disk**
    to prove the server serves the worktree under test. The commit sha cannot be
    that witness, because a dev build reports the literal `dev`. The guard exists
    because a dev server was found serving a deleted worktree.
  - **Fixtures are generated, never committed**, and two runs produce identical
    bytes. `big.mp4` became `big.webm`, because the duration probe reads a browser
    `<video>` element and Playwright's chromium carries no H.264 decoder.
  - `coverage-counts.spec.mjs` asserts the checklist's count table against
    `src/`. Running the rows corrected four claims in the checklist.
  - The suite is not in CI, and this release does not wire it up.
- **The ad path removal deleted `freememo.deployment-slot`,
  `db/user-has-purchase-or-event?`, the seller-file route and two of the four
  entry points on the deployment seam.** The seller path moved into
  `shell/text-only-paths`, so it answers 404 `text/plain` rather than falling
  through to the shell catch-all as markup. The ad-network crawler left the
  crawler allowlist and the campaign crawler stays. The signup conversion tag,
  the placeholder it uses and the "only the seam names the implementation" guard
  are all unchanged. The gating test walked the powerset of four config keys and
  now covers one. A second contribution should restore that walk rather than add
  a row to a list.

## v20260805-eb9cb3c

### For users

- **The Import page now leads with one place to put anything.** It was nine cards
  of equal weight and no drop target. You could not drag a file onto it, could not
  paste a link into it, and had to read nine descriptions before choosing.
  - **Drop a file anywhere on the page, paste a link or text, or press Browse
    files.** All three reach the same router, and it picks the import for you.
  - **A row of chips names every format the page takes, each beside its own size
    limit.** The number that governs a type now sits on that type. One list of
    four ceilings read as a comparison, and 25 MB next to 8 GB invited the wrong
    conclusion.
  - **The Upload card is gone.** Uploading a document is what the intake area
    does, and a card two hundred pixels away that opened a file dialog competed
    with the control the page is built around.
  - **Three cards stay in front — Link, Audio and Video — and six moved behind
    "More sources".**
  - Bug fix: **an oversized audio file used to upload in full before the server
    refused it.** The page checked the document limit alone, so a 40 MB recording
    travelled the whole way and then met the 25 MB transcription limit. The check
    now reads the limit that the file's own type carries.
  - Bug fix: **the source cards, the score toolbar and the video toolbar painted
    transparent.** All three named a color that no stylesheet defined.
- **Export your data.** Settings → Account has an **Export your data** card. It
  names what the archive will carry, and it downloads a `.zip`.
  - **The archive holds the account, not a summary.** Documents and their files,
    cards and their review schedule, the knowledge graph, your review history and
    your settings all travel.
  - **To load it, open another FreeMemo instance and drop the `.zip` on its
    Import page.** The contents of an archive pick the import now, not the file
    name, so the same drop takes an account export, a SuperMemo collection or a
    code repository.
  - **An import adds to what is already there.** It replaces nothing. Every
    identifier is renumbered for the target instance, so the links between your
    documents, cards and images survive the move.
  - **A failure before the metadata lands keeps nothing** and gives back the
    storage the attempt reserved. The uploaded archive stays, so a retry needs no
    second upload. A failure while the files copy keeps what already landed, and
    the next attempt continues from there.
  - **The same export cannot import twice by accident.** A second attempt is
    refused and names the date of the first one.
- **Review priority takes decimals now, to three places.** `12.345` is a legal
  value. The column held whole numbers before, so a collection above a hundred
  topics could not state a stable order — topics shared a priority and the queue
  broke the tie by a daily hash. The steppers, the history modal and the
  SuperMemo import all carry the extra precision.
- **Right-to-left text reads correctly in the Quiz tab.** Questions, reference
  answers, your typed answer, grader feedback and the session history each pick a
  direction from the text itself. A card that opens with an English heading and
  continues in Arabic now gives every paragraph its own direction, instead of
  forcing one direction on the whole card.
- **The Quiz answer boxes match the rest of the app.** They named a class that no
  stylesheet defined, so the browser drew them in its own monospace default.
- Bug fix: **dark mode had four unreadable spots.** Inline `code` in either editor
  was near-white text on a near-white pill. A link took one color in one editor
  and the browser's own dark-scheme color in the other, and neither reached 4.5:1.
  Text colored from the format menu kept a color chosen against a white page, and
  seven of the nine presets fell under the contrast floor — dark mode now drops
  the hue and keeps the theme text color, while light mode keeps every authored
  color. The keyboard skip link drew blue on white.
- Bug fix: **the knowledge graph now follows the theme, and hovering a node no
  longer hides its label.** The graph paints on a canvas, which page styles never
  reach, so every color in it was a light-theme constant. Labels stayed black on
  the dark background. The pill under a hovered node was filled white, which put
  the label at about 1.1:1 — hovering a node hid the word you were reaching for.
  Labels, that pill, the dimmed nodes outside your focus and the edges are now all
  mixed from the page's own text and background colors, so both themes get
  readable values from one palette. A change of the operating system preference
  while the graph is open moves with it.
- **The Zotero plugin reads its updates from the current repository address.**

### Known issues

- **One unreadable file refuses a whole drop, and so does a mixed one.** A
  selection holding a format the page does not take imports nothing, and a
  selection mixing kinds asks you to drop them separately. Importing the readable
  part would hide the refusal.
- **An audio-only `.mp4` or `.webm` lands as a video topic.** Both containers hold
  either kind, and one drop target has no signal to tell them apart, so the
  extension decides and it decides video. It still transcribes. The Audio card no
  longer offers either extension.
- **Four things the Import page is specified to do are not built yet** — a list of
  recent imports, a link to the migration sources, the number keys that select a
  card, and remembering whether "More sources" was open. Escape does not close that
  disclosure either.
- **An account import always lands at the top of your tree.** A picker for the
  destination parent is not built. Move the imported root yourself afterwards.
- **An import tells you almost nothing about what it did.** The report is
  recorded and it is not shown. Rows the importer had to drop, files it could not
  store, and a storage limit reached while copying files all land in that unseen
  report. If an import looks short, contact me.
- **A repeat import of the same export is refused, with no way to ask for a
  second copy.** The server takes that instruction and the modal does not offer
  it.
- **A resumed import does not say that it resumed.**
- **An export download shows no progress.** The size is not known until the
  archive is written, so the browser can show neither a percentage nor a total. A
  large account takes a while to start.
- **An archive carries a format version, and there is no path between versions.**
  Version 1 is what this build writes and the only version it reads. An added
  column needs no change of that number, because an import drops a column the
  target has no place for and names it in the report.
- **Four lines in the Quiz keep left-to-right order.** "Your answer: ",
  "Reference: " and two others share one paragraph with the text they label, so
  the English label decides the direction. The two history tables also keep
  left-to-right columns, because about eleven screens share that table layout.
- **The dark-mode text-color fix covers the editors only.** The same stored
  colors also appear in card tables, quiz fields and the assistant transcript,
  which share no common container to hang the rule on.

### Technical

<!-- Deliberately not recorded anywhere in this file: the ad config key names, the
     vendor, the script URL and the reader that validates the configured values.
     CHANGELOG.md is public, and the committed file names no ad vendor anywhere.
     That detail lives in CLAUDE.md and in plans/, both excluded. -->

- **Crawlers now receive a static document, because they cannot run the app.**
  Google's renderer supports no WebSocket connection, and this app sends every
  word over one, so a crawler read zero words at every address. Measured on the
  deployed page: an empty body, and 46 readable words in total, all of them in one
  meta tag. `freememo.shell` now recognizes a crawler and serves a plain document
  built from the landing page's own copy.
  - **The words moved into `freememo.landing-copy`, a data namespace both
    renderers read.** One home per sentence. A crawler that reads different words
    than a visitor sees is what search engines call cloaking.
  - That namespace holds no requires on purpose. The document renderer is plain
    Clojure on the boot path, and a require of the Electric page would pull the
    Electric compiler into the server's boot.
  - The document carries no script, no stylesheet and no font. A crawler runs no
    JavaScript here, so those would cost it four round-trips for nothing.
  - Only the root address gets the document. Every other address answers 404 to a
    crawler, so identical text never appears on many addresses. A future public
    page needs an entry in the renderer, or crawlers will not see it.
  - Link previews keep working. The document carries the full Open Graph set,
    which is all a chat or social preview reads.
- **`/robots.txt` is a real file, `/ads.txt` is a real route, and three
  machine-read paths now answer 404.** All of them used to return the app page as
  markup, because the page handler answers every address that matched nothing
  else. A machine that asks for a text file and receives markup treats the file as
  missing. `/ads.txt` answers 404 unless the deployment configures a body, which
  is the honest answer for a deployment that authorizes no seller.
- **`freememo.import-types` is the single definition of the accepted extension
  set.** That set existed four times before — the document and audio `accept`
  strings, `video-import-modal/accepted-extensions`, and
  `archive-upload/archive-file?` — and nothing kept them in agreement. The chips,
  the drop router, each card's file dialog and the rejection message all read from
  it, so adding an extension is one edit. `:transport` names the protocol that
  carries the bytes, not the server's import flow: the server still resolves an
  archive's flow from its entry names.
- **The intake area owns no transport.** It classifies what arrived, decides which
  modal opens, and hands the file over. Every ceiling check and every upload
  protocol stays in the modal that already had it, because a second copy of the
  archive-versus-multipart split is a second place for it to drift. `ImportModal`
  and `VideoImportModal` each took one more positional atom for that handoff.
- **Which modal is open is one fact.** `!open-source` holds the source keyword,
  and a modal closes itself by writing `false` there, so `nil` and `false` both
  read as closed. Eight independent booleans made "is anything open" a question
  you had to ask eight times.
- **`plan-drop` and `plan-paste` are pure**, and the reactive layer only applies
  what they return. The page-level `dragover` and `paste` listeners install
  through `e/snapshot` and are torn down by `e/on-unmount`, so they die with the
  page instead of surviving into another tab.
- **`max-archive-bytes` moved from `archive-http` to `archive`** for the reason
  `video/upload-limits` records: the import UI needs the number, and a UI
  namespace requiring an HTTP-handler namespace breaks Electric's JVM reload.
- **Every canvas color in the graph is mixed from the container's own computed
  `color` and `background-color`,** so `graph_render.cljs` holds no second
  palette. `graph_page` paints `--color-bg-page` on the container for exactly that
  read — canvas chrome cannot inherit a background. An unparseable or transparent
  surface falls back to sigma's light-theme constants rather than to a color
  derived from the wrong surface.
  - **The edge color is applied in the reducer and cannot be a tracked setting.**
    sigma bakes `:defaultEdgeColor` into each edge's display data during
    indexation, and `handleSettingsUpdate` re-indexes nothing, so a `setSetting`
    on a theme change repaints the old color. Reducers run on every refresh.
  - **`:colours` is seeded before the renderer is constructed.** The Sigma
    constructor indexes and refreshes, so both reducers read the atom before the
    tracker has run, and a nil color would reach sigma's parser.
  - `defaultDrawNodeHover` is replaced, because sigma 3.0.2 hardcodes a `#FFF`
    fill and a `#000` shadow for that pill. The replacement keeps the geometry,
    fills with the page background, outlines instead of shadowing, and delegates
    the label to sigma's own drawer.
- **`src/hyperfiddle/history4.cljc` is deleted.** It shadowed the namespace the
  `com.hyperfiddle/hyperfiddle` dependency already provides, which `CLAUDE.md`
  says not to do.
- **`freememo.account-export`, `freememo.account-import` and
  `freememo.account-transfer-format`** — the archive is one `.zip`:
  `manifest.edn` first, then one newline-delimited JSON file per table under
  `entities/`, then the blobs. PostgreSQL owns the encoding at both ends. Export
  selects `to_jsonb(t)`, and import feeds the row back through
  `jsonb_populate_record(null::<table>, ?::jsonb)`, so the target's own column
  types read every value and no JVM type map sits between them.
- **The column list comes from `information_schema` on the target, never from the
  archive.** An archive key naming no real column is dropped and reported. That
  is both how an archive from a wider build imports at all, and why a hostile key
  cannot reach SQL.
- **Import runs in three phases, not two.** Media rows and bytes land first,
  outside the transaction, because the media id map must exist before phase 1
  rewrites any HTML. Every other metadata table commits in one transaction.
  `topic_files` and `topic_videos` follow, one commit each, because both blob
  columns are `NOT NULL` and a row cannot exist before its bytes. Phase 1 is
  atomic, so a resume repeats phase 2 only.
- **Every id in the archive is a source id, and it hides in five places.**
  Foreign keys, id columns with no foreign key, ids inside JSONB,
  `/api/media/<id>` URLs inside stored HTML, and topic ids embedded in `settings`
  KEYS. Only the first is visible to a schema walk. `CLAUDE.md` now records which
  four kinds of schema change need a decision in the transfer format, and which
  three of them nothing catches.
- **`account_imports` and `account_import_ids`** — one row per import run, and
  the source-id to new-id map for that run. That map is what makes the
  already-imported refusal, the resume and the blob-gap check one mechanism. Both
  are target-instance state and never leave in an export.
- **`content-type/classify-archive` gained a third flow.** It tests for an account
  export, then a SuperMemo collection, then falls back to a code repository,
  because that fallback claims every remaining `.zip`. An account export must
  hold both `manifest.edn` and an `entities/*.ndjson` sibling to match, so a
  repository carrying a file called `manifest.edn` cannot be hijacked.
- **`/api/account/export` sends no `Content-Length` and declares
  `application/zip`.** The size is unknown until the archive is written, so the
  response is chunked. The mime type is load-bearing rather than cosmetic —
  Jetty's gzip handler decides by type, and a generic type would put the archive
  through the compressor.
- **`archive_zip64_test` proves the reader survives a 4 GiB entry** before
  anything was built on it. A ZIP64 entry declares `0xFFFFFFFF` in its local
  header and puts the real sizes in a data descriptor after the compressed data.
  The test writes a sentinel entry AFTER the oversized one: a reader that loses
  its stream position never reaches the sentinel, which reading the big entry
  back would not have shown.
- **`topics.priority` and `topic_repetitions.priority_before` are `DOUBLE
  PRECISION`.** `NUMERIC` was the exact type, but a PostgreSQL `NUMERIC` crosses
  the Transit wire as a tagged `BigDecimal` rather than a JS number, so every
  read site would have needed a cast. The widening is guarded on
  `information_schema` — `ALTER COLUMN … TYPE` rewrites the whole table, and an
  unguarded statement would rewrite it on every boot.
- **`freememo.priority` is the write boundary**, and it closes two defects the
  widening would otherwise have opened. The no-op guard compared the column value
  against the incoming one with no coercion, so `(not= 50.0 50)` made every save
  of an unchanged whole number log a `priority-change` row and run a pointless
  `UPDATE`. Nothing clamped the value either, and PostgreSQL accepts `NaN` in a
  `float8` column and sorts it last, so one bad write would have buried a topic at
  the foot of the learning queue forever.
  `topics_priority_range_check` backs the boundary up for `create-topic!` and
  `insert-topic-rows!`, which never call it. Confirmed on PostgreSQL 16.13:
  `NaN` and `Infinity` both fail `<= 100`, so the check needs no `NaN` clause.
  All 7602 topics on the production-sized database already validate.
- **`round-to` is generic and shared by both peers.** `freememo.number-stepper`
  calls it for all three steppers, including the two integer ones, so the
  client's optimistic mirror cannot round differently from the server. The
  integer controls keep `parseInt`: `parseFloat` would turn a typed `1.5` into 2
  and `1e3` into 1000, a silent change this feature never asked for.
- **`sm_rank` stays.** Three decimals give 100001 distinct priorities, enough for
  any realistic collection, but rows imported before this change keep integer
  priorities and still need it to break ties.
- **`freememo.deployment-hook`** — a seam for behavior a particular deployment
  adds and the app itself does not need. Two entry points: one may annotate the
  session of a newly created account, and one may contribute markup to a single
  shell response. Both are no-ops here. The implementation namespace is optional
  and resolves dynamically, so a build without it emits an empty head, arms no
  session and renders no container. That is the default, and it is what a
  self-hosted instance runs.
- **No third-party markup ships in the HTML shells.** `index.prod.html` holds one
  `$:freememo/deployment-head$` placeholder and no vendor id of any kind.
  `index.dev.html` and `index.metadata.html` hold neither, so local dev reaches no
  third party. Before this, a hardcoded verification tag put one publisher id on
  every self-hosted page with no way to turn it off.
- **`freememo.deployment-slot` renders an empty container and asks the deployment
  to fill it.** The first spot is the band under the Home page's quick links. The
  gate is the existence of the activation function, not a server flag: the head
  contribution that defines that function is the same thing a flag would derive
  from, so the two cannot disagree.
  - Electric must own the container. A container appended by plain JavaScript
    into a parent whose children change makes Electric remove the wrong node and
    strand orphaned subtrees, with nothing in the console. Injecting into an
    Electric-created leaf is safe. A browser check confirmed both cases.
  - `CLAUDE.md` records the narrow rule, which replaces a broader claim that said
    no injection inside an Electric element was safe.
  - Bug fix: **the app shell lost its viewport height and collapsed.** A filler
    that sizes itself rewrote the inline height of every element above it,
    including the shell, and nothing put it back. One navigation later the whole
    app measured 185 pixels in a 609-pixel window. Fillers now take a fixed size,
    so they measure nothing and rewrite nothing.
- **Each ad contribution is gated by its own configuration key**, and none of
  them gates another: proof that a deployment owns the account, the loader that
  starts serving, and the map of named units are three separate decisions. A test
  walks all sixteen combinations, because a shared gate is invisible when two keys
  are always set together.
- **The per-viewer exemption is one statement, and it fails closed.** It asks
  whether the user ever completed a paid order or carries an operator flag, as one
  `UNION ALL` with a `LIMIT` above it, so PostgreSQL stops at the first hit. It
  reads per request and is never cached — a purchase lands in a different request
  and cannot reach a session cookie. An unreadable answer withholds the ad,
  because a paying user seeing an ad breaks a promise while a missed impression
  does not. Only a signed-in viewer costs a query, and only when serving is
  configured. The exemption reaches every ad-serving part and nothing else, and a
  test pins both halves of that.
- **`freememo.shell`** — `wrap-prod-index-page`, `template` and the manifest
  reader moved here out of `prod.cljc` and `prod_jetty9.cljc`, which carried
  byte-identical copies. Both entrypoints now mount one handler. Verified by
  booting both and diffing the served head region.
- **`config/env-or-config` is public.** Deployment values resolve env-first and
  fall back to `config.edn`. Callers outside `freememo.config` reuse that
  precedence instead of reimplementing it.
- **Consent Mode v2 is not implemented.** The EEA and the UK require a consent
  signal, and this deployment sends none.
- **Two verification items stay open for the priority change** — the `:prod`
  release build and a manual browser pass. `CLAUDE.md` records that "Method code
  too large" is a `:prod`-only failure, and this change grows the `NumberStepper`
  body that two namespaces expand. The build needs port 9009, which a running dev
  server holds.
- **New tests.** `priority_test`, `account_transfer_format_test` and
  `archive_zip64_test` run 25 tests and 158 assertions green together.
  `account_import_test`, `crawler_page_test` and the deployment-hook suite cover
  the rest. `every-table-has-a-disposition` is the one that will catch a new table
  with no export decision — and `:test` is still not in CI, so run it by hand.
- **`crawler_page_test` needs a prod client build in the tree.** Eight of its nine
  tests pass anywhere. The ninth asserts that a request with a browser
  user-agent gets the app rather than the crawler document, and with no
  `resources/public/freememo/js` present the shell answers 404 with
  `::missing-shadow-build-manifest` instead. Run `clj -X:build:prod build-client`
  before reading that one as a failure.

## v20260803-dad3fce

### For users

- **A SuperMemo collection of any size now imports.** A large collection used to fail
  on the way up, and no setting could carry it. A `.zip` or a `.7z` now travels in
  8 MB pieces, so the size of the file no longer decides whether it arrives.
  - **The modal shows the upload as it goes** — the percentage and the total size.
  - **A collection costs you no storage.** The archive is only transport, so the
    import gives every one of its bytes back. Only the images it stored stay counted
    against you.
  - **A code repository takes the same route.** A repo `.zip` above the 100 MB
    request limit imports now too.
  - Measured on a real 1.7 GB collection: 5 seconds to extract and 110 seconds to
    import, for 59 109 topics, 36 305 cards, 1 879 sources, 106 208 repetitions and
    2 365 images.
- Bug fix: **a large collection died at the last step, after the upload, the
  extraction and the read had all worked.** 
- Bug fix: **a card longer than about 2 700 bytes could not be saved, by any path.**
- Bug fix: **a failed import no longer bills you for it.** The importer stores images
  before it opens its transaction, because the blobs are too large to hold one open
  across, so a rollback could never reach them. 
- Bug fix: **math renders again, everywhere.** Last release shipped math and then
  removed the CDN, which deleted the small script the math loader waited on — the
  first known issue of that release. 
- **The Push to Anki buttons and the Sync menu carry Anki's own mark** instead of a
  generic refresh arrow, and that menu reads **Anki Sync** rather than **Sync**.
- **A link to freememo.net now shows a card.** A shared link carries a title, a
  description and a preview image, and a search engine sees a description rather than
  an empty page.

### Known issues

- **If the math renderer cannot load,** the editor still opens after a 3-second wait,
  but math stays as `\(…\)` source and clicking it does nothing. Your content is
  unchanged either way.
- **An interrupted archive upload starts over.** The server keeps the bytes it already
  holds and can report how many, but the browser always sends from the first byte.
  There is no cancel button while an upload runs either.
- **A failed import keeps the archive, so you can retry without uploading it again.**
  Its bytes stay counted against your storage until the retry works, or until the
  sweep collects the session six hours after the last piece arrived.
- **A collection's sound and video files extract to disk and are then discarded.** The
  importer reads text and images only. Dropping those files before extraction was
  specified and not built, so they still cost disk while an import runs.
- **Disk is the limit now, not memory.** During an import the archive exists twice —
  once inside the database and once as a local file — and the extracted tree sits
  beside them. A collection may extract to 16 GB by default. Lower
  `SUPERMEMO_MAX_EXTRACT_BYTES` if the container has less room.
- **The import report is still not shown to you**, as last release said. The importer
  counts everything it could not carry and the app then drops that count. If an import
  looks short, contact me.

### Technical

- **`upload_sessions`** — was `video_upload_sessions`, renamed in place at boot when
  the old table is present. One flow-discriminated table backs both flows, so the
  orphan-object sweep still names exactly one table. New `archive_path` column caches
  the materialized file. The large object stays authoritative, so a deploy that lands
  the import in another container re-materializes rather than fails.
- **`freememo.chunked-upload`** (CLJS) and **`freememo.upload-http`** hold the shared
  transport: `init`, `chunk`, `finalize`, `abort`, `status`. Chunk, abort and status
  are flow-blind, so the archive flow added no second way to append a chunk.
  **`freememo.archive-upload`** and **`freememo.archive-http`** hold the two halves
  that differ. `finalize` deliberately does not import — it resolves the flow and
  returns in under a second, and the import stays in the Electric service where it
  already ran inside `e/Offload`.
- **No threshold.** A 30 MB collection and a 7.5 GB one take the same path, because a
  threshold would keep the heap-bound path alive for small files.
  `STORAGE_REQUEST_MAX_BYTES` stays at 100 MB and still bounds one chunk.
- **`freememo.archive` reads a `File` or an `InputStream` next to `byte[]`**, and
  `copy-entry!` streams to the target file instead of buffering each entry in a
  `ByteArrayOutputStream`. Heap high-water is one copy buffer whatever the source,
  which is what allows the 2 GB per-entry cap. 7z entries stay in stream order, for
  the solid-LZMA2 reason recorded last release. `every-entry-name?` classifies from
  entry names and decompresses nothing.
- **`content-type/classify-archive`** is the single definition of archive routing.
  `classify-multipart` delegates to it and the chunked `finalize` calls it with a
  `File`, so a `.zip` cannot route to `:supermemo` in one and `:repo` in the other.
- **`db/param-bounded-batches`** partitions rows so each rendered statement stays
  under PostgreSQL's 65 535 bound parameters. It normalizes rows to the union of all
  keys first, or a key present in one batch and absent from another would be an
  explicit NULL in the first and a column DEFAULT in the second. All four batch
  inserts use it. `test/freememo/batch_insert_test.clj` guards it in 7 tests.
- **`idx_flashcards_unique_basic_digest` / `..._cloze_digest`** index `md5(question)`
  and `md5(cloze)`. A `left(question, N)` prefix would not do: two cards differing
  only past N would collide and `ON CONFLICT DO NOTHING` would drop one silently. The
  replacements are created before the old indexes drop, so a failed migration cannot
  leave the table with no uniqueness, and a failure logs at `:error` — without the
  indexes inserts still succeed and stop deduping, which would surface much later as
  duplicate cards.
- **`upsert-media-row!` reports `:media/inserted?`**, `store-images!` accumulates
  `:created-ids`, and the import's catch calls `db/delete-media-rows!` with only
  those. A row that merely matched an existing sha256 is content the user already
  owned. That trades the old reuse-on-retry shortcut for re-storing images on a retry,
  which costs nothing in practice because the archive is re-extracted every attempt.
  The compensation is guarded separately so its own failure cannot mask the real error.
- **New env vars** — `ARCHIVE_MAX_BYTES` (16 GB, a bound on what one bad request can
  reserve), `SUPERMEMO_MAX_EXTRACT_BYTES` (16 GB, the cap that binds against disk),
  `SUPERMEMO_MAX_EXTRACT_ENTRIES` (1 000 000), `SUPERMEMO_MAX_EXTRACT_ENTRY_BYTES`
  (2 GB). `web-import/supermemo-extract-limits` is the one place they are set.
  `test/freememo/archive_test.clj` adds 14 tests over both source types, the guards,
  and the routing agreement.
- **The KaTeX gate is a `defonce` atom holding a promise**, claimed on the first
  `on-katex-ready!` call rather than at namespace load — an eager `defonce` would
  fetch 275 KB on every route and start the timer before an editor existed. It races
  `vendor-libs/ensure! :katex` against `katex-wait-ms` and resolves `false` on
  rejection, so no caller needs a `.catch`. `render-math!` is uncapped, because a late
  typeset is harmless where a late editor is not, and it calls `after-render` on the
  failure path so `set-html!`'s once-only postcondition holds with the vendor path
  blocked. `math/prefetch-katex!` warms the load at the `/library/cards` boundary, the
  one editor-bearing route with no other trigger. `freememo.math` owns the group key,
  and its `render-math!` replaces the private copy in `assistant_panel`. See
  `plans/katex-gate-vendor-ensure.md`.
- **`freememo.brand-icons`** — third-party brand marks as inline SVG, kept out of
  `freememo.icons` because that namespace's contract is the Lucide stroke contract and
  89 call sites depend on it. Children must come from `hyperfiddle.electric-svg3`
  macros, since `dom/element` creates HTML-namespace nodes that render invisibly
  inside an `<svg>`. No mark defines an `id`: the Sync dropdown mounts four instances
  at once, and a `<defs>` id would make every `url(#…)` resolve to whichever mounted
  first.
- **Page metadata** — `description`, Open Graph and `twitter:card` in both HTML
  shells. The body is empty and every word renders client-side over the WebSocket, so
  those tags are the only text a crawler without JavaScript sees. `og:url` and
  `og:image` must be absolute, so a self-hoster has to change the `freememo.net`
  origin.
- **`plans/supermemo-import-large-archives.md`** records the eleven bounds between the
  browser and `import-collection!`, why A1 (large object) won over A2 (temp file), and
  the two requirements this work shipped unmet or unbuilt.

## v20260802-2770fe9

### For users

- **Import a SuperMemo collection.** Drop a SuperMemo 17, 18, or 19 collection into
  Import as a `.zip` or a `.7z`. It arrives as a topic tree with its cards,
  schedules, priorities, images, and bibliography.
  - **Your schedule comes with it** - interval, last review, next review, and
    A-factor transfer to each topic. Nothing restarts from zero.
  - **Priority keeps its order** - SuperMemo stores priority only as a queue
    position, so the import writes both a percentile and the exact rank. Two topics
    in one priority band still review in the order the collection had them.
  - **Items become cards** - an item marked with a cloze deletion becomes a cloze
    card. One whose deletion cannot be cut cleanly becomes a question-and-answer
    card instead, and the importer counts how many took that route.
  - **References become bibliography sources**, images move into your media storage,
    and the importer rewrites SuperMemo's own font, size, and extract formatting into
    shapes the editor keeps.
  - **The repetition log is archived, not replayed** - imported repetitions carry
    their own event type, so they cannot inflate your review streak or your review
    counts.
  - Library and Search gained a **SuperMemo** kind, with its own badge and filter.
- **Cards now enter the Quiz queue.** Every basic and cloze card joins the
  spaced-repetition queue beside knowledge-graph questions — one queue, one shared
  daily budget, filled round-robin between the two.
  - **A cloze card becomes one item per deletion**, masked the way Anki masks it:
    the asked deletion is blank, the others read normally.
  - **Two ways to grade a card**, set by **Use LLM for Generated Cards in Quiz** in
    AI settings, on by default. With it on, you type an answer and the model returns
    a verdict. With it off, you reveal the answer and rate it yourself — Again,
    Hard, Good, or Easy — with no model call and no cost.
  - **A card renders as a card** - your formatting survives, and the prompt is the
    card's own text rather than a plain-text copy of it.
  - The due tile counts card items. Review history shows each card row with its
    rating and with who graded it, the model or you.
  - Occlusion, score, and overlapping cards stay out of the queue. So does the Learn
    queue, which is unchanged.
- **No third-party CDN.** Every client library — PDF.js, Quill, Konva, KaTeX,
  wavesurfer, and the rest — now comes from FreeMemo's own origin.
  - **The app starts faster.** Blocking third-party JavaScript is now 108 KB
    compressed, down from 413 KB. PDF.js, Konva, KaTeX, the graph libraries, and the
    audio libraries load when you open something that needs them, not at boot.
  - **No request leaves for anyone else.** Open Sans is served locally too, so no
    visitor's address reaches Google Fonts.
  - **A self-host runs without internet access** once its image is built.
- **Three ways to ask the assistant.** A **Mode** row in the assistant panel picks
  who answers you, and each chat remembers its own choice.
  - **General** answers the question outright and names the page it read. If the
    material does not cover your question, it says so instead of guessing. It then
    points you at the nearest thing the document does cover.
  - **Tutor** guides you with questions, in plain conversational prose.
  - **Socratic** guides you with questions in the fixed *"Where you are" /
    "Consider next"* shape, unchanged from before.
  - The starter suggestions on an empty chat now match the mode you picked.
  - **The ✦ button on a reply works in every mode.** A Socratic reply still
    contributes only its *"Where you are"* half, never the question. A Tutor or
    General reply contributes the whole answer.
- **Searchable chat and model pickers.** Both dropdowns in the assistant panel now
  filter as you type. Find one chat among many, or any model in the list, without
  scrolling.
- **Math.** Formulas now have a whole pipeline — write them in the editor, generate
  cards from them, and push them to Anki as real notation instead of raw LaTeX. **One
  caveat before the list: dropping the CDN, later in this same release, broke the
  loader that every surface below waits on, so none of it typesets yet.** The first
  known issue has the detail.
  - **Write `\(x^2\)` and it becomes a formula** the moment you type the closing
    `\)`. No button, no dialog — the delimiters are the whole gesture, in the
    document editor and in every card field.
  - **Click a formula to edit it.** A small panel opens above it with the LaTeX, a
    live preview that updates as you type, and Delete. Enter or clicking away saves.
    Escape leaves it alone. Clearing the field removes the formula.
  - **Scanned pages keep their math.** OCR now transcribes equations as LaTeX, so a
    formula on a PDF page arrives as a formula, not as garbled text.
  - **Generated cards preserve the math in your source** and are told not to invent
    notation where the source used words.
  - **Anki gets `<anki-mathjax>`** on both push and CSV export, and notes edited in
    Anki come back with their math intact.
  - **Cloze deletions may hide a whole formula** — `{{c1::\(x^2\)}}` — but cannot cut
    into one.
  - Math also renders in card tables, the card-compare view, card history, search
    result snippets, and the OCR text previews.
- **Three new models** - GPT-5.6 Luna, Terra, and Sol, for card generation and for
  OCR.
- Bug fix: **clozing formatted text no longer destroys the formatting.** Wrapping a
  selection in `{{c1::…}}` used to read that selection back through Quill's
  plain-text API and re-insert it. That silently dropped bold, italic, code, links,
  and colour inside the range, removed any image, and reset the block format of every
  line after the first. Bolding a word and then clozing it lost the bold. The two
  markers now go in at the selection's boundaries and nothing else moves.
- Bug fix: **text colour, Align Right, and right-to-left survive a card save.** The
  sanitizer dropped all three, so they reverted every time you reopened the card.
  Nothing logged and nothing failed. Indent levels 3 to 8 and the serif and monospace
  fonts were lost the same way.
- Bug fix: **grouped masks in image occlusion did not work at all.** Group,
  rubber-band selection, and opening any card that already held a mask group were all
  dead, in development and in production alike. One mis-compiled constructor call took
  out the whole feature that shipped last release.
- Bug fix: **card edit history opened from a card was unusable.** Clicks passed
  through it to the edit modal underneath, its Close button did nothing, its version
  list would not scroll, and Escape closed both modals at once and discarded the edit
  in progress.
- The Help page now describes the assistant modes rather than a single Socratic tutor.

### Known issues

- **Math does not render yet, anywhere except the assistant chat.** Removing the CDN
  deleted the small bootstrap that `freememo.math` waits on, and that commit landed
  after the math work in this same release. Every editor therefore mounts without its
  formula module. A formula stays as literal `\(x^2\)` text in the document editor, in
  card fields, in card tables, in card history, in search snippets, and in the Quiz,
  and clicking one does nothing. **No content is lost** — those delimiters are the
  stored form, and everything you have written is intact — but nothing typesets until
  I ship the repair. It is specified and it is next.
- **A `\(`…`\)` pair inside a code block is treated as math when pushing to Anki.**
  It renders as a formula in Anki instead of as source. Nothing is lost — the next
  pull restores it — but Clojure's `\(` character literal is the realistic way to hit
  this. In the editor itself, code blocks are excluded: typing `\(x\)` in one leaves
  it as source.
- **Writing prose *about* LaTeX can produce an unwanted formula.** Typing `\(` and
  later `\)` in ordinary text converts on the closing delimiter. Ctrl+Z undoes it.
- **A long formula can be cut mid-expression** in a search snippet or a card-table
  row, where it then shows as LaTeX source rather than rendering.
- **The SuperMemo layouts were confirmed against one collection.** SuperMemo publishes
  no specification for its collection files, and no library reads them on Linux, so
  every scheduling field was recovered by measurement against a single real SM19
  collection. Each claim states the check that confirms it, and the checks pass on that
  collection. A collection from another SuperMemo version may still decode wrongly. If
  an import looks wrong, contact me and keep the archive.
- **Grade and lapse count do not import.** No byte in SuperMemo's repetition record
  has a grade-shaped distribution, so those two values stay out rather than being
  guessed. An imported repetition also carries no before-snapshot, because SuperMemo
  records when a repetition happened and not what it replaced.
- **The import report is not shown to you.** The importer counts everything it did not
  carry — unresolved images, undated repetitions, repetitions belonging to elements
  that became cards, truncated titles, items it could not read, cloze items that fell
  back to question-and-answer — and then the app opens the new tree and drops that
  count on the floor. Surfacing it is a small follow-up. Until then, if an import looks
  short, contact me.
- **Re-importing the same collection makes a second tree.** There is no merge, and no
  duplicate check.
- **Cards in the Quiz queue shipped without the full manual pass.** The unit tests, an
  integration suite against a real Postgres, and the production build are all green,
  but the browser matrix — both grading arms end to end, the setting toggle, the due
  tile, and the two history views — has not been walked against a live app. If a card
  review misbehaves, contact me.
- **A cloze card multiplies your queue** by its deletion count, and generated cloze
  cards carry at least three deletions. The shared daily budget is first come, first
  served: a morning sitting that spends all of it on cards leaves questions unseen that
  day.
- **Renumbering a cloze card moves a schedule with it.** Delete `c2` from a
  three-deletion card and `c3` becomes `c2`, so the old memory history attaches to
  different text. Anki behaves the same way.
- **A card can carry two schedules.** A card you pushed to Anki keeps its Anki
  interval, which a pull never returns, and FreeMemo's own schedule is independent of
  it. The AI grading arm also rates only Again and Good, where self-rating gives all
  four, so one card's history can hold both grains.
- **Building a self-host image now needs outbound internet.** The client libraries are
  not in the repository. The build downloads them and checks every byte against a
  committed lockfile, so a drifted or tampered upstream fails the build instead of
  shipping. The running container needs no internet for them.

### Technical

- `freememo.math` is the single owner of the stored form (`\(TeX\)` inline text)
  and of all four boundary conversions. The delimiter form was chosen
  specifically so `clean-html` needs no allow-list change and `strip-html`
  carries math into `content_text` for search.
- Every read of a Quill root's innerHTML now goes through `quill-field/editor-html`
  (seven sites). A raw read persists the KaTeX subtree, which `clean-html` reduces to
  duplicated fallback text.
- `init-editor!` is asynchronous now — it waits on `math/on-katex-ready!` and a
  generation counter guards it. It returns nil, so read `!editor-state`. **Correction:**
  that gate races `window.__katexReady`, which `0604fe6` deleted from both HTML files
  later in this release. It resolves `false` on every page load, which is the regression
  in the first known issue.
- Anki push mapping is applied to the whole field map in `build-note` /
  `build-update-fields`, not inside the five per-kind builders.
- The Anki-modified overlay diff now normalises both sides before comparing, so math
  cards do not read as perpetually modified.
- `freememo.formula-ui` owns the authoring UI: the anchored edit popover plus the
  typed-source converter it installs. It is the fifth hand-rolled anchored popover in
  the app and the first to hold a text input — which is why it is the only one where
  an outside click commits rather than dismissing, and why scroll repositions instead
  of closing. Both departures are recorded in `plans/math-support.md` round two.
- Formula creation is Delta-driven, not focus-driven: a `text-change` schedules a
  microtask that converts complete `\(…\)` regions outside code (`quill.getFormat`)
  in reverse document order, with source `"user"` so undo can reach it.
- **`plans/katex-gate-vendor-ensure.md`** specifies the repair for the gate above:
  route `freememo.math` through `vendor/ensure! :katex`, memoize on the first
  `on-katex-ready!` call rather than at namespace load, keep the 3 s cap for the
  request-stall case, resolve `false` on rejection, prefetch at the `/library/cards`
  boundary, and collapse the duplicate `render-math!` that `assistant_panel` carries.
  That duplicate is why the assistant chat is the one surface still typesetting.
- **`freememo.supermemo-format`** decodes `contents.dat`, `ElementInfo.dat`,
  `compon.dat`, `repetitions.dat`, `RepetitionHistory.dat`, `reference.dat`, the
  `.mem`/`.rtx` registries, and the `.sub` subset files. Four record layouts come from
  SuperMemoAssistant. Every scheduling field, Borland Real48, and the per-collection day
  epoch were recovered by measurement. `plans/supermemo-import.md` records each layout
  beside the check that confirms it — item burden against `collection.ini`, the embedded
  element id in each component group, the trailing member id in each `.rtx` record.
- **A component header carries its own struct size** — the high byte is the size minus
  one, so `(inc (bit-shift-right header 8))` derives it. The first version used a lookup
  table built from the types one collection happened to contain, and the walk stopped at
  the first unlisted header, losing every later component of that element. That cost 289
  items their answer.
- **Priority is rank, not a stored number.** Every `ElementInfo` offset was scored
  against the `priority.sub` ordering at five widths and the best correlation was 0.72
  at a derived date. `topics.sm_rank` therefore joins the queue `ORDER BY`, or the
  per-day hash would discard the order the import preserved. Natively created topics
  hold null and keep the daily shuffle.
- **`freememo.archive`** reads ZIP and 7z behind one function, with the same guards on
  each: path traversal, entry count, per-entry bytes, and total uncompressed bytes.
  7z entries are read in **stream order** — SuperMemo archives are solid LZMA2, and
  `SevenZFile.getInputStream` per entry re-decompresses the whole block, which turned a
  26 MB archive into more than ten minutes. Stream order does it in 1.8 seconds. The
  SuperMemo flow raises the total cap to 1 GB, since a collection is routinely past
  150 MB uncompressed.
- **Deps** — added `org.apache.commons/commons-compress 1.27.1` and
  `org.tukaani/xz 1.10`, both for 7z.
- **Cloze conversion order matters.** The SuperMemo marker is matched as a class token,
  not as the string `class=cloze`, and the conversion to `{{c1::…}}` runs **before**
  `clean-html`, which strips the unlisted `cloze` class and would leave an
  unidentifiable `[...]`. `cloze/validate` alone is not sufficient either: a glossary
  laid out as a definition list puts the opener and closer on opposite sides of a block
  boundary, which the character-level parse accepts. An item becomes a cloze card only
  when `validate` passes, `mask-ord` output has balanced tags, and `answer-for-ord` is
  non-blank.
- **`card_schedules`** — new table keyed `(flashcard_id, ord)` carrying the same eight
  FSRS columns as `kg_questions`. No column was added to `flashcards`. Rows are created
  lazily on an item's first review, so no card write path needed a trigger or a
  materialize call, and an orphan ord left by an edit is simply never drawn.
- **`kg_reviews` holds both item types** — `question_id` relaxed to nullable,
  `flashcard_id` and `ord` added, and a CHECK requires exactly one of the two ids. A new
  `grade_source` column separates `ai` from `self`, without which `rating=3` and a null
  verdict is ambiguous against a legacy row. One log table leaves `fsrs-daily-counts`
  untouched, which is what makes the shared daily cap free.
- **One item reference shape** — `[:question <id>]` or `[:card <flashcard-id> <ord>]`,
  through the draw, the client queue, the apply function, and the log.
  `draw-fsrs-due-queue` is now `draw-review-queue` and `fsrs-due-count` is now
  `review-due-count`. New: `freememo.quiz-card-turn` (both arms plus the card history
  body) and `resources/prompts/card-grade.md`. Card grading bills under `:cards.grade`.
- **`freememo.cloze` was rebuilt on one shared `parse`** — `validate`, `ords`,
  `mask-ord`, and `answer-for-ord` all read it, so nesting, hints, repeated ords, and
  math regions behave identically everywhere. `test/freememo/cloze_test.clj` covers it
  in 16 tests, and `test/freememo/fsrs_integration_test.clj` adds 3 tests against a real
  Postgres for the mixed draw, lazy schedule creation, prompt re-masking, a stale ord
  after an edit, and cross-user isolation of both read and write.
- **`src-build/vendor.clj`** — JDK-only `fetch!`, `verify!`, `check-refs!`, and
  `write-lock!`. `vendor-lock.sha256` pins 47 files, 3.4 MB. A fresh clone must run
  `fetch!` before `clj -M:dev -m dev`, and `dev/assert-vendor-assets!` refuses to boot
  without it. The Dockerfile fetches before `COPY src` and verifies after
  `COPY resources`, because that merge is an assumption whose failure would ship a jar
  whose every asset 404s.
- **`freememo.vendor-libs`** returns one shared promise per group, resolving only after
  every file executes and every `:probes` global is defined. Never call it from an
  `e/defn` body. The `.cljs` wrappers keep their own presence guards, so a call site
  that forgets `with!` degrades silently instead of throwing. Open Sans ships as
  committed CSS with hash-locked woff2, because Google's `css2` endpoint serves
  release-hashed filenames.
- **`assistant_chats.mode`** — persona per chat, an `:id` from
  `freememo.assistant-modes/registry`. NULL on rows predating the column, which
  `resolve-mode` maps to Socratic, correctly: Socratic was the only persona that ever
  existed before it. `set-assistant-chat-mode!` deliberately does not touch
  `updated_at`, because the chat picker orders on that column.
- **`html_cleaner` allow-list** — 10 class tokens added (`ql-align-right`,
  `ql-direction-rtl`, `ql-indent-3`..`8`, `ql-font-serif`, `ql-font-monospace`), and
  `safe-color-props` replaces the hard-coded `background-color` test in
  `sanitize-style-value`. Every value is a literal from Quill 2.0.3's own attributor
  whitelists, so the set is closed. `ql-cursor` and `ql-resize-style` stay excluded.
  `test/freememo/html_cleaner_test.clj` holds a fixture per control, so it fails if the
  allow-list stops covering a format it covered — but a control added without a fixture
  still produces no failure. An admitted class token stays inert outside an editor,
  which is pre-existing: Quill scopes every format rule under `.ql-editor`.
- **`new` needs a plain name in the constructor position.** `(new (.-Group (konva)) …)`
  emits `new konva.call(null).Group(…)`, which JS parses as calling the class without
  `new`. Bind the library to a local first. The symptom names a minified class and comes
  from inside the library's own stack, so it is not greppable. Audit with
  `grep -rn '(new (\.-[A-Za-z]* (' src/`.
- **A stacked modal must be a DOM sibling of a `pointer-events: none` overlay**, never a
  descendant of it. The property inherits, so a nested modal is un-hit-testable while
  its Escape still bubbles into the outer `ModalEscape`. It must also be the last form
  of the body, since both overlays sit at `z-index: 1000`.
- **`broadcast-changelog.sh`** wraps `freememo.changelog` with preflight checks, a
  confirmation, and a per-version double-send guard whose markers live outside the repo.
  It renders nothing itself, so the email cannot drift from `render-broadcast`. Dry run
  by default. `--send` is the only path that delivers.
- **`docs/`** — the Done/Delete/Dismiss distinction, the Learn dashboard spec, and the
  Learn queue ordering contract. Added to `tools/publish-clean/EXCLUDED`, along with the
  broadcast script.

## v20260729-bb11960

### For users

- **Card edit history.** Every card save now keeps the rendition it replaced, so you
  can always look back at what a card used to say. **History** sits in the card edit
  modal (basic, cloze, overlapping) and in the image-occlusion editor; each entry is
  labelled with when it was superseded.
  - **Occlusion history shows masks where they used to be** - a rendition written
    before you moved or deleted a mask draws that mask at its old coordinates over
    the same image. Removing one mask from a group no longer erases the record of
    what it looked like.
  - **View only** - no restore, no diff, nothing in the modal writes. It is a record,
    not a rollback.
- **Grouped masks in image occlusion.** Several masks can act as one card, the way
  Anki's "group of shapes" does, instead of the old strict one-mask-one-card.
  - **Two tools** - Draw (R) adds masks; Select (S) adjusts them. In Select,
    Shift-click or drag a box over several masks, then Group (G) makes them one card
    and Ungroup (U) splits them again. A group moves and resizes as one.
  - **The counts tell you what you'll get** - the footer reads "5 masks → 3 cards",
    and a grouped row in the card tables reads "group 3 · 2 masks" instead of
    "mask 3". On the Anki front, every mask in the asked group hides together and
    reveals together.
- **Paste any image.** Screenshots, dragged files, and images copied off a web page
  now go into your media storage, with the editor holding a link to them.
  - Bug fix: pasting a screenshot into Add Card used to fail with "answer exceeds
    10000 character limit" — the image was being embedded in the card's text. The
    same paste into a document editor silently stored megabytes of it instead.
  - **WebP, GIF and AVIF paste now** - Quill accepted only PNG and JPEG and discarded
    everything else with no message at all. SVG, BMP and TIFF are still refused, but
    now say so rather than doing nothing.
  - A save that races the upload stores the image anyway, and HTML pasted out of an
    imported EPUB gets the same treatment on its way to the database. Three old cards
    that were already carrying embedded images were converted on this release's first
    boot.
- **Drag an image to resize it** - handles on any image at least 40px wide, in the
  document editor and in card fields. The size survives a reload, the card path, and
  a push to Anki.
- **Right-click an image → Copy image · Save image** - in the document editor, above
  the existing Pin and Image Occlusion items. Images hosted elsewhere also offer
  **Copy image address**, because their bytes depend on the remote server's headers
  and a plain copy can legitimately fail there. Escape closes the menu.
- Bug fix: an image could render distorted in Anki while looking correct in the
  editor. A leftover `height` attribute is inert in the editor and was being obeyed
  by Anki; the Anki stylesheet now matches the editor.
- **`Cmd/Ctrl+Shift+A` adds a card** - from anywhere in the viewer, and the passage
  you had selected is marked gold in the editor, the same mark Generate leaves. The
  mark means exactly one thing: this passage reached the modal.
- Bug fix: **a card could be saved twice.** With two tabs open on the same account,
  every add — cards, occlusion, score, bibliography, undo — ran once per tab, because
  the queue of pending writes belonged to the account rather than to the tab that
  made them.
- Bug fix: **a generation that comes back empty is now retried.** A model
  occasionally answers with no content at all; that was reported as an unreadable
  response, and the toast said "please try again" while the code did not permit a
  retry. Empty, truncated and unparseable responses now retry within your existing
  retry setting, and a refusal stops immediately and says the model declined rather
  than blaming the response format. A successful generation charges for every attempt
  it took; one that fails outright charges nothing.
- **A public roadmap** - `ROADMAP.md` lists what is under construction, what is
  queued, and what is merely direction. Nothing in it is a commitment.

### Known issues

- **Pending writes no longer survive a tab reload.** Moving the queue into the tab
  that made the writes is what removed the double-save above, so a write still in
  flight when you reload is dropped instead of resumed. Adds are cheap to reissue;
  that was the trade.
- **Card history starts now.** Renditions from before this release cannot be
  recovered, there is no restore and no diff, and a card overwritten by an Anki pull
  records no version. The list is capped at the 500 most recent renditions and says
  so when it hits the cap.
- **Mask numbers show permanent gaps.** Once you group or ungroup, a number is
  retired and never reused, because each one is bound to an uploaded Anki media file.
  Anki refills its gaps; FreeMemo can't.
- **Occlusion grouping shipped without its full manual pass.** Unit tests, the
  production build, and the underlying canvas primitives were all verified, but the
  end-to-end matrix — group two already-saved masks, ungroup, group resize, push,
  review both modes — had not been run against a live app. If a grouped card
  misbehaves in Anki, contact me.
- **Cancelling Add Card leaves the gold mark.** There is no un-highlight; Generate
  has had the same hole on a failed generation.
- **`Ctrl+Shift+A` is untested on Firefox**, where it opens the Add-ons Manager. On
  Chrome it is Search Tabs and FreeMemo wins the chord.
- **A failed image upload leaves the image embedded.** You see the image plus a
  message naming the failure, and the bytes are stored on the next save rather than
  lost. Resizing an image also leaves a few inert leftover attributes in stored
  document HTML.
- **Knowledge-graph leftovers.** Most stored entity rows reference no fact — the bulk
  of them from a document deleted long ago — and around fifty quiz questions have no
  fact behind them, yet still appear in Review's new-card tier with nothing to grade
  against. The sweep that collects both is specified but not written yet.
- **The empty-response incident's cause was never established** — nothing logged the
  response shape at the time. It does now, so a recurrence will name itself.

### Technical

- **`card_versions`** - write-behind (a row holds the *superseded* rendition, so the
  first edit captures the original LLM output for free), event-scoped via
  `scope_type`/`scope_id`, owned by `root_topic_id ON DELETE CASCADE` so history
  outlives `reconcile-occlusion-group!`'s row churn. `db/update-flashcard-versioned!`
  does read-old/compare/insert/update in one transaction; `db/update-flashcard!` is
  untouched, which keeps the Anki pull path version-free by construction rather than
  by a flag. Measured 356 B/version (225 heap + 111 index — the index term was 5×
  the pre-implementation estimate); whole-fleet projection stays under 5 MB against
  the 451 MB `topic_files` already holds.
- **Boot migration `normalize-inline-card-images!`** - runs inside `setup-schema`
  after the grandfather migration, so no inline-base64 card can exist by the time a
  save could version one. Per-card try/catch (an escaping exception would stop the
  server booting) and `:meter-quota? false`, since the bytes are being relocated out
  of the user's own rows.
- **`card_history_modal.cljc` is its own namespace** - `card_modals.cljc` is already
  near the `:prod`-only bytecode limit. `!history-open?` is positional, never a
  ctx-map key, and both History buttons mount the modal as a *sibling* of the edit
  modal's container, which is `pointer-events: none`.
- **Occlusion grouping is ordinal-as-group** - a shared `mask_ordinal` *is* the
  group; unsaved rects carry a transient `:gid` that is never persisted, so storage
  stays single-channel. `checked-geometry` (was `validate-geometry`) is the one
  normalization boundary. `uq_flashcards_mask_ordinal` is created inside try/catch
  like the other unique indexes — check the boot warn log for legacy duplicates.
  `db/occlusion-mask-count-expr` is a correlated subquery over
  `og.geometry->'rects'` projected through the shared `card-group-selects`, so the
  "k masks" label needs no denormalized column. `normalize-group-transform!` folds
  the Konva group's position *and* scale into its children after every drag and
  resize, so reparenting a child can never shift it.
- **`freememo.client-state`** - browser-local atom registry, fresh per page load. The
  double-save was `:pending-commands`/`:pending-cards` living in per-user *server*
  atoms while `CommandDispatcher` mounts once per session: N tabs meant N dispatchers
  draining one queue, and `e/Token` dedups per dispatcher only. `optimistic/run-command!`
  methods now return `{:ok? :real-ids :error}` and the client applies the outcome;
  invalidation channels stay server-shared so a card added in one tab still refreshes
  the other's table.
- **`openrouter/message-content` + `retryable-cause?`** - typed causes
  `::refused` > `::no-choices` > `::truncated` > `::empty-content`, plus
  `:freememo.llm-edn/unparseable`, with one predicate owning the retry/terminal
  mapping. Retry moved *inside* `kg-llm/chat!` for all six call sites with `:cost`
  redefined as summed across attempts, and the two hand-rolled
  `(:raw (ex-data e))` discriminators — which were falsy for exactly this failure, so
  both KG lanes silently declined to retry — are gone. Terminal-empty is routed to
  `:alert/pipeline-failures`; a refusal stays `:info`, like `::spend-refused`.
- **`image-rehost/rehost-data-uris!`** - data-URI-only, no HTTP, no URL
  absolutization, never throws. Deliberately *not* `rehost-html-images!`, which would
  download every remote `<img>` as a side effect of typing. Called from
  `cards/save-cards` and from `db/update-topic-content!` (which gained a `user-id`
  parameter). `util/storable-image-mime-types` and `util/mime->ext` are now the single
  sources of truth for both ends of that path; the MIME allowlist is a security gate
  too, since `clean-html` allow-lists the `data` protocol on `img src` with no MIME
  restriction.
- **`editor-actions/install-image-rehost!`** overrides `uploader.upload`, not
  `uploader.handler` - Quill 2.0.3 filters against `Uploader.DEFAULTS.mimetypes`
  *before* the handler runs, and `Clipboard.onCapturePaste` routes any files-bearing
  payload straight to `upload`, which is why the clipboard matcher this replaces could
  never fire. The data URI is deliberately its own placeholder (a `blob:` URL cannot
  be resolved server-side, so a racing save would persist a dead src), and the swap
  goes through the blot, not `setAttribute`.
- **`client-errors/report!` gained a 3-arity `notify`** that pushes an error toast
  beside the log entry — for failures in JS callbacks with no Electric frame to
  observe them, like a paste the browser rejected outright.
- **`editor_pin_menu.cljs` → `editor_image_menu.cljs`**; `#pin-context-menu` →
  `#image-context-menu`. `src->blob!` extracted so Copy and Save share the decode
  path with the media upload.
- **Dep** - `quill-resize-module@2.1.3` UMD from jsDelivr, loaded after `quill.js`;
  it self-registers `modules/resize`, so there is no `Quill.register` call. Options
  in `quill-field/quill-config`: `:attribute ["width"]` (Quill's image blot keeps only
  alt/height/width and `clean-html` allow-lists the same, so a width attribute is the
  only size representation that survives), `:minWidth 40`, `:embedTags []`, and the
  module's align toolbar omitted — its classes are stripped on the card path, so
  alignment would persist in documents and vanish on cards. CSS ported into
  `index.css` with `light-dark()` instead of a CDN stylesheet.
  `blot-formatter2` was rejected: its overlay is not click-through, so `elementFromPoint`
  returns a `DIV` and the image context menu dies.
- **`users.display_name`** - filled from Google's `name` claim at login but only when
  NULL, so a hand-curated name survives later logins. `freememo.outreach` sends one
  personalised message per recipient, which `changelog.clj` structurally cannot
  (single BCC, rendered from `CHANGELOG.md`); recipients are a literal id vector and
  each success writes a user event the send then skips, so a re-run after partial
  failure resumes instead of double-mailing. New `db/get-mail-addressees`,
  `db/user-ids-with-event`, `db/insert-user-event!` 3-arity with jsonb metadata.
- **Desktop shell POC** - an Electron window over the deployed origin, no local
  server and no local database, kept out of the published tree. Google blocks OAuth
  from embedded webviews, so the shell signs in over `POST /login`. The full local app
  is deferred: four bindings to a real PostgreSQL server (`pgcrypto`/`pg_trgm`, large
  objects, `jsonb` round-tripping, partial-unique `ON CONFLICT … WHERE`), three
  shelled-out native binaries, and every client library loading from a CDN.
- **KG orphan sweep, specified only** (`plans/kg-orphan-collection-sweep.md`) - one
  hourly job deleting unreferenced entities and *retiring* unlinked questions, never
  deleting them, because `kg_answers.question_id` is `NO ACTION` and many orphans have
  answer rows. Entity collection needs an age guard: distill inserts entities and
  facts in separate transactions, so an entity legitimately has no fact for that gap.
- **Tests** - new `openrouter_test.clj` (cause table, precedence, retryable
  false-table), `occlusion_ordinals_test.clj` (partition/assign rule),
  `image_rehost_test.clj`, plus an `llm_edn_test.clj` case asserting the unparseable
  throw carries `:type` alongside `:raw`/`:cleaned`. Still not wired to CI.

## v20260727-e36e8d7

### For users

- **Video, read the way you read a PDF.** Upload a video file and FreeMemo treats it
  like any other document: it plays, remembers where you stopped, transcribes itself,
  and you carve the parts worth keeping into ordinary editable topics that enter the
  Learn queue and generate cards exactly as text does. Import → **Video**.
  - **Playback that holds its place** - resume where you left off, a waveform strip
    under the player for scrubbing and marking, and the whole video column beside the
    editor with both splits resizable by drag.
  - **A transcript that follows the playhead** - the spoken segment is highlighted and
    scrolled into view; scroll away and it stops yanking you back until you seek
    again. Click any segment to jump the video there. **Copy** writes the whole
    transcript into the document so you can prune it down to what matters.
  - **Time-range extracts** - mark a range on the waveform, press **Extract**, and you
    get a child topic holding the transcript text that range covers. From then on it
    is an ordinary topic: editable, schedulable, card-generatable, with no
    video-specific special-casing anywhere downstream.
  - **Several files at once become a playlist** - stage as many as you like (duplicate
    detection, per-file removal, **＋ Add more**), then send them to a new playlist, an
    existing one, or separate documents.
  - **`.mkv`, `.mov` and `.webm` play now** - they are converted to MP4 on upload (the
    picture is copied untouched; audio is re-encoded only when your browser could not
    have played it), which also repairs seeking on files that carry no index.
  - **Transcription is opt-in per upload** - a "Transcribe after upload" checkbox with
    an estimated cost, remembered as your default. Skip it and you still get a
    playable, seekable, scrubbable video; **Transcribe** / **Re-transcribe** in the
    transcript pane runs it whenever you want. A re-run never touches content you
    curated — only **Copy** writes there.
  - **Transcription language** lives in Settings → AI features, default auto-detect.
    Bug fix: a short English recording could come back as fluent, invented Arabic.
    Auto-detect now pins the language detected on the first chunk across the whole
    file, so a quiet passage mid-lecture can't flip the rest into another language.
- **Transcription is now billed.** Both the audio kind and video, gated against your
  balance before the call and charged from what the provider actually reports, as one
  ledger row per run rather than one per chunk. It was an LLM call being given away,
  which was never intentional. Roughly: 5 minutes ≈ 80 credits, 43 minutes ≈ 680, a
  three-hour lecture ≈ 2,850.
- **Storage is now metered.** Video moves storage from incidental to a real cost, so
  stored bytes accrue rent at the rate now printed on the Storage card and in the
  Credits panel (currently 40 credits per GB-month). Your free cap is 1 GB; your first
  credit purchase raises it to 100 GB. If your balance reaches zero while you hold
  video, playback pauses, a 14-day grace window opens with a notification, and only
  after it expires are the video bytes reclaimed — topics, transcripts, extracts and
  schedules all survive.
  - **Credits moved to Settings → Account**, out from behind the Enable-LLM-features
    toggle. Rent accrues whether or not you ever turn an AI feature on, so hiding the
    balance behind an AI switch was wrong.
- **The Quiz is something you can curate, not just endure.**
  - **`/quiz` lands on a dashboard** - four tiles (Due now, Reviewed today, Flagged,
    Suspended) over **Reviews** and **Sessions** tabs. Reviewing is one click behind
    **Start**; an exam you left mid-way still resumes ahead of the dashboard.
  - **Every answer is now recorded** - the Review flow used to throw yours away.
    Clicking a Reviews row shows what you typed, the verdict, the explanation, the
    reference answer and the facts you missed with their `(Doc, p.N)` provenance.
  - **Flag, Suspend & Skip, and Edit** in all three flows. Flag marks a question for
    later editing and changes nothing about scheduling; Suspend withholds it from
    every future draw until you unsuspend it from the Questions bank; Edit opens the
    question editor without leaving the quiz. Suspending writes no FSRS state, so a
    suspension spanning the due date returns the card as overdue rather than
    rescheduling it.
  - **Submit is disabled on an empty answer** in all three flows, instead of silently
    doing nothing.
  - **Better questions.** `"Name one of the components that…"` was prescribed by the
    generation prompt, not an accident. Questions are now generated per *cluster* — a
    subject and predicate with several true objects yields one question per object,
    each discriminated by that object's own facts and never naming its siblings — and
    the "name one" family of phrasings is forbidden outright.
  - **History shows the wording you actually answered.** Lists keep the recorded
    wording; detail views lead with the live wording and raise "this question has been
    edited since you answered" when the two differ.
  - Bug fix: grading a *correct* answer in the Review flow crashed the session and
    latched the UI on "Grading…" — after the LLM call had already been billed. A
    grading step that fails now degrades to graded-but-unscheduled instead of killing
    the session.
- **Facts tab: scoped sub-views and bulk delete.**
  - Pressing **Entities** or **Questions** from a document's Facts view keeps that
    document's scope instead of dropping it. Documents stays unscoped, since a scope
    has no meaning there.
  - **Select facts and delete them as one operation** — either everything visible
    under the current filter, or the whole document — with one Undo entry for the lot.
  - Bug fix: the Questions table's headers no longer drift out of line with the
    columns beneath them (a reserved scrollbar gutter made header and body measure
    different widths — fixed for every table sharing that shell).
- **Library cards view, reworked.**
  - **Filters live in the URL** — type, sync state, flags, sort and search are all in
    the query string, so a filtered view is reloadable and shareable. Unknown values
    are ignored, so no URL can produce a broken view.
  - **Redesigned toolbar** - view switcher, search and Check Anki on one row, then
    labelled **Type** and **Sync** chip rows. Active chips are a tinted fill with
    accent text (contrast-checked in both themes) rather than a solid block, and
    FM/Anki chips carry their source colour in both states.
  - **A spinner while a filter is in flight**, blind to background re-queries — an
    Anki sync happening behind you never flashes it. Counts and rows stay on screen
    while superseded rather than blanking.
  - **Occlusion and score cards render properly** in the table, with their own badges,
    and the Type/Sync filters take multiple selections at once.
  - **Filtering no longer freezes the view.** Toggling a chip that matched nothing used
    to block the browser for ~2.6 s and eat your next click; it is now ~1.1 s, and the
    table is never torn down and rebuilt around an empty result.
- **The Generate button generates again.** It is now a split button: the primary zone
  generates immediately and reads what it will do (`Generate 2 Basic`), the caret opens
  the options. Card type is a segmented Basic/Cloze/Overlapping pill with a one-line
  description, the count lives in the menu only, and Compare finally has its own icon
  instead of sharing Generate's sparkles.
  - The editor's selection popup also gained a card count and a **Generate here**
    action, so you can make cards from a selection without going to the toolbar.
- **Per-document learning goal.** Document options gains a goal field — why you are
  studying this document. The Socratic assistant grounds its questions in it and steers
  toward facts you have not yet mastered, and card generation reads it too. The
  assistant also now points you at the passage to re-read rather than restating it.
- **PDF pages stop drifting.** The page you are on is part of the URL
  (`/viewer/topic/<id>/<page>`), so reloading or sharing a link lands on that page
  instead of somewhere near it, and the page stays anchored through zoom, fit-width,
  fit-page and the reflow that follows.
- **Security hardening.** An internal audit ran this cycle. Every mutation that
  touched a card, pin, schedule, occlusion/score group, Anki sync fetch or CSV export
  by bare id now verifies you own it first; EPUB import is capped against
  decompression bombs; and the database and SPARQL ports no longer publish to every
  interface by default.
- **Two new models** for card generation and OCR: Gemini 3.6 Flash and Gemini 3.5
  Flash-Lite.
- Bug fix: the document editor silently stopped auto-saving. A debounce left the write
  behind a binding whose value was discarded, which Electric never evaluated — so no
  edit was ever persisted.
- Bug fix: double-submitting the Add-card, Occlusion or Score modal created duplicate
  cards. Each modal-open now carries one idempotency key, so a re-fired submit collapses
  to a single save.
- Bug fix: **Done** on an extract marked it done but no longer advanced the learn
  session to the next item.
- Bug fix: the Anki Sync modal, the delete-confirm modal, autofocusing typeaheads and
  the command palette all opened unfocused, leaving Escape, Cmd+Enter and Tab dead
  until you clicked inside. (HTML `autofocus` does nothing on an element inserted after
  page load — it is the HTML spec, not a quirk.) The command palette and Anki modal
  also trap Tab within themselves now.
- Bug fix: hovering the selected card-type segment or the active side-panel tab left
  white text on a light background.
- Bug fix: a nested cloze such as `{{c3:: {{c1::x}} {{c2::y}}::hint}}` was rejected as
  "unclosed", and a stray `}}` in pasted code could trip the same check.
- Bug fix: three typeahead fields — the Anki deck picker, the assistant's `@`-mention
  picker and the pre-prompt field — had silently stopped writing their value; the
  pre-prompt field had lost its write path entirely.
- Bug fix: Undo History rows for knowledge-graph rejections rendered as an
  indistinguishable fallback; every action type is now named.
- Bug fix: the AI-cost table's header no longer drifts from its columns, and it sticks
  to the top while you scroll.

### Known issues

- **The free storage cap is 1 GB**, which one full-length lecture will exceed. A credit
  purchase raises it to 100 GB. Storage rent then accrues on what you hold, with no
  action needed on your part to incur it — that is the point of the rate being printed
  in two places.
- **The transcription cost estimate has not been checked against a real charge.** The
  arithmetic and every code path are verified, but the account I test with is at a
  negative balance and closing that gap means writing to a live financial ledger. Every
  actual charge still comes from the provider's reported cost, never the estimate. If a
  charge ever looks wrong against the estimate you were shown, contact me.
- **The estimate needs your browser to read the file's duration.** A container your
  browser cannot decode (an FFV1 `.mkv`, for instance) contributes no length, so a mixed
  batch states a floor — "40:01 of video, plus 1 file of unknown length" — rather than
  passing off a partial sum as the total.
- **Skipping transcription does not skip storage.** Remux, duration and audio extraction
  are free local work and are what make the video playable and scrubbable, so they always
  run; the bytes still count against your quota and still accrue rent.
- **A playlist parent enters the Learn queue** alongside its videos, matching how a PDF
  root behaves. Whether that is what you want is genuinely undecided — tell me if it
  isn't.
- **The transcript highlight is segment-level and can lag up to a quarter second.**
  Word-level timing is not stored. Chasing the last 250 ms would cost 16× the polling
  rate for no real gain.
- **The Transcript side-panel tab is gone** — the transcript now lives in the video's own
  column. If you had that tab selected, the side panel opens on Pins instead.
- **A failed grade in the Review flow discards your typed answer.** Quiz and Exam persist
  the answer before grading precisely so it survives a grading failure; Review does not,
  by design — a failed grade is not a review event. The existing "your answer was kept,
  try Submit again" toast is the recovery.
- **In an Exam, Suspend & Skip is the only way past a question you cannot answer**, and
  it suspends that question permanently until you unsuspend it from the bank. Accepted
  consequence of disabling empty submissions. A skipped question still counts in the
  exam's total and reports as ungraded.
- **Question generation is deliberately non-idempotent.** An object with nothing in the
  graph to distinguish it from its siblings is omitted rather than turned into a "name
  one" question — so it stays uncovered, and is re-sent and re-billed on the next run.
  The completion toast reports the omitted count, because a run writing 12 questions for
  40 facts otherwise reads as a silent failure.
- **Existing questions are not regenerated.** The quality change applies to new
  generation only; your current bank keeps whatever phrasing it was born with.
- **Filter changes in the cards view reset scroll to the top.** By design — filter and
  sort state is what resets the scroll position, so an in-place row-count change (a
  background sync, a delete) does not.
- **The two new Gemini models need a config change plus a restart on my side** before
  they appear in the credits-mode allowlist. If one is missing from your model picker,
  that is why.
- The remaining findings from this cycle's security audit are tracked privately and will
  land in later releases. None of them are reachable from the public internet.

### Technical

- **Video pipeline.** New `freememo.largeobj` (`lo_create`/`lo_write`/`lo_read`/
  `lo_unlink`; `open-range-stream!` owns its own pooled connection so a Ring body can
  outlive its handler), `freememo.ffmpeg` (bounded `ffmpeg`/`ffprobe` subprocesses),
  `freememo.video` (probe → remux → extract → chunk → Whisper, with chunk-offset
  arithmetic), `freememo.video-http` (`init`/`chunk`/`finalize`/`abort`/`status`/
  `position` plus ranged `GET /api/video/:id`), and `freememo.storage-meter`. Client
  side: `video_upload.cljs`, `video_wavesurfer.cljs`, `video_pane`, `video_transcript`,
  `video_extract`, `video_import_modal`, `video_probe.cljs`, `video_format`.
- **Why large objects.** Video exceeds Postgres' 1 GB `bytea` ceiling, and `lo_write` is
  the native append primitive — so a chunked three-call upload writes straight into the
  large object and the global 100 MB HTTP body ceiling never has to move. Heap holds one
  chunk. The quota gate runs once at `init` against the declared size inside one
  transaction, so two concurrent inits that would jointly exceed the cap can't both win.
- **Four new tables** — `topic_videos` (`lo_oid`, `byte_size`, `duration_ms`,
  `last_pos_ms`, `remux_pending`), `video_transcripts` (one row per segment),
  `video_segments` (extract ranges), `video_upload_sessions` — plus
  `users.last_metered_at`, `storage_grace_started_at` and `storage_debt_micro`.
  `topics.kind` is unchanged; `video` and `video-playlist` are just new values.
- **`usage_bytes` is now all stored bytes** — `SUM(topic_files.file_size) +
  SUM(topic_videos.byte_size)` — backfilled once, with large-object unlinking wired into
  every deletion path (hard delete, staged purge, user delete, orphan sweep,
  grace reclamation) and three sweep jobs. The orphan detector excludes OIDs held by
  live upload sessions.
- **Metering** prices GB-months lazily on session boot (throttled, total, never able to
  block sign-in) and carries sub-IQD remainders in `storage_debt_micro`, or an hourly
  tick over a small library rounds to zero forever. `STORAGE_IQD_PER_GB_MONTH` unset
  disables metering entirely — the self-host default; it fails open deliberately,
  because billing zero by accident is recoverable and reclaiming uploads by accident is
  not. Config: `STORAGE_PAID_QUOTA_BYTES` (100 GB), `STORAGE_GRACE_DAYS` (14),
  `VIDEO_MAX_BYTES` (8 GB), `VIDEO_TRANSCRIBE_SEGMENT_SECONDS` (900).
- **Serving.** `ffmpeg` added to the runtime image; without it a video still uploads and
  plays, it just gets no duration, waveform or transcript. `/api/video/*` and the video
  MIME types are excluded from gzip in `prod.cljc` — gzipping a ranged response forces
  Jetty to drop `Content-Length` and invalidates the `Content-Range` offsets, which
  breaks `<video>` seeking. Pre-remux bytes are served `no-store` rather than withheld
  behind a 409, so a pipeline that never runs leaves an uncached working video instead
  of a permanently unplayable one.
- **Position is written by `navigator.sendBeacon`** on pause and unmount: an Electric
  effect is cancelled with the frame that issued it and cannot survive teardown, but a
  beacon is queued by the browser.
- **Wavesurfer never owns the clock.** The extracted MP3 is decoded once in a throwaway
  instance, `exportPeaks()` is taken, and the display instance is built over the
  `<video>` element with those peaks — so the `<video>` is the single playback clock.
- **Quiz schema.** `kg_questions.flagged` / `.suspended` (with a partial index),
  `kg_reviews.user_answer` / `.explanation` / `.question_text` / `.missed_fact_ids`, and
  `kg_answers.question_text`. One shared suspended-exclusion predicate feeds all four
  draw/count sites; `question_text` is captured by subselect inside the writer rather
  than threaded through call sites. New `freememo.quiz-dashboard`,
  `freememo.quiz-feedback` and `freememo.question-curation` namespaces keep
  `quiz_page.cljc` under the bytecode ceiling.
- **`int-array-value` / `text-array-value`** in `db.clj`: a bare `[:array []]` renders as
  `ARRAY[]`, which Postgres rejects as untypable. `apply-fsrs-review!` hit exactly that
  on any correct answer, and the throw escaped an `e/Offload` and tore down the server
  session. Both writers route through the helpers now; regression test
  `empty-arrays-carry-an-explicit-type-cast`.
- **Cluster question generation** in `kg_questions.clj`: the work list groups approved
  facts by `(subject_entity_id, predicate_id)`, coverage becomes "linked to ≥1 approved
  atomic question", and object-entity neighborhoods are fetched in one batched query per
  batch instead of per object. `has_alternatives` is out of the work-list query and the
  `:alt true` paragraph is out of `kg-question-atomic.md`.
- **Cards view URL state.** `query-params` / `set-query-params!` in `freememo.util`; the
  filter vocabulary stays in `library_cards`. `replaceState` on every change including
  keystrokes, read once at mount — no history entries, therefore no `popstate` listener.
  Two text signals: instant drives the URL, 300 ms-debounced drives the query, the
  scroll reset key and `filters-active?`, so those agree with the rows on screen.
- **Filter-pending is derived, not observed.** `Offload-latch` buffers the previous
  result across a re-query, so there is no pending value to key an indicator off.
  The client hashes `opts`, the hash rides into the thunk and is `assoc`'d onto the
  result, and pending is `(not= opts-hash (:opts-hash result))` — which also makes it
  blind to `:refresh` / `:card-mutations` / `:sync-mutations` bumps.
- **The cards-table cost was reactive node count, not payload.** A CPU profile put all
  self time in `incseq`/`missionary` transfer frames with no application frame present;
  the zero-result transition moved 31 KB and burned 2.3 s. Two fixes: per-cell inline
  `:style` maps became CSS classes (Electric applies each style property as its own
  reactive prop — ~6 nodes × 8 cells × 22 rows), and per-row field reads are client-sited
  keyword lookups over one transferred row instead of 16 separate `e/server` reads.
  Empty-boundary block 2620 ms → ~1100 ms, remount 1307 → 752 ms.
- **`Method code too large!` is a `:prod`-only failure** and the reported line always
  names the file's *first* `e/defn`, because expanding it is what triggers loading the
  namespace's CLJ side. `library_cards.cljc` overflowed and two splits guided by that
  line changed nothing; a verbose load named the real file. Split into
  `CardsSearchRow`/`CardsKindFilterRow`/`CardsSyncFilterRow`,
  `SelectAllCell`/`SortHeaderCell`, `RowKindCell`/`RowAddedCell`/`RowDeleteCell`, with
  `overlay-ids-for` and `build-query-opts` lifted to plain `defn`s. The `(e/server …)`
  form binding for `result` was deliberately left in place — moving it into an `e/defn`
  flips wire behaviour. `clj -X:build:prod build-client` is now part of this namespace's
  done-condition; recipe recorded in `CLAUDE.md`.
- **Structured logging.** `freememo.logging` gains `audit!` (user-content mutations, one
  `:info` signal on a fixed `{:user-id :action :entity :entity-id :outcome}` schema) and
  `external!` (outbound call outcome + latency); `openrouter/post!` emits one
  `external!` per call with a feature tag. New `freememo.client-errors`: `report!`
  enqueues from any JS callback and a headless `ClientErrorForwarder` mounted in `Main`
  ships each entry to the server exactly once, tagged with a per-page-load session id.
- **IDOR closure.** Ownership predicates pushed into `db.clj` for flashcard
  edit/delete/anki-id writes, pins, topic scheduling, occlusion and score groups;
  `get-cards-for-sync` and `export-cards-csv` gate on user-scoped topic lookups.
  `epub/read-zip-entries` gained entry-count / per-entry / total-uncompressed caps
  throwing `::zip-too-large`, ported from `kg-code/unzip-repo!`. Published ports are
  `${FREEMEMO_BIND_IP:-127.0.0.1}` in tracked compose files, with the real interface in
  the untracked `.env` — never `0.0.0.0`, since the SPARQL endpoint is unauthenticated.
- **Dev boot stack.** `dev/-main` loads `freememo.main` on a 16 MB-stack thread via
  `dev-stack/call-on-big-stack` (`DEV_LOADER_STACK_MB`), because the `:dev` alias carries
  no `-Xss` and Electric's macroexpander overflowed the JVM default about one cold boot
  in five. `dev`'s CLJ requires must stay Electric-free, and `call-on-big-stack` must
  never be called from `user.clj` (`RT.class` monitor deadlock). Regression check:
  `clj -J-Xss512k -M:dev -e "(dev/load-app-namespaces!) (println :OK)"`.
- **`tools/publish-clean/`** — builds a public mirror with configured paths scrubbed from
  all history via `git filter-repo` into a persistent sibling directory; the source repo
  is never modified and re-running is idempotent. A standing `EXCLUDED` path list is
  unioned onto any flags. `CLAUDE.md` and `plans/` are now tracked in the private repo
  and scrubbed on publish.
- **Typeahead is id-keyed.** `Typeahead` takes `{:id :label}` and writes the `:id`;
  `Suggest` is the free-text sibling for fields where text absent from the list is a
  legal value. A string option used to fail silently — blank rows, a filter matching
  nothing, and `(reset! !atom nil)` *clearing* the selection. `check-options!` warns on
  the first non-conforming option as a backstop.
- **Add-card modal on Forms5**, with a capture-phase `submit` listener on an ancestor of
  the form so Quill's syntax-token flush lands in the field atoms before `:Parse` stamps
  the commit. `enqueue-pending-card!` is idempotent on a client-minted id via one atomic
  `swap-vals!`.
- **`HierarchySidePanel` split** into row / tree / panel — one ~190-line deeply nested
  `e/defn` was overflowing the compiler's stack. The Anki overlay's fetch set moved to a
  filter-independent `get-pushed-card-manifest`, so it no longer narrows with the
  `/library/cards` filters.
- **Tests.** New `test/freememo/video_test.clj` (range parsing, chunk offsets, storage
  pricing, MIME classification), `test/freememo/cloze_test.clj` (V1–V10 plus nil/empty
  against the extracted pure `freememo.cloze/validate`), and
  `test/freememo/kg_questions_test.clj`.

## v20260716-ceeb056

### For users

- **Knowledge graph, visualized.** A new **Graph** tab draws your knowledge graph
  the way Obsidian does — each concept is a node, each approved concept-to-concept
  fact is an edge — so you can finally see the *shape* of what you've collected,
  not just a list.
  - Click a concept to highlight its neighbours and open its fact panel.
  - Filter the view by document, relation, or connection count, and search to jump
    to a concept.
  - Facts whose object is a plain value (a date, a number) stay as attributes in
    the panel rather than drawing an edge, so the graph stays about how concepts
    connect.
- **Learn a codebase.** You can now upload a Clojure code repository (as a `.zip`,
  e.g. `git archive`) as a **Code Repository**. FreeMemo statically analyzes the
  sources and distills their structure — definitions, calls, requires, arglists,
  docstrings, visibility — into the knowledge graph, then generates and schedules
  quiz questions over them exactly like any other document. The code is only
  analyzed, never run.
  - Fact extraction is **opt-in**: tick "Extract facts into the knowledge graph"
    on the confirm step (off by default). If you skip it at import, you can still
    distill later from the Facts tab's **Distill** action.
  - The Help page has a new "Learning a codebase" workflow.
- **See your AI spend.** Settings → Account gains a read-only **AI costs** card
  showing your full credit ledger — searchable and filterable by kind, feature,
  model, date, and text, and virtual-scrolled so a long history stays fast.
  - A **Min amount** filter surfaces your largest spends (e.g. combine with the
    Debits filter to see everything 500+).
- **A sharper reading assistant.**
  - **Live reading context** - each question is now grounded in the pages
    *around where you're reading* (a configurable window, default 20 pages, in AI
    settings) plus the document's approved facts — no longer a single page frozen
    when the chat opened.
  - **Reliable math** - assistant replies render math with KaTeX, including inline
    `$…$`, and money like "$10" is no longer mistaken for math.
  - **Better Socratic tutoring** - replies follow a clear *"Where you are" /
    "Consider next"* structure, ask a smaller question instead of handing you the
    answer when you're stuck, and no longer open with empty praise.
  - Bug fix: your just-sent message no longer shows up as a doubled bubble while
    the reply is being generated.
- **The custom editor toolbar is now everywhere.** The on-selection bubble toolbar
  and code-block language picker (added for the main editor last release) now work
  in every card-edit modal — Basic, Cloze, Overlapping, and Occlusion. The toolbar
  image button also uploads the image now instead of embedding it inline.
- **Dismiss topics you're done with.** A SuperMemo-style **Dismiss** removes a
  topic *and its whole subtree* from the Learning Queue while keeping every row in
  your collection; **Undismiss** brings it back. Invoke it from the Library
  document row menu or the Viewer's document options. Dismissed rows grey out with
  a "Dismissed" tag.
- **Reworked top navigation.** Tabs that used to clip off-screen at certain window
  widths now collapse into an overflow (⋯) menu instead — nothing is ever hidden.
  Text destinations are grouped (Content / Discovery), **Import** is promoted to a
  left button, Help / Settings / Actions are pinned right as icons, the active tab
  is now a filled pill (readable for color-blind users), and every item has a
  tooltip. The **Knowledge** tab is now labeled **Facts**.
- **Live Document photo wizard.** Starting a Live Document from photos is now a
  focused wizard: the empty state is a clear, readable card with one **+** button
  that opens an Upload / Take-photo sheet, then a modal editor where you rotate
  *and crop* each photo on a canvas before committing the batch. Replaces the old
  clipped, low-contrast inline strip that only offered rotate.
- **Deployed build is visible.** Settings → Account now shows a `freememo · <sha>`
  line, so you can tell exactly which commit the live app was built from.
- Bug fix: on tablet-width screens the viewer was running desktop logic — it
  opened every panel and squeezed out the editor, popped the on-screen keyboard,
  and pushed the review bar below the fold. Panels now collapse by default under
  900px, touch no longer auto-focuses inputs, the Postpone/Next bar stays
  on-screen, and the copy/scan/generate menus no longer clip past the toolbar's
  edge. (The card-count stepper moved into the Generate menu.)

### Known issues

- **Code-repo import is Clojure-only for now.** Other languages, git/forge sync,
  and a code-tuned question prompt are deferred to a later phase. Fact extraction
  is off by default — enable it at import or distill later from the Facts tab.
- **Older chats and code repos don't get windowed context.** Chats created before
  this release still show their original frozen one-page context (no migration is
  applied). For code repositories the assistant grounds on whole-document facts —
  page/neighborhood scoping is deferred.
- **Making a cloze now needs a selection.** Because the cloze buttons live in the
  on-selection bubble toolbar, you must select text first before creating a cloze
  deletion. By design.
- **Dismiss leaves card/quiz queues alone.** Dismissing a topic removes it from the
  Learning Queue only; its flashcards and quiz questions stay in their own review
  queues. This is intentional.
- **AI cost history isn't live.** Debits from your current session appear on your
  next visit to the page rather than in real time, matching how the balance display
  already refreshes.

### Technical

- **Graph tab.** `freememo.graph-page` + `graph_render.cljs`; server layout in
  `freememo.kg-graph` via Graphviz `sfdp` (JGraphT dropped — 87s / OutOfMemory on
  the largest real graph; sfdp lays out the same graph in under a second).
  Positions cached per user in a new `kg_graph_layout` JSONB table keyed by the
  `:kg-mutations` counter and recomputed lazily on the next open after a mutation.
  Client renders with `sigma@3.0.2` + `graphology@0.26.0` as CDN UMD globals
  (`window.Sigma` / `window.graphology`) — the prod Docker build has no npm step —
  drawing fixed server positions with client-side visibility filters. `graphviz`
  **and** `libgvplugin-neato-layout8` added to the runtime image: the plugin is
  what actually supplies sfdp's layout engine, and its absence blanked every route
  in prod with an opaque "Broken pipe" (now `run-sfdp` also captures stderr). Added
  end-to-end pipeline logging (`graph-payload*`, `kg-graph`).
- **Code-repo ingestion.** New `freememo.kg-code` static analysis via `clj-kondo`
  (library API, added to `deps.edn`; not the native CLI) and
  `freememo.web-import/confirm-repo-upload!*`. `.zip` classified `:repo`, unzipped
  under entry-count / size / zip-slip guards into a `code` root topic plus one
  `code` child per source file; facts map definitions to entities by
  fully-qualified name over a fixed predicate vocabulary (`defined-in`, `calls`,
  `requires`, `has-arglist`, `has-docstring`, `is-private`), with calls/requires
  restricted to namespaces defined in the repo. `kg_facts` multi-row inserts are
  chunked under Postgres' 65,535-parameter cap (independent statements;
  `ON CONFLICT DO NOTHING` + idempotent re-distill let a partial run self-heal).
- **AI cost history.** `freememo.cost-history`; server reads
  `list-credit-transactions` (kind/endpoint/model/date/text filters, newest-first,
  capped at 5000) and filter-independent `credit-transaction-facets`. Virtual-
  scrolled like `search-page` — only the visible window rows plus the count cross
  the wire.
- **Assistant.** Windowed context assembled per send in `assemble-messages` /
  `send!`; new `assistant_pdf_window` setting (0–50, default 20) and
  `db/get-kg-facts-context` (page-ranged, row-limited). KaTeX replaces MathJax
  (`katex@0.16.11` CDN, gated on a `window.__katexReady` promise);
  `markdown/dollar-math->tex` rewrites real math to `\(…\)`/`\[…\]` server-side so
  no `$` delimiter reaches the client. New nullable `client_id` column on
  `assistant_messages` correlates the optimistic echo with its persisted row by a
  globally-unique id instead of a drifting reactive count.
- **Shared editor UI.** `QuillField` switched to the bubble theme;
  `insert-cloze!` / `cloze-max-n` / `upload-pasted-image!` extracted to
  `freememo.editor-actions` to break the `format-menu` ↔ `quill-field` cycle;
  `init-quill-field!` now returns `{:editor :teardowns}` so modals mounting several
  fields leave no orphan body cards or document listeners behind.
- **Recursive Dismiss.** New `topics.dismissed` boolean (modelled separately from
  `status`, so a done child survives a dismiss/undismiss round-trip); every queue
  and count query gains an `AND NOT dismissed` conjunct; the `topic_repetitions`
  `event_type` CHECK is widened for Dismiss/Undismiss on pre-existing installs.
- **Top nav / toolbar overflow.** Reuses the `ContentToolbar` content-aware
  overflow detector; new `freememo.toolbar-overflow`. Replaced the catch-all
  `button:hover { filter: brightness }` with a `background-color` tint wrapped in
  `:where()` — the filter promoted each hovered button to its own compositing layer
  and repainted large regions of the app on nav hover.
- **Live Document wizard.** `freememo.live-doc-wizard` + `live-doc-image-editor`
  (Konva crop canvas). Rotation/crop are metadata: original bytes are sent
  unchanged with parallel rotations/crops arrays, and `add-image-page!` bakes
  rotate-then-crop from normalized `{x,y,w,h}` rects (clamped at the API boundary).
  Server geometry covered by `test/freememo/live_doc_test.clj`.
- **Settings About.** `deploy.sh` exports `GIT_COMMIT` → Docker `:git-commit`
  uberjar arg → `build.clj` writes it into `electric-manifest.edn` →
  `freememo.config` reads it at boot (falling back to `"dev"` with no manifest).
- **Tablet viewport signals.** `!compact?` (≤900px) collapses hierarchy/right
  panels by default and `!coarse?` (`pointer:coarse`) suppresses composer/typeahead
  autofocus on touch, in `freememo.viewport`; `100dvh` keeps the pinned review bar
  on-screen.
- **Refactor pass** ("Audit v1–v3") - extracted a shared `freememo.modal-shell`,
  consolidated `number-stepper` and `settings-page`, and added `freememo.util`
  helpers; behavior-preserving.

## v20260712-cb16479

### For users

- **Overlapping cloze cards — one card type for ordered lists.** A new card
  kind for enumerations (steps, sequences, ranked lists) that reproduces the
  review presentation of Anki's Cloze Overlapper add-on without needing the
  add-on — each item becomes its own card, with the earlier items shown as
  context and the rest hidden.
  - Generate them with the LLM, or add and edit them by hand.
  - The top field is a **question** the list answers; the items are edited in a
    full rich-text editor (bold/italic, ordered and bullet lists, pasted images)
    just like the Basic and Cloze editors — no more one-line-per-item textarea.
  - Direction is automatic: an Arabic list renders right-to-left and an English
    one left-to-right, with no toggle to remember.
- **Spaced-repetition quiz (FSRS-6).** The Quiz tab now opens on **Review**: a
  live queue of every question due today, graded instantly, where each answer
  schedules its next review with the FSRS-6 algorithm — so review timing tracks
  what you actually recall instead of chance.
  - A daily cap keeps sessions bounded (20 new questions/day; reviews are
    effectively uncapped).
  - The old scope + count picker becomes **Custom quiz** — practice-only, and it
    never touches your schedule. Exams are unchanged.
- **The AI assistant now works with the rest of FreeMemo.**
  - **Capture a reply into cards** - one button turns the latest assistant reply
    straight into Basic cards.
  - Each message is grounded in the document's approved knowledge-graph facts,
    and still steers you with questions rather than reciting answers.
  - **@-mention other documents** in the composer (via a typeahead picker) to
    pull them into that message's context for the reply.
  - Empty chats offer starter prompts you can send with a click, and the
    composer re-focuses itself once a reply lands.
- **Anki note types renamed to a `FreeMemo …` prefix.** "Basic FreeMemo" becomes
  "FreeMemo Basic" (and the same for Cloze, Score, Image Occlusion, and
  Overlapping Cloze), so all five app-owned types sort together. Your existing
  notes are migrated onto the renamed types in place on the next push — note
  ids, tags, and review history are preserved.
- **Rebuilt formatting toolbar.** The bubble menu that pops up when you select
  text in the editor is a new custom toolbar (Notion-style): it no longer
  collides with the top command bar or covers the text you're reading, and it
  stays put while you scroll. Code blocks now have a language picker.
- **Keyboard shortcuts work off macOS.** Every shortcut was silently doing
  nothing on Windows and Linux; they now fire there too (`Cmd` maps to `Ctrl`,
  and the command palette is `Ctrl+Shift+P`, or `Ctrl+/` on Firefox).
- **Command palette reaches every tab.** Knowledge and Quiz are now openable
  from the palette (⌘K), alongside Home, Learn, Viewer, and Library.
- **The Help page shows your real shortcuts.** Each workflow step names the
  actual keyboard chord for your platform (⌘⇧E on macOS, `Ctrl+Shift+E`
  elsewhere) rather than a hard-coded key, so the instructions can no longer
  drift out of sync with what the keys actually do.
- **Undo / Actions modal shows card content.** A new **Card** column shows each
  entry's card text (cloze text or question, with a "+N more" for bulk
  deletes), so rows for deleted cards are no longer indistinguishable.
- Cloze deletions are now colored (red / light-blue in night mode) in the Anki
  cards FreeMemo pushes, matching every other cloze type.
- Bug fix: a bulk push from the Library cards view was overwriting each PDF's
  header with a stale, wrong global value, corrupting cards that had been pushed
  with the correct per-PDF header. Header now always resolves per-PDF; run one
  bulk push to re-apply your real headers.
- Bug fix: the code-block language picker was permanently hidden in production
  (a bug that only appeared in the optimized prod build); it works now.
- Bug fix: the ⋯ row-actions menu in the document list opened far from its
  button, mid-page; it now appears under the button where you clicked.
- Bug fix: hovering rows while scrolling could stack several stale tooltips over
  the list, and a tooltip could paint behind the rows below it; both fixed.
- Bug fix: the typeahead autocomplete dropdown rendered all its rows stacked on
  one line; rows are laid out correctly again.
- Bug fix: Arabic (and other right-to-left) text stayed left-aligned in every
  editor; it is now right-aligned throughout, and on pushed cards.
- Bug fix: the assistant echoed your message twice and could stall before
  replying; the echo now retires cleanly when the real reply arrives.

### Known issues

- **The note-type rename leaves the old models behind.** After the rename, the
  empty old-named note types (e.g. "Basic FreeMemo") stay in Anki — AnkiConnect
  offers no reliable way to delete a model, so remove them by hand if you want a
  clean list. Migrating a previously-synced note onto the renamed model also
  clears any fields FreeMemo doesn't own; your Remarks / Back Extra are kept and
  scheduling survives. By design.
- **Fixing the bulk-push header bug needs one corrective push.** The stale
  per-user global header rows are left inert in the database; a single bulk push
  after upgrading re-applies each PDF's real header. Contact me if a topic's
  header still looks wrong afterward.
- **FSRS scheduling isn't tunable from Settings yet.** Target retention, new
  cards/day, reviews/day, and interval fuzzing use fixed defaults for now (90%
  retention, 20 new/day, reviews uncapped, fuzz on). Contact me if you want
  these changed for your account.
- **Your existing quiz questions start fresh.** Every question that predates this
  release cold-starts as a *new* card and enters the queue through the daily
  new-card cap, so it may take a few days for a large backlog to fully surface.
- **Overlapping cloze doesn't pull back from Anki.** Editing an overlapping card
  in Anki won't sync back to FreeMemo — the Anki→list conversion is lossy, so
  pull is a deliberate no-op, same as Score cards. Edit these in FreeMemo.

### Technical

- **FSRS-6.** From-scratch port in `freememo.fsrs` (no new dependency), pinned to
  the py-fsrs reference by 303 conformance assertions over generated vectors
  (`test/freememo/fsrs_test.clj`, `fsrs_integration_test.clj`,
  `fsrs_vectors.edn`). Adds FSRS state columns to `kg_questions`, an append-only
  `kg_reviews` log (source of truth for daily caps + history), and supporting
  indexes; `db/draw-fsrs-due-queue` builds the learning→review→new queue.
  Grading was refactored into a session-less `grade-question!` core shared by the
  session quiz/exam path and the new Review path.
- **Overlapping cloze.** New `overlapping` JSONB column on `flashcards`;
  `freememo.overlapping/expand` purely derives the add-on's field layout
  (`Text1..TextN`, `Full` under `c21`, `Original`); clean-room "FreeMemo
  Overlapping Cloze" model (cloze-typed, 25 fields after dropping the Direction
  field, self-heals on push via `ensure-overlapping-model!`); new
  `resources/prompts/overlapping.md`.
- **Tooltip standardization.** A single `Tooltip!` wrapper (`freememo.tooltip`)
  owns the `data-tooltip` attribute and its aria coupling across ~60 call sites;
  `Icon` delegates to it. Hover z-index lift + instant hide fix the
  virtual-scroll stale/behind-row defects.
- **New CLJS modules.** `format_menu.cljs` (custom floating bubble toolbar
  replacing Quill's `.ql-tooltip`) and `code_lang_picker.cljs` (code-block
  language dropdown).
- **Prod build fix.** `^js` hint on the code-block line blot so shadow's
  `:infer-externs` keeps `.domNode` under advanced compilation (this was the
  cause of the picker being hidden only in prod), plus a nil-guard on the DOM
  node.
- **Note-type migration.** The push path reads each changed note's current model
  and field values in one `notesInfo` batch, field-updates notes already on the
  owned model, and `updateNoteModel`s foreign / old-named notes, re-supplying
  user-owned fields from current values.
- **Deployment.** Forgejo CI (`.forgejo/workflows/deploy.yml`) rebuilds and
  redeploys on every push to `unstable` by running `deploy.sh` on the box; a
  blue-green stack fronted by Caddy (`Caddyfile`) hot-reloads the active upstream
  so `:8080` stays bound and in-flight requests / live WebSockets survive a
  deploy. `docker-compose.prod.yml` reworked to match.
- **Shared chord display.** `commands/display-chord` resolves a command id +
  registry bind to its platform/browser display string; the Help page
  (`resolve-chords`, replacing `{command-id}` tokens) and the command palette now
  render from this one source, so the palette, the help text, and the key that
  actually fires can't diverge.

## v20260710-770a44e

### For users

- **AI assistant — a Socratic tutor for what you're reading.** A chat panel on
  the right side panel's new **AI Assistant** tab that helps you think a page
  through by asking questions rather than handing you answers.
  - Grounded in the page you're on; start a new chat per document, and your
    chats and their transcripts are saved.
  - Replies render as Markdown with real math — inline `$...$` and display
    `$$...$$` both typeset.
  - Pick the tutor's model per document from the panel, or leave it on your
    global default.
- **Generate cards from your knowledge graph.** When a document has approved
  facts, Generate now builds cards from those facts — a model picks the ones the
  current page supports — instead of raw page text; documents with no facts fall
  back to the old text path automatically.
- **Compare card-generation models side by side.** The Generate dropdown gains
  **Compare models**: run the same content through two or more models, see the
  candidate cards and each run's cost next to each other, and keep one.
- **FreeMemo now owns the Basic and Cloze Anki note types.** Like Score and
  Image Occlusion, Basic/Cloze are app-managed models, created and kept correct
  on every push, so cards always render the way FreeMemo intends.
  - The old Source and Bibliography fields collapse into one centered **Links**
    field — the citation stacked over an "Open in FreeMemo" link.
  - The per-type note-type pickers, the Field Defaults section, and the Card
    Stylesheet section are gone; the app manages all of that now.
- **Per-document models for card generation and the assistant.** Choose a card
  model (Document Options) or an assistant model (assistant panel) for one
  document without touching your global default; "Use my default" now names the
  model it resolves to, e.g. "Use my default (Gemini 3 Flash)".
- **A model per knowledge-graph step.** Settings exposes a model selector for
  each of the six KG steps — fact extraction, entity linking, atomic and
  synthesis questions, grading, and the card fact-selector.
- **Two more models to choose from.** Gemini 3.5 Flash and DeepSeek V4 Flash are
  now selectable for OCR and card generation.
- Bug fix: pushing Basic or Cloze cards to Anki crashed for everyone
  (`[object Object] is not ISeqable`); fixed.
- Bug fix: uploads between 10 and 100 MiB were rejected even when your quota
  allowed them; the real upload routes are no longer capped at 10 MiB.
- Bug fix: the format toolbar (bubble menu) is no longer clipped when your
  selection sits near the side panels.
- Bug fix: long lists (Library cards, the knowledge tree) no longer jump to the
  top when a row count changes under you — e.g. deleting a card — and no longer
  flicker at row boundaries while scrolling.

### Known issues

- **Comparing models costs credits per model.** Each model in a Compare run is a
  real, billed generation, so comparing N models spends credits N times. By
  design.
- **Cards from facts are not deduplicated or reviewed.** A fact can back both a
  quiz question and a card, and generated cards are inserted straight into the
  card table — prune the ones you don't want.
- **Your knowledge-graph work moves to Gemini 3 Flash.** Each KG step now
  defaults to Gemini 3 Flash rather than your card model, since these steps are
  high-volume and want a cheap, fast model. Choose a different model per step in
  Settings if you'd rather. Intended change.
- **The first Anki push migrates old Basic/Cloze notes.** A previously-pushed
  note on a foreign note type is moved onto the app-owned model on its next push,
  which clears fields the app doesn't own; your Remarks / Back Extra are
  preserved and scheduling survives. Customizing these note types in Anki is no
  longer supported.
- **The assistant's inline math can catch stray dollar signs.** Two `$` in one
  line render the text between them as math — acceptable for a math tutor, and
  plain numbers and money are handled so they don't trip it.
- **Per-user upload caps are bounded by a server ceiling.** An upload limit
  above `STORAGE_REQUEST_MAX_BYTES` is still capped there at the HTTP layer; if
  you self-host and want larger uploads, raise that env too. Contact me if you
  hit an upload limit on freememo.net.

### Technical

- **Assistant persistence.** New `assistant_chats` / `assistant_messages` tables
  (per-`(user_id, root_topic_id)` chats, cascade-deleted with the user and
  topic); server helpers in `freememo.assistant` / `freememo.db`; Socratic system
  prompt at `resources/prompts/assistant-socratic.md`.
- **Upload routing.** The `freememo.api` route table is now the single source of
  truth for both dispatch and body-size classification (`:upload`/`:small`); the
  divergent dev/prod whitelists that named deleted routes are removed. Adds the
  per-user `users.upload_max_bytes` column and `STORAGE_REQUEST_MAX_BYTES` as the
  absolute request-body ceiling.
- **Virtual scroll.** Vendored `freememo.scroll` (a copy of `electric-scroll0`
  with `:reset-key`) so in-place row-count changes no longer reset `scrollTop`;
  the reset key is derived from filter/sort state.
- **Memory / OOM.** `docker-compose.prod.yml` sets `mem_limit 3g`, sizes the JVM
  heap with `-XX:MaxRAMPercentage=70`, exits on OOM so `restart` recovers, dumps
  the heap on OOM, and persists `./logs` across rebuilds.
- **Anki sync observability.** Client-side sync exceptions are now logged
  server-side via `log-client-sync-error!` (message, source, browser stack, push
  context); the temporary payload-shape diagnostic is removed. The push crash
  was an un-awaited `m/?` inside a nested `fn` in `migrate-fields!`.
- **Enforced CSS.** The drift-check fetch of `freememo-anki.css` now uses
  `cache: no-store`, so a freshly deployed stylesheet is no longer silently
  reverted to a stale CDN copy on the next push.
- **Card-gen robustness.** `freememo.llm-edn` parse-error hardening with the
  repo's first test file (`test/freememo/llm_edn_test.clj`); new
  `resources/prompts/select-facts.md` prompt for the fact selector.

## v20260705-bfa0de2

### For users

- **Knowledge tab — a fact graph from your reading.** Distill a document into
  subject–predicate–object facts, then curate by exception.
  - Distill a document into entity-linked facts and generate atomic questions,
    in the background with a spinner and a cancel.
  - Fix things in four views: Facts (inline edit / relink), Entities (rename,
    merge, synthesize questions), Questions (edit, reject), Documents.
- **Quiz and Exam.** Test yourself against the fact graph.
  - Quiz - untimed, instant LLM grading with a correct/partial/incorrect
    verdict, explanation, your answer vs the reference, and missed facts with
    (Doc, p.N) provenance.
  - Exam - a frozen question draw with a server-anchored countdown; answers are
    forward-only and graded at submit.
  - Every entity in feedback links to a concept card — its fact neighborhood —
    so feedback becomes graph navigation. Sessions resume on reload.
- **Sheet-music (Score) cards.** Turn a score PDF plus a recording into
  Audio→Sheet / Sheet→Audio cards by selecting a waveform region and a notation
  rectangle.
- **Image-occlusion cards.** Draw mask rectangles over a pinned image (Hide-One
  or Hide-All), with six extra fields, and push them to Anki as image-occlusion
  notes.
- **Command palette (⌘K).** Fuzzy-search every action, scoped to what's
  available in the current view; keyboard shortcuts are registry-driven
  throughout.
- **Faster, decoupled actions.** Add-card and Anki push no longer block on the
  modal: Add-card closes immediately and shows a pending row that self-confirms;
  Push saves your settings, closes, and runs a background Quick Sync.
- **Three-way Anki sync scope.** Current-Page/Entire-Doc becomes
  **self / subtree / document**, resolved against the topic you're viewing.
  - Bug fix: syncing a nested non-PDF topic previously pushed all its siblings'
    cards; narrow scope now means exactly that topic.
- **Mobile reading-mode for Learn sessions.** On a phone mid-session the topic
  view drops the toolbars, side panels, and card table and gives content the
  full height, collapsing to just Extract + Add-Card; PDFs get a compact
  two-control bar with an overflow menu.
- **Code-aware card generation.** The editor selection is sent to the generator
  as HTML, so inline code and code blocks reach the model as code and come back
  styled in Anki.
- **Drag-and-drop nesting.** Re-parent topics by dragging them in the tree.
- **Choose and compare OCR.** Pick the OCR model (GPT-5.1, Gemini 3 Flash,
  Mistral OCR 4) per document or globally, and compare two engines side-by-side
  before keeping one.
- **Per-card bibliography.** Each card cites its own topic's source (or the
  nearest ancestor's), so extract cards carry the extract's citation.
- **Web import re-hosts images.** Imported articles copy their images onto
  FreeMemo instead of hotlinking, so they survive the source going away.
- **Help tab.** A new Help tab walks through the core workflows — Studying a PDF,
  Making a deck, Syncing to Anki, and Reviewing.
- **Self-hosting.** Choose the login mode (`password`, `google`, or `both`) and
  toggle the Secure-cookie flag for plain-HTTP LAN use.
- **Accessibility (WCAG 2.2 AA pass).** Keyboard-operable card-type radios,
  row-action buttons, topic-move mode, panel resize, and palette; see
  `ACCESSIBILITY.md`.
- Bug fix: newly created standalone topics were invisible in Library until an
  unrelated change; they now appear immediately.
- Bug fix: card generation failed when a model returned JSON instead of EDN
  (observed with Gemini 3 Flash); it now falls back to JSON.

### Known issues

- **Distilled facts are auto-approved.** With hundreds of facts per document,
  per-fact approval was unworkable — distillation lands facts as approved, so
  curate by exception (reject/edit in the Knowledge tab). Entity linking prefers
  creating a new entity when unsure, so duplicate entities can appear; merge them
  in Entities. A wrong merge is not auto-reversible.
- **Exam answers are forward-only**, and unanswered questions score zero — by
  design.
- **Virtual-scroll tables have limited screen-reader support.** The A11Y
  treatment on virtual scroll was reverted for now; see `ACCESSIBILITY.md`
  known limitations.

### Technical

- **Knowledge-graph substrate.** New `kg_entities` / `kg_predicates` /
  `kg_facts` / `kg_questions` / `kg_question_facts` / `kg_sessions` /
  `kg_answers`; a `pg_trgm` entity index; unique s/p/o indexes with
  reject-tombstones; `kg_extract` / `kg_questions` / `kg_grade` / `kg_llm`
  pipelines; a `:kg-mutations` channel. All lanes bill via credits.
- **Ontop SPARQL facade** (`ontop/`) publishes approved facts as read-only RDF
  over the `kg_*` tables. The endpoint has **no auth** — dev exposes `:8081`;
  prod/selfhost keep it on the internal network only.
- **Command architecture.** `freememo.commands` registry + `command-bus` + ⌘K
  palette with single-bump-authority invalidation; the hidden button-ref /
  `.click` shortcut indirection is deleted. New `:test` alias; `commands_test`
  (272 assertions).
- **doc-context refactor.** Ambient Electric dynamic vars
  (`freememo.doc-context`, ~49 vars) replace the large props maps threaded
  through the document tree — dodges the 64KB-per-method and 20-arg limits and
  gains per-var work-skipping.
- **Optimistic command queue** (`freememo.optimistic`): server-side,
  modal-decoupled effects; `insert-flashcards!` now RETURNs ids.
- **OpenRouter everywhere.** `freememo.openrouter` (clj-http) backs OCR, card
  generation, and transcription; removed `net.clojars.wkok/openai-clojure`;
  shared `freememo.llm-edn` EDN/JSON response parser.
- **Image re-hosting** (`freememo.image-rehost`): SSRF-guarded, throttled,
  size/count-capped; `web_import` and `media_migration` delegate to it.
- **Schema.** `topics.is_live`, `topic_files.role`; `occlusion_groups` and
  `score_groups` tables; `flashcards.{occlusion_group_id, mask_ordinal,
  io_fields, score_group_id, score_direction}`.
- **Changelog broadcast** (`freememo.changelog`): REPL `preview-broadcast` /
  `broadcast-latest!` render the newest release's `For users` + `Known issues`
  and send to opted-in users via SMTP BCC; `db/list-email-update-recipients`.
- **Config.** `:auth-mode`, `:cookie-secure?`, `:ocr-model-allowlist`,
  `:card-model-allowlist`, `:platform-openrouter-api-key`. Prod builds elide
  CLJS `:debug` (`taoensso.telemere.ct-min-level=:info`).
