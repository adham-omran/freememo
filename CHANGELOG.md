# Changelog

<!--
Format contract (see freememo.changelog):
- Each release is one `## <version>` block, newest first.
- Up to three subsections per release: `### For users`, `### Known issues`,
  `### Technical`.
- The email/Discord broadcast sends `For users` + `Known issues` ONLY.
  `### Technical` never leaves the repo — put developer-facing notes there.
-->

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
