# Changelog

<!--
Format contract (see freememo.changelog):
- Each release is one `## <version>` block, newest first.
- Up to three subsections per release: `### For users`, `### Known issues`,
  `### Technical`.
- The email/Discord broadcast sends `For users` + `Known issues` ONLY.
  `### Technical` never leaves the repo — put developer-facing notes there.
-->

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
