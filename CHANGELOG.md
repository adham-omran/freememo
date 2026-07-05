# Changelog

<!--
Format contract (see freememo.changelog):
- Each release is one `## <version>` block, newest first.
- Up to three subsections per release: `### For users`, `### Known issues`,
  `### Technical`.
- The email/Discord broadcast sends `For users` + `Known issues` ONLY.
  `### Technical` never leaves the repo — put developer-facing notes there.
-->

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
