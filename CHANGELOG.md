# Changelog

<!--
Format contract (see freememo.changelog):
- Each release is one `## <version>` block, newest first.
- Up to three subsections per release: `### For users`, `### Known issues`,
  `### Technical`.
- The email/Discord broadcast sends `For users` + `Known issues` ONLY.
  `### Technical` never leaves the repo — put developer-facing notes there.
-->

## Unreleased

### For users

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

- **Math.** Formulas work end to end — write them in the editor, generate cards from
  them, and push them to Anki where they render as real notation instead of raw LaTeX.
  - **Write `\(x^2\)` and it becomes a formula** the moment you type the closing
    `\)`. No button, no dialog — the delimiters are the whole gesture, in the
    document editor and in every card field.
  - **Click a formula to edit it.** A small panel opens above it with the LaTeX, a
    live preview that updates as you type, and Delete. Enter or clicking away saves;
    Escape leaves it alone; clearing the field removes the formula.
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

### Known issues

- **A `\(`…`\)` pair inside a code block is treated as math when pushing to Anki.**
  It renders as a formula in Anki instead of as source. Nothing is lost — the next
  pull restores it — but Clojure's `\(` character literal is the realistic way to hit
  this. In the editor itself, code blocks are excluded: typing `\(x\)` in one leaves
  it as source.
- **Writing prose *about* LaTeX can produce an unwanted formula.** Typing `\(` and
  later `\)` in ordinary text converts on the closing delimiter. Ctrl+Z undoes it.
- **A long formula can be cut mid-expression** in a search snippet or a card-table
  row, where it then shows as LaTeX source rather than rendering.
- **If the KaTeX CDN is unreachable,** the editor still opens after a 3-second wait,
  but math stays as `\(…\)` source and clicking it does nothing. Your content is
  unchanged either way.

### Technical

- `freememo.math` is the single owner of the stored form (`\(TeX\)` inline text) and
  of all four boundary conversions. See `plans/math-support.md` and the CLAUDE.md
  pattern entry; the delimiter form was chosen specifically so `clean-html` needs no
  allow-list change and `strip-html` carries math into `content_text` for search.
- Every read of a Quill root's innerHTML now goes through `quill-field/editor-html`
  (seven sites). A raw read persists the KaTeX subtree, which `clean-html` reduces to
  duplicated fallback text.
- `init-editor!` is asynchronous now — it waits on `math/on-katex-ready!` (a 3 s race
  against `window.__katexReady`, which never resolves on a blocked CDN) and is guarded
  by a generation counter. It returns nil; read `!editor-state`.
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
