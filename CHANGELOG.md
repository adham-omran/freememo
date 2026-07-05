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

- **Audio import & transcription.** Upload an audio file as a topic
  (`mp3`, `m4a`, `wav`, `webm`, `ogg`, `flac`; 25 MB cap) via a new Audio
  source in the import modal. An inline audio player sits above the editor,
  and a Transcribe button runs Whisper to replace the editor content with
  the transcript (it warns before overwriting existing text).
- **Undo system.** Undo the newest action from a toast, or browse and undo
  individual actions in a new Undo History modal — covering flashcard and
  pin mutations, with a keyboard shortcut.
- **Resizable side panels.** The hierarchy and pin side panels are now
  drag-resizable, with widths remembered per document.
- **Escape-to-close for modals.** Any modal now closes on Escape, and Tab
  stays trapped inside it — wired into the import, bibliography, history,
  Zotero-picker, and library-card modals.
- **User-configurable card markup.** Card markup is now resolved from a
  per-user setting.
- Assorted modal, card-table, and Quill-field UX refinements.
- Bug fix: audio topics no longer crash on open.
- Bug fix: each undo toast now stacks separately instead of collapsing into
  one, so every recent delete stays undoable.
- Bug fix: modals now receive focus on open and no longer let Tab leak to
  the page behind them.

### Known issues

- **Learn-session crash after merges.** Visiting the Learn session can, after
  a code merge, crash the live connection until the dev server is restarted.
  Workaround: restart the dev server JVM after merges. (Contact me if you hit
  this in a deployed build.)

### Technical

- One-time backfill of PDF topic sources (`backfill-pdf-sources!`).
- Audio/undo plumbing: new `transcribe.clj` / `audio.clj` / `undo.clj` /
  `undo_history_modal.cljc`; `:transcribing-topics` and `:undo-mutations`
  state channels; `GET /api/audio/:id`.
- **Modal crash from a frame-shape mismatch (fixed).** `ModalEscape` carried
  a `#?(:cljs …)` reader conditional inside its `e/defn` body, compiling
  different signal counts for client vs server and crashing the WebSocket on
  every modal open. Reduced to a plain keydown listener; focus-on-mount moved
  to a frame-safe rAF helper.
- **Audio topic crash (fixed).** `get-atom` had no `:transcribing-topics`
  clause; registered the channel.
- **Undo toasts collapsing (fixed).** Undo toasts shared identical
  `[level message]` text, so the toast queue's dedup replaced the prior toast
  in place. Added a `:dedup?` opt-out.
