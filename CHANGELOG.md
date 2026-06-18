# WIP

Integration of four in-progress feature branches (`undo`, `audio`,
`side-panels-resize`, `ux-quality`) on the `experimental` branch.

## Added

- **Audio import & transcription.** Upload an audio file as a topic
  (`mp3`, `m4a`, `wav`, `webm`, `ogg`, `flac`; 25 MB cap) via a new Audio
  source in the import modal, served back through `GET /api/audio/:id`. An
  inline audio player sits above the editor, and a Transcribe button runs
  Whisper to replace the editor content with the transcript (warns before
  overwriting existing text). New `transcribe.clj` / `audio.clj` and a
  per-user `:transcribing-topics` state channel.
- **Undo system.** Per-user undo log with pruning, covering flashcard and
  pin mutations. Undo the newest action from a toast, or browse and undo
  individual actions in a new Undo History modal. New `undo.clj` /
  `undo_history_modal.cljc`, an `:undo-mutations` state channel, and a
  keyboard shortcut.
- **Resizable side panels.** The hierarchy and pin side panels are now
  drag-resizable, with widths persisted per (user, topic).
- **Escape-to-close for modals.** Shared `ModalEscape` helper
  (`modal_shell.cljc`) closes any modal on Escape; wired into the import,
  bibliography, history, zotero-picker, and library-card modals.

## Changed

- **User-configurable card markup.** Markup is resolved from a per-user
  setting (`resolve-markup` / `get-user-markup` / `set-user-markup!`).
- Assorted modal, card-table, and Quill-field UX refinements.
- One-time backfill of PDF topic sources (`backfill-pdf-sources!`).

## Fixed

- **Modal crash from a frame-shape mismatch.** `ModalEscape` carried a
  `#?(:cljs …)` reader conditional inside its `e/defn` body, compiling
  different signal counts for client vs server and crashing the WebSocket
  on every modal open. Reduced to a plain keydown listener; focus-on-mount
  moved to `:autofocus` on each caller.
- **Audio topic crash from an unregistered state channel.** `get-atom` had
  no `:transcribing-topics` clause (and no default), so mounting the
  Transcribe button threw `No matching clause`. Registered the channel.
- **Undo toasts collapsing into one.** Undo toasts share identical
  `[level message]` text, so the toast queue's dedup replaced the prior
  toast in place and only the newest delete stayed undoable from a toast.
  Added a `:dedup?` opt-out so each undo toast stacks separately.
- **Modals not keyboard-accessible.** Modals opened on click never received
  focus (`:autofocus` does not fire on dynamically inserted elements), and
  Tab leaked to the page behind them. Modals now focus on mount via a
  frame-safe rAF helper and trap Tab/Shift-Tab within their focusable
  elements (document delete-confirm, import, bibliography, history, etc.).

## Known Issues

- **Learn-session WebSocket crash (frame-shape mismatch).** Visiting
  `/viewer/learn-session` can crash the WS connection with
  `ArrayIndexOutOfBoundsException: Index 18 out of bounds for length 16`.
  Suspected cause (unconfirmed): client/server build skew — the client
  carries reactive nodes (e.g. `ActionsNavButton`'s open-state `e/watch`,
  added in the Actions-pill restyle) that a stale server JVM lacks. Pending
  a full server restart + hard-refresh to distinguish skew from a genuine
  source divergence. Workaround: restart the dev server JVM after merges.
