(ns freememo.merged-page-bridge
  "The two-way link between focus reader's merged editor surface and the app's
   page state (plans/merged-page-reading.md §4.5, §5.1, §5.4).

   Three directions, and none can be a plain function call:

   OUT — the page-break control's availability. `format-menu/install!` takes its
   opts once per editor construction and the document editor is persistent, so
   the mode cannot reach the control through opts. It reads
   `editor-actions/!page-break-context`, and this writes it.

   ARRIVE — the page the reader is returned to after the editor installs a
   body. Every install puts the editor at scrollTop 0, which in this mode is
   page 1 whatever page was asked for. Four entrances reach that: a deep link
   at `/viewer/topic/<root>/<page>`, turning the mode on while reading,
   reopening the document in-app, and an OCR scan finishing mid-read. They share
   one cause, so the trigger is `rich-text-editor/!content-generation` — the
   install itself — and not a mount hook, which would have covered one of the
   four.

   IN — the page the reader has scrolled to. The scroll listener lives with the
   Quill instance and writes `editor-actions/!scrolled-page`; this watches that
   and calls `on-page-change!`, the app's one page-change funnel. So a scroll
   records the page as current AND requests the last-page write, exactly as the
   toolbar arrows do.

   ITS OWN NAMESPACE, not two blocks inside EditorPane. Adding them there pushed
   that e/defn past the JVM's 64KB-per-method cap — a `:prod`-only failure whose
   reported line names the file's first e/defn and tells you nothing. A
   namespace boundary is a real frame boundary, so this method is its own."
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.doc-context :as dctx]
   [freememo.editor-actions :as editor-actions]
   [freememo.rich-text-editor :as editor]))

(e/defn MergedPageBridge
  "Publish merged-mode context to the editor, and relay its scroll report back.

   Pre : mounted inside the document editor's subtree, with dctx/merged?,
         dctx/merged-page-numbers, dctx/current-page, dctx/editor-doc-id and
         dctx/on-page-change! bound. Renders no DOM.
   Post: `!page-break-context` names the merged document's pages while the mode
         is on, and nil otherwise.
   Post: after each body install the editor shows `current-page`, and the funnel
         holds whichever page the editor actually landed on.
   Post: a scroll to a new page reaches `on-page-change!` exactly once per page,
         and a scroll reported for another document reaches it never."
  []
  (e/client
    (let [merged? dctx/merged?
          merged-page-numbers dctx/merged-page-numbers
          current dctx/current-page
          doc-id dctx/editor-doc-id
          on-page-change! dctx/on-page-change!]

      ;; OUT. `e/drain` runs this for its effect and returns the empty amb, so
      ;; the write is not a let-body value Electric may skip — CLAUDE.md's
      ;; "Every Expression in a `let` Body Must Be Used". A `reset!` is
      ;; idempotent, so a reactive re-evaluation costs nothing.
      (e/drain
       (reset! editor-actions/!page-break-context
         (when merged? {:pages (vec merged-page-numbers)})))

      ;; ARRIVE. `!content-generation` is the trigger, so this fires once per
      ;; body install and not once per mount.
      ;;
      ;; The once-per-generation guard lives in `restore-page-on-install!`, a
      ;; plain fn — Electric re-evaluates this body unpredictably, and a guard
      ;; written here would either re-scroll the reader or fail to scroll at
      ;; all. It keys on the GENERATION alone: a page change with no new install
      ;; means the toolbar or the reader already moved, and pulling them back
      ;; would fight them.
      ;;
      ;; `current-editor` is passed as a FN and called inside that plain fn.
      ;; Calling it here instead — `(editor/current-editor)` — is what this
      ;; shipped as and it never worked: a zero-argument call has no inputs, so
      ;; Electric evaluates it once and holds the result, and the result was nil
      ;; from before the editor was built. Measured: the sibling `e/watch` fired
      ;; for generation 0 and 1 while the call answered nil both times.
      ;;
      ;; The generation is bumped after `!editor-state` is set, so an evaluation
      ;; woken by it always finds an editor. One woken by anything else finds
      ;; nil, restores nothing and records nothing, so the next generation still
      ;; gets its turn.
      ;;
      ;; `landed` is the page the editor IS showing, which differs from `current`
      ;; only when the marker was gone. Feeding it back keeps the page field and
      ;; the URL honest about what is on screen. `e/drain` for the reason above.
      (e/drain
       (let [generation (e/watch editor/!content-generation)
             landed (when merged?
                      (editor-actions/restore-page-on-install!
                        generation editor/current-editor current))]
         ;; The restore has no DOM of its own, so a log is the only way to watch
         ;; it. That log lives inside `restore-page-on-install!` rather than
         ;; here: a `js/…` symbol in an `e/defn` body is a shape this codebase
         ;; has nowhere else, and the plain fn is where its two neighbours
         ;; (`insert-cloze!`, `pdf-cache`) already keep theirs.
         (when (and landed on-page-change! (not= landed current))
           (on-page-change! landed))))

      ;; IN. The equality guard is what makes this converge: calling the funnel
      ;; moves `current` to the reported page, after which the condition is
      ;; false.
      ;;
      ;; The `:doc-id` test is what stops a report leaking between documents.
      ;; `!scrolled-page` is a `defonce` that outlives every editor and that no
      ;; teardown clears, so without it a page read in one document is applied
      ;; to the next one opened in-app — see that var.
      (let [scrolled (e/watch editor-actions/!scrolled-page)
            page (:page scrolled)]
        (when (and merged? (some? page) (= (:doc-id scrolled) doc-id)
                (not= page current) on-page-change!)
          (on-page-change! page))))))
