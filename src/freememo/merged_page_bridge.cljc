(ns freememo.merged-page-bridge
  "The two-way link between focus reader's merged editor surface and the app's
   page state (plans/merged-page-reading.md §4.5, §5.1, §5.4).

   Two directions, and neither can be a plain function call:

   OUT — the page-break control's availability. `format-menu/install!` takes its
   opts once per editor construction and the document editor is persistent, so
   the mode cannot reach the control through opts. It reads
   `editor-actions/!page-break-context`, and this writes it.

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
   [freememo.editor-actions :as editor-actions]))

(e/defn MergedPageBridge
  "Publish merged-mode context to the editor, and relay its scroll report back.

   Pre : mounted inside the document editor's subtree, with dctx/merged?,
         dctx/merged-page-numbers, dctx/current-page and dctx/on-page-change!
         bound. Renders no DOM.
   Post: `!page-break-context` names the merged document's pages while the mode
         is on, and nil otherwise. A scroll to a new page reaches
         `on-page-change!` exactly once per page."
  []
  (e/client
    (let [merged? dctx/merged?
          merged-page-numbers dctx/merged-page-numbers
          current dctx/current-page
          on-page-change! dctx/on-page-change!]

      ;; OUT. A `reset!` is idempotent, so a reactive re-evaluation costs
      ;; nothing — the same shape as EditorPane's !document-base-url reset.
      (reset! editor-actions/!page-break-context
        (when merged? {:pages (vec merged-page-numbers)}))

      ;; IN. The equality guard is what makes this converge: calling the funnel
      ;; moves `current` to `scrolled`, after which the condition is false.
      (let [scrolled (e/watch editor-actions/!scrolled-page)]
        (when (and merged? (some? scrolled) (not= scrolled current) on-page-change!)
          (on-page-change! scrolled))))))
