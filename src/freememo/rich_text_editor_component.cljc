(ns freememo.rich-text-editor-component
  "Rich text editor UI component — persistent Quill instance.
   Follows the CodeMirror pattern: init once, update content in-place.
   No e/for-by — Quill survives page navigation without destroy/recreate."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.editor-actions :as editor-actions]
   [freememo.logging :as log]
   [freememo.merged-window :as merged-window]
   [freememo.rich-text-editor :as editor]))

(e/defn RichTextEditorComponent
  "Renders a Quill rich text editor that persists across page navigations.
   - Init once when first mounted (no destroy/recreate on topic-id change).
   - Updates content in-place via set-content! when initial-html changes.
   - The topic id in the editor state moves with the save scope, inside
     `install-body!`, so the text-change listener and a highlight tag an edit
     with the topic of the body on screen.
   - Skips in-place update if user has unsaved edits (don't damage user input).
   - `read-only?` is true while the editor still shows the previous topic's
     body after a topic change, and from a Learn tap on Next or Postpone until
     the next body is in (`editor-pane/editor-read-only?`). The editor takes no
     user edit for that time
     (plans/learn-next-speed/issues/02-editor-stays-mounted-across-next.md,
     plans/learn-next-speed/issues/10-edit-during-save-window.md)."
  [read-only?]
  (e/client
    (dom/div
      (dom/props {:class "quill-editor-wrapper"
                  :style {:border "1px solid var(--color-border)"
                          :border-radius "4px"
                          :background "var(--color-bg-card)"
                          :flex "1"
                          :display "flex"
                          :flex-direction "column"
                          :min-height "0"}
                  :data-role "widget"})

      (let [initial-html dctx/initial-html topic-id dctx/topic-id
            ;; The DOCUMENT in the editor, which merged reading makes distinct
            ;; from `topic-id` — see doc-context/editor-doc-id. Every decision
            ;; below is about the document, so all three use this.
            doc-id (or dctx/editor-doc-id topic-id)
            body-doc-id (if dctx/merged? dctx/merged-root-id dctx/effective-content-topic-id)
            ;; A merged body belongs to its root, even when scrolling changes
            ;; the current page. Page metadata must not reinstall that body.
            save-scope {:user-id dctx/user-id :topic-id body-doc-id
                        :editor-doc-id body-doc-id :merged? dctx/merged?
                        :pdf-root-id dctx/merged-root-id :source-root-id dctx/pdf-root-id
                        :page-numbers dctx/merged-page-numbers :requested-page dctx/current-page
                        :read-request dctx/merged-read-request
                        :expected-read-request dctx/merged-expected-read-request
                        :body-key (merged-window/body-key body-doc-id dctx/merged? dctx/merged-page-numbers)}
            node dom/node
            ;; Store timer-id in atom so the e/on-unmount closure is constant
            ;; (no reactive captures). A reactive closure causes Electric to
            ;; re-mount the on-unmount input, spuriously firing destroy-editor!.
            !timer-id (atom nil)]
        ;; Init once — Quill persists for the component's lifetime.
        (when (and (not (some? (:editor @editor/!editor-state)))
                   (or (not dctx/merged?)
                       (merged-window/snapshot-current? dctx/merged-read-request dctx/merged-expected-read-request)))
          (log/log-debug (str "Editor init doc-id=" doc-id " html-len=" (count initial-html)))
          ;; Electric can retain the initial nil state above. A changed save
          ;; scope can then schedule another callback during ordinary scrolling.
          ;; Read the live editor inside the callback before initializing it.
          (reset! !timer-id
            (js/setTimeout
              (fn []
                (when (and (.-isConnected node) (nil? (editor/current-editor)))
                  ;; The body's document, the same id as the save scope's,
                  ;; so the tag and the scope agree from the first build.
                  (editor/init-editor-with-scope! node (or initial-html "")
                    (or body-doc-id doc-id) save-scope)))
              0)))
        (e/on-unmount (fn []
                        (when-let [t @!timer-id] (js/clearTimeout t))
                        (editor/destroy-editor!)))

        ;; Read-only while the previous topic's body is still on screen, and
        ;; while a Learn Next or Postpone saves this topic's edit. The
        ;; topic id no longer moves here: `install-body!` moves it with the
        ;; save scope, so an edit in the load window cannot be tagged with the
        ;; next topic. `ready?`'s watch makes this run again once Quill exists.
        (editor/set-read-only! read-only? (e/watch editor/!editor-ready?))

        ;; Update content in-place when initial-html changes reactively
        ;; (page navigation, OCR scan, !refresh re-fetch).
        ;; Skip if user has unsaved edits — "don't damage user input".
        ;;
        ;; `ready?` is load-bearing. `set-content!` is a NO-OP before the editor
        ;; exists — `init-editor!` defers its build behind `math/on-katex-ready!`
        ;; — and nothing else re-runs this node, so a body that arrived first was
        ;; applied to nothing and never applied again. Watching !editor-state
        ;; makes the node re-evaluate the moment the editor is built.
        ;;
        ;; Measured: focus reader's merged document (13,930 chars) reached here
        ;; and the editor stayed empty, with no error anywhere. A per-page body
        ;; had hidden this by usually arriving after the build.
        (when (some? initial-html)
          (let [dirty (e/watch editor/!dirty-html)
                ready? (e/watch editor/!editor-ready?)
                saved-edit-id (e/watch editor/!saved-edit-id)]
            (when (and ready? (or (nil? dirty) (some? (:edit-id dirty))))
              (editor/install-body-after-work! initial-html body-doc-id
                save-scope saved-edit-id (e/watch editor-actions/!pending-editor-work-revision)))))))))
