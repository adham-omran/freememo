(ns freememo.rich-text-editor-component
  "Rich text editor UI component — persistent Quill instance.
   Follows the CodeMirror pattern: init once, update content in-place.
   No e/for-by — Quill survives page navigation without destroy/recreate."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.logging :as log]
   [freememo.rich-text-editor :as editor]))

(e/defn RichTextEditorComponent
  "Renders a Quill rich text editor that persists across page navigations.
   - Init once when first mounted (no destroy/recreate on topic-id change).
   - Updates content in-place via set-content! when initial-html changes.
   - Updates topic-id in editor state so text-change listener tags dirty-html correctly.
   - Skips in-place update if user has unsaved edits (don't damage user input)."
  []
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
            node dom/node
            ;; Store timer-id in atom so the e/on-unmount closure is constant
            ;; (no reactive captures). A reactive closure causes Electric to
            ;; re-mount the on-unmount input, spuriously firing destroy-editor!.
            !timer-id (atom nil)]
        ;; Init once — Quill persists for the component's lifetime.
        (when-not (some? (:editor @editor/!editor-state))
          (log/log-debug (str "Editor init doc-id=" doc-id " html-len=" (count initial-html)))
          (reset! !timer-id (js/setTimeout (fn [] (editor/init-editor! node (or initial-html "") doc-id)) 0)))
        (e/on-unmount (fn []
                        (when-let [t @!timer-id] (js/clearTimeout t))
                        (editor/destroy-editor!)))

        ;; Tag dirty-html with the DOCUMENT, not the page. The text-change
        ;; listener reads this, and EditorPane's save gate compares against it —
        ;; in merged mode a scroll moves the page, and tagging by page would
        ;; make that scroll discard the reader's unsaved edit.
        (editor/update-topic-id! doc-id)

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
                ready? (e/watch editor/!editor-ready?)]
            (when (and ready? (not (and dirty (= (:topic-id dirty) doc-id))))
              (editor/set-content! initial-html))))))))
