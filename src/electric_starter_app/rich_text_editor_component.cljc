(ns electric-starter-app.rich-text-editor-component
  "Rich text editor UI component — persistent Quill instance.
   Follows the CodeMirror pattern: init once, update content in-place.
   No e/for-by — Quill survives page navigation without destroy/recreate."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.logging :as log]
   [electric-starter-app.rich-text-editor :as editor]))

(e/defn RichTextEditorComponent
  "Renders a Quill rich text editor that persists across page navigations.
   - Init once when first mounted (no destroy/recreate on topic-id change).
   - Updates content in-place via set-content! when initial-html changes.
   - Updates topic-id in editor state so text-change listener tags dirty-html correctly.
   - Skips in-place update if user has unsaved edits (don't damage user input)."
  [{:keys [initial-html topic-id]}]
  (e/client
    (dom/div
      (dom/props {:class "quill-editor-wrapper"
                  :style {:border "1px solid #ccc"
                          :border-radius "4px"
                          :background "#fff"
                          :flex "1"
                          :min-height "200px"}
                  :data-role "widget"})

      (let [node dom/node
            ;; Init once — Quill persists for the component's lifetime.
            ;; Capture timer-id so we can cancel on unmount (prevents ghost editor
            ;; on detached DOM when Electric processes atom resets as separate waves).
            timer-id (when-not (some? (:editor @editor/!editor-state))
                       (log/log-debug (str "Editor init topic-id=" topic-id " html-len=" (count initial-html)))
                       (js/setTimeout (fn [] (editor/init-editor! node (or initial-html "") topic-id)) 0))]
        (e/on-unmount (fn []
                        (when timer-id (js/clearTimeout timer-id))
                        (editor/destroy-editor!)))

        ;; Update topic-id in editor state (page navigation)
        ;; Text-change listener reads from !editor-state, so dirty-html gets correct topic-id
        (editor/update-topic-id! topic-id)

        ;; Update content in-place when initial-html changes reactively
        ;; (page navigation, OCR scan, !refresh re-fetch)
        ;; Skip if user has unsaved edits — "don't damage user input"
        ;; e/watch on !editor-state ensures re-trigger after init-editor! creates the editor
        (when (some? initial-html)
          (let [dirty (e/watch editor/!dirty-html)
                editor-state (e/watch editor/!editor-state)]
            (when (and (some? editor-state)
                       (not (and dirty (= (:topic-id dirty) topic-id))))
              (editor/set-content! initial-html))))))))
