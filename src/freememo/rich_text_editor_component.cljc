(ns freememo.rich-text-editor-component
  "Rich text editor UI component — persistent Quill instance.
   Follows the CodeMirror pattern: init once, update content in-place.
   No e/for-by — Quill survives page navigation without destroy/recreate."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.rich-text-editor :as editor]))

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
            ;; Store timer-id in atom so the e/on-unmount closure is constant
            ;; (no reactive captures). A reactive closure causes Electric to
            ;; re-mount the on-unmount input, spuriously firing destroy-editor!.
            !timer-id (atom nil)]
        ;; Init once — Quill persists for the component's lifetime.
        (when-not (some? (:editor @editor/!editor-state))
          (log/log-debug (str "Editor init topic-id=" topic-id " html-len=" (count initial-html)))
          (reset! !timer-id (js/setTimeout (fn [] (editor/init-editor! node (or initial-html "") topic-id)) 0)))
        (e/on-unmount (fn []
                        (when-let [t @!timer-id] (js/clearTimeout t))
                        (editor/destroy-editor!)))

        ;; Update topic-id in editor state (page navigation)
        ;; Text-change listener reads from !editor-state, so dirty-html gets correct topic-id
        (editor/update-topic-id! topic-id)

        ;; Update content in-place when initial-html changes reactively
        ;; (page navigation, OCR scan, !refresh re-fetch)
        ;; Skip if user has unsaved edits — "don't damage user input"
        (when (some? initial-html)
          (let [dirty (e/watch editor/!dirty-html)]
            (when-not (and dirty (= (:topic-id dirty) topic-id))
              (editor/set-content! initial-html))))))))
