(ns electric-starter-app.rich-text-editor-component
  "Rich text editor UI component — minimal wrapper, no reactive state."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]
    [electric-starter-app.rich-text-editor :as editor]))

(e/defn RichTextEditorComponent
  "Renders a rich text editor. Pure imperative widget — no callbacks, no reactive state.
   Props: {:initial-html <string>}
   The editor is created once. When initial-html changes reactively, content is
   updated in-place via set-content! — no teardown/rebuild."
  [{:keys [initial-html]}]
  (e/client
    (dom/div
      (dom/props {:class "quill-editor-wrapper"
                  :style {:border "1px solid #ccc"
                          :border-radius "4px"
                          :background "#fff"}
                  :data-role "widget"})

      (let [node dom/node]
        ;; Initialize or update: if no editor exists yet, create one.
        ;; If editor already exists (page changed), just update content.
        (js/setTimeout
          (fn []
            (if @editor/!editor-state
              (editor/set-content! initial-html)
              (editor/init-editor! node initial-html)))
          200))

      ;; Cleanup on unmount
      (e/on-unmount editor/destroy-editor!))))
