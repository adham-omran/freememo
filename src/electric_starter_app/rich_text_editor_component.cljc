(ns electric-starter-app.rich-text-editor-component
  "Rich text editor UI component — minimal wrapper, no reactive state."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.rich-text-editor :as editor]))

(e/defn RichTextEditorComponent
  "Renders a rich text editor. Remounts when topic-id changes (page/topic navigation).
   initial-html is captured once on mount — subsequent reactive changes are ignored
   so that !refresh bumps don't reinitialize the editor."
  [{:keys [initial-html topic-id]}]
  (e/client
    (e/for-by identity [_k [[topic-id]]]
      ;; Capture initial-html once on mount via plain atom deref (not e/watch)
      (let [!captured-html (atom initial-html)]
        (dom/div
          (dom/props {:class "quill-editor-wrapper"
                      :style {:border "1px solid #ccc"
                              :border-radius "4px"
                              :background "#fff"
                              :flex "1"
                              :min-height "200px"}
                      :data-role "widget"})

          (let [node dom/node
                timer-id (js/setTimeout
                           (fn [] (editor/init-editor! node @!captured-html topic-id))
                           0)]
            ;; Cleanup on unmount — cancel pending init and destroy editor
            (e/on-unmount
              (fn []
                (js/clearTimeout timer-id)
                (editor/destroy-editor!)))))))))
