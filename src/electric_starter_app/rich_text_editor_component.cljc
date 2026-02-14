(ns electric-starter-app.rich-text-editor-component
  "Rich text editor UI component using Quill."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]
    [electric-starter-app.rich-text-editor :as editor]))

(e/defn RichTextEditorComponent
  "Renders a rich text editor with the given HTML content.
   Props: {:initial-html <string>, :on-change <fn>}
   Returns: The current HTML content."
  [{:keys [initial-html on-change]}]
  (e/client
    (println "[RichTextEditorComponent] Rendering with initial-html:" initial-html)
    (let [!editor-html (atom (or initial-html ""))
          editor-html (e/watch !editor-html)
          !container (atom nil)]

      ;; Editor container - Quill will create its own toolbar
      (dom/div
        (dom/props {:class "quill-editor-wrapper"
                    :style {:border "1px solid #ccc"
                            :border-radius "4px"
                            :background "#fff"}
                    :data-role "widget"})
        (reset! !container dom/node)

        ;; Initialize Quill editor after DOM element exists
        (js/setTimeout
          (fn []
            (println "[Component] Attempting to initialize editor")
            (println "[Component] Container:" @!container)
            (println "[Component] Initial HTML:" initial-html)
            (when @!container
              (editor/init-editor!
                @!container
                initial-html
                (fn [html]
                  (println "[Component] Content changed, length:" (count html))
                  (reset! !editor-html html)
                  (when on-change (on-change html))))))
          200))

      ;; Return current HTML for parent component
      editor-html)))
