(ns electric-starter-app.rich-text-editor-component
  "Rich text editor UI component — minimal wrapper, no reactive state."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]
    [electric-starter-app.rich-text-editor :as editor]))

(e/defn RichTextEditorComponent
  "Renders a rich text editor. Pure imperative widget — no callbacks, no reactive state.
   Props: {:initial-html <string> :page-number <int> :doc-id <int> :content-item-id <int|nil>}
   Always (re)creates the editor on the current DOM node. init-editor! is idempotent
   (destroys previous instance first)."
  [{:keys [initial-html page-number doc-id content-item-id]}]
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
            timer-id (js/setTimeout
                       (fn [] (editor/init-editor! node initial-html page-number doc-id
                                :content-item-id content-item-id))
                       0)]
        ;; Cleanup on unmount — cancel pending init and destroy editor
        (e/on-unmount
          (fn []
            (js/clearTimeout timer-id)
            (editor/destroy-editor!)))))))
