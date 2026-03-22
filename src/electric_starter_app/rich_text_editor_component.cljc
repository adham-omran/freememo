(ns electric-starter-app.rich-text-editor-component
  "Rich text editor UI component — minimal wrapper, no reactive state."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.logging :as log]
   [electric-starter-app.rich-text-editor :as editor]))

(e/defn RichTextEditorComponent
  "Renders a rich text editor. Remounts when topic-id changes (page/topic navigation).
   Waits for initial-html to be non-nil before initializing Quill.
   Once initialized, subsequent changes to initial-html are ignored (e.g., from !refresh bumps)."
  [{:keys [initial-html topic-id]}]
  (e/client
    (e/for-by identity [_k [[topic-id]]]
      (let [!initialized (atom false)]
        (dom/div
          (dom/props {:class "quill-editor-wrapper"
                      :style {:border "1px solid #ccc"
                              :border-radius "4px"
                              :background "#fff"
                              :flex "1"
                              :min-height "200px"}
                      :data-role "widget"})

          (let [node dom/node]
            ;; Wait for initial-html to arrive (may be stale/nil at frame creation),
            ;; then init Quill once. Subsequent initial-html changes are ignored.
            (when (not @!initialized)
              (reset! !initialized true)
              (log/log-debug (str "Editor init topic-id=" topic-id " html-len=" (count initial-html)))
              (js/setTimeout (fn [] (editor/init-editor! node (or initial-html "") topic-id)) 0))
            (e/on-unmount
              (fn []
                (editor/destroy-editor!)))))))))
