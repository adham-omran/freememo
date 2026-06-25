(ns freememo.editor-toolbar
  "Unified top-bar slot for the document editor's Quill formatting toolbar.

   The Quill instance (a singleton in freememo.rich-text-editor) builds its own
   `.ql-toolbar` DOM from its module config; init-editor! relocates that node
   into the stable, inert div this component renders, so the formatting controls
   sit in the top toolbar stack alongside ContentToolbar and PdfToolbar.

   This component owns ONLY the empty host div. Its interior is owned by Quill —
   it must never re-render reactively, or Quill's handlers would dangle."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.rich-text-editor :as editor]))

(e/defn EditorToolbar
  "Renders the inert host div for the relocated Quill toolbar and publishes its
   node to editor/!toolbar-node for init-editor! to fill. Empty until Quill
   mounts (~one tick after the editor's content fetch)."
  []
  (e/client
    (dom/div
      (dom/props {:class "editor-toolbar-bar"
                  ;; data-role widget: Electric leaves the interior alone so
                  ;; Quill can own the relocated .ql-toolbar children. NOT the
                  ;; .toolbar class — its nowrap/overflow-clip would hide Quill's
                  ;; wrapping rows (E1: Quill keeps its own wrapping).
                  :data-role "widget"
                  ;; min-height reserves one toolbar row, avoiding a layout jump
                  ;; between mount and Quill filling it.
                  :style {:min-height "40px"}})
      (reset! editor/!toolbar-node dom/node)
      (e/on-unmount (fn [] (reset! editor/!toolbar-node nil))))))
