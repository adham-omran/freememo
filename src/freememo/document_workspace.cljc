(ns freememo.document-workspace
  "Viewer workspace layout below the document toolbars."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.desktop-section :as desktop-section]
   [freememo.document-view :refer [DocumentColumns]]
   [freememo.right-side-panel :refer [RightSidePanel]]
   [freememo.bottom-panel :refer [BottomPanel]]
   [freememo.command-bus :as bus]
   [freememo.topic-kinds :as topic-kinds]
   [freememo.tooltip :as tooltip]
   [freememo.util :as util]))

(e/defn DocumentSplitDivider []
  (e/client
    (let [top-pct dctx/top-pct
          !top-pct dctx/!top-pct
          !top-pct-save dctx/!top-pct-save
          reset-split! dctx/reset-split!]
      (dom/div
        (dom/props {:class "split-divider-v"
                    :role "separator" :aria-orientation "horizontal"
                    :aria-label "Resize content and card table" :tabindex "0"
                    :aria-valuenow (str (int (or top-pct 0)))})
        (tooltip/Tooltip! "Drag to resize panels (double-click to reset)")
        (dom/On "pointerdown"
          (fn [event]
            (util/start-drag! event :y !top-pct
              {:on-commit #(reset! !top-pct-save %)}))
          nil)
        (dom/On "keydown"
          (fn [event]
            (util/key-resize-pct! event :y !top-pct
              #(reset! !top-pct-save %)))
          nil)
        (dom/On "dblclick" (fn [_] (reset-split!)) nil)))))

(e/defn DocumentMainColumn [assistant-inside?]
  (e/client
    (let [;; Forge structure and a Google Doc root have no cards, so no card
          ;; table and no toggle for one (plans/repository-home.md §4.1.2).
          structure? (topic-kinds/home-kind? dctx/kind)
          card-table-open? (and dctx/card-table-open? (not structure?))
          toggle-card-table! (when-not structure? dctx/toggle-card-table!)
          ;; Published only on a document with a source pane, so the palette
          ;; lists :toggle-editor-pane nowhere else (its preparer reads it).
          toggle-editor-pane! (when dctx/has-source-pane? dctx/toggle-editor-hidden!)
          editor-hidden? dctx/editor-hidden?]
      (bus/publish-ctx! {:toggle-card-table! toggle-card-table!})
      (e/on-unmount #(bus/retract-ctx! [:toggle-card-table!]))
      ;; :export-topic-cards exports this topic and its descendants: the whole
      ;; document in a PDF, else the topic on screen. Not dctx/topic-id — this
      ;; column is outside both document_view frames that bind it, so it reads
      ;; nil here and the palette entry never shows.
      (bus/publish-ctx! {:export-topic-id (or dctx/pdf-root-id dctx/page-topic-id)})
      (e/on-unmount #(bus/retract-ctx! [:export-topic-id]))
      (bus/publish-ctx! {:toggle-editor-pane! toggle-editor-pane!
                         :editor-hidden? editor-hidden?})
      (e/on-unmount #(bus/retract-ctx! [:toggle-editor-pane! :editor-hidden?]))
      (dom/div
        (dom/props {:class "document-workspace__main"})
        (DocumentColumns assistant-inside? card-table-open?)
        (when card-table-open?
        (DocumentSplitDivider))
        (when card-table-open?
          (BottomPanel))))))

(e/defn DocumentWorkspace [assistant-full-height?]
  (e/client
    ;; A desktop local account has no assistant (15.4.4). A poem keeps the
    ;; panel for its الشرح tab.
    (if (and assistant-full-height?
          (or dctx/is-poem? (e/server (desktop-section/account-ai?* dctx/user-id))))
      (dom/div
        (dom/props {:class "document-workspace document-workspace--assistant-full"})
        (DocumentMainColumn false)
        (RightSidePanel dctx/page-topic-id dctx/root-topic-id dctx/user-id dctx/is-poem?))
      (DocumentMainColumn true))))
