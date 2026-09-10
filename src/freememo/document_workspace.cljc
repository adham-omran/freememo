(ns freememo.document-workspace
  "Viewer workspace layout below the document toolbars."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.document-view :refer [DocumentColumns]]
   [freememo.right-side-panel :refer [RightSidePanel]]
   [freememo.bottom-panel :refer [BottomPanel]]
   [freememo.command-bus :as bus]
   [freememo.util :as util]))

(e/defn DocumentSplitDivider []
  (e/client
    (let [top-pct dctx/top-pct
          !top-pct dctx/!top-pct
          !top-pct-save dctx/!top-pct-save
          reset-split! dctx/reset-split!]
      (dom/div
        (dom/props {:class "split-divider-v"
                    :title "Drag to resize panels (double-click to reset)"
                    :role "separator" :aria-orientation "horizontal"
                    :aria-label "Resize content and card table" :tabindex "0"
                    :aria-valuenow (str (int (or top-pct 0)))})
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
    (let [card-table-open? dctx/card-table-open?
          toggle-card-table! dctx/toggle-card-table!]
      (bus/publish-ctx! {:toggle-card-table! toggle-card-table!})
      (e/on-unmount #(bus/retract-ctx! [:toggle-card-table!]))
      (dom/div
        (dom/props {:class "document-workspace__main"})
        (DocumentColumns assistant-inside? card-table-open?)
        (when card-table-open?
        (DocumentSplitDivider))
        (when card-table-open?
          (BottomPanel))))))

(e/defn DocumentWorkspace [assistant-full-height?]
  (e/client
    (if assistant-full-height?
      (dom/div
        (dom/props {:class "document-workspace document-workspace--assistant-full"})
        (DocumentMainColumn false)
        (RightSidePanel dctx/page-topic-id dctx/root-topic-id dctx/user-id))
      (DocumentMainColumn true))))
