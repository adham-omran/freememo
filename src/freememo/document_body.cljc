(ns freememo.document-body
  "TopicPage's render body (layout-persist + the document layout column tree),
   in its own namespace so its cross-ns calls to document-view components are
   NOT inlined (Electric inlines same-namespace e/defn calls, which is what
   pushed TopicPage past the JVM 64KB-per-method limit). TopicPage does data
   resolution and hands the resolved values in as a prop map."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.document-view :refer [DocumentColumns DocumentToolbars]]
   [freememo.bottom-panel :refer [BottomPanel]]
   [freememo.util :as util]
   #?(:clj [freememo.settings :as settings])))

(e/defn DocumentBody
  "Renders the document body for a resolved topic. Reads all values from ambient
   doc-context (dctx), seeded by DocumentRoot. Split into DocumentToolbars /
   DocumentColumns (own e/defns) to stay under the JVM 64KB-per-method cap."
  []
  (e/client
    (let [user-id dctx/user-id page-topic-id dctx/page-topic-id pdf-root-id dctx/pdf-root-id
          is-pdf? dctx/is-pdf? reading-mode? dctx/reading-mode?
          card-font-size dctx/card-font-size card-refresh dctx/card-refresh
          !top-pct dctx/!top-pct !top-pct-save dctx/!top-pct-save reset-split! dctx/reset-split!
          t-layout dctx/t-layout layout-save dctx/layout-save]
    ;; Persist layout on toggle
    (when t-layout
      (if (and is-pdf? pdf-root-id)
        (let [r (e/server (e/Offload #(settings/save-pdf-layout user-id pdf-root-id layout-save)))]
          (when (some? r)
            (if (:success r) (case r (t-layout)) (t-layout (:error r)))))
        (t-layout)))

    (dom/div
      (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                          :min-height "0" :overflow "hidden"}})

      ;; Above-the-content chrome: command bar, PDF toolbar, biblio auto-open
      ;; + modal. Forward the whole prop map (no new literal here).
      (DocumentToolbars)

      ;; TOP REGION: hierarchy | content (PdfPane / EditorPane) | pins.
      (DocumentColumns)

      ;; Card table + its resize handle — hidden in reading-mode? (B1).
      ;; Add-Card still works; success surfaces as a toast (card_modals).
      (when-not reading-mode?
        ;; Vertical drag handle: resizes the top region ↕ the bottom bar.
        ;; Drag persists the split on release; double-click resets to default.
        (dom/div
          (dom/props {:class "split-divider-v"
                      :title "Drag to resize panels (double-click to reset)"})
          (dom/On "pointerdown"
            (fn [e] (util/start-drag! e :y !top-pct {:on-commit #(reset! !top-pct-save %)}))
            nil)
          (dom/On "dblclick" (fn [_] (reset-split!)) nil)))

      ;; BOTTOM BAR (full width): the card table.
      (when-not reading-mode?
        (BottomPanel)))
    )))

(e/defn DocumentRoot
  "Adapter that seeds the doc-context dynamic vars from TopicPage's resolved
   value map `m`, then renders DocumentBody. The map is internal plumbing, NOT
   component props: TopicPage can neither inline this 49-var `binding` (JVM
   64KB-per-method cap — the resolution `let` + binding overflow one method) nor
   pass 49 discrete args (Clojure 20-arity cap), so the resolved values ride one
   compact map to this thin binder frame, which unpacks them into ambient
   context. Every child COMPONENT below reads dctx vars; none take a props map."
  [m]
  (e/client
    (binding [dctx/user-id (:user-id m) dctx/enc-key (:enc-key m) dctx/page-topic-id (:page-topic-id m)
              dctx/kind (:kind m) dctx/pdf-root-id (:pdf-root-id m) dctx/root-topic-id (:root-topic-id m)
              dctx/is-pdf? (:is-pdf? m) dctx/current-page (:current-page m) dctx/effective-content (:effective-content m)
              dctx/llm-enabled? (:llm-enabled? m) dctx/extract-status (:extract-status m) dctx/navigate! (:navigate! m)
              dctx/queue-ctx (:queue-ctx m) dctx/citation (:citation m) dctx/page-info (:page-info m)
              dctx/pdf-root? (:pdf-root? m) dctx/pdf-status (:pdf-status m) dctx/reading-mode? (:reading-mode? m)
              dctx/card-refresh (:card-refresh m) dctx/!show-bib (:!show-bib m) dctx/show-bib? (:show-bib? m)
              dctx/total (:total m) dctx/layout (:layout m) dctx/is-live? (:is-live? m)
              dctx/scan-dpi (:scan-dpi m) dctx/scanning-pages (:scanning-pages m) dctx/ocr-errors (:ocr-errors m)
              dctx/phone? (:phone? m) dctx/!current-page (:!current-page m) dctx/toggle-layout! (:toggle-layout! m)
              dctx/bib-topic-id (:bib-topic-id m) dctx/initial-page (:initial-page m) dctx/target-page (:target-page m)
              dctx/pdf-has-file? (:pdf-has-file? m) dctx/pdf-page-count (:pdf-page-count m) dctx/top-pct (:top-pct m)
              dctx/top-bottom? (:top-bottom? m) dctx/top-split-pct (:top-split-pct m) dctx/left-pct (:left-pct m)
              dctx/!total (:!total m) dctx/!nav-target (:!nav-target m) dctx/!top-split-pct (:!top-split-pct m)
              dctx/!left-pct (:!left-pct m) dctx/card-font-size (:card-font-size m) dctx/t-layout (:t-layout m)
              dctx/layout-save (:layout-save m) dctx/!top-pct (:!top-pct m) dctx/!top-pct-save (:!top-pct-save m)
              dctx/reset-split! (:reset-split! m)]
      (DocumentBody))))
