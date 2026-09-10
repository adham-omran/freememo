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
   [freememo.document-view :refer [DocumentToolbars]]
   [freememo.document-workspace :refer [DocumentWorkspace]]
   [freememo.apply-code-language :refer [ApplyCodeLanguage]]
   [freememo.live-doc-wizard :refer [LiveDocEmptyState]]
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
          t-layout dctx/t-layout layout-save dctx/layout-save
          t-focus dctx/t-focus focus-save dctx/focus-save
          t-last-page dctx/t-last-page last-page-save dctx/last-page-save
          is-live? dctx/is-live? pdf-has-file? dctx/pdf-has-file?
          assistant-full-height? (e/server
                                   (settings/get-assistant-full-height user-id))]
    ;; Persist layout on toggle.
    ;;
    ;; `e/drain` holds this sibling at size 0. A bare `(when t …)` is a 0-to-1
    ;; size change, and an incseq diff falls back on POSITION — so a token
    ;; appearing here shifted every following sibling, including
    ;; DocumentColumns, which unmounted and rebuilt the whole top region.
    ;; `e/drain` runs the body for its effect and always returns the empty amb.
    (e/drain
     (when t-layout
      (if (and is-pdf? pdf-root-id)
        (let [r (e/server (e/Offload #(settings/save-pdf-layout user-id pdf-root-id layout-save)))]
          (when (some? r)
            (if (:success r) (case r (t-layout)) (t-layout (:error r)))))
        (t-layout))))

    ;; Persist the focus-reader flag on toggle. Same shape as the layout
    ;; persist above: a non-PDF has no per-root key, so the token is spent
    ;; without a server call.
    ;; Size-0 sibling, as above.
    (e/drain
     (when t-focus
      (if (and is-pdf? pdf-root-id)
        (let [r (e/server (e/Offload #(settings/save-pdf-focus user-id pdf-root-id focus-save)))]
          (when (some? r)
            (if (:success r) (case r (t-focus)) (t-focus (:error r)))))
        (t-focus))))

    ;; Persist the last page the reader turned to. This lived in PdfPane until
    ;; focus reader, which unmounts the pane — and a writer that unmounts stops
    ;; recording pages. DocumentViewState owns the request atom and
    ;; `on-page-change!` is its only setter, so this fires once per real page
    ;; change and never at mount.
    ;; Size-0 sibling, as above.
    (e/drain
     (when t-last-page
      (if (and is-pdf? pdf-root-id last-page-save)
        (let [r (e/server (e/Offload #(settings/save-last-page user-id pdf-root-id last-page-save)))]
          (when (some? r)
            (if (:success r) (case r (t-last-page)) (t-last-page (:error r)))))
        (t-last-page))))

    ;; Blob-less Live Document: the upload wizard is the whole body. It has no
    ;; toolbars, navigation panel, or card table. Normal layout returns once the
    ;; first page commits (pdf-has-file? flips true → this gate is false).
    (if (and is-live? (not pdf-has-file?))
      (LiveDocEmptyState)
      (dom/div
      (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                          :min-height "0" :overflow "hidden"}})

      ;; Above-the-content chrome: command bar, PDF toolbar, biblio auto-open
      ;; + modal. Forward the whole prop map (no new literal here).
      (DocumentToolbars)

      (DocumentWorkspace assistant-full-height?)))
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
              dctx/effective-content-topic-id (:effective-content-topic-id m)
              dctx/llm-enabled? (:llm-enabled? m) dctx/extract-status (:extract-status m) dctx/navigate! (:navigate! m)
              dctx/queue-ctx (:queue-ctx m) dctx/citation (:citation m) dctx/page-info (:page-info m)
              dctx/pdf-root? (:pdf-root? m) dctx/pdf-status (:pdf-status m) dctx/reading-mode? (:reading-mode? m)
              dctx/card-refresh (:card-refresh m) dctx/!show-bib (:!show-bib m) dctx/show-bib? (:show-bib? m)
              dctx/total (:total m) dctx/layout (:layout m) dctx/is-live? (:is-live? m)
              dctx/scan-dpi (:scan-dpi m) dctx/scanning-pages (:scanning-pages m) dctx/ocr-errors (:ocr-errors m)
              dctx/phone? (:phone? m) dctx/!current-page (:!current-page m) dctx/toggle-layout! (:toggle-layout! m)
              dctx/bib-topic-id (:bib-topic-id m) dctx/initial-page (:initial-page m) dctx/target-page (:target-page m)
              dctx/pdf-has-file? (:pdf-has-file? m) dctx/pdf-page-count (:pdf-page-count m) dctx/top-pct (:top-pct m)
              dctx/card-table-open? (:card-table-open? m)
              dctx/top-bottom? (:top-bottom? m) dctx/top-split-pct (:top-split-pct m) dctx/left-pct (:left-pct m)
              dctx/transcript-pct (:transcript-pct m) dctx/!transcript-pct (:!transcript-pct m)
              dctx/!total (:!total m) dctx/!nav-target (:!nav-target m) dctx/!top-split-pct (:!top-split-pct m)
              dctx/!left-pct (:!left-pct m) dctx/card-font-size (:card-font-size m) dctx/t-layout (:t-layout m)
              dctx/layout-save (:layout-save m) dctx/!top-pct (:!top-pct m) dctx/!top-pct-save (:!top-pct-save m)
              dctx/focus? (:focus? m) dctx/set-focus! (:set-focus! m)
              dctx/merged? (:merged? m) dctx/merged-page-numbers (:merged-page-numbers m)
              dctx/editor-doc-id (:editor-doc-id m)
              dctx/t-focus (:t-focus m) dctx/focus-save (:focus-save m)
              dctx/t-last-page (:t-last-page m) dctx/last-page-save (:last-page-save m)
              dctx/on-page-change! (:on-page-change! m)
              dctx/reset-split! (:reset-split! m)
              dctx/set-card-table-open! (:set-card-table-open! m)
              dctx/toggle-card-table! (:toggle-card-table! m)
              dctx/is-score? (:is-score? m) dctx/!score-region (:!score-region m)
              dctx/!score-pages (:!score-pages m) dctx/!score-modal-open? (:!score-modal-open? m)
              dctx/!score-edit (:!score-edit m)
              dctx/is-video? (:is-video? m) dctx/video-title (:video-title m)
              dctx/video-duration-ms (:video-duration-ms m)
              dctx/video-last-pos-ms (:video-last-pos-ms m)
              dctx/video-has-audio? (:video-has-audio? m)
              dctx/!video-el (:!video-el m) dctx/!video-region (:!video-region m)
              dctx/!video-playhead (:!video-playhead m)]
      (DocumentBody)
      ;; LAST, and outside DocumentBody, for two reasons. Last: the picker is a
      ;; 0-or-1 sibling, and this file's `e/drain` comments record what a size
      ;; change does to the siblings that follow one. Outside: DocumentBody
      ;; splits a blob-less Live Document off into LiveDocEmptyState, and a
      ;; branch with no consumer would leave the palette's invocation sitting in
      ;; !invocations to open a modal on some later mount.
      (ApplyCodeLanguage))))
