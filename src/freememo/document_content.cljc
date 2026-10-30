(ns freememo.document-content
  "Render the document layout and persist its display settings.
   Keep this graph separate from the context binder to avoid method-size limits."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.document-view :refer [DocumentToolbars]]
   [freememo.document-workspace :refer [DocumentWorkspace]]
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
          focus-available? dctx/focus-available? focus-scope-id dctx/focus-scope-id
          card-font-size dctx/card-font-size
          t-layout dctx/t-layout layout-save dctx/layout-save
          t-focus dctx/t-focus focus-save dctx/focus-save
          t-editor-hidden dctx/t-editor-hidden editor-hidden-save dctx/editor-hidden-save
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

    ;; Persist the focus-reader flag for the supported document scope.
    ;; Size-0 sibling, as above.
    (e/drain
     (when t-focus
      (if (and focus-available? focus-scope-id)
        (let [r (e/server (e/Offload #(settings/save-focus-reader user-id focus-scope-id focus-save)))]
          (when (some? r)
            (if (:success r) (case r (t-focus)) (t-focus (:error r)))))
        (t-focus))))

    ;; Persist the hidden-editor flag. The save map carries the scope it was
    ;; set under, so a topic change between the set and this write cannot
    ;; land it on another document. Size-0 sibling, as above.
    (e/drain
     (when t-editor-hidden
      (let [scope-id (:scope-id editor-hidden-save)
            hidden? (:hidden? editor-hidden-save)]
        (if scope-id
          (let [r (e/server (e/Offload #(settings/save-editor-hidden user-id scope-id hidden?)))]
            (when (some? r)
              (if (:success r) (case r (t-editor-hidden)) (t-editor-hidden (:error r)))))
          (t-editor-hidden)))))

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
