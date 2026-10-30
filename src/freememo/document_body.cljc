(ns freememo.document-body
  "Bind the resolved document context and render the document body.
   Separate binder frames keep generated methods below the JVM size limit."
  (:require [hyperfiddle.electric3 :as e]
            [freememo.doc-context :as dctx]
            [freememo.document-display-context :refer [DocumentDisplayContext]]))

(e/defn DocumentRoot
  "Bind identity and layout context. The inner frame binds editor and media context."
  [m]
  (e/client
    (binding [dctx/user-id (:user-id m) dctx/enc-key (:enc-key m) dctx/page-topic-id (:page-topic-id m)
              dctx/kind (:kind m) dctx/pdf-root-id (:pdf-root-id m) dctx/root-topic-id (:root-topic-id m)
              dctx/is-pdf? (:is-pdf? m) dctx/current-page (:current-page m) dctx/effective-content (:effective-content m)
              dctx/effective-content-topic-id (:effective-content-topic-id m)
              dctx/llm-enabled? (:llm-enabled? m) dctx/extract-status (:extract-status m) dctx/navigate! (:navigate! m)
              dctx/queue-ctx (:queue-ctx m) dctx/citation (:citation m) dctx/page-info (:page-info m)
              dctx/pdf-root? (:pdf-root? m) dctx/pdf-status (:pdf-status m) dctx/reading-mode? (:reading-mode? m)
              dctx/!show-bib (:!show-bib m) dctx/show-bib? (:show-bib? m)
              dctx/!doc-options-open (:!doc-options-open m) dctx/doc-options-open? (:doc-options-open? m)
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
              dctx/layout-save (:layout-save m) dctx/!top-pct (:!top-pct m) dctx/!top-pct-save (:!top-pct-save m)]
      ;; The anchor. These ids are computed on the client, and the server reads
      ;; them back in readers that unmount and remount on every advance. When
      ;; every request for an id drops at once, one peer can respawn its session
      ;; from size 0 while the other keeps its size, and the next diff crashes
      ;; the socket with :diff-corruption. This server read, drained on the
      ;; client, lives as long as DocumentRoot and keeps both peers' requests
      ;; above 0. `e/drain` holds the sibling at size 0.
      ;; See bugs/20260926-electric-input-diff-corruption/README.md.
      (e/drain (e/server [dctx/user-id dctx/page-topic-id dctx/root-topic-id]))
      (DocumentDisplayContext m))))
