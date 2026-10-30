(ns freememo.document-display-context
  "Bind editor and media context before rendering the document.
   This second binder keeps each generated method below the JVM size limit."
  (:require [hyperfiddle.electric3 :as e]
            [freememo.doc-context :as dctx]
            [freememo.document-content :refer [DocumentBody]]
            [freememo.apply-code-language :refer [ApplyCodeLanguage]]))

(e/defn DocumentDisplayContext [m]
  (e/client
    (binding [dctx/focus? (:focus? m) dctx/set-focus! (:set-focus! m)
              dctx/focus-available? (:focus-available? m) dctx/focus-scope-id (:focus-scope-id m)
              dctx/merged? (:merged? m) dctx/merged-page-numbers (:merged-page-numbers m)
              dctx/merged-range (:merged-range m)
              dctx/merged-read-request (:merged-read-request m)
              dctx/merged-expected-read-request (:merged-expected-read-request m)
              dctx/merged-root-id (:merged-root-id m)
              dctx/editor-doc-id (:editor-doc-id m)
              dctx/t-focus (:t-focus m) dctx/focus-save (:focus-save m)
              dctx/has-source-pane? (:has-source-pane? m) dctx/editor-hidden? (:editor-hidden? m)
              dctx/toggle-editor-hidden! (:toggle-editor-hidden! m)
              dctx/t-editor-hidden (:t-editor-hidden m) dctx/editor-hidden-save (:editor-hidden-save m)
              dctx/t-last-page (:t-last-page m) dctx/last-page-save (:last-page-save m)
              dctx/on-page-change! (:on-page-change! m)
              dctx/reset-split! (:reset-split! m)
              dctx/set-card-table-open! (:set-card-table-open! m)
              dctx/toggle-card-table! (:toggle-card-table! m)
              dctx/is-score? (:is-score? m) dctx/!score-region (:!score-region m)
              dctx/is-poem? (:is-poem? m)
              dctx/!score-pages (:!score-pages m) dctx/!score-modal-open? (:!score-modal-open? m)
              dctx/!score-edit (:!score-edit m)
              dctx/is-video? (:is-video? m) dctx/video-title (:video-title m)
              dctx/video-duration-ms (:video-duration-ms m)
              dctx/video-last-pos-ms (:video-last-pos-ms m)
              dctx/video-has-audio? (:video-has-audio? m)
              dctx/is-code? (:is-code? m) dctx/code-has-source? (:code-has-source? m)
              dctx/!video-el (:!video-el m) dctx/!video-region (:!video-region m)
              dctx/!video-playhead (:!video-playhead m)]
      (DocumentBody)
      ;; Keep the optional picker last so it cannot shift DocumentBody.
      ;; Mount it outside the Live Document wizard branch so it consumes pending invocations.
      (ApplyCodeLanguage))))
