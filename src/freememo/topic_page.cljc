(ns freememo.topic-page
  "Unified viewer for any topic — PDF root, PDF page, basic/wiki/web/epub.

   Composition only: resolves state via three cross-namespace providers
   (freememo.topic-state) and renders via freememo.document-body. Both splits
   exist so no single e/defn overflows the JVM 64KB-per-method cap that Electric
   hits on large components (see freememo.document-body / freememo.topic-state).

   Routing collapses /viewer/browse-pdf/<root>/<page> and /viewer/browse-topic/<id>
   into a single /viewer/topic/<id>. Page navigation inside a PDF does not
   update the URL — the URL is an entry hint."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.command-bus :as bus]
   [freememo.topic-state :as ts]
   [freememo.document-body :refer [DocumentRoot]]))

;; ---------------------------------------------------------------------------
(e/defn PublishTopicKind
  "Publish the kind of the topic in view as :topic-kind, which Help for this
   page reads at click time (plans/help-for-viewer-topic-kind.md 3.1). Renders
   nothing.

   Post: while `kind` is non-nil, (:topic-kind @bus/!ctx) is `kind`. On
         unmount the key is retracted (command-bus's publisher contract). On a
         topic switch the previous kind stays until the new read lands (3.2)."
  [kind]
  (e/client
    (e/on-unmount #(bus/retract-ctx! [:topic-kind]))
    (when (some? kind)
      (bus/publish-ctx! {:topic-kind kind}))))

(e/defn TopicPage [user-id enc-key topic-id navigate! llm-enabled? queue-ctx url-page]
  (e/client
    (dom/div
      ;; `.topic-page` draws a spinner in index.css while this div is still
      ;; empty, which lasts until every ResolveTopic read returns.
      (dom/props {:class "topic-page"
                  :style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})

      (if (nil? topic-id)
        (dom/div
          (dom/props {:style {:padding "32px" :text-align "center"
                              :color "var(--color-text-secondary)"}})
          (dom/text "No topic selected."))

        ;; Three providers each return a prop-keyed map; merge seeds DocumentRoot,
        ;; the thin binder that unpacks the map into the doc-context dynamic vars
        ;; (dctx). No prop map reaches any component — they read dctx.
        ;; DocumentViewState takes the LIVE focus flag, not resolved's
        ;; :initial-focus?. Merged reading is effective content, which that
        ;; provider owns, and the flag lives in DocumentLayoutState — so it
        ;; crosses as an argument rather than through the merged map.
        (let [resolved (ts/ResolveTopic user-id enc-key topic-id navigate! llm-enabled? queue-ctx url-page)
              layout   (ts/DocumentLayoutState resolved)
              view     (ts/DocumentViewState resolved topic-id (:focus? layout))]
          (DocumentRoot (merge resolved layout view))
          ;; Last, so its value is the body's and it is never skipped.
          (PublishTopicKind (:kind resolved)))))))
