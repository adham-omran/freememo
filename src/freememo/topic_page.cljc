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
   [freememo.topic-state :as ts]
   [freememo.document-body :refer [DocumentRoot]]))

;; ---------------------------------------------------------------------------
(e/defn TopicPage [user-id enc-key topic-id navigate! llm-enabled? queue-ctx]
  (e/client
    (dom/div
      (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})

      (if (nil? topic-id)
        (dom/div
          (dom/props {:style {:padding "32px" :text-align "center"
                              :color "var(--color-text-secondary)"}})
          (dom/text "No topic selected."))

        ;; Three providers each return a prop-keyed map; merge seeds DocumentRoot,
        ;; the thin binder that unpacks the map into the doc-context dynamic vars
        ;; (dctx). No prop map reaches any component — they read dctx.
        (let [resolved (ts/ResolveTopic user-id enc-key topic-id navigate! llm-enabled? queue-ctx)
              layout   (ts/DocumentLayoutState resolved)
              view     (ts/DocumentViewState resolved topic-id)]
          (DocumentRoot (merge resolved layout view)))))))
