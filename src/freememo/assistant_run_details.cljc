(ns freememo.assistant-run-details
  "Owner-only text details for one assistant answer run."
  (:require
   [clojure.pprint :as pprint]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.modal-shell :as modal]))

(defn- reported [value]
  (if (nil? value) "Not reported" (str value)))

(defn- pretty-text [value]
  (with-out-str (pprint/pprint value)))

(defn- citation-target [source]
  (when-let [document-id (:document_topic_id source)]
    (str "/viewer/topic/" document-id
      (when-let [page (:page_number source)] (str "/" page))
      (when-let [anchor (:anchor source)] (str "#" anchor)))))

(e/defn RunDetailsModal
  "Show retained answer facts. The parent gets details through an owner-scoped
   server query before it calls this component."
  [!run-uuid details]
  (e/client
    (when (and (e/watch !run-uuid) details)
      (dom/div
        (dom/props {:class "modal-backdrop assistant-run-details-backdrop"})
        (dom/On "click" (fn [_] (reset! !run-uuid nil)) nil)
        (modal/ModalEscape (fn [] (reset! !run-uuid nil)) "Answer details")
        (dom/div
          (dom/props {:class "modal-content assistant-run-details"})
          (dom/On "click" (fn [event] (.stopPropagation event)) nil)
          (dom/div
            (dom/props {:class "assistant-run-details__header"})
            (dom/h2 (dom/text "Answer details"))
            (dom/button
              (dom/props {:class "btn btn-secondary" :type "button"
                          :aria-label "Close answer details"})
              (dom/text "Close")
              (dom/On "click" (fn [_] (reset! !run-uuid nil)) nil)))
          (dom/dl
            (e/for [[label key]
                    (e/diff-by second
                      [["Status" :status]
                       ["Requested model" :requested_model]
                       ["Resolved model" :resolved_model]
                       ["Provider" :provider_name]
                       ["Generation ID" :generation_id]
                       ["Input tokens" :input_tokens]
                       ["Output tokens" :output_tokens]
                       ["Reasoning tokens" :reasoning_tokens]
                       ["Cached tokens" :cached_tokens]
                       ["Cache-write tokens" :cache_write_tokens]
                       ["Cost, USD" :cost_usd]
                       ["Provider latency, ms" :provider_latency_ms]
                       ["Generation time, ms" :generation_ms]
                       ["First token, ms" :first_token_ms]
                       ["Created" :created_at]
                       ["Started" :started_at]
                       ["First token at" :first_token_at]
                       ["Stopped" :stopped_at]
                       ["Completed" :completed_at]
                       ["Failed" :failed_at]])]
              (dom/div
                (dom/dt (dom/text label))
                (dom/dd (dom/text (reported (get details key)))))))
          (dom/h3 (dom/text "Context snapshot"))
          (dom/pre
            (dom/props {:class "assistant-run-details__snapshot"})
            (dom/text (pretty-text (:request_snapshot details))))
          (dom/h3 (dom/text "Sources"))
          (if (seq (:evidence details))
            (dom/ul
              (e/for [source (e/diff-by :id (:evidence details))]
                (dom/li
                  (if-let [target (citation-target source)]
                    (dom/a
                      (dom/props {:href target})
                      (dom/text (str (:document_title source)
                                  (when-let [page (:page_number source)]
                                    (str ", page " page))
                                  " — " (:display_label source))))
                    (dom/text (str (:document_title source) " — target unavailable")))
                  (when-let [revision (:source_revision source)]
                    (dom/div
                      (dom/props {:class "assistant-run-details__source-revision"})
                      (dom/text (str "Source revision: " revision))))
                  (when-let [quote (:quoted_text source)]
                    (dom/element "details"
                      (dom/element "summary" (dom/text "Retained excerpt"))
                      (dom/pre
                        (dom/props {:class "assistant-run-details__source-quote"})
                        (dom/text quote)))))))
            (dom/p (dom/text "No sources were retained."))))))))
