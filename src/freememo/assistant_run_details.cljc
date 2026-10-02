(ns freememo.assistant-run-details
  "Owner-only text details for one assistant answer run."
  (:require
   [clojure.pprint :as pprint]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.modal-shell :as modal]
   [freememo.tape-table :as tape]
   #?(:clj [freememo.assistant :as assistant])
   #?(:clj [freememo.user-state :as us])))

(defn- reported [value]
  (if (nil? value) "Not reported" (str value)))

(defn- pretty-text [value]
  (with-out-str (pprint/pprint value)))

(defn- citation-target [source]
  (when-let [document-id (:document_topic_id source)]
    (str "/viewer/topic/" document-id
      (when-let [page (:page_number source)] (str "/" page))
      (when-let [anchor (:anchor source)] (str "#" anchor)))))

(defn- source-label [source target]
  (if target
    (str (:document_title source)
      (when-let [page (:page_number source)] (str ", page " page))
      " — " (:display_label source))
    (str (:document_title source) " — target unavailable")))

(defn source-row
  "The part of one evidence row that the Sources table paints.

   The excerpt is not in it. A Tape row crosses the wire whole each time it
   enters the window, and only the one excerpt the user opens needs to cross.

   Post: nil for a nil source."
  [source]
  (when source
    (let [target (citation-target source)]
      {:target target
       :label (source-label source target)
       :revision (:source_revision source)
       :excerpt? (some? (:quoted_text source))})))

(defn source-excerpt
  "The opened excerpt of one evidence row. Post: nil for a nil source."
  [source]
  (when source
    {:label (source-label source (citation-target source))
     :quote (:quoted_text source)}))

(def ^:private source-headers ["Source" "Revision" "Excerpt"])

(def ^:private source-grid-cols "minmax(0, 1fr) minmax(0, 140px) 76px")

(e/defn SourceRow
  "One Sources row. `!excerpt` holds `[run-uuid i]` for the opened excerpt, so
   an index from one run never opens an excerpt of another."
  [row i run-uuid !excerpt]
  (e/client
    (let [open? (= [run-uuid i] (e/watch !excerpt))]
      (dom/tr
        (dom/props {:class (when (odd? i) "row-alt") :style {:--order i}})
        (dom/td
          (if-let [target (:target row)]
            (dom/a
              (dom/props {:class "tape-cell-text" :href target
                          :data-tooltip (:label row)})
              (dom/text (:label row)))
            (dom/span
              (dom/props {:class "tape-cell-text tape-cell-absent"
                          :data-tooltip (:label row)})
              (dom/text (:label row)))))
        (dom/td
          (dom/props {:class "tape-cell-meta"})
          (if-let [revision (:revision row)]
            (dom/span
              (dom/props {:class "tape-cell-text" :data-tooltip revision})
              (dom/text revision))
            (dom/span (dom/props {:class "tape-cell-absent"}) (dom/text "—"))))
        (dom/td
          (dom/props {:class "tape-cell-actions"})
          (when (:excerpt? row)
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :type "button"
                          :aria-expanded (str open?)})
              (dom/text (if open? "Hide" "Show"))
              (dom/On "click"
                (fn [_]
                  (swap! !excerpt
                    (fn [current] (when-not (= current [run-uuid i])
                                    [run-uuid i]))))
                nil))))))))

(e/defn SourcesTable
  "Every evidence row of the run, through the shared virtual-scroll table, and
   the one excerpt the user opened below it.

   `sources` is a server-form binding. Only its count, the window rows and the
   opened excerpt cross the wire."
  [sources run-uuid]
  (e/client
    (let [!excerpt (atom nil)
          [excerpt-run excerpt-i] (e/watch !excerpt)
          item-count (e/server (count sources))]
      (dom/div
        (dom/props {:class "assistant-run-details__sources"})
        (tape/TapeTable source-headers source-grid-cols item-count run-uuid
          "No sources were retained."
          (e/fn [i]
            ;; One window row crosses the wire, never the whole run.
            (when-let [row (e/server (source-row (nth sources i nil)))]
              (SourceRow row i run-uuid !excerpt)))))
      (when (and excerpt-i (= excerpt-run run-uuid))
        (when-let [excerpt (e/server (source-excerpt (nth sources excerpt-i nil)))]
          (dom/div
            (dom/props {:class "assistant-run-details__excerpt"})
            (dom/div
              (dom/props {:class "assistant-run-details__excerpt-title"})
              (dom/text (:label excerpt)))
            (dom/pre
              (dom/props {:class "assistant-run-details__source-quote"})
              (dom/text (:quote excerpt)))))))))

(e/defn RunDetailsModal
  "Show retained answer facts. The parent gets details through an owner-scoped
   server query before it calls this component. The sources are read here, as a
   server form, because a run can retain thousands of them."
  [!run-uuid details user-id]
  (e/client
    (let [run-uuid (e/watch !run-uuid)]
      (when (and run-uuid details)
        (let [sources (e/server
                        (assistant/run-sources*
                          (e/watch (us/get-atom user-id :assistant-mutations))
                          user-id run-uuid))]
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
              (SourcesTable sources run-uuid))))))))
