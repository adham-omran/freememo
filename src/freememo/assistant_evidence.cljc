(ns freememo.assistant-evidence
  "Citation targets and the assistant evidence dialog."
  (:require
   #?(:cljs [goog.object :as gobj])
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.math :as math]
   [freememo.modal-shell :as modal]))

(defn citation-target
  "Return the Viewer path for retained evidence. Evidence of a file at a
   commit that a walkthrough showed opens that walkthrough, never the bare
   repository node (plans/commit-range-walkthrough.md §6.3.5)."
  [source]
  (when-let [document-id (or (:document_topic_id source)
                           (:document-topic-id source))]
    (if-let [wid (or (:walkthrough_id source) (:walkthrough-id source))]
     (str "/viewer/topic/" document-id "/walkthrough/" wid)
     (str "/viewer/topic/" document-id
      (when-let [page (or (:page_number source) (:page-number source))]
        (str "/" page))
      (when-let [anchor (:anchor source)]
        (str "#" anchor))))))

(defn source-for-target
  "Find retained evidence for a citation path."
  [sources target]
  (some #(when (= target (citation-target %)) %) sources))

(defn source-for-key
  "Find retained evidence for a citation key."
  [sources citation-key]
  (some #(when (= citation-key (or (:citation_key %) (:citation-key %))) %) sources))

(defn other-document?
  "Return true when evidence belongs to a different document."
  [source current-document-id]
  (not= (str current-document-id)
    (str (or (:document_topic_id source) (:document-topic-id source)))))

(def citation-selector
  "A citation link after `decorate-citations!`. index.css styles it as a pill."
  "a[data-citation-key]")

(def ^:private key-title-prefix
  "The title prefix that carries a citation key out of the sanitizer.
   `assistant/render-evidence-citations` writes it, and `clean-html` keeps
   `title` on `<a>` while it strips data attributes."
  "evidence:")

#?(:cljs
   (defn decorate-citations!
     "Turn the citation links that `node` holds into pills.
      Pre:  `node` holds answer HTML that `render-evidence-citations` wrote, and
            `sources` is the evidence of that answer.
      Post: each citation link carries its key as `data-citation-key` and no
            `title`, so the browser shows no native tooltip, and its
            `aria-label` is the full display label. No text node changed:
            annotations hold text offsets into the leaf. Returns nil."
     [node sources]
     (doseq [anchor (array-seq (.querySelectorAll node "a[title]"))
             :let [title (.getAttribute anchor "title")]
             :when (str/starts-with? title key-title-prefix)]
       (.setAttribute anchor "data-citation-key" (subs title (count key-title-prefix)))
       (.removeAttribute anchor "title"))
     (doseq [anchor (array-seq (.querySelectorAll node citation-selector))
             :let [source (source-for-key sources (.getAttribute anchor "data-citation-key"))]
             :when source]
       (.setAttribute anchor "aria-label"
         (or (:display_label source) (:display-label source)))))
   :clj (defn decorate-citations! [_node _sources] nil))

#?(:cljs
   (defn clicked-citation [event]
     (when-let [anchor (.closest (.-target event) "a[href]")]
       {:target (.getAttribute anchor "href")
        :citation-key (.getAttribute anchor "data-citation-key")})))
#?(:clj (defn clicked-citation [_event] nil))

(defn source-for-citation
  "The evidence a clicked link cites: by its key, else by its target.
   A link with a key always resolves by key, because two excerpts can share a
   target (plans/assistant-citation-pills.md §5.2.4)."
  [sources {:keys [citation-key target]}]
  (if citation-key
    (source-for-key sources citation-key)
    (source-for-target sources target)))

(defn set-highlighted-html! [node html]
  #?(:cljs
     (let [source (or html "")
           hljs (gobj/get js/window "hljs")]
       (if-let [highlight (some-> hljs (gobj/get "highlight"))]
         (let [result (.call highlight hljs source
                        #js {:language "xml" :ignoreIllegals true})]
           (set! (.-innerHTML node) (gobj/get result "value")))
         (set! (.-textContent node) source)))
     :clj nil)
  nil)

(e/defn EvidenceFormattedLeaf [safe-html]
  (e/client
    (dom/div
      (dom/props {:class "assistant-evidence__quote assistant-evidence__formatted"
                  :dir "auto"})
      (math/set-html! dom/node safe-html))))

(e/defn EvidenceSourceLeaf [safe-html]
  (e/client
    (dom/pre
      (dom/props {:class "assistant-evidence__quote assistant-evidence__source"
                  :dir "ltr"})
      (dom/code
        (dom/props {:class "hljs language-html"})
        (set-highlighted-html! dom/node safe-html)))))

(e/defn EvidenceModal [!source current-document-id]
  (e/client
    (when-let [source (e/watch !source)]
      (let [!view (atom :preview)
            view (e/watch !view)
            target (citation-target source)
            other-document? (other-document? source current-document-id)
            close! (fn [] (reset! !source nil))]
        (dom/div
          (dom/props {:class "modal-backdrop assistant-evidence-backdrop"})
          (dom/On "click" (fn [_] (close!)) nil)
          (modal/ModalEscape close! "Source evidence")
          (dom/div
            (dom/props {:class "modal-content assistant-evidence"})
            (dom/On "click" (fn [event] (.stopPropagation event)) nil)
            (dom/div
              (dom/props {:class "assistant-evidence__header"})
              (dom/div
                (dom/h2 (dom/text "Source evidence"))
                (dom/p
                  (dom/props {:class "assistant-evidence__location"})
                  (dom/text
                    (str (or (:document_title source) "Untitled document")
                      (when-let [page (:page_number source)]
                        (str ", Page " page))))))
              (dom/button
                (dom/props {:class "btn btn-secondary" :type "button"
                            :aria-label "Close source evidence"})
                (dom/text "Close")
                (dom/On "click" (fn [_] (close!)) nil)))
            (if-let [safe-html (:quoted_html source)]
              (do
                (dom/div
                  (dom/props {:class "assistant-evidence__view-tabs"
                              :role "group" :aria-label "Evidence view"})
                  (dom/button
                    (dom/props {:class (str "btn btn-secondary"
                                        (when (= :preview view) " is-active"))
                                :type "button"
                                :aria-pressed (= :preview view)})
                    (dom/text "Preview")
                    (dom/On "click" (fn [_] (reset! !view :preview)) nil))
                  (dom/button
                    (dom/props {:class (str "btn btn-secondary"
                                        (when (= :html view) " is-active"))
                                :type "button"
                                :aria-pressed (= :html view)})
                    (dom/text "HTML")
                    (dom/On "click" (fn [_] (reset! !view :html)) nil)))
                (if (= :html view)
                  (EvidenceSourceLeaf safe-html)
                  (EvidenceFormattedLeaf safe-html)))
              (if-let [quote (:quoted_text source)]
                (dom/blockquote
                  (dom/props {:class "assistant-evidence__quote" :dir "auto"})
                  (dom/text quote))
                (dom/p
                  (dom/props {:class "assistant-evidence__empty"})
                  (dom/text "No retained excerpt is available."))))
            (when (and other-document? target)
              (dom/a
                (dom/props {:class "btn btn-primary assistant-evidence__open"
                            :href target :target "_blank"
                            :rel "noopener noreferrer"})
                (dom/text "Open document in new tab")))))))))
