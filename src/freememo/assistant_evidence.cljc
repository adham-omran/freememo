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
  "Return the Viewer path for retained evidence."
  [source]
  (when-let [document-id (or (:document_topic_id source)
                           (:document-topic-id source))]
    (str "/viewer/topic/" document-id
      (when-let [page (or (:page_number source) (:page-number source))]
        (str "/" page))
      (when-let [anchor (:anchor source)]
        (str "#" anchor)))))

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

#?(:cljs
   (defn clicked-citation [event]
     (when-let [anchor (.closest (.-target event) "a[href]")]
       (let [title (.getAttribute anchor "title")]
         {:target (.getAttribute anchor "href")
          :label (str/trim (.-textContent anchor))
          :citation-key (when (and title (str/starts-with? title "evidence:"))
                          (subs title (count "evidence:")))}))))
#?(:clj (defn clicked-citation [_event] nil))

(defn source-for-citation [sources {:keys [citation-key target label]}]
  (or (when citation-key (source-for-key sources citation-key))
    (some #(when (and (= target (citation-target %))
                   (= label (or (:display_label %) (:display-label %))))
             %)
      sources)
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
                        (str " · Page " page))))))
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
