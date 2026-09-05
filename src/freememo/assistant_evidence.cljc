(ns freememo.assistant-evidence
  "Citation targets and the assistant evidence dialog."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
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

(defn other-document?
  "Return true when evidence belongs to a different document."
  [source current-document-id]
  (not= (str current-document-id)
    (str (or (:document_topic_id source) (:document-topic-id source)))))

#?(:cljs
   (defn clicked-citation-target [event]
     (when-let [anchor (.closest (.-target event) "a[href]")]
       (.getAttribute anchor "href"))))
#?(:clj (defn clicked-citation-target [_event] nil))

(e/defn EvidenceModal [!source current-document-id]
  (e/client
    (when-let [source (e/watch !source)]
      (let [target (citation-target source)
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
            (if-let [quote (:quoted_text source)]
              (dom/blockquote
                (dom/props {:class "assistant-evidence__quote" :dir "auto"})
                (dom/text quote))
              (dom/p
                (dom/props {:class "assistant-evidence__empty"})
                (dom/text "No retained excerpt is available.")))
            (when (and other-document? target)
              (dom/a
                (dom/props {:class "btn btn-primary assistant-evidence__open"
                            :href target :target "_blank"
                            :rel "noopener noreferrer"})
                (dom/text "Open document in new tab")))))))))
