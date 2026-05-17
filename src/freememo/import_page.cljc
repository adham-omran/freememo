(ns freememo.import-page
  "Import tab — Link, Upload, Paste, New Topic. Four entry cards each open
   the unified ImportModal with a different :source-preset. All upload
   data flows over HTTP endpoints in `freememo.api`; Electric drives the
   reactive UI only."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.import-modal :refer [ImportModal]]))

;; ImportCard — single tile that opens its modal on click.
(e/defn ImportCard [emoji label desc !show]
  (dom/div
    (dom/props {:style {:border "1px solid var(--color-border)" :border-radius "var(--radius-md)"
                        :padding "20px" :cursor "pointer" :transition "border-color 0.15s, box-shadow 0.15s"
                        :background "var(--color-bg-surface)"}})
    (dom/On "mouseenter"
      (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")
        (set! (.-boxShadow (.-style (.-currentTarget e))) "0 0 0 1px var(--color-primary)"))
      nil)
    (dom/On "mouseleave"
      (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
        (set! (.-boxShadow (.-style (.-currentTarget e))) "none"))
      nil)
    (dom/On "click" (fn [_] (reset! !show true)) nil)
    (dom/div (dom/props {:style {:font-size "24px" :margin-bottom "8px"}}) (dom/text emoji))
    (dom/div (dom/props {:style {:font-size "14px" :font-weight "600" :margin-bottom "4px"}}) (dom/text label))
    (dom/div (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}}) (dom/text desc))))

(e/defn ImportPage [user-id navigate! _enc-key _llm-enabled?]
  (e/client
    (dom/div
      (dom/props {:class "page-container"})
      (dom/div
        (dom/props {:style {:display "grid" :grid-template-columns "repeat(auto-fill, minmax(200px, 1fr))"
                            :gap "12px"}})

        (let [!show-link (atom false)
              show-link (e/watch !show-link)]
          (ImportCard "🌐" "Link" "Fetch from any web page or Wikipedia URL" !show-link)
          (when show-link
            (ImportModal !show-link user-id :url navigate!)))

        (let [!show-upload (atom false)
              show-upload (e/watch !show-upload)]
          (ImportCard "⬆️" "Upload" "PDF, EPUB, HTML, or Markdown — chosen by file extension" !show-upload)
          (when show-upload
            (ImportModal !show-upload user-id :file navigate!)))

        (let [!show-paste (atom false)
              show-paste (e/watch !show-paste)]
          (ImportCard "📋" "Paste" "Paste HTML or Markdown content" !show-paste)
          (when show-paste
            (ImportModal !show-paste user-id :paste navigate!)))

        (let [!show-topic (atom false)
              show-topic (e/watch !show-topic)]
          (ImportCard "✏️" "New Topic" "Create a blank topic to write in" !show-topic)
          (when show-topic
            (ImportModal !show-topic user-id :new-topic navigate!)))))))
