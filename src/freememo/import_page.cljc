(ns freememo.import-page
  "Import tab — Link, Upload, Paste, New Topic, Zotero. Each entry card
   opens its respective modal. All upload data flows over HTTP endpoints
   in `freememo.api`; Electric drives the reactive UI only."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.import-modal :refer [ImportModal]]
   [freememo.zotero-picker-modal :refer [ZoteroPickerModal]]))

;; ImportCard — single tile that opens its modal on click.
;; `icon` is a Lucide keyword (e.g. :link, :upload). Rendered at size 24 so
;; the cards feel like launcher tiles rather than inline toolbar items.
(e/defn ImportCard [icon label desc !show]
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
    (dom/div (dom/props {:style {:margin-bottom "8px"}}) (icons/Icon icon :size 24))
    (dom/div (dom/props {:style {:font-size "14px" :font-weight "600" :margin-bottom "4px"}}) (dom/text label))
    (dom/div (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}}) (dom/text desc))))

;; Zotero entry tile — opens the Zotero library picker. Requires the
;; FreeMemo for Zotero plugin installed and Zotero running.

(e/defn ImportPage [user-id navigate! _enc-key _llm-enabled?]
  (e/client
    (dom/div
      (dom/props {:class "page-container"})
      (dom/div
        (dom/props {:style {:display "grid" :grid-template-columns "repeat(auto-fill, minmax(200px, 1fr))"
                            :gap "12px"}})

        (let [!show-link (atom false)
              show-link (e/watch !show-link)]
          (ImportCard :link "Link" "Fetch from any web page or Wikipedia URL" !show-link)
          (when show-link
            (ImportModal !show-link user-id :url navigate!)))

        (let [!show-upload (atom false)
              show-upload (e/watch !show-upload)]
          (ImportCard :upload "Upload" "PDF, EPUB, HTML, or Markdown — chosen by file extension" !show-upload)
          (when show-upload
            (ImportModal !show-upload user-id :file navigate!)))

        (let [!show-paste (atom false)
              show-paste (e/watch !show-paste)]
          (ImportCard :clipboard "Paste" "Paste HTML or Markdown content" !show-paste)
          (when show-paste
            (ImportModal !show-paste user-id :paste navigate!)))

        (let [!show-topic (atom false)
              show-topic (e/watch !show-topic)]
          (ImportCard :file-plus "New Topic" "Create a blank topic to write in" !show-topic)
          (when show-topic
            (ImportModal !show-topic user-id :new-topic navigate!)))

        (let [!show-zotero (atom false)
              show-zotero (e/watch !show-zotero)]
          (ImportCard :library "Zotero" "Pick a PDF from your local Zotero library" !show-zotero)
          (when show-zotero
            (ZoteroPickerModal !show-zotero user-id navigate!)))))))
