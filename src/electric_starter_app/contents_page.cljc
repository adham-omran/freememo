(ns electric-starter-app.contents-page
  "Contents tab — knowledge tree showing documents → extracts → sub-extracts."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [electric-starter-app.db :as db])
   #?(:clj [electric-starter-app.pdf :as pdf])
   #?(:clj [clojure.string :as str])))

;; Server wrappers — _refresh param creates Electric reactive dependency
(defn get-documents* [_refresh user-id]
  #?(:clj (vec (:documents (pdf/list-pdfs user-id)))
     :cljs nil))

(defn get-tree-items* [_refresh user-id]
  #?(:clj (vec (db/get-knowledge-tree user-id))
     :cljs nil))

;; Truncate HTML content to plain text preview
(defn content-preview [content max-len]
  (when (and content (seq content))
    (let [;; Strip HTML tags for preview
          text #?(:cljs (let [tmp (js/document.createElement "div")]
                          (set! (.-innerHTML tmp) content)
                          (.-textContent tmp))
                  :clj (clojure.string/replace content #"<[^>]*>" ""))
          trimmed (if (> (count text) max-len)
                    (str (subs text 0 max-len) "...")
                    text)]
      trimmed)))

;; Extract tree node — recursive component
(e/defn ExtractNode [item children-map depth !nav-target navigate!]
  (e/client
    (let [id (:content_items/id item)
          children (get children-map id)
          has-children (boolean (seq children))
          dismissed (:content_items/dismissed item)
          preview (content-preview (:content_items/content item) 60)
          !expanded (atom false)
          expanded (e/watch !expanded)]
      (dom/div
        (dom/props {:style {:padding-left "20px"
                            :opacity (if dismissed "0.4" "1")}})
        ;; Row
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :gap "6px"
                              :padding "4px 0" :border-bottom "1px solid #f5f5f5"}})
          ;; Arrow
          (if has-children
            (dom/span
              (dom/props {:style {:width "16px" :font-size "10px" :cursor "pointer"
                                  :user-select "none" :text-align "center" :flex-shrink "0"}})
              (dom/text (if expanded "\u25BC" "\u25B6"))
              (dom/On "click" (fn [e] (.stopPropagation e) (swap! !expanded not)) nil))
            (dom/span (dom/props {:style {:width "16px" :flex-shrink "0"}})))
          ;; Type badge
          (dom/span
            (dom/props {:class "type-badge" :style {:background "#44C2FF"}})
            (dom/text "Ext"))
          ;; Content preview — double-click opens
          (dom/span
            (dom/props {:style {:font-size "13px" :color "#333" :cursor "pointer"
                                :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}
                        :title "Click to open"})
            (dom/On "mouseenter" (fn [e] (set! (.-textDecoration (.-style (.-target e))) "underline")) nil)
            (dom/On "mouseleave" (fn [e] (set! (.-textDecoration (.-style (.-target e))) "none")) nil)
            (dom/text (or preview "(empty)"))
            (dom/On "click"
              (fn [_]
                (reset! !nav-target {:content-item-id id})
                (navigate! :extract))
              nil)))
        ;; Children
        (when (and expanded has-children)
          (e/for-by :content_items/id [child children]
            (ExtractNode child children-map (inc depth) !nav-target navigate!)))))))

;; Document root node
(e/defn DocumentNode [doc items-for-doc !nav-target navigate!]
  (e/client
    (let [doc-id (:documents/id doc)
          filename (:documents/filename doc)
          source-type (or (:documents/source_type doc) "pdf")
          dismissed (:documents/dismissed doc)
          type-label (case source-type "wikipedia" "Wiki" "web" "Web" "PDF")
          type-color (case source-type "wikipedia" "#fef3c7" "web" "#e0f2fe" "#dcfce7")
          ;; Build children map for this document's extracts
          children-map (group-by :content_items/parent_content_item_id items-for-doc)
          root-items (get children-map nil)
          has-children (boolean (seq root-items))
          !expanded (atom false)
          expanded (e/watch !expanded)]
      (dom/div
        (dom/props {:style {:opacity (if dismissed "0.4" "1")}})
        ;; Document row
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                              :padding "6px 0" :border-bottom "1px solid #e8e8e8"}})
          ;; Arrow
          (if has-children
            (dom/span
              (dom/props {:style {:width "16px" :font-size "11px" :cursor "pointer"
                                  :user-select "none" :text-align "center" :flex-shrink "0"}})
              (dom/text (if expanded "\u25BC" "\u25B6"))
              (dom/On "click" (fn [e] (.stopPropagation e) (swap! !expanded not)) nil))
            (dom/span (dom/props {:style {:width "16px" :flex-shrink "0"}})))
          ;; Type badge
          (dom/span
            (dom/props {:class "type-badge" :style {:background type-color}})
            (dom/text type-label))
          ;; Filename — double-click opens
          (dom/span
            (dom/props {:style {:font-size "14px" :font-weight "500" :color "#222" :cursor "pointer"}
                        :title "Click to open"})
            (dom/On "mouseenter" (fn [e] (set! (.-textDecoration (.-style (.-target e))) "underline")) nil)
            (dom/On "mouseleave" (fn [e] (set! (.-textDecoration (.-style (.-target e))) "none")) nil)
            (dom/text filename)
            (dom/On "click"
              (fn [_]
                (reset! !nav-target {:doc-id doc-id})
                (navigate! :learn))
              nil)))
        ;; Children (extracts)
        (when (and expanded has-children)
          (e/for-by :content_items/id [item root-items]
            (ExtractNode item children-map 1 !nav-target navigate!)))))))

#?(:clj
   (defn filter-tree-docs [docs filter-text]
     (if (or (nil? filter-text) (str/blank? filter-text))
       docs
       (let [q (str/lower-case (str/trim filter-text))]
         (filterv #(str/includes? (str/lower-case (or (:documents/filename %) "")) q) docs)))))

;; Document tree view — used by LibraryPage
;; Receives !refresh and filter-text from parent
(e/defn DocumentTreeView [user-id !nav-target navigate! !refresh filter-text]
  (e/client
    (e/server
      (let [refresh (e/watch !refresh)
            all-documents (get-documents* refresh user-id)
            documents (filter-tree-docs all-documents filter-text)
            all-items (get-tree-items* refresh user-id)]
        (e/client
          (let [items-by-doc (group-by :content_items/document_id all-items)]
            (if (seq documents)
              (dom/div
                (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"}})
                (e/for-by :documents/id [doc documents]
                  (DocumentNode doc (get items-by-doc (:documents/id doc)) !nav-target navigate!)))
              (dom/p
                (dom/props {:style {:color "var(--color-text-secondary)" :font-size "14px"}})
                (dom/text "No content yet. Import content from the Import tab.")))))))))

;; Legacy ContentsPage — no longer routed, kept for reference
(e/defn ContentsPage [user-id !nav-target navigate!]
  (e/client
    (dom/div
      (dom/props {:style {:padding "16px" :max-width "900px" :height "100%"
                          :display "flex" :flex-direction "column"}})
      (dom/h2
        (dom/props {:style {:margin "0 0 16px 0" :font-size "20px"}})
        (dom/text "Contents"))

      (let [documents (e/server (get-documents* 0 user-id))
            all-items (e/server (get-tree-items* 0 user-id))
            items-by-doc (group-by :content_items/document_id all-items)]
        (if (seq documents)
          (dom/div
            (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"}})
            (e/for-by :documents/id [doc documents]
              (DocumentNode doc (get items-by-doc (:documents/id doc)) !nav-target navigate!)))
          (dom/p
            (dom/props {:style {:color "var(--color-text-secondary)" :font-size "14px"}})
            (dom/text "No content yet. Import a document, then extract content items for study.")))))))
