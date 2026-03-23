(ns electric-starter-app.contents-page
  "Contents tab — knowledge tree showing topics and sub-topics."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.util :as util]
   #?(:clj [electric-starter-app.db :as db])
   #?(:clj [clojure.string :as str])))

;; Server wrapper — _refresh param creates Electric reactive dependency
(defn get-tree-items* [_refresh user-id]
  #?(:clj (vec (db/get-knowledge-tree user-id))
     :cljs nil))

;; Truncate HTML content to plain text preview
(defn content-preview [content max-len]
  (when (and content (seq content))
    (let [text #?(:cljs (let [tmp (js/document.createElement "div")]
                          (set! (.-innerHTML tmp) content)
                          (.-textContent tmp))
                  :clj (clojure.string/replace content #"<[^>]*>" ""))
          trimmed (if (> (count text) max-len)
                    (str (subs text 0 max-len) "...")
                    text)]
      trimmed)))

#?(:clj
   (defn extract-roots [items]
     (filterv #(nil? (:topics/parent_id %)) items)))

#?(:clj
   (defn filter-root-topics [topics filter-text]
     (if (or (nil? filter-text) (str/blank? filter-text))
       topics
       (let [q (str/lower-case (str/trim filter-text))]
         (filterv #(str/includes? (str/lower-case (or (:topics/title %) "")) q) topics)))))

;; Child topic node — recursive component
(e/defn TopicChildNode [item children-map depth !nav-target navigate!]
  (e/client
    (let [id (:topics/id item)
          children (get children-map id)
          has-children (boolean (seq children))
          item-status (or (:topics/status item) "active")
          item-kind (or (:topics/kind item) "basic")
          preview (or (:topics/title item) "(empty)")
          !expanded (atom false)
          expanded (e/watch !expanded)]
      (dom/div
        (dom/props {:style {:padding-left "20px"}})
        ;; Row
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :gap "6px"
                              :padding "4px 0" :border-bottom "1px solid #f5f5f5"
                              :opacity (case item-status "done" "0.6" "1")
                              :border-left (when (= item-status "done") "2px solid #86efac")
                              :padding-left (when (= item-status "done") "8px")}})
          ;; Arrow
          (if has-children
            (dom/span
              (dom/props {:style {:width "16px" :font-size "10px" :cursor "pointer"
                                  :user-select "none" :text-align "center" :flex-shrink "0"}})
              (dom/text (if expanded "\u25BC" "\u25B6"))
              (dom/On "click" (fn [e] (.stopPropagation e) (swap! !expanded not)) nil))
            (dom/span (dom/props {:style {:width "16px" :flex-shrink "0"}})))
          ;; Type badge
          (let [[badge-label badge-color]
                (case item-kind
                  "basic" ["Topic" "#f3e8ff"]
                  ("web" "wikipedia") ["Web" "#e0f2fe"]
                  "epub" ["EPUB" "#f3e8ff"]
                  "pdf" ["PDF" "#dcfce7"]
                  ["Topic" "#f3e8ff"])]
            (dom/span
              (dom/props {:class "type-badge" :style {:background badge-color}})
              (dom/text badge-label)))
          ;; Content preview — click opens
          (dom/span
            (dom/props {:style {:font-size "13px" :color "#333" :cursor "pointer"
                                :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}
                        :title "Click to open"})
            (dom/On "mouseenter" (fn [e] (set! (.-textDecoration (.-style (.-target e))) "underline")) nil)
            (dom/On "mouseleave" (fn [e] (set! (.-textDecoration (.-style (.-target e))) "none")) nil)
            (dom/text preview)
            (dom/On "click"
              (fn [_]
                (reset! !nav-target {:topic-id id})
                (navigate! :extract))
              nil))
          ;; Review button — only for nodes with children
          (when has-children
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary"
                          :style {:padding "2px 6px" :font-size "10px" :flex-shrink "0"}
                          :title "Review this topic and its children"})
              (dom/text "Review")
              (dom/On "click"
                (fn [e]
                  (.stopPropagation e)
                  (reset! !nav-target {:subset-review {:root-id id
                                                       :root-name preview}})
                  (navigate! :learn))
                nil))))
        ;; Children
        (when (and expanded has-children)
          (e/for-by :topics/id [child children]
            (TopicChildNode child children-map (inc depth) !nav-target navigate!)))))))

;; Root topic node
(e/defn TopicRootNode [topic children-map !nav-target navigate!]
  (e/client
    (let [topic-id (:topics/id topic)
          title (or (:topics/title topic) "Untitled")
          kind (or (:topics/kind topic) "basic")
          topic-status (or (:topics/status topic) "active")
          type-label (case kind "pdf" "PDF" "web" "Web" "wikipedia" "Web" "epub" "EPUB" "basic" "Topic" "Topic")
          type-color (case kind "pdf" "#dcfce7" "web" "#e0f2fe" "wikipedia" "#e0f2fe" "epub" "#f3e8ff" "basic" "#f3e8ff" "#f3e8ff")
          raw-children (get children-map topic-id)
          ;; For PDFs/EPUBs, flatten through page nodes to show extracts directly
          children (if (#{"pdf" "epub" "web" "wikipedia"} kind)
                     (vec (mapcat (fn [child]
                                    (if (= "page" (:topics/kind child))
                                      (get children-map (:topics/id child))
                                      [child]))
                            raw-children))
                     raw-children)
          has-children (boolean (seq children))
          !expanded (atom false)
          expanded (e/watch !expanded)]
      (dom/div
        ;; Root topic row
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                              :padding "6px 0" :border-bottom "1px solid #e8e8e8"
                              :opacity (case topic-status "done" "0.6" "1")
                              :border-left (when (= topic-status "done") "2px solid #86efac")
                              :padding-left (when (= topic-status "done") "8px")}})
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
          ;; Title — click opens
          (dom/span
            (dom/props {:style {:font-size "14px" :font-weight "500" :color "#222" :cursor "pointer"}
                        :title "Click to open"})
            (dom/On "mouseenter" (fn [e] (set! (.-textDecoration (.-style (.-target e))) "underline")) nil)
            (dom/On "mouseleave" (fn [e] (set! (.-textDecoration (.-style (.-target e))) "none")) nil)
            (dom/text (util/display-name title))
            (dom/On "click"
              (fn [_]
                (reset! !nav-target {:topic-id topic-id :kind kind :title title})
                (navigate! :learn))
              nil))
          ;; Review button
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"
                        :style {:padding "2px 8px" :font-size "11px" :flex-shrink "0"}
                        :title "Review all topics under this document"})
            (dom/text "Review")
            (dom/On "click"
              (fn [e]
                (.stopPropagation e)
                (reset! !nav-target {:subset-review {:root-id topic-id
                                                     :root-name title}})
                (navigate! :learn))
              nil)))
        ;; Children
        (when (and expanded has-children)
          (e/for-by :topics/id [child children]
            (TopicChildNode child children-map 1 !nav-target navigate!)))))))

;; Document tree view — used by LibraryPage
;; Receives !refresh and filter-text from parent
(e/defn DocumentTreeView [user-id !nav-target navigate! !refresh filter-text]
  (e/client
    (e/server
      (let [refresh (e/watch !refresh)
            all-items (get-tree-items* refresh user-id)
            all-roots (extract-roots all-items)
            roots (filter-root-topics all-roots filter-text)]
        (e/client
          (let [children-map (group-by :topics/parent_id all-items)]
            (if (seq roots)
              (dom/div
                (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"}})
                (e/for-by :topics/id [topic roots]
                  (TopicRootNode topic children-map !nav-target navigate!)))
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

      (let [all-items (e/server (get-tree-items* 0 user-id))
            children-map (group-by :topics/parent_id all-items)
            roots (get children-map nil)]
        (if (seq roots)
          (dom/div
            (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"}})
            (e/for-by :topics/id [topic roots]
              (TopicRootNode topic children-map !nav-target navigate!)))
          (dom/p
            (dom/props {:style {:color "var(--color-text-secondary)" :font-size "14px"}})
            (dom/text "No content yet. Import a document, then extract content for study.")))))))
