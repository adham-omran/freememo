(ns freememo.hierarchy-side-panel
  "Left side panel for the viewer — shows the parent + immediate siblings of
   the current page topic, plus the current page's extract children. User can
   expand other siblings to see their extracts.

   Reactivity: subscribes to the per-user :tree-mutations atom (NOT :refresh).
   :tree-mutations is bumped only when the topic tree shape changes (extract
   create/delete, rename, document import). Decoupling from :refresh keeps the
   rich-text editor's `topic` re-fetch out of the side-panel update path so
   extracting does not destroy and recreate the Quill instance.

   Open/collapsed state is persisted per-topic via freememo.settings."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [freememo.navigation :as nav]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.user-state :as us])))

#?(:clj
   (defn- slim-topic [t]
     (dissoc t :topics/content :topics/content_text)))

(defn get-hierarchy-data*
  "Server fetch — single subtree query rooted at the document root (topmost
   ancestor) of the current page topic. Strips :content / :content_text to
   keep the WebSocket payload small.
   `_tree-rev` is the watched value of (us/get-atom user-id :tree-mutations);
   bumping it re-runs this query."
  [_tree-rev user-id page-topic-id]
  #?(:clj
     (when (and user-id page-topic-id)
       (when-let [root-id (db/get-root-topic-id page-topic-id)]
         (when-let [scope-root (db/get-topic root-id)]
           (let [items (mapv slim-topic (db/get-subtree user-id root-id))]
             {:scope-root (slim-topic scope-root)
              :current-id page-topic-id
              :items      items}))))
     :cljs nil))

(defn side-panel-badge
  "Returns [text color] or nil to skip rendering. Page rows skip the badge
   because the row already shows 'p.<n>' which is more informative."
  [kind]
  (case kind
    "pdf"        ["PDF"     "var(--color-badge-pdf)"]
    "epub"       ["EPUB"    "var(--color-badge-epub)"]
    ("web" "wikipedia") ["Web" "var(--color-badge-web)"]
    "markdown"   ["MD"      "var(--color-badge-web)"]
    "basic"      ["E"       "var(--color-badge-epub)"]
    nil))

(defn build-rows
  "Pure: depth-first flatten of the subtree rooted at scope-root.
   Returns a vec of {:depth :topic :has-children :current?} rows.

   A node's children are rendered when the node is on the path from
   scope-root to current-id, or when the user explicitly expanded it.
   scope-root is always treated as expanded."
  [scope-root current-id children-map expanded-set items]
  (let [id->parent (into {} (map (juxt :topics/id :topics/parent_id)) items)
        path-set   (loop [id current-id acc (transient #{})]
                     (if (or (nil? id) (contains? acc id))
                       (persistent! acc)
                       (recur (get id->parent id) (conj! acc id))))]
    (letfn [(walk [node depth]
              (let [id   (:topics/id node)
                    kids (vec (get children-map id))
                    has? (boolean (seq kids))
                    row  {:depth depth
                          :topic node
                          :has-children has?
                          :current? (= id current-id)}
                    expanded? (or (zero? depth)
                                (contains? path-set id)
                                (contains? expanded-set id))]
                (if (and expanded? has?)
                  (cons row (mapcat #(walk % (inc depth)) kids))
                  [row])))]
      (vec (walk scope-root 0)))))

(e/defn HierarchySidePanel [user-id page-topic-id root-topic-id navigate! !nav-target]
  (e/client
    ;; Frame isolation — remounts only when root-topic-id changes (i.e. when
    ;; navigating between documents). Page navigation within one document
    ;; updates :current? reactively without remount, so !open? state and the
    ;; DOM subtree persist across page scrolls.
    (e/for-by identity [_k [root-topic-id]]
      (let [initial-open?   (e/server (settings/get-hierarchy-open user-id root-topic-id))
            !open?          (atom initial-open?)
            open?           (e/watch !open?)
            !save           (atom nil)
            save-val        (e/watch !save)
            [?save-token _] (e/Token save-val)]

        (when-some [token ?save-token]
          (e/server (settings/save-hierarchy-open user-id root-topic-id save-val))
          (token))

        (dom/div
          (dom/props {:class (str "hierarchy-side-panel"
                               (when-not open? " hierarchy-side-panel--collapsed"))})

          ;; Header row — toggle (always visible) + title (only when open).
          ;; The row renders even when collapsed so the 32px gutter still
          ;; surfaces the hamburger.
          (dom/div
            (dom/props {:class "side-panel__header"})
            (dom/button
              (dom/props {:class "side-panel__toggle"})
              (dom/text "☰")
              (dom/On "click"
                (fn [_]
                  (let [next-open? (not @!open?)]
                    (reset! !open? next-open?)
                    (reset! !save next-open?)))
                nil))
            (when open?
              (dom/span
                (dom/props {:class "side-panel__title"})
                (dom/text "Hierarchy"))))

          (when open?
            (if (nil? page-topic-id)
              (dom/div
                (dom/props {:style {:padding "16px 12px" :font-size "13px"
                                    :color "var(--color-text-secondary)"}})
                (dom/text "No page selected."))

              (let [tree-rev   (e/server (e/watch (us/get-atom user-id :tree-mutations)))
                    data       (e/server (get-hierarchy-data* tree-rev user-id page-topic-id))
                    scope-root (:scope-root data)
                    current-id (:current-id data)
                    items      (vec (:items data))]
                (when scope-root
                  (let [children-map (group-by :topics/parent_id items)
                        !expanded-set (atom #{})
                        expanded-set (e/watch !expanded-set)
                        rows (build-rows scope-root current-id children-map expanded-set items)
                        row-count (count rows)
                        row-height 36
                        !scroll-node (atom nil)]

              (dom/div
                (dom/props {:class "tape-scroll"
                            :style {:flex "1" :overflow-y "auto" :min-height "0"
                                    :scrollbar-gutter "stable"}})
                (reset! !scroll-node dom/node)
                (let [[offset limit] (Scroll-window row-height row-count dom/node {:overquery-factor 1})
                      occluded-height (clamp-left (* row-height (- row-count limit)) 0)]
                  (dom/props {:style {:--offset offset
                                      :--row-height (str row-height "px")}})
                  (dom/table
                    (dom/props {:style {:width "100%"
                                        :grid-template-columns "1fr"
                                        :font-size "13px"}})
                    (if (pos? row-count)
                      (e/for [i (Tape offset limit)]
                        (let [row (nth rows i nil)]
                          (when row
                            (let [{:keys [depth topic has-children current?]} row
                                  id (:topics/id topic)
                                  title (or (:topics/title topic) "(empty)")
                                  kind (or (:topics/kind topic) "basic")
                                  is-page (= kind "page")
                                  page-num (:topics/page_number topic)
                                  topic-status (or (:topics/status topic) "active")
                                  done? (= topic-status "done")
                                  expanded? (or (= id current-id)
                                              (contains? expanded-set id))
                                  badge (side-panel-badge kind)]
                              (dom/tr
                                (dom/props {:style {:--order (inc i)}})
                                (dom/td
                                  (dom/props {:style {:display "flex" :align-items "center" :gap "6px"
                                                      :padding-left (str (+ 6 (* depth 14)) "px")
                                                      :padding-right "8px"
                                                      :cursor (if current? "default" "pointer")
                                                      :background (when current? "var(--color-bg-card)")
                                                      :border-left (if done?
                                                                     "3px solid var(--color-success-lighter)"
                                                                     "3px solid transparent")
                                                      :opacity (when done? "0.6")
                                                      :outline (when current? "2px solid var(--color-primary)")
                                                      :outline-offset (when current? "-2px")
                                                      :font-weight (if current? "600" "400")}})
                                  (dom/On "click"
                                    (fn [_]
                                      (when-not current?
                                        (cond
                                          ;; Same-document page jump (PDF/EPUB sibling pages) — only when caller provided !nav-target
                                          (and is-page page-num !nav-target)
                                          (reset! !nav-target {:topic-id (:topics/parent_id topic)
                                                               :page page-num})
                                          ;; Otherwise navigate to the topic
                                          navigate!
                                          (navigate! :viewer (nav/nav-topic id nil)))))
                                    nil)

                                  (if has-children
                                    (dom/span
                                      (dom/props {:style {:width "14px" :font-size "10px" :flex-shrink "0"
                                                          :user-select "none" :text-align "center"
                                                          :cursor "pointer"
                                                          :color "var(--color-text-secondary)"}})
                                      (dom/text (if expanded? "▼" "▶"))
                                      (dom/On "click"
                                        (fn [e]
                                          (.stopPropagation e)
                                          (when-not (= id current-id)
                                            (let [sn @!scroll-node
                                                  st (when sn (.-scrollTop sn))]
                                              (swap! !expanded-set
                                                (fn [s] (if (contains? s id) (disj s id) (conj s id))))
                                              (when st
                                                (js/requestAnimationFrame
                                                  (fn [] (set! (.-scrollTop sn) st)))))))
                                        nil))
                                    (dom/span
                                      (dom/props {:style {:width "14px" :flex-shrink "0"}})))

                                  (when (and is-page page-num)
                                    (dom/span
                                      (dom/props {:style {:font-size "11px"
                                                          :color "var(--color-text-secondary)"
                                                          :flex-shrink "0" :font-variant-numeric "tabular-nums"}})
                                      (dom/text "p." page-num)))

                                  (when badge
                                    (let [[badge-text badge-color] badge]
                                      (dom/span
                                        (dom/props {:class "type-badge"
                                                    :style {:background badge-color}})
                                        (dom/text badge-text))))

                                  (dom/span
                                    (dom/props {:style {:overflow "hidden"
                                                        :text-overflow "ellipsis"
                                                        :white-space "nowrap"
                                                        :flex "1" :min-width "0"
                                                        :color "var(--color-text-primary)"}})
                                    (dom/text title))))))))
                      (dom/tr
                        (dom/props {:style {:--order 1}})
                        (dom/td
                          (dom/props {:style {:padding "16px 12px" :text-align "center"
                                              :color "var(--color-text-secondary)" :font-size "13px"}})
                          (dom/text "No items.")))))
                  (dom/div (dom/props {:style {:height (str occluded-height "px")}}))))))))))))))
