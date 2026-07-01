(ns freememo.hierarchy-side-panel
  "Left side panel for the viewer — shows the parent + immediate siblings of
   the current page topic, plus the current page's extract children. User can
   expand other siblings to see their extracts.

   Reactivity: subscribes to the per-user :tree-mutations atom (tree shape) and
   :meta-refresh (done-status, which drives the strike-through) — NOT :refresh.
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
   [freememo.util :as util]
   [freememo.viewport :as viewport]
   [freememo.tree-dnd :as tree-dnd]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.topic-move :as topic-move])
   #?(:clj [freememo.user-state :as us])))

#?(:clj
   (defn- slim-topic [t]
     (dissoc t :topics/content :topics/content_text)))

(declare build-rows)

(defn build-hierarchy-rows*
  "Server pipeline — subtree query + group-by + depth-first flatten, all
   sited here so the full topic list never crosses the wire; the client
   reads row-count and the virtual-scroll window rows only.
   `expanded-set` is the client's manually-expanded node ids (bounded).
   `_tree-rev` is the watched value of (us/get-atom user-id :tree-mutations);
   bumping it re-runs this query. Returns a row vec or nil when the topic
   has no resolvable root."
  [_tree-rev user-id page-topic-id expanded-set]
  #?(:clj
     (when (and user-id page-topic-id)
       (when-let [root-id (db/get-root-topic-id page-topic-id)]
         (when-let [scope-root (db/get-topic root-id)]
           (let [items (mapv slim-topic (db/get-subtree user-id root-id))
                 children-map (group-by :topics/parent_id items)]
             (build-rows (slim-topic scope-root) page-topic-id
               children-map (set expanded-set) items)))))
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
    "audio"      ["Audio"   "var(--color-badge-web)"]
    "basic"      ["E"       "var(--color-badge-epub)"]
    nil))

(defn build-rows
  "Pure: depth-first flatten of the subtree rooted at scope-root.
   Returns a vec of {:depth :topic :has-children :current? :expanded-display?}
   rows. :expanded-display? mirrors the arrow-glyph rule the panel renders
   (current row or manually expanded), which intentionally differs from the
   walk's descend rule (root / path-to-current / manually expanded).

   A node's children are rendered when the node is on the path from
   scope-root to current-id, or when the user explicitly expanded it.
   scope-root is always treated as expanded.

   Page nodes are flattened away at the root: for a pdf/epub/web/wikipedia
   scope-root, each `page` child is replaced by that page's own children (the
   extracts), so pages are not rendered and extracts appear directly under the
   root. Mirrors DocumentTreeView (knowledge_tree.cljc)."
  [scope-root current-id children-map expanded-set items]
  (let [id->parent (into {} (map (juxt :topics/id :topics/parent_id)) items)
        path-set   (loop [id current-id acc (transient #{})]
                     (if (or (nil? id) (contains? acc id))
                       (persistent! acc)
                       (recur (get id->parent id) (conj! acc id))))]
    (letfn [(child-nodes [node depth]
              (let [raw (vec (get children-map (:topics/id node)))]
                (if (and (zero? depth)
                      (#{"pdf" "epub" "web" "wikipedia"} (:topics/kind node)))
                  (vec (mapcat (fn [c]
                                 (if (= "page" (:topics/kind c))
                                   (get children-map (:topics/id c))
                                   [c]))
                         raw))
                  raw)))
            (walk [node depth]
              (let [id   (:topics/id node)
                    kids (child-nodes node depth)
                    has? (boolean (seq kids))
                    row  {:depth depth
                          :topic node
                          :has-children has?
                          :current? (= id current-id)
                          :expanded-display? (or (= id current-id)
                                               (contains? expanded-set id))}
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
      (let [phone?          (e/watch viewport/!phone?)
            persisted-open? (e/server (settings/get-hierarchy-open user-id root-topic-id))
            ;; Phone defaults to collapsed regardless of persisted desktop pref.
            ;; The toggle still works — user can open it manually on phone.
            initial-open?   (and (not phone?) persisted-open?)
            !open?          (atom initial-open?)
            open?           (e/watch !open?)
            !save           (atom nil)
            save-val        (e/watch !save)
            [?save-token _] (e/Token save-val)
            persisted-width (e/server (settings/get-hierarchy-width user-id root-topic-id))
            !width-px       (atom persisted-width)
            width-px        (e/watch !width-px)
            !width-save     (atom nil)
            width-save      (e/watch !width-save)
            [?width-token _] (e/Token width-save)]

        (when-some [token ?save-token]
          (e/server (settings/save-hierarchy-open user-id root-topic-id save-val))
          (token))

        (when-some [token ?width-token]
          (e/server (settings/save-hierarchy-width user-id root-topic-id width-save))
          (token))

        (dom/div
          (dom/props {:class (str "hierarchy-side-panel"
                               (when-not open? " hierarchy-side-panel--collapsed"))
                      :style (merge {:position "relative"}
                               (when open? {:width (str width-px "px")}))})

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

          ;; Resize handle on the inner (right) edge; only when open.
          (when open?
            (dom/div
              (dom/props {:class "side-panel__resize side-panel__resize--right"
                          :title "Drag to resize"})
              (dom/On "pointerdown"
                (fn [e]
                  (util/start-drag-px! e !width-px
                    {:min 180
                     :max (max 180 (util/panel-resize-max (.-currentTarget e) :after 320))
                     :invert? false
                     :on-commit #(reset! !width-save %)}))
                nil)))

          (when open?
            (if (nil? page-topic-id)
              (dom/div
                (dom/props {:style {:padding "16px 12px" :font-size "13px"
                                    :color "var(--color-text-secondary)"}})
                (dom/text "No page selected."))

              ;; :tree-mutations = tree shape; :meta-refresh = done-status (drives
              ;; the strike-through). Both re-run the rows query.
              (let [tree-rev   (e/server (+ (e/watch (us/get-atom user-id :tree-mutations))
                                           (e/watch (us/get-atom user-id :meta-refresh))))
                    !expanded-set (atom #{})
                    expanded-set (e/watch !expanded-set)
                    ;; Server-form binding (sited-by-use): the subtree rows
                    ;; stay server-side; only row-count and the window rows
                    ;; cross. expanded-set crosses upward (bounded).
                    rows (e/server (build-hierarchy-rows* tree-rev user-id
                                     page-topic-id expanded-set))]
                (when (e/server (some? rows))
                  (let [row-count (e/server (count rows))
                        row-height 36
                        !scroll-node (atom nil)
                        ;; Drag-and-drop nesting (no promote-to-root here — the
                        ;; hierarchy tab has no root target by design).
                        !drag-src (atom nil)
                        drag-src (e/watch !drag-src)
                        forbidden (e/server (if drag-src (set (db/get-subtree-ids drag-src)) #{}))
                        !drop-cmd (atom nil)
                        drop-cmd (e/watch !drop-cmd)]

                (let [[t _] (e/Token drop-cmd)]
                  (when t
                    (let [{:keys [src dst]} drop-cmd
                          ok (e/server (e/Offload #(topic-move/move-topic! user-id src dst)))]
                      (case ok
                        (case (e/client (reset! !drop-cmd nil)) (t))))))

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
                        (let [row (e/server (nth rows i nil))]
                          (when row
                            (let [{:keys [depth topic has-children current? expanded-display?]} row
                                  id (:topics/id topic)
                                  title (or (:topics/title topic) "(empty)")
                                  kind (or (:topics/kind topic) "basic")
                                  is-page (= kind "page")
                                  page-num (:topics/page_number topic)
                                  topic-status (or (:topics/status topic) "active")
                                  done? (= topic-status "done")
                                  expanded? expanded-display?
                                  badge (side-panel-badge kind)]
                              (dom/tr
                                (dom/props {:style {:--order (inc i)}})
                                (dom/td
                                  (dom/props {:style {:display "flex" :align-items "center" :gap "6px"
                                                      :padding-left (str (+ 6 (* depth 14)) "px")
                                                      :padding-right "8px"
                                                      :cursor (if current? "default" "pointer")
                                                      :background (when current? "var(--color-bg-card)")
                                                      :border-left "3px solid transparent"
                                                      :outline (when current? "2px solid var(--color-primary)")
                                                      :outline-offset (when current? "-2px")
                                                      :font-weight (if current? "600" "400")}})
                                  ;; DnD re-parenting on this cell (tr is
                                  ;; display:contents → not draggable). Page stubs
                                  ;; are structural — not draggable.
                                  (tree-dnd/DragDropRow! id (not is-page)
                                    !drag-src drag-src forbidden !drop-cmd)
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
                                          (when-not current?
                                            (let [sn @!scroll-node
                                                  st (when sn (.-scrollTop sn))]
                                              (swap! !expanded-set
                                                (fn [s] (if (contains? s id) (disj s id) (conj s id))))
                                              ;; Anchor scroll across the async re-render (see
                                              ;; util/restore-scroll-after-render!): a single rAF
                                              ;; resets scrollTop to 0; re-apply over a few frames.
                                              (util/restore-scroll-after-render! sn st))))
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
                                                        :color "var(--color-text-primary)"
                                                        :text-decoration (when done? "line-through")}})
                                    (dom/text title))))))))
                      (dom/tr
                        (dom/props {:style {:--order 1}})
                        (dom/td
                          (dom/props {:style {:padding "16px 12px" :text-align "center"
                                              :color "var(--color-text-secondary)" :font-size "13px"}})
                          (dom/text "No items.")))))
                  (dom/div (dom/props {:style {:height (str occluded-height "px")}}))))))))))))))
