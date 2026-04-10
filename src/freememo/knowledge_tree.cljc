(ns freememo.knowledge-tree
  "Contents tab — knowledge tree with flatten + virtual scroll."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [freememo.navigation :as nav]
   [freememo.util :as util]
   [freememo.card-components :as card-components]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [clojure.string :as str])))

;; Server wrapper — _refresh param creates Electric reactive dependency
(defn get-tree-items* [_refresh user-id]
  #?(:clj (vec (db/get-knowledge-tree user-id))
     :cljs nil))

#?(:clj
   (defn extract-roots [items]
     (filterv #(nil? (:topics/parent_id %)) items)))

#?(:clj
   (defn filter-root-topics [topics filter-text]
     (if (or (nil? filter-text) (str/blank? filter-text))
       topics
       (let [q (str/lower-case (str/trim filter-text))]
         (filterv #(str/includes? (str/lower-case (or (:topics/title %) "")) q) topics)))))

#?(:clj
   (defn sort-root-topics [topics sort-key]
     (case sort-key
       "recent" (vec (sort-by :topics/created_at #(compare %2 %1) topics))
       "oldest" (vec (sort-by :topics/created_at #(compare %1 %2) topics))
       "alpha" (vec (sort-by #(str/lower-case (or (:topics/title %) "")) topics))
       "done" (vec (sort-by #(if (pos? (:total-items %))
                               (/ (double (:done-items %)) (:total-items %))
                               2.0)
                     topics))
       "synced" (vec (sort-by #(if (pos? (:total-cards %))
                                 (/ (double (:synced-cards %)) (:total-cards %))
                                 2.0)
                       topics))
       topics)))

;; Root progress stats — reuses db/get-document-status for card counts
(defn get-root-stats* [_refresh user-id]
  #?(:clj (into {}
            (map (fn [row]
                   [(:topics/id row)
                    {:total-items (or (:total_items row) 0)
                     :done-items (or (:done_items row) 0)
                     :total-cards (or (:total_cards row) 0)
                     :synced-cards (or (:synced_cards row) 0)}]))
            (db/get-document-status user-id))
     :cljs {}))

#?(:clj
   (defn attach-stats [roots stats-map]
     (mapv (fn [root]
             (merge root (get stats-map (:topics/id root)
                           {:total-items 0 :done-items 0 :total-cards 0 :synced-cards 0})))
       roots)))

#?(:clj
   (defn completion-status [{:keys [done-items total-items total-cards]}]
     (cond
       (and (pos? total-items) (= done-items total-items)) :complete
       (or (pos? done-items) (pos? total-cards)) :in-progress
       :else :not-started)))

#?(:clj
   (defn filter-by-kind [roots kind-filter]
     (if (= kind-filter "all")
       roots
       (filterv #(= (:topics/kind %) kind-filter) roots))))

#?(:clj
   (defn filter-by-status [roots status-filter]
     (case status-filter
       "all" roots
       "unsynced" (filterv #(and (pos? (:total-cards %)) (< (:synced-cards %) (:total-cards %))) roots)
       (filterv #(= (completion-status %) (keyword status-filter)) roots))))

;; Flatten tree into a linear list respecting expand state.
;; Returns [{:depth N :topic <map> :has-children bool :is-root bool}]
(defn flatten-tree [roots children-map expanded-set]
  (loop [stack (mapv (fn [r] {:depth 0 :topic r :is-root true}) (reverse roots))
         result []]
    (if (empty? stack)
      result
      (let [{:keys [depth topic is-root]} (peek stack)
            rest-stack (pop stack)
            id (:topics/id topic)
            kind (:topics/kind topic)
            raw-children (get children-map id)
            ;; For PDFs/EPUBs/web, flatten through page nodes to show extracts directly
            children (if (and is-root (#{"pdf" "epub" "web" "wikipedia"} kind))
                       (vec (mapcat (fn [c]
                                      (if (= "page" (:topics/kind c))
                                        (get children-map (:topics/id c))
                                        [c]))
                              raw-children))
                       raw-children)
            has-children (boolean (seq children))
            expanded? (contains? expanded-set id)
            new-stack (if (and expanded? has-children)
                        (into rest-stack
                          (mapv (fn [c] {:depth (inc depth) :topic c :is-root false})
                            (reverse children)))
                        rest-stack)]
        (recur new-stack
          (conj result {:depth depth :topic topic
                        :has-children has-children :is-root is-root}))))))

;; Badge for topic kind
(defn kind-badge [kind]
  (case kind
    "pdf" ["PDF" "var(--color-badge-pdf)"]
    "epub" ["EPUB" "var(--color-badge-epub)"]
    ("web" "wikipedia") ["Web" "var(--color-badge-web)"]
    "markdown" ["MD" "var(--color-badge-web)"]
    ["Topic" "var(--color-badge-epub)"]))

;; Document tree view — used by LibraryPage
;; Flatten + virtual scroll for performance
(e/defn DocumentTreeView [user-id navigate! refresh filter-text sort-key kind-filter status-filter]
  (e/client
    (e/server
      (let [all-items (get-tree-items* refresh user-id)
            stats-map (get-root-stats* refresh user-id)
            all-roots (extract-roots all-items)
            roots (-> all-roots
                    (attach-stats stats-map)
                    (filter-root-topics filter-text)
                    (filter-by-kind kind-filter)
                    (filter-by-status status-filter)
                    (sort-root-topics sort-key))]
        (e/client
          (let [children-map (group-by :topics/parent_id all-items)
                !expanded-set (atom #{})
                expanded-set (e/watch !expanded-set)
                !show-confirm (atom nil)
                show-confirm (e/watch !show-confirm)
                flat-rows (flatten-tree roots children-map expanded-set)
                row-count (count flat-rows)
                row-height 36
                ;; Ref to scroll container — used to preserve scrollTop across expand/collapse
                !scroll-node (atom nil)]
            (dom/div
              (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0" :scrollbar-gutter "stable"}})
              (reset! !scroll-node dom/node)
              (let [[offset limit] (Scroll-window row-height row-count dom/node {:overquery-factor 1})
                    occluded-height (clamp-left (* row-height (- row-count limit)) 0)]
                (dom/props {:class "tape-scroll"
                            :style {:--offset offset :--row-height (str row-height "px")}})
                (dom/table
                  (dom/props {:style {:width "100%" :font-size "14px"}})
                  (if (pos? row-count)
                    (e/for [i (Tape offset limit)]
                      (let [row (nth flat-rows i nil)]
                        (when row
                          (let [{:keys [depth topic has-children is-root]} row
                                id (:topics/id topic)
                                title (or (:topics/title topic) "(empty)")
                                kind (or (:topics/kind topic) "basic")
                                topic-status (or (:topics/status topic) "active")
                                expanded? (contains? expanded-set id)
                                [badge-text badge-color] (kind-badge kind)
                                display-title (if is-root (util/display-name title) title)]
                            (dom/tr
                              (dom/props {:style {:height (str row-height "px")
                                                  :opacity (case topic-status "done" "0.6" "1")
                                                  :--order (inc i)}})
                              ;; Single cell with indentation
                              (dom/td
                                (dom/props {:style {:display "flex" :align-items "center" :gap "6px"
                                                    :padding-left (str (* depth 20) "px")
                                                    :height (str row-height "px")
                                                    :border-bottom "1px solid var(--color-bg-subtle)"
                                                    :border-left (when (= topic-status "done") "2px solid var(--color-success-lighter)")}})
                                ;; Arrow
                                (if has-children
                                  (dom/span
                                    (dom/props {:style {:width "16px" :font-size "10px" :cursor "pointer"
                                                        :user-select "none" :text-align "center" :flex-shrink "0"}})
                                    (dom/text (if expanded? "\u25BC" "\u25B6"))
                                    (dom/On "click"
                                      (fn [e]
                                        (.stopPropagation e)
                                        (let [sn @!scroll-node
                                              st (when sn (.-scrollTop sn))]
                                          (swap! !expanded-set (fn [s] (if (contains? s id) (disj s id) (conj s id))))
                                          (when st
                                            (js/requestAnimationFrame (fn [] (set! (.-scrollTop sn) st))))))
                                      nil))
                                  (dom/span (dom/props {:style {:width "16px" :flex-shrink "0"}})))
                                ;; Badge
                                (dom/span
                                  (dom/props {:class "type-badge" :style {:background badge-color}})
                                  (dom/text badge-text))
                                ;; Title
                                (dom/span
                                  (dom/props {:class "tree-title"
                                              :tabindex 0
                                              :role "link"
                                              :style {:flex "1" :min-width "0"
                                                      :font-size (if is-root "14px" "13px")
                                                      :font-weight (if is-root "500" "400")
                                                      :color (if is-root "var(--color-text-primary)" "var(--color-text-primary)")
                                                      :cursor "pointer"
                                                      :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}
                                              :title display-title})
                                  (dom/text display-title)
                                  (dom/On "click"
                                    (fn [_]
                                      (if (and is-root (= kind "pdf"))
                                        (navigate! :viewer (nav/nav-browse-pdf id nil :library))
                                        (navigate! :viewer (nav/nav-browse-topic id :library))))
                                    nil)
                                  (dom/On "keydown"
                                    (fn [e]
                                      (when (or (= (.-key e) "Enter") (= (.-key e) " "))
                                        (.preventDefault e)
                                        (.click (.-currentTarget e))))
                                    nil))
                                ;; Done progress (root only)
                                (when is-root
                                  (let [total (:total-items topic)
                                        done (:done-items topic)]
                                    (dom/span
                                      (dom/props {:style {:font-size "11px" :flex-shrink "0" :white-space "nowrap"
                                                          :color (cond
                                                                   (and (pos? total) (= done total)) "var(--color-success-dark)"
                                                                   (pos? done) "var(--color-text-primary)"
                                                                   :else "var(--color-text-secondary)")}
                                                  :data-tooltip "Done"})
                                      (dom/text (str done " / " total)))))
                                ;; Synced progress (root only)
                                (when is-root
                                  (let [total-cards (:total-cards topic)
                                        synced (:synced-cards topic)]
                                    (dom/span
                                      (dom/props {:style {:font-size "11px" :flex-shrink "0" :white-space "nowrap"
                                                          :color "var(--color-text-secondary)"}
                                                  :data-tooltip "Synced"})
                                      (dom/text (if (pos? total-cards)
                                                  (str synced " / " total-cards)
                                                  "\u2013")))))
                                ;; Review button (only for nodes with children)
                                (when has-children
                                  (dom/button
                                    (dom/props {:class "btn btn-sm btn-secondary"
                                                :style {:padding "2px 6px" :font-size "10px" :flex-shrink "0"}
                                                :title "Review this topic and its children"})
                                    (dom/text "Review")
                                    (dom/On "click"
                                      (fn [e]
                                        (.stopPropagation e)
                                        (navigate! :viewer (nav/nav-subset-review id title)))
                                      nil)))
                                ;; Delete button (root only)
                                (when is-root
                                  (dom/button
                                    (dom/props {:class "btn btn-sm btn-danger-fill"
                                                :style {:padding "2px 6px" :font-size "10px" :flex-shrink "0"}})
                                    (dom/text "Delete")
                                    (dom/On "click" (fn [e] (.stopPropagation e) (reset! !show-confirm id)) nil)))))))))
                    (dom/tr
                      (dom/td
                        (dom/props {:style {:grid-column "1 / -1" :text-align "center" :padding "24px 12px"
                                            :color "var(--color-text-secondary)" :font-size "14px"}})
                        (dom/text "No content yet. Import content from the Import tab.")))))
                (dom/div (dom/props {:style {:height (str occluded-height "px")}}))
                ;; Delete confirm dialog
                (when (some? show-confirm)
                  (dom/div
                    (dom/props {:class "modal-backdrop"})
                    (dom/On "click" (fn [_] (reset! !show-confirm nil)) nil)
                    (dom/On "keydown" (fn [e] (when (= (.-key e) "Escape") (reset! !show-confirm nil))) nil)
                    (dom/div
                      (dom/props {:class "modal-content modal-sm"})
                      (dom/On "click" (fn [e] (.stopPropagation e)) nil)
                      (dom/div
                        (dom/props {:class "confirm-modal-body"})
                        (dom/p (dom/text "Delete this topic? All children, extracts, and cards will be permanently removed.")))
                      (dom/div
                        (dom/props {:class "confirm-modal-actions"})
                        (dom/button
                          (dom/props {:class "btn btn-secondary"})
                          (dom/text "Cancel")
                          (dom/On "click" (fn [_] (reset! !show-confirm nil)) nil))
                        (dom/button
                          (dom/props {:class "btn btn-danger-fill"})
                          (dom/text "Delete")
                          (let [event (dom/On "click" (fn [_] show-confirm) nil)
                                [?token ?error] (e/Token event)]
                            (when-some [token ?token]
                              (let [topic-to-delete event
                                    note-ids (e/server (vec (db/get-all-anki-note-ids topic-to-delete)))]
                                (e/server (db/delete-topic-for-user! user-id topic-to-delete))
                                (e/server (swap! (us/get-atom user-id :refresh) inc))
                                (e/client (card-components/try-delete-anki-notes! note-ids))
                                (token)
                                (reset! !show-confirm nil)))))))))))))))))


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
            (dom/text "Use Library tab for tree view."))
          (dom/p
            (dom/props {:style {:color "var(--color-text-secondary)" :font-size "14px"}})
            (dom/text "No content yet. Import a document, then extract content for study.")))))))
