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
   [freememo.pdf-cache :as pdf-cache]
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
   (defn sort-root-topics [topics sort-col sort-dir]
     (let [key-fn (case sort-col
                    :document #(str/lower-case (or (:topics/title %) ""))
                    :done #(if (pos? (:total-items %))
                             (/ (double (:done-items %)) (:total-items %))
                             2.0)
                    :synced #(if (pos? (:total-cards %))
                               (/ (double (:synced-cards %)) (:total-cards %))
                               2.0)
                    :added :topics/created_at
                    :topics/created_at)
           cmp (if (= sort-dir :asc) compare (fn [a b] (compare b a)))]
       (vec (sort-by key-fn cmp topics)))))

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
(e/defn DocumentTreeView [user-id navigate! refresh filter-text sort-col sort-dir kind-filter status-filter tree-expanded !sort-cmd]
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
                    (sort-root-topics sort-col sort-dir))
            ;; Strip page topics to structural keys only — cuts ~70% of WebSocket payload
            slim-items (mapv (fn [item]
                               (if (= "page" (:topics/kind item))
                                 (select-keys item [:topics/id :topics/parent_id :topics/kind])
                                 item))
                         all-items)]
        (e/client
          (let [children-map (group-by :topics/parent_id slim-items)
                !expanded-set (atom #{})
                expand-cmd (if tree-expanded
                             (reset! !expanded-set (disj (set (keys children-map)) nil))
                             (reset! !expanded-set #{}))
                expanded-set (do expand-cmd (e/watch !expanded-set))
                !show-confirm (atom nil)
                show-confirm (e/watch !show-confirm)
                flat-rows (flatten-tree roots children-map expanded-set)
                row-count (count flat-rows)
                row-height 36
                grid-cols "1fr 70px 80px 80px 110px"
                !scroll-node (atom nil)]
            (dom/div
              (dom/props {:style {:display "flex" :flex-direction "column" :min-height "0" :flex "1"}})

              ;; Fixed header
              (dom/table
                (dom/props {:style {:width "100%" :display "grid" :grid-template-columns grid-cols :flex-shrink "0"}})
                (dom/thead
                  (dom/props {:style {:display "contents"}})
                  (dom/tr
                    (dom/props {:style {:display "contents"}})
                    (let [th-style {:padding "8px 10px" :border-bottom "2px solid var(--color-border)"
                                    :font-weight "600" :font-size "13px" :color "var(--color-text-primary)"
                                    :cursor "pointer" :user-select "none"}
                          arrow (fn [col] (when (= sort-col col) (if (= sort-dir :asc) " \u25B2" " \u25BC")))]
                      (dom/th
                        (dom/props {:style (merge th-style {:text-align "left"})})
                        (dom/text (str "Document" (arrow :document)))
                        (dom/On "click" (fn [_] (reset! !sort-cmd [:document :asc])) nil))
                      (dom/th
                        (dom/props {:style (merge th-style {:text-align "center" :padding "8px 6px"})})
                        (dom/text (str "Done" (arrow :done)))
                        (dom/On "click" (fn [_] (reset! !sort-cmd [:done :asc])) nil))
                      (dom/th
                        (dom/props {:style (merge th-style {:text-align "center" :padding "8px 6px"})})
                        (dom/text (str "Synced" (arrow :synced)))
                        (dom/On "click" (fn [_] (reset! !sort-cmd [:synced :asc])) nil))
                      (dom/th
                        (dom/props {:style (merge th-style {:text-align "right" :padding "8px 6px"})})
                        (dom/text (str "Added" (arrow :added)))
                        (dom/On "click" (fn [_] (reset! !sort-cmd [:added :desc])) nil))
                      (dom/th
                        (dom/props {:style {:padding "8px 6px" :border-bottom "2px solid var(--color-border)"}})
                        (dom/text ""))))))

              ;; Scrollable body
              (dom/div
                (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0" :scrollbar-gutter "stable"}})
                (reset! !scroll-node dom/node)
                (let [[offset limit] (Scroll-window row-height row-count dom/node {:overquery-factor 1})
                      occluded-height (clamp-left (* row-height (- row-count limit)) 0)]
                  (dom/props {:class "tape-scroll"
                              :style {:--offset offset :--row-height (str row-height "px")}})
                  (dom/table
                    (dom/props {:style {:width "100%" :display "grid" :grid-template-columns grid-cols :font-size "13px"}})
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
                                (dom/props {:style {:border-bottom "1px solid var(--color-bg-subtle)"
                                                    :height (str row-height "px")
                                                    :opacity (case topic-status "done" "0.6" "1")
                                                    :cursor "pointer"
                                                    :--order (inc i)}})
                                (dom/On "click"
                                  (fn [_]
                                    (if (and is-root (= kind "pdf"))
                                      (navigate! :viewer (nav/nav-browse-pdf id nil :library))
                                      (navigate! :viewer (nav/nav-browse-topic id :library))))
                                  nil)
                                ;; Column 1: Document (arrow + badge + title)
                                (dom/td
                                  (dom/props {:style {:display "flex" :align-items "center" :gap "6px"
                                                      :padding-left (str (+ 10 (* depth 20)) "px")
                                                      :overflow "hidden"
                                                      :border-left (when (= topic-status "done") "2px solid var(--color-success-lighter)")}})
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
                                  (dom/span
                                    (dom/props {:class "type-badge" :style {:background badge-color}})
                                    (dom/text badge-text))
                                  (dom/span
                                    (dom/props {:style {:overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"
                                                        :font-size (if is-root "13px" "12px")
                                                        :font-weight (if is-root "500" "400")}
                                                :data-tooltip display-title})
                                    (dom/text display-title)))
                                ;; Column 2: Done
                                (dom/td
                                  (dom/props {:style {:padding "4px 6px" :text-align "center"
                                                      :display "flex" :align-items "center" :justify-content "center"}})
                                  (when is-root
                                    (let [total (:total-items topic)
                                          done (:done-items topic)]
                                      (dom/span
                                        (dom/props {:style {:color (cond
                                                                     (and (pos? total) (= done total)) "var(--color-success-dark)"
                                                                     (pos? done) "var(--color-text-primary)"
                                                                     :else "var(--color-text-secondary)")}})
                                        (dom/text (str done " / " total))))))
                                ;; Column 3: Synced
                                (dom/td
                                  (dom/props {:style {:padding "4px 6px" :text-align "center"
                                                      :display "flex" :align-items "center" :justify-content "center"
                                                      :color "var(--color-text-secondary)"}})
                                  (when is-root
                                    (let [total-cards (:total-cards topic)
                                          synced (:synced-cards topic)]
                                      (dom/span
                                        (dom/text (if (pos? total-cards)
                                                    (str synced " / " total-cards)
                                                    "\u2013"))))))
                                ;; Column 4: Added
                                (dom/td
                                  (dom/props {:style {:padding "4px 6px" :text-align "right"
                                                      :display "flex" :align-items "center" :justify-content "flex-end"
                                                      :color "var(--color-text-secondary)"}})
                                  (when is-root
                                    (dom/span
                                      (dom/text (or (:formatted_date topic) "")))))
                                ;; Column 5: Actions
                                (dom/td
                                  (dom/props {:style {:padding "4px 6px" :display "flex" :align-items "center"
                                                      :justify-content "flex-end" :gap "4px"}})
                                  (when has-children
                                    (dom/button
                                      (dom/props {:class "btn btn-sm btn-secondary"
                                                  :style {:padding "2px 6px" :font-size "10px"}})
                                      (dom/text "Review")
                                      (dom/On "click"
                                        (fn [e]
                                          (.stopPropagation e)
                                          (navigate! :viewer (nav/nav-subset-review id title)))
                                        nil)))
                                  (when is-root
                                    (dom/button
                                      (dom/props {:class "btn btn-sm btn-danger-fill"
                                                  :style {:padding "2px 6px" :font-size "10px"}})
                                      (dom/text "Delete")
                                      (dom/On "click" (fn [e] (.stopPropagation e) (reset! !show-confirm id)) nil)))))))))
                      (dom/tr
                        (dom/td
                          (dom/props {:style {:grid-column "1 / -1" :text-align "center" :padding "24px 12px"
                                              :color "var(--color-text-secondary)" :font-size "13px"}})
                          (dom/text "No content yet. Import content from the Import tab.")))))
                  (dom/div (dom/props {:style {:height (str occluded-height "px")}}))))

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
                              (e/client (pdf-cache/cache-delete topic-to-delete))
                              (token)
                              (reset! !show-confirm nil))))))))))))))))


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
