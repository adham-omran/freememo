(ns freememo.knowledge-tree
  "Contents tab — knowledge tree with flatten + virtual scroll."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [clojure.string :as str]
   [freememo.navigation :as nav]
   [freememo.card-components :as card-components]
   [freememo.pdf-cache :as pdf-cache]
   [freememo.bibliography-form :as bibform]
   [freememo.icons :as icons]
   [freememo.viewport :as viewport]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.user-state :as us])))

;; Server wrapper — _refresh param creates Electric reactive dependency
(defn get-tree-items* [_refresh user-id]
  #?(:clj (vec (db/get-knowledge-tree user-id))
     :cljs nil))

;; Atomic rename + refresh — single e/server target so Electric can't drop
;; the side effect when the binding is "unused" in the client body.
(defn rename-and-refresh! [user-id id new-title]
  #?(:clj (do (db/rename-topic! id new-title)
            (swap! (us/get-atom user-id :refresh) inc)
            (swap! (us/get-atom user-id :tree-mutations) inc)
            :ok)
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

;; Badge helper — delegated to bibliography-form/topic-badge so wiki sources
;; render as "Wikipedia" via container-title even when topic.kind='web'.

;; ---------------------------------------------------------------------------
;; DocumentRow — extracted from DocumentTreeView so Electric's macroexpander
;; doesn't blow the stack on the giant inlined row. Atoms passed positionally
;; per CLAUDE.md "Never Put Atoms in Maps Passed to e/defn".
;; ---------------------------------------------------------------------------

(e/defn DocumentRow [user-id navigate! row i row-height
                     expanded-set editing-id
                     !expanded-set !editing-id !show-confirm !scroll-node]
  (e/client
    (let [{:keys [depth topic has-children is-root]} row
          id (:topics/id topic)
          title (or (:topics/title topic) "(empty)")
          kind (or (:topics/kind topic) "basic")
          source-container (:sources/container_title topic)
          topic-status (or (:topics/status topic) "active")
          expanded? (contains? expanded-set id)
          [badge-text badge-color] (bibform/topic-badge kind source-container)]
      (dom/tr
        (dom/props {:style {:border-bottom "1px solid var(--color-bg-subtle)"
                            :height (str row-height "px")
                            :opacity (case topic-status "done" "0.6" "1")
                            :cursor "pointer"
                            :--order (inc i)}})
        (dom/On "click"
          (fn [_] (navigate! :viewer (nav/nav-topic id :library)))
          nil)
        ;; Column 1: Document (arrow + badge + title)
        ;; --row-indent carries the depth indent as a custom property so the
        ;; phone media query can flatten it (`var(--row-indent, 10px)` → 10px).
        (dom/td
          (dom/props {:style {:display "flex" :align-items "center" :gap "6px"
                              :--row-indent (str (+ 10 (* depth 20)) "px")
                              :padding-left "var(--row-indent)"
                              :overflow "hidden"
                              :border-left (when (= topic-status "done") "2px solid var(--color-success-lighter)")}})
          (if has-children
            (dom/span
              (dom/props {:style {:width "28px" :padding "4px 6px" :margin "-4px -6px"
                                  :font-size "12px" :cursor "pointer"
                                  :user-select "none" :text-align "center" :flex-shrink "0"}})
              (dom/text (if expanded? "▼" "▶"))
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
          (if (= editing-id id)
            (e/for-by identity [_k [id]]
              (dom/input
                (dom/props {:type "text"
                            :placeholder "Title"
                            :maxlength "500"
                            :style {:flex "1" :min-width "0" :padding "2px 6px"
                                    :font-size (if is-root "13px" "12px")
                                    :border "1px solid var(--color-border)"
                                    :border-radius "3px"
                                    :background "var(--color-bg-card)"}})
                (set! (.-value dom/node) (or title ""))
                (let [n dom/node]
                  (js/setTimeout
                    (fn []
                      (.focus n)
                      (when (pos? (count (.-value n))) (.select n)))
                    0))
                (dom/On "click" (fn [e] (.stopPropagation e)) nil)
                (dom/On "keydown"
                  (fn [e]
                    (when (= (.-key e) "Escape")
                      (.preventDefault e)
                      (set! (.-value (.-target e)) (or title ""))
                      (.blur (.-target e))
                      (reset! !editing-id nil)))
                  nil)
                (let [event (dom/On "change" #(-> % .-target .-value) nil)
                      [t _] (e/Token event)]
                  (when t
                    (let [trimmed (str/trim event)]
                      (if (str/blank? trimmed)
                        (case (e/client (reset! !editing-id nil)) (t))
                        (let [ok (e/server (e/Offload #(rename-and-refresh! user-id id trimmed)))]
                          (case ok
                            (case (e/client (reset! !editing-id nil))
                              (t))))))))))
            (dom/span
              (dom/props {:style {:overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"
                                  :font-size (if is-root "13px" "12px")
                                  :font-weight (if is-root "500" "400")
                                  :color "var(--color-text-primary)"}
                          :data-tooltip title})
              (dom/text title))))
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
                            "–"))))))
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
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"
                        :style {:padding "2px 6px" :font-size "12px" :line-height "1"}
                        :data-tooltip "Rename"})
            (dom/text "✎")
            (dom/On "click"
              (fn [e]
                (.stopPropagation e)
                (reset! !editing-id id))
              nil))
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
                          :style {:padding "2px 6px" :font-size "12px" :line-height "1"}
                          :aria-label "Delete topic"
                          :data-tooltip "Delete topic"})
              (icons/Icon :x :size 14 :title "Delete topic")
              (dom/On "click" (fn [e] (.stopPropagation e) (reset! !show-confirm id)) nil))))))))

;; ---------------------------------------------------------------------------
;; DeleteConfirmModal — extracted to keep DocumentTreeView lean
;; ---------------------------------------------------------------------------

;; Pre:  `show-confirm` is the topic-id to delete, or nil (modal hidden).
;; Post: Esc / backdrop click / Cancel → close (reset! !show-confirm nil).
;;       Delete click or Cmd/Ctrl+Enter → run delete sequence, then close.
;; Invariant: Delete button receives focus on mount (via RAF, after DOM attach)
;;            so Tab cycles between Delete↔Cancel and the keydown shortcut works.
(e/defn DeleteConfirmModal [user-id show-confirm !show-confirm]
  (e/client
    (when (some? show-confirm)
      (let [!delete-btn (atom nil)]
        (dom/div
          (dom/props {:class "modal-backdrop"})
          (dom/On "click" (fn [_] (reset! !show-confirm nil)) nil)
          (dom/On "keydown"
            (fn [e]
              #?(:cljs
                 (cond
                   (= (.-key e) "Escape")
                   (reset! !show-confirm nil)

                   (and (= (.-key e) "Enter") (or (.-metaKey e) (.-ctrlKey e)))
                   (when-some [btn @!delete-btn]
                     (.preventDefault e)
                     (.click btn)))))
            nil)
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
                (reset! !delete-btn dom/node)
                ;; RAF-deferred focus — HTML autofocus doesn't fire on
                ;; dynamically inserted elements; synchronous .focus races
                ;; with DOM attach. RAF schedules after attach + layout.
                ((fn []
                   #?(:cljs (.requestAnimationFrame js/window
                              (fn [] (.focus dom/node))))))
                (dom/text "Delete")
                (let [event (dom/On "click" (fn [_] show-confirm) nil)
                      [t _] (e/Token event)]
                  (when t
                    (let [topic-to-delete event
                          note-ids (e/server (e/Offload #(vec (db/get-all-anki-note-ids topic-to-delete))))]
                      (case note-ids
                        (let [r (e/server (e/Offload #(do (db/delete-topic-for-user! user-id topic-to-delete) :ok)))]
                          (case r
                            (case (e/server (swap! (us/get-atom user-id :refresh) inc))
                              (case (e/server (swap! (us/get-atom user-id :tree-mutations) inc))
                                (case (e/client (card-components/try-delete-anki-notes! note-ids))
                                  (case (e/client (pdf-cache/cache-delete topic-to-delete))
                                    (case (e/client (reset! !show-confirm nil))
                                      (t))))))))))))))))))))

;; Document tree view — used by LibraryPage
;; Flatten + virtual scroll for performance
;; `refresh` (caller-provided) covers content-level mutations; combined with
;; the per-user :tree-mutations atom (tree-shape mutations: extract create,
;; rename, delete, document import) so the library tree refreshes on either.
(e/defn DocumentTreeView [user-id navigate! refresh filter-text sort-col sort-dir kind-filter status-filter tree-expanded !sort-cmd]
  (e/client
    (e/server
      (let [tree-mutations (e/watch (us/get-atom user-id :tree-mutations))
            rev (+ refresh tree-mutations)
            all-items (get-tree-items* rev user-id)
            stats-map (get-root-stats* rev user-id)
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
                !editing-id (atom nil)
                editing-id (e/watch !editing-id)
                flat-rows (flatten-tree roots children-map expanded-set)
                row-count (count flat-rows)
                phone? (e/watch viewport/!phone?)
                row-height (if phone? 80 36)
                grid-cols (if phone? "1fr" "1fr 70px 80px 80px 110px")
                !scroll-node (atom nil)]
            (dom/div
              (dom/props {:style {:display "flex" :flex-direction "column" :min-height "0" :flex "1"}})

              ;; Fixed header
              (dom/table
                (dom/props {:class "library-table library-table-header"
                            :style {:width "100%" :display "grid" :grid-template-columns grid-cols :flex-shrink "0"}})
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
                    (dom/props {:class "library-table library-table-body"
                                :style {:width "100%" :display "grid" :grid-template-columns grid-cols :font-size "13px"}})
                    (if (pos? row-count)
                      (e/for [i (Tape offset limit)]
                        (let [row (nth flat-rows i nil)]
                          (when row
                            (DocumentRow user-id navigate! row i row-height
                              expanded-set editing-id
                              !expanded-set !editing-id !show-confirm !scroll-node))))
                      (dom/tr
                        (dom/td
                          (dom/props {:style {:grid-column "1 / -1" :text-align "center" :padding "24px 12px"
                                              :color "var(--color-text-secondary)" :font-size "13px"}})
                          (dom/text "No content yet. Import content from the Import tab.")))))
                  (dom/div (dom/props {:style {:height (str occluded-height "px")}}))))

              (DeleteConfirmModal user-id show-confirm !show-confirm))))))))


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
