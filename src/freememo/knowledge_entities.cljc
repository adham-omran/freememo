(ns freememo.knowledge-entities
  "Knowledge → Entities: the entity browser as a curation table
   (plans/knowledge-tables-selection-and-bulk.md §6). A row shows Rename and, when
   the entity has facts to span, a ⋯ menu with Synthesize questions. Merging is a
   bulk action: select the entities that name one thing, then pick the one to keep.

   Its own namespace for the 64KB method cap, as knowledge_known is."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.knowledge-scope :as scope]
   [freememo.loading :as loading]
   [freememo.modal-shell :as modal]
   [freememo.row-menu :as menu]
   [freememo.tape-table :as tape]
   [freememo.typeahead :as typeahead]
   [freememo.util :as util]
   #?(:clj [freememo.commands :as commands])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.kg-questions :as kgq])
   #?(:clj [freememo.knowledge-bulk :as bulk])
   #?(:clj [freememo.logging :as log])))

(def ^:private entity-limit
  "Rows the list holds at most. Virtual scroll owns the DOM cost; the wire cost
   is ~60 B per row. The count reads `1000+` at this limit."
  1000)

(def ^:private sort-keys
  "Sortable column → the row key it sorts by. No sort means the server's order:
   busiest first, or closest match first while searching."
  {:entity :label :facts :fact_count})

(def ^:private default-sort [nil nil])

;; ── Server reads and writes ────────────────────────────────────────
;; Body-level reader conditionals: each name appears inside an (e/server …)
;; form, which CLJS compiles too.

(defn list-entities*
  "The entities the filters keep, at most `entity-limit`, in the requested order."
  [_kg-bump user-id query source-id sort-col sort-dir]
  #?(:clj (let [rows (db/list-kg-entities user-id query entity-limit source-id)]
            (if-let [k (sort-keys sort-col)] (tape/sort-rows rows k sort-dir) rows))
     :cljs nil))

(defn entity-choices*
  "The selected entities, with labels and fact counts, for the merge step. Read
   by id, because the filter may hide some of the selection."
  [user-id ids]
  #?(:clj (bulk/entity-choices user-id (vec ids)) :cljs nil))

(defn rename-entity!*
  "Rename a KG entity. Audits + bumps on success.
   Returns {:success bool :error str}."
  [user-id entity-id new-label]
  #?(:clj (let [ok? (db/rename-kg-entity! user-id entity-id new-label)]
            (when ok?
              (commands/bump! user-id :distill)
              (log/audit! {:id ::rename-entity :user-id user-id :action :update
                           :entity :kg-entity :entity-id entity-id}))
            {:success ok? :error (when-not ok? "Entity not found")})
     :cljs nil))

(defn run-entity-action!*
  "Run one entity action. `:synthesize` is one LLM call that pushes its own
   outcome toast; `:merge` keeps `:keep` and absorbs every other id, and toasts
   its own failure through `bulk/report-failure!`.
   Post: {:success true …} or {:success false :error msg}."
  [user-id {:keys [kind ids keep label]}]
  #?(:clj (case kind
            :synthesize (kgq/generate-synthesis-questions! user-id (first ids) label)
            :merge (bulk/report-failure! user-id
                     (bulk/merge-entities! user-id keep (vec (remove #{keep} ids))))
            {:success false :error "That action is not available."})
     :cljs nil))

;; ── Rows ───────────────────────────────────────────────────────────

(e/defn EntityLabelCell
  "The entity's label, or the rename field while this row is being renamed.
   Enter saves, Escape cancels."
  [user-id id label !renaming renaming]
  (e/client
    (dom/td
      (if (= renaming id)
        (dom/input
          (dom/props {:value label :class "input" :aria-label "Rename entity"})
          (let [keyev (dom/On "keydown"
                        (fn [ev]
                          (let [k (.-key ev) v (.. ev -target -value)]
                            (cond
                              (= k "Enter") {:id (str (random-uuid)) :value v}
                              (= k "Escape") (do (reset! !renaming nil) nil)
                              :else nil)))
                        nil)
                [t _] (e/Token keyev)]
            (when t
              (let [result (e/server
                             (e/Offload
                               #(rename-entity!* user-id id (:value keyev))))]
                (case result
                  (if (:success result)
                    (do (reset! !renaming nil) (t))
                    (t (:error result))))))))
        (dom/span (dom/props {:class "tape-cell-text" :dir "auto"}) (dom/text label))))))

(e/defn EntityRow [user-id entity i !renaming renaming !selected selected !menu open-row-key
                   selecting?]
  (e/client
    (let [{:keys [id label aliases fact_count]} entity]
      (dom/tr
        (dom/props {:class (when (even? i) "row-alt") :style {:--order i}})
        (tape/SelectCell id label !selected selected)
        (EntityLabelCell user-id id label !renaming renaming)
        (dom/td (dom/props {:class "tape-cell-meta"})
          (dom/span (dom/props {:class "tape-cell-text" :dir "auto"})
            (dom/text (str/join ", " (or aliases [])))))
        (dom/td (dom/props {:class "tape-cell-num"}) (dom/text (str fact_count)))
        (dom/td (dom/props {:class (if selecting? "tape-cell-actions tape-cell-actions-inert"
                                     "tape-cell-actions")})
          (when-not (= renaming id)
            (dom/button
              (dom/props {:type "button" :class "btn btn-sm btn-secondary"
                          :aria-label (str "Rename " label)})
              (dom/text "Rename")
              (dom/On "click" (fn [_] (reset! !renaming id)) nil)))
          ;; Synthesis spans several facts, so an entity with one has nothing in
          ;; its menu and shows no ⋯.
          (when (> fact_count 1)
            (menu/MenuButton !menu open-row-key id label entity)))))))

;; ── Merge step ─────────────────────────────────────────────────────

(e/defn MergeChoiceModal
  "Pick the entity to keep from the selection. Each choice is the confirm: a
   merge cannot be undone, so the dialog says so before any button does it.
   Pre:  `!merge-pick` holds {:ids […]} with at least two ids, or nil.
   Post: a choice writes the merge action and closes the dialog."
  [user-id !merge-pick merge-pick !action]
  (e/client
    (when (some? merge-pick)
      (let [choices (e/server (e/Offload #(entity-choices* user-id (:ids merge-pick))))
            n (count choices)]
        (dom/div
          (dom/props {:class "modal-backdrop"})
          (dom/On "click" (fn [_] (reset! !merge-pick nil)) nil)
          (modal/ModalEscape (fn [] (reset! !merge-pick nil)) "Merge entities")
          (dom/div
            (dom/props {:class "modal-content modal-sm"})
            (dom/On "click" (fn [e] (.stopPropagation e)) nil)
            (dom/div
              (dom/props {:class "confirm-modal-body"})
              (dom/p (dom/text (str "Pick the entity to keep. The other " (dec n)
                                 " are removed, and their facts and aliases move to it. "
                                 "You cannot undo a merge.")))
              (dom/div
                (dom/props {:class "merge-choices"})
                (e/for [{:keys [id label fact_count]} (e/diff-by :id choices)]
                  (dom/button
                    (dom/props {:type "button" :class "btn btn-secondary merge-choice"})
                    (dom/span (dom/props {:dir "auto"}) (dom/text (str "Keep " label)))
                    (dom/span (dom/props {:class "merge-choice-facts"})
                      (dom/text (str fact_count " fact" (when (not= 1 fact_count) "s"))))
                    (dom/On "click"
                      (fn [_]
                        (reset! !action (assoc (scope/action :merge (map :id choices) true)
                                          :keep id))
                        (reset! !merge-pick nil))
                      nil)))))
            (dom/div
              (dom/props {:class "confirm-modal-actions"})
              (dom/button
                (dom/props {:type "button" :class "btn btn-secondary"})
                (dom/text "Cancel")
                (dom/On "click" (fn [_] (reset! !merge-pick nil)) nil)))))))))

;; ── View ───────────────────────────────────────────────────────────

(e/defn EntitiesViewBody
  "The entity browser, scoped to `route-doc-id` (nil = all entities).
   Pre:  mounted by EntitiesView, which keys this frame on route-doc-id."
  [user-id kg-bump route-doc-id navigate!]
  (e/client
    (let [initial (util/query-params)
                    !query (atom (or (get initial "q") "")) query (e/watch !query)
          !search-epoch (atom 0) search-epoch (e/watch !search-epoch)
          !source-id (atom route-doc-id) source-id (e/watch !source-id)
          !order (atom (scope/parse-sort (get initial "sort") (set (keys sort-keys)) default-sort))
          order (e/watch !order)
          [sort-col sort-dir] order
          !renaming (atom nil) renaming (e/watch !renaming)
          !selected (atom #{}) selected (e/watch !selected)
          !menu (atom nil) menu (e/watch !menu)
          !action (atom nil) action (e/watch !action)
          !merge-pick (atom nil) merge-pick (e/watch !merge-pick)
          docs (e/server (e/Offload #(scope/list-source-docs* kg-bump user-id)))
          doc-options (mapv (fn [d] {:id (:id d) :label (:title d)}) docs)
          ;; Actions: Rename (~66px) and ⋯ (~38px), the cell's gap and padding.
          grid-cols "34px minmax(200px,1fr) minmax(160px,1fr) 60px 140px"]
      (loading/WithLatestServerLoading
        (e/fn [] (e/server (e/Offload #(list-entities* kg-bump user-id query source-id
                                         sort-col sort-dir))))
        (e/fn [entities]
          ;; entities stays server-sited — see WithLatestServerLoading.
          (let [item-count (e/server (count entities))
                counts (e/server (tape/selection-counts entities :id selected))
                n-selected (count selected)
                selecting? (pos? n-selected)
                filtered? (or (not (str/blank? query)) (some? source-id))
                clear-search! (fn [] (reset! !query "") (swap! !search-epoch inc))]
            (dom/div
              (dom/props {:class "knowledge-view"})
              (dom/div
                (dom/props {:class "knowledge-filters"})
                (scope/SearchInput "Search entities…" query !query search-epoch)
                (dom/div (dom/props {:class "knowledge-filter-pick"})
                  (typeahead/Typeahead !source-id doc-options "Filter by source…" nil nil))
                (dom/span (dom/props {:class "knowledge-count"})
                  (dom/text (cond
                              (>= item-count entity-limit) (str entity-limit "+ entities")
                              (= 1 item-count) "1 entity"
                              :else (str item-count " entities")))))
              (when filtered?
                (dom/div
                  (dom/props {:class "knowledge-filter-labels"})
                  (when-not (str/blank? query)
                    (tape/FilterLabel (str "Search: " query) clear-search!))
                  (when (some? source-id)
                    (tape/FilterLabel
                      (str "Source: " (or (some #(when (= source-id (:id %)) (:label %)) doc-options) source-id))
                      (fn [] (reset! !source-id nil))))
                  (tape/ClearFilters (fn [] (clear-search!) (reset! !source-id nil)))))
              (tape/HeadedTapeTable
                (e/fn []
                  (tape/SelectAllHeader entities :id item-count (:shown counts) !selected)
                  (tape/SortHeader "Entity" :entity :asc order !order)
                  (tape/PlainHeader "Aliases")
                  (tape/SortHeader "Facts" :facts :desc order !order)
                  (tape/PlainHeader ""))
                ;; Over the header row, not above the table: a bar that pushed the table
                ;; down by a row on every first tick was visually jarring.
                (e/fn []
                  (tape/BatchBar n-selected (:hidden counts) !selected
                    (e/fn []
                      (tape/BatchAction "Merge into one…" false (< n-selected 2)
                        (fn [] (reset! !merge-pick {:ids (vec @!selected)}))))))
                grid-cols item-count [query source-id sort-col sort-dir]
                (if filtered?
                  "No entities match these filters."
                  "No entities yet. Distill a document first.")
                (e/fn [i]
                  (when-let [entity (e/server (nth entities i nil))]
                    (EntityRow user-id entity i !renaming renaming !selected selected
                      !menu (:row-key menu) selecting?))))
              (menu/RowMenu !menu menu
                (e/fn [m]
                  (let [{:keys [id label]} (:row m)]
                    (menu/MenuItem !menu "Synthesize questions" false
                      (fn [] (reset! !action (assoc (scope/action :synthesize [id] false)
                                               :label label)))))))
              (MergeChoiceModal user-id !merge-pick merge-pick !action)
              ;; The one place a row or batch action runs; it outlives the menu and
              ;; the merge dialog that wrote it.
              (let [[t _] (e/Token action)]
                (when t
                  (let [r (e/server (e/Offload #(run-entity-action!* user-id action)))]
                    (case r
                      (case (when (and (:bulk? action) (:success r)) (reset! !selected #{}))
                        (t))))))
              (scope/ReflectScopeInUrl navigate! :knowledge-entities route-doc-id source-id)
              (scope/UrlState {"q" query
                               "sort" (scope/sort-param order default-sort)}))))))))

(e/defn EntitiesView
  "Remount-on-scope-change wrapper around EntitiesViewBody — see
   knowledge-questions/QuestionsView."
  [user-id kg-bump route-doc-id navigate!]
  (e/client
    (e/for-by identity [_k [route-doc-id]]
      (EntitiesViewBody user-id kg-bump route-doc-id navigate!))))
