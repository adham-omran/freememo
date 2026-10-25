(ns freememo.knowledge-questions
  "Knowledge → Questions: the question bank as a curation table
   (plans/knowledge-tables-selection-and-bulk.md §6). A row shows Edit and a ⋯
   menu; a selection opens a batch bar with Flag, Suspend and Delete.

   Its own namespace for the 64KB method cap, as knowledge_known is."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.a11y :as a11y]
   [freememo.knowledge-scope :as scope]
   [freememo.loading :as loading]
   [freememo.modal-shell :as modal]
   [freememo.question-curation :as curate]
   [freememo.quiz-item-stats :as stats]
   [freememo.row-menu :as menu]
   [freememo.tape-table :as tape]
   [freememo.tooltip :as tooltip]
   [freememo.typeahead :as typeahead]
   [freememo.util :as util]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.knowledge-bulk :as bulk])))

(def ^:private sort-keys
  "Sortable column → the row key it sorts by. No sort means the bank's own
   order, newest first."
  {:question :question :facts :fact_count})

(def ^:private default-sort [nil nil])

;; ── Server reads and writes ────────────────────────────────────────
;; Body-level reader conditionals: each name appears inside an (e/server …)
;; form, which CLJS compiles too.

(defn get-questions*
  "The bank's rows. Suspended questions are INCLUDED — the bank is the only
   unsuspend surface, so hiding them here would strand them. `flagged?` /
   `suspended?` narrow to the corresponding worklist."
  [_kg-bump user-id query source-id entity-id flagged? suspended? sort-col sort-dir]
  #?(:clj (let [rows (db/get-kg-questions user-id "approved" query
                       {:source-id source-id :entity-id entity-id
                        :flagged? flagged? :suspended? suspended?})]
            (if-let [k (sort-keys sort-col)] (tape/sort-rows rows k sort-dir) rows))
     :cljs nil))

(defn list-entity-options*
  "Lean [{:id :label}] of entities for the entity filter. With `source-id` (a
   graph_topic_id) set, only entities appearing in that document."
  [_kg-bump user-id source-id]
  #?(:clj (mapv (fn [ent] {:id (:id ent) :label (:label ent)})
            (db/list-kg-entities user-id nil 1000 source-id))
     :cljs nil))

(defn run-question-action!*
  "Run one question action over `ids` — one row from its menu, or a selection
   from the batch bar. The bulk fns push their own toast.
   Pre:  kind ∈ #{:delete :flag :unflag :suspend :unsuspend}.
   Post: {:success true …} or {:success false :error msg}."
  [user-id {:keys [kind ids]}]
  #?(:clj (bulk/report-failure! user-id
            (case kind
              :delete (bulk/reject-questions! user-id ids)
              :flag (bulk/set-questions-flags! user-id ids {:flagged true})
              :unflag (bulk/set-questions-flags! user-id ids {:flagged false})
              :suspend (bulk/set-questions-flags! user-id ids {:suspended true})
              :unsuspend (bulk/set-questions-flags! user-id ids {:suspended false})
              {:success false :error "That action is not available."}))
     :cljs nil))

;; ── Rows ───────────────────────────────────────────────────────────

(defn- state-text [flagged suspended]
  (cond (and flagged suspended) "Flagged, suspended"
        flagged "Flagged"
        suspended "Suspended"
        :else ""))

(e/defn QuestionRow
  "One question. Clicking the question or the answer opens the editor; the
   State column shows the flags the ⋯ menu changes."
  [q i !editing !selected selected !menu open-row-key selecting?]
  (e/client
    (let [{:keys [id kind question reference_answer fact_count flagged suspended]} q
          open! (fn [_] (reset! !editing q))]
      (dom/tr
        (dom/props {:class (str (when (even? i) "row-alt") (when suspended " row-withheld"))
                    :style {:--order i}})
        (tape/SelectCell id question !selected selected)
        (dom/td (dom/props {:class "tape-cell-click"})
          (a11y/KeyActivate {:label "Edit question"} open!)
          (dom/On "click" open! nil)
          (dom/span (dom/props {:class "tape-cell-text" :dir "auto"})
            (tooltip/Tooltip! question)
            (dom/text question)))
        (dom/td (dom/props {:class "tape-cell-muted tape-cell-click"})
          (dom/On "click" open! nil)
          (dom/span (dom/props {:class "tape-cell-text" :dir "auto"})
            (dom/text reference_answer)))
        (dom/td
          (dom/span (dom/props {:class (str "type-badge question-kind-" kind)})
            (dom/text kind)))
        (dom/td (dom/props {:class "tape-cell-num"}) (dom/text (str fact_count)))
        (dom/td (dom/props {:class "tape-cell-meta"}) (dom/text (state-text flagged suspended)))
        (dom/td (dom/props {:class (if selecting? "tape-cell-actions tape-cell-actions-inert"
                                     "tape-cell-actions")})
          (dom/button
            (dom/props {:type "button" :class "btn btn-sm btn-secondary"
                        :aria-label (str "Edit " question)})
            (dom/text "Edit")
            (dom/On "click" open! nil))
          (menu/MenuButton !menu open-row-key id question q))))))

;; ── View ───────────────────────────────────────────────────────────

(e/defn QuestionsViewBody
  "The question bank, scoped to `route-doc-id` (nil = the whole bank).
   Pre:  mounted by QuestionsView, which keys this frame on route-doc-id — the
         source atom below is seeded from it and relies on that remount to
         follow the URL. Every other filter is seeded from the query string."
  [user-id kg-bump route-doc-id navigate!]
  (e/client
    (let [initial (util/query-params)
                    !query (atom (or (get initial "q") "")) query (e/watch !query)
          !search-epoch (atom 0) search-epoch (e/watch !search-epoch)
          !source-id (atom route-doc-id) source-id (e/watch !source-id)
          !entity-id (atom (some-> (get initial "entity") parse-long)) entity-id (e/watch !entity-id)
          !flagged? (atom (scope/flag-set? initial "flagged")) flagged? (e/watch !flagged?)
          !suspended? (atom (scope/flag-set? initial "suspended")) suspended? (e/watch !suspended?)
          !order (atom (scope/parse-sort (get initial "sort") (set (keys sort-keys)) default-sort))
          order (e/watch !order)
          [sort-col sort-dir] order
          !selected (atom #{}) selected (e/watch !selected)
          !menu (atom nil) menu (e/watch !menu)
          !bar-menu (atom nil) bar-menu (e/watch !bar-menu)
          !action (atom nil) action (e/watch !action)
          !confirm-delete (atom nil) confirm-delete (e/watch !confirm-delete)
          !editing (atom nil)               ; the question map being edited, or nil
          !stats (atom nil)                 ; {:question id} whose stats are open
          docs (e/server (e/Offload #(scope/list-source-docs* kg-bump user-id)))
          ents (e/server (e/Offload #(list-entity-options* kg-bump user-id source-id)))
          doc-options (mapv (fn [d] {:id (:id d) :label (:title d)}) docs)
          ent-options (mapv (fn [x] {:id (:id x) :label (:label x)}) ents)
          ;; Select 34px, Kind 84px, Facts 50px, State 110px. Actions 104px: Edit
          ;; (~44px) and ⋯ (~38px) with the cell's 4px gap inside 10px padding.
          grid-cols "34px minmax(180px,1.4fr) minmax(180px,1.4fr) 84px 50px 110px 104px"]
      (loading/WithLatestServerLoading
        (e/fn [] (e/server (e/Offload #(get-questions* kg-bump user-id query source-id entity-id
                                         flagged? suspended? sort-col sort-dir))))
        (e/fn [questions]
          ;; questions stays server-sited — see WithLatestServerLoading.
          (let [item-count (e/server (count questions))
                counts (e/server (tape/selection-counts questions :id selected))
                selecting? (boolean (seq selected))
                filtered? (or (not (str/blank? query)) (some? source-id) (some? entity-id)
                            flagged? suspended?)
                clear-search! (fn [] (reset! !query "") (swap! !search-epoch inc))]
            (dom/div
              (dom/props {:class "knowledge-view"})
              (dom/div
                (dom/props {:class "knowledge-filters"})
                (scope/SearchInput "Search questions…" query !query search-epoch)
                (dom/div (dom/props {:class "knowledge-filter-pick"})
                  (typeahead/Typeahead !source-id doc-options "Filter by source…" nil nil))
                (dom/div (dom/props {:class "knowledge-filter-pick"})
                  (typeahead/Typeahead !entity-id ent-options "Filter by entity…" nil nil))
                (scope/FilterToggle "Flagged" flagged? !flagged?)
                (scope/FilterToggle "Suspended" suspended? !suspended?)
                (dom/span (dom/props {:class "knowledge-count"})
                  (dom/text (str item-count " question" (when (not= 1 item-count) "s")))))
              (when filtered?
                (dom/div
                  (dom/props {:class "knowledge-filter-labels"})
                  (when-not (str/blank? query)
                    (tape/FilterLabel (str "Search: " query) clear-search!))
                  (when (some? source-id)
                    (tape/FilterLabel
                      (str "Source: " (or (some #(when (= source-id (:id %)) (:label %)) doc-options) source-id))
                      (fn [] (reset! !source-id nil))))
                  (when (some? entity-id)
                    (tape/FilterLabel
                      (str "Entity: " (or (some #(when (= entity-id (:id %)) (:label %)) ent-options) entity-id))
                      (fn [] (reset! !entity-id nil))))
                  (when flagged? (tape/FilterLabel "Flagged" (fn [] (reset! !flagged? false))))
                  (when suspended? (tape/FilterLabel "Suspended" (fn [] (reset! !suspended? false))))
                  (tape/ClearFilters
                    (fn []
                      (clear-search!)
                      (reset! !entity-id nil)
                      (reset! !flagged? false)
                      (reset! !suspended? false)
                      (reset! !source-id nil)))))
              (tape/BatchBar (count selected) (:hidden counts) !selected
                (e/fn []
                  (tape/BatchAction "Flag" false false
                    (fn [] (reset! !action (scope/action :flag @!selected true))))
                  (tape/BatchAction "Suspend" false false
                    (fn [] (reset! !action (scope/action :suspend @!selected true))))
                  (menu/MenuButton !bar-menu (:row-key bar-menu) :batch "the selected questions" nil)
                  (tape/BatchAction "Delete" true false
                    (fn [] (reset! !confirm-delete {:ids (vec @!selected)})))))
              (tape/HeadedTapeTable
                (e/fn []
                  (tape/SelectAllHeader questions :id item-count (:shown counts) !selected)
                  (tape/SortHeader "Question" :question :asc order !order)
                  (tape/PlainHeader "Answer")
                  (tape/PlainHeader "Kind")
                  (tape/SortHeader "Facts" :facts :desc order !order)
                  (tape/PlainHeader "State")
                  (tape/PlainHeader ""))
                grid-cols item-count
                [query source-id entity-id flagged? suspended? sort-col sort-dir]
                (cond
                  flagged? "No flagged questions."
                  suspended? "No suspended questions."
                  (not filtered?)
                  "No questions yet. Generate them from a document, or synthesize them from an entity."
                  :else "No questions match these filters.")
                (e/fn [i]
                  (when-let [q (e/server (nth questions i nil))]
                    (QuestionRow q i !editing !selected selected !menu (:row-key menu) selecting?))))
              (menu/RowMenu !menu menu
                (e/fn [m]
                  (let [{:keys [id flagged suspended]} (:row m)]
                    (menu/MenuItem !menu (if flagged "Unflag" "Flag") false
                      (fn [] (reset! !action (scope/action (if flagged :unflag :flag) [id] false))))
                    (menu/MenuItem !menu (if suspended "Unsuspend" "Suspend") false
                      (fn [] (reset! !action (scope/action (if suspended :unsuspend :suspend) [id] false))))
                    (menu/MenuItem !menu "Stats" false (fn [] (reset! !stats {:question id})))
                    (menu/MenuItem !menu "Delete" true
                      (fn [] (reset! !action (scope/action :delete [id] false)))))))
              (menu/RowMenu !bar-menu bar-menu
                (e/fn [_]
                  (menu/MenuItem !bar-menu "Unflag" false
                    (fn [] (reset! !action (scope/action :unflag @!selected true))))
                  (menu/MenuItem !bar-menu "Unsuspend" false
                    (fn [] (reset! !action (scope/action :unsuspend @!selected true))))))
              ;; Delete has an Undo, but the selection can hold rows the filter
              ;; hides, so the count is stated before anything changes.
              (modal/ConfirmModal !confirm-delete
                "Delete questions?"
                (let [n (count (:ids confirm-delete))]
                  (str "Delete " n " selected question" (when (not= 1 n) "s")
                    "? One Undo restores all of them."))
                "Delete" "btn btn-danger-fill"
                (e/fn [payload]
                  (let [r (e/server (e/Offload #(run-question-action!* user-id
                                                  {:kind :delete :ids (:ids payload)})))]
                    (case r
                      (if (:success r)
                        (swap! !selected #(reduce disj % (:ids payload)))
                        r)))))
              ;; The one place a row or batch action runs. It outlives the menu
              ;; that wrote it, so closing the menu never tears down the work.
              (let [[t _] (e/Token action)]
                (when t
                  (let [r (e/server (e/Offload #(run-question-action!* user-id action)))]
                    (case r
                      (case (when (and (:bulk? action) (:success r)) (reset! !selected #{}))
                        (t))))))
              (curate/QuestionEditorModal user-id !editing)
              (stats/QuizItemStatsModal user-id !stats)
              (scope/ReflectScopeInUrl navigate! :knowledge-questions route-doc-id source-id)
              (scope/UrlState {"q" query
                               "entity" entity-id
                               "flagged" (scope/flag-param flagged?)
                               "suspended" (scope/flag-param suspended?)
                               "sort" (scope/sort-param order default-sort)}))))))))

(e/defn QuestionsView
  "Remount-on-scope-change wrapper around QuestionsViewBody.

   An atom in an e/defn let is created at mount and reused for the frame's life, so
   a bare (atom route-doc-id) inside the body would hold a stale scope once the URL
   changed under it. Keying the frame on the route id re-seeds it; the list
   resetting is correct anyway, since a new scope is a different list."
  [user-id kg-bump route-doc-id navigate!]
  (e/client
    (e/for-by identity [_k [route-doc-id]]
      (QuestionsViewBody user-id kg-bump route-doc-id navigate!))))
