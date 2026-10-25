(ns freememo.knowledge-facts
  "Knowledge → Documents → Facts: one document's facts as a curation table
   (plans/knowledge-tables-selection-and-bulk.md §6). Curation is by exception:
   facts land approved, and wrong ones are edited or deleted here. A row shows
   Edit and a ⋯ menu; a selection opens a batch bar with Delete, and with
   Not a duplicate while the status filter lists duplicates.

   Its own namespace for the 64KB method cap, as knowledge_known is."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.command-bus :as bus]
   [freememo.icons :as icons]
   [freememo.knowledge-scope :as scope]
   [freememo.loading :as loading]
   [freememo.modal-shell :as modal]
   [freememo.row-menu :as menu]
   [freememo.tape-table :as tape]
   [freememo.tooltip :as tooltip]
   [freememo.util :as util]
   #?(:clj [taoensso.telemere :as tel])
   #?(:clj [freememo.commands :as commands])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.kg-dedup :as kg-dedup])
   #?(:clj [freememo.knowledge-bulk :as bulk])
   #?(:clj [freememo.logging :as log])
   #?(:clj [freememo.toasts :as toasts])))

(def ^:private status-label
  "The statuses the Facts table can list, with their filter labels. 'duplicate'
   is this view's ONLY surface: a swept fact is filtered out of every other read
   in the app, so without the filter it would be marked, invisible and
   unrestorable. 'rejected' rides along for the same reason."
  {"approved" "Approved" "duplicate" "Duplicates" "rejected" "Deleted"})

(def ^:private sort-keys
  "Sortable column → the row key it sorts by."
  {:subject :subject_label :predicate :predicate_label :page :page_number})

(def ^:private default-sort
  "Page order, the order the server returns. Equal pages keep fact id order."
  [:page :asc])

;; ── Server reads and writes ────────────────────────────────────────
;; Body-level reader conditionals: each name appears inside an (e/server …)
;; form, which CLJS compiles too.

(defn get-doc-facts*
  "The document's facts in `status` that match `query`, in the requested order."
  [_kg-bump user-id doc-id status query sort-col sort-dir]
  #?(:clj (let [rows (db/get-kg-facts user-id doc-id status query)]
            (if-let [k (sort-keys sort-col)] (tape/sort-rows rows k sort-dir) rows))
     :cljs nil))

(defn doc-fact-ids*
  "Every fact id of the document in `status`, ignoring the view's search filter —
   the target set for \"select all in this document\". Takes the status the view
   is showing: a select-all that returned approved ids while the view listed
   duplicates would delete rows the user never saw."
  [_kg-bump user-id doc-id status]
  #?(:clj (db/kg-fact-ids-for-doc user-id doc-id status) :cljs nil))

(defn reject-facts!*
  "Reject a selection of facts as one operation: one undo entry, one bump, one
   audit line, one toast — the counts that a per-fact loop would multiply.
   Post: {:success true :deleted n} with the Undo toast pushed when n > 0, or
         {:success false :error msg}. n = 0 (everything already rejected or not
         owned) is a success with no toast, matching the single-fact no-op."
  [user-id fact-ids]
  #?(:clj (try
            (let [{:keys [rejected undo-id]} (db/reject-kg-facts! user-id fact-ids)]
              (when (pos? rejected)
                (commands/bump! user-id :bulk-reject-facts)
                (log/audit! {:id ::reject-facts :user-id user-id :action :delete
                             :entity :kg-fact :entity-id nil :n rejected})
                (toasts/push! user-id
                  (cond-> {:level :success
                           :message (str "Deleted " rejected " fact" (when (not= 1 rejected) "s"))
                           :dedup? false}
                    undo-id (assoc :actions [{:label "Undo" :undo-id undo-id}]))))
              {:success true :deleted rejected})
            (catch Exception e
              (tel/error! {:id ::reject-facts :data {:user-id user-id}} e)
              {:success false :error (.getMessage e)}))
     :cljs nil))

(defn run-fact-action!*
  "Run one fact action over `ids` — one row from its menu, or a selection from
   the batch bar.
   Pre:  kind ∈ #{:delete :unmark}.
   Post: {:success true …} or {:success false :error msg}."
  [user-id {:keys [kind ids]}]
  #?(:clj (bulk/report-failure! user-id
            (case kind
              :delete (reject-facts!* user-id ids)
              :unmark (bulk/unmark-duplicate-facts! user-id ids)
              {:success false :error "That action is not available."}))
     :cljs nil))

(defn edit-fact-value!*
  "Edit a fact's object: set a literal (`literal?` true) or relink to an entity
   by label. Audits + bumps; toasts on duplicate. The db result `r` is :done
   (saved), :duplicate (already-exists), or nil (fact gone / not literal) —
   only :done is a real save. Returns {:success bool :error str}."
  [user-id fact-id value literal?]
  #?(:clj (let [r (if literal?
                    (when (db/update-kg-fact-literal! user-id fact-id value) :done)
                    (db/relink-kg-fact-object! user-id fact-id value))]
            (when (= :duplicate r)
              (toasts/push! user-id
                {:level :error
                 :message "That fact already exists in this document."}))
            (commands/bump! user-id :distill)
            (when (= :done r)
              (log/audit! {:id ::edit-fact :user-id user-id :action :update
                           :entity :kg-fact :entity-id fact-id}))
            {:success (= :done r)
             :error (when (not= :done r)
                      (if (= :duplicate r)
                        "That fact already exists in this document."
                        "Fact not found"))})
     :cljs nil))

(defn sweep-duplicates!*
  "Mark near-duplicate facts in one document (freememo.kg-dedup)."
  [user-id doc-id]
  #?(:clj (kg-dedup/sweep-document-command! user-id doc-id) :cljs nil))

(defn sweep-duplicates-all!*
  "Mark near-duplicate facts across every document holding facts."
  [user-id]
  #?(:clj (kg-dedup/sweep-all-documents-command! user-id) :cljs nil))

;; ── Controls ───────────────────────────────────────────────────────

(e/defn FactObjectCell
  "Object cell: text, or an inline editor when this row is being edited.
   Enter saves (Offload), Escape cancels. Literal objects rewrite the literal;
   entity objects RELINK this fact to the matched-or-created entity — other
   facts of the old entity are untouched (rename-everywhere lives in the
   Entities tab). A relink that would duplicate an existing s/p/o toasts and
   leaves the fact unchanged."
  [user-id fact-id object_label object_literal !editing editing]
  (e/client
    (if (= editing fact-id)
      (dom/input
        (dom/props {:value (or object_label object_literal) :class "input"
                    :aria-label "Edit object"})
        (let [keyev (dom/On "keydown"
                      (fn [ev]
                        (let [k (.-key ev) v (.. ev -target -value)]
                          (cond
                            (= k "Enter") {:id (str (random-uuid)) :value v}
                            (= k "Escape") (do (reset! !editing nil) nil)
                            :else nil)))
                      nil)
              [t _] (e/Token keyev)
              literal? (some? object_literal)]
          (when t
            (let [result (e/server
                           (e/Offload
                             #(edit-fact-value!* user-id fact-id (:value keyev) literal?)))]
              (case result
                (if (:success result)
                  (do (reset! !editing nil) (t))
                  (t (:error result))))))))
      (dom/span (dom/props {:class "tape-cell-text" :dir "auto"})
        (dom/text (or object_label object_literal))))))

(e/defn FactStatusFilter
  "Which status the Facts table lists. A1-fallback select (Forms5 has no tracked
   <select>); the atom is local to FactsView, which owns the query it feeds.
   Purely local — no token, because switching a filter writes nothing."
  [!status status]
  (e/client
    (dom/select
      (dom/props {:class "input knowledge-filter-select"
                  :aria-label "Filter facts by status"})
      ;; `selected` on the option, not `value` on the select: a select's value
      ;; set before its options mount selects nothing, so a status seeded from
      ;; the URL showed Approved while the table listed duplicates.
      (dom/option (dom/props {:value "approved" :selected (= status "approved")}) (dom/text "Approved"))
      (dom/option (dom/props {:value "duplicate" :selected (= status "duplicate")}) (dom/text "Duplicates"))
      (dom/option (dom/props {:value "rejected" :selected (= status "rejected")}) (dom/text "Deleted"))
      (dom/On "change" (fn [ev] (reset! !status (.. ev -target -value))) nil))))

(e/defn SweepDuplicatesButton
  "Run the near-duplicate sweep over this document.

   Blocking JDBC through e/Offload, and the token gates on the server result so
   the button stays disabled for the whole run — the sweep compares every approved
   pair on every page and is not instant on a large document.
   Publishes a command-bus invoker so the palette can reach it; retracts on unmount
   (a stale invoker makes the command look available from another tab)."
  [user-id doc-id]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm" :aria-label "Mark duplicate facts in this document"})
      (tooltip/Tooltip! "Find near-duplicate facts in this document and mark them")
      (icons/Icon :git-compare :size 14)
      (dom/span (dom/props {:class "icon-label"}) (dom/text "Mark duplicates"))
      (let [node dom/node]
        (bus/publish-invoker! :sweep-duplicates (fn [] (.click node)))
        (e/on-unmount (fn [] (bus/retract-invoker! :sweep-duplicates))))
      ;; A FRESH value per click, never doc-id: work-skipping compares handler
      ;; results with =, so a stable value makes the second click a no-op and the
      ;; button fires exactly once per mount. This button never unmounts, so
      ;; nothing would re-arm it.
      (let [click (dom/On "click" (fn [_] {:id (str (random-uuid))}) nil)
            [t ?error] (e/Token click)]
        (dom/props {:disabled (some? t) :aria-busy (some? t)
                    :aria-invalid (some? ?error)})
        (when t
          (let [r (e/server (e/Offload #(sweep-duplicates!* user-id doc-id)))]
            (case r
              (if (:success r) (t) (t (:error r))))))))))

(e/defn SweepAllDuplicatesButton
  "Run the sweep over every document holding facts.

   Separate from the per-document button rather than a scope toggle on it: the two
   differ in blast radius, and a hidden scope would make the wider one impossible
   to name in the palette."
  [user-id]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm" :aria-label "Mark duplicate facts in all documents"})
      (tooltip/Tooltip! "Sweep every document that has facts. One Undo per document.")
      (dom/text "All docs")
      (let [node dom/node]
        (bus/publish-invoker! :sweep-duplicates-all (fn [] (.click node)))
        (e/on-unmount (fn [] (bus/retract-invoker! :sweep-duplicates-all))))
      (let [click (dom/On "click" (fn [_] {:id (str (random-uuid))}) nil)
            [t ?error] (e/Token click)]
        (dom/props {:disabled (some? t) :aria-busy (some? t)
                    :aria-invalid (some? ?error)})
        (when t
          (let [r (e/server (e/Offload #(sweep-duplicates-all!* user-id)))]
            (case r
              (if (:success r) (t) (t (:error r))))))))))

(e/defn SelectDocumentButton
  "Select every fact of the document in the listed status, ignoring the search.
   Shown in the batch bar while a search narrows the list (spec §4.2). The ids
   cross only inside the click token."
  [doc-ids doc-fact-count !selected]
  (e/client
    (dom/button
      (dom/props {:type "button" :class "btn btn-sm btn-secondary"})
      (tooltip/Tooltip! "Ignores the search box and selects the whole document")
      (dom/text (str "Select all " doc-fact-count " in this document"))
      (let [[t _] (e/Token (dom/On "click" identity nil))]
        (when t
          (let [ids (e/server (vec doc-ids))]
            (case (swap! !selected into ids)
              (t))))))))

;; ── Rows ───────────────────────────────────────────────────────────

(e/defn FactRow [user-id fact i status !editing editing !selected selected !menu open-row-key
                 selecting?]
  (e/client
    (let [{:keys [id subject_label predicate_label
                  object_label object_literal page_number]} fact]
      (dom/tr
        (dom/props {:class (when (even? i) "row-alt")
                    :style {:--order i}})
        (tape/SelectCell id (str subject_label " " predicate_label) !selected selected)
        (dom/td
          (dom/span (dom/props {:class "tape-cell-text" :dir "auto"}) (dom/text subject_label)))
        (dom/td (dom/props {:class "tape-cell-muted"})
          (dom/span (dom/props {:class "tape-cell-text" :dir "auto"}) (dom/text predicate_label)))
        (dom/td
          (FactObjectCell user-id id object_label object_literal !editing editing))
        (dom/td (dom/props {:class "tape-cell-meta"})
          (dom/text (if page_number (str "p." page_number) "")))
        (dom/td (dom/props {:class (if selecting? "tape-cell-actions tape-cell-actions-inert"
                                     "tape-cell-actions")})
          (when (not= editing id)
            (dom/button
              (dom/props {:type "button" :class "btn btn-sm btn-secondary"
                          :aria-label (if object_literal "Edit value" "Relink object")})
              (tooltip/Tooltip! (if object_literal
                                  "Edit value"
                                  "Relink this fact's object (other facts unaffected)"))
              (dom/text "Edit")
              (dom/On "click" (fn [_] (reset! !editing id)) nil)))
          ;; A deleted fact has nothing in its menu: it is already deleted, and
          ;; restoring one replays an undo snapshot this view does not hold.
          (when (not= "rejected" status)
            (menu/MenuButton !menu open-row-key id subject_label fact)))))))

;; ── View ───────────────────────────────────────────────────────────

(e/defn FactsView [user-id doc-id navigate! kg-bump]
  (e/client
    (let [initial (util/query-params)
                    seed-status (get initial "status")
          !query (atom (or (get initial "q") "")) query (e/watch !query)
          !search-epoch (atom 0) search-epoch (e/watch !search-epoch)
          ;; 'approved' is the default, so the view behaves as before for anyone
          ;; who never touches the filter.
          !status (atom (if (contains? status-label seed-status) seed-status "approved"))
          status (e/watch !status)
          !order (atom (scope/parse-sort (get initial "sort") (set (keys sort-keys)) default-sort))
          order (e/watch !order)
          [sort-col sort-dir] order
          ;; Ids of the whole document — the document-wide select-all's target.
          ;; Server-sited: only the count crosses here, and the ids themselves are
          ;; read inside that button's token frame. Fetched only while a search
          ;; is active, since that is the only time it differs from the list. The
          ;; gate sits INSIDE the e/server: a client-side `when` around it would
          ;; site the value on the client and pull the whole vector across.
          doc-ids (e/server (when-not (str/blank? query)
                              (e/Offload #(doc-fact-ids* kg-bump user-id doc-id status))))
          doc-fact-count (e/server (count doc-ids))
          !selected (atom #{}) selected (e/watch !selected)
          !menu (atom nil) menu (e/watch !menu)
          !action (atom nil) action (e/watch !action)
          !confirm-delete (atom nil) confirm-delete (e/watch !confirm-delete)
          !editing (atom nil) editing (e/watch !editing)
          ;; Actions: Edit (~44px) and ⋯ (~38px), the cell's gap and padding.
          grid-cols "34px minmax(140px,1fr) minmax(130px,1fr) minmax(160px,1.2fr) 70px 104px"]
      ;; Spinner covers the first offloaded fetch (blank otherwise); filter changes
      ;; revalidate in place via the Offload latch, no spinner reflash.
      (loading/WithLatestServerLoading
        (e/fn [] (e/server (e/Offload #(get-doc-facts* kg-bump user-id doc-id status query
                                         sort-col sort-dir))))
        (e/fn [facts]
          ;; facts stays server-sited — see WithLatestServerLoading. Every read of it
          ;; is wrapped, so counts and the window rows cross, never the fact bodies
          ;; and never a vector of ids.
          (let [item-count (e/server (count facts))
                counts (e/server (tape/selection-counts facts :id selected))
                n-selected (count selected)
                selecting? (pos? n-selected)
                searching? (not (str/blank? query))
                filtered? (or searching? (not= "approved" status))
                clear-search! (fn [] (reset! !query "") (swap! !search-epoch inc))]
            (dom/div
              (dom/props {:class "knowledge-view"})
              (dom/div
                (dom/props {:class "knowledge-filters"})
                (dom/button
                  (dom/props {:type "button" :class "btn btn-sm"})
                  (dom/text "← Documents")
                  (dom/On "click" (fn [_] (navigate! :knowledge)) nil))
                (scope/SearchInput "Search facts…" query !query search-epoch)
                (FactStatusFilter !status status)
                (SweepDuplicatesButton user-id doc-id)
                (SweepAllDuplicatesButton user-id)
                (dom/span (dom/props {:class "knowledge-count"})
                  (dom/text (str item-count " fact" (when (not= 1 item-count) "s")))))
              (when filtered?
                (dom/div
                  (dom/props {:class "knowledge-filter-labels"})
                  (when searching?
                    (tape/FilterLabel (str "Search: " query) clear-search!))
                  (when (not= "approved" status)
                    (tape/FilterLabel (str "Status: " (status-label status))
                      (fn [] (reset! !status "approved"))))
                  (tape/ClearFilters (fn [] (clear-search!) (reset! !status "approved")))))
              ;; reset-key, not the row count: a sweep changes the count under a user
              ;; who is mid-scroll, and that must not snap them to the top; changing
              ;; the status, the search or the sort genuinely means a different list.
              (tape/HeadedTapeTable
                (e/fn []
                  (tape/SelectAllHeader facts :id item-count (:shown counts) !selected)
                  (tape/SortHeader "Subject" :subject :asc order !order)
                  (tape/SortHeader "Predicate" :predicate :asc order !order)
                  (tape/PlainHeader "Object")
                  (tape/SortHeader "Page" :page :asc order !order)
                  (tape/PlainHeader ""))
                ;; Over the header row, not above the table: a bar that pushed the table
                ;; down by a row on every first tick was visually jarring.
                (e/fn []
                  (tape/BatchBar n-selected (:hidden counts) !selected
                    (e/fn []
                      (when (and searching? (< n-selected doc-fact-count))
                        (SelectDocumentButton doc-ids doc-fact-count !selected))
                      (when (= "duplicate" status)
                        (tape/BatchAction "Not a duplicate" false false
                          (fn [] (reset! !action (scope/action :unmark @!selected true)))))
                      (tape/BatchAction "Delete" true false
                        (fn [] (reset! !confirm-delete {:ids (vec @!selected)}))))))
                grid-cols item-count
                [status query sort-col sort-dir]
                (cond
                  searching? "No facts match this search."
                  (= "duplicate" status) "No duplicates marked in this document."
                  (= "rejected" status) "Nothing deleted in this document."
                  :else "No facts yet. Distill this document first.")
                (e/fn [i]
                  (when-let [fact (e/server (nth facts i nil))]
                    (FactRow user-id fact i status !editing editing !selected selected
                      !menu (:row-key menu) selecting?))))
              (menu/RowMenu !menu menu
                (e/fn [m]
                  (let [id (:id (:row m))]
                    (when (= "duplicate" status)
                      (menu/MenuItem !menu "Not a duplicate" false
                        (fn [] (reset! !action (scope/action :unmark [id] false)))))
                    (menu/MenuItem !menu "Delete" true
                      (fn [] (reset! !action (scope/action :delete [id] false)))))))
              ;; One op, one undo entry — see db/reject-kg-facts! for why this is not a
              ;; loop over the single-fact reject.
              (modal/ConfirmModal !confirm-delete
                "Delete facts?"
                (let [n (count (:ids confirm-delete))]
                  (str "Delete " n " selected fact" (when (not= 1 n) "s")
                    "? Questions covering them are retired too. One Undo restores all of it."))
                "Delete" "btn btn-danger-fill"
                (e/fn [payload]
                  (let [r (e/server (e/Offload #(reject-facts!* user-id (:ids payload))))]
                    (case r
                      (if (:success r)
                        ;; Drop exactly what was deleted; anything skipped (already
                        ;; rejected, not owned) stays ticked rather than silently vanishing.
                        (swap! !selected #(reduce disj % (:ids payload)))
                        r)))))
              ;; The one place a row or batch action runs; it outlives the menu
              ;; that wrote it.
              (let [[t _] (e/Token action)]
                (when t
                  (let [r (e/server (e/Offload #(run-fact-action!* user-id action)))]
                    (case r
                      (case (when (and (:bulk? action) (:success r)) (reset! !selected #{}))
                        (t))))))
              (scope/UrlState {"q" query
                               "status" (when (not= "approved" status) status)
                               "sort" (scope/sort-param order default-sort)}))))))))
