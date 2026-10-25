(ns freememo.knowledge-documents
  "Knowledge → Documents: root documents with their fact counts and extraction
   progress, as a curation table (plans/knowledge-tables-selection-and-bulk.md §6).
   A row shows Distill and a ⋯ menu; a selection opens a batch bar with Distill
   and Generate questions. Both spend credits, so both confirm first.

   Its own namespace for the 64KB method cap, as knowledge_known is."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.knowledge-scope :as scope]
   [freememo.loading :as loading]
   [freememo.modal-shell :as modal]
   [freememo.row-menu :as menu]
   [freememo.tape-table :as tape]
   [freememo.tooltip :as tooltip]
   [freememo.util :as util]
   #?(:clj [freememo.commands :as commands])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.kg-extract :as kg])
   #?(:clj [freememo.kg-questions :as kgq])
   #?(:clj [freememo.knowledge-bulk :as bulk])
   #?(:clj [freememo.logging :as log])
   #?(:clj [freememo.user-state :as us])))

(def ^:private sort-keys
  "Sortable column → the row key it sorts by."
  {:document :title :facts :facts :pages :pages-done})

(def ^:private default-sort
  "Most facts first, so the documents a reader curates lead the list (O4)."
  [:facts :desc])

;; ── Server reads and writes ────────────────────────────────────────
;; Body-level reader conditionals: each name appears inside an (e/server …)
;; form, which CLJS compiles too.

(defn get-knowledge-docs*
  "Root documents + fact counts + extraction progress, lean maps for the wire.
   `query` filters by title (case-insensitive substring, mirroring
   knowledge-tree/filter-root-topics); blank/nil = all.

   Post: [{:id :title :kind :facts :distilled? :pages-done :pages-at-cap
           :pages-total}], in the requested order. The three page figures are 0
         for a document nothing has ever extracted from."
  [_kg-bump user-id query sort-col sort-dir]
  #?(:clj
     (let [stats (db/kg-fact-stats user-id)
           ;; The cap MUST be the one the run enforces, or the column reports a
           ;; page as given up on that the next run will retry, or the reverse.
           ;; One query carries all three page figures — see kg-extraction-progress
           ;; for why splitting `total` out of it rendered "30/29".
           progress (db/kg-extraction-progress user-id kg/attempt-cap)
           q (some-> query str/trim not-empty str/lower-case)
           docs (mapv (fn [t]
                        (let [id (:topics/id t)
                              p (get progress id)]
                          {:id id
                           :title (:topics/title t)
                           :kind (:topics/kind t)
                           :facts (get-in stats [id "approved"] 0)
                           ;; distilled? — any extraction attempt OR any fact of any
                           ;; status. The fact half is not redundant: documents
                           ;; distilled before the extraction log existed have facts
                           ;; and no extraction rows, and dropping that half would
                           ;; take the spend confirm away from every one of them.
                           ;; The attempt half is what adds the all-barren document,
                           ;; which has attempts and no facts.
                           ;;
                           ;; `:attempted`, never `(some? p)`: every document with
                           ;; extractable text has a progress entry, so presence means
                           ;; "has pages" and would put the spend confirm in front of
                           ;; every first distill.
                           :distilled? (or (pos? (:attempted p 0))
                                         (pos? (reduce + 0 (vals (get stats id)))))
                           :pages-attempted (:attempted p 0)
                           :pages-done (:done p 0)
                           :pages-at-cap (:at-cap p 0)
                           :pages-total (:total p 0)}))
                  (db/get-root-topics user-id))
           shown (if q
                   (filterv #(str/includes? (str/lower-case (:title %)) q) docs)
                   docs)]
       (if-let [k (sort-keys sort-col)] (tape/sort-rows shown k sort-dir) shown))
     :cljs nil))

(defn bulk-preview*
  "What a bulk run over `ids` would do, for its confirmation: how many of the
   documents exist, were distilled before, and hold approved facts. Read over the
   unfiltered list, because the search may hide part of the selection."
  [_kg-bump user-id ids]
  #?(:clj (let [wanted (set ids)
                docs (filterv #(contains? wanted (:id %))
                       (get-knowledge-docs* nil user-id nil nil nil))]
            {:n (count docs)
             :distilled (count (filter :distilled? docs))
             :with-facts (count (filter #(pos? (:facts %)) docs))})
     :cljs nil))

(defn clear-extraction-records!*
  "Forget a document's extraction records so the next distill re-reads every page.

   The deliberate path for reaching pages already extracted after a prompt or
   model change, because the skip gates on page content alone. Destroys no
   curation: facts, edits and rejections live in kg_facts, and the per-document
   unique indexes stop a re-run resurrecting a rejected fact.

   Pre : doc-id belongs to user-id (callers pass ids from the user's own tree).
   Post: {:cleared n} — pages forgotten; the caller bumps :distill."
  [user-id doc-id]
  #?(:clj (let [n (db/clear-document-extraction-records! user-id doc-id)]
            (log/log-info (str "Cleared extraction records doc-id=" doc-id " pages=" n))
            (commands/bump! user-id :distill)
            {:cleared n})
     :cljs nil))

(defn run-document-action!*
  "Run one document action over `ids` — one row from its menu, or a selection
   from the batch bar. Both start background runs that toast when they end.
   Pre:  kind ∈ #{:distill :generate}.
   Post: {:success true …} or {:success false :error msg}."
  [user-id {:keys [kind ids]}]
  #?(:clj (bulk/report-failure! user-id
            (case kind
              :distill (bulk/start-distills! user-id ids)
              :generate (bulk/start-question-generations! user-id ids)
              {:success false :error "That action is not available."}))
     :cljs nil))

(defn start-distill!*
  "Start one document's distill. Fire-and-forget: the run toasts and bumps
   :distill from its background thread."
  [user-id doc-id]
  #?(:clj (kg/start-distill! user-id doc-id) :cljs nil))

(defn cancel-distill!* [user-id doc-id]
  #?(:clj (kg/cancel-distill! user-id doc-id) :cljs nil))

(defn cancel-generation!* [user-id doc-id]
  #?(:clj (kgq/cancel-atomic-generation! user-id doc-id) :cljs nil))

(defn distilling-docs* [user-id]
  #?(:clj (us/get-atom user-id :distilling-docs) :cljs nil))

(defn generating-docs* [user-id]
  #?(:clj (us/get-atom user-id :generating-questions) :cljs nil))

;; ── Controls ───────────────────────────────────────────────────────

(e/defn DistillButton [user-id doc-id distilling? distilled? !redistill]
  (e/client
    (dom/button
      (dom/props {:type "button" :class "btn btn-sm btn-secondary"
                  :disabled distilling?
                  :aria-label "Distill facts"})
      (tooltip/Tooltip! "Extract facts from this document into the knowledge graph")
      (if distilling?
        (icons/Icon :loader-2 :size 14 :class "spin")
        (icons/Icon :pen-sparkles :size 14))
      (dom/text (if distilling? " Distilling…" " Distill"))
      (if distilled?
        ;; Already distilled — confirm before spending credits/time again. The
        ;; single ConfirmModal lives in DocumentsView, keyed off this doc-id.
        (dom/On "click" (fn [_] (reset! !redistill doc-id)) nil)
        ;; First distill — nothing to lose, go straight through.
        (let [click (dom/On "click" (fn [_] {:id (str (random-uuid))}) nil)
              [t _] (e/Token click)]
          (when t
            ;; fire-and-forget: kg/start-distill! dispatches to a bounded async
            ;; executor and returns before the run finishes; completion toasts +
            ;; bumps :distill later from the background thread.
            (case (e/server (start-distill!* user-id doc-id))
              (t))))))
    (when distilling?
      (dom/button
        ;; "Pause", not "Cancel": every page extracted so far is already persisted,
        ;; so stopping keeps that work and the next Distill resumes from it.
        (dom/props {:type "button" :class "btn btn-sm" :aria-label "Pause distillation"})
        (tooltip/Tooltip! "Stop after the pages in flight. Pages already extracted are kept, and Distill resumes from where this stopped.")
        (dom/text "Pause")
        (let [click (dom/On "click" (fn [_] {:id (str (random-uuid))}) nil)
              [t _] (e/Token click)]
          (when t
            ;; fire-and-forget: kg/cancel-distill! always returns nil.
            (case (e/server (cancel-distill!* user-id doc-id))
              (t))))))))

(e/defn GenerationProgress
  "Shown while a question run is in flight for the document: what is running,
   and its Cancel. Starting a run lives in the ⋯ menu and the batch bar."
  [user-id doc-id]
  (e/client
    (dom/span
      (dom/props {:class "tape-cell-note"})
      (icons/Icon :loader-2 :size 14 :class "spin")
      (dom/text " Generating questions"))
    (dom/button
      (dom/props {:type "button" :class "btn btn-sm" :aria-label "Cancel question generation"})
      (dom/text "Cancel")
      (let [click (dom/On "click" (fn [_] {:id (str (random-uuid))}) nil)
            [t _] (e/Token click)]
        (when t
          ;; fire-and-forget: kgq/cancel-atomic-generation! always returns nil.
          (case (e/server (cancel-generation!* user-id doc-id))
            (t)))))))

(e/defn ExtractionProgressCell
  "Pages extracted, out of the pages the document holds, plus pages given up on.

   The at-cap figure is the reason this column exists rather than a tooltip: a page
   the run has stopped paying to retry is invisible everywhere else, and a silently
   abandoned page is the failure the extraction log was built to prevent.

   Styling is by class, never a per-row inline style map — every cell here mounts
   inside a Tape window, where Electric bills one prop node per style property."
  [pages-done pages-at-cap pages-total]
  (e/client
    (dom/td
      (dom/props {:class "tape-cell-progress"})
      (if (pos? pages-total)
        (dom/span
          (dom/props {:data-tooltip (str pages-done " of " pages-total
                                      " page" (when (not= 1 pages-total) "s")
                                      " extracted")})
          (dom/text (str pages-done "/" pages-total)))
        (dom/span (dom/props {:class "tape-cell-absent"}) (dom/text "—")))
      (when (pos? pages-at-cap)
        (dom/span
          (dom/props {:class "tape-cell-warn"
                      :data-tooltip (str pages-at-cap " page"
                                      (when (not= 1 pages-at-cap) "s")
                                      " failed twice and will not be retried."
                                      "\nEditing a page clears its attempts.")})
          (dom/text (str pages-at-cap " failed")))))))

;; ── Rows ───────────────────────────────────────────────────────────

(defn- has-menu?
  "Whether the row's ⋯ menu would hold anything."
  [{:keys [facts pages-attempted]}]
  (or (pos? facts) (pos? pages-attempted)))

(e/defn DocumentRow [user-id doc i distilling generating !redistill
                     !selected selected !menu open-row-key selecting?]
  (e/client
    (let [{:keys [id title kind facts distilled?
                  pages-done pages-at-cap pages-total]} doc]
      (dom/tr
        (dom/props {:class (when (even? i) "row-alt") :style {:--order i}})
        (tape/SelectCell id title !selected selected)
        (dom/td
          (dom/span (dom/props {:class "tape-cell-text" :dir "auto"}) (dom/text title))
          (dom/span (dom/props {:class "tape-cell-tag"}) (dom/text kind)))
        (dom/td (dom/props {:class "tape-cell-num"}) (dom/text (str facts)))
        (ExtractionProgressCell pages-done pages-at-cap pages-total)
        (dom/td (dom/props {:class (if selecting? "tape-cell-actions tape-cell-actions-inert"
                                     "tape-cell-actions")})
          (DistillButton user-id id (contains? distilling id) distilled? !redistill)
          (when (contains? generating id)
            (GenerationProgress user-id id))
          (when (has-menu? doc)
            (menu/MenuButton !menu open-row-key id title doc)))))))

;; ── View ───────────────────────────────────────────────────────────

(e/defn DocumentsView [user-id navigate! kg-bump]
  (e/client
    (let [initial (util/query-params)
                    !query (atom (or (get initial "q") "")) query (e/watch !query)
          !search-epoch (atom 0) search-epoch (e/watch !search-epoch)
          !order (atom (scope/parse-sort (get initial "sort") (set (keys sort-keys)) default-sort))
          order (e/watch !order)
          [sort-col sort-dir] order
          distilling (e/server (e/watch (distilling-docs* user-id)))
          generating (e/server (e/watch (generating-docs* user-id)))
          !selected (atom #{}) selected (e/watch !selected)
          !menu (atom nil) menu (e/watch !menu)
          !action (atom nil) action (e/watch !action)
          !confirm-bulk (atom nil) confirm-bulk (e/watch !confirm-bulk)
          !redistill (atom nil)               ; doc-id pending a re-distill confirm, or nil
          !forget (atom nil)                  ; doc-id pending a forget-extractions confirm
          grid-cols "34px minmax(240px,1fr) 70px 110px minmax(150px,max-content)"]
      ;; Spinner covers the first offloaded fetch (blank otherwise); the search
      ;; input lives inside Loaded and revalidates in place via the Offload latch.
      (loading/WithLatestServerLoading
        (e/fn [] (e/server (e/Offload #(get-knowledge-docs* kg-bump user-id query sort-col sort-dir))))
        (e/fn [docs]
          ;; docs stays server-sited — see WithLatestServerLoading. Every read of
          ;; it below is wrapped, so only counts and the window rows cross.
          (let [item-count (e/server (count docs))
                counts (e/server (tape/selection-counts docs :id selected))
                n-selected (count selected)
                selecting? (pos? n-selected)
                searching? (not (str/blank? query))
                clear-search! (fn [] (reset! !query "") (swap! !search-epoch inc))
                preview (e/server (when-let [ids (:ids confirm-bulk)]
                                    (e/Offload #(bulk-preview* kg-bump user-id ids))))]
            (dom/div
              (dom/props {:class "knowledge-view"})
              (dom/div
                (dom/props {:class "knowledge-filters"})
                (scope/SearchInput "Search documents…" query !query search-epoch)
                (dom/span (dom/props {:class "knowledge-count"})
                  (dom/text (str item-count " document" (when (not= 1 item-count) "s")))))
              (when searching?
                (dom/div
                  (dom/props {:class "knowledge-filter-labels"})
                  (tape/FilterLabel (str "Search: " query) clear-search!)
                  (tape/ClearFilters clear-search!)))
              (tape/HeadedTapeTable
                (e/fn []
                  (tape/SelectAllHeader docs :id item-count (:shown counts) !selected)
                  (tape/SortHeader "Document" :document :asc order !order)
                  (tape/SortHeader "Facts" :facts :desc order !order)
                  (tape/SortHeader "Pages" :pages :desc order !order)
                  (tape/PlainHeader ""))
                ;; Over the header row, not above the table: a bar that pushed the table
                ;; down by a row on every first tick was visually jarring.
                (e/fn []
                  (tape/BatchBar n-selected (:hidden counts) !selected
                    (e/fn []
                      (tape/BatchAction "Distill" false false
                        (fn [] (reset! !confirm-bulk {:kind :distill :ids (vec @!selected)})))
                      (tape/BatchAction "Generate questions" false false
                        (fn [] (reset! !confirm-bulk {:kind :generate :ids (vec @!selected)}))))))
                grid-cols item-count [query sort-col sort-dir]
                (if searching?
                  "No documents match this search."
                  "No documents yet. Import one first.")
                (e/fn [i]
                  (when-let [doc (e/server (nth docs i nil))]
                    (DocumentRow user-id doc i distilling generating !redistill
                      !selected selected !menu (:row-key menu) selecting?))))
              (menu/RowMenu !menu menu
                (e/fn [m]
                  (let [{:keys [id facts pages-attempted]} (:row m)]
                    (when (and (pos? facts) (not (contains? generating id)))
                      (menu/MenuItem !menu "Generate questions" false
                        (fn [] (reset! !action (scope/action :generate [id] false)))))
                    (when (pos? facts)
                      (menu/MenuItem !menu "Browse facts" false
                        (fn [] (navigate! :knowledge {:type :knowledge-facts :doc-id id}))))
                    ;; `pages-attempted`, not done+at-cap: a document whose only page
                    ;; is extracted-but-unlinked counts in neither, and it has
                    ;; records to forget.
                    (when (pos? pages-attempted)
                      (menu/MenuItem !menu "Forget extractions" false
                        (fn [] (reset! !forget id)))))))
              ;; Bulk runs spend credits per document, so the count and the share
              ;; already distilled are stated before anything starts.
              (modal/ConfirmModal !confirm-bulk
                (if (= :distill (:kind confirm-bulk)) "Distill documents?" "Generate questions?")
                (let [{:keys [n distilled with-facts]} preview
                      docs-text (fn [k] (str k " document" (when (not= 1 k) "s")))]
                  (cond
                    (nil? preview) "Counting the selected documents…"
                    (= :distill (:kind confirm-bulk))
                    (str "Distill " (docs-text n) "? "
                      (when (pos? distilled)
                        (str distilled " of them were distilled before, so they read only "
                          "the pages that changed. "))
                      "Each run spends credits.")
                    :else
                    (str "Generate questions for " (docs-text with-facts) "? "
                      (when (< with-facts n)
                        (str (- n with-facts) " of the selected have no approved facts and are skipped. "))
                      "Each run spends credits.")))
                (if (= :distill (:kind confirm-bulk)) "Distill" "Generate")
                "btn btn-primary"
                (e/fn [payload]
                  (case (reset! !action (scope/action (:kind payload) (:ids payload) true))
                    :started)))
              ;; Single re-distill confirm for the whole view (virtualization-safe;
              ;; per-row modals would ride the Tape's reused DOM nodes). !redistill holds
              ;; the doc-id to re-run; ConfirmModal passes it back as the payload.
              (modal/ConfirmModal !redistill
                "Re-distill document?"
                (str "Pages already extracted are skipped, so this reads only pages "
                  "that changed since the last run, plus any that failed. Your edits "
                  "and rejected facts are untouched.\n\n"
                  "To read every page again, after changing the extraction model, "
                  "use Forget extractions first.")
                "Re-distill" "btn btn-primary"
                (e/fn [doc-id] (e/server (start-distill!* user-id doc-id))))
              ;; Second confirm, for the OTHER direction. The skip gates on page
              ;; content alone, so this is the only path by which a prompt or model
              ;; change reaches pages already extracted.
              (modal/ConfirmModal !forget
                "Forget extraction records?"
                (str "The next Distill will read every page again and bill for all of "
                  "them. Nothing you curated is lost: facts, edits and rejections "
                  "stay, and a rejected fact is not resurrected.")
                "Forget" "btn btn-danger"
                (e/fn [doc-id] (e/server (clear-extraction-records!* user-id doc-id))))
              ;; The one place a row or batch action runs; it outlives the menu and
              ;; the confirmation that wrote it.
              (let [[t _] (e/Token action)]
                (when t
                  (let [r (e/server (e/Offload #(run-document-action!* user-id action)))]
                    (case r
                      (case (when (and (:bulk? action) (:success r)) (reset! !selected #{}))
                        (t))))))
              (scope/UrlState {"q" query
                               "sort" (scope/sort-param order default-sort)}))))))))
