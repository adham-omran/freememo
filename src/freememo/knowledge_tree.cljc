(ns freememo.knowledge-tree
  "Contents tab — knowledge tree with flatten + virtual scroll."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.scroll :refer [Scroll-window]]
   [contrib.data :refer [clamp-left]]
   [clojure.string :as str]
   [freememo.navigation :as nav]
   [freememo.loading :as loading]
   [freememo.read-region :as read-region]
   [freememo.route-timing :as route-timing]
   [freememo.row-motion :as row-motion]
   [freememo.a11y :as a11y]
   [freememo.bibliography-form :as bibform]
   [freememo.viewport :as viewport]
   [freememo.tree-dnd :as tree-dnd]
   [freememo.context-menu-surface :as menu-surface]
   [freememo.neoreader-kinds :as nrkinds]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.topic-move :as topic-move])
   #?(:clj [freememo.user-state :as us])))

;; Server wrapper — _refresh param creates Electric reactive dependency
(defn get-tree-items* [_refresh user-id]
  #?(:clj (vec (db/get-knowledge-tree user-id))
     :cljs nil))

(defn get-tree-items-result* [_refresh user-id expansion]
  #?(:clj
     (read-region/measured-result :library-tree
       #(vec (db/get-library-tree-items user-id expansion)))
     :cljs (loading/region-success [])))

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

(defn get-root-stats-result* [_refresh user-id]
  #?(:clj
     (read-region/measured-result :library-progress
       #(get-root-stats* _refresh user-id))
     :cljs (loading/region-success {})))

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

(def document-kind-options
  [["all" "All types"]
   ["basic" "Topic"]
   ["pdf" "PDF"]
   ["epub" "EPUB"]
   ["web" "Web"]
   ["wikipedia" "Wikipedia"]
   ["markdown" "Markdown"]
   ["audio" "Audio"]
   ["score" "Score"]
   ["video" "Video"]
   ["video-playlist" "Video playlist"]
   ["youtube" "YouTube"]
   ["code" "Code"]
   ["supermemo" "SuperMemo"]
   ["poem" "Poem"]])

#?(:clj
   (defn filter-by-status [roots status-filter]
     (case status-filter
       "all" roots
       "unsynced" (filterv #(and (pos? (:total-cards %)) (< (:synced-cards %) (:total-cards %))) roots)
       (filterv #(= (completion-status %) (keyword status-filter)) roots))))

;; Flatten tree into a linear list respecting expand state.
;; `expanded?` is a predicate over topic-id (a set works).
;; Returns [{:depth N :topic <map> :has-children bool :is-root bool :expanded? bool}]
(defn flatten-tree [roots children-map expanded?]
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
            ;; No `poem` (poem-reader-spec row 2.4.2), for the reason
            ;; `hierarchy-side-panel/build-rows` records: a poem has no children
            ;; to flatten.
            children (if (and is-root (#{"pdf" "epub" "web" "wikipedia"} kind))
                       (vec (mapcat (fn [c]
                                      (if (= "page" (:topics/kind c))
                                        (get children-map (:topics/id c))
                                        [c]))
                              raw-children))
                       raw-children)
            has-children (boolean (or (:has_children topic) (seq children)))
            is-expanded (boolean (expanded? id))
            new-stack (if (and is-expanded has-children)
                        (into rest-stack
                          (mapv (fn [c] {:depth (inc depth) :topic c :is-root false})
                            (reverse children)))
                        rest-stack)]
        (recur new-stack
          (conj result {:depth depth :topic topic
                        :has-children has-children :is-root is-root
                        :expanded? is-expanded}))))))

;; Server-side tree pipeline — query → stats → filter → sort → flatten.
;; The full collection never crosses the wire: the client receives
;; row-count and the virtual-scroll window rows only (B1).
;; `expansion` is client state {:mode :all|:set :ids #{id}} — in :all mode
;; ids are collapsed exceptions, in :set mode ids are the expanded nodes —
;; so Expand All never enumerates ids client-side (bounded payload at scale).
(defn build-tree-rows
  "Combine the Library structure and maintained root summaries.

   Pre : all-items and stats-map belong to the same user and request key.
   Post: returns filtered, exactly sorted, flattened rows.
   Invariant: progress filters and sorts use the maintained summary values."
  [all-items stats-map filter-text kind-filter status-filter sort-col sort-dir expansion]
  #?(:clj
     (let [roots (-> (extract-roots all-items)
                   (attach-stats stats-map)
                   (filter-root-topics filter-text)
                   (filter-by-kind kind-filter)
                   (filter-by-status status-filter)
                   (sort-root-topics sort-col sort-dir))
           children-map (group-by :topics/parent_id all-items)
           ids (set (:ids expansion))
           expanded? (if (= :all (:mode expansion)) (complement ids) ids)]
       (flatten-tree roots children-map expanded?))
     :cljs []))

(defn tree-row-dto
  "Project one flattened Library row to the fields that its DOM row uses."
  [row]
  (when row
    (let [topic (:topic row)]
      {:depth (:depth row)
       :has-children (:has-children row)
       :is-root (:is-root row)
       :expanded? (:expanded? row)
       :id (:topics/id topic)
       :title (:topics/title topic)
       :kind (:topics/kind topic)
       :source-container (:sources/container_title topic)
       :status (:topics/status topic)
       :dismissed? (:topics/dismissed topic)
       :total-items (:total-items topic)
       :done-items (:done-items topic)
       :total-cards (:total-cards topic)
       :synced-cards (:synced-cards topic)
       :priority (:topics/priority topic)
       :formatted-date (:formatted_date topic)})))

(defn tree-window
  "The DTOs of the flattened rows at display orders [offset, offset + limit).
   Post: each DTO carries its display order as :pos, and :fresh? when its
         topic id is in `arrived`."
  [flat-rows offset limit arrived]
  (let [end (min (count flat-rows) (+ offset limit))]
    (mapv (fn [i]
            (let [dto (tree-row-dto (nth flat-rows i))]
              (assoc dto :pos i :fresh? (contains? arrived (:id dto)))))
      (range (min offset end) end))))

(defn tree-row-ids
  "The topic ids of the flattened rows, as a set."
  [flat-rows]
  (into #{} (map (comp :topics/id :topic)) flat-rows))

(defn tree-row-subject
  "The context-menu subject for one Library row (plans/custom-context-menu.md
   4.2). Every fact the row's commands test is stated, never left absent.

   Delete and NeoReader sync are root-only here, as the ⋯ menu offered them
   before (4.2.3.2): a nested topic is an extract or a page, which its
   document's viewer deletes."
  [{:keys [id title kind is-root has-children dismissed? status priority]}]
  (let [root? (boolean is-root)]
    {:topic-id id
     :title (or title "")
     :topic-kind (or kind "basic")
     :root? root?
     :has-children? (boolean has-children)
     :dismissed? (boolean dismissed?)
     :status (or status "active")
     :priority priority
     :deletable? root?
     :neoreader-syncable? (boolean (and root? (nrkinds/syncable-kind? kind)))}))

;; Badge helper — delegated to bibliography-form/topic-badge so wiki sources
;; render as "Wikipedia" via container-title even when topic.kind='web'.

;; ---------------------------------------------------------------------------
;; DocumentRow — extracted from DocumentTreeView so Electric's macroexpander
;; doesn't blow the stack on the giant inlined row. Atoms passed positionally
;; per docs/electric-patterns.md "Never Put Atoms in Maps Passed to e/defn".
;; ---------------------------------------------------------------------------

(e/defn DocumentRow [user-id navigate! row-dto i row-height
                     !expansion !drag-src drag-src forbidden !drop-cmd]
  (e/client
    ;; The caller projects one bounded DTO on the server. Reading it here makes
    ;; one row payload cross instead of one reactive transfer per field.
    (let [{:keys [depth has-children is-root expanded? id title kind
                  source-container status dismissed? total-items done-items
                  total-cards synced-cards formatted-date fresh?]} row-dto
          title (or title "(empty)")
          kind (or kind "basic")
          is-page (= kind "page")
          topic-status (or status "active")
          dismissed? (boolean dismissed?)
          [badge-text badge-color] (bibform/topic-badge kind source-container)
          open-topic! (fn [_] (navigate! :viewer (nav/nav-topic id :library)))
          ;; The row's context-menu target: the same rows the ⋯ button opens.
          target (menu-surface/topic-row-target (tree-row-subject row-dto) :library)
          toggle-children! (fn [e]
                             (.stopPropagation e)
                             ;; toggling id flips expansion in either mode: it is an
                             ;; exception in :all mode, an expansion in :set mode.
                             ;; No scroll anchoring needed — Scroll-window keys its reset
                             ;; on filters/sort, not record-count, so growing the list
                             ;; in place leaves scrollTop untouched.
                             (swap! !expansion
                               (fn [{:keys [ids] :as ex}]
                                 (assoc ex :ids (if (contains? ids id) (disj ids id) (conj ids id))))))]
      (dom/tr
        ;; Border and height come from CSS (`.library-document-row` and the C1c
        ;; `--row-height`). Inline values beat the phone card rules, so each
        ;; card filled its whole slot and hid the next card's top edge.
        (dom/props {:class (str "library-document-row" (when (even? i) " row-alt"))
                    :role "row"
                    ;; freememo.row-motion follows the key when rows move, and
                    ;; slides in a row that an expand or an import just added.
                    :data-row-key (str id)
                    :data-enter (when fresh? "1")
                    :style {:opacity (if (or dismissed? (= topic-status "done")) "0.6" "1")
                            ;; 0-based absolute index → per-row translateY (C1c)
                            :--order i}})
        (dom/On "click" open-topic! nil)
        ;; Right-click, and the focus target for the palette and Shift+F10
        ;; (plans/custom-context-menu.md 4.2.1). The tr is display:contents,
        ;; but events from its cells still bubble through it.
        (menu-surface/RowMenu target)
        ;; Column 1: Document (arrow + badge + title)
        ;; --row-indent carries the depth indent as a custom property.
        ;; `.library-document-main` applies it as padding-left, and the phone
        ;; media query replaces that padding to flatten the tree. A stylesheet
        ;; rule cannot override the inline property itself.
        ;; Base 31 reserves the fixed left gutter the absolutely positioned
        ;; .drag-grip button occupies: grip spans 1–25px (24px hit area,
        ;; 2.5.8), and the expand arrow's -6px margin pulls its hit box back
        ;; to indent-6 — 31-6=25 keeps the two targets from overlapping
        ;; (see tree_dnd / index.css .drag-grip).
        (dom/td
          (dom/props {:role "cell" :class "library-document-main"
                      :style {:--row-indent (str (+ 31 (* depth 20)) "px")
                              :border-left (when (= topic-status "done") "2px solid var(--color-success-lighter)")}})
          (a11y/KeyActivate {:label (str "Open " title)} open-topic!)
          ;; Native drag-and-drop re-parenting lives on this cell (the tr is
          ;; display:contents → no box → not draggable). Page stubs are flattened
          ;; away under a root PDF, but a NESTED PDF's pages do render here — gate
          ;; them non-draggable so they can't be moved off their document.
          (tree-dnd/DragDropRow! id (not is-page) !drag-src drag-src forbidden !drop-cmd)
          (if has-children
            (dom/span
              (dom/props {:style {:width "28px" :padding "4px 6px" :margin "-4px -6px"
                                  :font-size "12px" :cursor "pointer"
                                  :user-select "none" :text-align "center" :flex-shrink "0"}})
              (a11y/KeyActivate
                {:role "button" :label (str (if expanded? "Collapse " "Expand ") title)}
                toggle-children!)
              (dom/text (if expanded? "▼" "▶"))
              (dom/On "click" toggle-children! nil))
            (dom/span (dom/props {:style {:width "16px" :flex-shrink "0"}})))
          (dom/span
            (dom/props {:class "type-badge" :style {:background badge-color}})
            (dom/text badge-text))
            (dom/span
              (dom/props {:class "library-document-title"
                          :style {:overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"
                                  :font-size (if is-root "13px" "12px")
                                  :font-weight (if is-root "500" "400")
                                  :color "var(--color-text-primary)"}})
              (dom/text title))
          ;; Dismissed tag — the row also greys (opacity above); the label keeps
          ;; Dismissed distinguishable from Done, which greys the same way.
          (when dismissed?
            (dom/span
              (dom/props {:class "type-badge"
                          :style {:background "var(--color-bg-subtle)"
                                  :color "var(--color-text-secondary)"
                                  :flex-shrink "0"}})
              (dom/text "Dismissed"))))
        ;; Column 2: Done
        ;; Stat and action cell layout lives in `.library-document-stat` and
        ;; `.library-document-actions`. Inline values beat the phone card rules.
        (dom/td
          (dom/props {:role "cell" :class "library-document-stat library-document-done"
                      :data-label "Done"})
          (when is-root
            (dom/span
              (dom/props {:style {:color (cond
                                           (and (pos? total-items) (= done-items total-items)) "var(--color-success-dark)"
                                           (pos? done-items) "var(--color-text-primary)"
                                           :else "var(--color-text-secondary)")}})
              (dom/text (str done-items " / " total-items)))))
        ;; Column 3: Synced
        (dom/td
          (dom/props {:role "cell" :class "library-document-stat library-document-synced"
                      :data-label "Synced"})
          (when is-root
            (dom/span
              (dom/text (if (pos? total-cards)
                          (str synced-cards " / " total-cards)
                          "–")))))
        ;; Column 4: Added
        (dom/td
          (dom/props {:role "cell" :class "library-document-stat library-document-added"
                      :data-label "Added"})
          (when is-root
            (dom/span
              (dom/text (or formatted-date "")))))
        ;; Column 5: Actions — the ⋯ button opens the context menu at the
        ;; button, with the rows a right-click shows (4.2.3).
        (dom/td
          (dom/props {:class "library-document-actions"})
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary row-actions-trigger tooltip-right"
                        :style {:padding "2px 8px" :font-size "14px" :line-height "1"}
                        :aria-haspopup "menu"
                        :aria-label (str "Actions for " title)})
            (tooltip/Tooltip! "Actions")
            (dom/text "⋯")
            (dom/On "click"
              (fn [e]
                (.stopPropagation e)
                (menu-surface/open-menu-below! (.-currentTarget e) target))
              nil)))))))

(e/defn LibraryTableBody
  [request-key user-id navigate! row-height grid-cols
   !expansion !scroll-node !drag-src drag-src forbidden !drop-cmd]
  (e/client
    ;; Row ids of the last window, server-side, for row-motion/arrivals!.
    ;; Outside the region, so a refresh that remounts it keeps them.
    (let [!seen (e/server (atom {}))
          ;; Filter and sort, as requested. A change to them scrolls to the top.
          view-key (subvec (vec request-key) 2 7)]
      (loading/WithLatestServerRegion request-key
        ;; Electric let bindings run concurrently. The tree shape and maintained
        ;; progress summary therefore start as separate database reads.
        (e/fn []
          (e/server
            (let [[requested-revision _user-id filter-text kind-filter status-filter
                   sort-col sort-dir expansion]
                  request-key
                  _rev (+ (e/watch (us/get-atom _user-id :refresh))
                         (e/watch (us/get-atom _user-id :tree-mutations)))
                  result-key [_rev _user-id filter-text kind-filter status-filter
                              sort-col sort-dir expansion]
                  items-result (e/Offload #(assoc
                                            (get-tree-items-result* _rev _user-id expansion)
                                            :source-key [_rev _user-id expansion]))
                  stats-result (e/Offload #(assoc
                                            (get-root-stats-result* _rev _user-id)
                                            :source-key [_rev _user-id]))]
              (cond
                (or (not= requested-revision _rev)
                    (not= [_rev _user-id expansion] (:source-key items-result))
                    (not= [_rev _user-id] (:source-key stats-result)))
                {:pending? true}

                (and (:ok? items-result) (:ok? stats-result))
                ;; The filters travel with the rows they built, for the
                ;; row-motion/arrivals! scope: the region keeps the old rows
                ;; while it refreshes, and the current filters must not be
                ;; paired with them.
                (loading/region-success result-key
                  {:rows (build-tree-rows (:value items-result) (:value stats-result)
                           filter-text kind-filter status-filter sort-col sort-dir expansion)
                   :view-key [filter-text kind-filter status-filter sort-col sort-dir]})

                :else
                (loading/region-failure result-key
                  (or (:error items-result) (:error stats-result) "Unknown error"))))))
        (e/fn [tree phase error]
          (let [flat-rows (e/server (:rows tree))
                row-count (e/server (count flat-rows))]
            (case (e/snapshot (route-timing/mark-first-useful! :library :documents))
              (dom/div
                (dom/props {:role "rowgroup"
                            :class (str "library-document-scroll"
                                     (when (= phase :refreshing)
                                       " progressive-region-refreshing"))
                            :style {:flex "1 1 auto" :overflow-y "auto"
                                    :min-height (if (zero? row-count) "72px" "0")
                                    :max-height (str (* 10 row-height) "px")
                                    :position "relative" :scrollbar-gutter "stable"}})
                (reset! !scroll-node dom/node)
                (let [[offset limit] (Scroll-window row-height row-count dom/node
                                       {:overquery-factor 1.5
                                        :reset-key view-key})]
                  (dom/props {:class "tape-scroll"
                              :style {:--count row-count
                                      :--row-height (str row-height "px")
                                      :--grid-cols grid-cols}})
                  (dom/table
                    (dom/props {:class "library-table library-table-body"
                                :role "presentation"
                                :style {:width "100%" :font-size "13px"}})
                    ;; Rows slide by key, whichever node paints them.
                    (e/on-unmount (row-motion/install! dom/node))
                    ;; The window is diffed by topic id on the server, so a
                    ;; row's content and its position cross together. Rows new
                    ;; since the last window slide in. The scroll offset is in
                    ;; the scope, so a row that scrolls into view is not new.
                    (let [arrived (e/server (row-motion/arrivals! !seen [(:view-key tree) offset]
                                              (tree-row-ids flat-rows)))
                          window (e/server
                                   (e/diff-by :id (tree-window flat-rows offset limit arrived)))]
                      (e/for [row-dto window]
                        (DocumentRow user-id navigate! row-dto (:pos row-dto) row-height
                          !expansion !drag-src drag-src forbidden !drop-cmd)))))
                (when (zero? row-count)
                  (dom/div
                    (dom/props {:class "library-document-empty" :role "status"})
                    (dom/text "No content yet. Import content from the Import tab.")))
                (loading/RegionNotice phase error)))))
        (e/fn [_phase]
          (dom/div (dom/props {:class "library-document-loading" :role "rowgroup"})
            (loading/SkeletonRows (if (> row-height 40) 3 6) (str row-height "px"))))
        (e/fn [error]
          (dom/div
            (dom/props {:class "library-document-failed" :role "status"})
            (dom/text (str "The document list could not load. " error))))))))

;; Document tree view — used by LibraryPage
;; Flatten + virtual scroll for performance
;; :refresh covers content mutations. :tree-mutations covers tree-shape changes.
(e/defn DocumentTreeView [user-id navigate! filter-text sort-col sort-dir kind-filter status-filter tree-expanded !sort-cmd]
  (e/client
    (let [!expansion (atom {:mode :set :ids #{}})
          !revision (atom ::pending)
          revision (e/watch !revision)
          ;; Expand All / Collapse All from the parent toggle — switches the
          ;; mode; per-row arrows then toggle ids (exceptions in :all mode).
          expand-cmd (if tree-expanded
                       (reset! !expansion {:mode :all :ids #{}})
                       (reset! !expansion {:mode :set :ids #{}}))
          expansion (do expand-cmd (e/watch !expansion))
          request-key [revision user-id filter-text kind-filter status-filter
                       sort-col sort-dir expansion]
          phone? (e/watch viewport/!phone?)
          row-height (if phone? 88 36)
          grid-cols (if phone? "1fr" "1fr 70px 80px 80px 110px")
          !scroll-node (atom nil)
          ;; Drag-and-drop nesting state. forbidden = the dragged node's subtree
          ;; ids, fetched once per drag start; rows in it are invalid targets.
          !drag-src (atom nil)
          drag-src (e/watch !drag-src)
          !forbidden (atom #{})
          forbidden (e/watch !forbidden)
          !drop-cmd (atom nil)
          drop-cmd (e/watch !drop-cmd)]
            ;; These server values update client atoms as sibling statements.
            ;; Pending reads cannot suppress the table shell or its controls.
            (let [server-revision
                  (e/server (+ (e/watch (us/get-atom user-id :refresh))
                             (e/watch (us/get-atom user-id :tree-mutations))))]
              (when (not= server-revision revision)
                (case server-revision (reset! !revision server-revision))))
            (let [next-forbidden
                  (e/server
                    (if drag-src
                      (e/Offload #(set (db/get-subtree-ids drag-src)))
                      #{}))]
              (when (not= next-forbidden forbidden)
                (case next-forbidden (reset! !forbidden next-forbidden))))

            ;; Commit a drop / promote-to-root via the standard mutation idiom.
            (let [[t _] (e/Token drop-cmd)]
              (when t
                (let [{:keys [src dst]} drop-cmd
                      ok (e/server (e/Offload #(topic-move/move-topic! user-id src dst)))]
                  (case ok
                    (case (e/client (reset! !drop-cmd nil)) (t))))))
            ;; ONE ARIA table spans header + virtual-scrolled body (see the
            ;; identical mapping on the Learn table for the rationale).
            (dom/div
              ;; `flex: 0 1 auto`, NOT `flex: 1`. .table-frame draws the border
              ;; around header + body, so a growing frame paints a box running to
              ;; the bottom of the page while the body inside it stops at its
              ;; 10-row max-height. Content-sized here, the border ends where the
              ;; rows end — the shape Cards gets for free, having no wrapper (its
              ;; border lives on .table-frame-head / .table-frame-body).
              ;; flex-shrink stays 1 so a viewport shorter than the table still
              ;; compresses the frame, and min-height 0 lets the body inside it
              ;; shrink with it.
              (dom/props {:class "table-frame"
                          :role "table" :aria-label "Library documents"
                          :style {:display "flex" :flex-direction "column" :min-height "0" :flex "0 1 auto"}})

              ;; Fixed header — sort headers are keyboard-operable (KeyActivate)
              ;; and expose aria-sort for the active column.
              (dom/table
                (dom/props {:class "library-table library-table-header"
                            :role "presentation"
                            :style {:width "100%" :display "grid" :grid-template-columns grid-cols :flex-shrink "0"}})
                (dom/thead
                  (dom/props {:role "rowgroup" :style {:display "contents"}})
                  (dom/tr
                    (dom/props {:role "row" :style {:display "contents"}})
                    (let [th-style {:padding "8px 10px" :border-bottom "2px solid var(--color-border)"
                                    :font-weight "600" :font-size "13px" :color "var(--color-text-primary)"
                                    :cursor "pointer" :user-select "none"}
                          arrow (fn [col] (when (= sort-col col) (if (= sort-dir :asc) " \u25B2" " \u25BC")))
                          aria-sort (fn [col] (if (= sort-col col)
                                                (if (= sort-dir :asc) "ascending" "descending")
                                                "none"))
                          sort-on! (fn [col dir] (fn [_] (reset! !sort-cmd [col dir])))
                          sort-document! (sort-on! :document :asc)
                          sort-done! (sort-on! :done :asc)
                          sort-synced! (sort-on! :synced :asc)
                          sort-added! (sort-on! :added :desc)]
                      (dom/th
                        (dom/props {:role "columnheader" :aria-sort (aria-sort :document)
                                    :style (merge th-style {:text-align "left"})})
                        (a11y/KeyActivate {} sort-document!)
                        (dom/text (str "Document" (arrow :document)))
                        (dom/On "click" sort-document! nil))
                      (dom/th
                        (dom/props {:role "columnheader" :aria-sort (aria-sort :done)
                                    :style (merge th-style {:text-align "center" :padding "8px 6px"})})
                        (a11y/KeyActivate {} sort-done!)
                        (dom/text (str "Done" (arrow :done)))
                        (dom/On "click" sort-done! nil))
                      (dom/th
                        (dom/props {:role "columnheader" :aria-sort (aria-sort :synced)
                                    :style (merge th-style {:text-align "center" :padding "8px 6px"})})
                        (a11y/KeyActivate {} sort-synced!)
                        (dom/text (str "Synced" (arrow :synced)))
                        (dom/On "click" sort-synced! nil))
                      (dom/th
                        (dom/props {:role "columnheader" :aria-sort (aria-sort :added)
                                    :style (merge th-style {:text-align "right" :padding "8px 6px"})})
                        (a11y/KeyActivate {} sort-added!)
                        (dom/text (str "Added" (arrow :added)))
                        (dom/On "click" sort-added! nil))
                      (dom/th
                        (dom/props {:role "columnheader"
                                    :style (merge th-style {:text-align "right" :padding "8px 6px" :cursor "default"})})
                        (dom/text "Actions"))))))

              (LibraryTableBody request-key user-id navigate! row-height grid-cols
                !expansion !scroll-node !drag-src drag-src forbidden !drop-cmd)))))


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
