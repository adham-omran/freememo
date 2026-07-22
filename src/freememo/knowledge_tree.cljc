(ns freememo.knowledge-tree
  "Contents tab — knowledge tree with flatten + virtual scroll."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Tape]]
   [freememo.scroll :refer [Scroll-window]]
   [contrib.data :refer [clamp-left]]
   [clojure.string :as str]
   [freememo.navigation :as nav]
   [freememo.a11y :as a11y]
   [freememo.card-components :as card-components]
   [freememo.pdf-cache :as pdf-cache]
   [freememo.bibliography-form :as bibform]
   [freememo.modal-shell :as modal]
   [freememo.viewport :as viewport]
   [freememo.util :as util]
   [freememo.tree-dnd :as tree-dnd]
   [freememo.library-row-actions :refer [RowActionsMenu]]
   [freememo.commands :as commands]
   [hyperfiddle.electric-forms5 :as forms]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.logging :as log])
   #?(:clj [freememo.staged-delete :as staged-delete])
   #?(:clj [freememo.topic-move :as topic-move])
   #?(:clj [freememo.user-state :as us])))

;; Trap Tab within `container` so focus cycles the modal's focusable elements
;; instead of escaping to the page behind it. Call from a keydown handler on
;; the modal container; top-level platform-split defn keeps it frame-safe.
#?(:cljs
   (defn trap-tab! [container e]
     (when (= (.-key e) "Tab")
       (let [els (.querySelectorAll container
                   "a[href], button:not([disabled]), input:not([disabled]), select:not([disabled]), textarea:not([disabled]), [tabindex]:not([tabindex='-1'])")
             n (.-length els)]
         (when (pos? n)
           (let [first-el (.item els 0)
                 last-el (.item els (dec n))
                 active (.-activeElement js/document)]
             (cond
               (and (.-shiftKey e) (or (= active first-el) (= active container)))
               (do (.preventDefault e) (.focus last-el))

               (and (not (.-shiftKey e)) (= active last-el))
               (do (.preventDefault e) (.focus first-el)))))))))
#?(:clj (defn trap-tab! [_container _e] nil))

;; Server wrapper — _refresh param creates Electric reactive dependency
(defn get-tree-items* [_refresh user-id]
  #?(:clj (vec (db/get-knowledge-tree user-id))
     :cljs nil))

;; Atomic rename + refresh — single e/server target so Electric can't drop
;; the side effect when the binding is "unused" in the client body.
(defn rename-and-refresh! [user-id id new-title]
  #?(:clj (do (db/rename-topic! id new-title)
            (commands/bump! user-id :rename-topic)
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
            children (if (and is-root (#{"pdf" "epub" "web" "wikipedia"} kind))
                       (vec (mapcat (fn [c]
                                      (if (= "page" (:topics/kind c))
                                        (get children-map (:topics/id c))
                                        [c]))
                              raw-children))
                       raw-children)
            has-children (boolean (seq children))
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
(defn build-tree-rows* [_rev user-id filter-text kind-filter status-filter sort-col sort-dir expansion]
  #?(:clj
     (let [all-items (vec (db/get-knowledge-tree user-id))
           stats-map (get-root-stats* 0 user-id)
           roots (-> (extract-roots all-items)
                   (attach-stats stats-map)
                   (filter-root-topics filter-text)
                   (filter-by-kind kind-filter)
                   (filter-by-status status-filter)
                   (sort-root-topics sort-col sort-dir))
           children-map (group-by :topics/parent_id all-items)
           ids (set (:ids expansion))
           expanded? (if (= :all (:mode expansion)) (complement ids) ids)]
       (flatten-tree roots children-map expanded?))
     :cljs nil))

;; Badge helper — delegated to bibliography-form/topic-badge so wiki sources
;; render as "Wikipedia" via container-title even when topic.kind='web'.

;; ---------------------------------------------------------------------------
;; DocumentRow — extracted from DocumentTreeView so Electric's macroexpander
;; doesn't blow the stack on the giant inlined row. Atoms passed positionally
;; per CLAUDE.md "Never Put Atoms in Maps Passed to e/defn".
;; ---------------------------------------------------------------------------

(e/defn DocumentRow [user-id navigate! row i row-height
                     editing-id
                     !expansion !editing-id !show-confirm !scroll-node
                     !drag-src drag-src forbidden !drop-cmd !dismiss-cmd]
  (e/client
    ;; `row` is a server-sited value (bound via e/server at the call site) —
    ;; pull each field through its own e/server so only the fields this row
    ;; actually renders cross the wire, not the whole row+topic map (mirrors
    ;; LibraryCardRow, library_cards.cljc).
    (let [depth (e/server (:depth row))
          has-children (e/server (:has-children row))
          is-root (e/server (:is-root row))
          expanded? (e/server (:expanded? row))
          id (e/server (get-in row [:topic :topics/id]))
          title (or (e/server (get-in row [:topic :topics/title])) "(empty)")
          kind (or (e/server (get-in row [:topic :topics/kind])) "basic")
          is-page (= kind "page")
          source-container (e/server (get-in row [:topic :sources/container_title]))
          topic-status (or (e/server (get-in row [:topic :topics/status])) "active")
          dismissed? (boolean (e/server (get-in row [:topic :topics/dismissed])))
          total-items (e/server (get-in row [:topic :total-items]))
          done-items (e/server (get-in row [:topic :done-items]))
          total-cards (e/server (get-in row [:topic :total-cards]))
          synced-cards (e/server (get-in row [:topic :synced-cards]))
          formatted-date (e/server (get-in row [:topic :formatted_date]))
          [badge-text badge-color] (bibform/topic-badge kind source-container)
          open-topic! (fn [_] (navigate! :viewer (nav/nav-topic id :library)))
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
        (dom/props {:class (when (even? i) "row-alt")
                    :role "row"
                    :style {:border-bottom "1px solid var(--color-bg-subtle)"
                            :height (str row-height "px")
                            :opacity (if (or dismissed? (= topic-status "done")) "0.6" "1")
                            :cursor "pointer"
                            ;; 0-based absolute index → per-row translateY (C1c)
                            :--order i}})
        (dom/On "click" open-topic! nil)
        ;; Column 1: Document (arrow + badge + title)
        ;; --row-indent carries the depth indent as a custom property so the
        ;; phone media query can flatten it (`var(--row-indent, 10px)` → 10px).
        ;; Base 31 reserves the fixed left gutter the absolutely positioned
        ;; .drag-grip button occupies: grip spans 1–25px (24px hit area,
        ;; 2.5.8), and the expand arrow's -6px margin pulls its hit box back
        ;; to indent-6 — 31-6=25 keeps the two targets from overlapping
        ;; (see tree_dnd / index.css .drag-grip).
        (dom/td
          (dom/props {:role "cell"
                      :style {:display "flex" :align-items "center" :gap "6px"
                              :--row-indent (str (+ 31 (* depth 20)) "px")
                              :padding-left "var(--row-indent)"
                              :overflow "hidden"
                              :border-left (when (= topic-status "done") "2px solid var(--color-success-lighter)")}})
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
              (dom/text (if expanded? "▼" "▶"))
              (dom/On "click" toggle-children! nil))
            (dom/span (dom/props {:style {:width "16px" :flex-shrink "0"}})))
          (dom/span
            (dom/props {:class "type-badge" :style {:background badge-color}})
            (dom/text badge-text))
          (if (= editing-id id)
            (e/for-by identity [_k [id]]
              ;; Select-all on focus so the autofocus-on-mount lets the user
              ;; immediately overwrite the whole title (was the .select half
              ;; of the old rAF focus+select dance). Delegated on "focusin"
              ;; (bubbles, unlike "focus") because Input! owns the <input>
              ;; node — we no longer hold a direct ref to it.
              (dom/On "focusin"
                (fn [e]
                  (let [n (.-target e)]
                    (when (and (instance? js/HTMLInputElement n) (pos? (count (.-value n))))
                      (.select n))))
                nil)
              (dom/On "click" (fn [e] (.stopPropagation e)) nil)
              ;; Esc cancels without saving. Input! unmounts with the rest of
              ;; this branch on reset — no manual value-revert/blur needed,
              ;; unlike the old imperative dance.
              (dom/On "keydown"
                (fn [e]
                  (when (and (= (.-key e) "Escape") (instance? js/HTMLInputElement (.-target e)))
                    (.preventDefault e)
                    (reset! !editing-id nil)))
                nil)
              ;; Managed Forms5 input, bound to `title` (authoritative): while
              ;; focused/dirty it holds the user's draft instead of reflecting
              ;; `title`, so a concurrent rename-and-refresh! elsewhere never
              ;; clobbers an in-progress edit (A5 fix). Its own per-keystroke
              ;; token is intentionally left unconsumed — committing on every
              ;; keystroke would bump :tree-mutations and re-sort/re-fetch the
              ;; whole tree while typing; see the delegated "change" commit
              ;; below instead, which fires once on blur like before.
              (forms/Input! :title title
                :type "text" :placeholder "Title" :maxlength "500" :autofocus true
                :style {:flex "1" :min-width "0" :padding "2px 6px"
                        :font-size (if is-root "13px" "12px")
                        :border "1px solid var(--color-border)"
                        :border-radius "3px"
                        :background "var(--color-bg-card)"})
              ;; Commit on change/blur — delegated (bubbles from Input!'s
              ;; node), same policy as before: blank title cancels, otherwise
              ;; rename + refresh, then close.
              (let [event (dom/On "change"
                            #(when (instance? js/HTMLInputElement (.-target %)) (-> % .-target .-value))
                            nil)
                    [t _] (e/Token event)]
                (when t
                  (let [trimmed (str/trim event)]
                    (if (str/blank? trimmed)
                      (case (e/client (reset! !editing-id nil)) (t))
                      ;; fire-and-forget: rename-and-refresh! has no failure
                      ;; return (returns :ok, or throws — no {:success ...}
                      ;; contract to branch on).
                      (let [ok (e/server (e/Offload #(rename-and-refresh! user-id id trimmed)))]
                        (case ok
                          (case (e/client (reset! !editing-id nil))
                            (t)))))))))
            (dom/span
              (dom/props {:style {:overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"
                                  :font-size (if is-root "13px" "12px")
                                  :font-weight (if is-root "500" "400")
                                  :color "var(--color-text-primary)"}})
              (dom/text title)))
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
        (dom/td
          (dom/props {:role "cell"
                      :style {:padding "4px 6px" :text-align "center"
                              :display "flex" :align-items "center" :justify-content "center"}})
          (when is-root
            (dom/span
              (dom/props {:style {:color (cond
                                           (and (pos? total-items) (= done-items total-items)) "var(--color-success-dark)"
                                           (pos? done-items) "var(--color-text-primary)"
                                           :else "var(--color-text-secondary)")}})
              (dom/text (str done-items " / " total-items)))))
        ;; Column 3: Synced
        (dom/td
          (dom/props {:role "cell"
                      :style {:padding "4px 6px" :text-align "center"
                              :display "flex" :align-items "center" :justify-content "center"
                              :color "var(--color-text-secondary)"}})
          (when is-root
            (dom/span
              (dom/text (if (pos? total-cards)
                          (str synced-cards " / " total-cards)
                          "–")))))
        ;; Column 4: Added
        (dom/td
          (dom/props {:role "cell"
                      :style {:padding "4px 6px" :text-align "right"
                              :display "flex" :align-items "center" :justify-content "flex-end"
                              :color "var(--color-text-secondary)"}})
          (when is-root
            (dom/span
              (dom/text (or formatted-date "")))))
        ;; Column 5: Actions — all row actions collapsed behind one ⋯ menu.
        (dom/td
          (dom/props {:style {:padding "4px 6px" :display "flex" :align-items "center"
                              :justify-content "flex-end"}})
          (RowActionsMenu user-id id title is-root has-children kind dismissed?
            navigate! !editing-id !drop-cmd !show-confirm !scroll-node !dismiss-cmd))))))

;; ---------------------------------------------------------------------------
;; DeleteConfirmModal — extracted to keep DocumentTreeView lean
;; ---------------------------------------------------------------------------

;; Pre:  `show-confirm` is the topic-id to delete, or nil (modal hidden).
;; Post: Esc / backdrop click / Cancel → close (reset! !show-confirm nil).
;;       Delete click or Cmd/Ctrl+Enter → run delete sequence, then close.
;; Invariant: Delete button receives focus on mount (:autofocus true)
;;            so Tab cycles between Delete↔Cancel and the keydown shortcut works.
(e/defn DeleteConfirmModal [user-id show-confirm !show-confirm !scroll-node]
  (e/client
    (when (some? show-confirm)
      (let [!delete-btn (atom nil)]
        (dom/div
          (dom/props {:class "modal-backdrop"
                      :role "dialog" :aria-modal "true"
                      :aria-label "Confirm delete" :tabindex "-1"})
          (modal/FocusReturn)
          (dom/On "click" (fn [_] (reset! !show-confirm nil)) nil)
          (dom/On "keydown"
            (fn [e]
              (cond
                (= (.-key e) "Escape")
                (reset! !show-confirm nil)

                (and (= (.-key e) "Enter") (or (.-metaKey e) (.-ctrlKey e)))
                (when-some [btn @!delete-btn]
                  (.preventDefault e)
                  (.click btn))

                (= (.-key e) "Tab")
                (trap-tab! (.-currentTarget e) e)))
            nil)
          (dom/div
            (dom/props {:class "modal-content modal-sm"})
            (dom/On "click" (fn [e] (.stopPropagation e)) nil)
            (dom/div
              (dom/props {:class "confirm-modal-body"})
              (dom/p (dom/text "Delete this topic? It and its cards are removed; you can undo for 12 hours.")))
            (dom/div
              (dom/props {:class "confirm-modal-actions"})
              (dom/button
                (dom/props {:class "btn btn-secondary"})
                (dom/text "Cancel")
                (dom/On "click" (fn [_] (reset! !show-confirm nil)) nil))
              (dom/button
                (dom/props {:class "btn btn-danger-fill" :autofocus true})
                (reset! !delete-btn dom/node)
                (dom/text "Delete")
                ;; Thread the pre-delete scrollTop through the confirm event so
                ;; it's captured in the click callback (not the reactive body).
                (let [event (dom/On "click"
                              (fn [_] {:topic show-confirm
                                       :st (some-> @!scroll-node .-scrollTop)})
                              nil)
                      [t _] (e/Token event)]
                  (when t
                    (let [topic-to-delete (:topic event)
                          r (e/server (e/Offload #(staged-delete/stage-deletion! user-id topic-to-delete true)))]
                      (case r
                        ;; Anchor scroll across the :tree-mutations re-render —
                        ;; same reset as expand (util/restore-scroll-after-render!).
                        (case (e/client (util/restore-scroll-after-render! @!scroll-node (:st event)))
                          (case (e/client (card-components/try-delete-anki-notes! (:note-ids r)))
                            (case (e/client (pdf-cache/cache-delete topic-to-delete))
                              (case (e/client (reset! !show-confirm nil))
                                (t)))))))))))))))))

;; Document tree view — used by LibraryPage
;; Flatten + virtual scroll for performance
;; `refresh` (caller-provided) covers content-level mutations; combined with
;; the per-user :tree-mutations atom (tree-shape mutations: extract create,
;; rename, delete, document import) so the library tree refreshes on either.
(e/defn DocumentTreeView [user-id navigate! refresh filter-text sort-col sort-dir kind-filter status-filter tree-expanded !sort-cmd]
  (e/client
    (let [!expansion (atom {:mode :set :ids #{}})
          ;; Expand All / Collapse All from the parent toggle — switches the
          ;; mode; per-row arrows then toggle ids (exceptions in :all mode).
          expand-cmd (if tree-expanded
                       (reset! !expansion {:mode :all :ids #{}})
                       (reset! !expansion {:mode :set :ids #{}}))
          expansion (do expand-cmd (e/watch !expansion))
          ;; Flattening is sited server-side: only row-count and the window
          ;; rows cross the wire (see build-tree-rows*). Previously the whole
          ;; slimmed topic list (~2x doc count items) shipped to the client
          ;; on every mount and tree mutation.
          flat-rows (e/server
                      (let [tree-mutations (e/watch (us/get-atom user-id :tree-mutations))
                            rev (+ refresh tree-mutations)]
                        (build-tree-rows* rev user-id filter-text kind-filter
                          status-filter sort-col sort-dir expansion)))
          row-count (e/server (count flat-rows))
          !show-confirm (atom nil)
          show-confirm (e/watch !show-confirm)
          !editing-id (atom nil)
          editing-id (e/watch !editing-id)
          phone? (e/watch viewport/!phone?)
          row-height (if phone? 80 36)
          grid-cols (if phone? "1fr" "1fr 70px 80px 80px 110px")
          !scroll-node (atom nil)
          ;; Drag-and-drop nesting state. forbidden = the dragged node's subtree
          ;; ids, fetched once per drag start; rows in it are invalid targets.
          !drag-src (atom nil)
          drag-src (e/watch !drag-src)
          forbidden (e/server (if drag-src (set (db/get-subtree-ids drag-src)) #{}))
          !drop-cmd (atom nil)
          drop-cmd (e/watch !drop-cmd)
          ;; Row-menu Dismiss/Undismiss. Handled here (not in the transient
          ;; RowActionsMenu) so the server roundtrip runs in a scope that stays
          ;; mounted while the menu closes; :dismiss/:undismiss bump
          ;; :tree-mutations so this tree re-queries and the row re-greys.
          !dismiss-cmd (atom nil)
          dismiss-cmd (e/watch !dismiss-cmd)]
            ;; Commit a drop / promote-to-root via the standard mutation idiom.
            (let [[t _] (e/Token drop-cmd)]
              (when t
                (let [{:keys [src dst]} drop-cmd
                      ok (e/server (e/Offload #(topic-move/move-topic! user-id src dst)))]
                  (case ok
                    (case (e/client (reset! !drop-cmd nil)) (t))))))
            (let [[t _] (e/Token dismiss-cmd)]
              (when t
                (let [{:keys [id dismissed?]} dismiss-cmd
                      ok (e/server (e/Offload #(do (if dismissed?
                                                     (do (db/undismiss-topic! user-id id)
                                                       (log/audit! {:id ::topic-undismissed :user-id user-id
                                                                    :action :update :entity :document :entity-id id}))
                                                     (do (db/dismiss-topic! user-id id)
                                                       (log/audit! {:id ::topic-dismissed :user-id user-id
                                                                    :action :update :entity :document :entity-id id})))
                                                   :ok)))]
                  (case ok
                    (case (e/server (commands/bump! user-id (if dismissed? :undismiss :dismiss)))
                      (case (e/client (reset! !dismiss-cmd nil)) (t)))))))
            ;; ONE ARIA table spans header + virtual-scrolled body (see the
            ;; identical mapping on the Learn table for the rationale).
            (dom/div
              (dom/props {:class "table-frame"
                          :role "table" :aria-label "Library documents"
                          :style {:display "flex" :flex-direction "column" :min-height "0" :flex "1"}})

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

              ;; Scrollable body
              (dom/div
                (dom/props {:role "rowgroup"
                            :style {:flex "1" :overflow-y "auto" :min-height "0" :scrollbar-gutter "stable"}})
                (reset! !scroll-node dom/node)
                (let [[offset limit] (Scroll-window row-height row-count dom/node
                                       {:overquery-factor 2
                                        ;; Reset to top on filter/sort change only.
                                        ;; Excludes expansion + tree mutations so
                                        ;; expand/collapse/delete hold scroll position.
                                        :reset-key [filter-text kind-filter status-filter sort-col sort-dir]})]
                  (dom/props {:class "tape-scroll"
                              ;; C1c per-row transform positioning (see .tape-scroll in index.css):
                              ;; --count → table height (scroll range), --grid-cols → the row grid.
                              :style {:--count row-count :--row-height (str row-height "px")
                                      :--grid-cols grid-cols}})
                  (dom/table
                    (dom/props {:class "library-table library-table-body"
                                :role "presentation"
                                ;; display:block + column template live in index.css (C1c).
                                :style {:width "100%" :font-size "13px"}})
                    (if (pos? row-count)
                      (e/for [i (Tape offset limit)]
                        (let [row (e/server (nth flat-rows i nil))]
                          (when row
                            (DocumentRow user-id navigate! row i row-height
                              editing-id
                              !expansion !editing-id !show-confirm !scroll-node
                              !drag-src drag-src forbidden !drop-cmd !dismiss-cmd))))
                      (dom/tr
                        ;; Opt out of the fixed row-height so the message isn't clipped (C1c).
                        (dom/props {:role "row" :style {:height "auto"}})
                        (dom/td
                          (dom/props {:role "cell"
                                      :style {:grid-column "1 / -1" :text-align "center" :padding "24px 12px"
                                              :color "var(--color-text-secondary)" :font-size "13px"}})
                          (dom/text "No content yet. Import content from the Import tab.")))))
                  ))

              (DeleteConfirmModal user-id show-confirm !show-confirm !scroll-node)))))


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
