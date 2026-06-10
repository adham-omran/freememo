(ns freememo.library-cards
  "Library 'Cards' sub-view — every card the user owns in one virtual-scrolled
   table, with sync-state badges and the shared edit/delete machinery.
   Routed at /library/cards (LibraryPage branches on the route sub-segment)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [clojure.string :as str]
   [missionary.core :as m]
   [freememo.navigation :as nav]
   [freememo.logging :as log]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.card-components :refer [card-row-html set-inner-html! DeleteCardButton]]
   [freememo.card-modals :refer [EditCardModal]]
   #?(:clj [freememo.anki-sync-server :as sync-server])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.user-state :as us])))

;; ---------------------------------------------------------------------------
;; Sync-state model — four states keyed on the RIGHT columns.
;; Deliberately diverges from card-components/sync-state: a card exported to
;; CSV gets anki_synced_at WITHOUT an anki_note_id, and must show :unpushed
;; here, not :synced. :anki-modified is reserved for the Phase 2 client
;; overlay (live AnkiConnect diff) and is never produced server-side.
;; ---------------------------------------------------------------------------

(defn flashcard-sync-state
  "Derive :unpushed | :modified | :synced from a flashcard row."
  [{:flashcards/keys [anki_note_id anki_synced_at updated_at]}]
  (cond
    (nil? anki_note_id) :unpushed
    (and updated_at anki_synced_at
      (pos? (compare (str updated_at) (str anki_synced_at)))) :modified
    :else :synced))

;; ---------------------------------------------------------------------------
;; Server-side filter/sort pipeline — runs on the full per-user card list,
;; before count; only the visible window crosses the wire (B1).
;; ---------------------------------------------------------------------------

#?(:clj
   (defn- filter-cards-kind [cards kind]
     (if (= kind "all")
       cards
       (filterv #(= (:flashcards/kind %) kind) cards))))

#?(:clj
   (defn- filter-cards-status [cards status]
     (if (= status "all")
       cards
       (filterv #(= status (name (:sync-state %))) cards))))

#?(:clj
   (defn- filter-cards-text
     "Case-insensitive match against stripped card content and document title."
     [cards text]
     (if (str/blank? text)
       cards
       (let [q (str/lower-case (str/trim text))]
         (filterv
           (fn [c]
             (let [hay (str/lower-case
                         (str (helpers/strip-html (or (:flashcards/question c) "")) " "
                           (helpers/strip-html (or (:flashcards/answer c) "")) " "
                           (helpers/strip-html (or (:flashcards/cloze c) "")) " "
                           (or (:root_title c) "")))]
               (str/includes? hay q)))
           cards)))))

#?(:clj
   (defn- sort-user-cards [cards sort-col sort-dir]
     (let [key-fn (case sort-col
                    :document #(str/lower-case (or (:root_title %) ""))
                    :status #(case (:sync-state %) :unpushed 0 :modified 1 :synced 2)
                    :added :flashcards/created_at
                    :flashcards/created_at)
           cmp (if (= sort-dir :asc) compare (fn [a b] (compare b a)))]
       (vec (sort-by key-fn cmp cards)))))

;; Query wrapper — _rev creates the Electric reactive dependency.
;; opts: {:text str :kind str :status str :sort-col kw :sort-dir kw}
;; Returns {:success true :cards [...] :count N :unpushed N :modified N}
;; (counts over the FILTERED set, so the summary narrows with the filters)
;; or {:success false :error msg}.
(defn query-user-cards* [_rev user-id opts]
  #?(:clj
     (try
       (let [{:keys [text kind status sort-col sort-dir]} opts
             all (mapv #(assoc % :sync-state (flashcard-sync-state %))
                   (db/get-user-flashcards user-id))
             filtered (-> all
                        (filter-cards-kind kind)
                        (filter-cards-status status)
                        (filter-cards-text text))
             sorted (sort-user-cards filtered sort-col sort-dir)]
         {:success true
          :cards sorted
          :count (count sorted)
          :unpushed (count (filterv #(= :unpushed (:sync-state %)) sorted))
          :modified (count (filterv #(= :modified (:sync-state %)) sorted))
          ;; Anki-overlay manifest — ALL pushed cards regardless of filters,
          ;; so deletion detection doesn't depend on the current filter state.
          :pushed-manifest (into []
                             (keep (fn [c]
                                     (when-let [nid (:flashcards/anki_note_id c)]
                                       {:card-id (:flashcards/id c) :note-id nid})))
                             all)})
       (catch Exception e
         {:success false :error (.getMessage e)}))
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Anki overlay fetch — client-side AnkiConnect batch.
;; One notesInfo over all pushed note ids + one areSuspended over the anki
;; card ids it returns. Generation-guarded: only the latest run applies.
;; ---------------------------------------------------------------------------

;; Both platforms, no reader conditional — referenced from e/defn bodies
;; (CLAUDE.md frame-mismatch rule).
(defonce !anki-fetch-gen (atom 0))

(defn start-anki-overlay-fetch!
  "Fire the AnkiConnect overlay batch for manifest [{:card-id :note-id} ...].
   pre:  manifest non-empty; !status / !payload are client atoms.
   post (async): !payload ← {:present [{:note-id :stripped-fields :tags
                                        :suspended {:total :suspended}} ...]
                             :absent [note-id ...]}
                 and !status ← :ready — or !status ← :unavailable on any
                 AnkiConnect failure (payload untouched).
   _trigger is unused — callers pass [sync-rev check-tick] so Electric
   re-runs this expression when either changes."
  [manifest _trigger !status !payload]
  #?(:cljs
     (let [my-gen (swap! !anki-fetch-gen inc)
           note-ids (mapv :note-id manifest)]
       (reset! !status :checking)
       (log/log-debug (str "[anki-overlay] fetch start gen=" my-gen
                        " notes=" (count note-ids)))
       ((m/sp
          (let [notes (m/? (helpers/anki-call! "notesInfo" {:notes note-ids}))
                notes-clj (js->clj notes :keywordize-keys true)
                pairs (map vector note-ids notes-clj)
                absent (into []
                         (keep (fn [[nid note]]
                                 (when (or (nil? note) (empty? note) (nil? (:noteId note)))
                                   nid)))
                         pairs)
                present (into []
                          (keep (fn [[nid note]]
                                  (when (and (map? note) (:noteId note))
                                    {:note-id nid
                                     :stripped-fields (mapv helpers/strip-html
                                                        (helpers/ordered-field-values (:fields note)))
                                     :tags (vec (:tags note))
                                     :card-ids (vec (:cards note))})))
                          pairs)
                all-card-ids (into [] (mapcat :card-ids) present)
                suspended-flags (if (seq all-card-ids)
                                  (vec (js->clj (m/? (helpers/anki-call! "areSuspended"
                                                       {:cards all-card-ids}))))
                                  [])
                card-id->suspended (zipmap all-card-ids suspended-flags)]
            {:present (mapv (fn [{:keys [card-ids] :as note}]
                              (-> note
                                (dissoc :card-ids)
                                (assoc :suspended
                                  {:total (count card-ids)
                                   :suspended (count (filter #(true? (get card-id->suspended %))
                                                       card-ids))})))
                        present)
             :absent absent}))
        (fn [result]
          (when (= my-gen @!anki-fetch-gen)
            (log/log-debug (str "[anki-overlay] fetch done gen=" my-gen
                             " present=" (count (:present result))
                             " absent=" (count (:absent result))))
            (reset! !payload result)
            (reset! !status :ready)))
        (fn [err]
          (when (= my-gen @!anki-fetch-gen)
            (log/log-debug (str "[anki-overlay] fetch failed gen=" my-gen
                             " error=" (.-message err)))
            (reset! !status :unavailable)))))
     :clj nil))

;; ---------------------------------------------------------------------------
;; View toggle — shared by LibraryPage (tree header) and LibraryCardsView.
;; Navigation is URL-backed: /library = documents tree, /library/cards = cards.
;; ---------------------------------------------------------------------------

(e/defn LibraryViewToggle [navigate! cards-view?]
  (e/client
    (dom/div
      (dom/props {:style {:display "flex" :gap "4px" :flex-shrink "0"}})
      (dom/button
        (dom/props {:class (str "btn btn-sm " (if cards-view? "btn-secondary" "btn-primary"))})
        (dom/text "Documents")
        (dom/On "click" (fn [_] (navigate! :library)) nil))
      (dom/button
        (dom/props {:class (str "btn btn-sm " (if cards-view? "btn-primary" "btn-secondary"))})
        (dom/text "Cards")
        (dom/On "click" (fn [_] (navigate! :library {:type :library-cards})) nil)))))

;; ---------------------------------------------------------------------------
;; Row — status dot · kind badge · front · back · document · added · delete.
;; Row click opens EditCardModal ({:id :kind :question :answer :cloze}
;; contract); document cell click navigates to the card's topic instead.
;; ---------------------------------------------------------------------------

(defn sync-state-glyph [sync-st]
  (case sync-st
    :unpushed "○"
    "●"))

(defn sync-state-color [sync-st]
  (case sync-st
    :synced "var(--color-success)"
    "var(--color-warning)"))

(defn sync-state-tooltip [sync-st]
  (case sync-st
    :unpushed "Not pushed to Anki"
    :modified "Edited in FreeMemo since last sync"
    :synced "Synced to Anki"
    ""))

(e/defn LibraryCardRow [card navigate! !editing-card user-id i anki-overlay]
  (e/client
    (let [id (e/server (:flashcards/id card))
          kind (e/server (:flashcards/kind card))
          question (e/server (:flashcards/question card))
          answer (e/server (:flashcards/answer card))
          cloze (e/server (:flashcards/cloze card))
          topic-id (e/server (:flashcards/topic_id card))
          root-title (e/server (:root_title card))
          added (e/server (:formatted_date card))
          sync-st (e/server (:sync-state card))
          cloze? (= kind "cloze")
          cell-style {:padding-block "6px" :padding-inline "8px"
                      :display "flex" :align-items "center"
                      :border-bottom "1px solid var(--color-bg-subtle)"}]
      (dom/tr
        (dom/props {:style {:--order (inc i) :cursor "pointer"}})
        (dom/On "click"
          (fn [_] (reset! !editing-card {:id id :kind kind :question question
                                         :answer answer :cloze cloze}))
          nil)
        ;; Status dot
        (dom/td
          (dom/props {:style (merge cell-style {:justify-content "center" :padding-inline "4px"
                                                :font-size "10px"
                                                :color (sync-state-color sync-st)})
                      :data-tooltip (sync-state-tooltip sync-st)})
          (dom/text (sync-state-glyph sync-st)))
        ;; Anki cell — live overlay flags; empty when clean or Anki unavailable
        (let [flags (get anki-overlay id)
              susp (:suspended flags)]
          (dom/td
            (dom/props {:style (merge cell-style {:justify-content "center" :gap "3px"
                                                  :padding-inline "4px" :font-size "11px"})})
            (when (:anki-modified flags)
              (dom/span
                (dom/props {:style {:color "var(--color-warning)"}
                            :data-tooltip "Edited in Anki since last sync"})
                (dom/text "◆")))
            (when (:marked flags)
              (dom/span
                (dom/props {:style {:color "var(--color-primary)"}
                            :data-tooltip "Marked in Anki"})
                (dom/text "★")))
            (when (and susp (pos? (:suspended susp)))
              (dom/span
                (dom/props {:style {:color "var(--color-text-secondary)"
                                    ;; I3 tri-state: solid = all suspended, dimmed = partial
                                    :opacity (if (= (:suspended susp) (:total susp)) "1" "0.45")}
                            :data-tooltip (str (:suspended susp) " of " (:total susp)
                                            " card" (when (not= (:total susp) 1) "s")
                                            " suspended")})
                (dom/text "⏸")))))
        ;; Kind badge
        (dom/td
          (dom/props {:style (merge cell-style {:padding-inline "4px"})})
          (dom/span
            (dom/props {:class "type-badge"
                        :style {:background (if cloze? "var(--color-badge-epub)" "var(--color-badge-pdf)")}})
            (dom/text (if cloze? "Cloze" "Basic"))))
        ;; Front — cloze spans both content columns
        (let [front-html (card-row-html (if cloze? cloze question))]
          (dom/td
            (dom/props {:dir "auto"
                        :class "card-row-cell"
                        :style (merge cell-style (when cloze? {:grid-column "span 2"}))})
            (e/for-by identity [_k [(str "f-" id)]]
              (dom/div
                (dom/props {:class "card-row-html"})
                (set-inner-html! dom/node front-html)))))
        ;; Back — hidden for cloze (front cell spans both)
        (let [back-html (card-row-html (if cloze? "" (or answer "")))]
          (dom/td
            (dom/props {:dir "auto"
                        :class "card-row-cell"
                        :style (merge cell-style (when cloze? {:display "none"}))})
            (e/for-by identity [_k [(str "b-" id)]]
              (dom/div
                (dom/props {:class "card-row-html"})
                (set-inner-html! dom/node back-html)))))
        ;; Document — click navigates to the card's topic
        (dom/td
          (dom/props {:style (merge cell-style {:overflow "hidden" :cursor "pointer"})})
          (dom/On "click"
            (fn [e]
              (.stopPropagation e)
              (navigate! :viewer (nav/nav-topic topic-id :library)))
            nil)
          (dom/span
            (dom/props {:style {:overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"
                                :font-size "12px" :color "var(--color-text-secondary)"}
                        :data-tooltip root-title})
            (dom/text (or root-title ""))))
        ;; Added
        (dom/td
          (dom/props {:style (merge cell-style {:justify-content "flex-end" :padding-inline "6px"
                                                :color "var(--color-text-secondary)" :font-size "12px"})})
          (dom/text (or added "")))
        ;; Delete
        (dom/td
          (dom/props {:style (merge cell-style {:justify-content "center" :padding-inline "4px"})})
          (DeleteCardButton id user-id))))))

;; ---------------------------------------------------------------------------
;; Main view
;; ---------------------------------------------------------------------------

(def ^:private grid-cols "36px 56px 64px 1fr 1fr 200px 80px 44px")

(e/defn LibraryCardsView [user-id navigate! refresh]
  (e/client
    (let [!text (atom "") text (e/watch !text)
          !kind (atom "all") kind (e/watch !kind)
          !status (atom "all") status (e/watch !status)
          !sort-col (atom :added) sort-col (e/watch !sort-col)
          !sort-dir (atom :desc) sort-dir (e/watch !sort-dir)
          !editing-card (atom nil) editing-card (e/watch !editing-card)
          sort-click (fn [col default-dir]
                       (fn [_]
                         (if (= col @!sort-col)
                           (swap! !sort-dir #(if (= % :asc) :desc :asc))
                           (do (reset! !sort-col col)
                             (reset! !sort-dir default-dir)))))
          opts {:text text :kind kind :status status
                :sort-col sort-col :sort-dir sort-dir}
          result (e/server
                   (let [rev (+ refresh
                               (e/watch (us/get-atom user-id :card-mutations))
                               (e/watch (us/get-atom user-id :sync-mutations))
                               (e/watch (us/get-atom user-id :tree-mutations)))]
                     (e/Offload #(query-user-cards* rev user-id opts))))
          success? (e/server (:success result))
          font-sz (or (e/server (settings/get-card-font-size user-id)) 13)
          row-height (+ font-sz 41)
          filters-active? (or (not (str/blank? text)) (not= kind "all") (not= status "all"))

          ;; Anki overlay (Phase 2): client fetches AnkiConnect state, server
          ;; diffs + applies F4 deletions, rows render the sparse flag map.
          !ov-status (atom :idle) ov-status (e/watch !ov-status)
          !ov-payload (atom nil) ov-payload (e/watch !ov-payload)
          !check-tick (atom 0) check-tick (e/watch !check-tick)
          sync-rev (e/server (e/watch (us/get-atom user-id :sync-mutations)))
          manifest (e/server (vec (:pushed-manifest result)))
          overlay-resp (when (some? ov-payload)
                         (let [present (:present ov-payload)
                               absent (:absent ov-payload)]
                           (e/server
                             (e/Offload #(sync-server/apply-anki-overlay! user-id present absent)))))
          anki-overlay (or (:per-card overlay-resp) {})]

      ;; Fetch trigger: view mount, :sync-mutations change (in-app push/pull/
      ;; deletion), manifest change, or Check Anki click. Work-skipping on
      ;; equal values prevents refetch when nothing relevant changed.
      (when (seq manifest)
        (start-anki-overlay-fetch! manifest [sync-rev check-tick] !ov-status !ov-payload))

      (when editing-card
        (EditCardModal !editing-card user-id))

      ;; Header row: toggle + filters
      (dom/div
        (dom/props {:style {:display "flex" :align-items "center" :gap "12px"
                            :margin-bottom "12px" :flex-wrap "wrap"}})
        (LibraryViewToggle navigate! true)
        (dom/input
          (dom/props {:type "text" :placeholder "Filter cards..."
                      :class "input" :style {:flex "1" :min-width "140px"}})
          (dom/On "input" (fn [e] (reset! !text (-> e .-target .-value))) nil))
        (dom/select
          (dom/props {:class "input"})
          (dom/option (dom/props {:value "all"}) (dom/text "All kinds"))
          (dom/option (dom/props {:value "basic"}) (dom/text "Basic"))
          (dom/option (dom/props {:value "cloze"}) (dom/text "Cloze"))
          (dom/On "change" (fn [e] (reset! !kind (-> e .-target .-value))) nil))
        (dom/select
          (dom/props {:class "input"})
          (dom/option (dom/props {:value "all"}) (dom/text "All statuses"))
          (dom/option (dom/props {:value "unpushed"}) (dom/text "Unpushed"))
          (dom/option (dom/props {:value "modified"}) (dom/text "Modified"))
          (dom/option (dom/props {:value "synced"}) (dom/text "Synced"))
          (dom/On "change" (fn [e] (reset! !status (-> e .-target .-value))) nil))
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary"
                      :disabled (or (= ov-status :checking) (empty? manifest))
                      :data-tooltip "Re-check Anki for edits, marks, suspensions and deletions"})
          (dom/text (if (= ov-status :checking) "Checking…" "Check Anki"))
          (dom/On "click" (fn [_] (swap! !check-tick inc)) nil)))

      (if (= false success?)
        (dom/div
          (dom/props {:style {:color "var(--color-danger)" :font-size "13px" :padding "8px 12px"}})
          (dom/text "Error loading cards: " (e/server (:error result))))
        (let [cards-vec (e/server (vec (:cards result)))
              card-count (e/server (or (:count result) 0))
              unpushed-count (e/server (or (:unpushed result) 0))
              modified-count (e/server (or (:modified result) 0))]

          ;; Count summary
          (dom/div
            (dom/props {:style {:padding "0 12px 6px" :font-size "12px"
                                :color "var(--color-text-hint)" :flex-shrink "0"}})
            (dom/text (str card-count " card" (when (not= card-count 1) "s")
                        (when (or (pos? unpushed-count) (pos? modified-count))
                          (str " (" unpushed-count " unpushed, " modified-count " modified)"))
                        (when (= ov-status :unavailable) " · Anki not connected"))))

          ;; Fixed header
          (dom/table
            (dom/props {:class "cards-table-header"
                        :style {:width "100%" :display "grid" :grid-template-columns grid-cols
                                :flex-shrink "0"}})
            (dom/thead
              (dom/props {:style {:display "contents"}})
              (dom/tr
                (dom/props {:style {:display "contents"}})
                (let [th-style {:padding "8px 6px" :border-bottom "2px solid var(--color-border)"
                                :font-weight "600" :font-size "13px"
                                :color "var(--color-text-primary)" :user-select "none"}
                      arrow (fn [col] (when (= sort-col col)
                                        (if (= sort-dir :asc) " ▲" " ▼")))]
                  (dom/th
                    (dom/props {:style (merge th-style {:text-align "center" :cursor "pointer"})
                                :data-tooltip "Sync status"})
                    (dom/text (str "⇄" (arrow :status)))
                    (dom/On "click" (sort-click :status :asc) nil))
                  (dom/th
                    (dom/props {:style (merge th-style {:text-align "center"})
                                :data-tooltip "Live Anki state: edited / marked / suspended"})
                    (dom/text "Anki"))
                  (dom/th
                    (dom/props {:style th-style})
                    (dom/text "Kind"))
                  (dom/th
                    (dom/props {:style (merge th-style {:text-align "left"})})
                    (dom/text "Front"))
                  (dom/th
                    (dom/props {:style (merge th-style {:text-align "left"})})
                    (dom/text "Back"))
                  (dom/th
                    (dom/props {:style (merge th-style {:text-align "left" :cursor "pointer"})})
                    (dom/text (str "Document" (arrow :document)))
                    (dom/On "click" (sort-click :document :asc) nil))
                  (dom/th
                    (dom/props {:style (merge th-style {:text-align "right" :cursor "pointer"})})
                    (dom/text (str "Added" (arrow :added)))
                    (dom/On "click" (sort-click :added :desc) nil))
                  (dom/th
                    (dom/props {:style th-style})
                    (dom/text ""))))))

          ;; Scrollable body
          (dom/div
            (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"
                                :scrollbar-gutter "stable"}})
            (let [[offset limit] (Scroll-window row-height card-count dom/node {:overquery-factor 1})
                  occluded-height (clamp-left (* row-height (- card-count limit)) 0)]
              (dom/props {:class "tape-scroll"
                          :style {:--offset offset :--row-height (str row-height "px")}})
              (dom/table
                (dom/props {:class "cards-table-body"
                            :style {:width "100%" :display "grid" :grid-template-columns grid-cols
                                    :font-size (str font-sz "px")}})
                (if (pos? card-count)
                  (e/for [i (Tape offset limit)]
                    (let [card (e/server (nth cards-vec i nil))]
                      (when card
                        (LibraryCardRow card navigate! !editing-card user-id i anki-overlay))))
                  (dom/tr
                    (dom/td
                      (dom/props {:style {:grid-column "1 / -1" :text-align "center"
                                          :padding "24px 12px" :font-size "13px"
                                          :color "var(--color-text-secondary)"}})
                      (dom/text (if filters-active?
                                  "No cards match the current filters."
                                  "No cards yet. Generate flashcards from your documents to see them here."))))))
              (dom/div (dom/props {:style {:height (str occluded-height "px")}})))))))))
