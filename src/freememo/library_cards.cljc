(ns freememo.library-cards
  "Library 'Cards' sub-view — every card the user owns in one virtual-scrolled
   table, with sync-state badges and the shared edit/delete machinery.
   Routed at /library/cards (LibraryPage branches on the route sub-segment)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Tape]]
   [freememo.scroll :refer [Scroll-window]]
   [contrib.data :refer [clamp-left]]
   [clojure.string :as str]
   [freememo.modal-shell :as modal]
   [missionary.core :as m]
   [freememo.navigation :as nav]
   [freememo.logging :as log]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.card-components :refer [card-row-html set-inner-html! DeleteCardButton
                                     try-delete-anki-notes!]]
   [freememo.card-modals :refer [EditCardModal]]
   #?(:clj [freememo.anki-sync-server :as sync-server])
   #?(:clj [freememo.cards :as cards])
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
;; Sync direction — one glyph combining DB state and live Anki overlay.
;; :local-ahead is timestamp-based (can show when content happens to match);
;; :anki-ahead is content-based (overlay diff). Both = :conflict.
;; ---------------------------------------------------------------------------

(defn anki-modified-ids
  "Card ids the overlay marked as edited in Anki — the danger set for push
   (push would clobber the Anki edit) and half of pull's conflict predicate."
  [anki-overlay]
  (set (keep (fn [[cid flags]] (when (:anki-modified flags) cid)) anki-overlay)))

(defn card-sync-direction [sync-st anki-flags]
  (let [local-ahead? (= :modified sync-st)
        anki-ahead? (boolean (:anki-modified anki-flags))]
    (cond
      (= :unpushed sync-st) :unpushed
      (and local-ahead? anki-ahead?) :conflict
      local-ahead? :local-ahead
      anki-ahead? :anki-ahead
      :else :in-sync)))

(defn direction-glyph [direction]
  (case direction
    :unpushed "○"
    :local-ahead "▲"
    :anki-ahead "▼"
    :conflict "▲▼"
    ""))

;; Glyphs are TEXT — use the text-safe variants (danger/warning base tokens
;; are filled-background colors and sit below 4.5:1 as glyph colors).
(defn direction-color [direction]
  (case direction
    :conflict "var(--color-danger-text)"
    :in-sync "var(--color-text-hint)"
    "var(--color-warning-dark)"))

(defn direction-tooltip [direction]
  (case direction
    :unpushed "Not pushed to Anki — click for diff"
    :local-ahead "Edited in FreeMemo — push pending. Click for diff"
    :anki-ahead "Edited in Anki — pull pending. Click for diff"
    :conflict "Edited on BOTH sides — conflict. Click for diff"
    :in-sync "In sync — click for diff"
    ""))

;; ---------------------------------------------------------------------------
;; Line diff — git-style :- :+ := ops via LCS. Inputs are stripped text;
;; fields are card-sized, so the O(n·m) table is trivial.
;; ---------------------------------------------------------------------------

(defn diff-lines
  "Line-level diff from old-text to new-text. Returns [[op line] ...]
   with op ∈ :- (only in old) :+ (only in new) := (common)."
  [old-text new-text]
  (let [a (if (str/blank? old-text) [] (vec (str/split-lines old-text)))
        b (if (str/blank? new-text) [] (vec (str/split-lines new-text)))
        n (count a)
        m (count b)
        ;; lcs[i][j] = LCS length of a[i:] vs b[j:]
        lcs (reduce
              (fn [t i]
                (reduce
                  (fn [t j]
                    (assoc-in t [i j]
                      (if (= (a i) (b j))
                        (inc (get-in t [(inc i) (inc j)]))
                        (max (get-in t [(inc i) j]) (get-in t [i (inc j)])))))
                  t
                  (range (dec m) -1 -1)))
              (vec (repeat (inc n) (vec (repeat (inc m) 0))))
              (range (dec n) -1 -1))]
    (loop [i 0 j 0 out []]
      (cond
        (and (< i n) (< j m) (= (a i) (b j)))
        (recur (inc i) (inc j) (conj out [:= (a i)]))

        (and (< i n) (or (= j m) (>= (get-in lcs [(inc i) j]) (get-in lcs [i (inc j)]))))
        (recur (inc i) j (conj out [:- (a i)]))

        (< j m)
        (recur i (inc j) (conj out [:+ (b j)]))

        :else out))))

(defn diff-section-html
  "One field's diff as HTML: label + −/+/context lines, − = FreeMemo side,
   + = Anki side. Text is escaped; rendered via set-inner-html!."
  [label local-text anki-text]
  (str "<div style=\"font-weight:600;font-size:12px;margin:10px 0 4px;color:var(--color-text-primary)\">"
    (helpers/html-escape label) "</div>"
    (apply str
      (map (fn [[op text]]
             (let [[color prefix] (case op
                                    :- ["var(--color-danger-text)" "− "]
                                    :+ ["var(--color-success-dark, var(--color-success))" "+ "]
                                    ["var(--color-text-secondary)" "  "])]
               (str "<div style=\"color:" color
                 ";font-family:monospace;font-size:12px;white-space:pre-wrap\">"
                 prefix (helpers/html-escape text) "</div>")))
        (diff-lines local-text anki-text)))))

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

;; Per-row projection for the virtual-scrolled list. The full flashcard row
;; carries ~20 keys (io_fields jsonb, occlusion/score/mask, root_topic_id, raw
;; timestamps) but LibraryCardRow renders only these ten. Every windowed row
;; crosses the wire on scroll, so shipping the full row is pure per-scroll waste
;; — this trims it to exactly what the row reads. question/answer/cloze stay:
;; the row renders them and seeds the edit/diff modals from them.
#?(:clj
   (defn- card-list-summary [c]
     {:flashcards/id           (:flashcards/id c)
      :flashcards/kind         (:flashcards/kind c)
      :flashcards/question     (:flashcards/question c)
      :flashcards/answer       (:flashcards/answer c)
      :flashcards/cloze        (:flashcards/cloze c)
      :flashcards/topic_id     (:flashcards/topic_id c)
      :flashcards/anki_note_id (:flashcards/anki_note_id c)
      :root_title              (:root_title c)
      :formatted_date          (:formatted_date c)
      :sync-state              (:sync-state c)}))

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
          ;; Only the trimmed summary crosses the wire; counts/ids/manifest
          ;; below still derive from the full `sorted`/`all` rows.
          :cards (mapv card-list-summary sorted)
          :count (count sorted)
          ;; ids of the filtered+sorted set — drives the header select-all.
          ;; Ints only; content stays server-side.
          :filtered-ids (mapv :flashcards/id sorted)
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
;; Diff modal fetch — one notesInfo for the card under inspection.
;; ---------------------------------------------------------------------------

(defonce !diff-fetch-gen (atom 0))

(defn fetch-anki-note-fields!
  "Fetch one note's stripped field values for the diff modal.
   post (async): !result ← {:state :ready :fields [str ...]}
                          | {:state :absent} | {:state :unavailable}."
  [note-id !result]
  #?(:cljs
     (let [g (swap! !diff-fetch-gen inc)]
       (reset! !result {:state :loading})
       ((helpers/anki-call! "notesInfo" {:notes [note-id]})
        (fn [notes]
          (when (= g @!diff-fetch-gen)
            (let [note (first (js->clj notes :keywordize-keys true))]
              (reset! !result
                (if (and (map? note) (:noteId note))
                  {:state :ready
                   :fields (mapv helpers/strip-html
                             (helpers/ordered-field-values (:fields note)))}
                  {:state :absent})))))
        (fn [_err]
          (when (= g @!diff-fetch-gen)
            (reset! !result {:state :unavailable})))))
     :clj nil))

;; ---------------------------------------------------------------------------
;; Bulk action runners — client async (AnkiConnect), writing phase atoms that
;; the BulkActionRunner's reactive blocks observe. Generation-guarded.
;; ---------------------------------------------------------------------------

(defonce !bulk-run-gen (atom 0))

(defn- bulk-skips-text [{:keys [unpushed conflicts errors skipped-local]}]
  (let [parts (cond-> []
                (pos? (or unpushed 0)) (conj (str unpushed " unpushed"))
                (pos? (or conflicts 0)) (conj (str conflicts " conflicts"))
                (pos? (or skipped-local 0)) (conj (str skipped-local " with local edits"))
                (pos? (or errors 0)) (conj (str errors " errors")))]
    (when (seq parts) (str " · skipped: " (str/join ", " parts)))))

(defn run-bulk-push!
  "Execute the update-push for each per-root bundle, sequentially.
   pre:  bundles-resp = success result of get-bulk-push-bundles; skips holds
   the client-side conflict-exclusion count.
   post (async): !bulk-pairs ← all pairs and !bulk-phase ← :recording-push,
   or (nothing pushed / failure) !action-result set and !bulk-phase ← nil.
   Stored field orderings can be empty (never pushed via modal with that
   model) — falls back to live modelFieldNames per model."
  [bundles-resp skips !bulk-pairs !bulk-skips !bulk-phase !action-result]
  #?(:cljs
     (let [g (swap! !bulk-run-gen inc)
           bundles (:bundles bundles-resp)
           skips (assoc skips :unpushed (:skipped-unpushed bundles-resp 0))]
       ((m/sp
          (let [results
                (m/? (m/reduce conj []
                       (m/ap
                         (let [{:keys [cards settings]} (m/?> 1 (m/seed bundles))
                               need-basic? (and (some #(= "basic" (:flashcards/kind %)) cards)
                                             (empty? (:basic-fields settings)))
                               need-cloze? (and (some #(= "cloze" (:flashcards/kind %)) cards)
                                             (empty? (:cloze-fields settings)))
                               basic-fields (if need-basic?
                                              (vec (js->clj (m/? (helpers/anki-call! "modelFieldNames"
                                                                   {:modelName (:basic-model settings)}))))
                                              (:basic-fields settings))
                               cloze-fields (if need-cloze?
                                              (vec (js->clj (m/? (helpers/anki-call! "modelFieldNames"
                                                                   {:modelName (:cloze-model settings)}))))
                                              (:cloze-fields settings))]
                           (m/? (helpers/do-anki-push! cards
                                  (assoc settings
                                    :basic-fields basic-fields
                                    :cloze-fields cloze-fields)))))))]
            {:pairs (vec (mapcat :pairs results))
             :errors (vec (mapcat :errors results))}))
        (fn [{:keys [pairs errors]}]
          (when (= g @!bulk-run-gen)
            (let [skips (assoc skips :errors (count errors))]
              (reset! !bulk-skips skips)
              (if (seq pairs)
                (do (reset! !bulk-pairs (vec pairs))
                  (reset! !bulk-phase :recording-push))
                (do (reset! !action-result (str "Pushed 0" (bulk-skips-text skips)))
                  (reset! !bulk-phase nil))))))
        (fn [err]
          (when (= g @!bulk-run-gen)
            (reset! !action-result (str "Push failed: " (.-message err)))
            (reset! !bulk-phase nil)))))
     :clj nil))

(defn run-bulk-pull!
  "Pull Anki content for the eligible cards (do-anki-pull! comparison).
   post (async): !bulk-updates ← {:updates :deleted :skipped-conflicts} and
   !bulk-phase ← :recording-pull, or already-in-sync/failure result text
   and !bulk-phase ← nil."
  [cards skipped-conflicts !bulk-updates !bulk-phase !action-result]
  #?(:cljs
     (let [g (swap! !bulk-run-gen inc)]
       ((helpers/do-anki-pull! cards)
        (fn [{:keys [updates deleted]}]
          (when (= g @!bulk-run-gen)
            (if (and (empty? updates) (empty? deleted))
              (do (reset! !action-result
                    (str "Pull: already in sync"
                      (bulk-skips-text {:conflicts skipped-conflicts})))
                (reset! !bulk-phase nil))
              (do (reset! !bulk-updates {:updates (vec updates)
                                         :deleted (vec deleted)
                                         :skipped-conflicts skipped-conflicts})
                (reset! !bulk-phase :recording-pull)))))
        (fn [err]
          (when (= g @!bulk-run-gen)
            (reset! !action-result (str "Pull failed: " (.-message err)))
            (reset! !bulk-phase nil)))))
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

;; Selection checkbox cell
(e/defn RowSelectCell [cell-style id !selected selected]
  (e/client
    (dom/td
      (dom/props {:style (merge cell-style {:justify-content "center" :padding-inline "4px"})})
      (dom/input
        (dom/props {:type "checkbox" :aria-label "Select card" :style {:cursor "pointer"}})
        (set! (.-checked dom/node) (contains? selected id))
        (dom/On "click"
          (fn [e]
            (.stopPropagation e)
            (swap! !selected #(if (contains? % id) (disj % id) (conj % id))))
          nil)))))

;; Diff cell — direction glyph + secondary Anki state icons; click → diff modal
(e/defn RowDiffCell [cell-style sync-st flags diff-payload !diff-card]
  (e/client
    (let [direction (card-sync-direction sync-st flags)
          susp (:suspended flags)]
      (dom/td
        (dom/props {:style (merge cell-style {:justify-content "center" :gap "3px"
                                              :padding-inline "4px" :font-size "11px"
                                              :cursor "pointer"})
                    :data-tooltip (direction-tooltip direction)})
        (let [show-diff! (fn [e]
                           (.stopPropagation e)
                           (reset! !diff-card diff-payload))]
          (dom/On "click" show-diff! nil))
        (dom/span
          (dom/props {:style {:color (direction-color direction)}})
          (dom/text (direction-glyph direction)))
        (when (:marked flags)
          (dom/span
            (dom/props {:style {:color "var(--color-primary-text)" :font-size "9px"}
                        :data-tooltip "Marked in Anki"})
            (dom/text "★")))
        (when (and susp (pos? (:suspended susp)))
          (dom/span
            (dom/props {:style {:color "var(--color-text-secondary)" :font-size "9px"
                                ;; I3 tri-state: solid = all suspended, dimmed = partial
                                :opacity (if (= (:suspended susp) (:total susp)) "1" "0.45")}
                        :data-tooltip (str (:suspended susp) " of " (:total susp)
                                        " card" (when (not= (:total susp) 1) "s")
                                        " suspended")})
            (dom/text "⏸")))))))

;; Front + Back cells — cloze spans both content columns, back hidden for cloze
(e/defn RowContentCells [cell-style cloze? id question answer cloze]
  (e/client
    (let [front-html (card-row-html (if cloze? cloze question))]
      (dom/td
        (dom/props {:dir "auto"
                    :class "card-row-cell"
                    :style (merge cell-style (when cloze? {:grid-column "span 2"}))})
        (e/for-by identity [_k [(str "f-" id)]]
          (dom/div
            (dom/props {:class "card-row-html"})
            (set-inner-html! dom/node front-html)))))
    (let [back-html (card-row-html (if cloze? "" (or answer "")))]
      (dom/td
        (dom/props {:dir "auto"
                    :class "card-row-cell"
                    :style (merge cell-style (when cloze? {:display "none"}))})
        (e/for-by identity [_k [(str "b-" id)]]
          (dom/div
            (dom/props {:class "card-row-html"})
            (set-inner-html! dom/node back-html)))))))

;; Document cell — click navigates to the card's topic
(e/defn RowDocCell [cell-style root-title topic-id navigate!]
  (e/client
    (dom/td
      (dom/props {:style (merge cell-style {:overflow "hidden" :cursor "pointer"})})
      (let [open-document! (fn [e]
                             (.stopPropagation e)
                             (navigate! :viewer (nav/nav-topic topic-id :library)))]
        (dom/On "click" open-document! nil))
      (dom/span
        (dom/props {:style {:overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"
                            :font-size "12px" :color "var(--color-text-secondary)"}
                    :data-tooltip root-title})
        (dom/text (or root-title ""))))))

(e/defn LibraryCardRow [card navigate! !editing-card !diff-card !selected selected user-id i anki-overlay]
  (e/client
    (let [id (e/server (:flashcards/id card))
          kind (e/server (:flashcards/kind card))
          question (e/server (:flashcards/question card))
          answer (e/server (:flashcards/answer card))
          cloze (e/server (:flashcards/cloze card))
          topic-id (e/server (:flashcards/topic_id card))
          note-id (e/server (:flashcards/anki_note_id card))
          root-title (e/server (:root_title card))
          added (e/server (:formatted_date card))
          sync-st (e/server (:sync-state card))
          cloze? (= kind "cloze")
          cell-style {:padding-block "6px" :padding-inline "8px"
                      :display "flex" :align-items "center"
                      :border-bottom "1px solid var(--color-bg-subtle)"}]
      (let [;; Occlusion rows have no text-field editor here — their editor is
            ;; the occlusion modal in the topic view. Ignore the activation
            ;; rather than open EditCardModal with empty fields.
            edit-card! (fn [_] (when (not= kind "occlusion")
                                 (reset! !editing-card {:id id :kind kind :question question
                                                        :answer answer :cloze cloze})))]
      (dom/tr
        (dom/props {:class (when (even? i) "row-alt")
                    ;; 0-based absolute index → per-row translateY (C1c)
                    :style {:--order i :cursor "pointer"}})
        (dom/On "click" edit-card! nil)
        (RowSelectCell cell-style id !selected selected)
        (RowDiffCell cell-style sync-st (get anki-overlay id)
          {:id id :kind kind :question question :answer answer
           :cloze cloze :note-id note-id}
          !diff-card)
        ;; Kind badge — clicking the row (anywhere) opens the editor via the tr.
        (dom/td
          (dom/props {:style (merge cell-style {:padding-inline "4px"})})
          (dom/span
            (dom/props {:class "type-badge"
                        :style {:background (if cloze? "var(--color-badge-epub)" "var(--color-badge-pdf)")}})
            (dom/text (if cloze? "Cloze" "Basic"))))
        (RowContentCells cell-style cloze? id question answer cloze)
        (RowDocCell cell-style root-title topic-id navigate!)
        ;; Added
        (dom/td
          (dom/props {:style (merge cell-style {:justify-content "flex-end" :padding-inline "6px"
                                                :color "var(--color-text-secondary)" :font-size "12px"})})
          (dom/text (or added "")))
        ;; Delete
        (dom/td
          (dom/props {:style (merge cell-style {:justify-content "center" :padding-inline "4px"})})
          (DeleteCardButton id user-id)))))))

;; ---------------------------------------------------------------------------
;; Main view
;; ---------------------------------------------------------------------------

(def ^:private grid-cols "32px 44px 64px 1fr 1fr 200px 80px 44px")

;; ---------------------------------------------------------------------------
;; Diff modal — read-only git-style view of FreeMemo (−) vs Anki (+).
;; ---------------------------------------------------------------------------

(e/defn CardDiffSections [id cloze? local-front local-back note-id anki-result]
  (e/client
    (let [anki-fields (cond
                        (nil? note-id) ["" ""]
                        (= :ready (:state anki-result)) (:fields anki-result)
                        :else nil)
          status-text (cond
                        (nil? note-id) "Not pushed to Anki — all content is local-only."
                        (= :loading (:state anki-result)) "Checking Anki…"
                        (= :unavailable (:state anki-result)) "Anki not connected."
                        (= :absent (:state anki-result)) "Note deleted in Anki — this card will be removed on the next check."
                        :else nil)]
      (when status-text
        (dom/p
          (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
          (dom/text status-text)))
      (when anki-fields
        (let [sections (if cloze?
                         [["Cloze" local-front (nth anki-fields 0 "")]]
                         [["Front" local-front (nth anki-fields 0 "")]
                          ["Back" local-back (nth anki-fields 1 "")]])
              html (apply str (map (fn [[label a b]] (diff-section-html label a b))
                                sections))]
          (e/for-by identity [_k [(str "diff-" id)]]
            (dom/div
              (dom/props {:style {:max-height "50vh" :overflow-y "auto"
                                  :border "1px solid var(--color-border)"
                                  :border-radius "var(--radius-sm)"
                                  :padding "8px 10px"}})
              (set-inner-html! dom/node html))))))))

(e/defn CardDiffModal [!diff-card]
  (e/client
    (let [dc (e/watch !diff-card)]
      (when dc
        (let [{:keys [id kind question answer cloze note-id]} dc
              cloze? (= kind "cloze")
              local-front (helpers/strip-html (or (if cloze? cloze question) ""))
              local-back (helpers/strip-html (or answer ""))
              !anki-result (atom nil)
              anki-result (e/watch !anki-result)]
          (when note-id
            (fetch-anki-note-fields! note-id !anki-result))
          (dom/div
            (dom/props {:class "modal-backdrop" :tabindex "-1" :autofocus true})
            (modal/ModalEscape (fn [] (reset! !diff-card nil)) "Card sync diff")
            (dom/On "click" (fn [_] (reset! !diff-card nil)) nil)
            (dom/div
              (dom/props {:class "modal-content modal-sm"
                          :style {:max-width "640px" :width "90%"}})
              (dom/On "click" (fn [e] (.stopPropagation e)) nil)
              (dom/h3
                (dom/props {:style {:margin "0 0 4px 0" :font-size "15px"}})
                (dom/text (str (if cloze? "Cloze" "Basic") " card — FreeMemo vs Anki")))
              (dom/div
                (dom/props {:style {:font-size "11px" :color "var(--color-text-hint)"
                                    :margin-bottom "8px"}})
                (dom/text "− FreeMemo · + Anki"))
              (CardDiffSections id cloze? local-front local-back note-id anki-result)
              (dom/div
                (dom/props {:style {:display "flex" :justify-content "flex-end" :margin-top "12px"}})
                (dom/button
                  (dom/props {:class "btn btn-secondary"})
                  (dom/text "Close")
                  (dom/On "click" (fn [_] (reset! !diff-card nil)) nil))))))))))

;; ---------------------------------------------------------------------------
;; Bulk delete confirm — destructive over the whole selection.
;; ---------------------------------------------------------------------------

(e/defn BulkDeleteButton [user-id !confirm-bulk-delete !selected !action-result]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-danger-fill"})
      (dom/text "Delete")
      (let [event (dom/On "click" (fn [_] (vec @!selected)) nil)
            [t ?error] (e/Token event)]
        (when ?error
          (dom/div
            (dom/props {:style {:color "var(--color-danger-text)" :font-size "11px"}})
            (dom/text ?error)))
        (when t
          (let [r (e/server (e/Offload #(cards/delete-cards! user-id event)))]
            (when (map? r)
              (if (:success r)
                (case (e/client (try-delete-anki-notes! (:anki-note-ids r)))
                  (case (e/client
                          (do (swap! !selected #(reduce disj % event))
                            (reset! !action-result (str "Deleted " (:deleted r) " card"
                                                     (when (not= 1 (:deleted r)) "s")))
                            (reset! !confirm-bulk-delete false)))
                    (t)))
                (t (:error r))))))))))

(e/defn BulkDeleteConfirmModal [user-id !confirm-bulk-delete !selected !action-result]
  (e/client
    (let [n (count (e/watch !selected))]
      (dom/div
        (dom/props {:class "modal-backdrop" :tabindex "-1" :autofocus true})
        (modal/ModalEscape (fn [] (reset! !confirm-bulk-delete false)) "Confirm bulk delete")
        (dom/On "click" (fn [_] (reset! !confirm-bulk-delete false)) nil)
        (dom/div
          (dom/props {:class "modal-content modal-sm"})
          (dom/On "click" (fn [e] (.stopPropagation e)) nil)
          (dom/div
            (dom/props {:class "confirm-modal-body"})
            (dom/p (dom/text (str "Delete " n " selected card" (when (not= n 1) "s")
                               "? Their Anki notes will also be deleted."))))
          (dom/div
            (dom/props {:class "confirm-modal-actions"})
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !confirm-bulk-delete false)) nil))
            (BulkDeleteButton user-id !confirm-bulk-delete !selected !action-result)))))))

;; ---------------------------------------------------------------------------
;; Bulk action state machine — :pushing → :recording-push, :pulling →
;; :recording-pull. Server prep crosses only ids/settings; AnkiConnect work
;; happens in the client runners; recording is token-gated.
;; ---------------------------------------------------------------------------

(e/defn BulkPushRunner [user-id !bulk-phase !bulk-args !bulk-pairs !bulk-skips !action-result]
  (e/client
    (let [bulk-phase (e/watch !bulk-phase)]

      (when (= bulk-phase :pushing)
        (let [args (e/watch !bulk-args)
              ids (:ids args)
              resp (e/server (e/Offload #(sync-server/get-bulk-push-bundles user-id ids)))]
          (when (map? resp)
            (if (:success resp)
              (run-bulk-push! resp @!bulk-skips !bulk-pairs !bulk-skips !bulk-phase !action-result)
              (do (reset! !action-result (str "Push failed: " (:error resp)))
                (reset! !bulk-phase nil))))))

      (when (= bulk-phase :recording-push)
        (let [pairs (e/watch !bulk-pairs)
              [?t _] (e/Token pairs)]
          (when-some [t ?t]
            (let [r (e/server (e/Offload #(sync-server/finalize-bulk-push! user-id pairs)))]
              (when (map? r)
                (case (e/client
                        (do (reset! !action-result
                              (if (:success r)
                                (str "Pushed " (:count r) (bulk-skips-text @!bulk-skips))
                                (str "Push record failed: " (:error r))))
                          (reset! !bulk-phase nil)))
                  (t))))))))))

(e/defn BulkPullRunner [user-id !bulk-phase !bulk-args !bulk-updates !action-result]
  (e/client
    (let [bulk-phase (e/watch !bulk-phase)]

      (when (= bulk-phase :pulling)
        (let [args (e/watch !bulk-args)
              ids (:ids args)
              include? (boolean (:include? args))
              anki-mod-ids (vec (:anki-modified-ids args))
              resp (e/server (e/Offload #(sync-server/get-cards-for-bulk-pull
                                           user-id ids include? anki-mod-ids)))]
          (when (map? resp)
            (if (:success resp)
              (run-bulk-pull! (vec (:cards resp)) (:skipped-conflicts resp)
                !bulk-updates !bulk-phase !action-result)
              (do (reset! !action-result (str "Pull failed: " (:error resp)))
                (reset! !bulk-phase nil))))))

      (when (= bulk-phase :recording-pull)
        (let [uv (e/watch !bulk-updates)
              [?t _] (e/Token uv)]
          (when-some [t ?t]
            (let [r (e/server (e/Offload #(sync-server/apply-pull-updates user-id
                                            (:updates uv) (:deleted uv))))]
              (when (map? r)
                (case (e/client
                        (do (reset! !action-result
                              (if (:success r)
                                (str "Pulled " (:count r) " updated, " (:deleted r) " deleted"
                                  (bulk-skips-text {:conflicts (:skipped-conflicts uv)}))
                                (str "Pull apply failed: " (:error r))))
                          (reset! !bulk-phase nil)))
                  (t))))))))))

(e/defn BulkActionRunner [user-id !bulk-phase !bulk-args !bulk-pairs !bulk-updates !bulk-skips !action-result]
  (e/client
    (BulkPushRunner user-id !bulk-phase !bulk-args !bulk-pairs !bulk-skips !action-result)
    (BulkPullRunner user-id !bulk-phase !bulk-args !bulk-updates !action-result)))

;; ---------------------------------------------------------------------------
;; Action bar — appears when the selection is non-empty.
;; Push skips Anki-side-edited cards (client overlay knowledge) and pull
;; skips locally-edited cards (server knowledge) unless include-conflicts.
;; ---------------------------------------------------------------------------

(e/defn BulkPushButton [anki-overlay none-selected? busy? anki-ready?
                        !selected !include-conflicts !action-result
                        !bulk-args !bulk-skips !bulk-phase]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-primary"
                  :disabled (or none-selected? busy? (not anki-ready?))
                  :data-tooltip (cond
                                  none-selected? "Select cards first"
                                  (not anki-ready?) "Requires Anki connection"
                                  :else "Update the Anki notes of selected pushed cards")})
      (dom/text "Push updates")
      (dom/On "click"
        (fn [_]
          (let [sel (vec @!selected)
                include? @!include-conflicts
                danger-ids (if include? #{} (anki-modified-ids anki-overlay))
                ids (vec (remove danger-ids sel))]
            (reset! !bulk-skips {:conflicts (count (filter danger-ids sel))})
            (reset! !action-result nil)
            (reset! !bulk-args {:ids ids})
            (reset! !bulk-phase :pushing)))
        nil))))

(e/defn BulkPullButton [anki-overlay none-selected? busy? anki-ready?
                        !selected !include-conflicts !action-result
                        !bulk-args !bulk-skips !bulk-phase]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-secondary"
                  :disabled (or none-selected? busy? (not anki-ready?))
                  :data-tooltip (cond
                                  none-selected? "Select cards first"
                                  (not anki-ready?) "Requires Anki connection"
                                  :else "Apply Anki's content to selected cards")})
      (dom/text "Pull")
      (dom/On "click"
        (fn [_]
          (reset! !bulk-skips nil)
          (reset! !action-result nil)
          ;; conflict = locally-edited AND in this set; the server joins
          ;; the two halves (it owns the timestamp half)
          (reset! !bulk-args {:ids (vec @!selected)
                              :include? @!include-conflicts
                              :anki-modified-ids (vec (anki-modified-ids anki-overlay))})
          (reset! !bulk-phase :pulling))
        nil))))

(e/defn BulkActionBar [anki-overlay ov-status busy?
                       !selected !include-conflicts !action-result
                       !bulk-args !bulk-skips !bulk-phase !confirm-bulk-delete]
  (e/client
    (let [selected (e/watch !selected)
          include-conflicts (e/watch !include-conflicts)
          action-result (e/watch !action-result)
          anki-ready? (= ov-status :ready)
          none-selected? (zero? (count selected))]
      (dom/div
        (dom/props {:style {:display "flex" :align-items "center" :gap "10px"
                            :padding "4px 12px 8px" :flex-wrap "wrap" :flex-shrink "0"
                            :font-size "12px"}})
        (dom/span
          (dom/props {:style {:font-weight "600"}})
          (dom/text (str (count selected) " selected")))
        (BulkPushButton anki-overlay none-selected? busy? anki-ready?
          !selected !include-conflicts !action-result
          !bulk-args !bulk-skips !bulk-phase)
        (BulkPullButton anki-overlay none-selected? busy? anki-ready?
          !selected !include-conflicts !action-result
          !bulk-args !bulk-skips !bulk-phase)
        (dom/button
          (dom/props {:class "btn btn-sm btn-danger-fill"
                      :disabled (or none-selected? busy?)})
          (dom/text "Delete")
          (dom/On "click" (fn [_] (reset! !confirm-bulk-delete true)) nil))
        (dom/label
          (dom/props {:style {:display "flex" :align-items "center" :gap "4px"
                              :cursor "pointer" :user-select "none"}
                      :data-tooltip "Conflicted cards (edited on both sides) are skipped unless checked"})
          (dom/input
            (dom/props {:type "checkbox"})
            (set! (.-checked dom/node) (boolean include-conflicts))
            (dom/On "change" (fn [e] (reset! !include-conflicts (-> e .-target .-checked))) nil))
          (dom/text "include conflicts"))
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary"
                      :disabled none-selected?})
          (dom/text "Clear")
          (dom/On "click" (fn [_] (reset! !selected #{})) nil))
        (when busy?
          (dom/span
            (dom/props {:style {:color "var(--color-text-hint)"}})
            (dom/text "Working…")))
        (when action-result
          (dom/span
            (dom/props {:style {:color "var(--color-text-secondary)"}})
            (dom/text action-result)))))))

;; Count summary line under the filter bar
(e/defn CardsCountSummary [result ov-status]
  (e/client
    (let [card-count (e/server (or (:count result) 0))
          unpushed-count (e/server (or (:unpushed result) 0))
          modified-count (e/server (or (:modified result) 0))]
      (dom/div
        (dom/props {:style {:padding "0 12px 6px" :font-size "12px"
                            :color "var(--color-text-hint)" :flex-shrink "0"}})
        (dom/text (str card-count " card" (when (not= card-count 1) "s")
                    (when (or (pos? unpushed-count) (pos? modified-count))
                      (str " (" unpushed-count " unpushed, " modified-count " modified)"))
                    (when (= ov-status :unavailable) " · Anki not connected")))))))

;; Header row: view toggle + text/kind/status filters + Check Anki.
;; Mounted inside LibraryCardsView's one-batch gate — see the `when` there.
;; Any sibling that mounts solo while the cards query is in flight gets
;; removed by the runtime when the result's children arrive (Electric v3
;; mount bug, observed on the documents→cards branch switch); nothing in
;; this view may mount before the query result exists.
(e/defn CardsFilterBar [navigate! ov-status !text !kind !status !check-tick]
  (e/client
    (dom/div
      (dom/props {:style {:display "flex" :align-items "center" :gap "12px"
                          :margin-bottom "12px" :flex-wrap "wrap"}})
      (LibraryViewToggle navigate! true)
      (dom/input
        (dom/props {:type "text" :placeholder "Filter cards..."
                    :class "input" :style {:flex "1" :min-width "140px"}})
        (dom/On "input" (fn [e] (reset! !text (-> e .-target .-value))) nil))
      (dom/select
        (dom/props {:class "input" :aria-label "Filter by card kind"})
        (dom/option (dom/props {:value "all"}) (dom/text "All kinds"))
        (dom/option (dom/props {:value "basic"}) (dom/text "Basic"))
        (dom/option (dom/props {:value "cloze"}) (dom/text "Cloze"))
        (dom/On "change" (fn [e] (reset! !kind (-> e .-target .-value))) nil))
      (dom/select
        (dom/props {:class "input" :aria-label "Filter by sync status"})
        (dom/option (dom/props {:value "all"}) (dom/text "All statuses"))
        (dom/option (dom/props {:value "unpushed"}) (dom/text "Unpushed"))
        (dom/option (dom/props {:value "modified"}) (dom/text "Modified"))
        (dom/option (dom/props {:value "synced"}) (dom/text "Synced"))
        (dom/On "change" (fn [e] (reset! !status (-> e .-target .-value))) nil))
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary"
                    :disabled (= ov-status :checking)
                    :data-tooltip "Re-check Anki for edits, marks, suspensions and deletions"})
        (dom/text (if (= ov-status :checking) "Checking…" "Check Anki"))
        (dom/On "click" (fn [_] (swap! !check-tick inc)) nil)))))

;; Fixed header table — sortable columns + select-all checkbox
(e/defn CardsTableHeader [filtered-ids selected !selected !sort-col !sort-dir]
  (e/client
    (let [sort-col (e/watch !sort-col)
          sort-dir (e/watch !sort-dir)
          sort-click (fn [col default-dir]
                       (fn [_]
                         (if (= col @!sort-col)
                           (swap! !sort-dir #(if (= % :asc) :desc :asc))
                           (do (reset! !sort-col col)
                             (reset! !sort-dir default-dir)))))]
      (dom/table
        (dom/props {:class "cards-table-header table-frame-head"
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
                (dom/props {:style (merge th-style {:text-align "center" :padding "8px 4px"})})
                (dom/input
                  (dom/props {:type "checkbox" :aria-label "Select all filtered"
                              :style {:cursor "pointer"}
                              :data-tooltip "Select all filtered"})
                  (set! (.-checked dom/node)
                    (boolean (and (seq filtered-ids) (every? selected filtered-ids))))
                  (dom/On "click"
                    (fn [_]
                      (if (and (seq filtered-ids) (every? @!selected filtered-ids))
                        (swap! !selected #(reduce disj % filtered-ids))
                        (swap! !selected into filtered-ids)))
                    nil)))
              (dom/th
                (dom/props {:style (merge th-style {:text-align "center" :cursor "pointer"})
                            :data-tooltip "Sync direction (▲ push pending · ▼ pull pending · ▲▼ conflict · ○ unpushed) — sorts by DB state"})
                (dom/text (str "Δ" (arrow :status)))
                (dom/On "click" (sort-click :status :asc) nil))
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
                (dom/text "")))))))))

;; Virtual-scrolled body
(e/defn CardsTableBody [cards-vec card-count row-height font-sz filters-active?
                        navigate! !editing-card !diff-card !selected selected
                        user-id anki-overlay]
  (e/client
    (dom/div
      (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0" :scrollbar-gutter "stable"}})
      (let [[offset limit] (Scroll-window row-height card-count dom/node {:overquery-factor 2})]
        (dom/props {:class "tape-scroll table-frame-body"
                    ;; C1c per-row transform positioning (see .tape-scroll in index.css):
                    ;; --count → table height (scroll range), --grid-cols → the row grid.
                    :style {:--count card-count :--row-height (str row-height "px")
                            :--grid-cols grid-cols}})
        (dom/table
          (dom/props {:class "cards-table-body"
                      ;; display:block + column template live in index.css (C1c).
                      :style {:width "100%" :font-size (str font-sz "px")}})
          (if (pos? card-count)
            (e/for [i (Tape offset limit)]
              (let [card (e/server (nth cards-vec i nil))]
                (when card
                  (LibraryCardRow card navigate! !editing-card !diff-card !selected
                    selected user-id i anki-overlay))))
            (dom/tr
              ;; Opt out of the fixed row-height so the message isn't clipped (C1c).
              (dom/props {:style {:height "auto"}})
              (dom/td
                (dom/props {:style {:grid-column "1 / -1" :text-align "center"
                                    :padding "24px 12px" :font-size "13px"
                                    :color "var(--color-text-secondary)"}})
                (dom/text (if filters-active?
                            "No cards match the current filters."
                            "No cards yet. Generate flashcards from your documents to see them here."))))))
        ))))

;; Anki overlay (Phase 2): client fetches AnkiConnect state, server diffs +
;; applies F4 deletions; returns the sparse per-card flag map.
;; Fetch trigger: view mount, :sync-mutations change (in-app push/pull/
;; deletion), manifest change, or Check Anki click. Work-skipping on equal
;; values prevents refetch when nothing relevant changed.
(e/defn AnkiOverlay [user-id manifest !ov-status !ov-payload !check-tick]
  (e/client
    (let [sync-rev (e/server (e/watch (us/get-atom user-id :sync-mutations)))
          card-rev (e/server (e/watch (us/get-atom user-id :card-mutations)))
          ov-payload (e/watch !ov-payload)
          check-tick (e/watch !check-tick)]
      (when (seq manifest)
        (start-anki-overlay-fetch! manifest [sync-rev check-tick] !ov-status !ov-payload))
      (let [overlay-resp (when (some? ov-payload)
                           (let [present (:present ov-payload)
                                 absent (:absent ov-payload)
                                 ;; local edits must recompute the diff even when
                                 ;; the Anki payload is byte-identical
                                 overlay-rev (+ sync-rev card-rev)]
                             (e/server
                               (e/Offload #(sync-server/apply-anki-overlay! overlay-rev user-id present absent)))))]
        (or (:per-card overlay-resp) {})))))

;; Diff + bulk-delete modals, mounted on their state atoms.
(e/defn CardsModals [user-id !diff-card !confirm-bulk-delete !selected !action-result]
  (e/client
    (when (e/watch !diff-card)
      (CardDiffModal !diff-card))
    (when (e/watch !confirm-bulk-delete)
      (BulkDeleteConfirmModal user-id !confirm-bulk-delete !selected !action-result))))

;; Fixed header + virtual-scrolled body pair.
(e/defn CardsTables [user-id navigate! result row-height font-sz filters-active?
                     anki-overlay selected !selected !sort-col !sort-dir
                     !editing-card !diff-card]
  (e/client
    (let [cards-vec (e/server (vec (:cards result)))
          card-count (e/server (or (:count result) 0))
          filtered-ids (e/server (vec (:filtered-ids result)))]

      (CardsTableHeader filtered-ids selected !selected !sort-col !sort-dir)

      (CardsTableBody cards-vec card-count row-height font-sz filters-active?
        navigate! !editing-card !diff-card !selected selected
        user-id anki-overlay))))

;; Phase 3: selection + diff modal + bulk action state, plus the tables.
;; Owns the selection/bulk atoms; mounted on the query success branch.
(e/defn CardsSelectionRegion [user-id navigate! result row-height font-sz filters-active?
                              anki-overlay ov-status !sort-col !sort-dir !editing-card]
  (e/client
    (let [!selected (atom #{}) selected (e/watch !selected)
          !diff-card (atom nil)
          !include-conflicts (atom false)
          !action-result (atom nil)
          !bulk-phase (atom nil) bulk-phase (e/watch !bulk-phase)
          !bulk-args (atom nil)
          !bulk-pairs (atom nil)
          !bulk-updates (atom nil)
          !bulk-skips (atom nil)
          !confirm-bulk-delete (atom false)]

      (CardsModals user-id !diff-card !confirm-bulk-delete !selected !action-result)

      (BulkActionRunner user-id !bulk-phase !bulk-args !bulk-pairs !bulk-updates
        !bulk-skips !action-result)

      (CardsCountSummary result ov-status)

      ;; Bulk action bar — always visible; buttons disabled at 0 selection
      (BulkActionBar anki-overlay ov-status (some? bulk-phase)
        !selected !include-conflicts !action-result
        !bulk-args !bulk-skips !bulk-phase !confirm-bulk-delete)

      (CardsTables user-id navigate! result row-height font-sz filters-active?
        anki-overlay selected !selected !sort-col !sort-dir
        !editing-card !diff-card))))

;; Everything downstream of the query result: edit modal, error branch,
;; selection region. Takes `result` from LibraryCardsView's gate — the query
;; runs there so the whole view mounts in one batch (see CardsFilterBar).
(e/defn CardsResultRegion [user-id navigate! result filters-active?
                           !sort-col !sort-dir !editing-card
                           !ov-status !ov-payload !check-tick]
  (e/client
    (let [success? (e/server (:success result))
          font-sz (or (e/server (settings/get-card-font-size user-id)) 13)
          ;; Fixed row height (see content_card_table): 12px padding + 28px line + 1px
          ;; border. font-sz sizes the text within the row, not the row itself.
          row-height 41
          ov-status (e/watch !ov-status)
          manifest (e/server (vec (:pushed-manifest result)))
          anki-overlay (AnkiOverlay user-id manifest
                         !ov-status !ov-payload !check-tick)]

      (when (e/watch !editing-card)
        (EditCardModal !editing-card user-id))

      (if (= false success?)
        (dom/div
          (dom/props {:style {:color "var(--color-danger-text)" :font-size "13px" :padding "8px 12px"}})
          (dom/text "Error loading cards: " (e/server (:error result))))
        (CardsSelectionRegion user-id navigate! result row-height font-sz filters-active?
          anki-overlay ov-status !sort-col !sort-dir !editing-card)))))

(e/defn LibraryCardsView [user-id navigate! refresh]
  (e/client
    (let [!text (atom "") text (e/watch !text)
          !kind (atom "all") kind (e/watch !kind)
          !status (atom "all") status (e/watch !status)
          !sort-col (atom :added) sort-col (e/watch !sort-col)
          !sort-dir (atom :desc) sort-dir (e/watch !sort-dir)
          !editing-card (atom nil)
          !ov-status (atom :idle) ov-status (e/watch !ov-status)
          !ov-payload (atom nil)
          !check-tick (atom 0)
          opts {:text text :kind kind :status status
                :sort-col sort-col :sort-dir sort-dir}
          filters-active? (or (not (str/blank? text)) (not= kind "all") (not= status "all"))
          ;; Server-FORM binding, deliberately not an e/defn call: an e/defn's
          ;; return value materializes at the (client) call site, which shipped
          ;; the entire result map — every card row — to the browser on each
          ;; view entry (~1 MB at 3k cards). A bare (e/server ...) form is
          ;; sited-by-use: only the fields the client actually reads cross
          ;; the wire (counts, filtered-ids, manifest, window rows).
          result (e/server
                   (let [rev (+ refresh
                               (e/watch (us/get-atom user-id :card-mutations))
                               (e/watch (us/get-atom user-id :sync-mutations))
                               (e/watch (us/get-atom user-id :tree-mutations)))]
                     (e/Offload #(query-user-cards* rev user-id opts))))
          success? (e/server (:success result))]
      ;; One-batch gate: nothing mounts until the query result exists.
      ;; A solo-mounted sibling (e.g. the filter bar during the in-flight
      ;; window) is removed by the runtime when the result's children
      ;; arrive — Electric v3 mount bug, triggered by the documents→cards
      ;; branch switch. e/Offload latches the previous result across
      ;; re-queries, so the gate only blanks on first mount, not on
      ;; filter changes.
      (when (some? success?)
        (CardsFilterBar navigate! ov-status !text !kind !status !check-tick)
        (CardsResultRegion user-id navigate! result filters-active?
          !sort-col !sort-dir !editing-card
          !ov-status !ov-payload !check-tick)))))
