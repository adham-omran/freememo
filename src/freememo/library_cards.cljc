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
                                     overlapping-row-html occlusion-row-html
                                     score-row-html try-delete-anki-notes!]]
   [freememo.card-modals :refer [EditCardModal]]
   [freememo.icons :as icons]
   [freememo.tooltip :as tooltip]
   [freememo.util :as util]
   #?(:clj [freememo.anki-sync-server :as sync-server])
   #?(:clj [freememo.cards :as cards])
   #?(:clj [freememo.db :as db])
   #?(:clj [taoensso.telemere :as tel])
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

;; Multi-select facets combine OR within a group, AND across groups. Empty
;; group = no constraint. Kind + the DB-derived origins (unpushed/fm-changed/
;; in-sync) filter server-side here; the Anki-derived facets (anki-changed,
;; both, marked, suspended) filter against overlay-ids the client supplies —
;; nil/empty whenever Anki is disconnected, so the disabled chips can't narrow.

(defn overlay->filter-ids
  "Split the client Anki overlay (per-card flag map) into the id-sets the
   server filter consumes: :anki-changed (also the 'both' input), :marked,
   :suspended. Client-side — the overlay materializes on the client."
  [anki-overlay]
  {:anki-changed (set (keep (fn [[cid f]] (when (:anki-modified f) cid)) anki-overlay))
   :marked       (set (keep (fn [[cid f]] (when (:marked f) cid)) anki-overlay))
   :suspended    (set (keep (fn [[cid f]]
                              (when-let [s (:suspended f)]
                                (when (pos? (:suspended s)) cid)))
                        anki-overlay))})

#?(:clj
   (defn- filter-cards-kinds [cards kinds]
     (if (empty? kinds)
       cards
       (filterv #(contains? kinds (:flashcards/kind %)) cards))))

#?(:clj
   (defn- filter-cards-origins [cards origins overlay-ids]
     (if (empty? origins)
       cards
       (let [anki-changed (:anki-changed overlay-ids)]
         (filterv
           (fn [c]
             (let [st (:sync-state c)
                   anki? (contains? anki-changed (:flashcards/id c))]
               (boolean
                 (some (fn [o]
                         (case o
                           "unpushed"     (= st :unpushed)
                           "fm-changed"   (= st :modified)
                           "in-sync"      (= st :synced)
                           "anki-changed" anki?
                           "both"         (and (= st :modified) anki?)
                           false))
                   origins))))
           cards)))))

#?(:clj
   (defn- filter-cards-flags [cards flags overlay-ids]
     (if (empty? flags)
       cards
       (let [marked (:marked overlay-ids)
             suspended (:suspended overlay-ids)]
         (filterv
           (fn [c]
             (let [id (:flashcards/id c)]
               (boolean
                 (some (fn [fl]
                         (case fl
                           "marked"    (contains? marked id)
                           "suspended" (contains? suspended id)
                           false))
                   flags))))
           cards)))))

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

;; Kinds whose front cell spans both content columns (the back cell is hidden).
(def ^:private span2-kinds #{"cloze" "occlusion" "score" "overlapping"})

#?(:clj
   (defn- card-front-html
     "Server-rendered front-cell HTML, one branch per kind — mirrors the item
      view's CardRow so /cards and the topic view render identically. Built
      here, server-side, so the heavy source fields (occlusion join columns,
      overlapping jsonb) never cross the wire per scrolled row; only the
      finished string does."
     [c]
     (case (:flashcards/kind c)
       "occlusion"   (occlusion-row-html (:occlusion_image_media_id c) (:flashcards/mask_ordinal c)
                       (:occlusion_mode c) (get-in c [:flashcards/io_fields :header]))
       "score"       (score-row-html (:flashcards/score_direction c) (:score_start_ms c) (:score_end_ms c))
       "overlapping" (overlapping-row-html (:flashcards/overlapping c))
       "cloze"       (card-row-html (:flashcards/cloze c))
       (card-row-html (:flashcards/question c)))))

;; Per-row projection for the virtual-scrolled list. The full flashcard row
;; carries ~20 keys but the row renders a server-built HTML string plus the
;; document context; every windowed row crosses the wire on scroll, so this
;; trims to exactly that. question/answer/cloze/overlapping stay ONLY to seed
;; the edit/diff modals on row click (basic/cloze/overlapping) — never for
;; display. topic_title + root_topic_id drive the "Root / Document" cell.
#?(:clj
   (defn- card-list-summary [c]
     (let [kind (:flashcards/kind c)
           span2? (contains? span2-kinds kind)]
       {:flashcards/id            (:flashcards/id c)
        :flashcards/kind          kind
        :flashcards/topic_id      (:flashcards/topic_id c)
        :flashcards/root_topic_id (:flashcards/root_topic_id c)
        :flashcards/anki_note_id  (:flashcards/anki_note_id c)
        :front-html               (card-front-html c)
        :back-html                (if span2? "" (card-row-html (or (:flashcards/answer c) "")))
        :span2?                   span2?
        :flashcards/question      (:flashcards/question c)
        :flashcards/answer        (:flashcards/answer c)
        :flashcards/cloze         (:flashcards/cloze c)
        :flashcards/overlapping   (:flashcards/overlapping c)
        :root_title               (:root_title c)
        :topic_title              (:topic_title c)
        :formatted_date           (:formatted_date c)
        :sync-state               (:sync-state c)})))

;; Query wrapper — _rev creates the Electric reactive dependency.
;; opts: {:text str :kinds #{str} :origins #{str} :flags #{str}
;;        :overlay-ids {:anki-changed #{id} :marked #{id} :suspended #{id}}
;;        :sort-col kw :sort-dir kw}
;; Returns {:success true :cards [...] :count N :unpushed N :modified N}
;; (counts over the FILTERED set, so the summary narrows with the filters)
;; or {:success false :error msg}.
(defn query-user-cards* [_rev user-id opts]
  #?(:clj
     (try
       (let [{:keys [text kinds origins flags overlay-ids sort-col sort-dir]} opts
             all (mapv #(assoc % :sync-state (flashcard-sync-state %))
                   (db/get-user-flashcards user-id))
             filtered (-> all
                        (filter-cards-kinds kinds)
                        (filter-cards-origins origins overlay-ids)
                        (filter-cards-flags flags overlay-ids)
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
          :modified (count (filterv #(= :modified (:sync-state %)) sorted))})
       (catch Exception e
         (tel/error! {:id ::query-user-cards :data {:user-id user-id}} e)
         {:success false :error (.getMessage e)}))
     :cljs nil))

;; Filter-independent overlay manifest. Separate from query-user-cards* so it
;; can be computed UPSTREAM of the filtered query — the overlay's flag id-sets
;; feed the query's opts, and sourcing the manifest here (not from the filtered
;; result) keeps that a plain acyclic value flow. _rev = reactive dependency.
(defn pushed-manifest* [_rev user-id]
  #?(:clj (db/get-pushed-card-manifest user-id)
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
   Note types are app-owned; do-anki-push! reads no model/field config."
  [bundles-resp skips !bulk-pairs !bulk-skips !bulk-phase !action-result]
  #?(:cljs
     (let [g (swap! !bulk-run-gen inc)
           bundles (:bundles bundles-resp)
           skips (assoc skips :unpushed (:skipped-unpushed bundles-resp 0))]
       ((m/sp
          (let [results
                (m/? (m/reduce conj []
                       (m/ap
                         (let [{:keys [cards settings]} (m/?> 1 (m/seed bundles))]
                           (m/? (helpers/do-anki-push! cards settings))))))]
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

;; Rendered with the shared .segmented CSS but NOT its ARIA: .segmented was
;; written for a role="radiogroup" (card-type picker in content_toolbar_settings,
;; whose rules must not be edited from here). This switcher navigates, so the
;; selected segment is aria-current="page", not a checked radio.
(e/defn LibraryViewToggle [navigate! cards-view?]
  (e/client
    (dom/div
      (dom/props {:class "segmented" :style {:flex-shrink "0"}})
      (dom/button
        (dom/props {:class (str "segmented-option" (when-not cards-view? " segmented-option--selected"))
                    :aria-current (if cards-view? "false" "page")})
        (dom/text "Documents")
        (dom/On "click" (fn [_] (navigate! :library)) nil))
      (dom/button
        (dom/props {:class (str "segmented-option" (when cards-view? " segmented-option--selected"))
                    :aria-current (if cards-view? "page" "false")})
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
                                              :cursor "pointer"})})
        (tooltip/Tooltip! (direction-tooltip direction))
        (let [show-diff! (fn [e]
                           (.stopPropagation e)
                           (reset! !diff-card diff-payload))]
          (dom/On "click" show-diff! nil))
        (dom/span
          (dom/props {:style {:color (direction-color direction)}})
          (dom/text (direction-glyph direction)))
        (when (:marked flags)
          (dom/span
            (dom/props {:style {:color "var(--color-primary-text)" :font-size "9px"}})
            (tooltip/Tooltip! "Marked in Anki")
            (dom/text "★")))
        (when (and susp (pos? (:suspended susp)))
          (dom/span
            (dom/props {:style {:color "var(--color-text-secondary)" :font-size "9px"
                                ;; I3 tri-state: solid = all suspended, dimmed = partial
                                :opacity (if (= (:suspended susp) (:total susp)) "1" "0.45")}})
            (tooltip/Tooltip! (str (:suspended susp) " of " (:total susp)
                                " card" (when (not= (:total susp) 1) "s")
                                " suspended"))
            (dom/text "⏸")))))))

;; Front + Back cells — span2? kinds (cloze/occlusion/score/overlapping) span
;; both content columns with the back cell hidden. front-html/back-html are
;; built server-side (card-front-html) so every kind renders one way.
(e/defn RowContentCells [cell-style span2? id front-html back-html]
  (e/client
    (dom/td
      (dom/props {:dir "auto"
                  :class "card-row-cell"
                  :style (merge cell-style (when span2? {:grid-column "span 2"}))})
      (e/for-by identity [_k [(str "f-" id)]]
        (dom/div
          (dom/props {:class "card-row-html"})
          (set-inner-html! dom/node front-html))))
    (dom/td
      (dom/props {:dir "auto"
                  :class "card-row-cell"
                  :style (merge cell-style (when span2? {:display "none"}))})
      (e/for-by identity [_k [(str "b-" id)]]
        (dom/div
          (dom/props {:class "card-row-html"})
          (set-inner-html! dom/node back-html))))))

;; Document cell — "Root / Document", or "Root" alone when the card sits on the
;; root topic. Click navigates to the card's topic.
(e/defn RowDocCell [cell-style doc-label topic-id navigate!]
  (e/client
    (dom/td
      (dom/props {:style (merge cell-style {:overflow "hidden" :cursor "pointer"})})
      (let [open-document! (fn [e]
                             (.stopPropagation e)
                             (navigate! :viewer (nav/nav-topic topic-id :library)))]
        (dom/On "click" open-document! nil))
      (dom/span
        (dom/props {:style {:overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"
                            :font-size "12px" :color "var(--color-text-secondary)"}})
        (tooltip/Tooltip! doc-label)
        (dom/text (or doc-label ""))))))

(e/defn LibraryCardRow [card navigate! !editing-card !diff-card !selected selected user-id i anki-overlay]
  (e/client
    ;; Plain client-sited keyword lookups, NOT 16 × (e/server (:k card)).
    ;; Each e/server read is its own cross-peer reactive node; at 22 windowed
    ;; rows that was 350+ nodes mounting and unmounting on every filter change.
    ;; Profiling a filter change put ~all the time in incseq/missionary
    ;; transfer machinery with no app frame on the list — the cost was node
    ;; COUNT, not payload (measured: 31 KB moved, 2.3 s of main thread burned).
    ;; A client-sited lookup forces `card` across exactly once and makes the
    ;; rest local. Every field below is consumed on the client anyway, and the
    ;; row map is already trimmed by card-list-summary, so no extra bytes.
    (let [id (:flashcards/id card)
          kind (:flashcards/kind card)
          question (:flashcards/question card)
          answer (:flashcards/answer card)
          cloze (:flashcards/cloze card)
          ol (:flashcards/overlapping card)
          topic-id (:flashcards/topic_id card)
          root-topic-id (:flashcards/root_topic_id card)
          note-id (:flashcards/anki_note_id card)
          root-title (:root_title card)
          topic-title (:topic_title card)
          front-html (:front-html card)
          back-html (:back-html card)
          span2? (:span2? card)
          added (:formatted_date card)
          sync-st (:sync-state card)
          ;; "Root / Document", or just "Root" when the card is on the root topic.
          doc-label (if (= topic-id root-topic-id)
                      root-title
                      (str root-title " / " topic-title))
          cell-style {:padding-block "6px" :padding-inline "8px"
                      :display "flex" :align-items "center"
                      :border-bottom "1px solid var(--color-bg-subtle)"}
          ;; Occlusion/score have no in-place editor here — their editors live
          ;; in the topic view (occlusion modal / score toolbar), which need
          ;; doc-context /cards lacks. Route the click to the card's document
          ;; instead. basic/cloze/overlapping seed the edit modal.
          edit-card! (fn [_]
                       (case kind
                         ("occlusion" "score") (navigate! :viewer (nav/nav-topic topic-id :library))
                         "overlapping" (reset! !editing-card {:id id :kind kind :overlapping ol})
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
                        :style {:background (case kind
                                             "overlapping" "var(--color-badge-web, #7c5cbf)"
                                             "cloze" "var(--color-badge-epub)"
                                             "occlusion" "var(--color-badge-occlusion)"
                                             "score" "var(--color-badge-score)"
                                             "var(--color-badge-pdf)")}})
            (dom/text (case kind
                        "overlapping" "Overlap" "cloze" "Cloze"
                        "occlusion" "Occlusion" "score" "Score" "Basic"))))
        (RowContentCells cell-style span2? id front-html back-html)
        (RowDocCell cell-style doc-label topic-id navigate!)
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
                (dom/props {:style {:display "flex" :align-items "center" :gap "6px"
                                    :font-size "11px" :color "var(--color-text-hint)"
                                    :margin-bottom "8px"}})
                (dom/span (dom/props {:class "source-chip source-chip-fm"}) (dom/text "FM"))
                (dom/span (dom/text "removed  ·"))
                (dom/span (dom/props {:class "source-chip source-chip-anki"}) (dom/text "Anki"))
                (dom/span (dom/text "added")))
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
                  :disabled (or none-selected? busy? (not anki-ready?))})
      (tooltip/Tooltip! (cond
                         none-selected? "Select cards first"
                         (not anki-ready?) "Requires Anki connection"
                         :else "Update the Anki notes of selected pushed cards"))
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
                  :disabled (or none-selected? busy? (not anki-ready?))})
      (tooltip/Tooltip! (cond
                         none-selected? "Select cards first"
                         (not anki-ready?) "Requires Anki connection"
                         :else "Apply Anki's content to selected cards"))
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
        (dom/props {:class "cards-summary-actions"})
        (dom/span
          (dom/props {:style {:font-weight "600"}})
          (dom/text (str (count selected) " selected")))
        (BulkPushButton anki-overlay none-selected? busy? anki-ready?
          !selected !include-conflicts !action-result
          !bulk-args !bulk-skips !bulk-phase)
        (BulkPullButton anki-overlay none-selected? busy? anki-ready?
          !selected !include-conflicts !action-result
          !bulk-args !bulk-skips !bulk-phase)
        ;; .btn-danger (outline) not .btn-danger-fill (solid): Push updates is
        ;; the one filled/primary action in this bar. FLAGGED — the mockup shows
        ;; a light-red FILL with red text, which no existing class provides.
        (dom/button
          (dom/props {:class "btn btn-sm btn-danger"
                      :disabled (or none-selected? busy?)})
          (dom/text "Delete")
          (dom/On "click" (fn [_] (reset! !confirm-bulk-delete true)) nil))
        (dom/label
          (dom/props {:style {:display "flex" :align-items "center" :gap "4px"
                              :cursor "pointer" :user-select "none"}})
          (tooltip/Tooltip! "Conflicted cards (edited on both sides) are skipped unless checked")
          (dom/input
            (dom/props {:type "checkbox"})
            (set! (.-checked dom/node) (boolean include-conflicts))
            (dom/On "change" (fn [e] (reset! !include-conflicts (-> e .-target .-checked))) nil))
          (dom/text "Include conflicts"))
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

;; Count cluster. One of the two flex children of .cards-summary — it owns no
;; padding or border of its own; the parent places it on the shared line.
;; The counts stay on screen while filter-pending? — they describe the PREVIOUS
;; filter set, which is also what the rows below still show (e/Offload latches),
;; so counts and rows never disagree. The spinner marks both as superseded
;; rather than blanking them, which would flicker on every chip click.
(e/defn CardsCountSummary [result ov-status filter-pending?]
  (e/client
    (let [card-count (e/server (or (:count result) 0))
          unpushed-count (e/server (or (:unpushed result) 0))
          modified-count (e/server (or (:modified result) 0))]
      (dom/div
        (dom/props {:class "cards-summary-counts"
                    :aria-busy (str (boolean filter-pending?))})
        (dom/text (str card-count " card" (when (not= card-count 1) "s")
                    " · " unpushed-count " unpushed"
                    " · " modified-count " modified"
                    (when (= ov-status :unavailable) " · Anki not connected")))
        ;; Always in the DOM at a reserved width — only visibility toggles — so
        ;; the action cluster beside it does not jump right on every chip click.
        ;; visibility:hidden also keeps it out of the a11y tree; aria-busy above
        ;; is what announces the state.
        (dom/span
          (dom/props {:class (str "cards-filtering" (when filter-pending? " cards-filtering--on"))})
          (dom/span (dom/props {:class "spinner"}))
          (dom/text "Filtering…"))))))

;; Filter facets. Multi-select within a group (OR); AND across groups.
;; Kind + the DB origins filter server-side; the overlay? origins and every
;; flag need the live Anki overlay and disable until it is :ready.
(def ^:private kind-facets
  [["basic" "Basic"] ["cloze" "Cloze"] ["overlapping" "Overlap"]
   ["occlusion" "Occlusion"] ["score" "Score"]])
;; [value label overlay-dependent? sync-source]
;; sync-source ("fm" / "anki" / nil) drives BOTH the leading dot colour and the
;; tinted inactive border — one field, not two, so the pair cannot disagree.
(def ^:private origin-facets
  [["unpushed" "Unpushed" false nil]
   ["fm-changed" "FM changed" false "fm"]
   ["anki-changed" "Anki changed" true "anki"]
   ["both" "Both" true nil]
   ["in-sync" "In sync" false nil]])
(def ^:private flag-facets [["marked" "Marked"] ["suspended" "Suspended"]])

;; ── URL filter state ────────────────────────────────────────────────
;; Query-string params, written with replaceState on every change (including
;; every keystroke) and read ONCE at mount. No history entries, therefore no
;; popstate listener. Param names and value grammar live here; the
;; replaceState mechanics live in freememo.util.
;;
;; :origins and :flags are separate params, not one merged "sync" list — they
;; are separate atoms with different Anki-readiness semantics, and the toolbar
;; divider between them is purely visual.
(def ^:private kind-values (into #{} (map first) kind-facets))
(def ^:private origin-values (into #{} (map first) origin-facets))
(def ^:private flag-values (into #{} (map first) flag-facets))
(def ^:private sort-col-values #{:status :document :added})
(def ^:private sort-dir-values #{:asc :desc})
(def ^:private default-sort [:added :desc])

(defn- csv->set
  "Comma-separated param → set, dropping anything not in `valid`.
   Unknown values are IGNORED, never an error and never an empty result —
   a hand-mangled URL degrades to 'no constraint', not to 'no cards'."
  [valid s]
  (into #{} (comp (map str/trim) (filter valid)) (str/split (or s "") #",")))

(defn- set->csv [s] (str/join "," (sort s)))

(defn- filters->query [text kinds origins flags sort-col sort-dir]
  {"q" text
   "type" (set->csv kinds)
   "sync" (set->csv origins)
   "flag" (set->csv flags)
   ;; Omitted at the default so a pristine view has a clean URL.
   "sort" (when (not= [sort-col sort-dir] default-sort)
            (str (name sort-col) ":" (name sort-dir)))})

(defn- query->filters
  "Params map → initial filter values. Total: every malformed input falls back
   to its default, so no URL can produce a broken view."
  [params]
  (let [[c d] (str/split (or (get params "sort") "") #":")
        col (keyword c)
        dir (keyword d)]
    {:text (or (get params "q") "")
     :kinds (csv->set kind-values (get params "type"))
     :origins (csv->set origin-values (get params "sync"))
     :flags (csv->set flag-values (get params "flag"))
     :sort-col (if (sort-col-values col) col (first default-sort))
     :sort-dir (if (sort-dir-values dir) dir (second default-sort))}))

;; One toggle chip. on-toggle fires only when enabled (disabled buttons emit
;; no click, but guard anyway). sync-source paints the leading swatch and tints
;; the inactive border; both colours live in CSS, keyed off the same string.
(e/defn FilterChip [label active? disabled? sync-source on-toggle]
  (e/client
    (dom/button
      (dom/props {:class (str "filter-chip"
                           (when sync-source (str " filter-chip-" sync-source))
                           (when active? " filter-chip-active"))
                  :disabled (boolean disabled?)
                  :aria-pressed (boolean active?)})
      (when sync-source
        (dom/span (dom/props {:class (str "filter-chip-dot filter-chip-dot-" sync-source)})))
      (dom/span (dom/text label))
      (dom/On "click" (fn [_] (when-not disabled? (on-toggle))) nil))))

(defn- toggle-in! [!set v]
  (swap! !set #(if (contains? % v) (disj % v) (conj % v))))

;; Header row: view toggle + text + Kind/Origin/Flag chip groups + Check Anki.
;; Mounted inside LibraryCardsView's one-batch gate — see the `when` there.
;; Any sibling that mounts solo while the cards query is in flight gets
;; removed by the runtime when the result's children arrive (Electric v3
;; mount bug, observed on the documents→cards branch switch); nothing in
;; this view may mount before the query result exists.
(e/defn CardsFilterBar [navigate! ov-status initial-text !text !text-debounced !text-timer
                        !kinds !origins !flags !check-tick]
  (e/client
    (let [kinds (e/watch !kinds)
          origins (e/watch !origins)
          flags (e/watch !flags)
          anki-ready? (= ov-status :ready)]
      (dom/div
        (dom/props {:class "cards-toolbar"})

        ;; Row 1 — view switcher, search, action. Check Anki is an ACTION and
        ;; must never share the chips' pill shape: shape encodes role here.
        (dom/div
          (dom/props {:class "cards-toolbar-row"})
          (LibraryViewToggle navigate! true)
          (dom/div
            (dom/props {:class "cards-search"})
            (dom/span
              (dom/props {:class "cards-search-icon"})
              (icons/Icon :search :size 15))
            (dom/input
              (dom/props {:type "text" :placeholder "Filter cards..." :class "input"})
              ;; Seeded ONCE from the URL. e/snapshot freezes the first value,
              ;; so this never fights the user's typing or moves the caret —
              ;; the input is uncontrolled from here on.
              (set! (.-value dom/node) (e/snapshot initial-text))
              ;; Two writes per keystroke: !text is instant and drives the URL;
              ;; !text-debounced lags 300 ms and drives the query. Undebounced,
              ;; every keystroke ran a full query and typing marched through
              ;; zero-result states, each costing ~2.4 s of main-thread block
              ;; remounting the windowed rows.
              (dom/On "input"
                (fn [e]
                  (let [v (-> e .-target .-value)]
                    (reset! !text v)
                    (util/debounce! !text-timer !text-debounced v 300)))
                nil)))
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary cards-action"
                        :disabled (= ov-status :checking)})
            (tooltip/Tooltip! "Re-check Anki for edits, marks, suspensions and deletions")
            (icons/Icon :refresh-cw :size 14)
            (dom/text (if (= ov-status :checking) "Checking…" "Check Anki"))
            (dom/On "click" (fn [_] (swap! !check-tick inc)) nil)))

        ;; Row 2 — card-kind chips
        (dom/div
          (dom/props {:class "cards-toolbar-row" :role "group" :aria-label "Filter by card kind"})
          (dom/span (dom/props {:class "filter-caption"}) (dom/text "Type"))
          (e/for-by first [facet kind-facets]
            (let [[v label] facet]
              (FilterChip label (contains? kinds v) false nil
                (fn [] (toggle-in! !kinds v))))))

        ;; Row 3 — sync-state chips, hairline, manual-flag chips. The divider is
        ;; a category break for the eye only; origins and flags stay separate
        ;; filter groups with separate atoms and separate query semantics.
        (dom/div
          (dom/props {:class "cards-toolbar-row" :role "group" :aria-label "Filter by sync state"})
          (dom/span (dom/props {:class "filter-caption"}) (dom/text "Sync"))
          ;; An ACTIVE overlay-dependent chip stays clickable even without Anki:
          ;; turning such a filter ON needs the live overlay, turning it OFF
          ;; never does. Without this, the URL can seed ?sync=anki-changed while
          ;; Anki is down and strand the user on 0 cards with no way to clear it.
          (e/for-by first [facet origin-facets]
            (let [[v label overlay? source] facet
                  active? (contains? origins v)]
              (FilterChip label active? (and overlay? (not anki-ready?) (not active?)) source
                (fn [] (toggle-in! !origins v)))))
          (dom/span (dom/props {:class "filter-divider" :aria-hidden "true"}))
          (e/for-by first [facet flag-facets]
            (let [[v label] facet
                  active? (contains? flags v)]
              (FilterChip label active? (and (not anki-ready?) (not active?)) nil
                (fn [] (toggle-in! !flags v))))))))))

;; Fixed header table — sortable columns + select-all checkbox.
;; Takes COUNTS, not the id vector. Reading :filtered-ids in client scope — via
;; seq/every?, or merely by closing over it in the click handler — materializes
;; all N ids on the client on EVERY filter change (measured: 531 ms of
;; main-thread block at 2.9k cards). The ids now cross only while a select-all
;; click is in flight, inside the token frame below.
(e/defn CardsTableHeader [result filtered-count all-filtered-selected? !selected !sort-col !sort-dir]
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
                  ;; Disabled at 0 results: with nothing to select,
                  ;; all-filtered-selected? stays false, so the reactive set!
                  ;; below work-skips and the browser's own toggle would leave
                  ;; the box visually checked with an empty selection.
                  (dom/props {:type "checkbox"
                              :disabled (zero? filtered-count)
                              :style {:cursor "pointer"}})
                  (tooltip/Tooltip! "Select all filtered" :aria? true)
                  (set! (.-checked dom/node) (boolean all-filtered-selected?))
                  ;; Token-gated so the id vector is subscribed only while this
                  ;; frame is mounted — i.e. from click until the swap lands.
                  ;; The effect is a local atom swap, so `case` is only here to
                  ;; sequence it before spending the token.
                  (let [[t _err] (e/Token (dom/On "click" identity nil))]
                    (when t
                      (let [ids (e/server (vec (:filtered-ids result)))]
                        (case (if all-filtered-selected?
                                (swap! !selected #(reduce disj % ids))
                                (swap! !selected into ids))
                          (t)))))))
              (dom/th
                (dom/props {:style (merge th-style {:text-align "center" :cursor "pointer"})})
                (tooltip/Tooltip! "Sync direction (▲ push pending · ▼ pull pending · ▲▼ conflict · ○ unpushed) — sorts by DB state")
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

;; Virtual-scrolled body. The table is ALWAYS mounted — it renders for every
;; card-count, including 0, and the empty-state message is an absolutely
;; positioned sibling overlay rather than a branch that swaps the table out.
;; Mirrors SearchPage's SearchResultsTable: there, unmounting the table (and
;; with it Scroll-window + Tape + the per-row server flows) raced the cancelled
;; query's e/Offload teardown and crashed the WS with :diff-corruption.
;; At card-count 0 the Tape still emits its window; every nth returns nil and
;; the rows render nothing, so the differential stays mounted and empty.
;; position:relative is inline, not on the shared .tape-scroll class — this is
;; the only virtual-scroll site with an overlay, and 10 others share that class.
(e/defn CardsTableBody [cards-vec card-count row-height font-sz filters-active? scroll-reset-key
                        navigate! !editing-card !diff-card !selected selected
                        user-id anki-overlay]
  (e/client
    (dom/div
      (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"
                          :scrollbar-gutter "stable" :position "relative"}})
      (let [[offset limit] (Scroll-window row-height card-count dom/node {:overquery-factor 2 :reset-key scroll-reset-key})]
        (dom/props {:class "tape-scroll table-frame-body"
                    ;; C1c per-row transform positioning (see .tape-scroll in index.css):
                    ;; --count → table height (scroll range), --grid-cols → the row grid.
                    :style {:--count card-count :--row-height (str row-height "px")
                            :--grid-cols grid-cols}})
        (dom/table
          (dom/props {:class "cards-table-body"
                      ;; display:block + column template live in index.css (C1c).
                      :style {:width "100%" :font-size (str font-sz "px")}})
          (e/for [i (Tape offset limit)]
            (let [card (e/server (nth cards-vec i nil))]
              (when card
                (LibraryCardRow card navigate! !editing-card !diff-card !selected
                  selected user-id i anki-overlay)))))
        ;; Sibling overlay — never a branch that unmounts the table above.
        (when (zero? card-count)
          (dom/div
            (dom/props {:style {:position "absolute" :inset "0"
                                :display "flex" :align-items "center" :justify-content "center"
                                :text-align "center" :padding "24px 12px" :font-size "13px"
                                :color "var(--color-text-secondary)"
                                :pointer-events "none"}})
            (dom/text (if filters-active?
                        "No cards match the current filters."
                        "No cards yet. Generate flashcards from your documents to see them here."))))))))

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
(e/defn CardsTables [user-id navigate! result row-height font-sz filters-active? scroll-reset-key
                     anki-overlay selected !selected !sort-col !sort-dir
                     !editing-card !diff-card]
  (e/client
    (let [cards-vec (e/server (vec (:cards result)))
          card-count (e/server (or (:count result) 0))
          ;; Counts only. Both of these are ints on the wire; the id vector
          ;; itself never leaves the server on a filter change. `selected`
          ;; crosses server-ward instead — #{} in the common case, and unchanged
          ;; across filter changes, so work-skipping keeps it off the wire.
          filtered-count (e/server (count (:filtered-ids result)))
          selected-in-filter (e/server (count (filterv selected (:filtered-ids result))))
          all-filtered-selected? (and (pos? filtered-count) (= filtered-count selected-in-filter))]

      (CardsTableHeader result filtered-count all-filtered-selected? !selected !sort-col !sort-dir)

      (CardsTableBody cards-vec card-count row-height font-sz filters-active? scroll-reset-key
        navigate! !editing-card !diff-card !selected selected
        user-id anki-overlay))))

;; Phase 3: selection + diff modal + bulk action state, plus the tables.
;; Owns the selection/bulk atoms; mounted on the query success branch.
(e/defn CardsSelectionRegion [user-id navigate! result row-height font-sz
                              filters-active? filter-pending? scroll-reset-key
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

      ;; Counts and the bulk-action bar share ONE line as two flex children, so
      ;; a narrow viewport wraps between the clusters, never mid-button-group.
      ;; The action bar is always visible; its buttons disable at 0 selection.
      (dom/div
        (dom/props {:class "cards-summary"})
        (CardsCountSummary result ov-status filter-pending?)
        (BulkActionBar anki-overlay ov-status (some? bulk-phase)
          !selected !include-conflicts !action-result
          !bulk-args !bulk-skips !bulk-phase !confirm-bulk-delete))

      (CardsTables user-id navigate! result row-height font-sz filters-active? scroll-reset-key
        anki-overlay selected !selected !sort-col !sort-dir
        !editing-card !diff-card))))

;; Everything downstream of the query result: edit modal, error branch,
;; selection region. Takes `result` from LibraryCardsView's gate — the query
;; runs there so the whole view mounts in one batch (see CardsFilterBar).
;; anki-overlay + ov-status are computed upstream in LibraryCardsView (the
;; overlay must exist before the filtered query so its flag id-sets can feed
;; opts as a plain value) and threaded through here to the rows + bulk bar.
(e/defn CardsResultRegion [user-id navigate! result anki-overlay ov-status
                           filters-active? filter-pending? scroll-reset-key
                           !sort-col !sort-dir !editing-card]
  (e/client
    (let [success? (e/server (:success result))
          font-sz (or (e/server (settings/get-card-font-size user-id)) 13)
          ;; Fixed row height (see content_card_table): 12px padding + 28px line + 1px
          ;; border. font-sz sizes the text within the row, not the row itself.
          row-height 41]

      (when (e/watch !editing-card)
        (EditCardModal !editing-card user-id))

      (if (= false success?)
        (dom/div
          (dom/props {:style {:color "var(--color-danger-text)" :font-size "13px" :padding "8px 12px"}})
          (dom/text "Error loading cards: " (e/server (:error result))))
        (CardsSelectionRegion user-id navigate! result row-height font-sz
          filters-active? filter-pending? scroll-reset-key
          anki-overlay ov-status !sort-col !sort-dir !editing-card)))))

(e/defn LibraryCardsView [user-id navigate! refresh]
  (e/client
    (let [;; Seed every filter from the query string, once. Read here and not
          ;; in CardsFilterBar so one place owns the whole filter set.
          initial (query->filters (util/query-params))
          ;; Two text signals, matching search_page.cljc: !text is what the
          ;; user has typed RIGHT NOW and drives the URL on every keystroke;
          ;; !text-debounced lags 300 ms and drives the query, the reset-key
          ;; and the empty-state message, so those all agree with the rows on
          ;; screen. Nothing sets .value from either — the input is
          ;; uncontrolled apart from its one-time seed.
          !text (atom (:text initial)) text (e/watch !text)
          !text-debounced (atom (:text initial)) text-debounced (e/watch !text-debounced)
          ;; Holds the in-flight setTimeout id for the text-filter debounce.
          ;; Declared here, not inside CardsFilterBar, so it survives any
          ;; re-evaluation of the bar's let and cannot drop a pending timer.
          !text-timer (atom nil)
          !kinds (atom (:kinds initial)) kinds (e/watch !kinds)
          !origins (atom (:origins initial)) origins (e/watch !origins)
          !flags (atom (:flags initial)) flags (e/watch !flags)
          !sort-col (atom (:sort-col initial)) sort-col (e/watch !sort-col)
          !sort-dir (atom (:sort-dir initial)) sort-dir (e/watch !sort-dir)
          !editing-card (atom nil)
          !ov-status (atom :idle) ov-status (e/watch !ov-status)
          !ov-payload (atom nil)
          !check-tick (atom 0)
          ;; Overlay computed UPSTREAM of the filtered query: its per-card flag
          ;; map feeds the rows (icons) AND, split into id-sets, the query opts.
          ;; The manifest is filter-independent (own query), so this is an
          ;; acyclic value flow — no atom bridge, no reactive write.
          manifest (e/server
                     (let [rev (+ refresh
                                 (e/watch (us/get-atom user-id :card-mutations))
                                 (e/watch (us/get-atom user-id :sync-mutations)))]
                       (vec (e/Offload #(pushed-manifest* rev user-id)))))
          anki-overlay (AnkiOverlay user-id manifest !ov-status !ov-payload !check-tick)
          ;; Overlay id-sets enter opts ONLY while a facet that needs them is
          ;; active — otherwise the query stays a pure function of the DB filters
          ;; and overlay changes trigger no re-query.
          overlay-active? (or (contains? origins "anki-changed")
                            (contains? origins "both")
                            (seq flags))
          overlay-ids (if overlay-active?
                        (overlay->filter-ids anki-overlay)
                        {:anki-changed #{} :marked #{} :suspended #{}})
          opts {:text text-debounced :kinds kinds :origins origins :flags flags
                :overlay-ids overlay-ids :sort-col sort-col :sort-dir sort-dir}
          ;; URL sync — a side effect during binding evaluation, referenced by
          ;; the (when url-synced nil) below or Electric elides it. Driven by
          ;; `text` (instant), so the address bar tracks every keystroke while
          ;; the query waits for the debounce. replaceState only: no history
          ;; entries, hence no popstate listener anywhere in this view.
          url-synced (do (util/set-query-params!
                           (filters->query text kinds origins flags sort-col sort-dir))
                       true)
          ;; Identity of the filter set the client is currently ASKING for.
          ;; e/Offload is Offload-LATCH, whose contract is that "intermediate
          ;; pending states are not seen" — it buffers the previous result
          ;; across a re-query, so there is no nil/pending value to hang a
          ;; spinner on. Echoing this hash back through the result (below) and
          ;; comparing requested-vs-delivered is the only in-band way to see
          ;; the gap without switching to Offload-reset, which would return
          ;; (e/amb) and blank the one-batch gate — the teardown WP2 removed.
          opts-hash (hash opts)
          ;; Debounced text, so the empty-state message matches the rows.
          filters-active? (or (not (str/blank? text-debounced)) (seq kinds) (seq origins) (seq flags))
          ;; Scroll resets to top only when the user navigates the list (search /
          ;; filter / sort), NOT on row-count churn from delete. Mirrors
          ;; knowledge-tree/DocumentTreeView's reset-key. Threaded down to the
          ;; CardsTableBody Scroll-window call.
          ;; Debounced text: keying on the instant value would snap the list to
          ;; the top on every keystroke, before the matching query had run.
          scroll-reset-key [text-debounced kinds origins flags sort-col sort-dir]
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
                     (e/Offload #(assoc (query-user-cards* rev user-id opts)
                                   :opts-hash opts-hash))))
          success? (e/server (:success result))
          ;; True while the rows on screen belong to a PREVIOUS filter set.
          ;; Deliberately blind to :refresh / :card-mutations / :sync-mutations
          ;; re-queries — those leave opts-hash equal, so a background sync
          ;; never flashes the indicator. Only a filter/sort change does.
          filter-pending? (not= opts-hash (e/server (:opts-hash result)))]
      ;; Reference the URL side effect so Electric evaluates its binding.
      (when url-synced nil)
      ;; One-batch gate: nothing mounts until the query result exists.
      ;; A solo-mounted sibling (e.g. the filter bar during the in-flight
      ;; window) is removed by the runtime when the result's children
      ;; arrive — Electric v3 mount bug, triggered by the documents→cards
      ;; branch switch. e/Offload latches the previous result across
      ;; re-queries, so the gate only blanks on first mount, not on
      ;; filter changes.
      (when (some? success?)
        (CardsFilterBar navigate! ov-status (:text initial) !text !text-debounced !text-timer
          !kinds !origins !flags !check-tick)
        (CardsResultRegion user-id navigate! result anki-overlay ov-status
          filters-active? filter-pending? scroll-reset-key
          !sort-col !sort-dir !editing-card)))))
