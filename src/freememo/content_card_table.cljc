(ns freememo.content-card-table
  "Unified card table component — virtual-scrolled card list with edit/delete.
   Queries by topic-id: a root lists every card in the document, and any other
   topic (page or extract) lists its own cards and its descendants' cards.
   A filter bar narrows the rows by text, kind and sync state
   (plans/card-table-filter.md)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [hyperfiddle.electric-scroll0 :refer [Tape]]
   [freememo.scroll :refer [Scroll-window]]
   [contrib.data :refer [clamp-left]]
   [freememo.card-components :refer [CardRow PendingCardRow rtl-text? unsynced?]]
   [freememo.card-rows :as rows]
   [freememo.card-filter :as card-filter]
   [freememo.forms-ext :as forms-ext]
   [freememo.util :as util]
   [freememo.anki-review :as review]
   [freememo.anki-review-overlay :as review-overlay]
   [freememo.command-bus :as bus]
   [freememo.card-modals :refer [EditCardModal CopyCardModal]]
   [freememo.occlusion-modal :refer [OcclusionModal]]
   [freememo.score-toolbar :refer [ScoreEditLoader]]
   [freememo.client-state :as cs]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [taoensso.telemere :as tel])
   [freememo.optimistic :as opt]))

(def review-command-states
  [[:find-next-marked-card :marked]
   [:find-next-suspended-card :suspended]
   [:find-next-remarked-card :remarked]])

(def review-state-label
  {:marked "Marked" :suspended "Suspended" :remarked "Remarked"})

(defn scroll-to-card-row! [node top]
  #?(:cljs (.scrollTo node #js {:top top :behavior "smooth"})
     :clj nil))

(defn row-count-label
  "The header's count text. `rows` and `total` are display rows (shown, and in
   the whole scope); `cards` and `unsynced` count the shown rows' members.
   Post: \"N of M rows\" while `filtering?`, else \"N rows\"; then \" · C cards\"
         when C differs from N, then \" (U unsynced)\" when U > 0."
  [rows total cards unsynced filtering?]
  (str (if filtering? (str rows " of " total) rows)
    " row" (when (not= (if filtering? total rows) 1) "s")
    ;; Rows and cards, stated separately whenever a group makes them differ —
    ;; the same dual idiom /library/cards uses.
    (when (not= rows cards) (str " · " cards " cards"))
    (when (pos? unsynced) (str " (" unsynced " unsynced)"))))

(defn focus-filter-input!
  "Focus the filter input and select its text, two frames from now: the palette
   that ran :filter-cards restores focus to its opener as it unmounts, and that
   restore has to land first (the nav_rail.cljc open-with-focus! shape)."
  [node]
  #?(:cljs (js/requestAnimationFrame
             (fn [] (js/requestAnimationFrame (fn [] (.focus node) (.select node)))))
     :clj nil))

(defn reset-filters!
  "Clear all three filters at once, cancelling a pending debounced text write
   so it cannot land after the clear."
  [!timer !filter-text !filter-kind !filter-unsynced?]
  #?(:cljs (do (some-> @!timer js/clearTimeout)
               (reset! !filter-text "")
               (reset! !filter-kind "")
               (reset! !filter-unsynced? false))
     :clj nil))

(defn on-filter-keydown!
  "Escape clears a non-empty filter input at once, and blurs an empty one. The
   key stops here, so no enclosing Escape handler also acts on it."
  [e !timer !filter-text]
  #?(:cljs (when (= "Escape" (.-key e))
             (.stopPropagation e)
             (.preventDefault e)
             (let [input (.-target e)]
               (if (= "" (.-value input))
                 (.blur input)
                 (do (set! (.-value input) "")
                     (some-> @!timer js/clearTimeout)
                     (reset! !filter-text "")))))
     :clj nil))

(e/defn CardFilterBar
  "Text, kind and Unsynced filters for the card table.
   Pre:  mounted in a frame keyed on the topic, so a topic change unmounts it.
         The atoms belong to ContentCardTable; `kind` and `unsynced?` are their
         watched values.
   Post: !filter-text holds the input's text 150 ms after the last keystroke.
         Unmount clears all three filters, so every topic starts unfiltered.
   The input is uncontrolled. Clearing !filter-text does not clear the box, and
   that is why a topic change remounts this component instead of resetting it."
  [!filter-text !filter-kind !filter-unsynced? kind unsynced?]
  (e/client
    (let [!timer (atom nil)]
      (e/on-unmount #(reset-filters! !timer !filter-text !filter-kind !filter-unsynced?))
      (dom/input
        (dom/props {:type "text" :class "input card-filter-input"
                    :placeholder "Filter cards…" :aria-label "Filter cards"})
        ;; Keyed by the node: the old and new bars of a topic change are both
        ;; mounted for a moment, in an order nothing promises.
        (let [node dom/node]
          (bus/publish-invoker! :filter-cards node #(focus-filter-input! node))
          (e/on-unmount #(bus/retract-invoker! :filter-cards node)))
        (dom/On "input"
          (fn [e] (util/debounce! !timer !filter-text (-> e .-target .-value) 150))
          nil)
        (dom/On "keydown" (fn [e] (on-filter-keydown! e !timer !filter-text)) nil))
      (e/for [[t {picked :kind}] (forms-ext/Select! :kind kind card-filter/kind-options
                                   :class "input card-filter-kind"
                                   :aria-label "Filter by card kind")]
        (case (reset! !filter-kind picked) (t)))
      (dom/button
        (dom/props {:type "button"
                    :class (str "filter-chip" (when unsynced? " filter-chip-active"))
                    :aria-pressed (boolean unsynced?)})
        (dom/text "Unsynced")
        (dom/On "click" (fn [_] (swap! !filter-unsynced? not)) nil)))))

;; Query wrapper: takes refresh arg to create Electric reactive dependency
;; For root topics (no parent), fetches ALL descendant cards via get-all-flashcards.
;; For child topics (pages, extracts), fetches the cards of the topic AND of
;; every descendant. A PDF page's cards are mostly on the extracts made from
;; it, so a page that listed only its own cards read "No cards yet" beside a
;; child extract that held four.
(defn get-cards-for-topic* [_refresh topic-id user-id]
  #?(:clj (try
            (if-not topic-id
              {:success true :cards [] :card-revision @(us/get-atom user-id :card-mutations)}
              (let [card-revision @(us/get-atom user-id :card-mutations)
                    topic (db/get-topic topic-id)
                    is-root? (nil? (:topics/parent_id topic))
                    cards (if is-root?
                            (db/get-all-flashcards topic-id)
                            (db/get-flashcards-for-subtree topic-id))]
                ;; One row per authoring group, matching /library/cards — an
                ;; occlusion group is ONE row here too (freememo.card-rows).
                {:success true :cards (rows/collapse-groups cards) :card-revision card-revision}))
            (catch Exception e
              (tel/error! {:id ::get-cards-for-topic :data {:topic-id topic-id}} e)
              {:success false :error (.getMessage e)}))
     :cljs nil))

(e/defn ContentCardTable []
  (e/client
    (let [topic-id dctx/page-topic-id card-font-size dctx/card-font-size user-id dctx/user-id
          ;; The refresh sum is built on the server, where it is read. A client
          ;; sum was a client-owned value that the server read back, here and in
          ;; ContentToolbar, and that round trip exposes a value to Electric's
          ;; :diff-corruption race. topic-id and user-id still make the trip,
          ;; and DocumentRoot's anchor holds them.
          ;; See bugs/20260926-electric-input-diff-corruption/README.md.
          cards-result (e/server
                         (let [rev (+ (e/watch (us/get-atom user-id :refresh))
                                     (e/watch (us/get-atom user-id :sync-mutations))
                                     (e/watch (us/get-atom user-id :card-mutations)))]
                           (e/Offload #(get-cards-for-topic* rev topic-id user-id))))
          !editing-card (atom nil)
          editing-card (e/watch !editing-card)
          !anki-status (atom :idle)
          !anki-payload (atom nil)
          !anki-check-tick (atom 0)
          !review-cursors (atom {})
          !review-target (atom nil)
          !review-message (atom nil)
          ;; The filters. !filter-text is the DEBOUNCED text; the input owns
          ;; what is typed. One map built here is what crosses to the server.
          !filter-text (atom "")
          !filter-kind (atom "")
          !filter-unsynced? (atom false)
          filter-kind (e/watch !filter-kind)
          filter-unsynced? (e/watch !filter-unsynced?)
          filters {:text (e/watch !filter-text)
                   :kind (not-empty filter-kind)
                   :unsynced? filter-unsynced?}
          filtering? (card-filter/active? filters)
          filter-query (card-filter/query (:text filters))]

      (when editing-card
        ;; :mode is checked before :kind because a copy request carries BOTH,
        ;; and its kind must not route it back to the editor it came from.
        (if (= :copy (:mode editing-card))
          (CopyCardModal !editing-card user-id)
          (case (:kind editing-card)
            ;; CardRow puts {:kind "occlusion" :mode :edit :group-id N} in the
            ;; atom — the occlusion modal treats it as its edit request.
            "occlusion" (OcclusionModal !editing-card user-id)
            ;; Score rows load their group into the in-view score editor
            ;; (waveform region + rect pages) instead of opening a modal.
            "score" (when dctx/is-score?
                      (ScoreEditLoader user-id !editing-card))
            (EditCardModal !editing-card user-id))))

      ;; Both branch reads are server-sited. `cards-result` holds the whole card
      ;; vector; reading :success (or :error) from client scope would pull that
      ;; vector across before any windowing below it runs. Same shape as
      ;; library_cards/CardsResultRegion.
      (if (e/server (:success cards-result))
        ;; `all-rows` is the scope; `cards-vec` is what the filters keep. Each
        ;; value below reads the one it means. present-ids, manifest and rtl?
        ;; read `all-rows`: a pending row whose card is filtered out must still
        ;; clear, and AnkiOverlay calls AnkiConnect whenever its manifest
        ;; changes, which a filtered manifest would do on every keystroke.
        (let [all-rows (e/server (vec (:cards cards-result)))
              row-texts (e/server (mapv card-filter/row-search-text all-rows))
              cards-vec (e/server (card-filter/filter-rows filters all-rows row-texts))
              total-rows (e/server (count all-rows))
              row-member-ids (e/server (mapv :member-ids cards-vec))
              visible-indexes (opt/VisibleCardIndexes row-member-ids (e/server (:card-revision cards-result)))
              row-count (count visible-indexes)
              card-count (e/server (rows/card-count cards-vec))
              manifest (e/server (review/pushed-manifest all-rows))
              anki-overlay (review-overlay/AnkiOverlay
                             user-id manifest !anki-status !anki-payload !anki-check-tick)
              review-target (e/watch !review-target)
              review-message (e/watch !review-message)
              ;; Counted over MEMBERS, not rows: this is an Anki-note count and
              ;; a group row holds one note per member.
              ;; `card-components/unsynced?` is the same rule `db/unsynced-sql`
              ;; runs, so this label and every SQL count answer one question. It
              ;; used to be a timestamp test while the reconcile ran the digest,
              ;; and the two disagreed on exactly the edits that move one and not
              ;; the other — which is how "13 rows (1 unsynced)" sat next to
              ;; "already in sync" with nothing able to clear it.
              unsynced-count (e/server (count (filter unsynced? (mapcat :members cards-vec))))
              font-sz (or card-font-size 13)
              ;; Fixed row height: 12px padding + 28px content line + 1px border.
              ;; font-sz drives the text line within this budget, not the row height
              ;; (adding it left an empty band below every row).
              row-height 41
              ;; Optimistic add-card overlay (freememo.optimistic :pending-cards,
              ;; per-session client state). present-ids crosses from the server
              ;; card query; the overlay merge is client-side.
              pending-map (e/watch (cs/get-atom :pending-cards))
              ;; Union of MEMBER ids, not row ids: an occlusion add's entry
              ;; holds one real-id per mask group, and landed-pending-tempids
              ;; only forgets it once every one of them is present. Row ids
              ;; alone would leave the pending row up forever.
              present-ids (e/server (set (mapcat :member-ids all-rows)))
              overlay-entries (opt/visible-pending-cards pending-map topic-id present-ids)
              landed-tempids (opt/landed-pending-tempids pending-map topic-id present-ids)
              pending-count (count overlay-entries)
              total-row-count (+ pending-count row-count)]
          ;; Auto-forget confirmed overlay entries once their cards have landed
          ;; in the refetched list (keeps :pending-cards from accumulating). The
          ;; forget mutates the overlay (unmounting this branch), so it fires from
          ;; e/on-unmount when (t) spends the token.
          (e/for [tid (e/diff-by identity landed-tempids)]
            (let [[t _] (e/Token tid)]
              (when t
                (e/on-unmount #(opt/forget-pending! tid))
                (t))))
          ;; The header sits OUTSIDE the scroll box, so the filter stays in
          ;; view and row 0 starts at scrollTop 0. It shows while a row shows
          ;; or a filter is on, so a filter that matches nothing can still be
          ;; cleared.
          ;; ALWAYS MOUNTED, hidden by a prop. A `when` on that test remounted
          ;; the header each time the test re-emitted — every filter change
          ;; does — and the remount unmounted CardFilterBar, whose unmount
          ;; clears the filters: one keystroke filtered for a frame and then
          ;; undid itself (CLAUDE.md, "A `case` Remounts Its Branch").
          ;; `hidden` is in its own props map: it derives from a server read,
          ;; and an absent map nulls every prop in it (CLAUDE.md).
          (dom/div
            (dom/props {:class "card-table-header"})
            (dom/props {:hidden (not (or filtering? (pos? row-count)))})
            (dom/div
              (dom/props {:class "card-table-count" :aria-live "polite"})
              (dom/text (row-count-label row-count total-rows card-count unsynced-count filtering?))
              (when review-message
                (dom/span
                  (dom/props {:class "card-review-message"})
                  (dom/text " · " review-message))))
            ;; Its own element: a keyed loop never shares a parent's child
            ;; slots (CLAUDE.md, "An e/for Gets an Element of Its Own").
            (dom/div
              (dom/props {:class "card-filter-bar"})
              (e/for-by identity [_topic [topic-id]]
                (CardFilterBar !filter-text !filter-kind !filter-unsynced?
                  filter-kind filter-unsynced?))))
          (dom/div
            (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0" :scrollbar-gutter "stable"}})
            (let [scroll-node dom/node
                  find-next! (fn [state]
                               (let [matches (review/matching-row-indexes
                                               (mapv #(nth row-member-ids %) visible-indexes) anki-overlay state)
                                     target (review/next-row-index
                                              matches (get @!review-cursors state -1))]
                                 (if (some? target)
                                   (let [position (inc (count (take-while #(not= % target) matches)))]
                                     (swap! !review-cursors assoc state target)
                                     (reset! !review-target target)
                                     (reset! !review-message
                                       (str (review-state-label state) " card " position
                                         " of " (count matches)))
                                     (scroll-to-card-row! scroll-node (* (+ pending-count target) row-height)))
                                   (do
                                     (reset! !review-target nil)
                                     (reset! !review-message
                                       (str "No " (review-state-label state)
                                         (if filtering?
                                           " cards match the filters."
                                           " cards in this content.")))))))]
              (e/for [[command-id state] (e/diff-by first review-command-states)]
                (e/for [request (bus/Requests command-id)]
                  (let [[t _] (e/Token (:invocation-id request))]
                    (when t
                      (case (find-next! state)
                        (case (bus/consume! (:invocation-id request))
                          (t))))))))
            (let [[offset limit] (Scroll-window row-height total-row-count dom/node
                                   {:overquery-factor 2
                                    :reset-key [topic-id (:text filters) filter-kind filter-unsynced?]})
                  first-card (e/server (first all-rows))
                  rtl? (e/server (rtl-text? (or (:flashcards/question first-card)
                                              (:flashcards/cloze first-card))))]
              (dom/props {:class "tape-scroll"
                          ;; C1c per-row transform positioning (see .tape-scroll in index.css):
                          ;; --count → table height (scroll range), --grid-cols → the row grid.
                          :style {:--count total-row-count :--row-height (str row-height "px")
                                  :--grid-cols "24px 1fr 1fr 40px"}})
              (dom/table
                (dom/props {:dir (if rtl? "rtl" "ltr")
                            :class "card-table"
                            ;; display:block + column template live in index.css (C1c).
                            :style {:width "100%"
                                    :border-collapse "separate"
                                    :border-spacing "0"
                                    :table-layout "fixed"
                                    :font-size (str font-sz "px")
                                    :direction (if rtl? "rtl" "ltr")}})
                ;; Pending and saved rows share one index space.
                (e/for [[i entry] (e/diff-by (comp :tempid second) (map-indexed vector overlay-entries))]
                  (PendingCardRow entry user-id i))
                (e/for [i (Tape offset limit)]
                  (let [visible-index (- i pending-count)
                        source-index (nth visible-indexes visible-index nil)
                        card-result (e/server [source-index (when (some? source-index) (nth cards-vec source-index nil))])
                        card (second card-result)]
                    ;; The anchor for `card`, which the client computes and
                    ;; CardRow reads back on the server. It lives as long as the
                    ;; row frame, as DocumentRoot's anchor lives for the ids.
                    (e/drain (e/server (some? card)))
                    (when (and card (= source-index (first card-result)))
                      (CardRow card !editing-card user-id i anki-overlay (= visible-index review-target)
                        filter-query))))
                ;; Not .card-table-empty when the filters hid every row: that
                ;; class collapses the panel on a phone, and the filter bar
                ;; above must stay usable. `filtering?`, not `total-rows`: an
                ;; optimistic delete of the last card empties the rows before
                ;; the query says the scope is empty.
                (when (zero? total-row-count)
                  (dom/tr
                    (dom/props {:class (if filtering? "card-table-no-match" "card-table-empty")
                                :style {:height "auto"}})
                    (dom/td
                      (dom/props {:style {:grid-column "1 / -1" :text-align "center" :padding "24px 12px"
                                          :color "var(--color-text-hint)" :font-size "13px"}})
                      (dom/text (if filtering?
                                  "No cards match the filters."
                                  "No cards yet. Use the Generate button above to create flashcards from this content.")))))))))
        (dom/div
          (dom/props {:style {:color "var(--color-danger-text)" :font-size "13px" :padding "8px 12px"}})
          (dom/text "Error loading cards: " (e/server (:error cards-result))))))))
