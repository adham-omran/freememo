(ns freememo.content-card-table
  "Unified card table component — virtual-scrolled card list with edit/delete.
   Queries by topic-id (page or extract topic)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [freememo.card-components :refer [CardRow PendingCardRow rtl-text? unsynced?]]
   [freememo.card-rows :as rows]
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

;; Query wrapper: takes refresh arg to create Electric reactive dependency
;; For root topics (no parent), fetches ALL descendant cards via get-all-flashcards.
;; For child topics (extracts, pages), fetches cards for that specific topic.
(defn get-cards-for-topic* [_refresh topic-id user-id]
  #?(:clj (try
            (if-not topic-id
              {:success true :cards [] :card-revision @(us/get-atom user-id :card-mutations)}
              (let [card-revision @(us/get-atom user-id :card-mutations)
                    topic (db/get-topic topic-id)
                    is-root? (nil? (:topics/parent_id topic))
                    cards (if is-root?
                            (db/get-all-flashcards topic-id)
                            (db/get-flashcards topic-id))]
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
          refresh dctx/card-refresh
          cards-result (e/server (e/Offload #(get-cards-for-topic* refresh topic-id user-id)))
          !editing-card (atom nil)
          editing-card (e/watch !editing-card)
          !anki-status (atom :idle)
          !anki-payload (atom nil)
          !anki-check-tick (atom 0)
          !review-cursors (atom {})
          !review-target (atom nil)
          !review-message (atom nil)]

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
        (let [cards-vec (e/server (vec (:cards cards-result)))
              row-member-ids (e/server (mapv :member-ids cards-vec))
              visible-indexes (opt/VisibleCardIndexes row-member-ids (e/server (:card-revision cards-result)))
              row-count (count visible-indexes)
              card-count (e/server (rows/card-count cards-vec))
              manifest (e/server (review/pushed-manifest cards-vec))
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
              unsynced-count (e/server (count (filter unsynced?
                                                (mapcat :members (:cards cards-result)))))
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
              present-ids (e/server (set (mapcat :member-ids cards-vec)))
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
                                         " cards in this content."))))))]
              (e/for [[command-id state] (e/diff-by first review-command-states)]
                (e/for [request (bus/Requests command-id)]
                  (let [[t _] (e/Token (:invocation-id request))]
                    (when t
                      (case (find-next! state)
                        (case (bus/consume! (:invocation-id request))
                          (t))))))))
            (when (pos? row-count)
              (dom/div
                (dom/props {:style {:padding "4px 12px" :font-size "12px" :color "var(--color-text-hint)"}
                            :aria-live "polite"})
                ;; Rows and cards, stated separately whenever a group makes
                ;; them differ — the same dual idiom /library/cards uses.
                (dom/text (str row-count " row" (when (not= row-count 1) "s")
                            (when (not= row-count card-count)
                              (str " · " card-count " cards"))
                            (when (pos? unsynced-count) (str " (" unsynced-count " unsynced)"))))
                (when review-message
                  (dom/span
                    (dom/props {:class "card-review-message"})
                    (dom/text " · " review-message)))))
            (let [[offset limit] (Scroll-window row-height total-row-count dom/node {:overquery-factor 2})
                  first-card (e/server (first cards-vec))
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
                    (when (and card (= source-index (first card-result)))
                      (CardRow card !editing-card user-id i anki-overlay (= visible-index review-target)))))
                (when (zero? total-row-count)
                  (dom/tr
                    (dom/props {:style {:height "auto"}})
                    (dom/td
                      (dom/props {:style {:grid-column "1 / -1" :text-align "center" :padding "24px 12px"
                                          :color "var(--color-text-hint)" :font-size "13px"}})
                      (dom/text "No cards yet. Use the Generate button above to create flashcards from this content."))))))))
        (dom/div
          (dom/props {:style {:color "var(--color-danger-text)" :font-size "13px" :padding "8px 12px"}})
          (dom/text "Error loading cards: " (e/server (:error cards-result))))))))
