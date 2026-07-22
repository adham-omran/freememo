(ns freememo.content-card-table
  "Unified card table component — virtual-scrolled card list with edit/delete.
   Queries by topic-id (page or extract topic)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [freememo.card-components :refer [CardRow PendingCardRow rtl-text?]]
   [freememo.card-modals :refer [EditCardModal]]
   [freememo.occlusion-modal :refer [OcclusionModal]]
   [freememo.score-toolbar :refer [ScoreEditLoader]]
   #?(:clj [freememo.db :as db])
   #?(:clj [taoensso.telemere :as tel])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.optimistic :as opt])))

;; Query wrapper: takes refresh arg to create Electric reactive dependency
;; For root topics (no parent), fetches ALL descendant cards via get-all-flashcards.
;; For child topics (extracts, pages), fetches cards for that specific topic.
(defn get-cards-for-topic* [_refresh topic-id]
  #?(:clj (try
            (if-not topic-id
              {:success true :cards []}
              (let [topic (db/get-topic topic-id)
                    is-root? (nil? (:topics/parent_id topic))
                    cards (if is-root?
                            (db/get-all-flashcards topic-id)
                            (db/get-flashcards topic-id))]
                {:success true :cards cards}))
            (catch Exception e
              (tel/error! {:id ::get-cards-for-topic :data {:topic-id topic-id}} e)
              {:success false :error (.getMessage e)}))
     :cljs nil))

(e/defn ContentCardTable []
  (e/client
    (let [topic-id dctx/page-topic-id card-font-size dctx/card-font-size user-id dctx/user-id
          refresh dctx/card-refresh
          cards-result (e/server (get-cards-for-topic* refresh topic-id))
          !editing-card (atom nil)
          editing-card (e/watch !editing-card)]

      (when editing-card
        (case (:kind editing-card)
          ;; CardRow puts {:kind "occlusion" :mode :edit :group-id N} in the
          ;; atom — the occlusion modal treats it as its edit request.
          "occlusion" (OcclusionModal !editing-card user-id)
          ;; Score rows load their group into the in-view score editor
          ;; (waveform region + rect pages) instead of opening a modal.
          "score" (when dctx/is-score?
                    (ScoreEditLoader !editing-card))
          (EditCardModal !editing-card user-id)))

      (if (:success cards-result)
        (let [cards-vec (e/server (vec (:cards cards-result)))
              card-count (e/server (count cards-vec))
              unsynced-count (e/server (count (filter #(or (nil? (:flashcards/anki_synced_at %))
                                                         (and (:flashcards/updated_at %)
                                                           (pos? (compare (str (:flashcards/updated_at %))
                                                                   (str (:flashcards/anki_synced_at %))))))
                                                (:cards cards-result))))
              font-sz (or card-font-size 13)
              ;; Fixed row height: 12px padding + 28px content line + 1px border.
              ;; font-sz drives the text line within this budget, not the row height
              ;; (adding it left an empty band below every row).
              row-height 41
              ;; Optimistic add-card overlay (freememo.optimistic :pending-cards).
              pending-map (e/server (e/watch (us/get-atom user-id :pending-cards)))
              present-ids (e/server (set (map :flashcards/id cards-vec)))
              overlay-entries (e/server (opt/visible-pending-cards pending-map topic-id present-ids))
              landed-tempids (e/server (opt/landed-pending-tempids pending-map topic-id present-ids))]
          ;; Auto-forget confirmed overlay entries once their cards have landed
          ;; in the refetched list (keeps :pending-cards from accumulating).
          (e/for [tid (e/server (e/diff-by identity landed-tempids))]
            (let [[t _] (e/Token tid)]
              (when t (case (e/server (opt/forget-pending-card! user-id tid)) (t)))))
          (dom/div
            (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0" :scrollbar-gutter "stable"}})
            (when (pos? card-count)
              (dom/div
                (dom/props {:style {:padding "4px 12px" :font-size "12px" :color "var(--color-text-hint)"}})
                (dom/text (str card-count " card" (when (not= card-count 1) "s")
                            (when (pos? unsynced-count) (str " (" unsynced-count " unsynced)"))))))
            (let [[offset limit] (Scroll-window row-height card-count dom/node {:overquery-factor 2})
                  first-card (e/server (first cards-vec))
                  rtl? (e/server (rtl-text? (or (:flashcards/question first-card)
                                              (:flashcards/cloze first-card))))]
              (dom/props {:class "tape-scroll"
                          ;; C1c per-row transform positioning (see .tape-scroll in index.css):
                          ;; --count → table height (scroll range), --grid-cols → the row grid.
                          :style {:--count card-count :--row-height (str row-height "px")
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
                ;; Optimistic pending/error rows above the real cards.
                (e/for [entry (e/server (e/diff-by :tempid overlay-entries))]
                  (PendingCardRow entry user-id))
                (if (pos? card-count)
                  (e/for [i (Tape offset limit)]
                    (let [card (e/server (nth cards-vec i nil))]
                      (when card
                        (CardRow card !editing-card user-id i))))
                  (when (empty? overlay-entries)
                    (dom/tr
                      ;; Opt out of the fixed row-height so the message isn't clipped (C1c).
                      (dom/props {:style {:height "auto"}})
                      (dom/td
                        (dom/props {:style {:grid-column "1 / -1" :text-align "center" :padding "24px 12px"
                                            :color "var(--color-text-hint)" :font-size "13px"}})
                        (dom/text "No cards yet. Use the Generate button above to create flashcards from this content.")))))))))
        (dom/div
          (dom/props {:style {:color "var(--color-danger-text)" :font-size "13px" :padding "8px 12px"}})
          (dom/text "Error loading cards: " (:error cards-result)))))))
