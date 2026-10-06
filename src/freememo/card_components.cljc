(ns freememo.card-components
  "Shared card display components — used by TopicPage's BottomPanel."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.a11y :as a11y]
   [clojure.string :as str]
   [freememo.logging :as log]
   #?(:clj [freememo.toasts :as toasts])
   [freememo.anki-review :as review]
   [freememo.occlusion-ordinals :as ord]
   [freememo.audio-clip :as clip]
   [freememo.math :as math]
   #?(:clj [freememo.cards :as cards])
   #?(:cljs [freememo.anki-sync-helpers :refer [anki-call!]])
   #?(:cljs [freememo.card-filter-highlight :as filter-highlight])
   [freememo.optimistic :as opt]))

(defn replace-img-with-chip
  "Replace each <img ...> tag with an inline `[image]` chip so card rows stay compact."
  [html]
  (when html
    (str/replace html #"(?i)<img\b[^>]*>"
      "<span class=\"card-row-img-chip\">[image]</span>")))

(defn truncate-html-for-row
  "Trim HTML to ~max-chars visible text. Combined with single-line clamp CSS,
   broken tail markup at the truncation point is invisible."
  [html max-chars]
  (when html
    (let [visible (str/replace html #"<[^>]+>" "")]
      (if (<= (count visible) max-chars)
        html
        (str (subs html 0 (min (count html) max-chars)) "…")))))

(defn replace-audio-with-chip
  "Replace each audio clip with an inline `[audio]` chip, as images are.

   A row is cut at a CHARACTER bound by `truncate-html-for-row`, so a clip left
   in place can be split into a broken `<audio` tag — and an unclosed media
   element is not the harmless tail the truncation comment describes. The chip
   is also what keeps a 36px row from mounting a player per cell.

   `freememo.audio-clip` owns the element's shape; this fn only names the chip."
  [html]
  (clip/replace-clips html "<span class=\"card-row-audio-chip\">[audio]</span>"))

(defn card-row-html
  "Prepare card-field HTML for inline rendering inside a CardRow cell."
  [html]
  (-> (or html "")
    replace-img-with-chip
    replace-audio-with-chip
    (truncate-html-for-row 200)))

(defn set-inner-html!
  "Write html into node.innerHTML and typeset its math. CLJ no-op.

   Kept as the name every card display site already calls; the behaviour lives in
   `freememo.math/set-html!` so non-card sites (search snippets, OCR previews) can
   share it without depending on this ns. That fn holds the reader conditional
   that keeps CLJ/CLJS signal counts identical inside reactive bodies (CLAUDE.md
   frame-mismatch rule); this wrapper needs none."
  [node html]
  (math/set-html! node html))

(defn set-cell-html!
  "Write a card-table cell's html, then mark `query`'s matches in it once any
   math is typeset. CLJ no-op.
   Pre:  `query` is nil or a card-filter/query string.
   Post: as math/set-html!, and the cell's `card-filter` ranges are its matches
         of `query` (freememo.card-filter-highlight/paint!)."
  [node html query]
  #?(:cljs (math/set-html! node html #(filter-highlight/paint! node query))
     :clj nil))

(defn clear-cell-highlight!
  "Remove a card-table cell's `card-filter` ranges. CLJ no-op."
  [node]
  #?(:cljs (filter-highlight/paint! node nil)
     :clj nil))

;; RTL detection — checks if text starts with Arabic/Hebrew characters
(defn rtl-text? [text]
  (boolean (and text (re-find #"[\u0600-\u06FF\u0750-\u077F\u08A0-\u08FF\uFB50-\uFDFF\uFE70-\uFEFF\u0590-\u05FF]" text))))

(defn try-delete-anki-notes!
  "Fire-and-forget: attempt to delete notes from Anki. Silently ignores errors."
  [note-ids]
  #?(:cljs
     (when (seq note-ids)
       ((anki-call! "deleteNotes" {:notes (vec note-ids)})
        (fn [_] nil)
        (fn [_] nil)))
     :clj nil))

#?(:clj
   (defmethod opt/run-command! :delete-card [user-id {:keys [payload]}]
     (let [result (cards/delete-card-row! user-id (:member-ids payload))]
       (when-not (:success result)
         (toasts/push! user-id {:level :error :message (or (:error result) "Could not delete card.")}))
       (assoc result :ok? (:success result)))))

(e/defn DeleteCardButton [member-ids user-id]
  (e/client
    (dom/button
      (dom/props {:class "btn-delete-x" :title "Delete card"})
      (dom/text "\u00D7")
      (let [click-event (dom/On "click" (fn [event] (.stopPropagation event) member-ids) nil)
            [t ?error] (e/Token click-event)]
        (when t
          (e/on-unmount #(opt/enqueue-delete-card! click-event))
          (t))))))

;; ---------------------------------------------------------------------------
;; Staleness — the Clojure restatement of `db/unsynced-sql`
;; ---------------------------------------------------------------------------
;;
;; Two predicates, and every sync badge in the app composes them: this file's
;; `sync-state` (the card-table dot) and `library-cards/flashcard-sync-state`
;; (the Library badge, which keeps its own :unpushed vocabulary). One rule, so a
;; badge cannot disagree with the count beside it.
;;
;; They must stay equivalent to `db/unsynced-sql`, which is the SQL half — the
;; two counts and the document tree's synced_cards read that. `not=` on two nils
;; is false and `NULL IS DISTINCT FROM NULL` is false, so the two agree on a card
;; that has never been digested.
;;
;; NEITHER reads `updated_at` or `anki_synced_at` any more. The timestamp moved on
;; writes the digest normalises away and stayed still on the settings changes the
;; digest now covers, so it and the reconcile disagreed — and a card caught
;; between them could never be cleared.

(defn pushed?
  "True iff the card has an Anki note. Pre: `card` carries
   :flashcards/anki_note_id."
  [card]
  (some? (:flashcards/anki_note_id card)))

(defn content-moved?
  "True iff the card's content no longer matches what the last sync agreed on.

   Pre : `card` carries :flashcards/local_current_digest and
         :flashcards/local_baseline_digest. A query that selects neither reads
         nil = nil and reports every card as settled, so a caller that renders a
         badge owes both columns."
  [card]
  (not= (:flashcards/local_current_digest card)
    (:flashcards/local_baseline_digest card)))

(defn unsynced?
  "True iff a sync has work to do for this card — the literal mirror of
   `db/unsynced-sql`, for the counts that are computed in Clojure rather than in
   SQL.

   Pre : `card` carries :flashcards/anki_note_id and both local digests.
   Inv : `(unsynced? c)` agrees with what `db/unsynced-sql` would say about the
         same row. That agreement is the whole contract — a visible unsynced count
         must mean a sync run has work, or the row is stuck."
  [card]
  (or (not (pushed? card)) (content-moved? card)))

;; Card table row component
(defn sync-state
  "Derive sync state from a flashcard row. Returns :unsynced, :synced or :modified.
   Pre: `card` carries the three columns `pushed?` and `content-moved?` read."
  [card]
  (cond
    (not (pushed? card)) :unsynced
    (content-moved? card) :modified
    :else :synced))

(defn occlusion-row-html
  "Compact front-cell HTML for an occlusion GROUP row: image thumbnail + the
   group's mask and card counts + mode + (stripped) header.
   Pre:  mask-count is every rect in the group (db's :occlusion_mask_count);
         card-count is how many flashcard rows it fans out to, i.e. how many
         mask groups it holds.
   Post: reads \"Occlusion · N masks → M cards\" — the same idiom the authoring
         modal's footer and the optimistic pending row use, so one occlusion
         reads the same before and after it lands."
  [image-media-id mask-count card-count mode header-html]
  (let [header-text (some-> header-html (str/replace #"<[^>]+>" "") str/trim not-empty)
        n (or mask-count 0)
        m (or card-count 1)]
    (str "<img src=\"/api/media/" image-media-id "\""
      " style=\"height:28px;max-width:60px;object-fit:cover;border-radius:3px;"
      "vertical-align:middle;margin-right:6px\" />"
      "<span>Occlusion · "
      n " mask" (when (not= 1 n) "s")
      " → " m " card" (when (not= 1 m) "s")
      " · "
      (if (= mode "hide-one") "Hide One" "Hide All")
      (when header-text (str " · " (truncate-html-for-row header-text 80)))
      "</span>")))

(defn- format-ms-brief [ms]
  (let [s (int (/ (or ms 0) 1000))
        m (int (/ s 60))]
    (str m ":" (let [r (rem s 60)] (if (< r 10) (str "0" r) r)))))

(defn score-direction-html
  "Compact HTML for ONE score card: its direction + the audio segment. Used
   where a score card stands alone rather than as a group — the card-history
   modal, whose renditions are single cards."
  [direction start-ms end-ms]
  (str "<span>Score · "
    (if (= direction "audio-front") "Audio → Sheet" "Sheet → Audio")
    " · " (format-ms-brief start-ms) "–" (format-ms-brief end-ms)
    "</span>"))

(defn score-row-html
  "Compact front-cell HTML for a score GROUP row: its directions + the audio
   segment they share.
   Pre:  members are the group's flashcard rows (freememo.card-rows), one per
         direction, in id order.
   Post: one direction names itself; two read \"2 directions\", because the
         pair is one authored card that fanned out."
  [members]
  (let [m (first members)
        n (count members)]
    (if (= 1 n)
      (score-direction-html (:flashcards/score_direction m)
        (:score_start_ms m) (:score_end_ms m))
      (str "<span>Score · " n " directions"
        " · " (format-ms-brief (:score_start_ms m)) "–" (format-ms-brief (:score_end_ms m))
        "</span>"))))

(defn overlapping-row-html
  "Compact front-cell HTML for an overlapping row: title (or first item) plus
   the item count."
  [ol]
  (let [items (:items ol)
        n (count items)
        label (or (not-empty (some-> (:question ol) (str/replace #"<[^>]+>" "") str/trim))
                (first items)
                "Overlapping list")]
    (str "<span>Overlapping · " (truncate-html-for-row label 80)
      " · " n " item" (when (not= 1 n) "s") "</span>")))

(defn row-front-html
  "Front-cell HTML for a display row, one branch per kind. Built from the row's
   members, so the occlusion and score branches describe the whole group.
   Mirrors library-cards/card-front-html — the two tables must render one
   occlusion the same way."
  [c]
  (case (:flashcards/kind c)
    "occlusion"   (occlusion-row-html (:occlusion_image_media_id c)
                    (:occlusion_mask_count c) (count (:member-ids c))
                    (:occlusion_mode c)
                    (get-in c [:flashcards/io_fields :header]))
    "score"       (score-row-html (:members c))
    "overlapping" (overlapping-row-html (:flashcards/overlapping c))
    "cloze"       (card-row-html (:flashcards/cloze c))
    (card-row-html (:flashcards/question c))))

(defn row-sync-state
  "The row's sync dot: the shared member state, or :mixed when they disagree.
   A one-member row returns exactly what sync-state gave before groups
   collapsed."
  [c]
  (let [states (into #{} (map sync-state) (:members c))]
    (if (= 1 (count states)) (first states) :mixed)))

(e/defn CardRow [card !editing-card user-id order anki-overlay review-target? filter-query]
  (e/client
    (let [id (e/server (:flashcards/id card))
          member-ids (e/server (:member-ids card))
          kind (e/server (:flashcards/kind card))
          question (e/server (:flashcards/question card))
          answer (e/server (:flashcards/answer card))
          cloze (e/server (:flashcards/cloze card))
          ol (e/server (:flashcards/overlapping card))
          group-id (e/server (:flashcards/occlusion_group_id card))
          score-group-id (e/server (:flashcards/score_group_id card))
          ;; The card's OWN topic, which is not the ambient one. On a root
          ;; topic get-cards-for-topic* calls get-all-flashcards, so this table
          ;; lists every descendant's cards while dctx/page-topic-id is the
          ;; root — a copy sited from the ambient topic would land on the wrong
          ;; page. Create copy… reads these instead.
          topic-id (e/server (:flashcards/topic_id card))
          root-topic-id (e/server (:flashcards/root_topic_id card))
          anki-note-id (e/server (:flashcards/anki_note_id card))
          ;; Built server-side from the members: a group row's front cell
          ;; describes the whole group, and its dot folds every member's state.
          front-html (e/server (row-front-html card))
          sync-st (e/server (row-sync-state card))
          anki-flags (review/row-flags member-ids anki-overlay)]
      (let [occ? (= kind "occlusion")
            score? (= kind "score")
            cloze? (= kind "cloze")
            overlapping? (= kind "overlapping")
            span2? (or cloze? occ? score? overlapping?)]
        (let [edit-card! (fn [_]
                           (let [data (cond
                                        occ?
                                        ;; Routes to OcclusionModal (edit mode)
                                        ;; — see ContentCardTable.
                                        {:kind "occlusion" :mode :edit :group-id group-id
                                         :anki-flags anki-flags}
                                        score?
                                        ;; Routes to ScoreEditLoader — loads the
                                        ;; pair into the in-view score editor.
                                        {:kind "score" :group-id score-group-id
                                         :anki-flags anki-flags}
                                        overlapping?
                                        ;; Routes to EditCardModal (default case)
                                        ;; carrying the parsed list so the
                                        ;; overlapping pane can pre-fill.
                                        {:id id :kind kind :overlapping ol
                                         :anki-flags anki-flags
                                         :anki-note-id anki-note-id
                                         :topic-id topic-id :root-topic-id root-topic-id}
                                        :else
                                        {:id id :kind kind :question question :answer answer :cloze cloze
                                         :anki-flags anki-flags
                                         :anki-note-id anki-note-id
                                         :topic-id topic-id :root-topic-id root-topic-id})]
                             (log/log-debug (str "Edit card clicked id=" id " kind=" kind))
                             (reset! !editing-card data)))]
        (dom/tr
          (dom/props {:class (str (review/row-class anki-flags)
                               (when review-target? " card-review-target"))
                      :style {:--order order :cursor "pointer"}})
          (dom/On "click" edit-card! nil)
          ;; Sync indicator
          (dom/td
            (dom/props {:style {:padding "6px 4px" :text-align "center"
                                :border-bottom "1px solid var(--color-border)" :font-size "10px"}
                        :title (case sync-st
                                 :unsynced "Not synced to Anki"
                                 :synced (str "Synced to Anki")
                                 :modified "Modified since last sync"
                                 :mixed "This row's cards are in different sync states")})
            (dom/text (case sync-st
                        :unsynced "\u25CB"
                        :synced "\u25CF"
                        :modified "\u25CF"
                        :mixed "\u25D0"))
            (dom/props {:style {:color (case sync-st
                                         :unsynced "var(--color-warning)"
                                         :synced "var(--color-success)"
                                         :modified "var(--color-warning)"
                                         :mixed "var(--color-warning)")}}))
          ;; Front column — cloze, occlusion, and score span both content columns
          (dom/td
            (dom/props {:dir "auto"
                        :class "card-row-cell"
                        :style (merge {:padding-block "6px" :padding-inline "8px"
                                       :border-bottom "1px solid var(--color-border)"}
                                 (when span2? {:grid-column "span 2"}))})
            (a11y/KeyActivate {:label "Edit card"} edit-card!)
            (e/for-by identity [_k [(str "f-" id)]]
              (dom/div
                (dom/props {:class "card-row-html"})
                (e/on-unmount (let [node dom/node] #(clear-cell-highlight! node)))
                (set-cell-html! dom/node front-html filter-query))))
          ;; Back column — hidden when the front cell spans both
          (let [back-text (if span2? "" (or answer ""))
                back-html (card-row-html back-text)]
            (dom/td
              (dom/props {:dir "auto"
                          :class "card-row-cell"
                          :style (merge {:padding-block "6px" :padding-inline "8px"
                                         :border-bottom "1px solid var(--color-border)"}
                                   (when span2? {:display "none"}))})
              (e/for-by identity [_k [(str "b-" id)]]
                (dom/div
                  (dom/props {:class "card-row-html"})
                  (e/on-unmount (let [node dom/node] #(clear-cell-highlight! node)))
                  (set-cell-html! dom/node back-html filter-query)))))
          ;; Delete column
          (dom/td
            (dom/props {:style {:padding "6px 4px" :text-align "center"
                                :border-bottom "1px solid var(--color-border)"}})
            (DeleteCardButton member-ids user-id))))))))

;; ---------------------------------------------------------------------------
;; Optimistic add-card rows (freememo.optimistic :pending-cards overlay).
;; Rendered by ContentCardTable above the real rows: light-green while pending,
;; green on confirm, danger with retry/dismiss on error.
;; ---------------------------------------------------------------------------

(defn overlay-front-text [entry]
  (let [{:keys [kind card-data geometry directions]} (:payload entry)
        c (first card-data)]
    (case kind
      "occlusion" (let [rects (:rects geometry)
                        n (count rects)
                        cards (ord/card-count rects)]
                    (str "Image occlusion · " n " mask" (when (not= 1 n) "s")
                      " → " cards " card" (when (not= 1 cards) "s")))
      "score" (let [n (transduce (map (comp count :rects)) + 0 (:pages geometry))]
                (str "Score · " (count directions) " card"
                  (when (not= 1 (count directions)) "s")
                  " · " n " rect" (when (not= 1 n) "s")))
      "overlapping" (let [items (:items c)
                          n (count items)]
                      (str "Overlapping · " (or (not-empty (:question c)) (first items) "list")
                        " · " n " item" (when (not= 1 n) "s")))
      "cloze" (:c c)
      (:q c))))

(defn overlay-back-text [entry]
  (let [{:keys [kind card-data]} (:payload entry)]
    (if (= kind "basic") (:a (first card-data)) "")))

(e/defn RetryPendingButton [tempid user-id]
  (e/client
    (dom/button
      (dom/props {:title "Retry"
                  :style {:background "transparent" :border "none" :cursor "pointer"
                          :color "var(--color-danger-text)" :font-size "13px"}})
      (dom/text "↻")
      (let [click (dom/On "click" (fn [e] (.stopPropagation e) tempid) nil)
            [t _] (e/Token click)]
        (dom/props {:disabled (some? t)})
        (when t
          ;; fire-and-forget: retry-pending! returns :done. Client-local: sets
          ;; the overlay row back to :pending and re-enqueues in this session
          ;; (the row stays mounted, so no e/on-unmount needed).
          (case (opt/retry-pending! tempid)
            (t)))))))

(e/defn DismissPendingButton [tempid user-id]
  (e/client
    (dom/button
      (dom/props {:title "Dismiss"
                  :style {:background "transparent" :border "none" :cursor "pointer"
                          :color "var(--color-text-hint)" :font-size "13px"}})
      (dom/text "×")
      (let [click (dom/On "click" (fn [e] (.stopPropagation e) tempid) nil)
            [t _] (e/Token click)]
        (dom/props {:disabled (some? t)})
        (when t
          ;; forget-pending! removes the overlay entry, unmounting this row, so
          ;; it fires from e/on-unmount when (t) spends the token (client-local).
          (e/on-unmount #(opt/forget-pending! tempid))
          (t))))))

(e/defn PendingCardRow [entry user-id row-index]
  (e/client
    (let [tempid (:tempid entry)
          status (:status entry)
          ;; cloze and occlusion rows both span the two content columns
          cloze? (contains? #{"cloze" "occlusion" "overlapping"} (get-in entry [:payload :kind]))
          front-html (card-row-html (overlay-front-text entry))
          back-html (card-row-html (overlay-back-text entry))
          bg (case status
               :error "var(--color-danger-bg)"
               "var(--color-success-light)")]   ; pending + confirmed both light-green
      (dom/tr
        (dom/props {:class "pending-card-row"
                    :style {:--order row-index :background bg
                            :transition "background-color 0.3s ease"}})
        ;; Status indicator
        (dom/td
          (dom/props {:style {:padding "6px 4px" :text-align "center" :font-size "10px"
                              :border-bottom "1px solid var(--color-border)"
                              :color (if (= :error status) "var(--color-danger)" "var(--color-success)")}
                      :title (case status
                               :pending "Saving…"
                               :confirmed "Saved"
                               :error (or (:error entry) "Save failed")
                               "")})
          (dom/text (case status :pending "…" :confirmed "✓" :error "!" "")))
        ;; Front — cloze spans both content columns
        (dom/td
          (dom/props {:dir "auto" :class "card-row-cell"
                      :style (merge {:padding-block "6px" :padding-inline "8px"
                                     :border-bottom "1px solid var(--color-border)"}
                               (when cloze? {:grid-column "span 2"}))})
          (e/for-by identity [_k [(str "pf-" tempid)]]
            (dom/div (dom/props {:class "card-row-html"})
              (set-inner-html! dom/node front-html))))
        ;; Back — hidden for cloze
        (dom/td
          (dom/props {:dir "auto" :class "card-row-cell"
                      :style (merge {:padding-block "6px" :padding-inline "8px"
                                     :border-bottom "1px solid var(--color-border)"}
                               (when cloze? {:display "none"}))})
          (e/for-by identity [_k [(str "pb-" tempid)]]
            (dom/div (dom/props {:class "card-row-html"})
              (set-inner-html! dom/node back-html))))
        ;; Action — retry + dismiss on error only
        (dom/td
          (dom/props {:style {:padding "6px 2px" :text-align "center"
                              :border-bottom "1px solid var(--color-border)"
                              :white-space "nowrap"}})
          (when (= :error status)
            (e/amb
              (RetryPendingButton tempid user-id)
              (DismissPendingButton tempid user-id))))))))
