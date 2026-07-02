(ns freememo.card-components
  "Shared card display components — used by TopicPage's BottomPanel."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.logging :as log]
   #?(:clj [freememo.cards :as cards])
   #?(:cljs [freememo.anki-sync-helpers :refer [anki-call!]])
   #?(:clj [freememo.optimistic :as opt])
   #?(:clj [freememo.user-state :as us])))

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

(defn card-row-html
  "Prepare card-field HTML for inline rendering inside a CardRow cell."
  [html]
  (-> (or html "")
    replace-img-with-chip
    (truncate-html-for-row 200)))

(defn set-inner-html!
  "CLJS-only side effect: write html into node.innerHTML. CLJ no-op.
   Plain defn so the reader conditional is invisible to Electric's reactive
   compiler — keeps CLJ/CLJS signal counts identical (CLAUDE.md frame-mismatch rule)."
  [node html]
  #?(:cljs (set! (.-innerHTML node) (str (or html "")))
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

;; Delete button — bumps per-user :card-mutations on success
(e/defn DeleteCardButton [id user-id]
  (e/client
    (dom/button
      (dom/props {:class "btn-delete-x"})
      (dom/text "\u00D7")
      (let [click-event (dom/On "click" (fn [e] (.stopPropagation e) id) nil)
            [t ?error] (e/Token click-event)]
        (dom/props {:disabled (some? t)
                    :class "btn-delete-x"
                    :style {:background (if (some? t) "var(--color-text-hint)" "var(--color-danger)")
                            :cursor (if (some? t) "not-allowed" "pointer")}})
        (when ?error
          (dom/div
            (dom/props {:style {:color "var(--color-danger)" :font-size "11px"}})
            (dom/text ?error)))
        (when t
          (let [result (e/server (e/Offload #(cards/delete-card user-id click-event)))]
            (when (some? result)
              (if (:success result)
                (case (e/server (swap! (us/get-atom user-id :card-mutations) inc))
                  (case (e/client (when-some [note-id (:anki-note-id result)]
                                    (try-delete-anki-notes! [note-id])))
                    (t)))
                (t (:error result))))))))))

;; Card table row component
(defn sync-state
  "Derive sync state from card fields. Returns :unsynced, :synced, or :modified."
  [synced-at updated-at]
  (cond
    (nil? synced-at) :unsynced
    (and updated-at (pos? (compare (str updated-at) (str synced-at)))) :modified
    :else :synced))

(defn occlusion-row-html
  "Compact front-cell HTML for an occlusion row: image thumbnail + mask
   ordinal + mode + (stripped) header."
  [image-media-id mask-ordinal mode header-html]
  (let [header-text (some-> header-html (str/replace #"<[^>]+>" "") str/trim not-empty)]
    (str "<img src=\"/api/media/" image-media-id "\""
      " style=\"height:28px;max-width:60px;object-fit:cover;border-radius:3px;"
      "vertical-align:middle;margin-right:6px\" />"
      "<span>Occlusion · mask " mask-ordinal " · "
      (if (= mode "hide-one") "Hide One" "Hide All")
      (when header-text (str " · " (truncate-html-for-row header-text 80)))
      "</span>")))

(e/defn CardRow [card !editing-card user-id order]
  (e/client
    (let [id (e/server (:flashcards/id card))
          kind (e/server (:flashcards/kind card))
          question (e/server (:flashcards/question card))
          answer (e/server (:flashcards/answer card))
          cloze (e/server (:flashcards/cloze card))
          group-id (e/server (:flashcards/occlusion_group_id card))
          mask-ordinal (e/server (:flashcards/mask_ordinal card))
          occ-image-id (e/server (:occlusion_image_media_id card))
          occ-mode (e/server (:occlusion_mode card))
          io-header (e/server (get-in card [:flashcards/io_fields :header]))
          sync-st (e/server (sync-state (:flashcards/anki_synced_at card)
                              (:flashcards/updated_at card)))]
      (let [occ? (= kind "occlusion")
            cloze? (= kind "cloze")
            span2? (or cloze? occ?)]
        (dom/tr
          (dom/props {:style {:--order order :cursor "pointer"}})
          (dom/On "click" (fn [_]
                            (let [data (if occ?
                                         ;; Routes to OcclusionModal (edit mode)
                                         ;; — see ContentCardTable.
                                         {:kind "occlusion" :mode :edit :group-id group-id}
                                         {:id id :kind kind :question question :answer answer :cloze cloze})]
                              (log/log-debug (str "Edit card clicked id=" id " kind=" kind))
                              (reset! !editing-card data))) nil)
          ;; Sync indicator
          (dom/td
            (dom/props {:style {:padding "6px 4px" :text-align "center"
                                :border-bottom "1px solid var(--color-border)" :font-size "10px"}
                        :title (case sync-st
                                 :unsynced "Not synced to Anki"
                                 :synced (str "Synced to Anki")
                                 :modified "Modified since last sync")})
            (dom/text (case sync-st
                        :unsynced "\u25CB"
                        :synced "\u25CF"
                        :modified "\u25CF"))
            (dom/props {:style {:color (case sync-st
                                         :unsynced "var(--color-warning)"
                                         :synced "var(--color-success)"
                                         :modified "var(--color-warning)")}}))
          ;; Front column — cloze and occlusion span both content columns
          (let [front-html (if occ?
                             (occlusion-row-html occ-image-id mask-ordinal occ-mode io-header)
                             (card-row-html (if cloze? cloze question)))]
            (dom/td
              (dom/props {:dir "auto"
                          :class "card-row-cell"
                          :style (merge {:padding-block "6px" :padding-inline "8px"
                                         :border-bottom "1px solid var(--color-border)"}
                                   (when span2? {:grid-column "span 2"}))})
              (e/for-by identity [_k [(str "f-" id)]]
                (dom/div
                  (dom/props {:class "card-row-html"})
                  (set-inner-html! dom/node front-html)))))
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
                  (set-inner-html! dom/node back-html)))))
          ;; Delete column
          (dom/td
            (dom/props {:style {:padding "6px 4px" :text-align "center"
                                :border-bottom "1px solid var(--color-border)"}})
            (DeleteCardButton id user-id)))))))

;; ---------------------------------------------------------------------------
;; Optimistic add-card rows (freememo.optimistic :pending-cards overlay).
;; Rendered by ContentCardTable above the real rows: light-green while pending,
;; green on confirm, danger with retry/dismiss on error.
;; ---------------------------------------------------------------------------

(defn overlay-front-text [entry]
  (let [{:keys [kind card-data geometry]} (:payload entry)
        c (first card-data)]
    (case kind
      "occlusion" (let [n (count (:rects geometry))]
                    (str "Image occlusion · " n " mask" (when (not= 1 n) "s")))
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
                          :color "var(--color-danger)" :font-size "13px"}})
      (dom/text "↻")
      (let [click (dom/On "click" (fn [e] (.stopPropagation e) tempid) nil)
            [t _] (e/Token click)]
        (dom/props {:disabled (some? t)})
        (when t
          (case (e/server (opt/retry-pending-card! user-id tempid))
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
          (case (e/server (opt/forget-pending-card! user-id tempid))
            (t)))))))

(e/defn PendingCardRow [entry user-id]
  (e/client
    (let [tempid (:tempid entry)
          status (:status entry)
          ;; cloze and occlusion rows both span the two content columns
          cloze? (contains? #{"cloze" "occlusion"} (get-in entry [:payload :kind]))
          front-html (card-row-html (overlay-front-text entry))
          back-html (card-row-html (overlay-back-text entry))
          bg (case status
               :error "var(--color-danger-bg)"
               "var(--color-success-light)")]   ; pending + confirmed both light-green
      (dom/tr
        (dom/props {:class "pending-card-row"
                    :style {:--order 0 :background bg
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
