(ns freememo.card-components
  "Shared card display components — used by both ocr-page and extract-page."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   #?(:clj [freememo.cards :as cards])
   #?(:cljs [freememo.anki-sync-helpers :refer [anki-call!]])
   #?(:clj [freememo.user-state :as us])))

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
            [?token ?error] (e/Token click-event)]
        (dom/props {:disabled (some? ?token)
                    :class "btn-delete-x"
                    :style {:background (if (some? ?token) "var(--color-text-hint)" "var(--color-danger)")
                            :cursor (if (some? ?token) "not-allowed" "pointer")}})
        (when ?error
          (dom/div
            (dom/props {:style {:color "var(--color-danger)" :font-size "11px"}})
            (dom/text ?error)))
        (when-some [token ?token]
          (let [result (e/server (cards/delete-card click-event))]
            (if (:success result)
              (do (e/server (swap! (us/get-atom user-id :card-mutations) inc))
                (when-some [note-id (:anki-note-id result)]
                  (e/client (try-delete-anki-notes! [note-id])))
                (token))
              (token (:error result)))))))))

;; Card table row component
(defn sync-state
  "Derive sync state from card fields. Returns :unsynced, :synced, or :modified."
  [synced-at updated-at]
  (cond
    (nil? synced-at) :unsynced
    (and updated-at (pos? (compare (str updated-at) (str synced-at)))) :modified
    :else :synced))

(e/defn CardRow [card !editing-card user-id order]
  (e/client
    (let [id (e/server (:flashcards/id card))
          kind (e/server (:flashcards/kind card))
          question (e/server (:flashcards/question card))
          answer (e/server (:flashcards/answer card))
          cloze (e/server (:flashcards/cloze card))
          synced-at (e/server (:flashcards/anki_synced_at card))
          updated-at (e/server (:flashcards/updated_at card))
          sync-st (sync-state synced-at updated-at)]
      (dom/tr
        (dom/props {:style {:--order order :cursor "pointer"}})
        (dom/On "click" (fn [_]
                          (let [data {:id id :kind kind :question question :answer answer :cloze cloze}]
                            (log/log-debug (str "Edit card clicked id=" id " kind=" kind))
                            (reset! !editing-card data))) nil)
        ;; Sync indicator
        (dom/td
          (dom/props {:style {:padding "6px 4px" :width "24px" :text-align "center"
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
        ;; Front column
        (let [front-text (if (= kind "basic") question cloze)]
          (dom/td
            (dom/props {:dir "auto"
                        :style {:padding-block "6px" :padding-inline "8px"
                                :border-bottom "1px solid var(--color-border)"}})
            (dom/text front-text)))
        ;; Back column
        (let [back-text (if (= kind "basic") (or answer "") "")]
          (dom/td
            (dom/props {:dir "auto"
                        :style {:padding-block "6px" :padding-inline "8px"
                                :border-bottom "1px solid var(--color-border)"}})
            (dom/text back-text)))
        ;; Delete column
        (dom/td
          (dom/props {:style {:padding "6px 4px" :width "40px" :text-align "center"
                              :border-bottom "1px solid var(--color-border)"}})
          (DeleteCardButton id user-id))))))
