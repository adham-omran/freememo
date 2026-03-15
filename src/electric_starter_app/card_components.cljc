(ns electric-starter-app.card-components
  "Shared card display components — used by both ocr-page and extract-page."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [electric-starter-app.cards :as cards])
   #?(:cljs [electric-starter-app.anki-sync-helpers :refer [anki-call!]])))

;; Query wrapper: takes refresh arg to create Electric reactive dependency
;; Plain defn visible on both CLJ and CLJS — callers wrap in e/server
(defn get-cards* [_refresh document-id page-number]
  #?(:clj (cards/get-cards document-id page-number)
     :cljs nil))

;; Query wrapper for extract-level cards
(defn get-cards-by-extract* [_refresh content-item-id]
  #?(:clj (cards/get-cards-by-content-item content-item-id)
     :cljs nil))

(defn try-delete-anki-notes!
  "Fire-and-forget: attempt to delete notes from Anki. Silently ignores errors."
  [note-ids]
  #?(:cljs
     (when (seq note-ids)
       (-> (anki-call! "deleteNotes" {:notes (vec note-ids)})
           (.catch (fn [_] nil))))
     :clj nil))

;; Delete button — parameterized with !refresh atom
(e/defn DeleteCardButton [id !refresh]
  (e/client
    (dom/button
      (dom/props {:class "btn-delete-x"})
      (dom/text "\u00D7")
      (let [click-event (dom/On "click" (fn [_] id) nil)
            [?token ?error] (e/Token click-event)]
        (dom/props {:disabled (some? ?token)
                    :class "btn-delete-x"
                    :style {:background (if (some? ?token) "#999" "var(--color-danger)")
                            :cursor (if (some? ?token) "not-allowed" "pointer")}})
        (when ?error
          (dom/div
            (dom/props {:style {:color "red" :font-size "11px"}})
            (dom/text ?error)))
        (when-some [token ?token]
          (let [result (e/server (cards/delete-card click-event))]
            (if (:success result)
              (do (e/server (swap! !refresh inc))
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

(e/defn CardRow [card !editing-card !refresh order]
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
        (dom/props {:style {:--order order}})
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
                                       :modified "#eab308")}}))
        ;; Front column
        (let [front-text (if (= kind "basic") question cloze)]
          (dom/td
            (dom/props {:style {:padding "6px 8px"
                                :border-bottom "1px solid var(--color-border)"}})
            (dom/text front-text)))
        ;; Back column
        (let [back-text (if (= kind "basic") (or answer "") "")]
          (dom/td
            (dom/props {:style {:padding "6px 8px"
                                :border-bottom "1px solid var(--color-border)"}})
            (dom/text back-text)))
        ;; Kind column
        (dom/td
          (dom/props {:style {:padding "6px 8px" :width "60px" :font-size "12px"
                              :border-bottom "1px solid var(--color-border)"}})
          (dom/text kind))
        ;; Edit column
        (dom/td
          (dom/props {:style {:padding "6px 4px" :width "40px" :text-align "center"
                              :border-bottom "1px solid var(--color-border)"}})
          (dom/button
            (dom/props {:style {:padding "2px 6px" :background "var(--color-primary)" :color "white"
                                :border "none" :border-radius "var(--radius-sm)" :cursor "pointer"
                                :font-size "12px"}})
            (dom/text "\u270E")
            (dom/On "click" (fn [_]
                              (let [data {:id id :kind kind :question question :answer answer :cloze cloze}]
                                (println "EDIT CLICK data:" (pr-str data))
                                (reset! !editing-card data))) nil)))
        ;; Delete column
        (dom/td
          (dom/props {:style {:padding "6px 4px" :width "40px" :text-align "center"
                              :border-bottom "1px solid var(--color-border)"}})
          (DeleteCardButton id !refresh))))))
