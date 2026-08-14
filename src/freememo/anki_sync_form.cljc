(ns freememo.anki-sync-form
  "Extracted form UI components for Anki sync — kept in a separate namespace
   to stay below the JVM 64KB method limit imposed by Electric v3's e/defn macro."
  (:require
   [clojure.string :as string]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.typeahead :refer [Typeahead]]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])))

;; `topic-scope-context*` and `scope-label` lived here to drive the modal's Scope
;; selector. Reconcile has no scope — see `AnkiSyncForm` — so both are gone, and
;; with them the `doc-context` require.

(e/defn TagInput
  "Multi-tag input with chip display and autocomplete from all-tags."
  [!tags all-tags]
  (e/client
    (let [tags (e/watch !tags)
          !search (atom "")
          search (e/watch !search)
          !focused (atom false)
          focused (e/watch !focused)
          !active-idx (atom -1)
          active-idx (e/watch !active-idx)
          filtered (when (and focused (seq search))
                     (->> all-tags
                       (filter (fn [t]
                                 (and (string/includes?
                                        (string/lower-case t)
                                        (string/lower-case search))
                                   (not (some #{t} tags)))))
                       (take 5)
                       vec))
          add-tag! (fn [t]
                     (when-not (some #{t} @!tags)
                       (swap! !tags conj t))
                     (reset! !search "")
                     (reset! !active-idx -1))]
      (dom/div
        (dom/props {:style {:position "relative"}})
        ;; Input row with chips
        (dom/div
          (dom/props {:style {:display "flex" :flex-wrap "wrap" :align-items "center"
                              :gap "var(--sp-1)" :padding "var(--sp-1) 6px"
                              :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"
                              :font-size "14px" :min-height "30px" :background "var(--color-bg-card)"}})
          ;; Chips
          (e/for [t (e/diff-by identity tags)]
            (dom/span
              (dom/props {:style {:display "inline-flex" :align-items "center" :gap "3px"
                                  :background "var(--color-border)" :border-radius "3px"
                                  :padding "1px 5px" :font-size "13px"}})
              (dom/text t)
              (dom/button
                (dom/props {:type "button"
                            :style {:background "none" :border "none" :cursor "pointer"
                                    :padding "0" :font-size "13px" :line-height "1"
                                    :color "var(--color-text-secondary)"}})
                (dom/text "\u00d7")
                (dom/On "click" (fn [_] (swap! !tags (fn [ts] (vec (remove #{t} ts))))) nil))))
          ;; Text input
          (dom/input
            (dom/props {:type "text"
                        :value search
                        :placeholder (if (seq tags) "" "Add tags...")
                        :style {:border "none" :outline "none" :font-size "14px"
                                :flex "1" :min-width "80px" :padding "2px"}})
            (dom/On "focus" (fn [_] (reset! !focused true)
                              (reset! !active-idx -1)) nil)
            (dom/On "blur"
              (fn [_]
                (reset! !focused false)
                (reset! !active-idx -1)
                (let [val (string/trim @!search)]
                  (when (seq val)
                    (when-not (some #{val} @!tags)
                      (swap! !tags conj val))
                    (reset! !search ""))))
              nil)
            (let [v (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
              (when (some? v)
                (reset! !search v)
                (reset! !active-idx -1)))
            (dom/On "keydown"
              (fn [e]
                (let [key (.-key e)
                      val (string/trim search)
                      n (count filtered)]
                  (cond
                    (= key "ArrowDown")
                    (do (.preventDefault e)
                      (reset! !active-idx (mod (inc active-idx) n)))
                    (= key "ArrowUp")
                    (do (.preventDefault e)
                      (reset! !active-idx (mod (dec active-idx) n)))
                    (and (= key "Enter") (>= active-idx 0))
                    (do (.preventDefault e)
                      (add-tag! (nth filtered active-idx)))
                    (and (= key " ") (seq val))
                    (do (.preventDefault e)
                      (add-tag! val))
                    (and (= key "Enter") (seq val))
                    (do (.preventDefault e)
                      (add-tag! val))
                    (and (= key "Escape") (seq filtered))
                    (do (.preventDefault e)
                      (reset! !active-idx -1)
                      (reset! !focused false))
                    (and (= key "Backspace") (empty? search) (seq tags))
                    (swap! !tags (fn [ts] (vec (butlast ts)))))))
              nil)))
        ;; Dropdown
        (when (seq filtered)
          (dom/div
            (dom/props {:style {:position "absolute" :top "100%" :left "0" :right "0"
                                :background "var(--color-bg-card)" :border "1px solid var(--color-border)"
                                :border-radius "var(--radius-sm)" :z-index "100"
                                :box-shadow "0 2px 4px rgba(0,0,0,0.15)"}})
            (e/for [[i t] (e/diff-by first (map-indexed vector filtered))]
              (dom/div
                (dom/props {:style {:padding "5px 8px" :cursor "pointer" :font-size "14px"
                                    :background (cond
                                                  (= i active-idx) "var(--color-highlight)"
                                                  (odd? i) "var(--color-bg-subtle)"
                                                  :else "var(--color-bg-card)")}})
                (dom/text t)
                (dom/On "mousemove" (fn [_] (reset! !active-idx i)) nil)
                (dom/On "mousedown"
                  (fn [e]
                    (.preventDefault e)
                    (add-tag! t))
                  nil)))))))))

(defn resolve-anki-header*
  "Effective header (per-PDF override, else off). {:use-header bool :header-text str}."
  [user-id root-topic-id]
  #?(:clj (settings/resolve-anki-header user-id root-topic-id)
     :cljs nil))

(defn save-anki-header-for-topic!*
  [user-id root-topic-id use-header header-text]
  #?(:clj (settings/save-anki-header-for-topic! user-id root-topic-id use-header header-text)
     :cljs nil))

(e/defn HeaderSettings
  "Per-PDF custom header: Forms5 standalone Checkbox! + Input!, auto-saving each
   edit to the topic's header rows. Loaded value comes from a server query
   (per-PDF override, else off), seeded into local atoms via e/snapshot so the
   managed widgets reflect it without depending on apply-prefs!. Decoupled from
   the form map; push resolves the header server-side independently."
  [user-id root-id]
  (e/client
    (let [loaded       (e/server (e/Offload #(resolve-anki-header* user-id root-id)))
          !use-header  (atom (e/snapshot (boolean (:use-header loaded))))
          !header-text (atom (e/snapshot (or (:header-text loaded) "")))
          use-header   (e/watch !use-header)
          header-text  (e/watch !header-text)
          edits
          (dom/div (dom/props {:style {:margin-bottom "var(--sp-3)"}})
            (e/amb
              (dom/label
                (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-2)" :font-size "14px" :margin-bottom "var(--sp-2)"}})
                (e/amb
                  (forms/Checkbox! :use-header use-header)
                  (do (dom/text "Add custom header to each card") (e/amb))))
              (if use-header
                (forms/Input! :header-text header-text
                  :placeholder "e.g., Chapter 5: Accounting"
                  :class "input input-full" :style {:font-size "15px"})
                (e/amb))))]
      ;; Save service: one branch per edit. Persist per-PDF, then update the
      ;; local atom on token-spend (e/on-unmount) so the managed widget reflects
      ;; the committed value without reverting. Disabling clears header-text.
      (e/for [[t edit] (e/diff-by first (e/as-vec edits))]
        (let [uh (if (contains? edit :use-header) (boolean (:use-header edit)) use-header)
              ht (if uh
                   (if (contains? edit :header-text) (:header-text edit) header-text)
                   "")]
          (e/on-unmount #(do (reset! !use-header uh) (reset! !header-text ht)))
          (case (e/server (save-anki-header-for-topic!* user-id root-id uh ht))
            (t)))))))

(e/defn AnkiSyncOptions
  "Custom header (per-PDF, Forms5) + allow-duplicates checkbox + tags.
   conn = {:!all-tags ...}  form = {:!allow-dupes :!use-tags :!tags ...}
   allow-dupes/use-tags have no per-field server save (unlike the header) —
   they're pure client form state, read at push-time (anki-sync-panels) and
   persisted only as part of the push's last-used prefs-map. Checkbox! still
   writes straight through to the same form atoms so that consumer keeps
   seeing the current value."
  [user-id root-id conn form]
  (e/client
    (let [all-tags (e/watch (:!all-tags conn))
          allow-dupes (e/watch (:!allow-dupes form))
          use-tags (e/watch (:!use-tags form))]
      (HeaderSettings user-id root-id)
      ;; Allow duplicates
      (dom/div
        (dom/props {:style {:margin-bottom "var(--sp-3)"}})
        (dom/label
          (dom/props {:style {:display "flex" :align-items "center" :gap "6px" :font-size "14px"}})
          (e/amb
            (e/for [[t {v :allow-dupes}] (forms/Checkbox! :allow-dupes allow-dupes)]
              (reset! (:!allow-dupes form) v)
              (t))
            (do (dom/text "Allow duplicates") (e/amb)))))
      ;; Tags
      (dom/div
        (dom/props {:style {:margin-bottom "var(--sp-4)"}})
        (dom/label
          (dom/props {:style {:display "flex" :align-items "center" :gap "6px" :font-size "14px" :margin-bottom "6px"}})
          (e/amb
            (e/for [[t {v :use-tags}] (forms/Checkbox! :use-tags use-tags)]
              (reset! (:!use-tags form) v)
              (t))
            (do (dom/text "Tags") (e/amb))))
        (when use-tags
          (TagInput (:!tags form) all-tags))))))

(e/defn AnkiSyncForm
  "The connected-state form: deck, custom header, tags. Note types are app-owned
   (FreeMemo Basic/Cloze), so there is no model/field selection.

   There is no scope control. A reconcile covers everything ever synced by
   definition — a card deleted in a document nobody has open still owes its Anki
   note a deletion, and a note edited in Anki still owes its card the edit.
   Narrowing the run would make both invisible. The deck and the options below
   are configuration for the whole collection, not choices for one run.

   conn = {:!decks :!selected-deck :!all-tags ...}"
  [user-id root-id conn form]
  (e/client
    (let [decks (e/watch (:!decks conn))
          ;; An Anki deck name IS its identity — AnkiConnect exposes no deck id,
          ;; and :!selected-deck must keep holding the name because it flows on
          ;; into `settings {:deck …}` and the persisted prefs row.
          deck-options (mapv (fn [d] {:id d :label d}) decks)]

      ;; Deck
      (dom/div
        (dom/props {:style {:margin-bottom "var(--sp-3)"}})
        (dom/label (dom/props {:style {:font-weight "600" :font-size "14px" :display "block" :margin-bottom "4px"}})
          (dom/text "Deck"))
        (Typeahead (:!selected-deck conn) deck-options "Start typing deck name..." nil nil))

      ;; Options
      (AnkiSyncOptions user-id root-id conn form))))

(e/defn AnkiSyncStatus
  "Status line and the modal's two buttons: Reconcile and Cancel.

   One action, no direction. Which way each card moves is decided per card by
   `anki-reconcile/classify`, from each side's own baseline — the Push and Pull
   buttons this replaces could only ever move every selected card the same way,
   which is what overwrote edits made in Anki.

   The click hands off rather than running the work here: prefs are saved, the
   modal closes, and `:quick-sync` runs the reconcile headlessly with toasts.
   A run covers the whole collection and outlives the dialog that started it.

   sync = {:!phase :!result :!error :!push-pairs :!push-btn}
   conn = {:!selected-deck ...} — the deck gate; note types are app-owned."
  [sync !show-modal conn]
  (e/client
    (let [sync-phase (e/watch (:!phase sync))
          sync-error (e/watch (:!error sync))
          can-run? (helpers/pushable? (e/watch (:!selected-deck conn)))
          {:keys [!phase !result !error !push-pairs !push-btn]} sync]

      ;; Status display
      (when sync-phase
        (dom/div
          (dom/props {:style {:margin-bottom "var(--sp-4)" :padding "var(--sp-3)" :background "var(--color-bg-subtle)"
                              :border-radius "var(--radius-sm)" :font-size "14px"}})
          (cond
            (= sync-phase :pushing) (dom/text "Starting reconcile…")
            (= sync-phase :error)
            (dom/div
              (dom/props {:style {:color "var(--color-danger-text)"}})
              (dom/text (str "Error: " (or (:message sync-error) "Unknown error"))))
            :else (dom/text "Working…"))))

      ;; Action buttons
      (dom/div
        (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "var(--sp-2)" :margin-top "var(--sp-2)"}})
        (dom/button
          (dom/props {:class "btn btn-secondary" :style {:font-size "15px"}})
          (dom/text "Cancel")
          (dom/On "click" (fn [_]
                            (reset! !show-modal false)
                            (reset! !phase nil)
                            (reset! !result nil)
                            (reset! !error nil)
                            (reset! !push-pairs nil))
            nil))
        (when-not (= :pushing sync-phase)
          (dom/button
            (dom/props {:class "btn btn-primary" :style {:font-size "15px"}
                        :disabled (not can-run?)})
            (dom/text "Reconcile")
            (reset! !push-btn dom/node)
            (e/on-unmount (fn [] (reset! !push-btn nil)))
            (dom/On "click"
              (fn [_]
                (reset! !phase :pushing)
                (reset! !result nil)
                (reset! !error nil)
                (reset! !push-pairs nil))
              nil)))))))
