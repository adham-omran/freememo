(ns freememo.typeahead
  "Filtered-dropdown input widgets: `Typeahead` (id-keyed combobox) and
   `Suggest` (free text with suggestions). They differ in their postcondition,
   not their rendering — the dropdown itself is `OptionRows`, shared."
  (:require
   [clojure.string :as string]
   [contrib.data :refer [clamp-left]]
   [freememo.logging :as log]
   [freememo.modal-shell :as modal]
   [freememo.viewport :as viewport]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]))

(def ^:private row-height 30)

(defn scroll-row-into-view!
  "Imperatively scroll `node` so the row at `idx` (each `rh` px) is visible —
   keyboard nav must keep the active row on screen in the virtualized dropdown.
   CLJS-only body; the var exists on both peers so the e/defn dual-compiler is
   satisfied. Returns nil."
  [node idx rh]
  #?(:cljs
     (when (and node (>= idx 0))
       (let [top (* idx rh)
             vh  (.-clientHeight node)
             cur (.-scrollTop node)]
         (cond
           (< top cur)                  (set! (.-scrollTop node) top)
           (> (+ top rh) (+ cur vh))    (set! (.-scrollTop node) (- (+ top rh) vh)))
         nil))
     :clj nil))

(defn check-options!
  "Dev guard: warn when a `Typeahead` option is not a map carrying a non-nil :id.

   A seq of strings is the historical mistake and it fails *silently*:
   `(:label \"Default\")` is nil, so rows paint blank, `(str nil)` makes the
   filter match nothing on the first keystroke, and mousedown writes nil to
   !atom — clearing the selection instead of setting it. No exception is
   thrown anywhere along that path, so the only symptom is an empty-looking
   dropdown. This turns it into a console warning.

   Pre:  none — an empty or nil `options` is legitimate (decks not yet fetched,
         no referenceable documents) and is not reported.
   Post: one warning naming the first offending option; returns nil."
  [options]
  (when-some [bad (first (remove #(and (map? %) (some? (:id %))) options))]
    (log/log-warn
      (str "Typeahead: options must be {:id :label} maps with a non-nil :id; got "
        (pr-str bad) " — rows will render blank and selection will write nil.")))
  nil)

(e/defn OptionRows
  "The virtualized dropdown shared by `Typeahead` and `Suggest`: absolutely
   positioned under the input, one row per label, `active-idx` highlighted,
   `on-pick!` invoked with the row index on mousedown.

   labels     — vec of display strings
   active-idx — index of the keyboard-active row, or -1 for none
   on-pick!   — client-side fn of the row index; mousedown is preventDefault'd
                first so the input keeps focus

   Pre:  labels is non-empty; every element is a string.
   Post: only the visible window of rows is mounted (Scroll-window/Tape), so
         cost is bounded regardless of option count."
  [labels active-idx on-pick!]
  (e/client
    (let [n (count labels)]
      (dom/div
        (dom/props {:class "tape-scroll"
                    :style {:position "absolute" :top "100%" :left "0" :right "0"
                            :background "var(--color-bg-card)" :border "1px solid var(--color-border)"
                            :border-radius "var(--radius-sm)" :z-index "100"
                            :max-height "240px" :overflow-y "auto"
                            :box-shadow "0 2px 4px rgba(0,0,0,0.15)"
                            :--row-height (str row-height "px")}})
        (let [[offset limit] (Scroll-window row-height n dom/node {:overquery-factor 2})]
          (dom/props {:style {:--count n :--grid-cols "1fr"}})
          ;; Keep the keyboard-active row on screen (case forces the call —
          ;; a bare unused-value statement would be work-skipped).
          (case (scroll-row-into-view! dom/node active-idx row-height) nil)
          (dom/table
            (dom/props {:style {:width "100%"}})
            (e/for [i (Tape offset limit)]
              (let [label (nth labels i nil)]
                (when label
                  (dom/tr
                    (dom/props {:style {:--order i}})
                    (dom/td
                      (dom/props {:title label
                                  :style {:padding "5px 8px" :cursor "pointer" :font-size "14px"
                                          :white-space "nowrap" :overflow "hidden" :text-overflow "ellipsis"
                                          :background (when (= i active-idx) "var(--color-highlight)")}})
                      (dom/text label)
                      (dom/On "mousedown"
                        (fn [e]
                          (.preventDefault e)
                          (on-pick! i))
                        nil))))))))))))

(e/defn Typeahead
  "Combobox over identified options: filtered dropdown, writes the selected
   option's ID to !atom.

   Keyed on id, not on the visible label. Keying on the label made every consumer
   resolve its selection by string-matching titles back to rows, which silently
   picked the wrong row whenever two rows shared a label.

   options      — seq of {:id :label}; :label is filtered and displayed, :id is
                  what gets written. When the domain has no id distinct from the
                  label (Anki deck names), pass the label as the id.
   placeholder  — input placeholder text
   ?!committed  — optional atom; reset to the selected :id only on definitive
                  selection (mousedown or Enter), not per keystroke. nil to disable.
   autofocus?   — when truthy, focus the input on mount (skipped on touch,
                  where autofocus would pop the on-screen keyboard)

   Pre:  every option has a non-nil :id; !atom holds an :id present in `options`,
         or nil for no selection.
   Post: !atom holds an option's :id or nil — never free text. Typing filters
         without selecting; emptying the field and leaving clears the selection;
         any other uncommitted text is discarded rather than guessed at.
   Invariant: an :id in !atom that is absent from `options` displays as blank
         rather than as a stale label.

   A field that must accept text absent from `options` is a different widget —
   use `Suggest`, not a mode flag on this one."
  [!atom options placeholder ?!committed autofocus?]
  (e/client
    (let [value (e/watch !atom)
          selected-label (some (fn [o] (when (= (:id o) value) (:label o))) options)
          !search (atom nil)
          search (e/watch !search)
          !open? (atom false)
          open? (e/watch !open?)
          !active-idx (atom -1)
          active-idx (e/watch !active-idx)
          ;; Touch: skip autofocus — it would pop the on-screen keyboard; the
          ;; learner taps the field when they actually mean to type.
          coarse? (e/watch viewport/!coarse?)
          ;; Dropdown contents while open: all options when not typing (browse),
          ;; filtered when typing. Scrolls (max-height below). Keyed on open?,
          ;; not on search, so focus keeps the value visible without clearing it.
          filtered (when open?
                     (if (some? search)
                       (vec (filter #(string/includes? (string/lower-case (str (:label %)))
                                       (string/lower-case search))
                              options))
                       (vec options)))]
      (dom/div
        (dom/props {:style {:position "relative"}})
        ;; Dev guard for the option shape — case forces the call.
        (case (check-options! options) nil)
        (dom/input
          (dom/props {:type "text" :dir "auto"
                      :value (if (some? search) search (or selected-label ""))
                      :placeholder placeholder
                      :class "input"
                      :style {:width "100%"}})
          ;; rAF focus-on-mount: HTML :autofocus is inert on dynamically
          ;; inserted elements (see modal-shell/focus-on-mount!, which
          ;; nil-guards — the touch gate rides inside the snapshot).
          (e/snapshot (modal/focus-on-mount! (when (and autofocus? (not coarse?)) dom/node)))
          ;; Focus opens the dropdown but keeps the value — search stays nil so
          ;; the input shows the current value; Tab-through no longer loses it.
          (dom/On "focus" (fn [_] (reset! !open? true)
                            (reset! !active-idx -1)) nil)
          ;; Emptying the field and leaving is how the selection is cleared. Other
          ;; uncommitted text is dropped: the atom holds ids, so half-typed labels
          ;; have nothing to write, and guessing a match is what the id-keying
          ;; exists to prevent.
          (dom/On "blur" (fn [_] (when (= "" @!search) (reset! !atom nil))
                           (reset! !open? false)
                           (reset! !search nil)
                           (reset! !active-idx -1)) nil)
          ;; Typing filters only — the selection changes on definitive commit, so
          ;; consumers reacting to !atom (URL sync, server queries) see one change
          ;; per selection instead of one per keystroke.
          (let [v (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
            (when (some? v)
              (reset! !search v)
              (reset! !active-idx -1)))
          (dom/On "keydown"
            (fn [e]
              ;; Arrow keys guard on (pos? n): the dropdown can be closed while
              ;; the input has focus, and (mod _ 0) is NaN — which would stick
              ;; in !active-idx and break every later comparison.
              (let [key (.-key e)
                    n (count filtered)]
                (cond
                  (and (= key "ArrowDown") (pos? n))
                  (do (.preventDefault e)
                    (reset! !active-idx (mod (inc active-idx) n)))
                  (and (= key "ArrowUp") (pos? n))
                  (do (.preventDefault e)
                    (reset! !active-idx (mod (dec active-idx) n)))
                  (and (= key "Enter") (>= active-idx 0))
                  (do (.preventDefault e)
                    (let [id (:id (nth filtered active-idx))]
                      (reset! !atom id)
                      (when ?!committed (reset! ?!committed id)))
                    (reset! !search nil)
                    (reset! !open? false)
                    (reset! !active-idx -1))
                  (= key "Escape")
                  (do (reset! !search nil)
                    (reset! !open? false)
                    (reset! !active-idx -1)))))
            nil))
        ;; Mouse hover highlight is CSS (.tape-scroll table tr:hover td); the
        ;; keyboard-active row gets an inline background (OptionRows).
        (when (seq filtered)
          (OptionRows (mapv #(str (:label %)) filtered) active-idx
            (fn [i]
              (let [id (:id (nth filtered i))]
                (reset! !atom id)
                (when ?!committed (reset! ?!committed id)))
              (reset! !search nil)
              (reset! !open? false)
              (reset! !active-idx -1))))))))

(e/defn Suggest
  "Free-text input with a filtered suggestion dropdown.

   Unlike `Typeahead`, the typed text IS the value: every keystroke writes to
   !atom, and `suggestions` is a convenience list, not the domain of legal
   values. Use this whenever the user must be able to submit text that is not
   in the list — a pre-prompt, a note, a new tag.

   !atom        — holds the field's text; typing and picking both write it
   suggestions  — seq of strings offered below the input, filtered by the
                  current text (all of them while the field is empty)
   placeholder  — input placeholder text
   autofocus?   — when truthy, focus the input on mount (skipped on touch)

   Pre:  !atom holds a string; \"\" for empty.
   Post: !atom holds exactly what the user typed or picked.
   Invariant: text absent from `suggestions` is preserved, never discarded or
         snapped to a near match."
  [!atom suggestions placeholder autofocus?]
  (e/client
    (let [value (e/watch !atom)
          !open? (atom false)
          open? (e/watch !open?)
          !active-idx (atom -1)
          active-idx (e/watch !active-idx)
          coarse? (e/watch viewport/!coarse?)
          ;; Filtered by the field's own text — there is no separate search
          ;; state, because the text is the value.
          filtered (when open?
                     (vec (filter #(string/includes? (string/lower-case (str %))
                                     (string/lower-case (str value)))
                            suggestions)))]
      (dom/div
        (dom/props {:style {:position "relative"}})
        (dom/input
          (dom/props {:type "text" :dir "auto"
                      :value (or value "")
                      :placeholder placeholder
                      :class "input"
                      :style {:width "100%"}})
          (e/snapshot (modal/focus-on-mount! (when (and autofocus? (not coarse?)) dom/node)))
          (dom/On "focus" (fn [_] (reset! !open? true)
                            (reset! !active-idx -1)) nil)
          ;; Blur closes the dropdown and nothing else — the text is the value,
          ;; so there is nothing uncommitted to discard.
          (dom/On "blur" (fn [_] (reset! !open? false)
                           (reset! !active-idx -1)) nil)
          (let [v (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
            (when (some? v)
              (reset! !atom v)
              (reset! !open? true)
              (reset! !active-idx -1)))
          (dom/On "keydown"
            (fn [e]
              (let [key (.-key e)
                    n (count filtered)]
                (cond
                  (and (= key "ArrowDown") (pos? n))
                  (do (.preventDefault e)
                    (reset! !active-idx (mod (inc active-idx) n)))
                  (and (= key "ArrowUp") (pos? n))
                  (do (.preventDefault e)
                    (reset! !active-idx (mod (dec active-idx) n)))
                  (and (= key "Enter") (>= active-idx 0))
                  (do (.preventDefault e)
                    (reset! !atom (nth filtered active-idx))
                    (reset! !open? false)
                    (reset! !active-idx -1))
                  (= key "Escape")
                  (do (reset! !open? false)
                    (reset! !active-idx -1)))))
            nil))
        (when (seq filtered)
          (OptionRows (mapv str filtered) active-idx
            (fn [i]
              (reset! !atom (nth filtered i))
              (reset! !open? false)
              (reset! !active-idx -1))))))))
