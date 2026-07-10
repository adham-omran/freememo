(ns freememo.typeahead
  "Shared UI components."
  (:require
   [clojure.string :as string]
   [contrib.data :refer [clamp-left]]
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

(e/defn Typeahead
  "Text input with filtered dropdown. Writes selected/typed value to !atom.
   options      — seq of strings to filter
   placeholder  — input placeholder text
   ?!committed  — optional atom; reset to selected item only on definitive selection
                  (mousedown or Enter), not on every keystroke. Pass nil to disable.
   autofocus?   — when truthy, focus the input on mount."
  [!atom options placeholder ?!committed autofocus?]
  (e/client
    (let [value (e/watch !atom)
          !search (atom nil)
          search (e/watch !search)
          !open? (atom false)
          open? (e/watch !open?)
          !active-idx (atom -1)
          active-idx (e/watch !active-idx)
          ;; Dropdown contents while open: all options when not typing (browse),
          ;; filtered when typing. Scrolls (max-height below). Keyed on open?,
          ;; not on search, so focus keeps the value visible without clearing it.
          filtered (when open?
                     (if (some? search)
                       (vec (filter #(string/includes? (string/lower-case %)
                                       (string/lower-case search))
                              options))
                       (vec options)))]
      (dom/div
        (dom/props {:style {:position "relative"}})
        (dom/input
          (dom/props {:type "text" :dir "auto"
                      :value (if (some? search) search (or value ""))
                      :placeholder placeholder
                      :class "input"
                      :style {:width "100%"}})
          (let [focus-node (when autofocus? dom/node)]
            (when focus-node
              (js/setTimeout (fn [] (.focus focus-node)) 50)))
          ;; Focus opens the dropdown but keeps the value — search stays nil so
          ;; the input shows the current value; Tab-through no longer loses it.
          (dom/On "focus" (fn [_] (reset! !open? true)
                            (reset! !active-idx -1)) nil)
          (dom/On "blur" (fn [_] (reset! !open? false)
                           (reset! !search nil)
                           (reset! !active-idx -1)) nil)
          (let [v (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
            (when (some? v)
              (reset! !search v)
              (reset! !atom v)
              (reset! !active-idx -1)))
          (dom/On "keydown"
            (fn [e]
              (let [key (.-key e)
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
                    (let [selected (nth filtered active-idx)]
                      (reset! !atom selected)
                      (when ?!committed (reset! ?!committed selected)))
                    (reset! !search nil)
                    (reset! !open? false)
                    (reset! !active-idx -1))
                  (= key "Escape")
                  (do (reset! !search nil)
                    (reset! !open? false)
                    (reset! !active-idx -1)))))
            nil))
        ;; Virtualized dropdown: only the visible window of rows renders
        ;; (Scroll-window/Tape), so cost is bounded regardless of option count.
        ;; Mouse hover highlight is CSS (.tape-scroll table tr:hover td); the
        ;; keyboard-active row gets an inline background.
        (when (seq filtered)
          (let [n (count filtered)]
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
                    (let [item (nth filtered i nil)]
                      (when item
                        (dom/tr
                          (dom/props {:style {:--order i}})
                          (dom/td
                            (dom/props {:title item
                                        :style {:padding "5px 8px" :cursor "pointer" :font-size "14px"
                                                :white-space "nowrap" :overflow "hidden" :text-overflow "ellipsis"
                                                :background (when (= i active-idx) "var(--color-highlight)")}})
                            (dom/text item)
                            (dom/On "mousedown"
                              (fn [e]
                                (.preventDefault e)
                                (reset! !atom item)
                                (when ?!committed (reset! ?!committed item))
                                (reset! !search nil)
                                (reset! !open? false)
                                (reset! !active-idx -1))
                              nil)))))))))))))))
