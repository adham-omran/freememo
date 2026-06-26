(ns freememo.typeahead
  "Shared UI components."
  (:require
   [clojure.string :as string]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   ))

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
        (when (seq filtered)
          (dom/div
            (dom/props {:style {:position "absolute" :top "100%" :left "0" :right "0"
                                :background "var(--color-bg-card)" :border "1px solid var(--color-border)"
                                :border-radius "var(--radius-sm)" :z-index "100"
                                :max-height "240px" :overflow-y "auto"
                                :box-shadow "0 2px 4px rgba(0,0,0,0.15)"}})
            (e/for [[i item] (e/diff-by {} (map-indexed vector filtered))]
              (dom/div
                (dom/props {:style {:padding "5px 8px" :cursor "pointer" :font-size "14px"
                                    :white-space "nowrap" :overflow "hidden" :text-overflow "ellipsis"
                                    :background (cond
                                                  (= i active-idx) "var(--color-highlight)"
                                                  (odd? i) "var(--color-bg-subtle)"
                                                  :else "var(--color-bg-card)")}})
                (dom/props {:title item})
                (dom/text item)
                (dom/On "mousemove" (fn [_] (reset! !active-idx i)) nil)
                (dom/On "mousedown"
                  (fn [e]
                    (.preventDefault e)
                    (reset! !atom item)
                    (when ?!committed (reset! ?!committed item))
                    (reset! !search nil)
                    (reset! !open? false)
                    (reset! !active-idx -1))
                  nil)))))))))
