(ns electric-starter-app.components
  "Shared UI components."
  (:require
   [clojure.string :as string]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(e/defn Typeahead
  "Text input with filtered dropdown. Writes selected/typed value to !atom.
   options     — seq of strings to filter
   placeholder — input placeholder text"
  [!atom options placeholder]
  (e/client
    (let [value    (e/watch !atom)
          !search  (atom nil)
          search   (e/watch !search)
          filtered (when (some? search)
                     (vec (filter #(string/includes? (string/lower-case %)
                                                     (string/lower-case search))
                                  options)))]
      (dom/div
        (dom/props {:style {:position "relative"}})
        (dom/input
          (dom/props {:type "text"
                      :value (if (some? search) search (or value ""))
                      :placeholder placeholder
                      :style {:padding "4px 8px" :border "1px solid #ccc" :border-radius "4px"
                              :font-size "15px" :width "100%" :box-sizing "border-box"}})
          (dom/On "focus" (fn [_] (reset! !search (or value ""))) nil)
          (dom/On "blur"  (fn [_] (reset! !search nil)) nil)
          (let [v (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
            (when (some? v)
              (reset! !search v)
              (reset! !atom v))))
        (when (seq filtered)
          (dom/div
            (dom/props {:style {:position "absolute" :top "100%" :left "0" :right "0"
                                :background "white" :border "1px solid #ccc"
                                :border-radius "4px" :z-index "100"
                                :box-shadow "0 2px 4px rgba(0,0,0,0.15)"
                                :max-height "200px" :overflow-y "auto"}})
            (e/for [item (e/diff-by {} filtered)]
              (dom/div
                (dom/props {:style {:padding "5px 8px" :cursor "pointer" :font-size "15px"}
                            :onmouseover "this.style.background='#f0f0f0'"
                            :onmouseout "this.style.background=''"})
                (dom/text item)
                (dom/On "mousedown"
                  (fn [e]
                    (.preventDefault e)
                    (reset! !atom item)
                    (reset! !search nil))
                  nil)))))))))
