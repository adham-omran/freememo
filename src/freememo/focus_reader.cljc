(ns freememo.focus-reader
  "Shared focus-reader rules and control."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.command-bus :as bus]
   [freememo.doc-context :as dctx]
   [freememo.icons :as icons]
   [freememo.tooltip :as tooltip]))

(def text-topic-kinds
  "Topic kinds that use the standard editable text view."
  #{"basic" "epub" "web" "wikipedia" "markdown"})

(defn available?
  "True when the current view supports the focus reader."
  [kind is-pdf?]
  (or (and is-pdf? (not= kind "score"))
      (contains? text-topic-kinds kind)))

(e/defn FocusReaderToggle
  "Toggle the focus reader for a supported topic."
  [extra-class]
  (e/client
    (when dctx/focus-available?
      (let [focus? dctx/focus?
            set-focus! dctx/set-focus!
            label (if focus? "Exit focus reader" "Focus reader")
            tooltip-text (cond
                           focus? "Return to the standard view"
                           dctx/is-pdf? "Hide the PDF and center the text"
                           :else "Read the topic in a centered column")]
        (dom/div
          (dom/props {:class (str "toolbar-group focus-reader-toggle"
                               (when extra-class (str " " extra-class)))})
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"
                        :aria-label label
                        :aria-pressed (str (boolean focus?))
                        :style {:outline (when focus? "1px solid currentColor")}})
            (tooltip/Tooltip! tooltip-text)
            (icons/Icon :book-open :size 16)
            (dom/span (dom/props {:class "icon-label"}) (dom/text label))
            (let [node dom/node]
              (bus/publish-invoker! :toggle-focus-reader node (fn [] (.click node)))
              (e/on-unmount (fn [] (bus/retract-invoker! :toggle-focus-reader node))))
            (dom/On "click" (fn [_] (set-focus! (not focus?))) nil)))))))
