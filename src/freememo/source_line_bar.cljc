(ns freememo.source-line-bar
  "The Source Document line, pinned between the toolbars and the body of the
   Topic View (plans/topic-view-source-line.md). It shows
   \"Source Document › topic title\" on one line, and each part ends with \"…\"
   when it does not fit. A tap opens the line to show the full text, and a second
   tap closes it. The tap does not navigate. On a desktop the tooltip also shows
   the full text. `freememo.source-line` decides the parts."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.source-line :as source-line]
   [freememo.tooltip :as tooltip]))

(defn- line-class
  "The class string of the line. Post: one string, so it is one prop."
  [open? title?]
  (str "source-line"
    (when open? " source-line--open")
    (when-not title? " source-line--no-title")))

(e/defn SourceLineBar []
  (e/client
    (let [line (or dctx/source-line {:source-document "" :title nil})
          doc (or (:source-document line) "")
          title (:title line)
          full (source-line/full-text {:source-document doc :title title})
          ;; Holds the full text that the user opened. A new topic has a new
          ;; text, so the line starts closed again with no reset effect.
          !opened (atom nil)
          open? (= full (e/watch !opened))]
      (dom/button
        (dom/props {:type "button"
                    :class (line-class open? (some? title))
                    :aria-expanded (str open?)
                    :aria-label (str "Source: " full)})
        (tooltip/Tooltip! full)
        (dom/On "click" (fn [_] (swap! !opened #(when-not (= % full) full))) nil)
        (dom/span (dom/props {:class "source-line__doc"}) (dom/text doc))
        (dom/span (dom/props {:class "source-line__sep" :aria-hidden "true"}) (dom/text "›"))
        (dom/span (dom/props {:class "source-line__title"}) (dom/text (or title "")))))))
