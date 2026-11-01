(ns freememo.assistant-reference-choice
  "Stable document choices from rendered mention options."
  (:require [freememo.assistant-panel-data :as data]))

(defn capture [document]
  {:request-id (str (random-uuid))
   :id (:id document) :title (:title document) :query (:mention-query document)})

(defn for-draft [choice draft]
  (when (and (integer? (:id choice)) (pos? (:id choice))
             (string? (:title choice)) (string? (:query choice))
             (= (:query choice) (data/mention-query draft)))
    (select-keys choice [:id :title])))

(defn pick-selected! [input draft]
  #?(:cljs
     (when-let [shell (.closest input ".assistant-panel__composer-shell")]
       (when-let [option (.querySelector shell
                           ".assistant-panel__mention-menu[aria-hidden='false'] .assistant-panel__mention-option[aria-selected='true']")]
         (when (= (data/mention-query draft) (.getAttribute option "data-mention-query"))
           (.dispatchEvent option
             (js/MouseEvent. "mousedown" #js {:bubbles true :cancelable true})))))
     :clj nil))

(defn keep-selected-visible! [node selected-index row-height]
  #?(:cljs
     (let [height (.-clientHeight node)
           current (.-scrollTop node)
           top (* selected-index row-height)
           bottom (+ top row-height)]
       (when (pos? height)
         (cond
           (< top current) (set! (.-scrollTop node) top)
           (> bottom (+ current height)) (set! (.-scrollTop node) (- bottom height)))))
     :clj nil))
