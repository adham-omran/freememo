(ns freememo.search-result-row
  "Own the native cells and listeners inside one Electric result row."
  (:require [freememo.a11y :as a11y]
            [freememo.context-menu-surface :as menu]
            [freememo.math :as math]
            [freememo.navigation :as nav]
            [freememo.tooltip :as tooltip]))

(defn target [row]
  (menu/topic-row-target
    {:topic-id (:id row) :title (or (:title row) "") :topic-kind (:kind row)
     :root? (nil? (:parent-id row)) :deletable? (not= "page" (:kind row))}
    :search))

#?(:cljs
   (defn- element! [document parent tag class-name]
     (let [node (.createElement document tag)]
       (set! (.-className node) class-name)
       (.appendChild parent node)
       node)))

(defn mount! [node after-render]
  #?(:cljs
     (let [document (.-ownerDocument node)
           document-cell (element! document node "td" "search-result-document")
           badge (element! document document-cell "span" "type-badge")
           title (element! document document-cell "span" "search-result-title")
           source-cell (element! document node "td" "search-result-source")
           source (element! document source-cell "span" "search-result-source-label")
           snippet-cell (element! document node "td" "search-result-snippet")
           snippet (element! document snippet-cell "div" "search-result-snippet-text")
           !state (atom {:alive? true :visible? false})
           open! (fn [_]
                   (let [{:keys [row navigate! visible? alive?]} @!state]
                     (when (and alive? visible? row)
                       (navigate! :viewer (nav/nav-topic (:id row) :search)))))
           guard! (fn [event]
                    (when-not (and (:alive? @!state) (:visible? @!state))
                      (.preventDefault event)
                      (.stopImmediatePropagation event)))
           listeners [[node "click" guard! true]
                      [node "keydown" guard! true]
                      [node "contextmenu" guard! true]
                      [node "focusin" guard! true]
                      [node "click" open! false]
                      [document-cell "keydown"
                       (fn [event]
                         (when (a11y/focused-enter-or-space? event)
                           (.preventDefault event)
                           (open! event))) false]
                      [node "contextmenu" (fn [event] (menu/open-menu! event (:target @!state))) false]
                      [node "focusin" (fn [_] (menu/publish-focus-target! node (:target @!state))) false]
                      [node "focusout" (fn [event] (menu/focus-left! node event)) false]]]
       (set! (.-className node) "search-result-row")
       (.setAttribute document-cell "tabindex" "0")
       (.setAttribute snippet "dir" "auto")
       (doseq [[owner event listener capture?] listeners]
         (.addEventListener owner event listener capture?))
       {:node node :document-cell document-cell :badge badge :title title :source source
        :snippet snippet :after-render after-render :state !state :listeners listeners})
     :clj nil))

(defn update! [owner row index visible? badge navigate!]
  #?(:cljs
     (let [{:keys [node document-cell title source snippet after-render state]} owner
           previous @state
           current-target (target row)
           html (:snippet row)]
       (reset! state (assoc previous :row row :navigate! navigate!
                       :target current-target :visible? visible?))
       (set! (.-hidden node) (not visible?))
       (set! (.-inert node) (not visible?))
       (if visible?
         (let [[badge-text badge-class] badge
               title-text (or (:title row) "(untitled)")
               source-text (or (:source-title row) "")]
           (.setProperty (.-style node) "--order" (str index))
           (.setAttribute document-cell "aria-label" (str "Open " (or (:title row) "untitled result")))
           (set! (.-className (:badge owner)) (str "type-badge " badge-class))
           (set! (.-textContent (:badge owner)) badge-text)
           (set! (.-textContent title) title-text)
           (tooltip/set-tooltip! title title-text)
           (set! (.-textContent source) source-text)
           (tooltip/set-tooltip! source source-text)
           (when (or (not (:rendered? previous)) (not= html (:html previous)))
             (swap! state assoc :rendered? true :html html)
             (math/set-html! snippet html
               #(when (:alive? @state) (after-render snippet))))
           (when (.contains node (.-activeElement js/document))
             (menu/publish-focus-target! node current-target)))
         (do
           (when (.contains node (.-activeElement js/document))
             (.blur (.-activeElement js/document)))
           (menu/retract-focus-target! node)))
       nil)
     :clj nil))

(defn dispose! [owner]
  #?(:cljs
     (let [{:keys [node state listeners]} owner]
       (reset! state {:alive? false :visible? false})
       (doseq [[owner event listener capture?] listeners]
         (.removeEventListener owner event listener capture?))
       (menu/retract-focus-target! node)
       (.replaceChildren node))
     :clj nil))
