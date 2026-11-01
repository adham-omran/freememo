(ns freememo.nav-item-dom
  "Own the display children of navigation items. Electric keeps their handlers."
  (:require [freememo.icon-dom :as icons]))

#?(:cljs
   (do
     (defonce ^:private views (js/WeakMap.))

     (defn- element! [parent tag class-name]
       (let [node (.createElement (.-ownerDocument parent) tag)]
         (when class-name (.setAttribute node "class" class-name))
         (.appendChild parent node)
         node))

     (defn- attribute! [node key value]
       (if (some? value)
         (let [value (str value)]
           (when (not= value (.getAttribute node key)) (.setAttribute node key value)))
         (when (.hasAttribute node key) (.removeAttribute node key))))

     (defn- text! [node value]
       (let [value (str (or value ""))]
         (when (not= value (.-nodeValue node)) (set! (.-nodeValue node) value))))

     (defn- caption! [node value]
       (let [value (str (or value ""))]
         (when (not= value (.-textContent node)) (set! (.-textContent node) value))))))

(defn icon! [node glyph size]
  #?(:cljs
     (let [view (or (.get views node)
                    (let [svg (.createElementNS (.-ownerDocument node) "http://www.w3.org/2000/svg" "svg")]
                      (.appendChild node svg)
                      {:svg svg}))]
       (attribute! node "class" "nav-rail__icon")
       (when (not= [glyph size] (:glyph view))
         (icons/install! (:svg view) glyph size nil nil)
         (.set views node (assoc view :glyph [glyph size])))
       nil)
     :clj nil))

(defn label! [node text options]
  #?(:cljs
     (let [{:keys [subtext shortcut]} options
           view (or (.get views node)
                    (let [primary (element! node "span" nil)
                          text (.createTextNode (.-ownerDocument node) "")]
                      (.appendChild primary text)
                      {:primary primary :text text}))
           shortcut-node (or (:shortcut view) (when shortcut (element! (:primary view) "kbd" nil)))
           sub-node (or (:sub view) (when (some? subtext) (element! node "span" "nav-rail__sub")))]
       (attribute! node "class" "nav-rail__label")
       (attribute! (:primary view) "class" (when shortcut "nav-rail__keep-row"))
       (text! (:text view) text)
       (when shortcut-node
         (caption! shortcut-node shortcut)
         (set! (.. shortcut-node -style -display) (if shortcut "" "none")))
       (when sub-node
         (caption! sub-node subtext)
         (set! (.. sub-node -style -display) (if (some? subtext) "" "none")))
       (.set views node (assoc view :shortcut shortcut-node :sub sub-node))
       nil)
     :clj nil))

(defn item! [node label glyph tour active? extra-class]
  #?(:cljs
     (let [view (or (.get views node)
                    {:icon (element! node "span" "nav-rail__icon")
                     :label (element! node "span" "nav-rail__label")})]
       (.set views node view)
       (attribute! node "class" (str "nav-rail__item"
                                 (when extra-class (str " " extra-class))
                                 (when active? " is-active")))
       (attribute! node "aria-current" (when active? "page"))
       (attribute! node "data-tour" tour)
       (icon! (:icon view) glyph 18)
       (label! (:label view) label nil)
       nil)
     :clj nil))

(defn due! [node value class-name]
  #?(:cljs
     (let [view (or (.get views node)
                    (let [text (.createTextNode (.-ownerDocument node) "")]
                      (.appendChild node text)
                      (caption! (element! node "span" "nav-rail__sr") " due")
                      {:text text}))]
       (.set views node view)
       (attribute! node "class" class-name)
       (text! (:text view) value)
       nil)
     :clj nil))
