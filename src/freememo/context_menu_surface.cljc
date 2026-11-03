(ns freememo.context-menu-surface
  "What an Electric surface calls to take part in the context menu
   (plans/custom-context-menu.md §4).

   The menu itself is freememo.context-menu, a .cljs namespace. An e/defn body
   cannot name a CLJS-only var (docs/electric-patterns.md, \"Server-Only Functions in .cljc
   Files\", which holds in the other direction too), so every call goes through
   a plain defn here whose body is reader-conditional.

   Three jobs:

   1. OPEN the menu from a row's own listener: `open-menu!` from a
      contextmenu callback, `open-menu-below!` from a ⋯ button.
   2. Publish the FOCUS TARGET, so the palette and the keyboard act on the
      focused row: `publish-focus-target!` on focusin, `focus-left!` on
      focusout.
   3. Lend FACTS to targets built inside a DOM subtree: `provide-facts!` on an
      ancestor, `facts-for` from a node inside it. A Quill editor is plain JS
      and knows no topic. The card modal around a card field knows the card's
      topic, so it lends it, and the field's image rows can pin to it."
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [freememo.command-bus :as bus]
            #?(:cljs [freememo.context-menu :as cm])))

(def keeps-focus-target-attr
  "An element carrying this attribute does not clear the focus target when
   focus moves into it. The command palette carries it: opening the palette
   moves focus there, and the palette must still see the row it was opened
   over."
  "data-keeps-focus-target")

(defn open-menu!
  "Open the menu for `target` from contextmenu event `e`.
   Post: true when it opened. A Shift+right-click, a nil target or a target no
         command applies to leaves the browser's menu (false)."
  ([e target] (open-menu! e target nil))
  ([e target keyboard-anchor]
   #?(:cljs (cm/open-from-event! e target keyboard-anchor)
      :clj false)))

(defn open-menu-below!
  "Open the menu for `target` under `node`, as a ⋯ button does.
   Pre : `node` is connected. Post: as context-menu/open!."
  [node target]
  #?(:cljs (let [r (.getBoundingClientRect node)]
             (cm/open! {:left (.-left r) :top (.-top r) :right (.-right r) :bottom (.-bottom r)}
               target))
     :clj false))

(defn publish-focus-target!
  "Make `target` the focus target, owned by `owner` (the row's node).
   Returns nil."
  [owner target]
  (bus/publish-focus-target! owner target))

(defn focus-left!
  "On a focusout from `node`: clear the focus target `owner` published,
   unless focus moved within `node` or into an element that keeps it.
   Returns nil.
   Pre : `e` is the focusout event. `owner` defaults to `node`."
  ([node e] (focus-left! node node e))
  ([owner node e]
   #?(:cljs (let [^js to (.-relatedTarget e)]
              (when-not (and to (or (.contains node to)
                                    (.closest to (str "[" keeps-focus-target-attr "]"))))
                (bus/retract-focus-target! owner)))
      :clj nil)
   nil))

(defn retract-focus-target!
  "Clear the focus target `owner` holds. Call from e/on-unmount. Returns nil."
  [owner]
  (bus/retract-focus-target! owner))

;; ---------------------------------------------------------------------------
;; A row
;; ---------------------------------------------------------------------------

(e/defn RowMenu
  "Make the current DOM node a context-menu row for `target`: a right-click
   opens the menu, and focus inside the row makes `target` the focus target.
   Call it inside the row element, before its cells.
   Post: on unmount the row's focus target is gone."
  [target]
  (e/client
    (let [row dom/node]
      (dom/On "contextmenu" (fn [e] (open-menu! e target)) nil)
      (dom/On "focusin" (fn [_] (publish-focus-target! row target)) nil)
      (dom/On "focusout" (fn [e] (focus-left! row e)) nil)
      (e/on-unmount #(retract-focus-target! row)))))

(defn topic-row-target
  "The target of a topic row: one subject, opened from `origin` (a nav origin
   such as :library or :search)."
  [subject origin]
  {:kind :topic :subjects [subject] :surface {:origin origin}})

;; ---------------------------------------------------------------------------
;; Facts lent by an ancestor
;; ---------------------------------------------------------------------------

#?(:cljs (defonce ^:private providers (js/WeakMap.)))

(defn provide-facts!
  "Lend `facts` (a plain map) to every target built under `node`. Calling it
   again replaces the map, so a reactive caller keeps it current.
   Returns nil."
  [node facts]
  #?(:cljs (when node (.set providers node facts)))
  nil)

(defn facts-for
  "The facts the nearest providing ancestor of `node` (or `node` itself)
   lends, or nil."
  [node]
  #?(:cljs (loop [^js n node]
             (cond
               (nil? n) nil
               (.has providers n) (.get providers n)
               :else (recur (.-parentElement n))))
     :clj nil))
