(ns freememo.row-menu
  "The overflow menu of a table row (plans/knowledge-tables-selection-and-bulk.md §2).

   One `RowMenu` per view, mounted OUTSIDE the scroll box. Rows carry a
   `transform` and sit in an `overflow` container, so a menu inside a row is
   clipped at the last visible row — the reason `knowledge_known/RowMore` once
   opened its items in the row instead. Each row's `MenuButton` writes the
   view's `!menu` atom, and the one menu places itself beside that button with
   the vendored Floating UI.

   The atom holds the button's DOM node. It is client state only: no value read
   from it may reach an `e/server` form except plain ids.

   A menu item never runs a server effect itself. Picking an item closes the
   menu, which unmounts the item, and an effect inside it would be torn down
   mid-flight. Items write the view's action atom instead, and the view runs it
   from a frame that outlives the menu."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   #?(:cljs [goog.object :as gobj])
   #?(:cljs [freememo.vendor-libs :as vendor])))

#?(:cljs
   (defn- menu-items [^js menu]
     (vec (array-seq (.querySelectorAll menu "[role=menuitem]")))))

#?(:cljs
   (defn- focus-item!
     "Focus item `i` of `menu`, wrapping at both ends."
     [^js menu i]
     (let [items (menu-items menu)
           n (count items)]
       (when (pos? n)
         (.focus ^js (nth items (mod i n)) #js {:preventScroll true})))))

#?(:cljs
   (defn- focused-index [^js menu]
     (let [items (menu-items menu)]
       (or (first (keep-indexed (fn [i el] (when (identical? el (.-activeElement js/document)) i))
                    items))
           -1))))

#?(:cljs
   (defn- place-below!
     "Fallback placement when Floating UI does not load: under the anchor,
      right edges aligned."
     [^js menu ^js anchor]
     (let [r (.getBoundingClientRect anchor)]
       (set! (.. menu -style -top) (str (+ (.-bottom r) 4) "px"))
       (set! (.. menu -style -left)
         (str (max 8 (- (.-right r) (.-offsetWidth menu))) "px"))
       (.setAttribute menu "data-placed" "true"))))

#?(:cljs
   (defn attach!
     "Place `menu` beside `anchor` and wire the ways it closes.

      Pre:  `menu` and `anchor` are mounted; `close!` empties the view's menu atom.
      Post: the menu is placed below the anchor (above when there is no room)
            and its first item has focus. Escape closes and returns focus to
            the anchor; Tab, a pointer press outside menu and anchor, and any
            scroll outside the menu close it. Arrow keys, Home and End move
            between items. Returns a fn that removes every listener."
     [^js menu ^js anchor close!]
     (let [!alive? (atom true)
           !stop (atom nil)
           on-key (fn [^js e]
                    (case (.-key e)
                      "Escape" (do (.preventDefault e)
                                 (close!)
                                 (.focus anchor #js {:preventScroll true}))
                      "ArrowDown" (do (.preventDefault e) (focus-item! menu (inc (focused-index menu))))
                      "ArrowUp" (do (.preventDefault e) (focus-item! menu (dec (focused-index menu))))
                      "Home" (do (.preventDefault e) (focus-item! menu 0))
                      "End" (do (.preventDefault e) (focus-item! menu -1))
                      "Tab" (close!)
                      nil))
           on-press (fn [^js e]
                      (let [t (.-target e)]
                        (when-not (or (.contains menu t) (.contains anchor t))
                          (close!))))
           on-scroll (fn [^js e]
                       (when-not (.contains menu (.-target e))
                         (close!)))
           placed! (fn []
                     (when @!alive?
                       (.setAttribute menu "data-placed" "true")
                       (focus-item! menu 0)))]
       (.addEventListener menu "keydown" on-key)
       (.addEventListener js/document "pointerdown" on-press true)
       (.addEventListener js/document "scroll" on-scroll true)
       (-> (vendor/ensure! :floating-ui)
         (.then (fn [_]
                  (when @!alive?
                    (let [^js F (gobj/get js/window "FloatingUIDOM")
                          place (fn []
                                  (-> (.computePosition F anchor menu
                                        #js {:strategy "fixed"
                                             :placement "bottom-end"
                                             :middleware #js [(.offset F 4)
                                                              (.flip F)
                                                              (.shift F #js {:padding 8})]})
                                    (.then (fn [^js p]
                                             (set! (.. menu -style -left) (str (.-x p) "px"))
                                             (set! (.. menu -style -top) (str (.-y p) "px"))))))]
                      (-> (place) (.then placed!))
                      (reset! !stop (.autoUpdate F anchor menu place))))))
         (.catch (fn [_]
                   (when @!alive?
                     (place-below! menu anchor)
                     (focus-item! menu 0)))))
       (fn detach! []
         (reset! !alive? false)
         (.removeEventListener menu "keydown" on-key)
         (.removeEventListener js/document "pointerdown" on-press true)
         (.removeEventListener js/document "scroll" on-scroll true)
         (when-let [stop! @!stop] (stop!))))))

#?(:clj (defn attach! [_menu _anchor _close!] (fn [] nil)))

(e/defn MenuButton
  "The ⋯ button of one row. A click opens the view's menu for this row, or
   closes it when it is already open for this row.
   Pre:  `open-row-key` is the row key of the open menu, or nil.
   Post: `!menu` holds {:key :row-key :label :row :anchor} or nil."
  [!menu open-row-key row-key label row]
  (e/client
    (dom/button
      (dom/props {:type "button" :class "btn btn-sm btn-secondary row-menu-button"
                  :aria-haspopup "menu" :aria-expanded (str (= open-row-key row-key))
                  :aria-label (str "More actions for " label)})
      (icons/Icon :ellipsis :size 14)
      (let [node dom/node]
        (dom/On "click"
          (fn [e]
            (.stopPropagation e)
            (swap! !menu (fn [m]
                           (when-not (= (:row-key m) row-key)
                             {:key (str (random-uuid)) :row-key row-key :label label
                              :row row :anchor node}))))
          nil)))))

(e/defn RowMenu
  "The view's one menu. Mounts while `menu` is non-nil, and once per opening,
   so a reopened menu is placed and focused again.
   Pre:  `Items` is an e/fn of [menu] mounting `MenuItem`s for `(:row menu)`."
  [!menu menu Items]
  (e/client
    (when (some? menu)
      (e/for-by identity [_k [(:key menu)]]
        (dom/div
          (dom/props {:class "row-menu" :role "menu"
                      :aria-label (str "Actions for " (:label menu))})
          (let [detach! (e/snapshot (attach! dom/node (:anchor menu) (fn [] (reset! !menu nil))))]
            (e/on-unmount detach!))
          (Items menu))))))

(e/defn MenuItem
  "One menu item. `pick!` is a plain fn; the menu closes after it runs."
  [!menu label danger? pick!]
  (e/client
    (dom/button
      (dom/props {:type "button" :role "menuitem" :tabindex "-1"
                  :class (if danger? "row-menu-item row-menu-item-danger" "row-menu-item")})
      (dom/text label)
      (dom/On "click" (fn [_] (pick!) (reset! !menu nil)) nil))))
