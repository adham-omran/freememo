(ns freememo.context-menu
  "The one right-click menu (plans/custom-context-menu.md §3).

   A surface builds a TARGET (freememo.command-targets) for what was clicked
   and calls `open-from-event!` from its own contextmenu listener. The rows are
   the registry entries that apply to that target, so this ns holds no item
   list. A row dispatches its command through freememo.command-bus with the
   target as the payload, and the handler that runs is the command's.

   One menu exists at a time, on `document.body`, in the TOP LAYER through
   `popover=\"manual\"`. The top layer paints above every z-index, so the menu
   is above the card modals and the toast stack alike, and the card modals'
   `pointer-events: none` overlay cannot reach it: it is not their descendant.

   A command with `:menu-input` swaps the menu body for one field in the same
   element (§3.10). The bus asks for that field through
   `register-input-requester!`, so the palette gets the same field for the
   same command.

   Plain JS DOM, not Electric: every opener is a DOM listener, and the menu
   outlives no frame (CLAUDE.md, \"JS Library Init Side Effects in e/defn\")."
  (:require [clojure.string :as str]
            [freememo.command-bus :as bus]
            [freememo.command-targets :as targets]
            [freememo.commands :as commands]
            [freememo.editor-popovers :as editor-popovers]
            [freememo.menu-placement :as placement]))

;; {:el <div> :teardown (fn []) :opener <element> :anchor <anchor>} | nil
(defonce ^:private !menu (atom nil))

(defn open?
  "True while a menu is on screen."
  []
  (some? @!menu))

(defn- restore-focus! [^js opener]
  (when (and opener (.-isConnected opener) (fn? (.-focus opener)))
    (.focus opener #js {:preventScroll true})))

(defn close!
  "Remove the menu and every listener it installed. Idempotent.
   With `restore? true`, focus returns to the element that had it at open.
   Post: (open?) is false."
  ([] (close! false))
  ([restore?]
   (when-let [{:keys [^js el teardown opener]} @!menu]
     (reset! !menu nil)
     (teardown)
     (when (.-isConnected el)
       (when (and (fn? (.-hidePopover el)) (.matches el ":popover-open"))
         (.hidePopover el))
       (.remove el))
     (when restore? (restore-focus! opener)))
   nil))

(defonce ^:private unregister-popover!
  (editor-popovers/register! open? #(close! true)))

;; ---------------------------------------------------------------------------
;; DOM
;; ---------------------------------------------------------------------------

(defn- el [tag class]
  (let [n (js/document.createElement tag)]
    (when class (set! (.-className n) class))
    n))

(defn- viewport []
  {:width (.-clientWidth js/document.documentElement)
   :height (.-clientHeight js/document.documentElement)})

(defn- place! [^js menu anchor]
  (let [pos (placement/place anchor
              {:width (.-offsetWidth menu) :height (.-offsetHeight menu)}
              (viewport))]
    (set! (.. menu -style -left) (str (:left pos) "px"))
    (set! (.. menu -style -top) (str (:top pos) "px"))))

(defn- show! [^js menu]
  (.appendChild js/document.body menu)
  ;; Without the Popover API the menu is an ordinary fixed element, and the
  ;; z-index on .context-menu keeps it above the modals.
  (when (fn? (.-showPopover menu))
    (.setAttribute menu "popover" "manual")
    (.showPopover menu)))

(defn- items-of [^js menu]
  (vec (array-seq (.querySelectorAll menu "[role=menuitem]"))))

(defn- move-focus! [^js menu step]
  (let [items (items-of menu)
        n (count items)
        i (.indexOf (to-array items) js/document.activeElement)]
    (when (pos? n)
      (.focus (nth items (case step
                           :first 0
                           :last (dec n)
                           (mod (+ (if (neg? i) (if (pos? step) -1 0) i) step) n)))))))

;; ---------------------------------------------------------------------------
;; Input step (§3.10)
;; ---------------------------------------------------------------------------

(defn- parse-input
  "The value a field holds, or nil when it is not a value the command takes.
   Pre : `spec` is a registry :menu-input."
  [{:keys [type]} raw]
  (case type
    :text (let [v (str/trim raw)] (when-not (str/blank? v) v))
    :number (let [v (js/parseFloat raw)] (when-not (js/isNaN v) v))
    :confirm true))

(defn- input-body!
  "Replace `menu`'s children with the field for `command-id`.
   Post: submitting a valid value dispatches the command with it as :input and
         closes the menu. Escape and Cancel close without dispatching."
  [^js menu command-id target]
  (let [{:keys [type label prompt confirm-label value-key max-length min max]
         :as spec} (:menu-input (commands/command command-id))
        subject (first (:subjects target))
        form (el "form" "context-menu__form")
        field (when-not (= :confirm type) (el "input" "context-menu__input"))
        ok (el "button" "btn btn-sm btn-primary")
        cancel (el "button" "btn btn-sm btn-secondary")
        actions (el "div" "context-menu__actions")
        submit! (fn [^js e]
                  (.preventDefault e)
                  (if-some [v (parse-input spec (if field (.-value field) ""))]
                    (do (close! true)
                        (bus/dispatch! command-id (assoc target :input v) :menu))
                    (when field
                      (.setAttribute field "aria-invalid" "true")
                      (.focus field))))]
    (set! (.-textContent menu) "")
    (.setAttribute menu "role" "dialog")
    (.setAttribute menu "aria-label" (targets/menu-label (commands/command command-id)))
    (if field
      (let [lbl (el "label" "context-menu__label")]
        (set! (.-textContent lbl) label)
        (set! (.-type field) (if (= :number type) "number" "text"))
        (when (= :number type)
          (set! (.-step field) "any")
          (when min (set! (.-min field) (str min)))
          (when max (set! (.-max field) (str max))))
        (when max-length (set! (.-maxLength field) max-length))
        (set! (.-value field) (str (get subject value-key "")))
        (.appendChild lbl field)
        (.appendChild form lbl))
      (let [p (el "p" "context-menu__prompt")]
        (set! (.-textContent p) prompt)
        (.appendChild form p)))
    (set! (.-type ok) "submit")
    (set! (.-textContent ok) (or confirm-label "Save"))
    (when (= :confirm type) (set! (.-className ok) "btn btn-sm btn-danger-fill"))
    (set! (.-type cancel) "button")
    (set! (.-textContent cancel) "Cancel")
    (.addEventListener cancel "click" (fn [_] (close! true)))
    (.appendChild actions cancel)
    (.appendChild actions ok)
    (.appendChild form actions)
    (.addEventListener form "submit" submit!)
    (.appendChild menu form)
    (if field
      (do (.focus field) (.select field))
      (.focus ok))))

;; ---------------------------------------------------------------------------
;; Menu
;; ---------------------------------------------------------------------------

(defn- item-row! [^js menu target {:keys [id label danger? disabled-reason]}]
  (let [b (el "button" (str "context-menu__item" (when danger? " context-menu__item--danger")))]
    (set! (.-type b) "button")
    (.setAttribute b "role" "menuitem")
    (.setAttribute b "tabindex" "-1")
    (set! (.-textContent b) label)
    (when disabled-reason
      (.setAttribute b "aria-disabled" "true")
      (.setAttribute b "data-tooltip" disabled-reason))
    (.addEventListener b "click"
      (fn [^js e]
        (.stopPropagation e)
        (when-not disabled-reason
          (if (:menu-input (commands/command id))
            (bus/dispatch! id target :menu)          ; the requester swaps the body
            (do (close! true)
                (bus/dispatch! id target :menu))))))
    (.appendChild menu b)))

(defn- install-dismissal!
  "Close the menu on what ends a menu: a pointerdown, scroll or wheel outside
   it, a resize, the window losing focus, Escape and Tab.
   Returns the teardown fn."
  [^js menu]
  (let [outside? (fn [^js e] (not (.contains menu (.-target e))))
        on-down (fn [e] (when (outside? e) (close! false)))
        on-scroll (fn [e] (when (outside? e) (close! false)))
        on-resize (fn [_] (close! false))
        on-blur (fn [_] (close! false))
        on-key (fn [^js e]
                 (case (.-key e)
                   "Escape" (do (.preventDefault e) (.stopPropagation e) (close! true))
                   "Tab" (do (.preventDefault e) (close! true))
                   "ArrowDown" (do (.preventDefault e) (move-focus! menu 1))
                   "ArrowUp" (do (.preventDefault e) (move-focus! menu -1))
                   "Home" (do (.preventDefault e) (move-focus! menu :first))
                   "End" (do (.preventDefault e) (move-focus! menu :last))
                   nil))]
    (.addEventListener js/document "pointerdown" on-down true)
    (.addEventListener js/document "scroll" on-scroll true)
    (.addEventListener js/document "wheel" on-scroll #js {:capture true :passive true})
    (.addEventListener js/window "resize" on-resize)
    (.addEventListener js/window "blur" on-blur)
    (.addEventListener menu "keydown" on-key)
    (fn []
      (.removeEventListener js/document "pointerdown" on-down true)
      (.removeEventListener js/document "scroll" on-scroll true)
      (.removeEventListener js/document "wheel" on-scroll #js {:capture true :passive true})
      (.removeEventListener js/window "resize" on-resize)
      (.removeEventListener js/window "blur" on-blur)
      (.removeEventListener menu "keydown" on-key))))

(defn- mount!
  "Put a new, empty menu on screen at `anchor`. Returns the element.
   Post: any previous menu is gone; this one is in !menu with its dismissal."
  [anchor build!]
  (let [opener js/document.activeElement]
    (close! false)
    (let [menu (el "div" "context-menu")]
      (build! menu)
      (show! menu)
      (place! menu anchor)
      (reset! !menu {:el menu :opener opener :anchor anchor
                     :teardown (install-dismissal! menu)})
      menu)))

(defn has-items?
  "True when at least one command applies to `target`."
  [target]
  (boolean (seq (targets/menu-items commands/registry target))))

(defn open!
  "Open the menu for `target` at `anchor`.

   Pre : `anchor` is {:x :y} or {:left :top :right :bottom}, in client
         coordinates. `target` is a command-targets target.
   Post: true and one menu with focus on its first row when any command
         applies to `target`. Otherwise false, and nothing opens."
  [anchor target]
  (let [items (targets/menu-items commands/registry target)]
    (if (empty? items)
      false
      (let [menu (mount! anchor
                   (fn [^js menu]
                     (.setAttribute menu "role" "menu")
                     (.setAttribute menu "aria-label" "Actions")
                     (doseq [[prev item] (map vector (cons nil items) items)]
                       (when (and prev (not= (:group prev) (:group item)))
                         (let [d (el "div" "context-menu__divider")]
                           (.setAttribute d "role" "separator")
                           (.appendChild menu d)))
                       (item-row! menu target item))))]
        (move-focus! menu :first)
        true))))

(defn- rect-of [^js node]
  (let [r (.getBoundingClientRect node)]
    {:left (.-left r) :top (.-top r) :right (.-right r) :bottom (.-bottom r)}))

(defn- center-anchor []
  (let [{:keys [width height]} (viewport)]
    {:x (/ width 3) :y (/ height 3)}))

(defn- input-anchor
  "Where a field opens when no menu is open: the target's anchor element, else
   the focused element, else a third of the way into the viewport."
  [target]
  (let [^js node (or (-> target :surface :anchor-el) js/document.activeElement)]
    (if (and node (.-isConnected node) (not= node js/document.body))
      (rect-of node)
      (center-anchor))))

(defn request-input!
  "Ask for `command-id`'s :menu-input value, then dispatch it with the value.
   With a menu open the field replaces its rows in place. Otherwise a menu
   opens holding only the field — the palette's path."
  [command-id target]
  (if-let [{:keys [^js el anchor]} @!menu]
    (do (input-body! el command-id target)
        (place! el anchor))
    (let [anchor (input-anchor target)
          menu (mount! anchor (fn [_]))]
      (input-body! menu command-id target)
      (place! menu anchor))))

(bus/register-input-requester! request-input!)

(defn native-requested?
  "True for a Shift+right-click with a mouse, which keeps the browser's menu.
   A keyboard-opened contextmenu reports a button other than 2, so Shift+F10
   still opens this menu."
  [^js e]
  (and (.-shiftKey e) (= 2 (.-button e))))

(defn keyboard-opened?
  "True when Shift+F10 or the Menu key opened the contextmenu: no secondary
   mouse button took part."
  [^js e]
  (not= 2 (.-button e)))

(defn open-from-event!
  "The contextmenu listener every surface calls.

   Pre : `e` is a contextmenu event. `target` is the surface's target for its
         event target, or nil when the surface has nothing there.
         `keyboard-anchor`, when given, is a 0-arg fn returning the rect a
         keyboard open anchors to (an editor's caret); else the event target's
         box is used.
   Post: true when this menu opened, and the event's default and propagation
         are stopped. False, with the event untouched, for a Shift+right-click,
         a nil target, or a target no command applies to — the browser's menu
         opens then."
  ([e target] (open-from-event! e target nil))
  ([^js e target keyboard-anchor]
   (if (or (nil? target) (native-requested? e) (not (has-items? target)))
     false
     (let [anchor (if (keyboard-opened? e)
                    (or (when keyboard-anchor (keyboard-anchor))
                        (rect-of (.-target e)))
                    {:x (.-clientX e) :y (.-clientY e)})]
       (.preventDefault e)
       (.stopPropagation e)
       (open! anchor target)))))
