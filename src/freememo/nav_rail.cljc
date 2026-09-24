(ns freememo.nav-rail
  "The app's navigation rail (plans/nav-stage-rail.md).

   One `nav` at the left of `.app-shell`. Closed, it is 48px of icons. Hover or
   keyboard focus opens it to 232px over the page, and Keep open docks it at
   232px and pushes the page. At 900px and below Keep open only opens it over
   the page (D16). At 600px and below the rail is a drawer, and a slim top bar
   holds the ☰ that opens it.

   State is client-owned (1.1.4). `!kept?` is the Keep open preference: its
   settings row seeds it once, and a Token saves it with no channel bump — the
   viewer side panels' pattern (D17). `!open?` is the transient open, and
   `!via` records what opened it. `install-rail-controller!` owns every
   pointer, focus and key rule as plain listeners, because hover timing is
   imperative and an Electric body re-runs unpredictably.

   A prop derived from a server read never shares a `dom/props` map with a
   client prop (N2). An absent value nulls every prop in its map, so a
   Continue title or a due count in flight would strip an item's class and
   aria-current."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.activity-panel :refer [ActivityNavButton]]
   [freememo.command-bus :as bus]
   [freememo.commands :as commands]
   [freememo.icons :as icons]
   [freememo.navigation :as nav]
   [freememo.undo-history-modal :refer [ActionsNavButton]]
   [freememo.viewport :as viewport]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.user-state :as us])))

;; ---------------------------------------------------------------------------
;; Items
;; ---------------------------------------------------------------------------

(def import-item
  "The rail's one action, above every destination (W3)."
  {:tab :import :label "Import" :icon :plus})

(def destinations
  "The rail's destinations below Import, in reading-stage order: arrive, read,
   review, plan, explore. One vector per group. The rail draws a divider
   between groups and no headings, because a heading has nowhere to go in a
   closed rail (D1).

   `:tab` is what `navigate!` receives. Continue is the exception: it opens
   the last document read, and `:viewer` names the route it marks active."
  [[{:tab :home :label "Home" :icon :home}]
   [{:tab :viewer :label "Continue" :icon :book-open}
    {:tab :library :label "Library" :icon :library}]
   [{:tab :learn :label "Learn" :icon :graduation-cap}
    {:tab :quiz :label "Quiz" :icon :clipboard}]
   [{:tab :schedule :label "Schedule" :icon :clock}
    {:tab :curriculum :label "Curriculum" :icon :route}]
   [{:tab :search :label "Search" :icon :search}
    {:tab :knowledge :label "Facts" :icon :link}
    {:tab :graph :label "Graph" :icon :share-2}]])

(def utilities
  "The navigating items in the rail's foot. Actions and Activity follow them.
   Those two open a surface and change no route, so they are not listed here."
  [{:tab :help :label "Help" :icon :circle-help}
   {:tab :settings :label "Settings" :icon :settings}])

(defn active-item
  "The rail item a route marks active (1.7.1).

   Pre : `route` is router5's route, whose first segment names the tab.
   Post: :learn for a learn session or a subset review. nil for :credits,
         which no item leads to. Otherwise the tab keyword, so a topic and a
         bare /viewer both give :viewer, which is Continue."
  [route]
  (let [[page sub] route
        tab (if page (keyword (str page)) :home)]
    (cond
      (and (= tab :viewer) (#{"learn-session" "subset-review"} (some-> sub str))) :learn
      (= tab :credits) nil
      :else tab)))

(defn viewer-topic-id
  "The topic id in a /viewer/topic/<id>[/<page>] route, or nil.

   A learn session and a subset review are viewer routes of another shape, so
   they give nil and never change Continue (2.1.1, D8)."
  [route]
  (let [[page sub id] route]
    (when (and (= "viewer" (some-> page str)) (= "topic" (some-> sub str)))
      (some-> id str parse-long))))

(defn rail-state
  "The rail's data-state: \"kept\", \"open\" or \"closed\".

   Keep open docks the rail only above 900px. At 900px and below it acts like
   a hover-open rail (D16), so only `open?` shows the rail there."
  [kept? open? compact?]
  (cond
    (and kept? (not compact?)) "kept"
    open? "open"
    :else "closed"))

(defn remember-topic!
  "Post: `!continue-id` holds `topic-id`. Returns `topic-id`, so a caller can
   make a later step depend on the reset."
  [!continue-id topic-id]
  (reset! !continue-id topic-id)
  topic-id)

(defn open-continue!
  "Open the last document read, or Library when there is none (2.1.2, 2.1.3).
   `target` is `continue-target*`'s result."
  [navigate! target]
  (if-let [id (:id target)]
    (navigate! :viewer (nav/nav-topic id :continue))
    (navigate! :library)))

;; ---------------------------------------------------------------------------
;; Server reads and writes. Body-level reader conditionals: every one is named
;; inside an e/server form, which CLJS compiles too.
;; ---------------------------------------------------------------------------

(defn get-nav-rail-kept* [user-id]
  #?(:clj (settings/get-nav-rail-kept user-id)
     :cljs false))

(defn save-nav-rail-kept* [user-id kept?]
  #?(:clj (settings/save-nav-rail-kept user-id kept?)
     :cljs nil))

(defn get-last-document* [user-id]
  #?(:clj (settings/get-last-document user-id)
     :cljs nil))

(defn save-last-document* [user-id topic-id]
  #?(:clj (settings/save-last-document user-id topic-id)
     :cljs nil))

(defn continue-target*
  "{:id :title} for the topic Continue opens, or nil.
   Post: nil for no id, another user's topic, and a topic deleted or staged
         for deletion — the cases where Continue falls back to Library."
  [_rev user-id topic-id]
  #?(:clj (when topic-id
            (when-let [title (db/get-topic-title-for-user user-id topic-id)]
              {:id topic-id :title title}))
     :cljs nil))

(defn learn-due-count*
  "The count Home shows as \"N topics due\" (2.2.1)."
  [_rev user-id]
  #?(:clj (db/get-learning-queue-count user-id)
     :cljs 0))

;; ---------------------------------------------------------------------------
;; Tab-visibility tick (2.2.3). The due count changes at midnight with no
;; write, so a tab that comes back re-queries. The atom is e/watch'd, so it
;; carries no reader conditional.
;; ---------------------------------------------------------------------------

(defonce !visible-tick (atom 0))

#?(:cljs
   (defonce ^:private visibility-listener-installed?
     (do (.addEventListener js/document "visibilitychange"
           (fn [_]
             (when (= "visible" (.-visibilityState js/document))
               (swap! !visible-tick inc))))
         true)))

;; ---------------------------------------------------------------------------
;; Controller. Plain listeners, never the reactive body.
;; ---------------------------------------------------------------------------

(def ^:private open-delay-ms
  "Hover this long before the rail opens (D14)."
  150)

(def ^:private close-delay-ms
  "The rail closes this long after the pointer leaves. Re-entry cancels it."
  300)

(defn docked?
  "True when Keep open docks the rail: kept, and wider than 900px (D16)."
  [kept?]
  (and kept? (not @viewport/!compact?)))

(defn close-rail!
  "Post: the rail's transient open is off. A docked rail stays docked."
  [!open? !via]
  (reset! !via nil)
  (reset! !open? false)
  nil)

(defn open-rail!
  "Post: the rail is open, and `via` records what opened it.
   :hover closes when the pointer leaves. :keyboard closes when focus leaves.
   :toggle closes only on an outside press, a navigation or Esc."
  [!open? !via via]
  (reset! !via via)
  (reset! !open? true)
  nil)

(defn toggle-kept!
  "Flip Keep open, stage the new value for the save Token, and apply it.
   Above 900px a kept rail docks, so the transient open resets. At 900px and
   below Keep open acts like a hover-open rail (D16): on, it opens the rail
   over the page, and an outside press or a navigation closes it."
  [!kept? !save !open? !via]
  (let [kept? (not @!kept?)]
    (reset! !kept? kept?)
    (reset! !save kept?)
    (if (and kept? @viewport/!compact?)
      (open-rail! !open? !via :toggle)
      (close-rail! !open? !via))))

(defn toggle-drawer!
  "The phone bar's ☰: open the drawer, or close it when it is open."
  [!open? !via]
  (if @!open?
    (close-rail! !open? !via)
    (open-rail! !open? !via :toggle)))

(defn install-rail-controller!
  "Install the rail's pointer, focus and key rules on `panel`.

   Pre : `panel` is the rail's `.nav-rail__panel`, a child of the `nav`.
         `!kept?`, `!open?` and `!via` are the rail's own atoms.
   Post: returns {:uninstall f, :open-with-focus! f}. The rules (1.3, 1.4):
         - hover opens after 150 ms and closes 300 ms after the pointer
           leaves, only for a :hover open, never on touch or at phone width;
         - keyboard focus (:focus-visible) that enters opens at once, and
           focus that leaves closes a :keyboard open;
         - Esc, an outside press, and a click on a rail item close any
           transient open;
         - a docked rail ignores all of them.
         Transitions start two frames after install, so a saved state
         applies on load with no animation (1.3.7)."
  [panel !kept? !open? !via]
  #?(:clj {:uninstall (fn []) :open-with-focus! (fn [])}
     :cljs
     (let [open-timer (atom nil)
           close-timer (atom nil)
           return-focus (atom nil)
           clear! (fn [timer] (some-> @timer js/clearTimeout) (reset! timer nil))
           docked-now? (fn [] (docked? @!kept?))
           hover-capable? (fn [] (.-matches (.matchMedia js/window "(hover: hover)")))
           close! (fn [] (clear! open-timer) (clear! close-timer) (close-rail! !open? !via))
           on-enter (fn [e]
                      (clear! close-timer)
                      (when-not (or (= "touch" (.-pointerType e))
                                    (not (hover-capable?))
                                    @viewport/!phone?
                                    (docked-now?)
                                    @!open?)
                        (clear! open-timer)
                        (reset! open-timer
                          (js/setTimeout #(open-rail! !open? !via :hover) open-delay-ms))))
           on-leave (fn [e]
                      (clear! open-timer)
                      (when (and (not= "touch" (.-pointerType e))
                                 @!open?
                                 (= :hover @!via)
                                 (not (docked-now?)))
                        (reset! close-timer (js/setTimeout close! close-delay-ms))))
           on-focus-in (fn [e]
                         (when-not (or (docked-now?) @!open?)
                           (when (.matches (.-target e) ":focus-visible")
                             (open-rail! !open? !via :keyboard))))
           on-focus-out (fn [_]
                          ;; The next focused element is only known after this
                          ;; event, so read it one task later.
                          (js/setTimeout
                            (fn []
                              (when (and @!open?
                                         (= :keyboard @!via)
                                         (not (docked-now?))
                                         (not (.contains panel (.-activeElement js/document))))
                                (close!)))
                            0))
           on-key (fn [e]
                    (when (and (= "Escape" (.-key e)) @!open? (not (docked-now?)))
                      (close!)
                      (when-some [el @return-focus]
                        (reset! return-focus nil)
                        (when (.contains js/document.documentElement el)
                          (.focus el #js {:preventScroll true})))))
           on-press (fn [e]
                      (let [t (.-target e)]
                        (when (and @!open?
                                   (not (docked-now?))
                                   (not (.contains panel t))
                                   (not (.closest t "[data-nav-rail-toggle]")))
                          (close!))))
           on-click (fn [e]
                      (let [t (.-target e)]
                        (when (and (.closest t ".nav-rail__item, .nav-rail__brand")
                                   (not (.closest t ".nav-rail__keep"))
                                   (not (docked-now?)))
                          (close!))))
           open-with-focus! (fn []
                              (clear! close-timer)
                              (open-rail! !open? !via :keyboard)
                              ;; Two frames: the palette that ran the command
                              ;; restores focus to its opener as it unmounts,
                              ;; and that restore has to land first.
                              (js/requestAnimationFrame
                                (fn []
                                  (js/requestAnimationFrame
                                    (fn []
                                      (reset! return-focus (.-activeElement js/document))
                                      (some-> (or (.querySelector panel "[aria-current='page']")
                                                  (.querySelector panel ".nav-rail__item"))
                                              (.focus)))))))
           rail (.-parentElement panel)]
       (.addEventListener panel "pointerenter" on-enter)
       (.addEventListener panel "pointerleave" on-leave)
       (.addEventListener panel "focusin" on-focus-in)
       (.addEventListener panel "focusout" on-focus-out)
       (.addEventListener panel "click" on-click)
       (.addEventListener js/document "keydown" on-key)
       (.addEventListener js/document "pointerdown" on-press true)
       (js/requestAnimationFrame
         (fn [] (js/requestAnimationFrame
                  (fn [] (some-> rail (.setAttribute "data-motion" ""))))))
       {:open-with-focus! open-with-focus!
        :uninstall (fn []
                     (clear! open-timer)
                     (clear! close-timer)
                     (.removeEventListener panel "pointerenter" on-enter)
                     (.removeEventListener panel "pointerleave" on-leave)
                     (.removeEventListener panel "focusin" on-focus-in)
                     (.removeEventListener panel "focusout" on-focus-out)
                     (.removeEventListener panel "click" on-click)
                     (.removeEventListener js/document "keydown" on-key)
                     (.removeEventListener js/document "pointerdown" on-press true))})))

;; ---------------------------------------------------------------------------
;; Components
;; ---------------------------------------------------------------------------

(e/defn RailIcon [icon]
  (e/client
    (dom/span
      (dom/props {:class "nav-rail__icon"})
      (icons/Icon icon :size 18))))

(e/defn RailLabel [text]
  (e/client
    (dom/span
      (dom/props {:class "nav-rail__label"})
      (dom/span (dom/text text)))))

(e/defn RailItem
  "One navigating row. `extra-class` is nil or one more class token."
  [{:keys [label icon]} active? extra-class on-click]
  (e/client
    (dom/button
      (dom/props {:class (str "nav-rail__item"
                           (when extra-class (str " " extra-class))
                           (when active? " is-active"))
                  :aria-current (when active? "page")})
      (RailIcon icon)
      (RailLabel label)
      (dom/On "click" (fn [_] (on-click)) nil))))

(e/defn ContinueItem
  "Continue: the last document read, with its title on a second line (W1).
   `target` comes from a server read, so it only ever reaches a text node."
  [{:keys [label icon]} active? target navigate!]
  (e/client
    (dom/button
      (dom/props {:class (str "nav-rail__item nav-rail__item--continue"
                           (when active? " is-active"))
                  :aria-current (when active? "page")})
      (RailIcon icon)
      (dom/span
        (dom/props {:class "nav-rail__label"})
        (dom/span (dom/text label))
        (dom/span
          (dom/props {:class "nav-rail__sub"})
          (dom/text (if target (:title target) "Nothing open yet"))))
      (dom/On "click" (fn [_] (open-continue! navigate! target)) nil))))

(e/defn DueCount
  "The due count, as a dot on the closed rail's icon or a pill on the open
   rail's row. Renders nothing at 0 (2.2.1)."
  [due class]
  (e/client
    (when (pos? (or due 0))
      (dom/span
        (dom/props {:class class})
        (dom/text (str due))
        (dom/span (dom/props {:class "nav-rail__sr"}) (dom/text " due"))))))

(e/defn LearnItem
  "Learn, carrying the count of topics due (W2)."
  [{:keys [label icon]} active? due navigate!]
  (e/client
    (dom/button
      (dom/props {:class (str "nav-rail__item" (when active? " is-active"))
                  :aria-current (when active? "page")})
      (dom/span
        (dom/props {:class "nav-rail__icon"})
        (icons/Icon icon :size 18)
        (DueCount due "nav-rail__dot"))
      (RailLabel label)
      (DueCount due "nav-rail__count")
      (dom/On "click" (fn [_] (navigate! :learn)) nil))))

(e/defn ContinueTracker
  "Remember a /viewer/topic/<id> route as Continue's target (D15) and save it
   as `last_document` (2.1.1). Renders nothing.

   `remember-topic!` returns the id, so the save depends on it and the reset
   runs before the write. The Offload closes over the id alone, which this
   write never changes."
  [user-id topic-id !continue-id]
  (e/client
    (when (some? topic-id)
      (let [remembered (remember-topic! !continue-id topic-id)]
        (e/server (e/Offload #(save-last-document* user-id remembered)))))))

(e/defn KeepOpenButton [kept? on-click]
  (e/client
    (dom/button
      (dom/props {:class "nav-rail__item nav-rail__keep"
                  :aria-pressed (str (boolean kept?))})
      (RailIcon :panel-left)
      (dom/span
        (dom/props {:class "nav-rail__label"})
        (dom/span
          (dom/props {:class "nav-rail__keep-row"})
          (dom/text "Keep open")
          (dom/kbd (dom/text (commands/display-chord :toggle-nav-rail)))))
      (dom/On "click" (fn [_] (on-click)) nil))))

(e/defn PhoneBar
  "The slim top bar at 600px and below: the ☰ and the brand (1.5.3)."
  [open? !open? !via navigate!]
  (e/client
    (dom/div
      (dom/props {:class "nav-rail__phonebar"})
      (dom/button
        (dom/props {:class "nav-rail__menu" :data-nav-rail-toggle ""
                    :aria-label "Open navigation"
                    :aria-expanded (str (boolean open?))})
        (icons/Icon :menu :size 20)
        (dom/On "click" (fn [_] (toggle-drawer! !open? !via)) nil))
      (dom/a
        (dom/props {:href "/" :class "nav-rail__phone-brand" :aria-label "FreeMemo — home"})
        (dom/img (dom/props {:class "nav-rail__mark" :src "/freememo/favicon.svg" :alt ""}))
        (dom/span (dom/text "FreeMemo"))
        (dom/On "click" (fn [e] (.preventDefault e) (navigate! :home)) nil)))))

(e/defn RailDestinations
  "Import, then every destination group."
  [active continue-target due navigate!]
  (e/client
    (RailItem import-item (= active :import) "nav-rail__import" (fn [] (navigate! :import)))
    (dom/div
      (dom/props {:class "nav-rail__scroll"})
      (e/for [group (e/diff-by (comp :tab first) destinations)]
        (dom/div
          (dom/props {:class "nav-rail__group"})
          (e/for [item (e/diff-by :tab group)]
            (let [tab (:tab item)
                  active? (= tab active)]
              (cond
                (= tab :viewer) (ContinueItem item active? continue-target navigate!)
                (= tab :learn) (LearnItem item active? due navigate!)
                :else (RailItem item active? nil (fn [] (navigate! tab)))))))))))

(e/defn RailFoot [user-id active navigate! !undo-modal-open? !activity-open?]
  (e/client
    (dom/div
      (dom/props {:class "nav-rail__foot"})
      (e/for [item (e/diff-by :tab utilities)]
        (let [tab (:tab item)]
          (RailItem item (= tab active) nil (fn [] (navigate! tab)))))
      (ActionsNavButton !undo-modal-open?)
      (ActivityNavButton user-id !activity-open?))))

(e/defn NavRail
  "The rail. Mount once, as the first flex item of `.app-shell`, outside every
   route branch, so it never remounts on navigation (1.1.3).

   `route` is router5's full route. `!undo-modal-open?` and `!activity-open?`
   are the shell atoms Actions and Activity toggle."
  [user-id route navigate! !undo-modal-open? !activity-open?]
  (e/client
    (let [;; Synchronous, like Main's theme read, so the value is most likely
          ;; in the same frame as the shell and a kept rail does not shift
          ;; the page after the first paint.
          persisted-kept? (e/server (boolean (get-nav-rail-kept* user-id)))
          !kept? (atom persisted-kept?)
          kept? (e/watch !kept?)
          !save (atom nil)
          save-val (e/watch !save)
          [?save-token _] (e/Token save-val)
          !open? (atom false)
          open? (e/watch !open?)
          !via (atom nil)
          compact? (e/watch viewport/!compact?)
          active (active-item route)
          persisted-last-doc (e/server (e/Offload #(get-last-document* user-id)))
          !continue-id (atom persisted-last-doc)
          continue-id (e/watch !continue-id)
          continue-target (e/server
                            (let [rev (e/watch (us/get-atom user-id :tree-mutations))]
                              (e/Offload #(continue-target* rev user-id continue-id))))
          visible-tick (e/watch !visible-tick)
          due (e/server
                (let [q (e/watch (us/get-atom user-id :queue-mutations))
                      r (e/watch (us/get-atom user-id :refresh))
                      t (e/watch (us/get-atom user-id :tree-mutations))]
                  (e/Offload #(learn-due-count* [q r t visible-tick] user-id))))]

      (when-some [token ?save-token]
        (let [r (e/server (e/Offload #(save-nav-rail-kept* user-id save-val)))]
          (case r (if (:success r) (token) (token (:error r))))))

      (ContinueTracker user-id (viewer-topic-id route) !continue-id)

      (bus/publish-ctx! {:open-continue! (fn [] (open-continue! navigate! continue-target))})

      (dom/nav
        (dom/props {:class "nav-rail" :aria-label "Main"})
        (dom/props {:data-state (rail-state kept? open? compact?)})

        (PhoneBar open? !open? !via navigate!)

        (dom/div
          (dom/props {:class "nav-rail__panel"})
          (let [controller (install-rail-controller! dom/node !kept? !open? !via)
                toggle-kept (fn [] (toggle-kept! !kept? !save !open? !via))]
            (e/on-unmount (:uninstall controller))
            (e/on-unmount #(bus/retract-ctx! [:toggle-nav-rail! :open-nav-rail! :open-continue!]))
            (bus/publish-ctx! {:toggle-nav-rail! toggle-kept
                               :open-nav-rail! (:open-with-focus! controller)})

            (dom/a
              (dom/props {:href "/" :class "nav-rail__brand" :aria-label "FreeMemo — home"})
              (dom/span
                (dom/props {:class "nav-rail__icon"})
                (dom/img (dom/props {:class "nav-rail__mark" :src "/freememo/favicon.svg" :alt ""})))
              (dom/span (dom/props {:class "nav-rail__label nav-rail__wordmark"}) (dom/text "FreeMemo"))
              (dom/On "click" (fn [e] (.preventDefault e) (navigate! :home)) nil))

            (RailDestinations active continue-target due navigate!)
            (RailFoot user-id active navigate! !undo-modal-open? !activity-open?)
            (KeepOpenButton kept? toggle-kept)))))))
