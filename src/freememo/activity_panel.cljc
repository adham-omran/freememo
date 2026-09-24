(ns freememo.activity-panel
  "The Activity monitor's surface: a nav badge and the popover it opens
   (plans/activity-monitor.md).

   A POPOVER, not a modal. The page behind it stays usable — that is the whole
   point of the feature — so it takes `modal-shell`'s composable pieces
   (`FocusReturn`, `focus-on-mount!`, `trap-tab!`) rather than `ModalEscape`,
   which sets `aria-modal=\"true\"` and would tell assistive tech the rest of the
   page is inert when it is not.

   Button and popover are SEPARATE components sharing an `!open?` atom the caller
   owns, exactly like `ActionsNavButton` / `UndoHistoryModal`. They cannot be one
   component: the button is a row in the nav rail, and the popover is a sibling
   of the page column, anchored to the rail's docked edge. A popover inside the
   rail would move with it as a hover-open rail closes, and at phone width it
   would sit inside the drawer that the Activity click just closed
   (plans/nav-stage-rail.md N3).

   Refresh is two-rate, and deliberately so. The badge watches
   `:activity-mutations`, which bumps only on real transitions — a 20s keepalive
   per live activity would re-query every consumer for no state change. The
   popover additionally polls while OPEN, so a determinate upload's bar advances
   between transitions. Closed, it costs nothing."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.activity-client :as ac]
   [freememo.icons :as icons]
   [freememo.modal-shell :as modal-shell]
   [freememo.navigation :as nav]
   #?(:clj [freememo.user-state :as us])))

(def poll-ms
  "How often an OPEN popover re-reads. Must be <= the reporter's tick, or the
   panel would render the same numbers repeatedly and pay for the query anyway."
  5000)

;; ---------------------------------------------------------------------------
;; Poll timer. Plain defns, never the reactive body: Electric re-evaluates
;; bodies unpredictably and a setInterval there would stack duplicates.
;; `!poll-tick` carries no reader conditional because it is e/watch'd — the var
;; must resolve on both peers or the dual compile mismatches.
;; ---------------------------------------------------------------------------

(defonce !poll-tick (atom 0))

#?(:cljs (defonce ^:private !poll-interval (atom nil)))

(defn start-poll!
  "Begin ticking. Idempotent — a second call while running is a no-op, so a
   remount cannot stack intervals."
  []
  #?(:cljs (when-not @!poll-interval
             (reset! !poll-interval
               (js/setInterval #(swap! !poll-tick inc) poll-ms)))
     :clj nil)
  nil)

(defn stop-poll! []
  #?(:cljs (when-some [i @!poll-interval]
             (js/clearInterval i)
             (reset! !poll-interval nil))
     :clj nil)
  nil)

;; ---------------------------------------------------------------------------
;; Presentation helpers — plain fns, so they compile to their own methods and
;; stay out of the reactive body's bytecode.
;; ---------------------------------------------------------------------------

(defn- mb [n] (str (.toFixed (/ (double n) 1048576.0) 1) " MB"))

(defn progress-text
  "The proportion, read in the unit its type declares, or nil when there is none.

   The unit matters: an upload reports BYTES, so \"12345678 / 99999999\" is the
   literal truth and unreadable. Counts stay bare — \"3 / 12\" is exactly right
   for graded answers, and rounding those to MB would be absurd.

   A total with no done never reaches here; the writer rejects that pair."
  [{:keys [done total progress-unit]}]
  (when (and (some? done) (some? total) (pos? total))
    (if (= "bytes" (some-> progress-unit name))
      (str (mb done) " / " (mb total))
      (str done " / " total))))

(defn progress-pct
  "Bar width 0-100, or nil when the activity is indeterminate."
  [{:keys [done total]}]
  (when (and (some? done) (some? total) (pos? total))
    (max 0 (min 100 (int (* 100 (/ (double done) total)))))))

(defn outcome-class [outcome]
  (case outcome
    "success" "activity-outcome activity-outcome-success"
    "failure" "activity-outcome activity-outcome-failure"
    "cancelled" "activity-outcome activity-outcome-cancelled"
    "interrupted" "activity-outcome activity-outcome-interrupted"
    "activity-outcome"))

(defn outcome-text [outcome]
  (case outcome
    "success" "Done"
    "failure" "Failed"
    "cancelled" "Cancelled"
    "interrupted" "Interrupted"
    outcome))

(defn status-line
  "The one line under the label: a proportion when there is one, else the phase."
  [{:keys [phase] :as activity}]
  (or (progress-text activity) phase))

;; ---------------------------------------------------------------------------
;; Components
;; ---------------------------------------------------------------------------

(e/defn ActivityNavButton
  "The nav rail's Activity row: a spinner icon carrying the live count, toggling
   the popover. It changes no route, so it is not a rail destination."
  [user-id !open?]
  (e/client
    ;; The leave warning installs from here because this button is the one part
    ;; of the monitor that is ALWAYS mounted — the popover is not, and a driver
    ;; may start before the user ever opens it. e/snapshot fires it once per
    ;; mount; the listener itself is a defonce, so a remount costs nothing.
    (e/snapshot (ac/install-unload-warning!))
    (let [open? (e/watch !open?)
          refresh (e/server (e/watch (us/get-atom user-id :activity-mutations)))
          live (e/server (ac/live-count* refresh user-id))]
      (dom/button
        (dom/props {:class (str "nav-rail__item" (when open? " is-open"))
                    :aria-pressed (str (boolean open?))})
        (dom/span
          (dom/props {:class "nav-rail__icon"})
          (icons/Icon :loader-2 :size 18)
          ;; `live` is a server read, so the badge is its own node with its own
          ;; props: in the button's map it would null them while in flight.
          (when (pos? (or live 0))
            (dom/span
              (dom/props {:class "activity-badge"})
              (dom/text (str live)))))
        (dom/span (dom/props {:class "nav-rail__label"}) (dom/span (dom/text "Activity")))
        (dom/On "click" (fn [_] (swap! !open? not)) nil)))))

(e/defn ActivityRow
  "One activity. `drivers` is this tab's driver map, already dereferenced by the
   caller so the list derefs once rather than once per row."
  [activity user-id drivers navigate! !open?]
  (e/client
    (let [{:keys [id label outcome error subject-id cancellable]} activity
          tab-owned? (ac/tab-owns? drivers id)
          live? (nil? outcome)
          pct (progress-pct activity)]
      (dom/div
        (dom/props {:class (str "activity-row" (when-not live? " is-terminal"))})

        (dom/div
          (dom/props {:class "activity-row-main"})
          (dom/div
            (dom/props {:class "activity-row-head"})
            (dom/span (dom/props {:class "activity-label"}) (dom/text label))
            (when-not live?
              (dom/span (dom/props {:class (outcome-class outcome)})
                (dom/text (outcome-text outcome)))))

          (dom/div
            (dom/props {:class "activity-status"})
            (dom/text (if live? (status-line activity) (or error ""))))

          ;; Determinate work only. An indeterminate bar that never moves reads
          ;; as stuck, which is worse than no bar.
          (when (and live? (some? pct))
            (dom/div
              (dom/props {:class "activity-bar"})
              (dom/div (dom/props {:class "activity-bar-fill"
                                   :style {:width (str pct "%")}})))))

        (dom/div
          (dom/props {:class "activity-row-actions"})

          ;; Open what the activity is about. Absent for the types whose subject
          ;; is :none — a reconcile spans the collection, an exam sitting is the
          ;; page the user is already on.
          (when (some? subject-id)
            (dom/button
              (dom/props {:class "activity-action" :aria-label "Open"})
              (icons/Icon :book-open :size 14)
              (dom/On "click"
                (fn [_]
                  (navigate! :viewer (nav/nav-topic subject-id :activity))
                  (reset! !open? false))
                nil)))

          ;; Cancellable two different ways, and the button cannot tell the user
          ;; which: pulled-tier work is stopped server-side, while pushed-tier
          ;; work can only be stopped by the tab driving it. Work started in
          ;; ANOTHER tab therefore offers no Cancel here, which is honest — this
          ;; tab has no handle on it.
          (when (and live? (or cancellable tab-owned?))
            (dom/button
              (dom/props {:class "activity-action activity-cancel" :aria-label "Cancel"})
              (dom/text "Cancel")
              (let [ev (dom/On "click"
                         (fn [_]
                           (if tab-owned?
                             ;; Local effect, no token: the driver notices its own
                             ;; cancellation and writes the terminal row itself.
                             (do (ac/tab-cancel! id) nil)
                             id))
                         nil)
                    [?t _] (e/Token ev)]
                (when-some [t ?t]
                  (case (e/server (ac/cancel!* user-id ev)) (t)))))))))))

(e/defn ActivityPanel
  "The popover list. Mount as a sibling of the page column in `.app-shell`, so
   `--nav-rail-docked` reaches it and the rail's overflow cannot clip it."
  [user-id !open? navigate!]
  (e/client
    (when (e/watch !open?)
      (let [close! (fn [] (reset! !open? false))]
        ;; Backdrop first so the panel paints over it.
        (dom/div
          (dom/props {:class "activity-backdrop"})
          (dom/On "click" (fn [_] (close!)) nil))

        (dom/div
          (dom/props {:class "activity-panel" :role "dialog" :aria-label "Activity"
                      :tabindex "-1"})
          ;; FocusReturn captures the opener synchronously; focus-on-mount! defers
          ;; via rAF — so capture-before-focus holds whatever order this
          ;; concurrent body runs in.
          (modal-shell/FocusReturn)
          (e/snapshot (modal-shell/focus-on-mount! dom/node))
          (dom/On "keydown"
            (fn [e]
              (cond
                (= (.-key e) "Escape") (close!)
                (= (.-key e) "Tab") (modal-shell/trap-tab! (.-currentTarget e) e)))
            nil)

          ;; Poll only while open. Idempotent start, so a remount costs nothing.
          (e/snapshot (start-poll!))
          (e/on-unmount stop-poll!)

          (dom/div
            (dom/props {:class "activity-panel-head"})
            (dom/text "Activity"))

          (let [refresh (e/server (e/watch (us/get-atom user-id :activity-mutations)))
                tick (e/watch !poll-tick)
                drivers (e/watch ac/!tab-drivers)
                activities (e/server (ac/user-activities* [refresh tick] user-id))]
            (dom/div
              (dom/props {:class "activity-list"})
              (if (empty? activities)
                (dom/div (dom/props {:class "activity-empty"})
                  (dom/text "Nothing running."))
                (e/for [a (e/diff-by :id activities)]
                  (ActivityRow a user-id drivers navigate! !open?))))))))))
