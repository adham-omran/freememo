(ns freememo.toast-stack
  "Project-wide toast UI. Watches per-user :toasts atom and renders a
   fixed top-right stack. Sticky errors persist until dismissed; non-sticky
   auto-dismiss after 5s. Action buttons route via navigate! and dismiss."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   #?(:clj [freememo.toasts :as toasts])
   #?(:clj [freememo.undo :as undo])
   #?(:clj [freememo.user-state :as us])))

;; ---------------------------------------------------------------------------
;; Auto-dismiss timers (CLJS only, per CLAUDE.md "JS Library Init Side Effects":
;; idempotency guards live in plain defns, not the reactive body).
;; ---------------------------------------------------------------------------

#?(:cljs (defonce !auto-dismiss-timers (atom {})))

#?(:cljs
   (defn ensure-scheduled!
     "Schedule a setTimeout for toast-id. Idempotent — repeated calls are no-ops."
     [toast-id ms on-fire]
     (when-not (contains? @!auto-dismiss-timers toast-id)
       (let [tid (js/setTimeout
                   (fn []
                     (swap! !auto-dismiss-timers dissoc toast-id)
                     (on-fire))
                   ms)]
         (swap! !auto-dismiss-timers assoc toast-id tid)))))

#?(:cljs
   (defn cancel-scheduled! [toast-id]
     (when-let [tid (get @!auto-dismiss-timers toast-id)]
       (js/clearTimeout tid)
       (swap! !auto-dismiss-timers dissoc toast-id))))

;; ---------------------------------------------------------------------------
;; Server-side wrappers — keep reactive graph clean of #?(:clj ...)
;; ---------------------------------------------------------------------------

(defn toasts-for [_refresh user-id]
  #?(:clj (vec @(us/get-atom user-id :toasts))
     :cljs []))

(defn dismiss!* [user-id toast-id]
  #?(:clj (toasts/dismiss! user-id toast-id)
     :cljs nil))

(defn undo-from-toast!* [user-id undo-id]
  #?(:clj (do (undo/undo-entry! user-id undo-id) nil)
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Components
;; ---------------------------------------------------------------------------

(e/defn ToastCard [{:keys [id level message actions sticky?]} user-id navigate!]
  (e/client
    (let [[glyph level-color]
          (case level
            :success [:circle-check   "var(--color-success)"]
            :warning [:triangle-alert "var(--color-warning)"]
            :error   [:circle-alert   "var(--color-danger-text)"]
            :info    [:info           "var(--color-primary-text)"]
            [:info "var(--color-text-secondary)"])
          ;; Per-toast trigger atom for auto-dismiss.
          !timer-fired (atom false)
          timer-fired? (e/watch !timer-fired)]

      ;; Schedule (idempotent) when this card mounts; cancel on unmount.
      (when-not sticky?
        (e/client
          (ensure-scheduled! id 5000 #(reset! !timer-fired true)))
        (e/on-unmount #(cancel-scheduled! id)))

      (dom/div
        (dom/props {:class "toast-card"
                    :role "alert"})

        ;; Level icon + message
        (dom/div
          (dom/props {:class "toast-header"})
          (dom/span
            (dom/props {:class "toast-icon" :style {:color level-color}})
            (icons/Icon glyph :size 16))
          (dom/div
            (dom/props {:class "toast-message"})
            (dom/text message)))

        ;; Action buttons + close button row
        (dom/div
          (dom/props {:class "toast-actions"})
          (e/for [{:keys [label nav undo-id]} (e/diff-by :label actions)]
            (dom/button
              (dom/props {:class "toast-action-btn" :type "button"})
              (dom/text label)
              (let [click (dom/On "click" identity nil)
                    [t _] (e/Token click)]
                (when t
                  (case (e/server (dismiss!* user-id id))
                    (do (when undo-id
                          (case (e/server (undo-from-toast!* user-id undo-id)) nil))
                        (when nav
                          (if (vector? nav)
                            (apply navigate! nav)
                            (navigate! nav)))
                        (t)))))))

          ;; Close × — always present
          (dom/button
            (dom/props {:class "toast-close-btn" :type "button" :aria-label "Dismiss"})
            (icons/Icon :x :size 14)
            (let [click (dom/On "click" identity nil)
                  [t _] (e/Token click)]
              (when t
                (case (e/server (dismiss!* user-id id)) (t))))))

        ;; Auto-dismiss trigger — fires server dismiss when timer expires.
        (when timer-fired?
          (let [[t _] (e/Token timer-fired?)]
            (when t
              (case (e/server (dismiss!* user-id id)) (t)))))))))

(e/defn ToastStack [user-id navigate!]
  (e/client
    (dom/div
      (dom/props {:class "toast-stack"})
      ;; Reverse (newest-first) + diff-by keyed by :id computed server-side so
      ;; only the differential — not the whole toasts collection — crosses the wire.
      (e/for [t (e/server (e/diff-by :id (reverse (e/watch (us/get-atom user-id :toasts)))))]
        (ToastCard t user-id navigate!)))))
