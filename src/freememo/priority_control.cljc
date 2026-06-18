(ns freememo.priority-control
  "Manual review-priority control for a topic.

   A value-button showing the current priority that opens a popover with a free
   0–100 number input. Saves via db/update-topic-priority! (which logs a
   priority-change event) and bumps :refresh so every mount — the inline button
   beside History, the overflow-panel proxy, and the History modal — re-reads.

   Priority: 0 = highest … 100 = lowest. Reads default to 50.

   Mounted twice (inline + overflow proxy); both read get-topic-priority* and
   write through the DB, so the DB value (via :refresh) is the single source of
   truth — no shared client atom needed."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.toolbar-generate-dropdown :refer [install-dropdown-listeners!]]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.user-state :as us])))

;; Server bridges — plain defns with reader conditionals so both peers see the
;; var (CLJS gets a no-op) while the DB call only runs on CLJ.
(defn get-topic-priority* [_refresh topic-id]
  #?(:clj (or (db/get-topic-priority topic-id) 50)
     :cljs 50))

(defn update-priority!* [topic-id priority]
  #?(:clj (do (db/update-topic-priority! topic-id priority) :ok)
     :cljs nil))

(e/defn PriorityControl
  "Inline value-button + number-input popover for a topic's priority.
   `priority` is the current value (read reactively by the caller)."
  [user-id topic-id priority]
  (e/client
    (let [!open (atom false)
          open (e/watch !open)]
      (dom/div
        (dom/props {:class "priority-control" :style {:position "relative" :display "inline-flex"}})

        ;; Value button — shows current priority, toggles the popover.
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary toolbar-dropdown-trigger toolbar-priority-trigger"
                      :style {:font-weight "500"}
                      :aria-label "Priority"
                      :data-tooltip "Review priority (0 = highest, 100 = lowest)"})
          (dom/span (dom/props {:class "icon-label"}) (dom/text (str "Priority " priority)))
          (icons/Icon :chevron-down :size 14)
          (dom/On "click" (fn [e] #?(:cljs (.stopPropagation e)) (swap! !open not)) nil))

        ;; Popover — Escape + outside-click close via the shared dropdown listeners.
        (when open
          (let [cleanup (install-dropdown-listeners! !open "toolbar-priority-trigger" "toolbar-priority-menu")]
            (e/on-unmount cleanup)
            (dom/div
              (dom/props {:class "toolbar-dropdown-menu toolbar-priority-menu" :role "menu"})
              (dom/label
                (dom/props {:style {:display "flex" :align-items "center" :gap "6px" :font-size "12px"}})
                (dom/text "Priority")
                ;; e/for-by frame-isolates the input (CLAUDE.md inline-number pattern);
                ;; remounts when the topic changes so the value resets correctly.
                (e/for-by identity [_k [topic-id]]
                  (dom/input
                    (dom/props {:type "number" :min "0" :max "100"
                                :style {:width "60px" :font-size "13px" :padding "4px 6px"
                                        :text-align "center"}})
                    (set! (.-value dom/node) (str priority))
                    ;; Validation lives in the plain callback (not the reactive
                    ;; body): returns the int only when in [0,100], else nil — so
                    ;; e/Token never fires on an invalid/empty value (no write).
                    (let [ev (dom/On "change"
                               (fn [e]
                                 #?(:cljs (let [v (js/parseInt (.. e -target -value))]
                                            (when (and (not (js/isNaN v)) (>= v 0) (<= v 100)) v))))
                               nil)
                          [t _] (e/Token ev)]
                      (when t
                        ;; Close the popover only after the write commits (on (t)).
                        (e/on-unmount #(reset! !open false))
                        (case (e/server (e/Offload #(update-priority!* topic-id ev)))
                          (case (e/server (swap! (us/get-atom user-id :refresh) inc))
                            (t)))))))))))))))
