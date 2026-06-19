(ns freememo.priority-control
  "Manual review-priority control for a topic.

   An inline number input (matching the toolbar's Context-pages spinner) that
   writes via db/update-topic-priority! (which logs a priority-change event) and
   bumps :refresh so every mount — the inline control beside History, the
   overflow-panel proxy, and the History modal — re-reads.

   Priority: 0 = highest … 100 = lowest. Reads default to 50.

   Mounted twice (inline + overflow proxy); both read get-topic-priority* and
   write through the DB, so the DB value (via :refresh) is the single source of
   truth — no shared client atom needed."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
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
  "Inline `Priority [n]` number input for a topic, styled like the Context-pages
   spinner. `priority` is the current value (read reactively by the caller)."
  [user-id topic-id priority]
  (e/client
    (dom/label
      (dom/props {:class "priority-control"
                  :style {:display "flex" :align-items "center" :gap "4px"
                          :font-size "13px" :color "var(--color-text-secondary)"}
                  :title "Review priority (0 = highest, 100 = lowest)"})
      (dom/text "Priority")
      ;; e/for-by frame-isolates the input (CLAUDE.md inline-number pattern);
      ;; imperative set! avoids the reactive :value prop flickering on the
      ;; server-fetched snapshot. Remounts when the topic changes.
      (e/for-by identity [_k [topic-id]]
        (dom/input
          (dom/props {:type "number" :min "0" :max "100"
                      :style {:padding "2px 4px" :font-size "13px" :width "48px"}})
          (set! (.-value dom/node) (str priority))
          (let [ev (dom/On "change"
                     (fn [e] (let [v (-> e .-target .-value)]
                               (when (seq v) (js/parseInt v))))
                     nil)
                [t _] (e/Token ev)]
            (when t
              (case (e/server (e/Offload #(update-priority!* topic-id ev)))
                (case (e/server (swap! (us/get-atom user-id :refresh) inc))
                  (t))))))))))
