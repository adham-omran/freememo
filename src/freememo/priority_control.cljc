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
   [freememo.number-stepper :refer [NumberStepper]]
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
  "Inline `Priority [−] N [+]` stepper for a topic. `priority` is the current
   value (read reactively by the caller). No client mirror atom: Save writes the
   DB and bumps :refresh, so every mount re-reads the DB value (single source of
   truth across the inline + overflow-proxy + History-modal mounts). mount-key
   is topic-id so the input remounts when the viewed topic changes."
  [user-id topic-id priority]
  (e/client
    (NumberStepper
      {:value priority :min-val 0 :max-val 100
       :mount-key topic-id :label "Priority"}
      nil
      (e/fn [nv]
        (case (e/server (e/Offload #(update-priority!* topic-id nv)))
          (e/server (swap! (us/get-atom user-id :refresh) inc)))))))
