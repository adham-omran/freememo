(ns freememo.priority-control
  "Manual review-priority control for a topic.

   A `[−] N [+]` stepper (NumberStepper) with an OPTIMISTIC client mirror, like
   CardCount: −/+ and typed edits update the displayed number instantly, then the
   DB write (db/update-topic-priority!, which logs a priority-change event) + a
   :refresh bump persist in the background.

   Unlike CardCount (one per-user value), priority is per-topic, so the mirror is
   reconciled to the server value whenever (topic-id, server-priority) changes —
   on topic switch or after the write lands — keeping it correct across topics.

   Priority: 0 = highest … 100 = lowest. Reads default to 50.

   Mounted twice (inline beside History + overflow proxy); they are never
   co-visible, so each owns its own mirror and reconciles to the DB via :refresh."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.number-stepper :refer [NumberStepper]]
   #?(:clj [freememo.db :as db])))

;; Server bridges — plain defns with reader conditionals so both peers see the
;; var (CLJS gets a no-op) while the DB call only runs on CLJ.
(defn get-topic-priority* [_refresh topic-id]
  #?(:clj (or (db/get-topic-priority topic-id) 50)
     :cljs 50))

(defn update-priority!* [user-id topic-id priority]
  #?(:clj (do (db/update-topic-priority! user-id topic-id priority) :ok)
     :cljs nil))

(e/defn PriorityControl
  "Inline `Priority [−] N [+]` stepper for a topic. `priority` is the server
   value (read reactively by the caller). `!mirror` is the optimistic client
   source — NumberStepper resets it on edit (instant display); Save persists.

   Reconcile: re-seed `!mirror` from the server value whenever (topic-id,
   priority) changes — key on the PAIR so a same-priority switch between topics
   still re-seeds. Pre: in-flight optimistic edits hold because `priority` only
   changes after the write lands (no clobber). Gap: a failed write would leave
   the mirror ahead of the DB, matching the control's existing no-error-surface
   behavior."
  [user-id topic-id priority]
  (e/client
    (let [!mirror (atom priority)
          mirror-val (e/watch !mirror)]
      ;; Reconcile is keyed on the (topic-id, priority) pair so it re-seeds when
      ;; a new topic's value settles. Save must NOT bump :refresh: that would
      ;; re-read this topic's server priority on our own write, and each
      ;; in-flight echo (49,48,…) would re-fire this reset! and snap the mirror
      ;; backward mid-burst. Without the self-bump the query only re-reads when
      ;; topic-id changes, so within a topic the optimistic mirror is the truth.
      (e/for-by identity [_pair [[topic-id priority]]]
        (let [v (reset! !mirror priority)] (when v nil)))
      (NumberStepper mirror-val 0 100
        topic-id "Priority" "Priority" nil nil
        !mirror
        (e/fn [nv]
          (e/server (e/Offload #(update-priority!* user-id topic-id nv))))))))
