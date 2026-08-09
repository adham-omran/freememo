(ns freememo.bibliography-toolbar
  "DocumentMetaGroup — the former bibliography bar, folded into the toolbar.
   Holds document-level readouts + Mark-PDF-Done: the citation, the
   page-progress X/Y badge, and the schedule pace badge beside it.
   Edit/Refetch/Push bibliography moved to Document Options (item-scoped).
   Replaces the full-width strip below the toolbar."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.icons :as icons]
   [freememo.commands :as commands]
   [freememo.schedule :as sched]
   [freememo.schedule-forecast :as fc]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.user-state :as us])))

(defn toggle-pdf-done!*
  "Flip a PDF root's completion status (DB write only — the :meta-refresh bump
   is a separate e/server step at the call site, so (t) can spend and unmount
   the effect before `done?` re-reads; bumping here under e/Offload would let
   latest-wins restart the thunk with the new `done?` and undo the toggle).
   Pre:  pdf-root-id non-nil; `currently-done?` is the status at click time.
   Post: status becomes the opposite of `currently-done?`; returns :ok.
   Blame: caller bug if pdf-root-id is nil."
  [user-id pdf-root-id currently-done?]
  #?(:clj (do (if currently-done?
                (db/restore-topic! user-id pdf-root-id)
                (db/done-topic! user-id pdf-root-id))
              :ok)
     :cljs nil))

(defn pace-word
  "The state word the pace badge shows.

   Pre : `state` is `schedule-forecast/pace-state`'s verdict, or nil when no
         schedule covers the document.
   Post: \"Behind\", \"On plan\", \"Ahead\", or nil.
   Inv : nil in, nil out — the caller renders NOTHING for nil rather than an
         empty capsule, so an unscheduled document grows no badge."
  [state]
  (case state
    :behind "Behind"
    :on-plan "On plan"
    :ahead "Ahead"
    nil))

;; Pre:  user-id + bib-topic-id from doc-context; bib-topic-id is the document
;;       ROOT, which is what `study_schedules.root_topic_id` names.
;; Post: one badge when an active schedule covers the document, nothing at all
;;       when none does.
;; Inv : its own e/defn, not a branch inside DocumentMetaGroup. Each e/defn
;;       compiles to its own JVM method, and DocumentMetaGroup is already a
;;       large one in a namespace the prod build has to fit under 64KB.
(e/defn SchedulePaceBadge
  []
  (e/client
    (let [user-id dctx/user-id
          root-topic-id dctx/bib-topic-id
          ;; A server-FORM binding, not an e/defn call: the map stays on the
          ;; server and each read below pulls one field. e/Offload because
          ;; `root-schedule-pace` projects every card in the document at h=0,
          ;; and a bare e/server would stall this session's reactive graph.
          pace (e/server
                 (let [refresh (e/watch (us/get-atom user-id :schedule-mutations))]
                   (e/Offload #(sched/root-schedule-pace refresh user-id root-topic-id))))]
      ;; No collapse class of its own: it sits inside the doc-meta group, so it
      ;; hides at tier 2 with the progress badge beside it. A tier-1 class was
      ;; tried and reverted — a 1200px window already reports collapse-1, so
      ;; the badge was `display: none` on an ordinary desktop.
      (when-some [word (pace-word (e/server (:state pace)))]
        (dom/span
          (dom/props {:class "toolbar-badge tooltip-right"})
          (tooltip/Tooltip! (e/server (:tooltip pace)))
          (dom/text word " · " (fc/deadline-token (e/server (:days-to-target pace)))))))))

;; Pre:  user-id non-nil; bib-topic-id = the citation's item (document root for
;;       PDF pages); citation is a string-or-nil; page-info is
;;       {:done :total :remaining-tooltip}-or-nil; pdf-root? + pdf-status
;;       describe the PDF root (both nil for non-PDF).
;; Post: Mark-PDF-Done toggles topics.status via toggle-pdf-done!* (PDF root
;;       only). Bibliography actions (Edit/Refetch/Push) live in Document
;;       Options now.
;; Invariant: variant :inline adds the citation + progress readouts (early-
;;            collapse classes); :overflow renders only Mark-PDF-Done.
(e/defn DocumentMetaGroup
  []
  (e/client
    (let [user-id dctx/user-id bib-topic-id dctx/bib-topic-id citation dctx/citation
          page-info dctx/page-info pdf-root? dctx/pdf-root? pdf-status dctx/pdf-status
          variant dctx/variant
          done? (= pdf-status "done")
          inline? (= variant :inline)]

      ;; Mark PDF Done / Restore — PDF root only.
      (when pdf-root?
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary"
                      :aria-label (if done? "Restore PDF" "Mark PDF Done")})
          (tooltip/Tooltip! (if done?
                              "Restore this PDF to the active queue"
                              "Mark this PDF as completed"))
          (icons/Icon (if done? :rotate-ccw :check) :size 16)
          (dom/span (dom/props {:class "icon-label"})
            (dom/text (if done? "Restore PDF" "Mark PDF Done")))
          (let [event (dom/On "click" (fn [_] (str (random-uuid))) nil)
                [t _] (e/Token event)]
            (when t
              ;; Two-step: DB write, then bump :meta-refresh, then (t). The bump
              ;; is last so (t) unmounts this block before `done?` re-reads —
              ;; otherwise the status flips back. Mirrors the old TitleBar.
              (case (e/server (toggle-pdf-done!* user-id bib-topic-id done?))
                (case (e/server (commands/bump! user-id :done))
                  (t)))))))

      ;; Citation — inline only; first element to collapse (variable width).
      (when (and inline? citation)
        (dom/span
          (dom/props {:class "toolbar-collapse-citation"
                      :style {:font-size "12px" :font-style "italic"
                              :color "var(--color-text-hint)" :max-width "240px"
                              :overflow "hidden" :text-overflow "ellipsis"
                              :white-space "nowrap"}})
          (tooltip/Tooltip! citation)
          (dom/text citation)))

      ;; Page-progress X/Y — inline only, PDF only. The capsule styling moved
      ;; to `.toolbar-badge` in index.css so the pace badge beside it cannot
      ;; drift to a different radius or font.
      (when (and inline? page-info (pos? (:total page-info)))
        (dom/span
          (dom/props {:class "toolbar-badge tooltip-right"})
          (tooltip/Tooltip! (:remaining-tooltip page-info))
          (dom/text (:done page-info) "/" (:total page-info))))

      ;; Schedule pace — inline only, and only when an active schedule covers
      ;; this document. Gated on `inline?` alone, NOT on page-info: a schedule
      ;; exists for the document, not for the page being read.
      (when inline?
        (SchedulePaceBadge)))))
