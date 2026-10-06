(ns freememo.bibliography-toolbar
  "DocumentMetaGroup — the former bibliography bar, folded into the toolbar.
   Holds document-level readouts + the document done toggle: the citation, the
   page-progress X/Y badge, and the schedule pace badge beside it.
   Edit/Refetch/Push bibliography moved to Document Options (item-scoped).
   Replaces the full-width strip below the toolbar."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.command-bus :as bus]
   [freememo.done-toggle :refer [DoneToggleFace]]
   [freememo.schedule :as sched]
   [freememo.schedule-forecast :as fc]
   [freememo.client-state :as cs]
   [freememo.optimistic :as opt]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.user-state :as us])))

#?(:clj
   (defmethod opt/run-command! :set-pdf-done
     [user-id {:keys [payload]}]
     (let [{:keys [pdf-root-id done?]} payload
           result (if done?
                    (db/done-topic! user-id pdf-root-id)
                    (db/restore-topic! user-id pdf-root-id))]
       (if result
         {:ok? true}
         {:ok? false :error "PDF was not found"}))))

;; Pre:  user-id + bib-topic-id from doc-context; bib-topic-id is the document
;;       ROOT, which is what `study_schedules.root_topic_id` names.
;; Pre:  `dctx/page-number` is the page in view, and it is nil for every
;;       non-PDF — `document_view.cljc` binds it as `(when is-pdf? current-page)`
;;       for this toolbar's subtree. It decides WHICH schedule this badge speaks
;;       for on a document carrying more than one, so dropping it is not a
;;       neutral simplification.
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
          page dctx/page-number
          ;; A server-FORM binding, not an e/defn call: the map stays on the
          ;; server and each read below pulls one field. e/Offload because
          ;; `root-schedule-pace` projects every card in the document at h=0,
          ;; and a bare e/server would stall this session's reactive graph.
          ;;
          ;; `page` is a reactive dependency, so turning a page re-runs the
          ;; selection. That is what a page-scoped answer costs, and the cost is
          ;; one indexed read plus one h=0 projection — the same ~6 ms the badge
          ;; already pays per mount. Latest-wins is correct here: a reader who
          ;; pages past the boundary between two schedules wants the new one.
          pace (e/server
                 (let [refresh (e/watch (us/get-atom user-id :schedule-mutations))]
                   (e/Offload #(sched/root-schedule-pace refresh user-id root-topic-id page))))]
      ;; No collapse class of its own: it sits inside the doc-meta group, so it
      ;; hides at tier 2 with the progress badge beside it. A tier-1 class was
      ;; tried and reverted — a 1200px window already reports collapse-1, so
      ;; the badge was `display: none` on an ordinary desktop.
      ;;
      ;; The gate is the MAP's presence, not a word derived from it. `pace-word`
      ;; used to render the state and double as this test; the state left the
      ;; face, and a fn kept only for its nil-ness would have been a gate
      ;; wearing a renderer's name.
      (when (e/server (some? pace))
        (dom/span
          (dom/props {:class "toolbar-badge tooltip-right"})
          ;; The pace verdict lives HERE now, in words, through `pace-phrase`.
          ;; It left the face because a cumulative lag and a daily share are
          ;; measured against different lines: `Behind, today 16 pages` beside
          ;; `9 pages behind` asked the reader to reconcile two numbers that
          ;; need not agree.
          (tooltip/Tooltip! (e/server (:tooltip pace)))
          ;; One string, not five nodes. `schedule-badge-face` owns both tokens
          ;; and the rule that they must not both say `today`; composing them
          ;; here would put that rule in a view, where the next surface pairing
          ;; them cannot see it.
          ;;
          ;; It is also one characterData mutation per change rather than
          ;; several, and the toolbar's tier detector observes characterData on
          ;; this subtree (`toolbar_overflow.cljc:70`).
          (dom/text (fc/schedule-badge-face (e/server (:pages-today pace))
                      (e/server (:days-to-target pace)))))))))

;; Pre:  bib-topic-id is the document: the PDF root, or the score itself (a
;;       score is its own pdf-root, topic_state.cljc). pdf-status is that
;;       topic's status for both kinds. variant is :inline or :overflow.
;; Post: a PDF root or a score shows a `.done-toggle` whose aria-pressed is the
;;       document's done state; a click enqueues :set-pdf-done. Every other
;;       kind renders nothing.
;;       On a score the :inline copy also publishes the :done invoker, so
;;       meta+d and the palette's "Mark done" reach it. No other publisher is
;;       mounted on a score (PdfMarkDone is gated off there); a PDF root does
;;       NOT publish, because PdfMarkDone owns :done on a PDF. The :overflow
;;       copy never publishes — one publisher per surface.
;; Inv : `:class` sits in its own dom/props, apart from the state-derived
;;       props (CLAUDE.md, "An Absent Props Map Nulls Every Prop").
;;       Its own e/defn: DocumentMetaGroup is near the 64KB method cap.
(e/defn DocumentDoneToggle
  []
  (e/client
    (let [bib-topic-id dctx/bib-topic-id pdf-root? dctx/pdf-root?
          score? dctx/is-score? pdf-status dctx/pdf-status
          inline? (= dctx/variant :inline)
          pending-commands (e/watch (cs/get-atom :pending-commands))
          done? (opt/queued-pdf-done? pending-commands bib-topic-id (= pdf-status "done"))
          noun (if score? "score" "PDF")]
      (when (or pdf-root? score?)
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary done-toggle"})
          (dom/props {:aria-pressed (str (boolean done?))
                      :aria-label (if score? "Score done" "PDF done")})
          (tooltip/Tooltip! (if done?
                              (str "This " noun " is done. Click to restore it to the active queue.")
                              (str "Mark this " noun " as completed")))
          (DoneToggleFace done? (if score? "Score done" "PDF done")
            (str "Restore " noun))
          (when (and score? inline?)
            (let [node dom/node]
              (bus/publish-invoker! :done node (fn [] (.click node)))
              (e/on-unmount (fn [] (bus/retract-invoker! :done node)))))
          (dom/On "click"
            (fn [_]
              (opt/enqueue! {:type :set-pdf-done
                             :payload {:pdf-root-id bib-topic-id
                                       :done? (not done?)}}))
            nil))))))

;; Pre:  user-id non-nil; bib-topic-id = the citation's item (document root for
;;       PDF pages); citation is a string-or-nil; page-info is
;;       {:done :total :remaining-tooltip}-or-nil; pdf-root? + pdf-status
;;       describe the PDF root or score (both nil otherwise).
;; Post: DocumentDoneToggle toggles topics.status via :set-pdf-done (PDF root
;;       and score only). Bibliography actions (Edit/Refetch/Push) live in
;;       Document Options now.
;; Invariant: variant :inline adds the citation + progress readouts (early-
;;            collapse classes); :overflow renders only Mark-PDF-Done.
(e/defn DocumentMetaGroup
  []
  (e/client
    (let [bib-topic-id dctx/bib-topic-id citation dctx/citation
          page-info dctx/page-info
          page-number dctx/page-number
          variant dctx/variant
          inline? (= variant :inline)
          pending-commands (e/watch (cs/get-atom :pending-commands))
          persisted-page-done? (:current-done? page-info)
          queued-page-done? (when page-info
                              (opt/queued-page-done? pending-commands bib-topic-id page-number
                                persisted-page-done?))
          optimistic-page-change? (and page-info
                                       (not= persisted-page-done? queued-page-done?))
          page-done-count (if optimistic-page-change?
                            (+ (:done page-info) (if queued-page-done? 1 -1))
                            (:done page-info))
          page-tooltip (if optimistic-page-change?
                         (str page-done-count " of " (:total page-info)
                              " pages completed. Saving page " page-number "…")
                         (:remaining-tooltip page-info))]

      ;; Document done toggle — PDF root and score only.
      (DocumentDoneToggle)

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
          (tooltip/Tooltip! page-tooltip)
          (dom/text page-done-count "/" (:total page-info))))

      ;; Schedule pace — inline only, and only when an active schedule covers
      ;; this document. Gated on `inline?` alone, NOT on page-info: a schedule
      ;; exists for the document, not for the page being read.
      (when inline?
        (SchedulePaceBadge)))))
