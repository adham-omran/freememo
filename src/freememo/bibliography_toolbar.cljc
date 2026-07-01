(ns freememo.bibliography-toolbar
  "DocumentMetaGroup — the former bibliography bar, folded into the toolbar.
   Holds document-level controls: Edit-Bibliography (opens the bib modal),
   the citation, the page-progress X/Y badge, and Mark-PDF-Done. Replaces the
   separate full-width strip that used to sit below the toolbar (TitleBar)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.icons :as icons]
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
  [pdf-root-id currently-done?]
  #?(:clj (do (if currently-done?
                (db/restore-topic! pdf-root-id)
                (db/done-topic! pdf-root-id))
              :ok)
     :cljs nil))

;; Pre:  user-id non-nil; bib-topic-id = the document root; citation is a
;;       string-or-nil; page-info is {:done :total :remaining-tooltip}-or-nil;
;;       pdf-root? + pdf-status describe the PDF root (both nil for non-PDF).
;;       !show-bib is the modal-open atom owned by TopicPage (passed positionally
;;       — atoms cannot ride inside a serialized props map).
;; Post: Edit-Bibliography click sets !show-bib true (opens the modal mounted in
;;       TopicPage); Mark-PDF-Done toggles topics.status via toggle-pdf-done!*.
;; Invariant: variant :inline renders all four elements (citation + progress are
;;            informational, carry early-collapse classes); variant :overflow
;;            renders only the two actions for the ⋮ panel.
(e/defn DocumentMetaGroup
  []
  (e/client
    (let [user-id dctx/user-id bib-topic-id dctx/bib-topic-id citation dctx/citation
          page-info dctx/page-info pdf-root? dctx/pdf-root? pdf-status dctx/pdf-status
          variant dctx/variant !show-bib dctx/!show-bib
          done? (= pdf-status "done")
          inline? (= variant :inline)]

      ;; Edit Bibliography — opens the modal. Co-located with Refetch.
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary"
                    :aria-label "Edit Bibliography"
                    :data-tooltip "Edit bibliography"})
        (icons/Icon :book-open :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text "Edit Bibliography"))
        (dom/On "click" (fn [_] (reset! !show-bib true)) nil))

      ;; Mark PDF Done / Restore — PDF root only.
      (when pdf-root?
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary"
                      :aria-label (if done? "Restore PDF" "Mark PDF Done")
                      :data-tooltip (if done?
                                      "Restore this PDF to the active queue"
                                      "Mark this PDF as completed")})
          (icons/Icon (if done? :rotate-ccw :check) :size 16)
          (dom/span (dom/props {:class "icon-label"})
            (dom/text (if done? "Restore PDF" "Mark PDF Done")))
          (let [event (dom/On "click" (fn [_] (str (random-uuid))) nil)
                [t _] (e/Token event)]
            (when t
              ;; Two-step: DB write, then bump :meta-refresh, then (t). The bump
              ;; is last so (t) unmounts this block before `done?` re-reads —
              ;; otherwise the status flips back. Mirrors the old TitleBar.
              (case (e/server (toggle-pdf-done!* bib-topic-id done?))
                (case (e/server (swap! (us/get-atom user-id :meta-refresh) inc))
                  (t)))))))

      ;; Citation — inline only; first element to collapse (variable width).
      (when (and inline? citation)
        (dom/span
          (dom/props {:class "toolbar-collapse-citation"
                      :style {:font-size "12px" :font-style "italic"
                              :color "var(--color-text-hint)" :max-width "240px"
                              :overflow "hidden" :text-overflow "ellipsis"
                              :white-space "nowrap"}
                      :data-tooltip citation})
          (dom/text citation)))

      ;; Page-progress X/Y — inline only, PDF only.
      (when (and inline? page-info (pos? (:total page-info)))
        (dom/span
          (dom/props {:class "tooltip-right"
                      :style {:color "var(--color-text-primary)" :font-size "11px"
                              :font-family "ui-monospace, 'SF Mono', Menlo, Consolas, monospace"
                              :font-variant-numeric "tabular-nums" :letter-spacing "0.04em"
                              :white-space "nowrap" :padding "3px 8px"
                              :background "var(--color-bg-card)"
                              :border "1px solid var(--color-border)" :border-radius "10px"}
                      :data-tooltip (:remaining-tooltip page-info)})
          (dom/text (:done page-info) "/" (:total page-info)))))))
