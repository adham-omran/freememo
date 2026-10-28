(ns freememo.content-toolbar
  "The document toolbar for both page-level and extract-level content: Anki
   Sync, the lifecycle actions, and the document context and meta groups.

   Extract, Add new card and Generate have no buttons here. Their controls are
   the selection popover and the right-click menu, and their owners mount in
   freememo.card-actions-host (plans/toolbar-card-actions-fold.md). Surfaces
   with no selection popover keep a compact Extract and Add new card pair."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.content-toolbar-helpers :as helpers]
   [freememo.gen-settings :as gs]
   [freememo.topic-kinds :as topic-kinds]
   [freememo.card-action-buttons :refer [CardActionButtons]]
   [freememo.article-split-dialog :refer [SplitArticleButton]]
   [freememo.content-toolbar-extract :refer [ExtractActions]]
   [freememo.anki-sync :refer [AnkiSyncButton]]
   [freememo.bibliography-toolbar :refer [DocumentMetaGroup]]
   [freememo.document-options :refer [DocumentOptionsTrigger]]
   [freememo.document-options-targets :as doc-options-targets]
   [freememo.transcribe-button :refer [TranscribeButton]]
   [freememo.jump-to-source-button :refer [JumpToSourcePageButton]]
   #?(:clj [freememo.user-state :as us])
   [freememo.icons :as icons]
   [freememo.brand-icons :refer [AnkiMark]]
   [freememo.command-bus :as bus]
   [freememo.toolbar-overflow :refer [install-overflow-detector!]]
   [freememo.editor-pin-layer :refer [EditorPinControls]]
   [freememo.tooltip :as tooltip]
   [freememo.focus-reader :refer [FocusReaderToggle]]))

;; Ambient doc-context the toolbar reads:
;; {:user-id       — user identifier
;;  :enc-key       — encryption key
;;  :topic-id      — the entity (page or extract topic) the toolbar acts on
;;  :root-topic-id — the root topic for scoping (PDF root, standalone root, etc.)
;;  :context-mode  — :page or :extract
;; }

;; install-overflow-detector! lives in freememo.toolbar-overflow (shared with
;; PdfToolbar). Required above.

(e/defn OverflowPanel
  "The ⋮ overflow dropdown/panel body — proxy buttons for actions that collapse
   out of the inline toolbar at narrow widths (Document-options,
   DocumentMeta, Sync and Delete). Split out of ContentToolbar
   per the JVM 64KB method-limit convention (§7.4); always mounted (display via
   CSS/.overflow-open) so its e/Token handlers stay live."
  [biblio-target-id is-pdf? unsynced-count extract-status !overflow-open]
  (e/client
    (dom/div (dom/props {:class "toolbar-overflow-panel"})

      ;; Priority / Context / Basic-Cloze / count proxies removed: Priority
      ;; moved into the Document-Options modal (C5); card type, count and
      ;; Context moved into the selection popover
      ;; (plans/toolbar-card-actions-fold.md), so no overflow proxy is needed.

      ;; Bibliography (Refetch) proxy removed — Refetch folded into the
      ;; Edit-Bibliography modal (C4).

      ;; Document-options proxy (.toolbar-overflow-bib, reveals T5+). It only
      ;; opens the modal, which DocumentToolbars mounts outside this toolbar.
      (dom/div
        (dom/props {:class "toolbar-overflow-panel-action toolbar-overflow-bib"})
        (DocumentOptionsTrigger dctx/!doc-options-open nil
          (fn [] (reset! !overflow-open false))))

      (dom/div
        (dom/props {:class "toolbar-overflow-panel-action toolbar-overflow-bib"})
        (SplitArticleButton))

      (when (and dctx/focus-available? (not is-pdf?))
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-panel-action toolbar-overflow-bib"
                      :aria-label (if dctx/focus? "Exit focus reader" "Focus reader")})
          (icons/Icon :book-open :size 16)
          (dom/span (dom/props {:class "icon-label"})
            (dom/text (if dctx/focus? "Exit focus reader" "Focus reader")))
          (dom/On "click" (fn [_]
                            (bus/dispatch! :toggle-focus-reader)
                            (reset! !overflow-open false)) nil)))

      ;; DocumentMeta proxy (.toolbar-overflow-docmeta, reveals T2+) —
      ;; actions only (Edit-Bibliography + Mark-PDF-Done); citation and
      ;; progress are informational and drop when collapsed.
      (dom/div
        (dom/props {:class "toolbar-overflow-panel-action toolbar-overflow-docmeta"
                    :style {:flex-direction "column" :gap "6px"}})
        (binding [dctx/bib-topic-id biblio-target-id
                  dctx/citation nil dctx/page-info nil
                  dctx/variant :overflow]
          (DocumentMetaGroup)))

      ;; Separator between settings/context group and action buttons
      (dom/div (dom/props {:class "toolbar-overflow-panel-separator"}))

      ;; Proxy action buttons — hidden on desktop, visible in dropdown on mobile.
      ;; Each dispatches its command through the bus (same path as
      ;; keyboard shortcuts and the palette).
      ;; One Anki action. The separate Pull item is gone — a pull is an outcome
      ;; reconcile chooses per card, not a direction the user picks.
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-panel-action toolbar-overflow-sync"
                    :aria-label "Sync with Anki"})
        (AnkiMark :size 16)
        (dom/span (dom/props {:class "icon-label"})
          (dom/text (if (and unsynced-count (pos? unsynced-count))
                      (str "Sync with Anki (" unsynced-count " new)...")
                      "Sync with Anki...")))
        (dom/On "click" (fn [_]
                          (bus/dispatch! :anki-sync)
                          (reset! !overflow-open false)) nil))
      (when extract-status
        (dom/button
          (dom/props {:class "btn btn-sm btn-danger toolbar-overflow-panel-action toolbar-overflow-default"
                      :aria-label "Delete"})
          (icons/Icon :trash-2 :size 16)
          (dom/span (dom/props {:class "icon-label"}) (dom/text "Delete..."))
          (dom/On "click" (fn [_]
                            (bus/dispatch! :delete-document)
                            (reset! !overflow-open false)) nil))))))

(e/defn ContentToolbar []
  (e/client
    (let [user-id dctx/user-id enc-key dctx/enc-key topic-id dctx/topic-id audio? dctx/audio?
          root-topic-id dctx/root-topic-id context-mode dctx/context-mode
          extract-status dctx/extract-status navigate! dctx/navigate!
          reading-mode? dctx/reading-mode?
          ;; Unsynced card count. The refresh sum is built on the server, as in
          ;; ContentCardTable, for the reason given there.
          unsynced-count (e/server
                           (let [rev (+ (e/watch (us/get-atom user-id :refresh))
                                       (e/watch (us/get-atom user-id :sync-mutations))
                                       (e/watch (us/get-atom user-id :card-mutations)))]
                             (helpers/get-unsynced-count* rev topic-id)))
          ;; biblio-target-id: the item that OWNS the bibliography — the rule
          ;; lives in document-options-targets, shared with the Document
          ;; options mount. Feeds the citation display.
          biblio-target-id (:bib-id (doc-options-targets/targets
                                      {:context-mode context-mode :extract-status extract-status
                                       :root-topic-id root-topic-id :topic-id topic-id}))
          ;; PDF (root or page) → :page context; extract/web/epub → :extract.
          ;; Document-options shows the extraction-style select only for PDFs.
          is-pdf? (= context-mode :page)
          ;; Anki Sync's default kind is the kind Generate makes.
          card-type (:kind (gs/effective (e/watch gs/!gen-settings)))
          ;; Surfaces where a selection opens no popover keep the compact
          ;; Extract + Add new card pair: the code pane and a read-only reader
          ;; take their selection outside Quill.
          popover-less? (or dctx/is-code?
                          (contains? topic-kinds/read-only-content-kinds dctx/kind))

          ;; Overflow menu state
          !overflow-open (atom false)
          overflow-open (e/watch !overflow-open)
          ;; Content-aware collapse — !collapse-tier atom drives the `.collapse-N`
          ;; class on .toolbar-container. ResizeObserver lives in
          ;; install-overflow-detector! (plain Clojure fn) to keep this e/defn's
          ;; AST identical between JVM and CLJS compile — slot counts must
          ;; match or the wire protocol crashes (frame_signal aget OOB).
          !collapse-tier (atom 0)
          ;; Subscribing to the atom keeps Electric reactive on tier changes
          ;; even though the class binding doesn't include it (install owns
          ;; the collapse-N class via classList; we still e/watch so reactive
          ;; updates fire downstream where tier matters reactively).
          _collapse-tier (e/watch !collapse-tier)]

      (if reading-mode?
        ;; Reading mode (mobile learn, A2): reduce to the IR verbs — Extract +
        ;; Add-Card. Everything else (Sync, doc-meta, history, …) and the
        ;; overflow machinery are dropped for a distraction-free read. A phone
        ;; has no right-click and no chords, so the pair stays here.
        (dom/div
          (dom/props {:class "toolbar-container"})
          (dom/div
            (dom/props {:class "toolbar"})
            (CardActionButtons)))

        (dom/div
        ;; Electric's reactive class binding owns base + overflow-open only.
        ;; collapse-N is mutated imperatively by install-overflow-detector!
        ;; via classList — non-overlapping ownership prevents races.
        (dom/props {:class (str "toolbar-container"
                             (when overflow-open " overflow-open"))})
        (let [container-node dom/node]
          (dom/div
            (dom/props {:class "toolbar"})
            (let [toolbar-node dom/node
                  cleanup (install-overflow-detector!
                            container-node toolbar-node
                            !collapse-tier !overflow-open)]
              (e/on-unmount cleanup)

          ;; Overflow panel — display:contents on desktop (items flow inline),
          ;; dropdown on mobile (toggled via .overflow-open on container).
          ;; Always mounted so settings e/Token handlers stay active.
              (OverflowPanel biblio-target-id is-pdf? unsynced-count extract-status !overflow-open)

          ;; Toolbar groups (flat runs + dividers), left-to-right:
          ;; 1 Extract/Add (popover-less surfaces only), 2 Sync,
          ;; 3 History/Priority/Done/Delete, 4 Transcribe/Jump-to-source/
          ;; Document-options, 5 Document meta.
          ;; Collapse tiers are by CSS class (not position). Dividers between
          ;; groups carry the collapse class of the neighbor that disappears so
          ;; they hide in lockstep (no dangling separator): the Sync→lifecycle
          ;; divider hides with Sync (.toolbar-collapse-sync, tier 7); the
          ;; lifecycle→doc-context divider hides with doc-context
          ;; (.toolbar-collapse-bib, tier 5). Group 1 always shows both
          ;; buttons, so its divider never dangles.

          ;; 1. Extract + Add new card, where a selection has no popover.
              (when popover-less?
                (CardActionButtons)
                (dom/div (dom/props {:class "toolbar-group-divider"})))

          ;; 2. Anki Sync (unboxed, lone button). .toolbar-collapse-sync sits on
          ;; the button; its modal is a sibling and stays mounted.
              (AnkiSyncButton user-id root-topic-id card-type unsynced-count)

          ;; Divider hides with Sync at tier 7 (no dangling separator).
              (dom/div (dom/props {:class "toolbar-group-divider toolbar-collapse-sync"}))

          ;; 3. History / Priority / Done / Delete. History + Priority always
          ;; present; Done/Delete only for extract topics.
              (dom/div
                (dom/props {:class "toolbar-group"})
                (ExtractActions))

          ;; Question pins on the document in the editor, with their own
          ;; divider, or nothing (plans/document-question-pins.md §3.4.2).
              (EditorPinControls)

          ;; Divider hides with the doc-context group at tier 5.
              (dom/div (dom/props {:class "toolbar-group-divider toolbar-collapse-bib"}))

          ;; 4. Transcribe (audio) + Jump-to-source + Document-options.
          ;; Refetch-bibliography folded into the Edit-Bibliography modal (C4).
              (dom/div
                (dom/props {:class "toolbar-group toolbar-doc-context-group toolbar-collapse-bib"})
                (when audio?
                  (TranscribeButton user-id topic-id enc-key))
                (when (= context-mode :extract)
                  (JumpToSourcePageButton topic-id navigate!))
                (DocumentOptionsTrigger dctx/!doc-options-open nil nil))

              (dom/div
                (dom/props {:class "toolbar-group toolbar-collapse-bib"})
                (SplitArticleButton))

              (when (and dctx/focus-available? (not is-pdf?))
                (FocusReaderToggle "toolbar-collapse-bib"))

          ;; Divider hides with the document-meta group at tier 2.
              (dom/div (dom/props {:class "toolbar-group-divider toolbar-collapse-docmeta"}))

          ;; 5. Document meta (last group, first to collapse): Edit-Bibliography
          ;; + Mark-PDF-Done + citation + progress. Citation collapses at tier 1,
          ;; the rest at tier 2 (.toolbar-collapse-docmeta on the group).
              (dom/div
                (dom/props {:class "toolbar-group toolbar-collapse-docmeta"})
                (binding [dctx/bib-topic-id biblio-target-id
                          dctx/variant :inline]
                  (DocumentMetaGroup)))

          ;; Overflow trigger — visible only on mobile/tablet via CSS
              (dom/div
                (dom/props {:class "toolbar-overflow-trigger"})
                (dom/button
                  (dom/props {:class "btn btn-sm btn-secondary"
                              :style {:font-weight "bold" :padding "4px 10px" :font-size "16px" :line-height "1"}})
                  (tooltip/Tooltip! "More" :aria? true)
                  (icons/Icon :more-vertical :size 16)
                  (dom/span (dom/props {:class "icon-label"}) (dom/text "More"))
                  (dom/On "click" (fn [_] (swap! !overflow-open not)) nil))))

        ;; Backdrop — outside .toolbar to avoid scroll clipping
            (when overflow-open
              (dom/div
                (dom/props {:class "toolbar-overflow-backdrop"})
                (dom/On "click" (fn [_] (reset! !overflow-open false)) nil))))))))))
