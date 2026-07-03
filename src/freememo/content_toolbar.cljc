(ns freememo.content-toolbar
  "Unified card generation toolbar for both page-level and extract-level content.
   Orchestrator — delegates to ToolbarSettings, ToolbarActions, GenerateDropdown,
   and SyncDropdown. Generate + Generate-with-Prompt are consolidated into the
   Generate dropdown; Export + Pull + Anki Sync are consolidated into the Sync
   dropdown. The original button e/defns are mounted hidden inside each
   dropdown so their refs/modals/tokens stay live."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.content-toolbar-helpers :as helpers]
   [freememo.content-toolbar-settings :as settings]
   [freememo.content-toolbar-actions :as actions]
   [freememo.content-toolbar-extract :refer [ExtractActions]]
   [freememo.toolbar-generate-dropdown :refer [GenerateDropdown]]
   [freememo.toolbar-sync-dropdown :refer [SyncDropdown]]
   [freememo.bibliography-toolbar :refer [DocumentMetaGroup]]
   [freememo.document-options :refer [DocumentOptionsButton]]
   [freememo.auto-extract-button :refer [AutoExtractButton]]
   [freememo.transcribe-button :refer [TranscribeButton]]
   [freememo.jump-to-source-button :refer [JumpToSourcePageButton]]
   #?(:clj [freememo.settings :as user-settings])
   #?(:clj [freememo.user-state :as us])
   [freememo.icons :as icons]
   [freememo.command-bus :as bus]
   [freememo.toolbar-overflow :refer [install-overflow-detector!]]
   [freememo.util :refer [mac-platform?]]))

;; State map keys:
;; {:user-id       — user identifier
;;  :enc-key       — encryption key
;;  :topic-id      — the entity (page or extract topic) where cards are created
;;  :root-topic-id — the root topic for scoping (PDF root, standalone root, etc.)
;;  :page-number   — current page number (for page-level context queries, nil for non-PDF)
;;  :content-text  — full text (page text for :page mode, extract text for :extract mode)
;;  :context-mode  — :page or :extract (extract context fetches the parent
;;                   topic's content server-side at the generate site)
;;  :context-tooltip — string for the Context checkbox title attribute
;;  :llm-enabled?  — whether LLM features are enabled
;; }

;; install-overflow-detector! lives in freememo.toolbar-overflow (shared with
;; PdfToolbar). Required above.

(e/defn ContentToolbar []
  (e/client
    (let [mod-key (if (mac-platform?) "Cmd" "Ctrl")
          user-id dctx/user-id enc-key dctx/enc-key topic-id dctx/topic-id audio? dctx/audio?
          root-topic-id dctx/root-topic-id page-number dctx/page-number content-text dctx/content-text
          context-mode dctx/context-mode context-tooltip dctx/context-tooltip llm-enabled? dctx/llm-enabled?
          extract-status dctx/extract-status navigate! dctx/navigate! origin dctx/origin on-done! dctx/on-done!
          citation dctx/citation page-info dctx/page-info pdf-root? dctx/pdf-root? pdf-status dctx/pdf-status
          reading-mode? dctx/reading-mode?
          refresh dctx/card-refresh
          !show-bib dctx/!show-bib
          ;; Unsynced card count — uses refresh value for reactivity
          unsynced-count (e/server (helpers/get-unsynced-count* refresh topic-id))
          ;; biblio-target-id: the item that OWNS the bibliography. Extracts and
          ;; web/epub topics own theirs; PDF pages own none, so they resolve to
          ;; the document root. Feeds the citation display and the item-scoped
          ;; Edit/Refetch/Push in Document Options. Server fns resolve the
          ;; effective source (own, else nearest ancestor) from this id.
          biblio-target-id (if (= context-mode :page) (or root-topic-id topic-id) topic-id)
          ;; Review-unit topic: PDF root for pages, the topic itself for
          ;; extracts — the entity Priority/History act on. Document Options
          ;; hosts the Priority field (C5); the inline stepper is gone.
          review-topic-id (if extract-status topic-id (or root-topic-id topic-id))
          ;; PDF (root or page) → :page context; extract/web/epub → :extract.
          ;; Document-options shows the extraction-style select only for PDFs.
          is-pdf? (= context-mode :page)
          ;; Load settings from server
          server-context-enabled (e/server (user-settings/get-context-enabled user-id))
          server-context-pages (e/server (user-settings/get-context-pages user-id))
          server-card-type (e/server (user-settings/get-card-type user-id))
          server-card-count (e/server (user-settings/get-card-count user-id))

          ;; Shared settings atoms (used by both ToolbarSettings and ToolbarGenerate/Actions)
          !use-context (atom server-context-enabled)
          use-context (e/watch !use-context)
          !context-window (atom server-context-pages)
          context-window (e/watch !context-window)
          !card-type (atom server-card-type)
          card-type (e/watch !card-type)
          !card-count (atom server-card-count)
          card-count-val (e/watch !card-count)

          ;; Generation status (per-user, keyed by topic-id)
          card-gen-status (e/server (get (e/watch (us/get-atom user-id :card-gen-status)) topic-id))
          gen-pending (or (:pending card-gen-status) 0)
          gen-active? (or (some? (:active-id card-gen-status)) (pos? gen-pending))
          gen-error (:error card-gen-status)

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
        ;; Add-Card. Everything else (Generate, Sync, doc-meta, history, …) and
        ;; the overflow machinery are dropped for a distraction-free read.
        (dom/div
          (dom/props {:class "toolbar-container"})
          (dom/div
            (dom/props {:class "toolbar"})
            (dom/div
              (dom/props {:class "toolbar-group"})
              (binding [dctx/mod-key mod-key dctx/card-type card-type]
                (actions/ToolbarActions)))))

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
              (dom/div (dom/props {:class "toolbar-overflow-panel"})

            ;; Priority / Context / Basic-Cloze proxies removed: Priority moved
            ;; into the Document-Options modal (C5); card-type + Context moved
            ;; into the Generate dropdown menu (C3), which is itself a dropdown
            ;; and works on mobile, so no overflow proxy is needed.

            ;; CardCount proxy (.toolbar-overflow-cardcount, reveals T4+).
            ;; Same atom as inline stepper — instances stay in sync.
                (when llm-enabled?
                  (dom/div
                    (dom/props {:class "toolbar-overflow-panel-action toolbar-overflow-cardcount"})
                    (settings/CardCountStepper user-id card-count-val !card-count)))

            ;; Bibliography (Refetch) proxy removed — Refetch folded into the
            ;; Edit-Bibliography modal (C4).

            ;; Auto-extract proxy (.toolbar-overflow-bib, reveals T5+).
            ;; Disabled pending release — tooltip surfaces the status.
                (dom/div
                  (dom/props {:class "toolbar-overflow-panel-action toolbar-overflow-bib"})
                  (dom/button
                    (dom/props {:class "btn btn-sm btn-secondary"
                                :aria-label "Auto-extract (future feature)"
                                :data-tooltip "Future Feature"
                                :disabled true})
                    (icons/Icon :scan-text :size 16)
                    (dom/span (dom/props {:class "icon-label"}) (dom/text "Auto-extract"))))

            ;; Document-options proxy (.toolbar-overflow-bib, reveals T5+).
            ;; Hosts the Priority field (C5), so it targets review-topic-id.
                (dom/div
                  (dom/props {:class "toolbar-overflow-panel-action toolbar-overflow-bib"})
                  (DocumentOptionsButton user-id biblio-target-id is-pdf? (or root-topic-id topic-id) review-topic-id !show-bib true nil))

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
                (dom/button
                  (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-panel-action toolbar-overflow-default"
                              :aria-label "Add new"})
                  (icons/Icon :plus :size 16)
                  (dom/span (dom/props {:class "icon-label"}) (dom/text "Add new"))
                  (dom/On "click" (fn [_]
                                    (bus/dispatch! :add-new)
                                    (reset! !overflow-open false)) nil))
                (dom/button
                  (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-panel-action toolbar-overflow-sync"
                              :aria-label "Export"})
                  (icons/Icon :download :size 16)
                  (dom/span (dom/props {:class "icon-label"})
                    (dom/text (if (and unsynced-count (pos? unsynced-count))
                                (str "Export (" unsynced-count ")...")
                                "Export...")))
                  (dom/On "click" (fn [_]
                                    (bus/dispatch! :export)
                                    (reset! !overflow-open false)) nil))
                (dom/button
                  (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-panel-action toolbar-overflow-sync"
                              :aria-label "Pull from Anki"})
                  (icons/Icon :cloud-download :size 16)
                  (dom/span (dom/props {:class "icon-label"}) (dom/text "Pull from Anki"))
                  (dom/On "click" (fn [_]
                                    (bus/dispatch! :pull-anki)
                                    (reset! !overflow-open false)) nil))
                (dom/button
                  (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-panel-action toolbar-overflow-sync"
                              :aria-label "Push to Anki"})
                  (icons/Icon :refresh-cw :size 16)
                  (dom/span (dom/props {:class "icon-label"})
                    (dom/text (if (and unsynced-count (pos? unsynced-count))
                                (str "Push to Anki (" unsynced-count ")...")
                                "Push to Anki...")))
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
                                      (reset! !overflow-open false)) nil))))

          ;; Toolbar groups (flat runs + dividers), left-to-right:
          ;; 1 Extract/Add · 2 Generate · 3 Sync · 4 History/Priority/Done/Delete
          ;; · 5 Transcribe/Bibliography-refetch/Auto-extract.
          ;; Collapse tiers are by CSS class (not position). Dividers between
          ;; groups carry the collapse class of the neighbor that disappears so
          ;; they hide in lockstep (no dangling separator): the Sync→lifecycle
          ;; divider hides with Sync (.toolbar-collapse-sync, tier 7); the
          ;; lifecycle→doc-context divider hides with doc-context
          ;; (.toolbar-collapse-bib, tier 5). Groups 1/2 always show their main
          ;; button, so D1/D2 never dangle.

          ;; 1. Extract + Add new (IR Tools).
              (dom/div
                (dom/props {:class "toolbar-group"})
                (binding [dctx/mod-key mod-key dctx/card-type card-type]
                  (actions/ToolbarActions)))

              (dom/div (dom/props {:class "toolbar-group-divider"}))

          ;; 2. Generate group — Generate dropdown + parameters together.
          ;; ToolbarGenerate mounts hidden inside the dropdown so its e/Token
          ;; paths, PromptDialog, and button refs stay live; generate/prompt
          ;; atoms stay LOCAL to it (avoids reactive loops). Context is the
          ;; first to collapse (.toolbar-collapse-first).
              (dom/div
                (dom/props {:class "toolbar-group toolbar-generate-cluster"})
                (binding [dctx/mod-key mod-key dctx/card-type card-type
                          dctx/card-count-val card-count-val dctx/use-context use-context
                          dctx/context-window context-window dctx/gen-active? gen-active?
                          dctx/gen-pending gen-pending dctx/gen-error gen-error
                          dctx/!use-context !use-context dctx/!context-window !context-window
                          dctx/!card-type !card-type]
                  (GenerateDropdown))

                ;; Card-count stays inline (commonly changed). Card-type +
                ;; Context moved into the Generate dropdown menu (C3).
                (when llm-enabled?
                  (dom/div
                    (dom/props {:class "toolbar-collapse-cardcount"})
                    (settings/CardCountStepper user-id card-count-val !card-count))))

              (dom/div (dom/props {:class "toolbar-group-divider"}))

          ;; 3. Sync dropdown (unboxed, lone trigger) — Export + Pull + Anki Sync.
          ;; Source buttons mount hidden inside so refs/modals/e/Token paths stay
          ;; live; .toolbar-collapse-sync sits on the visible trigger inside.
              (binding [dctx/card-type card-type dctx/unsynced-count unsynced-count
                        dctx/mod-key mod-key]
                (SyncDropdown))

          ;; Divider hides with Sync at tier 7 (no dangling separator).
              (dom/div (dom/props {:class "toolbar-group-divider toolbar-collapse-sync"}))

          ;; 4. History / Priority / Done / Delete. History + Priority always
          ;; present; Done/Delete only for extract topics.
              (dom/div
                (dom/props {:class "toolbar-group"})
                (ExtractActions))

          ;; Divider hides with the doc-context group at tier 5.
              (dom/div (dom/props {:class "toolbar-group-divider toolbar-collapse-bib"}))

          ;; 5. Transcribe (audio) + Auto-extract + Document-options.
          ;; Refetch-bibliography folded into the Edit-Bibliography modal (C4).
              (dom/div
                (dom/props {:class "toolbar-group toolbar-doc-context-group toolbar-collapse-bib"})
                (when audio?
                  (TranscribeButton user-id topic-id enc-key))
                (when (= context-mode :extract)
                  (JumpToSourcePageButton topic-id navigate!))
                (AutoExtractButton)
                (DocumentOptionsButton user-id biblio-target-id is-pdf? (or root-topic-id topic-id) review-topic-id !show-bib true nil))

          ;; Divider hides with the document-meta group at tier 2.
              (dom/div (dom/props {:class "toolbar-group-divider toolbar-collapse-docmeta"}))

          ;; 6. Document meta (last group, first to collapse): Edit-Bibliography
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
                              :style {:font-weight "bold" :padding "4px 10px" :font-size "16px" :line-height "1"}
                              :aria-label "More"
                              :data-tooltip "More"})
                  (icons/Icon :more-vertical :size 16)
                  (dom/span (dom/props {:class "icon-label"}) (dom/text "More"))
                  (dom/On "click" (fn [_] (swap! !overflow-open not)) nil))))

        ;; Backdrop — outside .toolbar to avoid scroll clipping
            (when overflow-open
              (dom/div
                (dom/props {:class "toolbar-overflow-backdrop"})
                (dom/On "click" (fn [_] (reset! !overflow-open false)) nil))))))))))
