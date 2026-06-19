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
   [freememo.content-toolbar-helpers :as helpers]
   [freememo.content-toolbar-settings :as settings]
   [freememo.content-toolbar-actions :as actions]
   [freememo.content-toolbar-extract :refer [ExtractActions]]
   [freememo.priority-control :refer [PriorityControl get-topic-priority*]]
   [freememo.toolbar-generate-dropdown :refer [GenerateDropdown]]
   [freememo.toolbar-sync-dropdown :refer [SyncDropdown]]
   [freememo.bibliography-button :as bib-btn :refer [BibliographyButton]]
   [freememo.bibliography-toolbar :refer [DocumentMetaGroup]]
   [freememo.auto-extract-button :refer [AutoExtractButton]]
   [freememo.transcribe-button :refer [TranscribeButton]]
   #?(:clj [freememo.settings :as user-settings])
   #?(:clj [freememo.user-state :as us])
   [freememo.icons :as icons]
   [freememo.keyboard :as keyboard]
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

;; Pre:  `container` is a mounted DOM element with class "toolbar-container";
;;       `toolbar` is its child with class "toolbar". `!tier` and `!overflow-open`
;;       are atoms whose values must reach the rendered className.
;; Post: A ResizeObserver is installed on both nodes. On each observed size
;;       change, the tier loop (0..5) writes `.toolbar-container collapse-N`
;;       imperatively and emits the smallest fitting tier into `!tier`. Returns
;;       a 0-arg cleanup fn that disconnects the observer.
;; Invariant: kept as a plain (defn) outside any e/defn so its body is opaque
;;            to Electric's analyzer — slot counts on the e/defn caller stay
;;            identical between JVM and CLJS compile (the divergence-causing
;;            #?(:cljs …) lives inside this fn, never inside Electric AST).
(defn install-overflow-detector! [container toolbar !tier !overflow-open]
  #?(:cljs
     (let [classlist (.-classList container)
           tier-classes #js ["collapse-0" "collapse-1" "collapse-2"
                             "collapse-3" "collapse-4" "collapse-5"
                             "collapse-6" "collapse-7"]
           apply-tier!
           (fn [t]
             ;; classList ops touch only collapse-N; Electric's reactive
             ;; class binding owns the rest of the className. No race.
             (.apply (.-remove classlist) classlist tier-classes)
             (.add classlist (aget tier-classes t)))
           ;; Pure measure — CSS owns the trigger's space (see index.css
           ;; `.toolbar-overflow-trigger` per-tier rules). At tier 1 the
           ;; trigger is `display:flex; visibility:hidden` so its width is in
           ;; `scrollWidth` even before it's visible: the algorithm naturally
           ;; escalates to tier 2 when content + trigger slot can't fit.
           recompute
           (fn []
             (loop [t 0]
               (when (<= t 7)
                 (apply-tier! t)
                 (if (<= (.-scrollWidth toolbar) (+ (.-clientWidth toolbar) 1))
                   (reset! !tier t)
                   (recur (inc t))))))
           resize-obs (js/ResizeObserver. recompute)
           ;; ResizeObserver fires only on the observed element's own box
           ;; changes (parent resize). Internal content growth (e.g. an
           ;; Export label gaining "(2)" suffix) doesn't change the toolbar's
           ;; box, so we ALSO observe DOM subtree mutations to catch text /
           ;; child-list / attribute changes that may grow content width.
           mut-obs (js/MutationObserver. recompute)]
       (.observe resize-obs toolbar)
       (.observe resize-obs container)
       (.observe mut-obs toolbar
         #js {:childList true :subtree true :characterData true :attributes true})
       (recompute)
       (fn []
         (.disconnect resize-obs)
         (.disconnect mut-obs)))
     :clj (fn [] nil)))

(e/defn ContentToolbar [state refresh !show-bib]
  (e/client
    (let [mod-key (if (mac-platform?) "Cmd" "Ctrl")
          {:keys [user-id enc-key topic-id audio? root-topic-id page-number content-text
                  context-mode context-tooltip llm-enabled?
                  extract-status navigate! origin on-done!
                  citation page-info pdf-root? pdf-status]} state
          ;; Unsynced card count — uses refresh value for reactivity
          unsynced-count (e/server (helpers/get-unsynced-count* refresh topic-id))
          ;; Whether the document's root topic has a sources row attached.
          ;; Gates the Refetch-bibliography button. Reactive on :refresh so
          ;; attach/detach flips the button's enabled state without a reload.
          ;;
          ;; Source rows live on the document's root, not on PDF page children
          ;; or extract children — see topic_page.cljc's `bib-topic-id (or
          ;; pdf-root-id root-topic-id)`. The state map's :root-topic-id is
          ;; already that value, so refetch always targets the document root.
          biblio-target-id (or root-topic-id topic-id)
          has-source? (e/server (bib-btn/has-source?* refresh user-id biblio-target-id))
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

          ;; Unique radio group name
          radio-name (if (= context-mode :extract) "extract-card-type" "card-type")

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

            ;; Priority proxy (.toolbar-overflow-priority, reveals T2+) — mirrors
            ;; the inline control beside History. Targets the review-unit topic
            ;; (PDF root for pages), same rule as ExtractActions, so it edits the
            ;; priority the queue orders by. biblio-target-id is NOT equivalent —
            ;; it would scope extracts to their root instead of themselves.
                (dom/div
                  (dom/props {:class "toolbar-overflow-panel-action toolbar-overflow-priority"})
                  (let [review-topic-id (if extract-status topic-id (or root-topic-id topic-id))]
                    (PriorityControl user-id review-topic-id (e/server (get-topic-priority* refresh review-topic-id)))))

            ;; Context proxy (.toolbar-overflow-first, reveals T2+).
            ;; Two mounts bind to the same atoms; atoms are the source of truth.
                (when llm-enabled?
                  (dom/div
                    (dom/props {:class "toolbar-overflow-panel-action toolbar-overflow-first"})
                    (settings/ContextSettings
                      {:user-id user-id :context-tooltip context-tooltip
                       :llm-enabled? llm-enabled?
                       :use-context use-context :context-window context-window}
                      !use-context !context-window)))

            ;; Basic/Cloze proxy (.toolbar-overflow-card-type, reveals T3+).
                (when llm-enabled?
                  (dom/div
                    (dom/props {:class "toolbar-overflow-panel-action toolbar-overflow-card-type"})
                    (settings/ToolbarSettings
                      {:user-id user-id :radio-name radio-name :llm-enabled? llm-enabled?
                       :card-type card-type}
                      !use-context !context-window !card-type)))

            ;; CardCount proxy (.toolbar-overflow-cardcount, reveals T4+).
            ;; Same atom as inline stepper — instances stay in sync.
                (when llm-enabled?
                  (dom/div
                    (dom/props {:class "toolbar-overflow-panel-action toolbar-overflow-cardcount"})
                    (settings/CardCountStepper user-id card-count-val !card-count)))

            ;; Bibliography proxy (.toolbar-overflow-bib, reveals T5+).
            ;; Passes !overflow-open so the dropdown closes only after the
            ;; refetch token spends (success or error) — see BibliographyButton.
                (dom/div
                  (dom/props {:class "toolbar-overflow-panel-action toolbar-overflow-bib"})
                  (BibliographyButton user-id biblio-target-id has-source? !overflow-open))

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

            ;; DocumentMeta proxy (.toolbar-overflow-docmeta, reveals T2+) —
            ;; actions only (Edit-Bibliography + Mark-PDF-Done); citation and
            ;; progress are informational and drop when collapsed.
                (dom/div
                  (dom/props {:class "toolbar-overflow-panel-action toolbar-overflow-docmeta"
                              :style {:flex-direction "column" :gap "6px"}})
                  (DocumentMetaGroup
                    {:user-id user-id :bib-topic-id biblio-target-id
                     :pdf-root? pdf-root? :pdf-status pdf-status :variant :overflow}
                    !show-bib))

            ;; Separator between settings/context group and action buttons
                (dom/div (dom/props {:class "toolbar-overflow-panel-separator"}))

            ;; Proxy action buttons — hidden on desktop, visible in dropdown on mobile.
            ;; Each .click()s the hidden real button via ref (same pattern as keyboard shortcuts).
                (dom/button
                  (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-panel-action toolbar-overflow-default"
                              :aria-label "Add new"})
                  (icons/Icon :plus :size 16)
                  (dom/span (dom/props {:class "icon-label"}) (dom/text "Add new"))
                  (dom/On "click" (fn [_]
                                    (when-let [btn (deref keyboard/!add-new-btn-ref)]
                                      (.click btn))
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
                                    (when-let [btn (deref keyboard/!export-btn-ref)]
                                      (.click btn))
                                    (reset! !overflow-open false)) nil))
                (dom/button
                  (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-panel-action toolbar-overflow-sync"
                              :aria-label "Pull from Anki"})
                  (icons/Icon :cloud-download :size 16)
                  (dom/span (dom/props {:class "icon-label"}) (dom/text "Pull from Anki"))
                  (dom/On "click" (fn [_]
                                    (when-let [btn (deref keyboard/!pull-anki-btn-ref)]
                                      (.click btn))
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
                                    (when-let [btn (deref keyboard/!anki-sync-btn-ref)]
                                      (.click btn))
                                    (reset! !overflow-open false)) nil))
                (when extract-status
                  (dom/button
                    (dom/props {:class "btn btn-sm btn-danger toolbar-overflow-panel-action toolbar-overflow-default"
                                :aria-label "Delete"})
                    (icons/Icon :trash-2 :size 16)
                    (dom/span (dom/props {:class "icon-label"}) (dom/text "Delete..."))
                    (dom/On "click" (fn [_]
                                      (when-let [btn (deref keyboard/!delete-btn-ref)]
                                        (.click btn))
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
                (actions/ToolbarActions
                  {:user-id user-id :topic-id topic-id :root-topic-id root-topic-id
                   :context-mode context-mode :mod-key mod-key
                   :card-type card-type}))

              (dom/div (dom/props {:class "toolbar-group-divider"}))

          ;; 2. Generate group — Generate dropdown + parameters together.
          ;; ToolbarGenerate mounts hidden inside the dropdown so its e/Token
          ;; paths, PromptDialog, and button refs stay live; generate/prompt
          ;; atoms stay LOCAL to it (avoids reactive loops). Context is the
          ;; first to collapse (.toolbar-collapse-first).
              (dom/div
                (dom/props {:class "toolbar-group toolbar-generate-cluster"})
                (GenerateDropdown
                  (assoc state
                    :mod-key mod-key
                    :card-type card-type :card-count-val card-count-val
                    :use-context use-context :context-window context-window
                    :gen-active? gen-active? :gen-pending gen-pending :gen-error gen-error))

                (dom/div
                  (dom/props {:class "toolbar-collapse-card-type"})
                  (settings/ToolbarSettings
                    {:user-id user-id :radio-name radio-name :llm-enabled? llm-enabled?
                     :card-type card-type}
                    !use-context !context-window !card-type))
                (when llm-enabled?
                  (dom/div
                    (dom/props {:class "toolbar-collapse-cardcount"})
                    (settings/CardCountStepper user-id card-count-val !card-count)))
                (when llm-enabled?
                  (dom/div
                    (dom/props {:class "toolbar-collapse-first"})
                    (settings/ContextSettings
                      {:user-id user-id :context-tooltip context-tooltip
                       :llm-enabled? llm-enabled?
                       :use-context use-context :context-window context-window}
                      !use-context !context-window))))

              (dom/div (dom/props {:class "toolbar-group-divider"}))

          ;; 3. Sync dropdown (unboxed, lone trigger) — Export + Pull + Anki Sync.
          ;; Source buttons mount hidden inside so refs/modals/e/Token paths stay
          ;; live; .toolbar-collapse-sync sits on the visible trigger inside.
              (SyncDropdown
                {:user-id user-id :topic-id topic-id :root-topic-id root-topic-id
                 :page-number page-number :card-type card-type
                 :unsynced-count unsynced-count :mod-key mod-key})

          ;; Divider hides with Sync at tier 7 (no dangling separator).
              (dom/div (dom/props {:class "toolbar-group-divider toolbar-collapse-sync"}))

          ;; 4. History / Priority / Done / Delete. History + Priority always
          ;; present; Done/Delete only for extract topics.
              (dom/div
                (dom/props {:class "toolbar-group"})
                (ExtractActions {:user-id user-id :topic-id topic-id :root-topic-id root-topic-id
                                 :extract-status extract-status
                                 :navigate! navigate! :origin origin :on-done! on-done!}))

          ;; Divider hides with the doc-context group at tier 5.
              (dom/div (dom/props {:class "toolbar-group-divider toolbar-collapse-bib"}))

          ;; 5. Transcribe (audio) + Bibliography-refetch + Auto-extract.
              (dom/div
                (dom/props {:class "toolbar-group toolbar-doc-context-group toolbar-collapse-bib"})
                (when audio?
                  (TranscribeButton user-id topic-id enc-key))
                (BibliographyButton user-id biblio-target-id has-source? nil)
                (AutoExtractButton))

          ;; Divider hides with the document-meta group at tier 2.
              (dom/div (dom/props {:class "toolbar-group-divider toolbar-collapse-docmeta"}))

          ;; 6. Document meta (last group, first to collapse): Edit-Bibliography
          ;; + Mark-PDF-Done + citation + progress. Citation collapses at tier 1,
          ;; the rest at tier 2 (.toolbar-collapse-docmeta on the group).
              (dom/div
                (dom/props {:class "toolbar-group toolbar-collapse-docmeta"})
                (DocumentMetaGroup
                  {:user-id user-id :bib-topic-id biblio-target-id
                   :citation citation :page-info page-info
                   :pdf-root? pdf-root? :pdf-status pdf-status :variant :inline}
                  !show-bib))

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
                (dom/On "click" (fn [_] (reset! !overflow-open false)) nil)))))))))
