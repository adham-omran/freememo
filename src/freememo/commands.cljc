(ns freememo.commands
  "Central command registry — the single inventory of user-invocable actions.

   Every action the app can perform has an entry here. The registry is PURE
   DATA (no functions) so both peers can read it: the client enumerates it for
   the command palette and keyboard bindings; the server derives invalidation
   bumps from it.

   Entry schema:
     :label  string        — palette display text
     :class  keyword       — :mutation | :query | :nav
     :exec   keyword       — how the effect runs (defaults by :class):
                             :queue    server effect via freememo.optimistic
                                       queue + run-command! method (mutations)
                             :client   synchronous client handler fn
                                       (nav, queries, modal toggles)
                             :electric effect lives in a mounted Electric
                                       component that consumes invocations via
                                       command-bus/Requests (client-driven
                                       flows, e.g. AnkiConnect fetches)
                             :ui-button a mounted button component owns the
                                       flow and publishes an invoke handle
                                       (command-bus/publish-invoker!); its
                                       server steps bump via bump!
     :bind   string|nil    — goog KeyboardShortcutHandler spec (\"meta+e\")
     :when   set|nil       — active-tab keywords where the command applies;
                             nil = everywhere
     :data   map           — payload fields and their sources (documentation;
                             resolution happens in command-bus preparers)
     :table  set           — DB tables the effect writes (audit/docs only)
     :views  set           — invalidation channels bumped after the effect;
                             MUST ⊆ invalidation-channels

   Invariants (see README \"Command architecture\"):
   1. Single bump authority — only this namespace increments invalidation
      channel atoms. Domain code declares :views here and calls bump!/
      bump-channels!; it never swap!s a channel atom itself.
   2. This map is the complete inventory — palette, keybindings and
      invalidation all derive from it; adding an action = adding an entry.
   3. :views values are the existing freememo.user-state counter channels;
      no other reactivity mechanism."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [freememo.util :as util]
            #?(:clj [freememo.user-state :as us])))

;; ── Invalidation channels ──────────────────────────────────────────────────
;; The per-user counter atoms in freememo.user-state. Views e/watch these;
;; bumping re-runs their queries.

(def invalidation-channels
  #{:refresh :credits-refresh :meta-refresh :settings-refresh
    :card-mutations :sync-mutations :tree-mutations
    :queue-mutations :pin-mutations :assistant-mutations :assistant-stream
    :undo-mutations :kg-mutations
    :schedule-mutations :curriculum-mutations :activity-mutations})

;; ── Channel groups ─────────────────────────────────────────────────────────
;; Named unions used by entries below and by dynamic bumpers (freememo.undo
;; bumps by the undone entry's entity type, which is only known at runtime).

(def document-views
  "A document-level mutation touches the tree, its content and cards."
  #{:refresh :tree-mutations})

(def entity-views
  "undo_log entity_type → channels to bump when an entry of that type is
   undone. The undo path is the one place bumps are selected at runtime."
  {"flashcard" #{:card-mutations}
   "pin"       #{:pin-mutations}
   "setting"   #{:settings-refresh}
   "document"  #{:card-mutations :tree-mutations :refresh}
   "curriculum" #{:curriculum-mutations :schedule-mutations}
   "assistant-chat" #{:assistant-mutations}
   "question"  #{:kg-mutations}
   "fact"      #{:kg-mutations}})

;; ── Registry ───────────────────────────────────────────────────────────────

(def domain-registry
  "Mutation and modal-opener commands, grouped by execution shape.
   :views transcribed 1:1 from the pre-registry hand-written bump sites
   (:done/:restore carry the union of their topic/page/PDF contexts).
   Empty :views = either a pure UI opener or a runtime-dynamic bump
   (bump-channels! inside the effect — undo, staged delete)."
  (merge
    ;; — Optimistic-queue mutations (run-command! methods; the pump bumps) —
    {:curriculum-change {:label "Update curriculum" :class :mutation
                         :palette-hidden true
                         :table #{:curricula :curriculum_blocks :curriculum_materials
                                  :curriculum_exam_forms :curriculum_exam_sittings
                                  :curriculum_grade_revisions :curriculum_unlock_overrides}
                         :views #{:curriculum-mutations :schedule-mutations}}
     ;; An exam answer draft autosaves on blur, so it bumps NOTHING. A channel
     ;; here would re-run the exam page model at every field boundary, and no
     ;; other view reads a draft answer. The write is silent by design.
     :save-exam-answer-draft {:label "Save exam answer draft" :class :mutation
                              :palette-hidden true
                              :data {:sitting-id :payload :index :payload
                                     :answer :payload :do-not-know :payload}
                              :table #{:curriculum_exam_sittings} :views #{}}
     ;; Cancel and archive are palette-hidden for the reason the answer draft is:
     ;; both need a sitting id that only the exam page holds, so a palette entry
     ;; could name the action but never supply its subject.
     :cancel-exam-sitting {:label "Cancel exam sitting" :class :mutation
                           :palette-hidden true
                           :data {:sitting-id :payload}
                           :table #{:curriculum_exam_sittings}
                           :views #{:curriculum-mutations}}
     :archive-exam-sitting {:label "Archive exam sitting" :class :mutation
                            :palette-hidden true
                            :data {:sitting-id :payload}
                            :table #{:curriculum_exam_sittings}
                            :views #{:curriculum-mutations}}
     ;; Both are hidden for the reason cancel and archive are: each needs a
     ;; subject the detail page holds, and the palette can name the act but
     ;; never supply the curriculum or the block.
     :delete-curriculum {:label "Delete curriculum" :class :mutation
                         :palette-hidden true
                         :data {:curriculum-id :payload}
                         :table #{:curricula :curriculum_blocks :curriculum_materials
                                  :curriculum_exam_forms :curriculum_exam_sittings
                                  :curriculum_grade_revisions :curriculum_unlock_overrides
                                  :undo_log}
                         :views #{:curriculum-mutations :schedule-mutations}}
     ;; The assistant history view's writes (plans/assistant-chat-history.md
     ;; §5.2). Each is hidden because it needs a chat id that only the panel
     ;; holds; the palette reaches them through the :…-assistant-chat… entries
     ;; below, which act on the panel's active chat. :delete-assistant-chat
     ;; bumps :undo-mutations too, because its stage writes an undo entry.
     :rename-assistant-chat {:label "Rename an assistant chat" :class :mutation
                             :palette-hidden true
                             :data {:chat-id :payload :title :payload}
                             :table #{:assistant_chats}
                             :views #{:assistant-mutations}}
     :set-assistant-chat-pinned {:label "Pin or unpin an assistant chat" :class :mutation
                                 :palette-hidden true
                                 :data {:chat-id :payload :pinned? :payload}
                                 :table #{:assistant_chats}
                                 :views #{:assistant-mutations}}
     :delete-assistant-chat {:label "Delete an assistant chat" :class :mutation
                             :palette-hidden true
                             :data {:chat-id :payload}
                             :table #{:assistant_chats :assistant_runs :undo_log}
                             :views #{:assistant-mutations :undo-mutations}}
     ;; Read only by the last-chat seed on the next mount, so it bumps nothing —
     ;; the shape of :save-reading-position.
     :mark-assistant-chat-opened {:label "Mark an assistant chat opened" :class :mutation
                                  :palette-hidden true
                                  :data {:chat-id :payload}
                                  :table #{:assistant_chats} :views #{}}
     :rename-block     {:label "Rename curriculum block" :class :mutation
                        :palette-hidden true
                        :data {:block-id :payload :title :payload}
                        :table #{:curriculum_blocks}
                        :views #{:curriculum-mutations}}
     :add-card         {:label "Add card" :class :mutation :when #{:viewer}
                        :palette-hidden true ; submitted by AddCardModal; :add-new opens it
                        :data {:topic-id :payload :root-topic-id :payload
                               :kind :payload :card-data :payload}
                        :table #{:flashcards} :views #{:card-mutations}}
     :set-page-done    {:label "Set PDF page completion" :class :mutation
                        :palette-hidden true
                        :data {:pdf-root-id :payload :page-number :payload :done? :payload}
                        :table #{:topics}
                        :views #{:refresh :meta-refresh :schedule-mutations}}
     ;; Writes `flashcards` too: a citation change fans out onto every citing
     ;; card's `anki_bibliography_html` snapshot, so the unsynced counts move.
     :save-biblio      {:label "Save bibliography" :class :mutation :palette-hidden true
                        :data {:topic-id :payload :data :payload}
                        :table #{:sources :topics :flashcards}
                        :views #{:refresh :card-mutations}}
     :add-occlusion    {:label "Add occlusion cards" :class :mutation :palette-hidden true
                        :table #{:flashcards :occlusion_groups} :views #{:card-mutations}}
     :update-occlusion {:label "Update occlusion" :class :mutation :palette-hidden true
                        :table #{:flashcards :occlusion_groups :card_versions}
                        :views #{:card-mutations}}
     :add-score-group    {:label "Add score cards" :class :mutation :palette-hidden true
                          :table #{:flashcards :score_groups :media} :views #{:card-mutations}}
     :update-score-group {:label "Update score card" :class :mutation :palette-hidden true
                          :table #{:flashcards :score_groups :media :card_versions}
                          :views #{:card-mutations}}
     :undo-newest      {:label "Undo last action" :class :mutation :bind "meta+z"
                        :table #{:undo_log :flashcards :topics :topic_pins :settings}
                        :views #{}} ; runtime-dynamic: undo/bump-views! by entity type
     :undo-entry       {:label "Undo action" :class :mutation :palette-hidden true
                        :data {:id :payload}
                        :table #{:undo_log :flashcards :topics :topic_pins :settings}
                        :views #{}}} ; runtime-dynamic, as :undo-newest

    ;; — Toolbar flows owned by mounted components (:exec :ui-button) —
    {:extract        {:label "Extract selection" :class :mutation :exec :ui-button
                      :bind "meta+e" :when #{:viewer}
                      :data {:selection :dom} :table #{:topics} :views #{:tree-mutations}}
     :generate       {:label "Generate cards" :class :mutation :exec :ui-button
                      :bind "meta+g" :when #{:viewer}
                      :table #{:flashcards} :views #{:card-mutations}}
     :parse-questions {:label "Parse questions" :class :mutation :exec :ui-button
                       :palette-hidden true :when #{:viewer}
                       :table #{:flashcards} :views #{:card-mutations}}
     :gen-prompt     {:label "Generate with prompt…" :class :mutation :exec :ui-button
                      :when #{:viewer} :views #{}} ; opens dialog; the run bumps :generate
     :compare-card-gen {:label "Compare card models" :class :mutation :exec :ui-button
                        :when #{:viewer} :table #{:flashcards} :views #{:card-mutations}}
     :scan           {:label "Scan page (OCR)" :class :mutation :exec :ui-button
                      :bind "meta+s" :when #{:viewer}
                      :table #{:topics} :views #{:refresh}}
     :distill        {:label "Distill facts (KG)" :class :mutation :exec :ui-button
                      :when #{:viewer}
                      :table #{:kg_facts :kg_entities :kg_predicates}
                      :views #{:kg-mutations}}
     :generate-questions {:label "Generate questions (KG)" :class :mutation :exec :ui-button
                          :when #{:viewer}
                          :table #{:kg_questions :kg_question_facts}
                          :views #{:kg-mutations}}
     ;; Bulk fact reject — one op over a selection, bumped directly by the Facts
     ;; view's confirm modal; no palette entry or invoker, so no :exec. Retiring the
     ;; covering questions is part of the same transaction, hence kg_questions here.
     :bulk-reject-facts {:label "Delete facts" :class :mutation :palette-hidden true
                         :table #{:kg_facts :kg_questions} :views #{:kg-mutations}}
     ;; Near-duplicate sweep (freememo.kg-dedup). Two entries, not one: the
     ;; document-scoped run and the whole-library run differ in blast radius, and
     ;; a single command with a hidden scope would make the wider one unreachable
     ;; by name. Both are reachable only where a document's facts are on screen,
     ;; hence :when — a globally listed command that does nothing elsewhere is the
     ;; failure the registry exists to prevent. Withdrawing a fact retires the
     ;; questions it grounded, hence kg_questions here.
     :sweep-duplicates {:label "Mark duplicate facts (this document)" :class :mutation
                        :exec :ui-button :when #{:knowledge}
                        :table #{:kg_facts :kg_questions} :views #{:kg-mutations}}
     :sweep-duplicates-all {:label "Mark duplicate facts (all documents)" :class :mutation
                            :exec :ui-button :when #{:knowledge}
                            :table #{:kg_facts :kg_questions} :views #{:kg-mutations}}
     ;; Reset of the three duplicate-detection thresholds. Bumped by the Settings
     ;; panel because it rewrites values OTHER than the one the user touched, and
     ;; each threshold field only ever updates its own local atom; a per-field save
     ;; needs no bump and does not have one.
     :reset-dedup-thresholds {:label "Reset duplicate thresholds" :class :mutation
                              :palette-hidden true
                              :table #{:settings} :views #{:settings-refresh}}
     ;; Flag / Suspend — per-question curation toggles, orthogonal to each other and
     ;; to status. Bumped directly by the in-quiz control bar and the Questions bank;
     ;; no palette entry or invoker, so no :exec (same shape as :dismiss/:undismiss).
     :flag-question    {:label "Flag question" :class :mutation :palette-hidden true
                        :table #{:kg_questions} :views #{:kg-mutations}}
     :suspend-question {:label "Suspend question" :class :mutation :palette-hidden true
                        :table #{:kg_questions} :views #{:kg-mutations}}
     ;; Global quiz navigation — invokers published by GlobalQuizInvokers
     ;; (mounted once in Main), so they're reachable from every tab.
     :start-quiz     {:label "Start quiz…" :class :nav :exec :ui-button :views #{}}
     :start-exam     {:label "Start timed quiz…" :class :nav :exec :ui-button :views #{}}
     :quiz-history   {:label "Quiz history" :class :nav :exec :ui-button :views #{}}
     :compare-ocr    {:label "Compare OCR models" :class :mutation :exec :ui-button
                      :when #{:viewer} :table #{:topics} :views #{:refresh}}
     :copy-text      {:label "Copy text" :class :mutation :exec :ui-button
                      :when #{:viewer} :table #{:topics} :views #{:refresh}}
     :copy-all       {:label "Copy all text" :class :mutation :exec :ui-button
                      :when #{:viewer} :table #{:topics} :views #{:refresh}}
     ;; The only :exec :electric entry. The mechanism was documented and routed
     ;; (dispatch! line ~251, Requests, consume!) with no user until this one.
     ;; It is not :ui-button because there is no button: the palette is the
     ;; whole surface, and an invisible publisher whose only job is to make the
     ;; command look available would be a component pretending to be a control.
     ;; :electric also gives the availability this command wants — always listed
     ;; on the viewer tab, doing nothing on a document with no code block. That
     ;; matches the rule :when states (a surface, never a moment); :scan is
     ;; listed on every viewer topic and only its dispatch checks it can run.
     ;; :views omits :refresh ON PURPOSE, and :table still names :topics. The
     ;; prose half of this command writes through the editor's own dirty-html
     ;; autosave, which owns that reactivity already. Bumping :refresh here
     ;; would re-fetch the topic while the editor still holds content the
     ;; autosave has not written, and `set-content!` would push the stale body
     ;; back over it.
     :apply-code-language {:label "Apply language to all code…" :class :mutation
                           :exec :electric :when #{:viewer}
                           :table #{:topics :flashcards}
                           :views #{:card-mutations}}
     ;; The palette twin of the format menu's Insert audio button, for the
     ;; document editor only: a card field's editor lives inside its modal and
     ;; publishes nothing the palette can reach. freememo.rich-text-editor
     ;; registers the handler and a preparer that is nil while no document
     ;; editor is mounted. :views is empty for the reason :apply-code-language
     ;; gives — the clip reaches `topics` through the editor's own autosave.
     :insert-audio   {:label "Insert audio" :class :mutation :exec :client
                      :when #{:viewer}
                      :table #{:media :topics} :views #{}}
     ;; :schedule-mutations is load-bearing on both. A page's done status IS the
     ;; study plan's coverage figure, so day 0 of the plan — a quota that counts
     ;; down — cannot move without it. Declaring it here rather than widening
     ;; the three readers' watches fixes all three from the bump authority: the
     ;; reading-view pace badge, the Schedule tab's Today tile and plan row 0,
     ;; and the home-view `today:` segment. A document with no schedule pays one
     ;; indexed read, because `schedule/root-schedule-pace` returns nil straight
     ;; after `db/active-study-schedules-for-root` and never projects.
     :done           {:label "Mark done" :class :mutation :exec :ui-button
                      :bind "meta+d" :when #{:viewer}
                      :table #{:topics}
                      :views #{:refresh :meta-refresh :schedule-mutations}}
     :set-page-done  {:label "Set PDF page completion" :class :mutation
                      :palette-hidden true
                      :data {:pdf-root-id :payload :page-number :payload :done? :payload}
                      :table #{:topics}
                      :views #{:refresh :meta-refresh :schedule-mutations}}
     :set-pdf-done   {:label "Set PDF completion" :class :mutation
                      :palette-hidden true
                      :data {:pdf-root-id :payload :done? :payload}
                      :table #{:topics}
                      :views #{:refresh :meta-refresh :schedule-mutations}}
     :restore        {:label "Restore (un-done)" :class :mutation :exec :ui-button
                      :when #{:viewer} :table #{:topics}
                      :views #{:refresh :meta-refresh :schedule-mutations}}
     ;; Dismiss/Undismiss — recursive queue-visibility toggle, bumped directly
     ;; by RowActionsMenu (library) and DocumentOptionsModal (viewer); no palette
     ;; entry or invoker, so no :exec.
     :dismiss        {:label "Dismiss" :class :mutation :palette-hidden true
                      :table #{:topics} :views #{:refresh :meta-refresh :tree-mutations}}
     :undismiss      {:label "Undismiss" :class :mutation :palette-hidden true
                      :table #{:topics} :views #{:refresh :meta-refresh :tree-mutations}}
     :advance-topic  {:label "Next topic" :class :mutation :exec :ui-button
                      :when #{:viewer} :table #{:topic_repetitions} :views #{:queue-mutations}}
     :postpone-topic {:label "Postpone topic" :class :mutation :exec :ui-button
                      :when #{:viewer} :table #{:topic_repetitions} :views #{:queue-mutations}}
     :add-new        {:label "Add new card…" :class :nav :exec :ui-button
                      :bind "meta+shift+a" :when #{:viewer} :views #{}}
     :toggle-assistant-pane {:label "Toggle Assistant pane" :class :query :exec :client
                             :when #{:viewer} :views #{}}
     ;; The assistant chat history (plans/assistant-chat-history.md §5.1).
     ;; RightSidePanel publishes the context for all four, so each runs with
     ;; the panel collapsed. The last two act on the active chat, and their
     ;; preparers hide them while no chat is active.
     :show-assistant-history {:label "Show chat history" :class :nav :exec :client
                              :when #{:viewer} :views #{}}
     :new-assistant-chat {:label "New assistant chat" :class :nav :exec :client
                          :when #{:viewer} :views #{}}
     :edit-assistant-chat-title {:label "Rename assistant chat" :class :nav :exec :client
                                 :when #{:viewer} :views #{}}
     :delete-active-assistant-chat {:label "Delete assistant chat" :class :nav :exec :client
                                    :when #{:viewer} :views #{}}
     :toggle-left-pane {:label "Toggle Left pane" :class :query :exec :client
                        :when #{:viewer} :views #{}}
     :show-hierarchy {:label "Show Hierarchy" :class :query :exec :client
                      :when #{:viewer} :views #{}}
     :show-pins      {:label "Show Pins" :class :query :exec :client
                      :when #{:viewer} :views #{}}
     ;; Declared exactly as :show-pins is, and for the same reason: it reveals a
     ;; side panel on one of its tabs, which changes no route and writes no row.
     ;; :when is the surface, NOT "this topic is a poem" — a registry entry
     ;; cannot say that, so the handler answers a non-poem with a toast rather
     ;; than by going quiet.
     :show-poem-panel {:label "Show poem panel" :class :query :exec :client
                       :when #{:viewer} :views #{}}
     :toggle-card-table-view {:label "Toggle Card Table View" :class :query :exec :client
                              :when #{:viewer} :views #{}}
     ;; One settings row per document, read once at open and never re-queried,
     ;; so :views stays empty — the same reasoning as :toggle-focus-reader.
     ;; :when is the tab; command-bus's preparer narrows it to documents with a
     ;; source pane (plans/editor-pane-toggle.md §4).
     :toggle-editor-pane {:label "Toggle Editor pane" :class :query :exec :client
                          :when #{:viewer} :views #{}}
     ;; Three components publish this invoker, and exactly one is ever mounted:
     ;; card-modals/EditCardModal, occlusion-modal (edit mode) and the score
     ;; toolbar. :when is tab-scoped and cannot say "a card editor is open", so
     ;; the invoker IS the modal-scope guard (command-bus/available?).
     ;; The label stays neutral because the three surfaces differ in WHEN they
     ;; write: the text editor opens an unsaved copy, while occlusion and score
     ;; duplicate the group at once. Their buttons say so; one palette row
     ;; cannot, so it promises nothing about timing.
     :copy-card      {:label "Copy card…" :class :nav :exec :ui-button
                      :when #{:viewer :library} :views #{}}
     ;; The focus-reader flag is one settings row per supported document.
     ;; Nothing re-queries it after the initial read, so :views stays empty.
     :toggle-focus-reader {:label "Focus reader" :class :mutation :exec :ui-button
                           :when #{:viewer} :table #{:settings} :views #{}}
     ;; Page flip. :query, not :mutation — the flip writes a client atom and
     ;; two reads follow it. The `last_page_` write is PdfPane's, not this
     ;; command's. The chord is substituted off macOS by effective-bind:
     ;; the default meta→ctrl rule would produce ctrl+pg-down, which is
     ;; Chrome's Select-Next-Tab accelerator.
     :prev-page      {:label "Previous page" :class :query :exec :ui-button
                      :bind "meta+pg-up" :when #{:viewer} :views #{}}
     :next-page      {:label "Next page" :class :query :exec :ui-button
                      :bind "meta+pg-down" :when #{:viewer} :views #{}}
     ;; :card_versions is reached by cascade, not by a direct write — deleting
     ;; the topics rows drops the version log that FKs them.
     :delete-document {:label "Delete document…" :class :mutation :exec :ui-button
                       :when #{:viewer}
                       :table #{:topics :flashcards :undo_log :card_versions}
                       :views #{}} ; runtime-conditional: staged-delete/stage-deletion!
     ;; One reconcile, two ways in. `:anki-sync` opens the modal (deck, header
     ;; and tag configuration, then a Reconcile button); `:quick-sync` runs the
     ;; same reconcile headlessly with the saved configuration. Neither picks a
     ;; direction — `anki-reconcile/classify` does that per card.
     ;; `:pull-anki` is gone: a pull is an outcome now, not a command.
     :anki-sync      {:label "Sync with Anki…" :class :mutation :exec :ui-button
                      :bind "meta+shift+x" :when #{:viewer}
                      :table #{:flashcards} :views #{:sync-mutations}}
     :quick-sync     {:label "Sync with Anki now" :class :mutation :exec :ui-button
                      :bind "meta+shift+alt+x" :when #{:viewer}
                      :table #{:flashcards} :views #{:sync-mutations}}}

    ;; — Import launchers (freememo.import-page registers the handlers:
    ;;   set the pending-import intent, navigate to :import, page opens the
    ;;   matching modal) —
    {:import-link    {:label "Import from link…" :class :nav}
     :import-upload  {:label "Upload file…" :class :nav}
     ;; Same modal as :import-upload. The palette matches on :label only, so a
     ;; user who types "import" finds file upload only through this entry.
     :import-file    {:label "Import document…" :class :nav}
     :import-code    {:label "Import code…" :class :nav}
     :import-audio   {:label "Import audio…" :class :nav}
     :import-video   {:label "Import video…" :class :nav}
     :import-score   {:label "Import score…" :class :nav}
     :import-paste   {:label "Paste content…" :class :nav}
     :new-topic      {:label "New topic…" :class :nav}
     :import-zotero  {:label "Import from Zotero…" :class :nav}
     :import-poem    {:label "Import poem…" :class :nav}
     :create-live-doc {:label "New live document" :class :mutation :exec :client
                       :table #{:topics}
                       :views #{}}} ; /api/create-live-doc bumps :import-document

    ;; — Boundary-declared mutations (bump! called at their server boundary;
    ;;   not directly invocable, so hidden from the palette) —
    {:edit-card       {:label "Edit card" :class :mutation :palette-hidden true
                       :table #{:flashcards :card_versions} :views #{:card-mutations}}
     ;; Read-only history viewer: a client-local modal toggle, so :exec :client
     ;; (NOT :ui-button — that shape obliges the button to publish an invoke
     ;; handle via command-bus/publish-invoker!, and this one cannot, because it
     ;; needs the card context that only the open edit modal has).
     ;; Palette-hidden for the same reason; :views #{} because it writes nothing.
     :card-history    {:label "Card edit history" :class :query :exec :client
                       :palette-hidden true :views #{}}
     ;; Both delete whole card-table ROWS, so an occlusion or score group goes
     ;; with its last member and is snapshotted for undo (cards/delete-cards-with-undo!).
     :delete-card     {:label "Delete card" :class :mutation :palette-hidden true
                       :table #{:flashcards :occlusion_groups :score_groups :undo_log}
                       :views #{:card-mutations}}
     :bulk-delete-cards {:label "Delete cards" :class :mutation :palette-hidden true
                         :table #{:flashcards :occlusion_groups :score_groups :undo_log}
                         :views #{:card-mutations}}
     :rename-topic    {:label "Rename topic" :class :mutation :palette-hidden true
                       :table #{:topics} :views #{:refresh :tree-mutations}}
     :move-topic      {:label "Move topic" :class :mutation :palette-hidden true
                       :table #{:topics :undo_log} :views #{:tree-mutations}}
     :remove-pin      {:label "Remove pin" :class :mutation :palette-hidden true
                       :table #{:topic_pins :undo_log} :views #{:pin-mutations}}
     :toggle-pin-placement {:label "Toggle pin placement" :class :mutation :palette-hidden true
                            :table #{:topic_pins} :views #{:pin-mutations}}
     :add-pin         {:label "Pin image" :class :mutation :palette-hidden true
                       :table #{:topic_pins} :views #{:pin-mutations}}
     :set-setting     {:label "Change setting" :class :mutation :palette-hidden true
                       :data {:key :payload :value :payload}
                       :table #{:settings} :views #{:settings-refresh}}
     :transcribe      {:label "Transcribe audio" :class :mutation :palette-hidden true
                       :table #{:topics} :views #{:refresh}}
     ;; Video pipeline: ffmpeg probe/extract + Whisper. Bumped at BOTH ends of
     ;; the run so the in-flight indicator appears and clears; :refresh carries
     ;; the transcript and duration back to the viewer.
     :process-video   {:label "Process video" :class :mutation :palette-hidden true
                       :table #{:topics :topic_files :topic_videos :video_transcripts}
                       :views #{:refresh}}
     ;; Video time-range extract. Same channel as :extract — it creates a child
     ;; topic, so the tree is what changes.
     :extract-video   {:label "Extract video range" :class :mutation :palette-hidden true
                       :table #{:topics :video_segments} :views #{:tree-mutations}}
     :save-video-position {:label "Save video position" :class :mutation :palette-hidden true
                           ;; No :views — the position is written on pause and
                           ;; unmount and read only on the next mount. Bumping
                           ;; here would re-render the player mid-playback.
                           :table #{:topic_videos} :views #{}}
     ;; Same shape as :save-video-position, for the same reason: a beacon on a
     ;; scroll pause and on leave, read only on the next mount
     ;; (plans/reading-position-memory.md §4 4.5, 4.6).
     :save-reading-position {:label "Save reading position" :class :mutation :palette-hidden true
                             :table #{:reading_positions} :views #{}}
     ;; Both write `flashcards` — see the note on `:save-biblio`.
     :push-biblio     {:label "Copy bibliography to extracts" :class :mutation :palette-hidden true
                       :table #{:sources :topics :flashcards}
                       :views #{:refresh :card-mutations}}
     :refetch-biblio  {:label "Refetch bibliography" :class :mutation :palette-hidden true
                       :table #{:sources :flashcards}
                       :views #{:refresh :card-mutations}}
     :import-document {:label "Import document" :class :mutation :palette-hidden true
                       :table #{:topics :topic_files :sources} :views document-views}
     ;; A SuperMemo collection lands a whole tree at once — topics, their
     ;; cards, their bibliography and an archived repetition log — so it
     ;; declares more tables than :import-document and adds :card-mutations.
     :import-supermemo {:label "Import SuperMemo collection" :class :mutation :palette-hidden true
                        :table #{:topics :flashcards :sources :media :topic_repetitions}
                        :views (conj document-views :card-mutations)}
     ;; An account import lands everything :import-supermemo does plus the
     ;; knowledge graph, the card schedules and the settings — it is the widest
     ;; mutation in the app, so it declares every table it writes.
     :import-account  {:label "Import account export" :class :mutation :palette-hidden true
                       :table #{:topics :flashcards :sources :media :topic_repetitions
                                :topic_pins :occlusion_groups :score_groups
                                :card_schedules :card_versions :assistant_chats
                                :kg_entities :kg_predicates :kg_facts :kg_questions
                                :kg_reviews :settings :topic_files :topic_videos}
                       :views (conj document-views :card-mutations :settings-refresh)}
     :append-images   {:label "Append images" :class :mutation :palette-hidden true
                       :table #{:topics :topic_files} :views document-views}
     ;; A NeoReader sync rewrites chapter CONTENT, so it bumps :refresh — the
     ;; editor has to re-fetch to show the new marks. Not :sync-mutations, which
     ;; belongs to Anki push/pull and is watched by the Anki UI. :settings-refresh
     ;; covers the Drive file-id binding row this flow writes.
     :neoreader-sync  {:label "Sync with NeoReader" :class :mutation :palette-hidden true
                       :table #{:topics :settings} :views #{:refresh :settings-refresh}}
     :credits-granted {:label "Credits granted" :class :mutation :palette-hidden true
                       :table #{:credit_orders :credit_transactions} :views #{:credits-refresh}}
     ;; A completed AI action's charge. credits/debit-with-retry! bumps it once,
     ;; after the ledger write commits, so the balance and AI Costs re-query.
     :debit-credits   {:label "Debit credits" :class :mutation :palette-hidden true
                       :table #{:users :credit_transactions} :views #{:credits-refresh}}}))

(def registry
  (merge
    ;; — Navigation (client-only, no DB footprint) —
    ;; :nav-tab / nav constructors are consumed by the generic nav handler in
    ;; freememo.command-bus.
    {:go-home       {:label "Go to Home"     :class :nav :nav-tab :home}
     :go-learn      {:label "Go to Learn"    :class :nav :nav-tab :learn}
     :go-schedule   {:label "Go to Schedule" :class :nav :nav-tab :schedule}
     ;; Replaces "Go to Viewer", which opened the empty /viewer. The nav rail
     ;; publishes :open-continue!, which opens the last document read, or
     ;; Library when there is none (plans/nav-stage-rail.md 6.2).
     :go-continue   {:label "Continue reading" :class :nav}
     :go-library    {:label "Go to Library"  :class :nav :nav-tab :library}
     :go-knowledge  {:label "Go to Facts"    :class :nav :nav-tab :knowledge}
     :go-quiz       {:label "Go to Quiz"     :class :nav :nav-tab :quiz}
     :go-search     {:label "Go to Search"   :class :nav :nav-tab :search}
     :go-import     {:label "Go to Import"   :class :nav :nav-tab :import}
     :go-settings   {:label "Go to Settings" :class :nav :nav-tab :settings}
     :go-curriculum {:label "Go to Curriculum" :class :nav :nav-tab :curriculum}
     :go-graph      {:label "Go to Graph"    :class :nav :nav-tab :graph}
     :go-help       {:label "Go to Help"     :class :nav :nav-tab :help}
     :open-topic   {:label "Open topic"     :class :nav :nav-tab :viewer
                     :data {:topic-id :payload} :palette-hidden true}
     :start-learn-session {:label "Start learn session" :class :nav :nav-tab :viewer}
     :open-subset-review  {:label "Review subset" :class :nav :nav-tab :viewer
                           :data {:root-id :payload :root-name :payload}
                           :palette-hidden true}
     ;; The mounted viewer consumes these and moves to the next matching row.
     :find-next-marked-card    {:label "Find next marked card" :class :query
                                :exec :electric :when #{:viewer}}
     :find-next-suspended-card {:label "Find next suspended card" :class :query
                                :exec :electric :when #{:viewer}}
     :find-next-remarked-card  {:label "Find next remarked card" :class :query
                                :exec :electric :when #{:viewer}}

     ;; — App shell —
     :toggle-palette    {:label "Command palette" :class :nav :bind "meta+k"}
     ;; The nav rail (plans/nav-stage-rail.md 1.4.3, 1.4.4). Declared as the
     ;; viewer's pane toggles are: a client handler flips rail state the rail
     ;; published, and the rail saves Keep open itself, bumping nothing.
     ;; "Toggle Sidebar" toggles Keep open. "Open Sidebar" opens the rail and
     ;; moves focus into it, and Esc closes it.
     :toggle-nav-rail   {:label "Toggle Sidebar" :class :query :exec :client
                         :bind "meta+." :views #{}}
     :open-nav-rail     {:label "Open Sidebar"   :class :query :exec :client
                         :views #{}}
     :open-undo-history {:label "Action history"  :class :nav}
     ;; Clears a Dismiss and goes Home, where the checklist shows again while
     ;; a step is open. freememo.getting-started registers the handler and
     ;; owns the settings write (plans/getting-started-checklist.md 5.1).
     :show-getting-started {:label "Show getting started" :class :nav}
     ;; Opens the Activity popover. :class :nav with no :nav-tab — it is a
     ;; toggle, not a destination, so command-bus flips the shell atom rather
     ;; than routing. Globally available: work runs on every tab, so a :when
     ;; would hide the command exactly where a user went looking for it.
     :open-activity     {:label "Activity"        :class :nav}

     ;; — Queries —
     :search {:label "Search" :class :query :data {:text :palette-input}}}

    ;; Domain commands (mutations + modal openers) are appended by domain
    ;; sections below, grouped to mirror the namespaces that implement them.
    domain-registry))

(defn command
  "Registry entry for `command-id`, or nil."
  [command-id]
  (get registry command-id))

(defn exec-mode
  "Effective :exec of an entry — explicit :exec, else by :class
   (:mutation → :queue, :nav/:query → :client)."
  [entry]
  (or (:exec entry)
      (case (:class entry)
        :mutation :queue
        (:nav :query) :client)))

(defn bindings
  "Seq of [command-id bind-string] for all bound commands."
  []
  (for [[id {:keys [bind]}] registry :when bind] [id bind]))

(def ^:private page-flip-ids
  "The two commands whose `meta` substitutes to `alt` rather than `ctrl`.
   See effective-bind."
  #{:prev-page :next-page})

(defn effective-bind
  "The chord actually registered and displayed for command `id` on this
   platform+browser, given its registry `bind`.

   macOS keeps `meta` (Cmd). Everywhere else `meta`→`ctrl`, because a page
   never receives `meta` (Windows/Super) key strokes. The palette is special:
   its `meta+k` would translate to `ctrl+k`, which the browser reserves for
   the address bar, so it gets a non-reserved chord instead — `ctrl+shift+p`,
   or `ctrl+/` on Firefox (which reserves `ctrl+shift+p` for private windows).

   The page-flip pair is special the same way. `meta`→`ctrl` would give
   `ctrl+pg-up` / `ctrl+pg-down`, which Chrome reserves for previous-tab and
   next-tab (`IsReservedCommandOrKey`), so the page never receives them. They
   take `alt` instead. If `alt+pg-up` / `alt+pg-down` turn out to be taken on
   a target platform, the recorded fallback is `ctrl+alt+pg-up` /
   `ctrl+alt+pg-down` (plans/focus-reader.md §5.2).

   Pre : `bind` is a goog KeyboardShortcutHandler spec string.
   Post: goog-parseable spec string; on macOS returns `bind` unchanged."
  [id bind]
  (cond
    (util/mac-platform?)   bind
    (= id :toggle-palette) (if (util/firefox-browser?) "ctrl+/" "ctrl+shift+p")
    (page-flip-ids id)     (str/replace bind "meta" "alt")
    :else                  (str/replace bind "meta" "ctrl")))

(defn display-chord
  "Human-readable chord for command `id` (registry `bind`), for the current
   platform+browser. Renders the resolved chord (effective-bind), so the shown
   key always matches the key that actually fires: \"⌘E\" on macOS (glyphs,
   joined tight), \"Ctrl+E\" (or \"Ctrl+/\" for the palette on Firefox)
   elsewhere.

   The 1-arity reads the bind from the registry, which is what a UI label
   wants: a tooltip that names its own chord must never be able to state one
   that does not fire, so it may not carry a hand-written copy. Four labels
   read `(str mod-key \"+Shift+E\")` and went stale the day those chords
   shortened — see plans/short-keyboard-chords.md.

   Pre : `bind` is a goog spec string or nil.
   Post: display string, or nil when the command has no bind."
  ([id] (display-chord id (:bind (command id))))
  ([id bind]
   (when bind
     (let [mac? (util/mac-platform?)
           eff  (effective-bind id bind)
           part (fn [p] (case p
                          "meta"  "⌘"
                          "ctrl"  (if mac? "⌃" "Ctrl")
                          "shift" (if mac? "⇧" "Shift")
                          "alt"   (if mac? "⌥" "Alt")
                          ;; goog names these keys `pg-up` / `pg-down`
                          ;; (goog/events/keynames.js). The upper-case fallback
                          ;; below would render them "PG-UP" / "PG-DOWN".
                          "pg-up"   (if mac? "⇞" "PgUp")
                          "pg-down" (if mac? "⇟" "PgDn")
                          "enter"   (if mac? "↩" "Enter")
                          (str/upper-case p)))]
       (str/join (if mac? "" "+") (map part (str/split eff #"\+")))))))

(defn palette-listed
  "Entries the palette shows on `active-tab`: not :palette-hidden and
   :when either nil or containing the tab. Availability beyond this
   (payload resolvable from context) is checked by command-bus."
  [active-tab]
  (for [[id {:keys [when palette-hidden] :as entry}] registry
        :when (and (not palette-hidden)
                   (or (nil? when) (contains? when active-tab)))]
    (assoc entry :id id)))

;; ── Bump authority (server) ────────────────────────────────────────────────
;; The ONLY place invalidation channel atoms are incremented.

#?(:clj
   (defn bump-channels!
     "Increment each channel counter for `user-id`. Pre: channels ⊆
      invalidation-channels (violation = caller bug). Returns :done — a
      definite value, so Electric callers can sequence on it with `case`."
     [user-id channels]
     {:pre [(set/subset? (set channels) invalidation-channels)]}
     (doseq [ch channels]
       (swap! (us/get-atom user-id ch) inc))
     :done))

#?(:clj
   (defn bump!
     "Bump the channels declared in `command-id`'s :views. Pre: command-id is
      registered (violation = caller bug). The universal post-effect call:
      the optimistic pump calls it after run-command!; command boundaries
      outside the pump (REST handlers, Electric form flows, :electric-exec
      terminal steps) call it directly. Returns :done."
     [user-id command-id]
     (let [entry (command command-id)]
       (assert entry (str "bump! on unregistered command " command-id))
       (bump-channels! user-id (:views entry #{})))))

;; ── Registry validation ────────────────────────────────────────────────────

(defn validate-registry
  "Returns a seq of violation strings; empty = valid. Checked by
   freememo.commands-test and safe to call at REPL."
  []
  (concat
    (for [[id {:keys [views]}] registry
          :let [bad (set/difference (set views) invalidation-channels)]
          :when (seq bad)]
      (str id " declares unknown channels " bad))
    (for [[id {:keys [class]}] registry
          :when (not (#{:mutation :query :nav} class))]
      (str id " has invalid :class " class))
    (let [binds (for [[id {:keys [bind]}] registry :when bind] [bind id])
          dupes (for [[bind ids] (group-by first binds)
                      :when (> (count ids) 1)]
                  [bind (map second ids)])]
      (for [[bind ids] dupes]
        (str "duplicate :bind " bind " on " (vec ids))))))
