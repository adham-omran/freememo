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
     :bind   string|nil    — goog KeyboardShortcutHandler spec (\"meta+shift+e\")
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
            #?(:clj [freememo.user-state :as us])))

;; ── Invalidation channels ──────────────────────────────────────────────────
;; The 10 per-user counter atoms in freememo.user-state. Views e/watch these;
;; bumping re-runs their queries.

(def invalidation-channels
  #{:refresh :credits-refresh :meta-refresh :settings-refresh
    :card-mutations :sync-mutations :tree-mutations
    :queue-mutations :pin-mutations :undo-mutations})

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
   "document"  #{:card-mutations :tree-mutations :refresh}})

;; ── Registry ───────────────────────────────────────────────────────────────

(def domain-registry
  "Mutation and modal-opener commands, grouped by execution shape.
   :views transcribed 1:1 from the pre-registry hand-written bump sites
   (:done/:restore carry the union of their topic/page/PDF contexts).
   Empty :views = either a pure UI opener or a runtime-dynamic bump
   (bump-channels! inside the effect — undo, staged delete)."
  (merge
    ;; — Optimistic-queue mutations (run-command! methods; the pump bumps) —
    {:add-card         {:label "Add card" :class :mutation :when #{:viewer}
                        :palette-hidden true ; submitted by AddCardModal; :add-new opens it
                        :data {:topic-id :payload :root-topic-id :payload
                               :kind :payload :card-data :payload}
                        :table #{:flashcards} :views #{:card-mutations}}
     :save-biblio      {:label "Save bibliography" :class :mutation :palette-hidden true
                        :data {:topic-id :payload :data :payload}
                        :table #{:sources :topics} :views #{:refresh}}
     :add-occlusion    {:label "Add occlusion cards" :class :mutation :palette-hidden true
                        :table #{:flashcards :occlusion_groups} :views #{:card-mutations}}
     :update-occlusion {:label "Update occlusion" :class :mutation :palette-hidden true
                        :table #{:flashcards :occlusion_groups} :views #{:card-mutations}}
     :add-score-group    {:label "Add score cards" :class :mutation :palette-hidden true
                          :table #{:flashcards :score_groups :media} :views #{:card-mutations}}
     :update-score-group {:label "Update score card" :class :mutation :palette-hidden true
                          :table #{:flashcards :score_groups :media} :views #{:card-mutations}}
     :undo-newest      {:label "Undo last action" :class :mutation :bind "meta+shift+z"
                        :table #{:undo_log :flashcards :topics :topic_pins :settings}
                        :views #{}} ; runtime-dynamic: undo/bump-views! by entity type
     :undo-entry       {:label "Undo action" :class :mutation :palette-hidden true
                        :data {:id :payload}
                        :table #{:undo_log :flashcards :topics :topic_pins :settings}
                        :views #{}}} ; runtime-dynamic, as :undo-newest

    ;; — Toolbar flows owned by mounted components (:exec :ui-button) —
    {:extract        {:label "Extract selection" :class :mutation :exec :ui-button
                      :bind "meta+shift+e" :when #{:viewer}
                      :data {:selection :dom} :table #{:topics} :views #{:tree-mutations}}
     :generate       {:label "Generate cards" :class :mutation :exec :ui-button
                      :bind "meta+shift+g" :when #{:viewer}
                      :table #{:flashcards} :views #{:card-mutations}}
     :gen-prompt     {:label "Generate with prompt…" :class :mutation :exec :ui-button
                      :when #{:viewer} :views #{}} ; opens dialog; the run bumps :generate
     :scan           {:label "Scan page (OCR)" :class :mutation :exec :ui-button
                      :bind "meta+shift+s" :when #{:viewer}
                      :table #{:topics} :views #{:refresh}}
     :compare-ocr    {:label "Compare OCR models" :class :mutation :exec :ui-button
                      :when #{:viewer} :table #{:topics} :views #{:refresh}}
     :copy-text      {:label "Copy text" :class :mutation :exec :ui-button
                      :when #{:viewer} :table #{:topics} :views #{:refresh}}
     :copy-all       {:label "Copy all text" :class :mutation :exec :ui-button
                      :when #{:viewer} :table #{:topics} :views #{:refresh}}
     :done           {:label "Mark done" :class :mutation :exec :ui-button
                      :bind "meta+shift+d" :when #{:viewer}
                      :table #{:topics} :views #{:refresh :meta-refresh}}
     :restore        {:label "Restore (un-done)" :class :mutation :exec :ui-button
                      :when #{:viewer} :table #{:topics} :views #{:refresh :meta-refresh}}
     :advance-topic  {:label "Next topic" :class :mutation :exec :ui-button
                      :when #{:viewer} :table #{:topic_repetitions} :views #{:queue-mutations}}
     :postpone-topic {:label "Postpone topic" :class :mutation :exec :ui-button
                      :when #{:viewer} :table #{:topic_repetitions} :views #{:queue-mutations}}
     :add-new        {:label "Add new card…" :class :nav :exec :ui-button
                      :when #{:viewer} :views #{}}
     :export         {:label "Export cards…" :class :query :exec :ui-button
                      :when #{:viewer} :views #{}}
     :delete-document {:label "Delete document…" :class :mutation :exec :ui-button
                       :when #{:viewer} :table #{:topics :flashcards :undo_log}
                       :views #{}} ; runtime-conditional: staged-delete/stage-deletion!
     :anki-sync      {:label "Push to Anki…" :class :mutation :exec :ui-button
                      :bind "meta+shift+x" :when #{:viewer}
                      :table #{:flashcards} :views #{:sync-mutations}}
     :quick-sync     {:label "Quick Anki push" :class :mutation :exec :ui-button
                      :bind "meta+shift+alt+x" :when #{:viewer}
                      :table #{:flashcards} :views #{}} ; record step bumps :anki-sync
     :pull-anki      {:label "Pull from Anki" :class :mutation :exec :ui-button
                      :when #{:viewer} :table #{:flashcards} :views #{:sync-mutations}}}

    ;; — Import launchers (freememo.import-page registers the handlers:
    ;;   set the pending-import intent, navigate to :import, page opens the
    ;;   matching modal) —
    {:import-link    {:label "Import from link…" :class :nav}
     :import-upload  {:label "Upload file…" :class :nav}
     :import-audio   {:label "Import audio…" :class :nav}
     :import-score   {:label "Import score…" :class :nav}
     :import-paste   {:label "Paste content…" :class :nav}
     :new-topic      {:label "New topic…" :class :nav}
     :import-zotero  {:label "Import from Zotero…" :class :nav}
     :create-live-doc {:label "New live document" :class :mutation :exec :client
                       :table #{:topics}
                       :views #{}}} ; /api/create-live-doc bumps :import-document

    ;; — Boundary-declared mutations (bump! called at their server boundary;
    ;;   not directly invocable, so hidden from the palette) —
    {:edit-card       {:label "Edit card" :class :mutation :palette-hidden true
                       :table #{:flashcards} :views #{:card-mutations}}
     :delete-card     {:label "Delete card" :class :mutation :palette-hidden true
                       :table #{:flashcards :undo_log} :views #{:card-mutations}}
     :bulk-delete-cards {:label "Delete cards" :class :mutation :palette-hidden true
                         :table #{:flashcards :undo_log} :views #{:card-mutations}}
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
     :push-biblio     {:label "Push bibliography to children" :class :mutation :palette-hidden true
                       :table #{:sources :topics} :views #{:refresh}}
     :refetch-biblio  {:label "Refetch bibliography" :class :mutation :palette-hidden true
                       :table #{:sources} :views #{:refresh}}
     :import-document {:label "Import document" :class :mutation :palette-hidden true
                       :table #{:topics :topic_files :sources} :views document-views}
     :append-images   {:label "Append images" :class :mutation :palette-hidden true
                       :table #{:topics :topic_files} :views document-views}
     :credits-granted {:label "Credits granted" :class :mutation :palette-hidden true
                       :table #{:credit_orders :credit_transactions} :views #{:credits-refresh}}}))

(def registry
  (merge
    ;; — Navigation (client-only, no DB footprint) —
    ;; :nav-tab / nav constructors are consumed by the generic nav handler in
    ;; freememo.command-bus.
    {:go-home       {:label "Go to Home"     :class :nav :nav-tab :home}
     :go-learn      {:label "Go to Learn"    :class :nav :nav-tab :learn}
     :go-viewer     {:label "Go to Viewer"   :class :nav :nav-tab :viewer}
     :go-library    {:label "Go to Library"  :class :nav :nav-tab :library}
     :go-search     {:label "Go to Search"   :class :nav :nav-tab :search}
     :go-import     {:label "Go to Import"   :class :nav :nav-tab :import}
     :go-settings   {:label "Go to Settings" :class :nav :nav-tab :settings}
     :open-topic    {:label "Open topic"     :class :nav :nav-tab :viewer
                     :data {:topic-id :payload} :palette-hidden true}
     :start-learn-session {:label "Start learn session" :class :nav :nav-tab :viewer}
     :open-subset-review  {:label "Review subset" :class :nav :nav-tab :viewer
                           :data {:root-id :payload :root-name :payload}
                           :palette-hidden true}

     ;; — App shell —
     :toggle-palette    {:label "Command palette" :class :nav :bind "meta+k"}
     :open-undo-history {:label "Action history"  :class :nav}

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
