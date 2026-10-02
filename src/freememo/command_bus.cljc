(ns freememo.command-bus
  "Client-side command dispatch. All three invocation frontends — buttons,
   keyboard shortcuts, command palette — funnel through dispatch!; the
   registry entry's exec mode (freememo.commands/exec-mode) decides the path:

     :client   a synchronous client handler runs immediately
               (navigation, modal toggles, queries)
     :queue    the invocation is shipped to the server's optimistic queue
               by QueueInvoker (mounted once in Main); the pump executes the
               run-command! method and bumps the command's :views
     :electric a mounted Electric component owns the flow (e.g. AnkiConnect
               fetches run in the browser); it consumes its invocations via
               (Requests <command-id>) + consume!
     :ui-button a mounted button component owns the flow and publishes an
               invoke handle via publish-invoker!; dispatch! calls it.
               Availability = the handle is published (button mounted &
               enabled), replacing the old hidden-button-ref nil check.

   Context: mounted views publish what commands need (active tab, navigate!,
   topic-id, modal atoms, …) into !ctx via publish-ctx!/retract-ctx!.
   Publishers MUST retract on unmount (e/on-unmount) — a stale key makes a
   command look available after its owner is gone (violation = publisher bug).

   Availability contract: a command is available iff its :when set (if any)
   contains the active tab AND its preparer (if any) returns non-nil payload
   from the current context. dispatch! on an unavailable command is a safe
   no-op returning :unavailable.

   Payload contract: payloads cross the Electric wire for :queue commands —
   they MUST be plain serializable data (violation = preparer/caller bug).

   Design note: this is a deliberate hand-rolled pub/sub, not forms.md's
   e/amb service pattern — commands fire app-wide from keyboard/palette,
   but the button that owns a command's flow may be off-page (unmounted)
   at invocation time, so there is no single reactive scope to run an
   e/amb service in. QueueInvoker below IS the idiomatic service-pattern
   part: it owns the :queue lane's reactive lifecycle via e/diff-by +
   e/Token, same as any other Electric service."
  (:require [hyperfiddle.electric3 :as e]
            [freememo.commands :as commands]
            [freememo.interaction-trail :as trail]
            [freememo.navigation :as nav]
            [freememo.optimistic :as opt]))

;; ── Context ────────────────────────────────────────────────────────────────

(defonce !ctx (atom {}))

(defn publish-ctx!
  "Merge `m` into the command context. Returns nil."
  [m]
  (swap! !ctx merge m)
  nil)

(defn retract-ctx!
  "Remove `ks` from the command context. Call from e/on-unmount. Returns nil."
  [ks]
  (swap! !ctx #(apply dissoc % ks))
  nil)

;; ── Client handlers and preparers ──────────────────────────────────────────
;; Registered at namespace load by the ns that owns the behavior — the client
;; mirror of (defmethod optimistic/run-command! …) on the server.

(defonce !handlers (atom {}))   ; command-id → (fn [ctx payload])
(defonce !preparers (atom {}))  ; command-id → (fn [ctx] payload | nil=unavailable)

(defn register-handler!
  "Client handler for a :client-exec command. Last registration wins."
  [command-id f]
  (swap! !handlers assoc command-id f)
  nil)

(defn register-preparer!
  "Payload preparer: builds a command's payload from the context when the
   invoker (keyboard/palette) has none. Returning nil marks the command
   unavailable. Must be side-effect-free apart from reads (may read DOM
   selection)."
  [command-id f]
  (swap! !preparers assoc command-id f)
  nil)

;; ── Invoker handles (:ui-button exec) ──────────────────────────────────────
;; A mounted button component owns an Electric flow (tokens, e/server calls in
;; its own reactive scope) that cannot run outside it. The component publishes
;; a 0-arg invoke handle (typically #(.click node)) so keyboard/palette/menus
;; trigger the SAME flow the visible button runs.

(defonce !invokers (atom {}))   ; command-id → {publisher-key → 0-arg client fn}

(def ^:private sole-publisher
  "The publisher key a 2-arity publish uses.

   Most :ui-button commands have exactly one component that can be mounted, so
   they need no key and share this one. A command with two or more possible
   publishers MUST pass distinct keys — see the 3-arity."
  ::sole)

(defn publish-invoker!
  "Publish the invoke handle for a :ui-button command. The publisher MUST
   retract on unmount (e/on-unmount) — see the context contract above.

   Pre : with the 3-arity, `publisher-key` is unique per mounted publisher and
         equal between a publish and its matching retract. The publisher's DOM
         node is the natural choice: every publisher already holds one, and two
         mounted components can never share it.
   Post: the command is available while at least ONE publisher holds it.

   The key exists because the registry is per command, not per component, and
   two components CAN publish the same command at once. :copy-card is the
   deliberate case — one command, three surfaces (the card edit modal, the
   occlusion modal, the score toolbar), and a score topic holding a basic card
   mounts two of them together. Without a key the second publish overwrites the
   first, and whichever unmounts first retracts the command out from under the
   one still on screen, leaving a visible button the palette calls unavailable.

   :done has the same shape today (content_toolbar_extract and pdf_toolbar both
   publish it on a PDF page) and has not been migrated."
  ([command-id f] (publish-invoker! command-id sole-publisher f))
  ([command-id publisher-key f]
   (swap! !invokers update command-id assoc publisher-key f)
   nil))

(defn retract-invoker!
  "Remove one publisher's handle. Pre: `publisher-key` matches the publish.
   Post: the command-id key is gone once its LAST publisher retracts, so
   `available?`'s contains? check stays exact."
  ([command-id] (retract-invoker! command-id sole-publisher))
  ([command-id publisher-key]
   (swap! !invokers
     (fn [m]
       (let [remaining (dissoc (get m command-id) publisher-key)]
         (if (seq remaining)
           (assoc m command-id remaining)
           (dissoc m command-id)))))
   nil))

(defn- invoker-for
  "Any one mounted publisher's handle, or nil.
   Post: with several publishers the choice is arbitrary but every candidate is
   mounted, so whichever runs is a live button. Only :copy-card reaches this,
   and its three surfaces are mutually exclusive in what a user is looking at."
  [command-id]
  (first (vals (get @!invokers command-id))))

;; ── Invocation queue (for :queue and :electric exec) ───────────────────────

(defonce !invocations (atom []))  ; [{:invocation-id :command-id :payload}]

(defn consume!
  "Remove one invocation. QueueInvoker and :electric owners MUST call this
   exactly once per handled invocation (missing = replay on remount)."
  [invocation-id]
  (swap! !invocations (fn [v] (filterv #(not= invocation-id (:invocation-id %)) v)))
  nil)

;; ── Dispatch ───────────────────────────────────────────────────────────────

(defn- run-nav!
  "Generic :nav execution from pure registry data (:nav-tab + payload)."
  [{:keys [nav-tab] :as entry} ctx payload]
  (when-let [navigate! (:navigate! ctx)]
    (case (:id entry)
      :open-topic          (navigate! nav-tab (nav/nav-topic (:topic-id payload) (:origin payload)))
      :start-learn-session (navigate! nav-tab (nav/nav-learn-session))
      :open-subset-review  (navigate! nav-tab (nav/nav-subset-review (:root-id payload) (:root-name payload)))
      (navigate! nav-tab))))

(defn applies-here?
  "True when `command-id`'s declared surface includes the active tab — the
   :when half of the availability contract, WITHOUT the can-act-right-now half.

   The keyboard layer needs the halves apart, because they answer different
   questions. `applies-here?` answers \"is this key the app's on this screen\",
   which is a property of the surface and holds even while no button is
   mounted to serve it. `available?` answers \"can the app act on it right
   now\", which a collapsed toolbar or a non-PDF document can flip at any
   moment. Suppressing the browser default on the second one sends ⌘S to
   Chrome's Save Page dialog on every non-PDF topic — see
   plans/short-keyboard-chords.md.

   Post: true iff the entry exists and its :when is nil or contains the active
         tab. Implies nothing about whether dispatch! will act."
  ([command-id] (applies-here? command-id @!ctx))
  ([command-id ctx]
   (let [{active-when :when :as entry} (commands/command command-id)]
     (boolean (and entry
                   (or (nil? active-when)
                       (contains? active-when (:active-tab ctx))))))))

(defn available?
  "See ns docstring for the availability contract.
   Post: available? ⇒ applies-here? — the surface check is the first half."
  ([command-id] (available? command-id @!ctx))
  ([command-id ctx]
   (let [entry (commands/command command-id)]
     (boolean
       (and entry
            (applies-here? command-id ctx)
            (if (= :ui-button (commands/exec-mode entry))
              (contains? @!invokers command-id)
              (if-let [prepare (get @!preparers command-id)]
                (some? (prepare ctx))
                true)))))))

(def invocation-sources
  "Which frontend invoked a command. :button is every in-page control — toolbar
   buttons, dropdown rows, the undo-history list — and is the default because a
   control that reaches dispatch! without saying otherwise IS one. The keyboard
   handler and the palette name themselves, so the trail can answer whether a
   bound chord is ever pressed."
  #{:button :keyboard :palette})

(defn dispatch!
  "Invoke a command. With no `payload`, resolves one via the registered
   preparer (buttons that already hold their data pass it explicitly).
   `source` ∈ invocation-sources, default :button — see that var.
   Returns :dispatched | :unavailable | :unknown-command.
   Post: :dispatched ⇒ the effect ran (client) or exactly one invocation was
   queued (queue/electric); exactly one :command trail entry is recorded on
   EVERY path, including the two refusals — an :unavailable a user keeps
   retrying is the signal that a command's availability rule is wrong, and
   dropping it would hide that."
  ([command-id] (dispatch! command-id nil :button))
  ([command-id payload] (dispatch! command-id payload :button))
  ([command-id payload source]
   (let [entry (some-> (commands/command command-id) (assoc :id command-id))
         ctx @!ctx
         result
         (cond
           (nil? entry) :unknown-command
           (and (nil? payload) (not (available? command-id ctx))) :unavailable
           :else
           (let [payload (or payload
                             (when-let [prepare (get @!preparers command-id)]
                               (prepare ctx))
                             {})]
             (case (commands/exec-mode entry)
               :client
               (if-let [handle (get @!handlers command-id)]
                 (do (handle ctx payload) :dispatched)
                 (if (:nav-tab entry)
                   (do (run-nav! entry ctx payload) :dispatched)
                   :unavailable))

               :ui-button
               (if-let [invoke (invoker-for command-id)]
                 (do (invoke) :dispatched)
                 :unavailable)

               (:queue :electric)
               (do (swap! !invocations conj {:invocation-id (random-uuid)
                                             :command-id command-id
                                             :payload payload})
                   :dispatched))))]
     ;; Payload is deliberately NOT recorded — it carries user content (a search
     ;; string, a pasted card body). Command id, source, exec mode and outcome
     ;; are what the trail is for.
     (trail/record! :command {:command-id command-id
                              :source source
                              :exec (some-> entry commands/exec-mode)
                              :active-tab (:active-tab ctx)
                              :result result})
     result)))

;; ── Electric bridges ───────────────────────────────────────────────────────

(e/defn QueueInvoker
  "Ships :queue-exec invocations to the server's optimistic queue, exactly
   once each (token keyed by invocation id, mirroring CommandDispatcher).
   Mounted once in Main."
  [user-id]
  (e/client
    (e/for [inv (e/diff-by :invocation-id
                  (filterv #(= :queue (commands/exec-mode (commands/command (:command-id %))))
                    (e/watch !invocations)))]
      (let [[t _] (e/Token (:invocation-id inv))]
        (when t
          (when-some [_ (opt/enqueue! {:type (:command-id inv)
                                       :payload (:payload inv)})]
            (t)
            (consume! (:invocation-id inv))))))))

(e/defn Requests
  "Differential collection of pending invocations for `command-id` — the
   consumption side for :electric-exec commands. Owner pattern:
     (e/for [{:keys [invocation-id payload]} (bus/Requests :quick-sync)]
       …run the flow… (bus/consume! invocation-id))"
  [command-id]
  (e/client
    (e/diff-by :invocation-id
      (filterv #(= command-id (:command-id %)) (e/watch !invocations)))))

;; ── App-shell handlers ─────────────────────────────────────────────────────
;; Owned here because they act purely on shell context published by Main.

(register-handler! :open-undo-history
  (fn [ctx _payload]
    (some-> (:undo-modal-open-atom ctx) (reset! true))))

;; The panel is a toggle, not a destination: opening it changes no route, so the
;; handler flips the shell's atom rather than navigating. Same shape as
;; :open-undo-history above.
(register-handler! :open-activity
  (fn [ctx _payload]
    (some-> (:activity-open-atom ctx) (reset! true))))

(register-handler! :toggle-assistant-pane
  (fn [ctx _payload]
    (when-let [toggle! (:toggle-assistant-pane! ctx)]
      (toggle!))))

(register-handler! :toggle-left-pane
  (fn [ctx _payload]
    (when-let [toggle! (:toggle-left-pane! ctx)]
      (toggle!))))

(register-handler! :show-hierarchy
  (fn [ctx _payload]
    (when-let [show! (:show-left-pane! ctx)]
      (show! "hierarchy"))))

(register-handler! :show-pins
  (fn [ctx _payload]
    (when-let [show! (:show-left-pane! ctx)]
      (show! "pins"))))

;; The nav rail publishes all three while it is mounted, which is always in
;; the signed-in shell.
(register-handler! :toggle-nav-rail
  (fn [ctx _payload]
    (when-let [toggle! (:toggle-nav-rail! ctx)]
      (toggle!))))

(register-handler! :open-nav-rail
  (fn [ctx _payload]
    (when-let [open! (:open-nav-rail! ctx)]
      (open!))))

(register-handler! :go-continue
  (fn [ctx _payload]
    (when-let [open! (:open-continue! ctx)]
      (open!))))

;; The poem reader publishes :poem-topic-id while it is mounted, and it mounts
;; for exactly the topics that have a package — so an absent key IS "this is not
;; a poem". RightSidePanel owns both outcomes because the toast queue is a
;; server atom and a handler has no Electric scope to reach it from.
(register-handler! :show-poem-panel
  (fn [ctx _payload]
    (when-let [request! (:request-poem-panel! ctx)]
      (request! (:poem-topic-id ctx)))))

(register-handler! :toggle-card-table-view
  (fn [ctx _payload]
    (when-let [toggle! (:toggle-card-table! ctx)]
      (toggle!))))

;; DocumentMainColumn publishes :toggle-editor-pane! only for a document with a
;; source pane beside the editor (PDF, video, git forge file). :when can name
;; only a tab, so the preparer is the kind gate: an absent fn hides the command
;; (plans/editor-pane-toggle.md §4.2).
(register-preparer! :toggle-editor-pane
  (fn [ctx] (when (:toggle-editor-pane! ctx) {})))

(register-handler! :toggle-editor-pane
  (fn [ctx _payload]
    (when-let [toggle! (:toggle-editor-pane! ctx)]
      (toggle!))))

(register-handler! :search
  (fn [ctx payload]
    (when-let [navigate! (:navigate! ctx)]
      (if-let [text (not-empty (:text payload))]
        (navigate! :search (nav/nav-search-query text))
        (navigate! :search)))))
