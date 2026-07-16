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

(defonce !invokers (atom {}))   ; command-id → 0-arg client fn

(defn publish-invoker!
  "Publish the invoke handle for a :ui-button command. The publisher MUST
   retract on unmount (e/on-unmount) — see the context contract above."
  [command-id f]
  (swap! !invokers assoc command-id f)
  nil)

(defn retract-invoker!
  [command-id]
  (swap! !invokers dissoc command-id)
  nil)

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

(defn available?
  "See ns docstring for the availability contract."
  ([command-id] (available? command-id @!ctx))
  ([command-id ctx]
   (let [{active-when :when :as entry} (commands/command command-id)]
     (boolean
       (and entry
            (or (nil? active-when) (contains? active-when (:active-tab ctx)))
            (if (= :ui-button (commands/exec-mode entry))
              (contains? @!invokers command-id)
              (if-let [prepare (get @!preparers command-id)]
                (some? (prepare ctx))
                true)))))))

(defn dispatch!
  "Invoke a command. With no `payload`, resolves one via the registered
   preparer (buttons that already hold their data pass it explicitly).
   Returns :dispatched | :unavailable | :unknown-command.
   Post: :dispatched ⇒ the effect ran (client) or exactly one invocation was
   queued (queue/electric)."
  ([command-id] (dispatch! command-id nil))
  ([command-id payload]
   (let [entry (some-> (commands/command command-id) (assoc :id command-id))
         ctx @!ctx]
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
           (if-let [invoke (get @!invokers command-id)]
             (do (invoke) :dispatched)
             :unavailable)

           (:queue :electric)
           (do (swap! !invocations conj {:invocation-id (random-uuid)
                                         :command-id command-id
                                         :payload payload})
               :dispatched)))))))

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
          (when-some [_ (e/server (opt/enqueue-command! user-id {:type (:command-id inv)
                                                                 :payload (:payload inv)}))]
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

(register-handler! :search
  (fn [ctx payload]
    (when-let [navigate! (:navigate! ctx)]
      (if-let [text (not-empty (:text payload))]
        (navigate! :search (nav/nav-search-query text))
        (navigate! :search)))))
