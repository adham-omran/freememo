(ns freememo.optimistic
  "Optimistic-update command pipeline.

   A modal enqueues a plain-data command and closes immediately; a single
   always-mounted CommandDispatcher (mounted in Main, sibling to ToastStack)
   runs each command's effect, toasts the outcome, and — for add-card — drives
   the :pending-cards row overlay. Decoupling the server work from the modal's
   lifecycle means closing (unmounting) the modal never tears down an
   in-flight save.

   Two per-user server atoms (freememo.user-state):
     :pending-commands  vector of {:id :type :payload} — the dispatch queue;
                        appended on submit, removed when the command completes.
     :pending-cards     map of tempid -> overlay entry — add-card row display
                        only (owned by the card table / card-modals).

   Command map contract:
     :id       unique per submit; supplied by the client for add-card (so the
               overlay entry and the command share identity), else generated
               here.
     :type     a freememo.commands registry id with a run-command! method;
               unknown types are dropped.
     :payload  type-specific map.

   Execution contract (see freememo.commands and README):
     run-command! methods own the EFFECT and its toast — nothing else.
     execute! (the pump's unit of work) then bumps the invalidation channels
     declared in the registry entry's :views and removes the command from the
     queue. Methods MUST NOT bump channels or call drop-command! themselves —
     a method that does is double-bumping (implementation bug)."
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.commands :as commands]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [taoensso.telemere :as tel])
   #?(:clj [freememo.toasts :as toasts])))

#?(:clj
   (defn enqueue-command!
     "Append `command` to the user's pending-commands queue; returns :enqueued.
      Pre: (:type command) has a run-command! method or the command is dropped.
      Generates :id when absent (fire-and-forget commands with no overlay)."
     [user-id command]
     (let [command (update command :id #(or % (java.util.UUID/randomUUID)))]
       (swap! (us/get-atom user-id :pending-commands) conj command)
       :enqueued)))

#?(:clj
   (defn drop-command!
     "Remove the command identified by `id` from the queue. Idempotent."
     [user-id id]
     (swap! (us/get-atom user-id :pending-commands)
       (fn [cmds] (filterv #(not= id (:id %)) cmds)))
     nil))

;; ---------------------------------------------------------------------------
;; :pending-cards overlay API (add-card optimistic rows).
;; Entry shape: {:tempid uuid :topic-id id :status :pending|:confirmed|:error
;;               :real-ids [id...] (confirmed) :error msg (error)
;;               :payload {:topic-id :root-topic-id :kind :card-data}}
;; ---------------------------------------------------------------------------

#?(:clj
   (defn enqueue-pending-card!
     "Register a :pending overlay row and enqueue its save command under `id`
      (shared by overlay entry and command, so they correlate). `id` is the
      client's idempotency key for one logical add; when nil a fresh one is
      minted (fire-and-forget callers with no client key). `type` is the
      run-command! dispatch keyword; it is remembered on the entry so retry
      re-enqueues the same command.

      Idempotent on `id`: a second call whose `id` is already an overlay entry
      is a no-op — the re-fired or double-submitted add does NOT enqueue a
      second command. The check-and-insert is one atomic swap-vals!, so two
      concurrent enqueues (each execute! runs on its own thread) of the same id
      still enqueue exactly once.

      Returns :enqueued (newly registered) or :duplicate (id already present)."
     ([user-id type payload] (enqueue-pending-card! user-id type nil payload))
     ([user-id type id payload]
      (let [id (or id (java.util.UUID/randomUUID))
            entry {:tempid id :topic-id (:topic-id payload) :status :pending
                   :command-type type :payload payload}
            [old _] (swap-vals! (us/get-atom user-id :pending-cards)
                      (fn [m] (if (contains? m id) m (assoc m id entry))))]
        (if (contains? old id)
          :duplicate
          (do (enqueue-command! user-id {:id id :type type :payload payload})
              :enqueued))))))

#?(:clj
   (defn enqueue-add-card!
     "enqueue-pending-card! for the :add-card command. `id` is the client's
      per-modal-open idempotency key; `payload` is
      {:topic-id :root-topic-id :kind :card-data}."
     ([user-id payload] (enqueue-add-card! user-id nil payload))
     ([user-id id payload] (enqueue-pending-card! user-id :add-card id payload))))

#?(:clj
   (defn retry-pending-card!
     "Reset an errored overlay row to :pending and re-enqueue its save; returns
      :done. No-op when the entry is gone."
     [user-id tempid]
     (when-let [entry (get @(us/get-atom user-id :pending-cards) tempid)]
       (swap! (us/get-atom user-id :pending-cards) update tempid merge {:status :pending :error nil})
       (enqueue-command! user-id {:id tempid
                                  :type (or (:command-type entry) :add-card)
                                  :payload (:payload entry)}))
     :done))

#?(:clj
   (defn forget-pending-card!
     "Remove an overlay entry — used both for user-dismiss of an errored row and
      for auto-cleanup once a confirmed card has landed. Idempotent; returns :done."
     [user-id tempid]
     (swap! (us/get-atom user-id :pending-cards) dissoc tempid)
     :done))

#?(:clj
   (defn visible-pending-cards
     "Overlay entries for `topic-id` that should render as rows, given
      `present-ids` (ids in the refetched card list). Confirmed entries hide
      once their cards land, so optimistic and real rows never duplicate."
     [pending-map topic-id present-ids]
     (->> (vals pending-map)
       (filter #(= topic-id (:topic-id %)))
       (filter (fn [{:keys [status real-ids]}]
                 (or (contains? #{:pending :error} status)
                     (and (= :confirmed status)
                          (seq real-ids)
                          (not (every? present-ids real-ids))))))
       vec)))

#?(:clj
   (defn landed-pending-tempids
     "Tempids of `topic-id`'s confirmed entries whose cards are now in
      `present-ids` (or that produced no id on a duplicate insert) — safe to
      forget."
     [pending-map topic-id present-ids]
     (->> pending-map
       (keep (fn [[tid entry]]
               (when (and (= topic-id (:topic-id entry))
                          (= :confirmed (:status entry))
                          (let [rids (:real-ids entry)]
                            (or (empty? rids) (every? present-ids rids))))
                 tid)))
       vec)))

#?(:clj
   (defmulti run-command!
     "Execute one command server-side: perform the effect, toast the outcome,
      update any overlay, and remove the command from the queue (see the ns
      invariant). Returns a non-nil status (:done). Dispatched on (:type
      command); domain namespaces provide the methods."
     (fn [_user-id command] (:type command))))

#?(:clj
   (defmethod run-command! :default [user-id command]
     (toasts/push! user-id {:level :warning
                            :message (str "Unknown action: " (pr-str (:type command)))})
     :done))

#?(:clj
   (defn execute!
     "The pump's unit of work: run the effect, bump the registry-declared
      :views, drop the command from the queue. Pre: command has :id and :type.
      Post: command absent from :pending-commands, its :views channels bumped
      exactly once (even when the method throws — the queue must not wedge,
      and a partial effect may still have written; a stale view hides a
      failure worse than a spurious refetch reveals one). Returns :done."
     [user-id command]
     (try
       (run-command! user-id command)
       (catch Throwable t
         (tel/error! {:id ::command-failed
                      :data {:user-id user-id :type (:type command)
                             :command-id (:id command)}}
           t)
         (toasts/push! user-id {:level :error
                                :message (str (name (:type command)) " failed: "
                                           (.getMessage t))}))
       (finally
         (when (commands/command (:type command))
           (commands/bump! user-id (:type command)))
         (drop-command! user-id (:id command))))
     :done))

(e/defn CommandDispatcher
  "Always-mounted, headless command pump. Watches the user's pending-commands
   queue and runs each command exactly once: a per-entry token (keyed by the
   command id) arms on mount and is spent when the offloaded execute!
   returns, which also removes the entry (unmounting the branch)."
  [user-id]
  (e/client
    (e/for [cmd (e/server (e/diff-by :id (e/watch (us/get-atom user-id :pending-commands))))]
      (let [[t _] (e/Token (:id cmd))]
        (when t
          (when-some [_ (e/server (e/Offload #(execute! user-id cmd)))]
            (t)))))))
