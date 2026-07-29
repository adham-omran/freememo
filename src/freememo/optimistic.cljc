(ns freememo.optimistic
  "Optimistic-update command pipeline (per-session, client-local).

   A modal enqueues a plain-data command and closes immediately; a single
   always-mounted CommandDispatcher (mounted in Main, sibling to ToastStack)
   runs each command's effect server-side, and the client applies the outcome to
   the :pending-cards row overlay. Decoupling the effect from the modal's
   lifecycle means closing (unmounting) the modal never tears down an in-flight
   save.

   Two per-SESSION client atoms (freememo.client-state):
     :pending-commands  vector of {:id :type :payload} — this tab's dispatch
                        queue; appended on submit, removed when the command acks.
     :pending-cards     map of tempid -> overlay entry — add-* row display only.

   Per-session (not per-user) is the fix for cross-tab duplication: each tab
   drains its OWN queue, so a command is executed exactly once — by the tab that
   enqueued it. Invalidation channels (freememo.user-state) stay per-user/shared,
   so a card added in one tab still refreshes the other tab's card table.

   Command map contract:
     :id       unique per submit; supplied by the client for add-* (so the
               overlay entry and the command share identity), else generated.
     :type     a freememo.commands registry id with a run-command! method;
               unknown types toast a warning.
     :payload  type-specific map.

   Execution contract:
     run-command! methods own the EFFECT and its toast, and RETURN an outcome map
     {:ok? bool :real-ids [id...]? :error str?}. They MUST NOT touch the queue,
     the overlay, or invalidation channels. run-command-effect! bumps the
     registry :views; the client dispatcher acks — applying the outcome to the
     overlay and dropping the command from the queue."
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.client-state :as cs]
   [freememo.commands :as commands]
   #?(:clj [taoensso.telemere :as tel])
   #?(:clj [freememo.toasts :as toasts])))

;; ---------------------------------------------------------------------------
;; Client queue + overlay ops. Plain cljc; every call site is e/client-sited,
;; so these mutate this session's browser-local atoms.
;; ---------------------------------------------------------------------------

(defn enqueue!
  "Append `command` to this session's queue; returns :enqueued. Generates :id
   when absent (fire-and-forget commands with no overlay)."
  [command]
  (let [command (update command :id #(or % (random-uuid)))]
    (swap! (cs/get-atom :pending-commands) conj command)
    :enqueued))

;; :pending-cards overlay API (add-* optimistic rows).
;; Entry shape: {:tempid uuid :topic-id id :status :pending|:confirmed|:error
;;               :real-ids [id...] (confirmed) :error msg (error)
;;               :command-type kw :payload {:topic-id :root-topic-id :kind …}}

(defn enqueue-pending!
  "Register a :pending overlay row and enqueue its command under `id` (shared by
   overlay entry and command, so they correlate). `id` is the client's
   idempotency key for one logical add; when nil a fresh one is minted. `type`
   is the run-command! dispatch keyword; it is remembered on the entry so retry
   re-enqueues the same command.

   Idempotent on `id`: a second call whose `id` is already an overlay entry is a
   no-op (a double-submit within this tab enqueues once). The check-and-insert is
   one swap-vals! — the client is single-threaded, so it is exactly-once here.

   Returns :enqueued (newly registered) or :duplicate (id already present)."
  ([type payload] (enqueue-pending! type nil payload))
  ([type id payload]
   (let [id (or id (random-uuid))
         entry {:tempid id :topic-id (:topic-id payload) :status :pending
                :command-type type :payload payload}
         [old _] (swap-vals! (cs/get-atom :pending-cards)
                   (fn [m] (if (contains? m id) m (assoc m id entry))))]
     (if (contains? old id)
       :duplicate
       (do (enqueue! {:id id :type type :payload payload})
           :enqueued)))))

(defn enqueue-add-card!
  "enqueue-pending! for the :add-card command. `id` is the client's
   per-modal-open idempotency key; `payload` is
   {:topic-id :root-topic-id :kind :card-data}."
  ([payload] (enqueue-add-card! nil payload))
  ([id payload] (enqueue-pending! :add-card id payload)))

(defn retry-pending!
  "Reset an errored overlay row to :pending and re-enqueue its command; returns
   :done. No-op when the entry is gone."
  [tempid]
  (when-let [entry (get @(cs/get-atom :pending-cards) tempid)]
    (swap! (cs/get-atom :pending-cards) update tempid merge {:status :pending :error nil})
    (enqueue! {:id tempid
               :type (or (:command-type entry) :add-card)
               :payload (:payload entry)}))
  :done)

(defn forget-pending!
  "Remove an overlay entry — user-dismiss of an errored row, or auto-cleanup once
   a confirmed card has landed. Idempotent; returns :done."
  [tempid]
  (swap! (cs/get-atom :pending-cards) dissoc tempid)
  :done)

(defn ack-command!
  "Apply a completed command's `result` ({:ok? :real-ids :error}) to this
   session's overlay (if it has an entry for the command) and drop the command
   from the queue. Runs on the client, driven by the dispatcher on effect
   completion. Idempotent on a missing overlay entry (fire-and-forget commands)."
  [command result]
  (let [id (:id command)]
    (swap! (cs/get-atom :pending-cards)
      (fn [m] (if (contains? m id)
                (update m id merge (if (:ok? result)
                                     {:status :confirmed :real-ids (:real-ids result)}
                                     {:status :error :error (:error result)}))
                m)))
    (swap! (cs/get-atom :pending-commands)
      (fn [cmds] (filterv #(not= id (:id %)) cmds)))
    nil))

(defn visible-pending-cards
  "Overlay entries for `topic-id` that should render as rows, given `present-ids`
   (ids in the refetched card list). Confirmed entries hide once their cards
   land, so optimistic and real rows never duplicate."
  [pending-map topic-id present-ids]
  (->> (vals pending-map)
    (filter #(= topic-id (:topic-id %)))
    (filter (fn [{:keys [status real-ids]}]
              (or (contains? #{:pending :error} status)
                  (and (= :confirmed status)
                       (seq real-ids)
                       (not (every? present-ids real-ids))))))
    vec))

(defn landed-pending-tempids
  "Tempids of `topic-id`'s confirmed entries whose cards are now in `present-ids`
   (or that produced no id on a duplicate insert) — safe to forget."
  [pending-map topic-id present-ids]
  (->> pending-map
    (keep (fn [[tid entry]]
            (when (and (= topic-id (:topic-id entry))
                       (= :confirmed (:status entry))
                       (let [rids (:real-ids entry)]
                         (or (empty? rids) (every? present-ids rids))))
              tid)))
    vec))

;; ---------------------------------------------------------------------------
;; Server effect. run-command! methods (domain namespaces) perform the effect,
;; toast, and RETURN an outcome map — no queue/overlay/channel side effects.
;; ---------------------------------------------------------------------------

#?(:clj
   (defmulti run-command!
     "Perform one command's effect server-side and toast the outcome. Returns
      {:ok? bool :real-ids [id...]? :error str?}. MUST NOT touch the queue, the
      overlay, or invalidation channels — run-command-effect! bumps :views and
      the client dispatcher acks. Dispatched on (:type command); domain
      namespaces provide the methods."
     (fn [_user-id command] (:type command))))

#?(:clj
   (defmethod run-command! :default [user-id command]
     (toasts/push! user-id {:level :warning
                            :message (str "Unknown action: " (pr-str (:type command)))})
     {:ok? false :error "unknown command"}))

(defn run-command-effect!
  "Server unit of work: run the effect, bump the registry-declared :views, and
   return the outcome map to the client (which acks: overlay + queue drop). Pre:
   command has :id and :type. Post: :views bumped exactly once (even when the
   method throws — a partial effect may still have written, and a stale view
   hides a failure worse than a spurious refetch reveals one). Never mutates the
   queue or overlay (those are per-session client state).

   Body-level #?(:clj …): the client dispatcher names this var inside
   (e/server (e/Offload #(run-command-effect! …))), and e/server does NOT strip
   the symbol from CLJS compilation — so it MUST resolve on the CLJS peer too, or
   the dual compile emits mismatched frame-signal counts (ArrayIndexOutOfBounds
   on the wire). :cljs is a never-called nil placeholder."
  [user-id command]
  #?(:clj
     (do
       (tel/log! {:level :info :id ::execute-command
                  :data {:user-id user-id :type (:type command) :command-id (:id command)}}
         "execute command")
       (let [result (try
                      (run-command! user-id command)
                      (catch Throwable t
                        (tel/error! {:id ::command-failed
                                     :data {:user-id user-id :type (:type command)
                                            :command-id (:id command)}}
                          t)
                        (toasts/push! user-id {:level :error
                                               :message (str (name (:type command)) " failed: "
                                                          (.getMessage t))})
                        {:ok? false :error (.getMessage t)}))]
         (when (commands/command (:type command))
           (commands/bump! user-id (:type command)))
         result))
     :cljs nil))

(e/defn CommandDispatcher
  "Always-mounted, headless command pump. Drains THIS session's client queue
   (freememo.client-state :pending-commands): a per-command token (keyed by the
   command id) arms on mount, the effect runs server-side via e/Offload, and on
   completion the client acks — applying the outcome to the overlay and dropping
   the command (which unmounts the branch). Per-session: two tabs each drain
   their own queue, so a command is executed exactly once, by the tab that
   enqueued it."
  [user-id]
  (e/client
    (e/for [cmd (e/diff-by :id (e/watch (cs/get-atom :pending-commands)))]
      (let [[t _] (e/Token (:id cmd))]
        (when t
          (let [result (e/server (e/Offload #(run-command-effect! user-id cmd)))]
            (when (some? result)
              ;; ack (overlay + queue drop) mutates the queue → unmounts this
              ;; branch, so per the token lifecycle it fires from e/on-unmount
              ;; when (t) spends the token.
              (e/on-unmount #(ack-command! cmd result))
              (t))))))))
