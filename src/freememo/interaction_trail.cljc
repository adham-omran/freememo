(ns freememo.interaction-trail
  "Client→server interaction trail — the reach counterpart to
   freememo.client-errors.

   Command dispatch, tab navigation and the PDF page controls run entirely in the
   browser. They mutate no server row, so `log/audit!` never sees them and the
   server log cannot answer which commands users reach for, which tabs they open,
   or whether a keyboard chord is ever pressed. `record!` enqueues one
   interaction from any JS callback; `InteractionForwarder` ships each entry to
   the server exactly once — the same client→server bridge idiom as
   ClientErrorForwarder and freememo.optimistic/CommandDispatcher.

   Server-side interactions (search) call `log/interaction!` directly and never
   touch this queue; they carry session-id nil for that reason.

   Invariant: the queue holds at most `queue-cap` entries. A trail is diagnostic,
   never content — so when the socket is down the OLDEST entries are dropped and
   a long offline stretch cannot grow the tab's heap without bound. Losing trail
   entries is correct here; freememo.client-errors keeps an unbounded queue
   because its entries can carry :notify text a user must see.

   Known and accepted loss: a full DOCUMENT load (typed URL, hard reload, tab
   close) discards whatever the queue still holds, because the queue is a
   client-only atom. Measured: a palette command that navigates records its
   :command entry after the effect, so it sits behind its own :nav entry and is
   lost if a document load lands in that window. In-app navigation is pushState
   and does not unload, so this only bites the rare real page load; closing it
   would need an HTTP beacon endpoint, which is more surface than a diagnostic
   trail earns."
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.client-state :as cs]
   [freememo.logging :as log]))

(def queue-cap
  "Maximum pending trail entries. Chosen to outlast a normal reconnect (an
   active reader emits single-digit interactions per minute) while bounding the
   heap at a few tens of KB."
  500)

;; Defined on both peers so InteractionForwarder resolves under the JVM Electric
;; compiler; only ever written/read on the client (e/client scope).
(defonce ^{:doc "Client-side queue of pending trail entries:
                 [{:id uuid :kind kw :detail map} …]."}
  !queue (atom []))

(defn record!
  "Enqueue one client interaction for server-side logging. Safe to call from any
   JS callback. No-op on the server. Returns nil.

   Pre  : kind ∈ log/interaction-kinds; detail is plain serializable data (it
          crosses the Electric wire) carrying no user content — a search's text,
          a topic title and a card's fields are all forbidden, an id, a count and
          an enum are all fine.
   Post : the queue ends with this entry and holds at most queue-cap entries."
  [kind detail]
  #?(:cljs
     (swap! !queue
       (fn [q]
         (let [q' (conj q {:id (random-uuid) :kind kind :detail detail})
               n  (count q')]
           (if (> n queue-cap)
             ;; vec, not a bare subvec: a subvec retains its parent, so a chain
             ;; of them at the cap would pin every dropped entry.
             (vec (subvec q' (- n queue-cap)))
             q')))))
  nil)

(defn log-interaction!
  "Log one queued interaction on the server.
   Pre  : user-id may be nil (pre-auth surfaces); kind/detail as recorded.
   Post : one :info signal is recorded. Returns :logged (truthy — the forwarder
          spends its token on it).
   Inv  : never throws; a logging failure must not strand the queue entry, which
          would stall the pump behind an entry it can never drain."
  [user-id session-id kind detail]
  #?(:clj (do (try (log/interaction! user-id session-id kind detail)
                   (catch Exception _ nil))
              :logged)
     :cljs nil))

(e/defn InteractionForwarder
  "Always-mounted, headless pump: ships each recorded interaction to the server
   exactly once, then drops it from the queue. Mount once in Main, sibling to
   ClientErrorForwarder. Pre: user-id the current user (may be nil pre-auth)."
  [user-id]
  (e/client
    ;; sid MUST be let-bound at client scope. A bare cs/session-id inside the
    ;; (e/server …) form below is a SERVER-side var read, and the var's :clj
    ;; branch is nil — so the id would be nil on every entry, silently, which is
    ;; exactly the defect freememo.client-errors shipped with.
    (let [sid cs/session-id]
      (e/for [ev (e/diff-by :id (e/watch !queue))]
        (let [[t _] (e/Token (:id ev))]
          (when t
            (when-some [_ (e/server (log-interaction! user-id sid
                                      (:kind ev) (:detail ev)))]
              (t)
              (swap! !queue (fn [q] (filterv #(not= (:id ev) (:id %)) q))))))))))
