(ns freememo.client-errors
  "Client→server error trail (spec §4.1/§4.2).

   Client-side failures (raw JS promise .catch, event handlers) run OUTSIDE
   Electric's reactive frame, so they can't call e/server directly. `report!`
   enqueues onto a client-only atom; the always-mounted `ClientErrorForwarder`
   watches that queue and ships each entry to the server via e/server — the same
   client→server bridge idiom as freememo.optimistic/CommandDispatcher.

   `session-id` (freememo.client-state) is a stable per-page-load id; it tags
   every shipped error so one browser session's trail is joinable. It lives in
   client-state rather than here because freememo.interaction-trail tags its own
   entries with the same id — a session is not an error concept."
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.client-state :as cs]
   [freememo.logging :as log]))

;; Defined on both peers so the ClientErrorForwarder e/defn resolves under the
;; JVM Electric compiler; only ever written/read on the client (e/client scope).
(defonce ^{:doc "Client-side queue of pending client-error entries:
                 [{:id uuid :op kw-or-str :message str} …]."}
  !queue (atom []))

(defn report!
  "Enqueue a client-originating failure for server-side logging. Safe to call
   from any JS callback. `op` names the failed operation (keyword or string);
   `ex` is an error/string (str'd here). No-op on the server. Returns nil.

   This only logs. A failure the user must see goes where the user acted: the
   caller shows it on its own surface (an editor's status line, a menu row, an
   error beside a control). The log entry is for the operator."
  [op ex]
  #?(:cljs (swap! !queue conj {:id (random-uuid) :op op :message (str ex)}))
  nil)

(defn log-client-error!
  "Log one queued client failure.
   Pre  : user-id may be nil (pre-auth surfaces).
   Post : one :warn signal is recorded. Returns :logged (truthy — the
          forwarder spends its token on it).
   Inv  : never throws."
  [user-id session-id op message]
  #?(:clj (do (log/client-error! user-id session-id op message)
              :logged)
     :cljs nil))

(e/defn ClientErrorForwarder
  "Always-mounted, headless pump: ships each queued client error to the server
   (log/client-error!) exactly once, then drops it from the queue. Mount once in
   Main, sibling to CommandDispatcher. Pre: user-id the current user (may be nil
   for pre-auth surfaces)."
  [user-id]
  (e/client
    ;; sid MUST be let-bound at client scope — a bare session-id inside the
    ;; (e/server …) form is a SERVER-side var read of a var whose :clj branch is
    ;; nil. Every ::client-error signal recorded before this binding existed
    ;; carries :session-id nil for that reason; they cannot be joined by session.
    (let [sid cs/session-id]
      (e/for [err (e/diff-by :id (e/watch !queue))]
        (let [[t _] (e/Token (:id err))]
          (when t
            (when-some [_ (e/server (log-client-error! user-id sid
                                      (:op err) (:message err)))]
              (t)
              (swap! !queue (fn [q] (filterv #(not= (:id err) (:id %)) q))))))))))
