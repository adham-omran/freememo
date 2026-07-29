(ns freememo.client-errors
  "Client→server error trail (spec §4.1/§4.2).

   Client-side failures (raw JS promise .catch, event handlers) run OUTSIDE
   Electric's reactive frame, so they can't call e/server directly. `report!`
   enqueues onto a client-only atom; the always-mounted `ClientErrorForwarder`
   watches that queue and ships each entry to the server via e/server — the same
   client→server bridge idiom as freememo.optimistic/CommandDispatcher.

   `session-id` is a stable per-page-load id (defonce = evaluated once per JS app
   load); it tags every shipped error so one browser session's trail is joinable."
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.logging :as log]
   #?(:clj [freememo.toasts :as toasts])))

(defonce ^{:doc "Per-page-load correlation id (client-only; nil on the server)."}
  session-id
  #?(:cljs (str (random-uuid)) :clj nil))

;; Defined on both peers so the ClientErrorForwarder e/defn resolves under the
;; JVM Electric compiler; only ever written/read on the client (e/client scope).
(defonce ^{:doc "Client-side queue of pending client-error entries:
                 [{:id uuid :op kw-or-str :message str :notify str-or-nil} …]."}
  !queue (atom []))

(defn report!
  "Enqueue a client-originating failure for server-side logging. Safe to call
   from any JS callback. `op` names the failed operation (keyword or string);
   `ex` is an error/string (str'd here). No-op on the server. Returns nil.

   The 3-arity adds `notify`: a message the USER must see, not just the log —
   for failures with no other surface, such as a pasted image the browser
   rejected before any Electric frame could observe it. It rides the same queue
   because the transition is identical (JS callback → server); the forwarder
   pushes an error toast alongside the log entry."
  ([op ex] (report! op ex nil))
  ([op ex notify]
   #?(:cljs (swap! !queue conj {:id (random-uuid) :op op :message (str ex)
                                :notify notify}))
   nil))

(defn log-client-error!
  "Log one queued client failure and, when it carries :notify, surface that text
   to the user as an error toast.
   Pre  : user-id may be nil (pre-auth surfaces); notify is a str or nil.
   Post : one :warn signal is recorded, plus one toast iff notify is non-nil and
          user-id is non-nil. Returns :logged (truthy — the forwarder spends its
          token on it).
   Inv  : never throws; a toast failure must not strand the queue entry."
  [user-id session-id op message notify]
  #?(:clj (do (log/client-error! user-id session-id op message)
              (when (and notify user-id)
                (try (toasts/push! user-id {:level :error :message notify})
                     (catch Exception _ nil)))
              :logged)
     :cljs nil))

(e/defn ClientErrorForwarder
  "Always-mounted, headless pump: ships each queued client error to the server
   (log/client-error!) exactly once, then drops it from the queue. Mount once in
   Main, sibling to CommandDispatcher. Pre: user-id the current user (may be nil
   for pre-auth surfaces)."
  [user-id]
  (e/client
    (e/for [err (e/diff-by :id (e/watch !queue))]
      (let [[t _] (e/Token (:id err))]
        (when t
          (when-some [_ (e/server (log-client-error! user-id session-id
                                    (:op err) (:message err) (:notify err)))]
            (t)
            (swap! !queue (fn [q] (filterv #(not= (:id err) (:id %)) q)))))))))
