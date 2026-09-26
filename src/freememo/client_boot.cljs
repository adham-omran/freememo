(ns freememo.client-boot
  "Boot the Electric client process, and report every way it can end.

   Two things live here because all four entrypoints need them and none of them
   owns them: the pair of continuations the reactor terminates into, and the
   banner a termination raises.

   **The continuations are not optional.** `e/boot-client` wraps the reactor in
   `with-default-shutdown-prints`, whose ZERO-argument arity is the only one that
   installs the default callbacks. `reload-when-stale` is strictly 2-arity, so
   calling the wrapped task with no arguments reaches the `([s f])` arity with
   `undefined` for both. Every client failure then surfaces as `TypeError:
   Cannot read properties of undefined (reading 'j')` raised from inside
   `reload-when-stale`, and the real message is discarded. That shipped, and it
   hid an `[unserializable]` crash that stopped every Anki sync from the day the
   Activity monitor landed. See `plans/anki-sync-client-peer-crash.md`.

   **A reactor that terminates has stopped serving the page, whatever value it
   ends with.** Close codes 1011 (server crash) and 1012 (program mismatch)
   return `electric-client3/connector` NORMALLY, so a banner on the failure path
   alone would leave that freeze silent — which is the symptom the user reported.
   Both continuations raise it. Exactly two terminations do not: a deliberate
   stop through `boot!`'s own thunk, and the stale-client reload, which owns its
   exit.

   **A server crash can also leave the reactor running, so the socket's close
   event reports too.** After a server-side failure, the client's close handler
   runs and cancels the process, but the root frame can fail to finish its
   teardown. `connector` then never reaches its close-code branch, and neither
   continuation runs. The close event itself always arrives, so `boot!` also
   listens for 1011 and 1012 on Electric's socket, and sends the report and
   raises the banner from there. The measurements are in
   `plans/card-refresh-server-local.md` §4.

   **The banner is plain DOM on purpose.** Electric is dead by the time it
   renders, so it cannot be an `e/defn`. It is appended to `document.body`, the
   one parent Electric adds to and removes from by identity rather than by child
   index — a foreign node in an Electric-created parent strands that subtree.

   `.cljs`, not `.cljc`: the CLJ peer has no use for any of this, and `dev.cljc`
   keeps Electric-heavy CLJ requires off its main thread."
  ;; Required for `::electric-client/stale-client` and `*ws-server-url*`.
  ;; The alias is compile-checked where the literal
  ;; `:hyperfiddle.electric-client3/stale-client` would not be, and the namespace
  ;; is already loaded by every caller.
  (:require [hyperfiddle.electric-client3 :as electric-client]
            [clojure.string :as str]
            [freememo.assistant-diagnostics :as assistant-diagnostics]
            [freememo.interaction-trail :as interaction-trail]))

(def ^:private banner-class "connection-lost-banner")

(defn- bounded-string [value max-length]
  (let [text (str (or value ""))]
    (subs text 0 (min max-length (count text)))))

(defn- error-message [error]
  (let [message (str (or (some-> error .-message) error ""))]
    (bounded-string
      (if (str/starts-with? message "[unserializable]")
        (str/replace message #"(?s)Value:.*$" "Value: [redacted]")
        message)
      2000)))

(defn- error-stack [error]
  (bounded-string (some-> error .-stack) 8000))

(defn- error-data [error]
  (try
    (bounded-string (pr-str (keys (ex-data error))) 2000)
    (catch :default _ "")))

(defn- interaction-json []
  (try
    (bounded-string
      (.stringify js/JSON (clj->js (interaction-trail/recent))) 6000)
    (catch :default _ "[]")))

(defn- failure-form [termination error]
  (let [{:keys [session-id topic-id page-topic-id chat-id run-uuid run-kind
                run-status render-phase]}
        (assistant-diagnostics/snapshot)
        form (js/URLSearchParams.)]
    (doseq [[key value]
            {:termination termination
             :message (error-message error)
             :stack (error-stack error)
             :error-data (error-data error)
             :route (bounded-string (str (.-pathname js/location)
                                      (.-search js/location)) 1000)
             :client-version electric-client/ELECTRIC_USER_VERSION
             :session-id session-id
             :topic-id topic-id
             :page-topic-id page-topic-id
             :chat-id chat-id
             :run-uuid run-uuid
             :run-kind run-kind
             :run-status run-status
             :render-phase render-phase
             :interactions (interaction-json)}]
      (when (some? value)
        (.append form (name key) (str value))))
    form))

(defn- send-failure! [termination error]
  (try
    (let [form (failure-form termination error)
          sent? (and (.-sendBeacon js/navigator)
                  (.sendBeacon js/navigator "/api/client-reactor-failure" form))]
      (when-not sent?
        (-> (js/fetch "/api/client-reactor-failure"
              #js {:method "POST" :body form :credentials "same-origin"
                   :keepalive true})
          (.catch (fn [_] nil)))))
    (catch :default report-error
      (js/console.error "Client failure report failed" report-error)))
  nil)

(defn- raise-banner!
  "Tell the user the app stopped, and offer the one action left.

   Pre : none. Safe with the reactor dead, and safe before first paint.
   Post: exactly one banner node under `document.body`.
   Invariant: plain DOM only, and `document.body` only."
  []
  (when-not (.querySelector js/document (str "." banner-class))
    (let [banner (.createElement js/document "div")
          message (.createElement js/document "span")
          reload (.createElement js/document "button")]
      (set! (.-className banner) banner-class)
      (.setAttribute banner "role" "alert")
      (set! (.-textContent message)
        "FreeMemo lost its connection to the server. Reload the page.")
      (set! (.-className reload) "connection-lost-reload")
      (set! (.-textContent reload) "Reload")
      (.addEventListener reload "click" (fn [_] (.reload (.-location js/window))))
      (.appendChild banner message)
      (.appendChild banner reload)
      (.appendChild (.-body js/document) banner)))
  nil)

(defn- stale-client?
  "Whether this failure is the deploy-triggered reload, which owns its own exit.

   `reload-when-stale` logs its own line and reloads the page, so neither a
   banner nor a second console error belongs to this case. Returns false for a
   plain JS error, because `ex-data` is nil there."
  [error]
  (= ::electric-client/stale-client (:hyperfiddle.electric/type (ex-data error))))

(def ^:private server-terminated-codes
  "Close codes after which Electric never reconnects: 1011 is a server process
   crash and 1012 a program mismatch."
  #{1011 1012})

(defonce ^:private !electric-close-handler (atom nil))

(defn- observe-electric-closes!
  "Give every socket opened to Electric's URL a `close` listener that calls the
   handler current at the time the socket opens.

   Pre : called before the process opens its socket.
   Post: `js/WebSocket` still constructs native sockets, with the native
         prototype and constants. It is replaced at most once per page load.
   Invariant: a socket reports to the process that opened it, so the close of a
         socket from an earlier dev reload never reaches a later process."
  [handler]
  (reset! !electric-close-handler handler)
  ;; String keys for the marker, read and write alike: Closure renames a dotted
  ;; property that no extern names.
  (when-not (aget js/WebSocket "freememoObservesCloses")
    (let [native js/WebSocket
          observed (fn [url protocols]
                     (let [socket (if (undefined? protocols)
                                    (new native url)
                                    (new native url protocols))]
                       (when (= url electric-client/*ws-server-url*)
                         (.addEventListener socket "close" @!electric-close-handler))
                       socket))]
      (set! (.-prototype observed) (.-prototype native))
      (doseq [constant ["CONNECTING" "OPEN" "CLOSING" "CLOSED"]]
        (aset observed constant (aget native constant)))
      (aset observed "freememoObservesCloses" true)
      (set! (.-WebSocket js/window) observed))))

(defn boot!
  "Start the Electric client process with continuations that report themselves.

   Pre : `task` is a Missionary task — a fn of `[success failure]`. Pass the
         wrapped boot thunk itself, never the result of calling it: a
         zero-argument call through `reload-when-stale` is the defect this fn
         exists to prevent.
   Post: returns a 0-arity thunk that stops the process AND records the stop as
         deliberate, so that teardown raises no banner. It is the only handle a
         caller gets, so the recording cannot be forgotten — stopping the process
         any other way is a caller bug, and it banners.
   Invariant: the banner appears only for a termination nobody asked for, and at
         most once per page load. At most one failure report goes out per
         process, from whichever of the close event and the two continuations
         comes first.

   **The deliberate-stop flag is per process, not per module, and that is the
   whole reason this shape exists.** A dev hot reload cancels the process, and
   the connector's `finally` then closes the socket and awaits that close under
   `m/compel` (`electric_client3.cljs:135-138`) — so the success continuation
   fires a tick or more LATER, after `^:dev/after-load` has already remounted. A
   module-level flag that the next mount cleared would therefore be clear again
   by the time the old process reported, and every save would raise the banner."
  [task]
  (let [!deliberate-stop? (atom false)
        !reported? (atom false)
        report-once! (fn [termination error]
                       (when (compare-and-set! !reported? false true)
                         (send-failure! termination error)))]
    (observe-electric-closes!
      (fn [event]
        (when (and (server-terminated-codes (.-code event))
                (not @!deliberate-stop?))
          (js/console.error "Electric socket closed by the server"
            (.-code event) (.-reason event))
          (report-once! "server-close"
            (str "close " (.-code event) ": " (.-reason event)))
          (raise-banner!))))
    (let [cancel (task
                   (fn [_]
                     ;; Success is not a healthy state here — only a deliberate
                     ;; stop is.
                     (when-not @!deliberate-stop?
                       (report-once! "unexpected-success" nil)
                       (raise-banner!)))
                   (fn [error]
                     (when-not (stale-client? error)
                       ;; The error OBJECT, not its message: `socket-emit` enriches
                       ;; an unserializable transfer with the let-bound values it
                       ;; could be, and stringifying it loses them.
                       (js/console.error "Electric client process failed" error)
                       (when-not @!deliberate-stop?
                         (report-once! "failure" error)
                         (raise-banner!)))))]
      (fn stop-electric-process! []
        (reset! !deliberate-stop? true)
        (cancel)))))
