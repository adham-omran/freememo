(ns freememo.desktop-link
  "The desktop JVM's link to freememo.net (plans/electron-local-db-sync.md
   §3.4, 4.6, 4.10).

   Electron passes the server origin, the desktop token and the account on
   stdin at launch (3.4), so the token never sits in the process environment.
   `freememo.desktop` stores them here. Everything that talks to freememo.net
   reads them here: the sync client, the AI seam in `freememo.openrouter`, and
   the offline guard.

   On the server peer every value stays nil and `require-online!` is a no-op.")

(defonce ^:private !link (atom {:origin nil :token nil :user-id nil :online? true}))

(defn configure!
  "Store the launch configuration. Called once at desktop boot."
  [{:keys [origin token user-id]}]
  (swap! !link assoc :origin origin :token token :user-id user-id))

(defn origin [] (:origin @!link))
(defn token [] (:token @!link))
(defn user-id [] (:user-id @!link))

(defn online? [] (:online? @!link))

(def ^:private !online-listeners (atom []))

(defn on-online!
  "Call `f` each time the link goes from offline to online (2.11.1)."
  [f]
  (swap! !online-listeners conj f))

(defn set-online!
  "Record connectivity, from Electron's report or from a failed request.
   Post: the listeners run once per offline→online transition."
  [online?]
  (let [[before _] (swap-vals! !link assoc :online? (boolean online?))]
    (when (and online? (not (:online? before)))
      (doseq [f @!online-listeners] (f)))))

(defn offline-message
  "4.6 — the text a network-only feature shows instead of running, or nil
   when it may run. Each feature returns it in its own error shape, so its
   own error path shows it and writes no row."
  [feature]
  (when (and (:origin @!link) (not (:online? @!link)))
    (str feature " needs an internet connection. FreeMemo is offline.")))

(defn require-online!
  "4.6 — refuse a network-only feature while the desktop is offline.
   Post: returns nil when online or on the server peer. Otherwise throws an
         ex-info whose message is the text the user sees, so the feature's
         own error path shows it and writes no row."
  [feature]
  (when-let [message (offline-message feature)]
    (throw (ex-info message {:type ::offline :feature feature}))))
