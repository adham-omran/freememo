(ns freememo.update-feed
  "The update feed of plans/local-install.md (\"New: updates\").

   Server side: freememo.net publishes `GET /api/desktop/feed`, a public route.
   The body is `{current_version, installers {macos windows linux}}`.
   The version and the installer URLs come from `DESKTOP_CURRENT_VERSION`, `DESKTOP_INSTALLER_MACOS`,
   `DESKTOP_INSTALLER_WINDOWS` and `DESKTOP_INSTALLER_LINUX` (env or config.edn
   `:desktop`). With no version set, the route answers 404.

   Local Install side: the launcher calls `start!`. It reads the feed from the
   account's origin at start and every few hours. When the feed holds a newer
   version, `banner-state` says so and the web app and the tray show \"Update
   available\". A feed that cannot be read shows nothing and logs one line.

   `banner-state` also says when the server refused sync for an old Sync Protocol
   (the sync state `:update-required`). The web app then shows \"Update to keep
   syncing\"."
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.string :as str]
            [freememo.commands :as commands]
            [freememo.config :as config]
            [freememo.desktop-link :as link]
            [freememo.launch-env :as launch-env]
            [freememo.release-version :as release-version]
            [freememo.local-http :as lhttp]
            [freememo.peer :as peer]
            [taoensso.telemere :as tel])
  (:import [java.util.concurrent ScheduledExecutorService TimeUnit]))

(def interval-hours "How often a running Local Install reads the feed." 4)

(def feed-path "/api/desktop/feed")

;; ── Pure parts ─────────────────────────────────────────────────────

(defn parse-version
  "Post: a vector of numbers for \"2026.11.512\", or nil when `v` is not dotted numbers."
  [v]
  (when (and (string? v) (re-matches #"\d+(\.\d+)*" (str/trim v)))
    (mapv parse-long (str/split (str/trim v) #"\."))))

(defn newer?
  "True when `candidate` is a later version than `current`. A version that does
   not parse is never newer, so a bad feed shows nothing."
  [candidate current]
  (let [a (parse-version candidate) b (parse-version current)]
    (boolean
      (and a b
        (let [n (max (count a) (count b))
              pad #(into % (repeat (- n (count %)) 0))]
          (pos? (compare (pad a) (pad b))))))))

(defn os-key
  "Post: \"macos\", \"windows\" or \"linux\" for the OS of this JVM."
  []
  (let [os (str/lower-case (System/getProperty "os.name" ""))]
    (cond (str/includes? os "mac") "macos"
          (str/includes? os "win") "windows"
          :else "linux")))

(defn available
  "Post: `{:version :url}` when `feed` (a map with string keys) holds a version
   newer than `current` and an installer URL for `os`, else nil."
  [feed current os]
  (let [version (get feed "current_version")
        url (get-in feed ["installers" os])]
    (when (and (newer? version current) (string? url) (not (str/blank? url)))
      {:version version :url url})))

;; ── Server: the public route ───────────────────────────────────────

(defn feed-body
  "Post: the feed map, or nil when no version is configured."
  []
  (when-let [version (config/env-or-config "DESKTOP_CURRENT_VERSION" [:desktop :version])]
    {"current_version" version
     "installers" (into {}
                    (for [[k env] {"macos" "DESKTOP_INSTALLER_MACOS"
                                   "windows" "DESKTOP_INSTALLER_WINDOWS"
                                   "linux" "DESKTOP_INSTALLER_LINUX"}
                          :let [url (config/env-or-config env [:desktop :installers (keyword k)])]
                          :when url]
                      [k url]))}))

(defn feed-handler
  "GET /api/desktop/feed. Public: it carries no account data."
  [_request]
  (if-let [body (feed-body)]
    {:status 200 :headers {"Content-Type" "application/json" "Cache-Control" "public, max-age=300"}
     :body (json/generate-string body)}
    {:status 404 :headers {"Content-Type" "application/json"} :body "{\"error\":\"no-feed\"}"}))

;; ── Local Install: read the feed ───────────────────────────────────

(defonce ^:private !state (atom {:available nil}))

(defn- log [& parts] (tel/log! {:level :info :id ::update-feed} (apply str parts)))

(defn- fetch-feed
  "Post: the feed map. Throws on a network error, a status other than 200, or a body that is not a map."
  [origin]
  (let [resp (http/request {:method :get :url (str origin feed-path)
                            :throw-exceptions false
                            :connection-timeout 10000 :socket-timeout 10000})
        body (when (= 200 (:status resp)) (json/parse-string (:body resp)))]
    (when-not (map? body)
      (throw (ex-info (str "feed answered HTTP " (:status resp)) {})))
    body))

(defn refresh!
  "Read the feed once.
   Pre:  `opts` has `:origin`, `:version` (this install's version), and may have
         `:on-change` (a fn of the new `:available`).
   Post: `banner-state` is current. A feed that cannot be read shows nothing: it
         clears an earlier `:available` and logs one line."
  [{:keys [origin version on-change]}]
  (let [now (try (available (fetch-feed origin) version (os-key))
                 (catch Throwable e
                   (log "cannot read the update feed: " (or (ex-message e) (str e)))
                   nil))
        [before _] (swap-vals! !state assoc :available now)]
    (when (not= (:available before) now)
      (when on-change
        (try (on-change now)
             (catch Throwable e (log "update callback failed: " (or (ex-message e) (str e)))))))))

(defonce ^:private !timer (atom nil))

(defn start!
  "Read the feed now, every `interval-hours`, and when the network comes back,
   off the caller's thread.
   Post: no effect when this install has no Release Version (a dev build): it logs one line."
  [{:keys [version] :as opts}]
  (if (str/blank? version)
    (log "dev build has no Release Version, so no update check")
    (when-not @!timer
      (let [^ScheduledExecutorService ex (lhttp/daemon-scheduler "freememo-update-feed")]
        (reset! !timer ex)
        (link/on-online! #(.execute ex ^Runnable (fn [] (refresh! opts))))
        (.scheduleWithFixedDelay ex ^Runnable #(refresh! opts) 0 interval-hours TimeUnit/HOURS)))))

(defn installed-version
  "Post: the Release Version of this install, or nil for a dev build."
  []
  (let [v (config/release-version)]
    (when-not (= v release-version/dev) v)))

(defn bump-banners!
  "Make every open tab of `user-id` re-read the banner strip."
  [user-id]
  (commands/bump-channels! user-id #{:desktop-mutations}))

;; ── What the web app shows ─────────────────────────────────────────

(defn banner-state
  "Post: `{:update-available {:version :url} or nil, :update-required? bool}`.
   Both are nil/false anywhere but a Local Install."
  []
  (if-not (peer/local-install?)
    {:update-available nil :update-required? false}
    {:update-available (:available @!state)
     :update-required? (= :update-required
                         (:state ((requiring-resolve 'freememo.sync-client/status))))}))
