(ns freememo.update-feed
  "The update feed of plans/local-install.md (\"New: updates\").

   Server side: freememo.net publishes `GET /api/desktop/feed`, a public route.
   The body is `{current_version, installers {macos windows linux},
   app_update {url sha256 signature_url}, runtime, sync_protocol}`.
   It comes from the latest release of `PUBLIC_REPO` on GitHub (default
   `adham-omran/freememo`): the asset URLs and `freememo-<rv>-release.json`.
   A draft or a release that lacks an asset is not used. The last good answer
   stays in memory, so a GitHub outage does not empty the feed. With no good
   answer yet, the route answers 404.

   Local Install side: the launcher calls `start!`. It reads the feed from the
   account's origin at start and every few hours. When the feed holds a newer
   version, `banner-state` says so and the web app and the tray show \"Update
   available\". Since ticket 19 only the tray shows it, and only when the new Release needs
   an installer; the web app shows no such banner, and an App Update downloads by itself
   (`freememo.app-update`). A feed that cannot be read shows nothing and logs one line.

   `banner-state` also says when the server refused sync for an old Sync Protocol
   (the sync state `:update-required`). The web app then shows \"Update to keep
   syncing\"."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
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

(def asset-manifest
  "The one list of release assets: `resources/freememo/release-assets.json`.
   `tools/installer/asset-names.mjs` and `tools/release/asset-names.sh` read it
   too, and `release-assets-test` checks that the three agree."
  (delay (json/parse-string (slurp (io/resource "freememo/release-assets.json")))))

(defn asset-name
  "Post: the name of the asset of `kind` (`:macos`, `:windows`, `:linux`, `:app`,
   `:sig` or `:release`; `:release-json` is the old spelling of `:release`) on
   the release `rv`."
  [rv kind]
  (let [kind (if (= kind :release-json) "release" (name kind))
        entry (first (filter #(= kind (get % "kind")) (get @asset-manifest "assets")))]
    (str/replace (get entry "name") "{version}" rv)))

(defn required-asset-names
  "Post: the names that a release must carry before the feed uses it."
  [rv]
  (->> (get @asset-manifest "assets")
    (remove #(get % "optional"))
    (mapv #(str/replace (get % "name") "{version}" rv))))

(defn feed-from-release
  "Pre:  `release` is the parsed GitHub release (string keys); `meta` is the parsed
         `freememo-<rv>-release.json`.
   Post: the feed map, or nil when the release is a draft, its tag is not a
         Release Version, an asset is missing, or `meta` does not match it."
  [release meta]
  (let [tag (get release "tag_name")
        rv (when (string? tag) (str/replace tag #"^v" ""))
        urls (into {} (for [a (get release "assets")] [(get a "name") (get a "browser_download_url")]))]
    (when (and rv (not (get release "draft")) (parse-version rv) (map? meta)
            (= rv (get meta "release_version"))
            (number? (get meta "runtime")) (number? (get meta "sync_protocol"))
            (string? (get meta "app_jar_sha256"))
            (every? #(not (str/blank? (get urls %))) (required-asset-names rv)))
      {"current_version" rv
       "installers" (into {} (for [os [:macos :windows :linux]] [(name os) (get urls (asset-name rv os))]))
       "app_update" (cond-> {"url" (get urls (asset-name rv :app))
                             "sha256" (get meta "app_jar_sha256")}
                      (get urls (asset-name rv :sig)) (assoc "signature_url" (get urls (asset-name rv :sig))))
       "runtime" (get meta "runtime")
       "sync_protocol" (get meta "sync_protocol")})))

(def cache-ttl-ms "How long a good answer is used before GitHub is asked again (60 requests an hour, unauthenticated)." (* 10 60 1000))
(def retry-ms "How long to wait after a failed read before GitHub is asked again." (* 2 60 1000))

(defonce ^:private !release (atom {:good nil :checked-at 0 :failed-at 0}))
(defonce ^:private release-lock (Object.))

(defn reset-release-cache! [] (reset! !release {:good nil :checked-at 0 :failed-at 0}))

(defn log-github-failure [msg]
  (tel/log! {:level :warn :id ::github-feed} msg))

(defn- public-repo [] (or (config/env-or-config "PUBLIC_REPO" [:deployment :public-repo]) "adham-omran/freememo"))

(defn- get-json [url]
  (let [resp (http/request {:method :get :url url :throw-exceptions false :as :text
                            :headers {"Accept" "application/vnd.github+json" "User-Agent" "freememo-update-feed"}
                            :connection-timeout 4000 :socket-timeout 4000})]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str url " answered HTTP " (:status resp)) {})))
    (json/parse-string (:body resp))))

(defn fetch-feed-from-github
  "Post: the feed map for the latest release of `repo` at `api-base`. Throws when
   GitHub fails or the release is not usable."
  [api-base repo]
  (let [release (get-json (str api-base "/repos/" repo "/releases/latest"))
        tag (str (get release "tag_name"))
        rv (str/replace tag #"^v" "")
        url (some #(when (= (get % "name") (asset-name rv :release-json)) (get % "browser_download_url"))
              (get release "assets"))
        meta (when url (get-json url))]
    (or (feed-from-release release meta)
        (throw (ex-info (str "release " tag " is a draft or lacks an asset") {})))))

(defn feed-body
  "Post: the feed map, or nil when no good answer exists yet. Asks GitHub at most
   once per `cache-ttl-ms`, and again `retry-ms` after a failure. On a failure it
   logs one line and gives the last good answer. The GitHub calls have a 4 s timeout.
   `opts` may hold `:api-base` and `:repo`, for tests."
  ([] (feed-body {}))
  ([{:keys [api-base repo]}]
   (let [fresh? (fn [{:keys [good checked-at failed-at]} now]
                  (or (and good (< (- now checked-at) cache-ttl-ms))
                    (< (- now failed-at) retry-ms)))]
     (if (fresh? @!release (System/currentTimeMillis))
       (:good @!release)
       (locking release-lock
         (let [now (System/currentTimeMillis)]
           (if (fresh? @!release now)
             (:good @!release)
             (try
               (let [fresh (fetch-feed-from-github (or api-base "https://api.github.com") (or repo (public-repo)))]
                 (swap! !release assoc :good fresh :checked-at now)
                 fresh)
               (catch Throwable e
                 (log-github-failure
                   (str "cannot read the latest GitHub release: " (or (ex-message e) (str e))))
                 (swap! !release assoc :failed-at now)
                 (:good @!release))))))))))

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
         `:on-change` (a fn of the new `:available`) and `:on-feed` (a fn of the
         feed map, or nil when it cannot be read; the App Update check, ticket 19).
   Post: `banner-state` is current. A feed that cannot be read shows nothing: it
         clears an earlier `:available` and logs one line."
  [{:keys [origin version on-change on-feed]}]
  (let [feed-map (try (fetch-feed origin)
                      (catch Throwable e
                        (log "cannot read the update feed: " (or (ex-message e) (str e)))
                        nil))
        now (when feed-map (available feed-map version (os-key)))
        _ (when on-feed
            (try (on-feed feed-map)
                 (catch Throwable e (log "app update callback failed: " (or (ex-message e) (str e))))))
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
  "Post: `{:update-available {:version :url} or nil, :update-required? bool,
           :installer nil or {:status :unsynced} or {:status :ready :version :url}}`.
   `:installer` is the Runtime gate (`freememo.installer-gate`, ticket 20).
   All are nil/false anywhere but a Local Install."
  []
  (if-not (peer/local-install?)
    {:update-available nil :update-required? false :installer nil}
    {:update-available (:available @!state)
     :installer ((requiring-resolve 'freememo.installer-gate/state))
     :update-required? (= :update-required
                         (:state ((requiring-resolve 'freememo.sync-client/status))))}))
