(ns freememo.local-install
  "The Local Install main (docs/adr/0001-jvm-launcher-replaces-electron.md,
   plans/local-install.md). One JVM, no Electron:

     clojure.main -m freememo.local-install [--data-dir DIR] [--port N] [--origin URL]

   It takes an exclusive lock on the data directory, recovers a PostgreSQL
   that a killed JVM left behind, starts PostgreSQL through the zonky library,
   and serves the app on 127.0.0.1 behind `freememo.local-gate`. A tray icon
   offers \"Open FreeMemo\" and \"Quit\". Quit stops the web server, then
   PostgreSQL.

   Files in the data directory:

     account.json   the account: {origin, token, user_id, username, id_block,
                    first_download}. The sync token is a secret, so it is a
                    0600 file. The first-launch sign-in writes it
                    (`freememo.local-signin`). Without it, the launcher serves
                    only the first-launch page and starts no PostgreSQL.
     install.json   {port, cookie_secret, enc_key_secret, pg_password}, 0600, written at
                    the first launch. The port and both secrets stay across
                    restarts, so a bookmark and its cookie keep working.
     pgdata/        the PostgreSQL cluster
     postgres.log   its output
     launcher.lock  the single-instance lock

   Server settings (`DB_*`, `FREEMEMO_PEER`, `ENC_KEY_SECRET`) come from
   `freememo.launch-env` system properties. This namespace sets them before it
   loads any server namespace, so its own requires stay light.

   Headless mode (`FREEMEMO_HEADLESS=true`, or no system tray): no tray and no
   browser. The launcher prints `FREEMEMO_OPEN_URL=<url>` on stdout instead.
   The Seam 1 harness (`freememo.local-install-harness`) uses it. SIGTERM
   quits the same way as the tray item.

   `--no-tab` starts the same way but opens no browser tab. The login item
   (`freememo.local-login`) starts the installed app with it.

   `:gen-class` gives the jpackage launcher a main class (`freememo.local_install`).
   It cannot use `clojure.main -m`: the arguments of the launcher command line,
   such as `--no-tab`, replace every `--arguments` default of jpackage."
  (:gen-class)
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [freememo.launch-env :as launch-env]
            [freememo.local-files :as files]
            [freememo.local-gate :as gate]
            [freememo.local-license :as local-license]
            [freememo.local-log :refer [log]]
            [freememo.local-login :as login]
            [freememo.local-postgres :as postgres]
            [freememo.local-signin :as signin]
            [freememo.local-tray :as tray]
            [ring.adapter.jetty :as jetty])
  (:import [io.zonky.test.db.postgres.embedded EmbeddedPostgres]
           [java.io File]
           [java.net InetAddress ServerSocket]
           [java.nio.channels FileChannel]
           [java.nio.file OpenOption StandardOpenOption]
           [java.security SecureRandom]
           [java.util Base64]))

(def default-port
  "The port a fresh install tries first."
  47077)

(def ^:private port-tries 100)

(defn default-data-dir
  "Post: the per-user data directory of this OS. It is not Electron's
   directory, whose pgdata another PostgreSQL user and password own.
   Ticket 09 (macOS installer and move from Electron) moves the default to
   Electron's directory and resets the PostgreSQL auth there."
  ^File []
  (let [home (System/getProperty "user.home")
        os (str/lower-case (System/getProperty "os.name" ""))]
    (cond
      (str/includes? os "mac") (io/file home "Library" "Application Support" "FreeMemo Local")
      (str/includes? os "win") (io/file (or (System/getenv "APPDATA") home) "FreeMemo Local")
      :else (io/file (or (System/getenv "XDG_DATA_HOME") (str home "/.local/share")) "FreeMemo Local"))))

;; ── Files ──────────────────────────────────────────────────────────

(defn- random-secret []
  (let [bytes (byte-array 32)]
    (.nextBytes (SecureRandom.) bytes)
    (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bytes)))

(defn- read-json [^File f]
  (when (.exists f) (json/parse-string (slurp f))))

(defn find-account
  "Post: the account map with string keys, or nil when there is no account yet."
  [^File data-dir]
  (let [account (read-json (io/file data-dir "account.json"))]
    (when (get account "user_id") account)))

(defn read-account
  "Post: the account map with string keys. Throws when the file is missing,
         because there is nothing to serve without an account."
  [^File data-dir]
  (or (find-account data-dir)
    (throw (ex-info (str "This install has no account yet. Expected " (io/file data-dir "account.json"))
             {:type ::no-account}))))

;; ── Single instance ────────────────────────────────────────────────

(defn- lock-data-dir!
  "Post: the held file lock, or throws when another launcher holds it."
  [^File data-dir]
  (let [channel (FileChannel/open (.toPath (io/file data-dir "launcher.lock"))
                  (into-array OpenOption [StandardOpenOption/CREATE StandardOpenOption/WRITE]))
        lock (try (.tryLock channel) (catch java.nio.channels.OverlappingFileLockException _ nil))]
    (when-not lock
      (.close channel)
      (throw (ex-info "FreeMemo Local is already running for this data directory."
               {:type ::already-running})))
    lock))

;; ── Ports ──────────────────────────────────────────────────────────

(defn- port-free? [port]
  (try (with-open [_ (ServerSocket. port 1 (InetAddress/getByName "127.0.0.1"))] true)
       (catch java.io.IOException _ false)))

(defn choose-port
  "Post: the first free port from `preferred` up, within 100 tries."
  [preferred]
  (or (some #(when (port-free? %) %) (range preferred (+ preferred port-tries)))
      (throw (ex-info (str "No free port from " preferred) {:type ::no-port}))))

(defn- load-install!
  "Post: install.json as a map. It holds the kept port and both secrets. The
         port is the saved one, else `preferred`, moved up to a free port.
         The file is written when anything in it changed."
  [^File data-dir preferred]
  (let [f (io/file data-dir "install.json")
        old (or (read-json f) {})
        port (choose-port (or (get old "port") preferred))
        new (assoc old
              "port" port
              "cookie_secret" (or (get old "cookie_secret") (random-secret))
              "enc_key_secret" (or (get old "enc_key_secret") (random-secret))
              "pg_password" (or (get old "pg_password") (random-secret)))]
    (when (not= old new) (files/write-private! f (json/generate-string new)))
    new))

(defn- start-update-check!
  "Read the update feed of the account's server (tickets 08 and 19). Quiet when
   the feed cannot be read. On each answer:
   - a newer Release that needs a higher Runtime downloads no jar. The installer
     gate (`freememo.installer-gate`) syncs every pending edit; when nothing is
     pending, the tray gets \"Download the new installer\" (nil `menu`: headless);
   - a newer Release with the same Runtime downloads as an App Update
     (`freememo.app-update`). When its jar is verified, and this JVM runs under
     the bootstrap, the tray gets \"Restart to update\": it exits with code 75
     and the bootstrap starts the newest verified jar."
  [account menu data-dir]
  (let [ns-of (fn [sym] (require sym) (find-ns sym))
        uf (ns-of 'freememo.update-feed)
        au (ns-of 'freememo.app-update)
        gate (ns-of 'freememo.installer-gate)
        resolve-fn #(ns-resolve uf %)
        au-fn #(ns-resolve au %)
        user-id (get account "user_id")
        installed ((resolve-fn 'installed-version))]
    ((resolve-fn 'start!)
     {:origin (get account "origin")
      :version installed
      :on-feed (fn [feed]
                 ((ns-resolve gate 'on-feed!) feed
                  {:installed installed
                   :on-change (fn [st]
                                (when menu (tray/show-update! menu st))
                                ((resolve-fn 'bump-banners!) user-id))})
                 ((au-fn 'check!)
                  {:data-dir data-dir :installed installed :feed feed
                   :on-ready (fn [_version]
                               (when (and menu ((au-fn 'restart-available?)))
                                 (tray/show-restart! menu #(System/exit (int @(au-fn 'restart-code))))))}))
      :on-change (fn [_available] ((resolve-fn 'bump-banners!) user-id))})))

(defn- show-error! [message]
  (binding [*out* *err*] (println (str "FreeMemo Local could not start: " message)))
  (when-not (tray/headless?)
    (try (javax.swing.JOptionPane/showMessageDialog nil message "FreeMemo Local"
           javax.swing.JOptionPane/ERROR_MESSAGE)
         (catch Throwable _ nil))))

;; ── Main ───────────────────────────────────────────────────────────

(defn- parse-args [args]
  (loop [[a & more] args opts {}]
    (case a
      nil opts
      "--data-dir" (recur (next more) (assoc opts :data-dir (io/file (first more))))
      "--port" (recur (next more) (assoc opts :port (parse-long (first more))))
      "--origin" (recur (next more) (assoc opts :origin (first more)))
      "--no-tab" (recur more (assoc opts :no-tab true))
      (recur more opts))))

(defn- set-server-env! [pg-port install]
  (doseq [[k v] {"DB_HOST" "127.0.0.1" "DB_PORT" pg-port "DB_NAME" "postgres"
                 "DB_USER" "postgres" "DB_PASSWORD" (get install "pg_password")
                 "ENC_KEY_SECRET" (get install "enc_key_secret")
                 "FREEMEMO_PEER" "desktop" "FREEMEMO_LOCAL_INSTALL" "true"
                 "COOKIE_SECURE" "false" "AUTH_MODE" "password"}]
    (launch-env/set! k v)))

(defn- sign-in-phase!
  "No account yet: serve the first-launch page on the install's port until the
   student signs in (`freememo.local-signin`).
   Post: the account map. `account.json` is written, 0600, in one step. The
         sign-in server runs on, in `!jetty`, until the caller stops it."
  [{:keys [data-dir origin local-origin http-port install !jetty on-gate]}]
  (let [{:keys [handler account]} (signin/make-flow
                                    {:origin origin :local-origin local-origin
                                     :save-account! #(files/write-private! (io/file data-dir "account.json")
                                                       (json/generate-string %))})
        {:keys [wrap issue-token!]} (gate/make-gate {:origin local-origin
                                                     :secret (get install "cookie_secret")
                                                     :public? signin/public-request?})]
    (reset! !jetty (jetty/run-jetty (wrap handler) {:host "127.0.0.1" :port http-port :join? false}))
    (on-gate issue-token!)
    (log "waiting for the first sign-in at " local-origin)
    (println (str "FREEMEMO_SIGNIN_READY port=" http-port))
    (flush)
    @account))

(defn- first-download-pending?
  "A sign-in writes `first_download` into the account. Until a download has
   committed, GET / shows the progress page, not an empty library."
  [account]
  (let [!done (atom false)]
    (fn []
      (and (get account "first_download")
        (not @!done)
        (try (let [ready? (boolean (:library-ready ((ns-resolve 'freememo.sync-client 'status))))]
               (when ready? (reset! !done true))
               (not ready?))
             (catch Throwable _ false))))))

(defn- start-network-check!
  "Ticket 06: the JVM checks freememo.net itself, so no shell reports it."
  []
  ((ns-resolve 'freememo.connectivity 'start!)
   {:on-offline! @(ns-resolve 'freememo.sync-client 'note-offline!)}))

(defn- wrap-license-reminder
  "Ticket 07: the license reminder is the first page of a launch. It reads the
   license that the last sync cached."
  [handler origin]
  (local-license/wrap-reminder handler
    {:origin origin
     :license-fn #(:license ((ns-resolve 'freememo.sync-client 'status)))
     :buy? #(do (require 'freememo.desktop-license)
          (not @(ns-resolve 'freememo.desktop-license 'trial-open?)))}))

;; Ticket 05: one library, one account.

(defn- make-account-guard
  "Post: `{:wrap f :user-id (fn [])}` from `freememo.local-account`, over the
   database functions of `freememo.local-library`."
  [account data-dir origin local-origin]
  (require 'freememo.local-account 'freememo.local-library)
  (let [lib #(deref (ns-resolve 'freememo.local-library %))]
    ((ns-resolve 'freememo.local-account 'make-guard)
     {:origin origin :local-origin local-origin :account account
      :save-file! #(files/write-private! (io/file data-dir "account.json") (json/generate-string %))
      :delete-file! #(io/delete-file (io/file data-dir "account.json") true)
      :adopt! (lib 'adopt!) :renew! (lib 'renew!) :wipe! (lib 'wipe!)
      :unsynced-count (lib 'unsynced-count) :revoked? (lib 'revoked?)
      :library-ready? (lib 'library-ready?)})))

(defn start!
  "Start everything. With no account, the first-launch page comes first and
   PostgreSQL starts after the sign-in.
   Post: `{:port :pg-port :open! :quit!}`. `quit!` stops the web server, then
         PostgreSQL, once, and releases the lock."
  [{:keys [data-dir port origin no-tab]}]
  (let [data-dir (or data-dir (default-data-dir))
        _ (.mkdirs ^File data-dir)
        lock (lock-data-dir! data-dir)
        install (load-install! data-dir (or port default-port))
        http-port (get install "port")
        local-origin (str "http://127.0.0.1:" http-port)
        origin (str/replace (or origin (System/getenv "FREEMEMO_ORIGIN") "https://freememo.net") #"/+$" "")
        !pg (atom nil)
        !jetty (atom nil)
        !issue (atom nil)
        !shown (atom false)
        !menu (atom nil)
        quit-once (delay
                    (log "quitting")
                    (when-let [server @!jetty]
                      (try (.stop ^org.eclipse.jetty.server.Server server)
                           (catch Throwable e (log "web server: " (ex-message e)))))
                    (when-let [pg @!pg]
                      (try (.close ^EmbeddedPostgres pg) (catch Throwable e (log "postgres: " (ex-message e)))))
                    (try (.release ^java.nio.channels.FileLock lock) (catch Throwable _ nil))
                    (log "stopped"))
        quit! #(force quit-once)
        open-url! (fn [] (str local-origin "/?" gate/token-param "=" (@!issue)))
        open! #(tray/open-browser! (open-url!))
        ;; The first gate shows the browser: the tray, and either the page or
        ;; the URL on stdout. A second gate (after a sign-in) does not.
        on-gate (fn [issue-token!]
                  (reset! !issue issue-token!)
                  (when (compare-and-set! !shown false true)
                    (if (tray/headless?)
                      (println (str "FREEMEMO_OPEN_URL=" (open-url!)))
                      (do (reset! !menu (tray/install-tray! open! #(System/exit 0) (tray/server-label http-port nil)
                                                   ((requiring-resolve 'freememo.config/release-version))
                                                   (login/login-item)))
                          (when-not no-tab (open!))))))]
    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable quit! "local-install-quit"))
    (try
      (let [account (or (find-account data-dir)
                      (sign-in-phase! {:data-dir data-dir :origin origin :local-origin local-origin
                                       :http-port http-port :install install :!jetty !jetty :on-gate on-gate}))
            pg (reset! !pg (postgres/start-postgres! (io/file data-dir "pgdata") data-dir (get install "pg_password")))]
        (set-server-env! (.getPort pg) install)
        (require 'prod 'freememo.desktop 'freememo.logging 'freememo.connectivity 'freememo.sync-client)
        ((ns-resolve 'freememo.logging 'init!))
        ((ns-resolve 'freememo.desktop 'set-launch-config!) account)
        (let [guard (make-account-guard account data-dir origin local-origin)
              {:keys [wrap issue-token!]} (gate/make-gate
                                            {:origin local-origin
                                             :secret (get install "cookie_secret")
                                             :user-id (:user-id guard)
                                             :public? signin/public-request?
                                             :local-route @(ns-resolve 'freememo.desktop 'local-route)})
              pending? (first-download-pending? account)
              _ (when-let [first-server @!jetty]
                  ;; The sign-in server gives the port to the app.
                  (.stop ^org.eclipse.jetty.server.Server first-server)
                  (reset! !jetty nil))
              server ((ns-resolve 'prod 'start-server!)
                      {:port http-port
                       :wrap-gate (fn [handler] (wrap ((:wrap guard)
                                                       (-> handler
                                                         (signin/wrap-first-download pending?)
                                                         (wrap-license-reminder (get account "origin"))))))})]
          (reset! !jetty server)
          (start-network-check!)
          (log "serving " local-origin)
          (on-gate issue-token!)
          (start-update-check! account @!menu data-dir)
          (println (str "FREEMEMO_READY port=" http-port " pg_port=" (.getPort pg)))
          (flush)
          {:port http-port :pg-port (.getPort pg) :open! open! :quit! quit!}))
      (catch Throwable t
        (quit!)
        (throw t)))))

(defn -main [& args]
  (try
    ;; Jetty's threads are not daemons, so the JVM stays up until Quit.
    (start! (parse-args args))
    (catch Throwable t
      (show-error! (or (ex-message t) (str t)))
      (.printStackTrace t)
      (System/exit 1))))
