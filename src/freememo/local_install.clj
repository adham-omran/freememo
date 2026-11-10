(ns freememo.local-install
  "The Local Install main (docs/adr/0001-jvm-launcher-replaces-electron.md,
   plans/local-install.md). One JVM, no Electron:

     clojure.main -m freememo.local-install [--data-dir DIR] [--port N]

   It takes an exclusive lock on the data directory, recovers a PostgreSQL
   that a killed JVM left behind, starts PostgreSQL through the zonky library,
   and serves the app on 127.0.0.1 behind `freememo.local-gate`. A tray icon
   offers \"Open FreeMemo\" and \"Quit\". Quit stops the web server, then
   PostgreSQL.

   Files in the data directory:

     account.json   the account: {origin, token, user_id, username, id_block}.
                    The sync token is a secret, so it is a 0600 file. Ticket
                    04 writes it at sign-in. Today a test or a person seeds it.
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
   quits the same way as the tray item."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [freememo.launch-env :as launch-env]
            [freememo.local-gate :as gate])
  (:import [io.zonky.test.db.postgres.embedded EmbeddedPostgres]
           [java.lang ProcessBuilder$Redirect ProcessHandle]
           [java.io File]
           [java.net InetAddress ServerSocket]
           [java.nio.channels FileChannel]
           [java.nio.file Files OpenOption StandardOpenOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]
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

(defn- log [& parts]
  (println (apply str "[local-install] " parts))
  (flush))

;; ── Files ──────────────────────────────────────────────────────────

(defn- write-private!
  "Post: `f` holds `text` and only its owner can read it (0600 where the OS has it)."
  [^File f text]
  (let [path (.toPath f)]
    (Files/deleteIfExists path)
    (try
      (Files/createFile path (into-array FileAttribute
                               [(PosixFilePermissions/asFileAttribute (PosixFilePermissions/fromString "rw-------"))]))
      (catch UnsupportedOperationException _
        (Files/createFile path (make-array FileAttribute 0))))
    (spit f text)))

(defn- random-secret []
  (let [bytes (byte-array 32)]
    (.nextBytes (SecureRandom.) bytes)
    (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bytes)))

(defn- read-json [^File f]
  (when (.exists f) (json/parse-string (slurp f))))

(defn read-account
  "Post: the account map with string keys. Throws when the file is missing,
         because there is nothing to serve without an account."
  [^File data-dir]
  (let [f (io/file data-dir "account.json")
        account (read-json f)]
    (when-not (get account "user_id")
      (throw (ex-info (str "This install has no account yet. Expected " f)
               {:type ::no-account})))
    account))

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
    (when (not= old new) (write-private! f (json/generate-string new)))
    new))

;; ── PostgreSQL ─────────────────────────────────────────────────────

(defn- pid-alive? [pid] (.isPresent (ProcessHandle/of pid)))

(defn- postgres-process? [pid]
  (let [handle (.orElse (ProcessHandle/of pid) nil)]
    (boolean (some-> handle .info .command (.orElse "") (str/includes? "postgres")))))

(defn- signal! [pid sig]
  (.waitFor (.start (ProcessBuilder. ^java.util.List ["kill" (str "-" sig) (str pid)]))))

(defn- wait-dead [pid ms]
  (loop [n (quot ms 100)]
    (cond (not (pid-alive? pid)) true
          (zero? n) false
          :else (do (Thread/sleep 100) (recur (dec n))))))

(defn recover-postgres!
  "A killed JVM leaves `postmaster.pid`, and often a live postmaster.
   Pre:  this process holds the data directory lock, so no other launcher owns
         that postmaster.
   Post: no postmaster runs on `pgdata`, and `postmaster.pid` is gone."
  [^File pgdata]
  (let [pid-file (io/file pgdata "postmaster.pid")]
    (when (.exists pid-file)
      (let [pid (some-> (slurp pid-file) str/split-lines first str/trim parse-long)]
        (when (and pid (pid-alive? pid) (postgres-process? pid))
          (log "stopping a PostgreSQL that an earlier launch left (pid " pid ")")
          (when-not (and (zero? (signal! pid "INT")) (wait-dead pid 20000))
            (.destroyForcibly (.get (ProcessHandle/of pid)))
            (wait-dead pid 10000)))
        (.delete pid-file)
        (log "removed the stale postmaster.pid")))))

(defn- harden-postgres!
  "Zonky's initdb uses trust auth, so any local user could read the library.
   Post: the `postgres` role has `password`, and `pg_hba.conf` accepts only
         scram-sha-256 on 127.0.0.1. Idempotent: it runs at every start. The
         connection works under trust (first start, or a crash in this
         window) and under scram (every later start), because it sends the
         password. Only the first start has a few seconds of trust."
  [^EmbeddedPostgres pg ^File pgdata password]
  (with-open [conn (java.sql.DriverManager/getConnection
                     (str "jdbc:postgresql://127.0.0.1:" (.getPort pg) "/postgres")
                     "postgres" password)
              stmt (.createStatement conn)]
    ;; The password is base64url text, so it is safe inside a SQL literal.
    (.execute stmt (str "ALTER ROLE postgres PASSWORD '" password "'"))
    (write-private! (io/file pgdata "pg_hba.conf") "host all all 127.0.0.1/32 scram-sha-256\n")
    (.execute stmt "SELECT pg_reload_conf()")))

(defn- start-postgres!
  "Post: a running zonky `EmbeddedPostgres` on a random loopback port, with
         password auth (`harden-postgres!`). Zonky runs initdb when `pgdata`
         is empty."
  ^EmbeddedPostgres [^File pgdata ^File data-dir password]
  (recover-postgres! pgdata)
  (let [pg-log (ProcessBuilder$Redirect/appendTo (io/file data-dir "postgres.log"))
        pg (-> (EmbeddedPostgres/builder)
      (.setDataDirectory pgdata)
      (.setCleanDataDirectory false)
      (.setServerConfig "listen_addresses" "127.0.0.1")
      (.setServerConfig "unix_socket_directories" "")
      (.setOutputRedirector pg-log)
      (.setErrorRedirector pg-log)
      (.setPGStartupWait (java.time.Duration/ofSeconds 60))
      ;; Zonky's own connections use this once scram is on.
      (.setConnectConfig "password" password)
      (.start))]
    (harden-postgres! pg pgdata password)
    pg))

;; ── Tray and browser ───────────────────────────────────────────────

(defn open-browser! [url]
  (try
    (if (and (java.awt.Desktop/isDesktopSupported)
          (.isSupported (java.awt.Desktop/getDesktop) java.awt.Desktop$Action/BROWSE))
      (.browse (java.awt.Desktop/getDesktop) (java.net.URI. url))
      (let [os (str/lower-case (System/getProperty "os.name" ""))
            cmd (cond (str/includes? os "mac") "open" (str/includes? os "win") "explorer" :else "xdg-open")]
        (.start (ProcessBuilder. ^java.util.List [cmd url]))))
    (catch Exception e (log "cannot open the browser: " (ex-message e)))))

(defn- headless? []
  (or (= "true" (System/getenv "FREEMEMO_HEADLESS"))
      (java.awt.GraphicsEnvironment/isHeadless)
      (not (java.awt.SystemTray/isSupported))))

(defn- install-tray! [open! quit!]
  (let [image (javax.imageio.ImageIO/read (io/resource "public/freememo/icon-192.png"))
        menu (java.awt.PopupMenu.)
        listener (fn [f] (reify java.awt.event.ActionListener
                           (actionPerformed [_ _] (f))))
        item (fn [label f] (doto (java.awt.MenuItem. label) (.addActionListener (listener f))))]
    (.add menu (item "Open FreeMemo" open!))
    (.add menu (item "Quit" quit!))
    (.add (java.awt.SystemTray/getSystemTray)
      (doto (java.awt.TrayIcon. image "FreeMemo Local" menu)
        (.setImageAutoSize true)
        (.addActionListener (listener open!))))))

(defn- show-error! [message]
  (binding [*out* *err*] (println (str "FreeMemo Local could not start: " message)))
  (when-not (headless?)
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
      (recur more opts))))

(defn- set-server-env! [pg-port install]
  (doseq [[k v] {"DB_HOST" "127.0.0.1" "DB_PORT" pg-port "DB_NAME" "postgres"
                 "DB_USER" "postgres" "DB_PASSWORD" (get install "pg_password")
                 "ENC_KEY_SECRET" (get install "enc_key_secret")
                 "FREEMEMO_PEER" "desktop" "FREEMEMO_LOCAL_INSTALL" "true"
                 "COOKIE_SECURE" "false" "AUTH_MODE" "password"}]
    (launch-env/set! k v)))

(defn start!
  "Start everything.
   Post: `{:port :pg-port :open! :quit!}`. `quit!` stops the web server, then
         PostgreSQL, once, and releases the lock."
  [{:keys [data-dir port]}]
  (let [data-dir (or data-dir (default-data-dir))
        _ (.mkdirs ^File data-dir)
        lock (lock-data-dir! data-dir)
        account (read-account data-dir)
        install (load-install! data-dir (or port default-port))
        http-port (get install "port")
        origin (str "http://127.0.0.1:" http-port)
        pg (start-postgres! (io/file data-dir "pgdata") data-dir (get install "pg_password"))
        !jetty (atom nil)
        quit-once (delay
                    (log "quitting")
                    (when-let [server @!jetty]
                      (try (.stop ^org.eclipse.jetty.server.Server server)
                           (catch Throwable e (log "web server: " (ex-message e)))))
                    (try (.close pg) (catch Throwable e (log "postgres: " (ex-message e))))
                    (try (.release ^java.nio.channels.FileLock lock) (catch Throwable _ nil))
                    (log "stopped"))
        quit! #(force quit-once)]
    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable quit! "local-install-quit"))
    (try
      (set-server-env! (.getPort pg) install)
      (require 'prod 'freememo.desktop 'freememo.logging)
      ((ns-resolve 'freememo.logging 'init!))
      ((ns-resolve 'freememo.desktop 'set-launch-config!) account)
      (let [{:keys [wrap issue-token!]} (gate/make-gate
                                          {:origin origin
                                           :secret (get install "cookie_secret")
                                           :user-id (get account "user_id")
                                           :local-route @(ns-resolve 'freememo.desktop 'local-route)})
            server ((ns-resolve 'prod 'start-server!) {:port http-port :wrap-gate wrap})
            open-url! (fn [] (str origin "/?" gate/token-param "=" (issue-token!)))
            open! #(open-browser! (open-url!))]
        (reset! !jetty server)
        (log "serving " origin)
        (if (headless?)
          (println (str "FREEMEMO_OPEN_URL=" (open-url!)))
          (do (install-tray! open! #(System/exit 0))
              (open!)))
        (println (str "FREEMEMO_READY port=" http-port " pg_port=" (.getPort pg)))
        (flush)
        {:port http-port :pg-port (.getPort pg) :open! open! :quit! quit!})
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
