(ns freememo.local-postgres
  "The embedded PostgreSQL of a Local Install (zonky): start, recover from a
   killed JVM, and harden (password and scram-only `pg_hba.conf`)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [freememo.local-files :as files]
            [freememo.local-log :refer [log]])
  (:import [io.zonky.test.db.postgres.embedded EmbeddedPostgres]
           [java.io File]
           [java.lang ProcessBuilder$Redirect ProcessHandle]))

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

(def pg-major
  "The PostgreSQL major version that this Runtime embeds (ADR 0003)."
  18)

(defn retire-old-major!
  "A data directory made by another major is never opened (ADR 0003): the
   Local Install has no library, and the next sync makes a fresh one by a full
   download. It moves the directory aside and never deletes it.
   Post: `pgdata` is absent, or its `PG_VERSION` is `pg-major`. Returns the
         moved directory, or nil when nothing moved."
  [^File pgdata]
  (let [version-file (io/file pgdata "PG_VERSION")]
    (when (.exists version-file)
      (let [found (str/trim (slurp version-file))]
        (when (not= found (str pg-major))
          (let [stamp (.format (java.time.LocalDate/now) java.time.format.DateTimeFormatter/BASIC_ISO_DATE)
                aside (loop [n 0]
                        (let [f (io/file (.getParentFile pgdata)
                                         (str (.getName pgdata) ".pg" found "-" stamp (when (pos? n) (str "-" n))))]
                          (if (.exists f) (recur (inc n)) f)))]
            (when-not (.renameTo pgdata aside)
              (throw (ex-info (str "Cannot move the PostgreSQL " found " data directory aside") {:pgdata (str pgdata)})))
            (log "found a PostgreSQL " found " data directory, which PostgreSQL " pg-major
                 " cannot open; moved it to " (.getName aside) " and starting with no library")
            aside))))))

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
    (files/write-private! (io/file pgdata "pg_hba.conf") "host all all 127.0.0.1/32 scram-sha-256\n")
    (.execute stmt "SELECT pg_reload_conf()")))

(defn start-postgres!
  "Post: a running zonky `EmbeddedPostgres` on a random loopback port, with
         password auth (`harden-postgres!`). Zonky runs initdb when `pgdata`
         is empty."
  ^EmbeddedPostgres [^File pgdata ^File data-dir password]
  (retire-old-major! pgdata)
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
