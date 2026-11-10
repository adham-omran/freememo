(ns freememo.local-install-harness
  "Seam 1 of plans/local-install.md: a test starts the Local Install main in its
   own JVM, with a temporary data directory, and talks to it only through HTTP.
   Tickets 04, 06 and 08 build on this namespace.

   Pieces:

   - `start-test-server!`: a server JVM that plays freememo.net. It runs the
     `prod` main on a throwaway database in the shared dev PostgreSQL.
   - `start-local-install!`: the Local Install JVM (`freememo.local-install`)
     in headless mode. It seeds `account.json` before it starts. It returns a
     handle that holds the port, the one-time `:open-url` and the process.
   - `http`: a request with no redirect following and no cookie jar, so a test
     sees every header. It may send any Host and Origin header.
   - `stop!` (SIGTERM, the same path as the tray's Quit), `kill!` (SIGKILL).

   A JVM loads the app from source, so each start takes about a minute. The
   classpath comes from `clojure -Spath` once per run. The client build must
   exist: `clj -X:build:prod build-client`."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [next.jdbc :as jdbc])
  (:import [java.io File]
           [java.lang ProcessBuilder$Redirect]
           [java.net URI]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.time Duration]))

;; The JDK client refuses to send a Host header unless this is set before its
;; first use. A gate test must send a wrong Host.
(System/setProperty "jdk.httpclient.allowRestrictedHeaders" "host")

(def ^:private start-timeout-ms 300000)

(defn temp-dir
  "Post: a new empty directory."
  ^File [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn delete-tree! [^File f]
  (when (.exists f)
    (doseq [c (reverse (file-seq f))] (.delete ^File c))))

(def ^:private classpath
  (delay (-> (ProcessBuilder. ^java.util.List ["clojure" "-Spath" "-A:prod:local-install"])
           (.redirectErrorStream false)
           (.start)
           (#(let [out (slurp (.getInputStream %))] (.waitFor %) (str/trim out))))))

(defn free-port []
  (with-open [s (java.net.ServerSocket. 0 1 (java.net.InetAddress/getByName "127.0.0.1"))]
    (.getLocalPort s)))

(defn- spawn!
  "Post: {:process :log-file} for `java -cp … clojure.main -m <main> <args>`."
  [main args env log-file]
  (let [pb (ProcessBuilder. ^java.util.List
             (into ["java" "-Xss4m" "-cp" @classpath "clojure.main" "-m" main] args))]
    (doseq [[k v] env] (.put (.environment pb) k (str v)))
    (doto pb
      (.directory (io/file (System/getProperty "user.dir")))
      (.redirectErrorStream true)
      (.redirectOutput (ProcessBuilder$Redirect/appendTo log-file)))
    {:process (.start pb) :log-file log-file}))

(defn- await-line
  "Post: the first line of the log that matches `re`. Throws when the process
         exits first or the time runs out. The message holds the log."
  [{:keys [^Process process ^File log-file]} re]
  (let [deadline (+ (System/currentTimeMillis) start-timeout-ms)]
    (loop []
      (let [text (if (.exists log-file) (slurp log-file) "")]
        (cond
          (re-find re text) (re-find re text)
          (not (.isAlive process)) (throw (ex-info (str "The JVM exited before " re "\n" text) {}))
          (> (System/currentTimeMillis) deadline) (throw (ex-info (str "Timed out waiting for " re "\n" text) {}))
          :else (do (Thread/sleep 250) (recur)))))))

(declare http)

(defn- await-http
  "Post: `target` answers any HTTP status. Throws as `await-line` does."
  [{:keys [^Process process ^File log-file]} target]
  (let [deadline (+ (System/currentTimeMillis) start-timeout-ms)]
    (loop []
      (cond
        (try (http target {}) true (catch Exception _ false)) true
        (not (.isAlive process)) (throw (ex-info (str "The server exited\n" (slurp log-file)) {}))
        (> (System/currentTimeMillis) deadline) (throw (ex-info (str "Timed out\n" (slurp log-file)) {}))
        :else (do (Thread/sleep 500) (recur))))))

(defn log-text [{:keys [^File log-file]}] (slurp log-file))

;; ── The server that plays freememo.net ─────────────────────────────

(def ^:private pg-env
  {"DB_HOST" "localhost" "DB_PORT" "5432" "DB_USER" "cardmaker" "DB_PASSWORD" "dev"})

(defn- admin-ds []
  (jdbc/get-datasource {:dbtype "postgresql" :host "localhost" :port 5432 :dbname "cardmaker"
                        :user "cardmaker" :password "dev"}))

(defn start-test-server!
  "Pre:  the dev PostgreSQL answers on localhost:5432 and the role may create a
         database.
   Post: `{:origin \"http://127.0.0.1:<port>\" :db-name … :process …}`. The
         server is the real `prod` main in the server role, on its own
         database."
  []
  (let [db-name (str "fm_li_" (System/nanoTime))
        port (free-port)
        log-file (File/createTempFile "li-test-server" ".log")]
    (jdbc/execute! (admin-ds) [(str "CREATE DATABASE " db-name)])
    (let [handle (spawn! "prod" []
                   (merge pg-env {"DB_NAME" db-name "PORT" port "COOKIE_SECURE" "false"
                                  "AUTH_MODE" "password"
                                  "ENC_KEY_SECRET" "li-test-server-secret"})
                   log-file)]
      (try
        (await-http handle (str "http://127.0.0.1:" port "/"))
        (catch Throwable t (.destroyForcibly ^Process (:process handle)) (throw t)))
      (assoc handle :origin (str "http://127.0.0.1:" port) :db-name db-name))))

(defn stop-test-server! [{:keys [^Process process db-name]}]
  (.destroy process)
  (.waitFor process)
  (jdbc/execute! (admin-ds) [(str "DROP DATABASE IF EXISTS " db-name " WITH (FORCE)")]))

;; ── The Local Install ──────────────────────────────────────────────

(defn default-account
  "An account file for a server at `origin`. The token is not one the server
   issued, so sync answers 401 and the tests do not depend on it."
  [origin]
  {"origin" origin "token" "test-token" "user_id" 7 "username" "local-test"})

(defn seed-account!
  "Post: `account.json` in `data-dir`."
  [^File data-dir account]
  (.mkdirs data-dir)
  (spit (io/file data-dir "account.json") (json/generate-string account)))

(defn start-local-install!
  "Start the Local Install JVM in headless mode and wait until it serves.
   `opts`: `:data-dir` (a File; an empty temp dir by default), `:account` (the
   account.json map; `default-account` by default; nil leaves an existing
   file), `:origin` (the server's origin for the default account),
   `:port` (the preferred port of a fresh install; a free one by default),
   `:env` (more environment variables for the JVM, such as FREEMEMO_VERSION).
   Post: `{:data-dir :port :pg-port :open-url :process :log-file}`. `:open-url`
         is the one-time token URL of this start."
  [{:keys [data-dir account origin port env] :or {origin "http://127.0.0.1:1"}}]
  (let [data-dir (or data-dir (temp-dir "li-data"))
        _ (when (or account (not (.exists (io/file data-dir "account.json"))))
            (seed-account! data-dir (or account (default-account origin))))
        log-file (File/createTempFile "li-install" ".log")
        handle (spawn! "freememo.local-install"
                 ["--data-dir" (.getPath data-dir) "--port" (str (or port (free-port)))]
                 (merge {"FREEMEMO_HEADLESS" "true"} env) log-file)
        [_ ready-port pg-port] (await-line handle #"FREEMEMO_READY port=(\d+) pg_port=(\d+)")
        open-url (second (re-find #"FREEMEMO_OPEN_URL=(\S+)" (log-text handle)))]
    (assoc handle :data-dir data-dir :port (parse-long ready-port) :pg-port (parse-long pg-port)
      :open-url open-url)))

(defn alive? [{:keys [^Process process]}] (.isAlive process))

(defn stop!
  "Quit the way the tray does: SIGTERM runs the Quit hook. Waits for exit.
   Post: the JVM has exited. Returns the exit code."
  [{:keys [^Process process]}]
  (.destroy process)
  (if (.waitFor process 60 java.util.concurrent.TimeUnit/SECONDS)
    (.exitValue process)
    (do (.destroyForcibly process) (.waitFor process) :forced)))

(defn kill!
  "SIGKILL the JVM. PostgreSQL and the lock are left as a crash leaves them."
  [{:keys [^Process process]}]
  (.destroyForcibly process)
  (.waitFor process))

(defn url [li path] (str "http://127.0.0.1:" (:port li) path))

;; ── HTTP ───────────────────────────────────────────────────────────

(def ^:private client
  (delay (-> (HttpClient/newBuilder)
           (.followRedirects HttpClient$Redirect/NEVER)
           (.connectTimeout (Duration/ofSeconds 10))
           (.build))))

(defn http
  "One request, no redirects, no cookie jar.
   `target` is a full URL. `opts`: `:method` (:get), `:headers` (a map; set
   \"Host\" or \"Origin\" to test the gate), `:cookie` (a string \"name=value\"),
   `:body` (a string).
   Post: `{:status :headers :body}`. Header names are lower case, and each
         value is its first one, except \"set-cookie\" which is a vector."
  [target {:keys [method headers cookie body] :or {method :get}}]
  (let [builder (HttpRequest/newBuilder (URI. target))
        publisher (if body (HttpRequest$BodyPublishers/ofString body) (HttpRequest$BodyPublishers/noBody))]
    (doseq [[k v] (cond-> headers cookie (assoc "Cookie" cookie))]
      (.header builder k v))
    (.method builder (str/upper-case (name method)) publisher)
    (.timeout builder (Duration/ofSeconds 60))
    (let [response (.send @client (.build builder) (HttpResponse$BodyHandlers/ofString))
          raw (into {} (.map (.headers response)))]
      {:status (.statusCode response)
       :headers (into {} (for [[k vs] raw]
                           [(str/lower-case k) (if (= "set-cookie" (str/lower-case k)) (vec vs) (first vs))]))
       :body (.body response)})))

(defn cookie-pair
  "Post: \"name=value\" from a response's first Set-Cookie header, or nil."
  [response]
  (some-> (get-in response [:headers "set-cookie"]) first (str/split #";") first))

(defn sign-in!
  "Open the token URL and keep the cookie.
   Post: `{:cookie \"fm_local=…\" :location …}` from the token exchange."
  [li]
  (let [response (http (:open-url li) {})]
    {:cookie (cookie-pair response)
     :location (get-in response [:headers "location"])
     :response response}))
