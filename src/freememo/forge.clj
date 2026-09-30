(ns freememo.forge
  "Read access to repositories on GitHub, GitLab and Forgejo
   (plans/git-forge-integration.md §4.2, D1).

   Three operations per forge and nothing more: list the account's
   repositories, resolve a ref to a commit sha, and download the zip archive at
   that sha. The archive is the only byte transport, so pagination, tree
   truncation and per-file limits — the parts the three APIs disagree on — never
   reach this namespace.

   Every request passes the same three gates:
     1. The address gate. `url-validate/safe-url?` runs on the first URL AND on
        every redirect hop, unless the operator set
        `config/forge-private-hosts-allowed?`. A forge host is user input, and a
        per-user switch would let any user of a hosted instance probe its
        internal network (D8).
     2. Explicit connection and socket timeouts on every call (§4.2 2.5), and a
        wall-clock limit on an archive download.
     3. A status check that turns a refusal into a message the user can act on.

   A token never follows a redirect to another host. GitHub sends a private
   archive to codeload with a signed URL, so the header is not needed there,
   and forwarding it would hand the credential to whatever host a forge names.

   Failures are `ex-info` with `:type ::forge-failure` and a message written for
   the user. The import services catch that type and show the message as is."
  (:require [clj-http.client :as http]
            [clojure.string :as str]
            [freememo.config :as config]
            [freememo.forge-kinds :as kinds]
            [freememo.url-validate :as url-validate])
  (:import [java.io File FileOutputStream IOException InputStream]
           [java.net URI URLEncoder]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;; ── Limits ─────────────────────────────────────────────────────────

(def ^:private connect-timeout-ms 10000)

(def ^:private socket-timeout-ms
  "Longest silence between two reads. A hung host fails here; a slow one that
   keeps sending is bounded by `max-download-ms` instead."
  30000)

(def ^:private max-download-ms (* 15 60 1000))

(def ^:private max-redirects 5)

(def max-archive-bytes
  "Largest archive one download may write to disk. FORGE_MAX_ARCHIVE_BYTES
   overrides the 1 GiB default."
  (or (some-> (System/getenv "FORGE_MAX_ARCHIVE_BYTES") parse-long)
    (* 1024 1024 1024)))

(defn- failure [msg data]
  (ex-info msg (assoc data :type ::forge-failure)))

(defn failure?
  "Post: true when `e` is a failure this namespace raised for the user to read."
  [e]
  (= ::forge-failure (:type (ex-data e))))

;; ── Origins and repository paths ───────────────────────────────────

(defn parse-origin
  "Read a forge host the way a user types it.

   Pre : `s` is `gitlab.example.com`, `https://gitlab.example.com/`,
         `http://10.0.0.5:3000`, or a URL inside the forge.
   Post: {:scheme \"https\"|\"http\" :host \"name[:port]\"} with a lower-case
         host, or nil when `s` names no usable host. The scheme defaults to
         https. A URL carrying credentials is refused, because they would reach
         a log line."
  [s]
  (when-let [t (some-> s str/trim not-empty)]
    (try
      (let [uri (URI. (if (str/includes? t "://") t (str "https://" t)))
            scheme (some-> (.getScheme uri) str/lower-case)
            host (some-> (.getHost uri) str/lower-case)
            port (.getPort uri)]
        (when (and host (#{"http" "https"} scheme) (nil? (.getRawUserInfo uri)))
          {:scheme scheme
           :host (if (pos? port) (str host ":" port) host)}))
      (catch Exception _ nil))))

(defn origin-url
  "Post: `scheme://host` for an origin from `parse-origin`."
  [{:keys [scheme host]}]
  (str scheme "://" host))

(def ^:private path-segment-re #"[A-Za-z0-9_.][A-Za-z0-9_.-]*")

(defn valid-repository-path?
  "Post: true when `path` can name a repository on `forge`. GitHub and Forgejo
   take exactly `owner/name`; GitLab also takes nested groups."
  [forge path]
  (let [segs (str/split (str path) #"/" -1)]
    (and (every? #(and (re-matches path-segment-re %) (not= "." %) (not= ".." %)) segs)
      (if (= "gitlab" forge)
        (<= 2 (count segs) 20)
        (= 2 (count segs))))))

(def ^:private route-segments
  "Words GitHub and Forgejo put after `owner/name` in a browser URL."
  #{"tree" "blob" "src" "commit" "commits" "releases" "tags" "issues" "pulls"
    "pull" "wiki" "actions" "archive" "raw"})

(defn- clean-path [p]
  (let [p (-> (str p) (str/replace #"^/+" "") (str/replace #"/+$" ""))
        ;; GitLab separates the project path from its pages with `/-/`.
        p (if-let [i (str/index-of p "/-/")] (subs p 0 i) p)
        segs (str/split p #"/")
        segs (if (and (> (count segs) 2) (route-segments (nth segs 2)))
               (take 2 segs)
               segs)]
    (str/replace (str/join "/" segs) #"(?i)\.git$" "")))

(defn parse-repository-input
  "Read a repository the way a user gives it.

   Pre : `s` is `owner/name`, a browser URL of the repository or of a page
         inside it, or an SSH clone address `git@host:owner/name.git`.
   Post: {:path \"owner/name\" :origin origin-or-nil}, or nil when `s` holds no
         path. `:origin` is present only when `s` named a host. The path is NOT
         validated here; `valid-repository-path?` owns that, per forge."
  [s]
  (when-let [t (some-> s str/trim not-empty)]
    (let [[origin raw]
          (cond
            (re-matches #"(?i)[a-z][a-z0-9+.-]*://.*" t)
            [(parse-origin t) (try (.getPath (URI. t)) (catch Exception _ nil))]

            (re-matches #"[^@\s]+@[^:\s]+:.+" t)
            (let [[_ host path] (re-matches #"[^@\s]+@([^:\s]+):(.+)" t)]
              [(parse-origin host) path])

            :else [nil t])
          path (some-> raw clean-path not-empty)]
      (when path
        {:path path :origin origin}))))

;; ── HTTP ───────────────────────────────────────────────────────────

(defn address-permitted?
  "Post: true when this server may send a request to `url`."
  [url]
  (if (config/forge-private-hosts-allowed?)
    (boolean
      (try
        (let [u (URI. (str url))]
          (and (.getHost u) (#{"http" "https"} (some-> (.getScheme u) str/lower-case))))
        (catch Exception _ false)))
    (url-validate/safe-url? url)))

(defn- authority-of [url]
  (some-> (try (URI. (str url)) (catch Exception _ nil)) .getAuthority str/lower-case))

(defn- close-body! [resp]
  (when (instance? InputStream (:body resp))
    (try (.close ^InputStream (:body resp)) (catch Exception _ nil))))

(def ^:private base-headers {"Accept" "application/json" "User-Agent" "FreeMemo"})

(defn- auth-headers [forge token]
  (when-not (str/blank? token)
    (case forge
      "github" {"Authorization" (str "Bearer " token)}
      "gitlab" {"PRIVATE-TOKEN" token}
      "forgejo" {"Authorization" (str "token " token)})))

(defn- request!
  "GET `url`, following redirects by hand.

   Pre : `headers` carries no credential; `auth` carries only the credential.
   Post: the final response, whatever its status. Redirects are followed up to
         `max-redirects`, each hop re-checked by `address-permitted?`, and
         `auth` is sent only to the authority the request started at.
   Throws ::forge-failure for a refused address, a network failure or too many
         redirects."
  [url {:keys [headers auth as]}]
  (let [start (authority-of url)]
    (loop [url url, hops 0]
      (when-not (address-permitted? url)
        (throw (failure (str "FreeMemo does not connect to " (or (authority-of url) url)
                          ". The address is private or not valid.")
                 {:reason :address})))
      (let [resp (try
                   (http/get url {:headers (merge headers (when (= start (authority-of url)) auth))
                                  :as as
                                  :connection-timeout connect-timeout-ms
                                  :socket-timeout socket-timeout-ms
                                  :redirect-strategy :none
                                  :throw-exceptions false})
                   (catch IOException e
                     (throw (failure (str "Could not reach " (authority-of url) ": " (.getMessage e))
                              {:reason :unreachable}))))]
        (if (#{301 302 303 307 308} (:status resp))
          (let [location (get-in resp [:headers "location"])]
            (close-body! resp)
            (cond
              (str/blank? location) resp
              (>= hops max-redirects)
              (throw (failure (str (authority-of url) " redirected too many times.") {:reason :redirects}))
              :else
              (recur (str (.resolve (URI. url) (URI. ^String location))) (inc hops))))
          resp)))))

(defn- check-status!
  "Post: `resp` when its status is 2xx.
   Throws ::forge-failure with a message naming the cause and the next step."
  [resp forge {:keys [origin what token?]}]
  (let [status (:status resp)]
    (if (<= 200 status 299)
      resp
      (let [label (kinds/forge-label forge)
            host (:host origin)
            settings-hint (str " in Settings → Git Forges")]
        (close-body! resp)
        (throw
          (failure
            (case (int status)
              401 (if token?
                    (str label " refused the token saved for " host ". Replace it" settings-hint ".")
                    (str label " asks for a token. Add one for " host settings-hint "."))
              403 (if (= "0" (str (get-in resp [:headers "x-ratelimit-remaining"])))
                    (str label " has no requests left for this hour."
                      (if token? " Try again later." (str " Add a token for " host settings-hint ".")))
                    (str label " refused access to " what "."))
              404 (str (str/capitalize what) " was not found on " host "."
                    (when-not token? (str " A private repository needs a token" settings-hint ".")))
              (str label " answered HTTP " status " for " what "."))
            {:reason :status :status status}))))))

(defn- enc [s]
  (-> (URLEncoder/encode (str s) "UTF-8") (str/replace "+" "%20")))

(defn- api-base [forge {:keys [host] :as origin}]
  (case forge
    "github" (if (= "github.com" host) "https://api.github.com" (str (origin-url origin) "/api/v3"))
    "gitlab" (str (origin-url origin) "/api/v4")
    "forgejo" (str (origin-url origin) "/api/v1")))

(defn- repo-api
  "Pre: `path` passed `valid-repository-path?`, so a GitHub or Forgejo path is
   two URL-safe segments and needs no encoding."
  [forge origin path]
  (if (= "gitlab" forge)
    (str (api-base forge origin) "/projects/" (enc path))
    (str (api-base forge origin) "/repos/" path)))

(defn- get-json! [forge origin token url what]
  (-> (request! url {:headers base-headers :auth (auth-headers forge token) :as :json})
    (check-status! forge {:origin origin :what what :token? (not (str/blank? token))})
    :body))

;; ── The three operations ───────────────────────────────────────────

(defn list-repositories
  "The repositories the token's account can read.

   Pre : `forge` is in `forge-kinds/forge-ids`; `origin` came from `parse-origin`.
   Post: [{:path :default-branch :private?} …], most recently active first, one
         page at most: 100 on GitHub and GitLab, 50 on Forgejo. Empty with no
         request when `token` is blank, because a forge lists an account's
         repositories only to that account.
   Throws ::forge-failure."
  [forge origin token]
  (if (str/blank? token)
    []
    (let [base (api-base forge origin)
          what "the repository list"]
      (case forge
        "github"
        (mapv (fn [r] {:path (:full_name r) :default-branch (:default_branch r)
                       :private? (boolean (:private r))})
          (get-json! forge origin token (str base "/user/repos?per_page=100&sort=updated") what))

        "gitlab"
        (mapv (fn [r] {:path (:path_with_namespace r) :default-branch (:default_branch r)
                       :private? (not= "public" (:visibility r))})
          (get-json! forge origin token
            (str base "/projects?membership=true&per_page=100&order_by=last_activity_at") what))

        "forgejo"
        (mapv (fn [r] {:path (:full_name r) :default-branch (:default_branch r)
                       :private? (boolean (:private r))})
          (get-json! forge origin token (str base "/user/repos?limit=50") what))))))

(def ^:private sha-re #"[0-9a-f]{40}|[0-9a-f]{64}")

(defn resolve-commit
  "The commit a ref names now.

   Pre : `path` passed `valid-repository-path?` for `forge`.
   Post: {:sha S :ref R}. `R` is `ref` trimmed, or the repository's default
         branch when `ref` is blank. `S` is the full sha `R` names at the moment
         of the call — a branch, a tag and a sha all resolve the same way.
   Throws ::forge-failure."
  [forge origin token path ref]
  (let [repo (repo-api forge origin path)
        token? (not (str/blank? token))
        ref (or (some-> ref str/trim not-empty)
              (not-empty (str (:default_branch (get-json! forge origin token repo
                                                 (str "repository " path)))))
              (throw (failure (str path " has no default branch. It may be empty.")
                       {:reason :empty})))
        what (str "ref " ref " in " path)
        sha (case forge
              ;; The sha media type answers with the bare sha, not a commit
              ;; object that carries up to 300 file diffs.
              "github"
              (-> (request! (str repo "/commits/" (enc ref))
                    {:headers (assoc base-headers "Accept" "application/vnd.github.sha")
                     :auth (auth-headers forge token) :as :string})
                (check-status! forge {:origin origin :what what :token? token?})
                :body str str/trim)

              "gitlab"
              (:id (get-json! forge origin token (str repo "/repository/commits/" (enc ref)) what))

              "forgejo"
              (:sha (first (get-json! forge origin token
                             (str repo "/commits?sha=" (enc ref)
                               "&limit=1&stat=false&verification=false&files=false")
                             what))))]
    (if (and (string? sha) (re-matches sha-re sha))
      {:sha sha :ref ref}
      (throw (failure (str (str/capitalize what) " does not name a commit.") {:reason :no-commit})))))

(defn archive-url
  "Post: the URL of the zip archive of `sha`."
  [forge origin path sha]
  (let [repo (repo-api forge origin path)]
    (case forge
      "github" (str repo "/zipball/" sha)
      "gitlab" (str repo "/repository/archive.zip?sha=" sha)
      "forgejo" (str repo "/archive/" sha ".zip"))))

(defn blob-url
  "Post: the forge's web page for file `path` at commit `sha` in the repository
   at `repo-url` (`origin-url` + \"/\" + owner/name, as the import stores it).
   Each path segment is percent-encoded, so a space or `#` in a file name
   cannot end the path."
  [forge repo-url sha path]
  (let [segments (str/join "/" (map enc (str/split path #"/")))]
    (case forge
      "github" (str repo-url "/blob/" sha "/" segments)
      "gitlab" (str repo-url "/-/blob/" sha "/" segments)
      "forgejo" (str repo-url "/src/commit/" sha "/" segments))))

(defn download-archive!
  "Stream the zip archive of `sha` to a temp file.

   Pre : `sha` came from `resolve-commit` for the same repository.
   Post: a temp File holding the archive. The caller owns it and MUST delete it.
   Invariant: heap holds one copy buffer; the body streams to disk.
   Invariant: past `max-archive-bytes` or `max-download-ms` the transfer stops,
         the partial file is deleted, and ::forge-failure is thrown.
   Invariant: `cancelled?` is read after every buffer; once it answers true the
         transfer stops the same way, with `:reason :cancelled`. An interrupt
         cannot do this: the socket read ignores it."
  ^File [forge origin token path sha cancelled?]
  (let [what (str "the archive of " path)
        resp (-> (request! (archive-url forge origin path sha)
                   {:headers (assoc base-headers "Accept" "*/*")
                    :auth (auth-headers forge token) :as :stream})
               (check-status! forge {:origin origin :what what
                                     :token? (not (str/blank? token))}))
        f (.toFile (Files/createTempFile "forge-archive" ".zip" (make-array FileAttribute 0)))
        deadline (+ (System/currentTimeMillis) max-download-ms)]
    (try
      (with-open [^InputStream in (:body resp)
                  out (FileOutputStream. f)]
        (let [buf (byte-array 262144)]
          (loop [total 0]
            (let [n (try (.read in buf)
                      (catch IOException e
                        (throw (failure (str "The download of " what " stopped: " (.getMessage e))
                                 {:reason :unreachable}))))]
              (when (pos? n)
                (when (cancelled?)
                  (throw (failure (str "The download of " what " was cancelled.")
                           {:reason :cancelled})))
                (let [total (+ total n)]
                  (when (> total max-archive-bytes)
                    (throw (failure (str "The archive of " path " is larger than "
                                      (quot max-archive-bytes (* 1024 1024)) " MB.")
                             {:reason :too-large})))
                  (when (> (System/currentTimeMillis) deadline)
                    (throw (failure (str "The download of " what " took longer than "
                                      (quot max-download-ms 60000) " minutes.")
                             {:reason :too-slow})))
                  (.write out buf 0 n)
                  (recur total)))))))
      f
      (catch Throwable t
        (.delete f)
        (throw t)))))
