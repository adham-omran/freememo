(ns freememo.app-update
  "The App Update of a Local Install (ticket 19, plans/local-install.md, \"New: updates\").

   The update feed (`freememo.update-feed`) names a newer Release with the same
   Runtime. This namespace downloads its jar into `{data dir}/app/`, checks
   it, and says when it is ready. The bootstrap (tools/installer/bootstrap/FreememoBoot.java,
   which an App Update never replaces) runs the newest verified jar at the next
   start, and falls back to the previous one when the new one never prints READY.

   Files in `{data dir}/app/`:

     freememo-<rv>.jar          a verified App Update
     freememo-<rv>.jar.sig      its signature (raw Ed25519, 64 bytes)
     freememo-<rv>.jar.failed   the bootstrap or a bad signature rejected this version
     .download-<rv>.*.tmp       a download in progress. The bootstrap never picks it.

   The signature covers the text `freememo-app-jar\\n<rv>\\n<sha256 hex>\\n`, not the
   jar bytes, so a check needs no memory and a signed jar cannot pass as another version.
   The public key (base64, X.509 DER) comes from the system property
   `freememo.update-public-key`, which the installer sets, or from the env var
   `FREEMEMO_UPDATE_PUBLIC_KEY` (tests). Without a key App Update is off."
  (:require [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [freememo.desktop-version :as dv]
            [freememo.update-feed :as feed]
            [taoensso.telemere :as tel])
  (:import [java.io File InputStream]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files StandardCopyOption]
           [java.security KeyFactory MessageDigest Signature]
           [java.security.spec X509EncodedKeySpec]
           [java.util Base64]))

(def restart-code
  "The exit code with which the app asks the bootstrap to start again."
  75)

(def keep-count "Jars kept in the data directory: the current and the previous." 2)

(defn- log [& parts] (tel/log! {:level :info :id ::app-update} (apply str parts)))

(defn updates-dir ^File [^File data-dir] (io/file data-dir "app"))
(defn jar-file ^File [^File dir rv] (io/file dir (str "freememo-" rv ".jar")))
(defn sig-file ^File [^File dir rv] (io/file dir (str "freememo-" rv ".jar.sig")))
(defn failed-file ^File [^File dir rv] (io/file dir (str "freememo-" rv ".jar.failed")))

;; ── Signature ──────────────────────────────────────────────────────

(defn parse-public-key
  "Post: a PublicKey for base64 X.509 DER text, or nil when `text` is blank or not a key."
  [text]
  (when-not (str/blank? text)
    (try (.generatePublic (KeyFactory/getInstance "Ed25519")
           (X509EncodedKeySpec. (.decode (Base64/getMimeDecoder) (str/trim text))))
         (catch Exception _ nil))))

(defn configured-public-key
  "Post: the trusted PublicKey of this install, or nil (App Update is off)."
  []
  (parse-public-key (or (System/getProperty "freememo.update-public-key")
                      (System/getenv "FREEMEMO_UPDATE_PUBLIC_KEY"))))

(defn sha256-hex [^File f]
  (let [md (MessageDigest/getInstance "SHA-256")
        buf (byte-array 65536)]
    (with-open [in (io/input-stream f)]
      (loop []
        (let [n (.read in buf)]
          (when (pos? n) (.update md buf 0 n) (recur)))))
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest md)))))

(defn statement
  "Post: the text that the release workflow signs."
  [rv sha-hex]
  (str "freememo-app-jar\n" rv "\n" sha-hex "\n"))

(defn verify
  "Post: true when `sig` (a File) is the signature of `jar` as release `version`
         under `public-key`. False for anything else, and never throws."
  [{:keys [public-key version jar sig]}]
  (try
    (and public-key (.isFile ^File jar) (.isFile ^File sig)
      (let [s (doto (Signature/getInstance "Ed25519") (.initVerify public-key))]
        (.update s (.getBytes ^String (statement version (sha256-hex jar)) StandardCharsets/UTF_8))
        (.verify s (Files/readAllBytes (.toPath ^File sig)))))
    (catch Exception _ false)))

;; ── The directory ──────────────────────────────────────────────────

(defn- versions-in
  "Post: the versions that have a jar in `dir`, newest first."
  [^File dir]
  (->> (.listFiles dir)
    (keep #(second (re-matches #"freememo-(\d+(?:\.\d+)*)\.jar" (.getName ^File %))))
    (sort (fn [a b] (cond (feed/newer? a b) -1 (feed/newer? b a) 1 :else 0)))))

(defn prune!
  "Keep the `keep-count` newest jars that have no .failed marker. Delete every
   other jar with its signature, and orphan signatures. A .failed marker stays,
   so the feed does not make the app download that version again.
   Post: the versions that stay."
  [^File dir]
  (when (.isDirectory dir)
    (let [versions (versions-in dir)
          usable (remove #(.exists (failed-file dir %)) versions)
          keep (set (take keep-count usable))]
      (doseq [v versions :when (not (keep v))]
        (io/delete-file (jar-file dir v) true)
        (io/delete-file (sig-file dir v) true)
        (log "deleted the old app jar " v))
      (doseq [^File f (.listFiles dir)
              :let [m (re-matches #"freememo-(\d+(?:\.\d+)*)\.jar\.sig" (.getName f))]
              :when (and m (not (.exists (jar-file dir (second m)))))]
        (.delete f))
      (vec (sort-by identity keep)))))

(defn staged
  "Post: `{:version :jar}` for the newest verified jar in `{data dir}/app/` that
         is newer than `installed` and not marked failed, else nil."
  [^File data-dir installed public-key]
  (let [dir (updates-dir data-dir)]
    (when (and public-key (.isDirectory dir))
      (some (fn [v]
              (when (and (feed/newer? v installed)
                      (not (.exists (failed-file dir v)))
                      (verify {:public-key public-key :version v
                               :jar (jar-file dir v) :sig (sig-file dir v)}))
                {:version v :jar (jar-file dir v)}))
        (versions-in dir)))))

;; ── Decide and download ────────────────────────────────────────────

(defn decision
  "Pre:  `feed` is the feed map (string keys). `installed` is this install's
         Release Version.
   Post: nil when the feed holds no newer version. Else `{:action :skip :reason …}`
         or `{:action :download :version :url :sha256 :signature-url}`.
         An App Update applies only when the feed's Runtime equals this install's.
         A higher Runtime needs an installer (ticket 20). The feed carries one
         Sync Protocol, the one that freememo.net runs, so the check is that it is a number."
  [feed-map installed]
  (let [version (get feed-map "current_version")
        {:strs [url sha256 signature_url]} (get feed-map "app_update")]
    (when (feed/newer? version installed)
      (cond
        (not= (get feed-map "runtime") dv/runtime)
        {:action :skip :version version
         :reason (str "release " version " needs Runtime " (get feed-map "runtime")
                   ", this install has " dv/runtime ", so it needs a new installer")}

        (not (number? (get feed-map "sync_protocol")))
        {:action :skip :version version :reason (str "release " version " names no Sync Protocol")}

        (or (str/blank? url) (str/blank? sha256) (str/blank? signature_url))
        {:action :skip :version version :reason (str "release " version " has no signed app jar")}

        :else {:action :download :version version :url url :sha256 sha256 :signature-url signature_url}))))

(defn- http-stream
  "Post: an InputStream of `url`. Throws on a status other than 200."
  ^InputStream [url]
  (let [resp (http/request {:method :get :url url :as :stream :throw-exceptions false
                            :connection-timeout 15000 :socket-timeout 30000})]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp)) {})))
    (:body resp)))

(defn- delete-quietly! [& files] (doseq [^File f files] (when f (.delete f))))

(defn download!
  "Download, check and keep one App Update.
   Pre:  `decision` is a `:download` map. `public-key` is a PublicKey.
         `opts` may hold `:open-stream` (a fn of a URL, for tests).
   Post: `{:status :ready :version :jar}`, `{:status :rejected :reason}` (the
         signature is bad: the version is marked failed) or `{:status :failed
         :reason}` (a network or hash error: a later check tries again).
         The jar goes to a temp file in `{data dir}/app/`, is checked there, and
         moves to its name in one atomic rename, after its signature. A partial
         or bad download is deleted and never has a pickable name. One log line on a failure."
  [^File data-dir {:keys [version url sha256 signature-url]} public-key & [{:keys [open-stream] :or {open-stream http-stream}}]]
  (let [dir (updates-dir data-dir)
        _ (.mkdirs dir)
        tmp-jar (io/file dir (str ".download-" version ".jar.tmp"))
        tmp-sig (io/file dir (str ".download-" version ".sig.tmp"))
        jar (jar-file dir version)
        sig (sig-file dir version)
        fail (fn [status reason]
               (delete-quietly! tmp-jar tmp-sig)
               (log "app update " version " not used: " reason)
               {:status status :reason reason})]
    (try
      (with-open [in ^InputStream (open-stream url)]
        (io/copy in tmp-jar))
      (let [actual (sha256-hex tmp-jar)]
        (if-not (= (str/lower-case sha256) actual)
          (fail :failed (str "the jar hash " actual " is not the feed hash"))
          (do
            (with-open [in ^InputStream (open-stream signature-url)]
              (io/copy in tmp-sig))
            (if-not (verify {:public-key public-key :version version :jar tmp-jar :sig tmp-sig})
              (do (spit (failed-file dir version) "bad signature\n")
                  (fail :rejected "the signature is not valid, so the jar is deleted"))
              (do (Files/move (.toPath tmp-sig) (.toPath sig) (into-array [StandardCopyOption/ATOMIC_MOVE StandardCopyOption/REPLACE_EXISTING]))
                  (Files/move (.toPath tmp-jar) (.toPath jar) (into-array [StandardCopyOption/ATOMIC_MOVE StandardCopyOption/REPLACE_EXISTING]))
                  {:status :ready :version version :jar jar})))))
      (catch Throwable e
        (fail :failed (or (ex-message e) (str e)))))))

;; ── State and the check ────────────────────────────────────────────

(defonce ^:private !ready (atom nil))

(defn ready-version
  "Post: the version of a verified App Update that waits for a restart, or nil."
  []
  @!ready)

(defonce ^:private !dismissed
  ;; The version whose banner the user dismissed. In memory only: the next start
  ;; runs that version, so the banner has nothing left to say.
  (atom nil))

(defn reset-state! [] (reset! !ready nil) (reset! !dismissed nil))

(defn restart-available?
  "True when this JVM runs under the bootstrap, which can start the next jar."
  []
  (= "true" (System/getProperty "freememo.bootstrap")))

;; ── What the web app shows ─────────────────────────────────────────

(defn banner-state
  "What the banner strip says about an App Update (`failure-banners/update-banners`).
   The tray item \"Restart to update\" alone was too easy to miss: a Mac that
   starts at login may not restart for weeks.
   Post: `{:version v}` while a verified update waits, this JVM can restart into
         it, and the user has not dismissed `v`. nil otherwise, always nil on the
         server."
  []
  (let [v (ready-version)]
    (when (and v (restart-available?) (not= v @!dismissed))
      {:version v})))

(defn dismiss-banner!
  "Hide the banner for `version`. A later version shows it again."
  [version]
  (reset! !dismissed version))

(defn restart!
  "Exit with `restart-code`, so the bootstrap starts the newest verified jar.
   The tray item does the same. The exit runs on another thread after a short
   delay, so the caller's response can leave first.
   Post: true when the restart is under way; false, and nothing happens, when no
         update waits or no bootstrap runs this JVM."
  []
  (if (and (ready-version) (restart-available?))
    (do (log "restarting into App Update " (ready-version))
        (future (Thread/sleep 300) (System/exit (int restart-code)))
        true)
    false))

(defn check!
  "Look at one feed answer, and download an App Update when it applies.
   Pre:  `opts` has `:data-dir`, `:installed` (Release Version), `:feed` (map or nil),
         and may have `:on-ready` (a fn of the version), `:public-key` and `:open-stream`.
   Post: `ready-version` is set when a verified jar newer than this one is
         in the data directory. A jar that already waits is not hashed again
         while the feed names nothing newer: the feed is read every few minutes
         and an app jar is about 160 MB. A feed newer than the waiting jar is
         downloaded. Never throws. Returns the status keyword."
  [{:keys [data-dir installed feed public-key on-ready open-stream]}]
  (try
    (let [key (or public-key (configured-public-key))
          ready! (fn [v] (when (not= v @!ready) (reset! !ready v) (when on-ready (on-ready v))) :ready)
          feed-version (get feed "current_version")
          waiting @!ready
          on-disk (when (and key (not (and waiting (not (feed/newer? feed-version waiting)))))
                    (staged data-dir installed key))]
      (cond
        (nil? key) (do (log "App Update is off: this install has no public key") :off)

        (and waiting (not (feed/newer? feed-version waiting))) :ready

        (and on-disk (not (feed/newer? feed-version (:version on-disk))))
        (do (prune! (updates-dir data-dir)) (ready! (:version on-disk)))

        (nil? feed) :no-feed

        :else
        (let [d (decision feed installed)]
          (case (:action d)
            nil :current
            :skip (do (log "app update skipped: " (:reason d)) :skipped)
            :download (if (.exists (failed-file (updates-dir data-dir) (:version d)))
                        :failed-before
                        (let [{:keys [status version]} (download! data-dir d key {:open-stream (or open-stream http-stream)})]
                          (if (= :ready status)
                            (do (prune! (updates-dir data-dir)) (ready! version))
                            status)))))))
    (catch Throwable e
      (log "app update check failed: " (or (ex-message e) (str e)))
      :error)))
