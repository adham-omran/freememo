(ns freememo.local-install-app-update-test
  "Seam 1 for ticket 19 of plans/local-install.md: a Local Install JVM reads a
   feed from a stub, downloads the signed app jar, and keeps it in its data
   directory. The key pair is a throwaway: the JVM trusts it through the env var
   FREEMEMO_UPDATE_PUBLIC_KEY (the installer sets the system property of the
   same purpose). That the next start runs the new jar is the bootstrap's job:
   `freememo.app-bootstrap-test` starts the real bootstrap over fake apps, because
   Seam 1 starts the app from source, not from a jar, so it has no bootstrap to
   restart through. Each test starts one Local Install JVM, about a minute."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [freememo.app-update :as au]
            [freememo.app-update-test :as t]
            [freememo.local-install-harness :as h]
            [freememo.local-install-stub :as stub]))

(def ^:private installed "26.11.100")
(def ^:private new-version "26.12.5")

(defn- feed [sha origin & [runtime]]
  {"current_version" new-version
   "installers" {"macos" "https://example.test/mac.dmg" "windows" "https://example.test/win.msi"
                 "linux" "https://example.test/linux.deb"}
   "app_update" {"url" (str origin "/dl/app.jar") "sha256" sha "signature_url" (str origin "/dl/app.jar.sig")}
   "runtime" (or runtime 1) "sync_protocol" 1})

(defn- with-app-update*
  "Run `(f li dir)` with a feed that offers a jar signed by `signer`, where the
   Local Install trusts the public key of `trusted`. `dir` is its app directory."
  [signer trusted runtime f]
  (let [src (t/temp-dir)
        {:keys [jar sig]} (t/write-jar-and-sig! src new-version "a new app jar" signer)
        files {"/dl/app.jar" (java.nio.file.Files/readAllBytes (.toPath jar))
               "/dl/app.jar.sig" (java.nio.file.Files/readAllBytes (.toPath sig))}
        s (stub/start! {:files files})]
    (swap! (:state s) assoc :feed (feed (au/sha256-hex jar) (:origin s) runtime))
    (try
      (let [li (h/start-local-install!
                 {:origin (:origin s)
                  :env {"FREEMEMO_TEST_RELEASE_VERSION" installed
                        "FREEMEMO_UPDATE_PUBLIC_KEY" (.encodeToString (java.util.Base64/getEncoder)
                                                      (.getEncoded (.getPublic trusted)))}})]
        (try (f li (au/updates-dir (:data-dir li)))
             (finally (when (h/alive? li) (h/stop! li)) (h/delete-tree! (:data-dir li)))))
      (finally (stub/stop! s) (t/delete-tree! src)))))

(defn- with-app-update
  [signer trusted f] (with-app-update* signer trusted 1 f))

(defn- await-file [^java.io.File f]
  (let [deadline (+ (System/currentTimeMillis) 90000)]
    (while (and (not (.exists f)) (< (System/currentTimeMillis) deadline)) (Thread/sleep 500))
    (Thread/sleep 1000)))

(deftest a-newer-signed-jar-is-downloaded-and-kept
  (let [kp (t/key-pair)]
    (with-app-update kp kp
      (fn [_ dir]
        (await-file (au/jar-file dir new-version))
        (is (.exists (au/jar-file dir new-version)))
        (is (.exists (au/sig-file dir new-version)))
        (is (= #{"freememo-26.12.5.jar" "freememo-26.12.5.jar.sig"}
              (set (map #(.getName ^java.io.File %) (.listFiles dir)))) "no temp file stays")))))

(deftest a-jar-with-a-bad-signature-is-deleted-and-the-log-has-one-line
  (let [good (t/key-pair) evil (t/key-pair)]
    (with-app-update evil good
      (fn [li dir]
        (await-file (au/failed-file dir new-version))
        (is (not (.exists (au/jar-file dir new-version))))
        (is (not (.exists (au/sig-file dir new-version))))
        (is (= 1 (count (re-seq #"app update 26.12.5 not used: the signature is not valid" (h/log-text li)))))))))

(deftest a-higher-runtime-downloads-no-jar-and-offers-the-installer
  (let [kp (t/key-pair)]
    (with-app-update* kp kp 2
      (fn [li dir]
        (let [cookie (:cookie (h/sign-in! li))
              deadline (+ (System/currentTimeMillis) 60000)
              state (loop []
                      (let [st (json/parse-string (:body (h/http (h/url li "/api/desktop/update") {:cookie cookie})))]
                        (if (or (get st "installer") (> (System/currentTimeMillis) deadline))
                          st (do (Thread/sleep 500) (recur)))))]
          (Thread/sleep 3000)
          (is (= "ready" (get-in state ["installer" "status"])))
          (is (= new-version (get-in state ["installer" "version"])))
          (is (re-find #"example\.test/" (get-in state ["installer" "url"])))
          (is (not (.exists (au/jar-file dir new-version))) "no jar is downloaded")
          (is (nil? (some #(re-find #"^freememo-.*\.jar" (.getName ^java.io.File %)) (or (.listFiles dir) []))))
          (is (re-find #"needs a newer Runtime" (h/log-text li))))))))
