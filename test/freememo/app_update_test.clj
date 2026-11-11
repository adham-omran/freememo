(ns freememo.app-update-test
  "Ticket 19: the App Update half that runs inside the app. Key pairs here are
   throwaways. The bootstrap is in `freememo.app-bootstrap-test`."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.app-update :as au]
            [freememo.desktop-version :as dv])
  (:import [java.io ByteArrayInputStream File InputStream]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.security KeyPairGenerator Signature]))

(defn temp-dir ^File [] (.toFile (Files/createTempDirectory "au-test" (make-array FileAttribute 0))))

(defn delete-tree! [^File f]
  (when (.exists f) (doseq [c (reverse (file-seq f))] (.delete ^File c))))

(defn key-pair [] (.generateKeyPair (KeyPairGenerator/getInstance "Ed25519")))

(defn sign-bytes
  "Post: the signature bytes of the release statement, as the workflow makes them."
  [kp rv ^File jar]
  (let [s (doto (Signature/getInstance "Ed25519") (.initSign (.getPrivate kp)))]
    (.update s (.getBytes ^String (au/statement rv (au/sha256-hex jar)) StandardCharsets/UTF_8))
    (.sign s)))

(defn write-jar-and-sig!
  "Post: `{:jar :sig}` files in `dir` with the pickable names for `rv`."
  [^File dir rv content kp]
  (.mkdirs dir)
  (let [jar (au/jar-file dir rv)]
    (spit jar content)
    (let [sig (au/sig-file dir rv)]
      (Files/write (.toPath sig) ^bytes (sign-bytes kp rv jar) (make-array java.nio.file.OpenOption 0))
      {:jar jar :sig sig})))

(deftest verify-accepts-only-the-signed-jar-of-that-version
  (let [dir (temp-dir) kp (key-pair) other (key-pair)
        {:keys [jar sig]} (write-jar-and-sig! dir "26.11.2" "jar bytes" kp)
        args {:public-key (.getPublic kp) :version "26.11.2" :jar jar :sig sig}]
    (try
      (is (true? (au/verify args)))
      (testing "another version"
        (is (not (au/verify (assoc args :version "26.11.3")))))
      (testing "another key"
        (is (not (au/verify (assoc args :public-key (.getPublic other))))))
      (testing "a changed jar"
        (spit jar "jar bytes!")
        (is (not (au/verify args))))
      (testing "a missing signature or key"
        (.delete sig)
        (is (not (au/verify args)))
        (is (not (au/verify (assoc args :public-key nil)))))
      (finally (delete-tree! dir)))))

(deftest a-signature-made-by-openssl-as-the-workflow-does-verifies
  (let [dir (temp-dir)]
    (try
      (let [key (io/file dir "key.pem") jar (io/file dir "freememo-26.11.9-app.jar")
            stmt (io/file dir "statement") sig (io/file dir "sig")
            _ (spit jar "the app")
            _ (sh/sh "openssl" "genpkey" "-algorithm" "ed25519" "-out" (.getPath key))
            pub (:out (sh/sh "sh" "-c" (str "openssl pkey -in " (.getPath key) " -pubout -outform DER | base64 -w0")))
            _ (spit stmt (au/statement "26.11.9" (au/sha256-hex jar)))
            r (sh/sh "openssl" "pkeyutl" "-sign" "-rawin" "-inkey" (.getPath key) "-in" (.getPath stmt) "-out" (.getPath sig))]
        (is (zero? (:exit r)) (:err r))
        (is (true? (au/verify {:public-key (au/parse-public-key pub) :version "26.11.9" :jar jar :sig sig}))))
      (finally (delete-tree! dir)))))

(def ^:private feed
  {"current_version" "26.12.1" "runtime" dv/runtime "sync_protocol" 1
   "app_update" {"url" "http://x/app.jar" "sha256" "abc" "signature_url" "http://x/app.jar.sig"}})

(deftest decision-follows-runtime-and-signature
  (is (nil? (au/decision feed "26.12.1")) "the same version")
  (is (= :download (:action (au/decision feed "26.11.1"))))
  (let [d (au/decision (assoc feed "runtime" (inc dv/runtime)) "26.11.1")]
    (is (= :skip (:action d)))
    (is (str/includes? (:reason d) "new installer")))
  (is (= :skip (:action (au/decision (update feed "app_update" dissoc "signature_url") "26.11.1")))
    "an unsigned release is not used")
  (is (= :skip (:action (au/decision (dissoc feed "sync_protocol") "26.11.1")))))

(defn- stream-of [^bytes b] (ByteArrayInputStream. b))

(defn- failing-stream
  "A stream that gives `n` bytes, then throws, as a stopped download does."
  ^InputStream [n]
  (let [!left (atom n)]
    (proxy [InputStream] []
      (read
        ([] (if (pos? @!left) (do (swap! !left dec) 65) (throw (java.io.IOException. "connection reset"))))
        ([^bytes buf]
         (if (pos? @!left)
           (let [k (min (alength buf) @!left)] (dotimes [i k] (aset-byte buf i 65)) (swap! !left - k) k)
           (throw (java.io.IOException. "connection reset"))))))))

(defn- listing [^File dir] (sort (map #(.getName ^File %) (.listFiles dir))))

(deftest a-good-download-gets-its_name-in-one-rename
  (let [data (temp-dir) kp (key-pair) src (temp-dir)]
    (try
      (let [{:keys [jar sig]} (write-jar-and-sig! src "26.12.1" "new app" kp)
            sha (au/sha256-hex jar)
            files {"http://x/app.jar" (Files/readAllBytes (.toPath jar))
                   "http://x/app.jar.sig" (Files/readAllBytes (.toPath sig))}
            r (au/download! data {:version "26.12.1" :url "http://x/app.jar" :sha256 sha :signature-url "http://x/app.jar.sig"}
                (.getPublic kp) {:open-stream #(stream-of (files %))})]
        (is (= :ready (:status r)))
        (is (= ["freememo-26.12.1.jar" "freememo-26.12.1.jar.sig"] (listing (au/updates-dir data))))
        (is (= "26.12.1" (:version (au/staged data "26.11.1" (.getPublic kp)))))
        (is (nil? (au/staged data "26.12.1" (.getPublic kp))) "not newer than the installed one"))
      (finally (delete-tree! data) (delete-tree! src)))))

(deftest a-download-that-stops-halfway-leaves-nothing-pickable
  (let [data (temp-dir) kp (key-pair)]
    (try
      (let [r (au/download! data {:version "26.12.1" :url "u" :sha256 "abc" :signature-url "s"}
                (.getPublic kp) {:open-stream (fn [_] (failing-stream 1000))})]
        (is (= :failed (:status r)))
        (is (str/includes? (:reason r) "connection reset"))
        (is (empty? (listing (au/updates-dir data))) "no jar, no signature, no temp file"))
      (finally (delete-tree! data)))))

(deftest a-wrong-hash-is-deleted-and-may-be-tried-again
  (let [data (temp-dir) kp (key-pair)]
    (try
      (let [r (au/download! data {:version "26.12.1" :url "u" :sha256 "00" :signature-url "s"}
                (.getPublic kp) {:open-stream (fn [_] (stream-of (.getBytes "data")))})]
        (is (= :failed (:status r)))
        (is (empty? (listing (au/updates-dir data)))))
      (finally (delete-tree! data)))))

(deftest a-bad-signature-is-deleted-marked-and-never-downloaded-again
  (let [data (temp-dir) kp (key-pair) bad (key-pair) src (temp-dir) logs (atom [])]
    (try
      (let [{:keys [jar sig]} (write-jar-and-sig! src "26.12.1" "evil" bad)
            files {"u" (Files/readAllBytes (.toPath jar)) "s" (Files/readAllBytes (.toPath sig))}
            d {:version "26.12.1" :url "u" :sha256 (au/sha256-hex jar) :signature-url "s"}
            r (au/download! data d (.getPublic kp) {:open-stream #(stream-of (files %))})]
        (is (= :rejected (:status r)))
        (is (= ["freememo-26.12.1.jar.failed"] (listing (au/updates-dir data))) "no jar; a marker stops a new download")
        (let [opens (atom 0)
              status (au/check! {:data-dir data :installed "26.11.1" :public-key (.getPublic kp)
                                 :feed (assoc feed "app_update" {"url" "u" "sha256" "x" "signature_url" "s"})
                                 :open-stream (fn [u] (swap! opens inc) (stream-of (files u)))})]
          (is (= :failed-before status))
          (is (zero? @opens))))
      (finally (delete-tree! data) (delete-tree! src) (reset! logs [])))))

(deftest prune-keeps-the-current-and-the-previous-jar
  (let [dir (temp-dir) kp (key-pair)]
    (try
      (doseq [v ["26.11.1" "26.11.2" "26.11.3" "26.11.4"]] (write-jar-and-sig! dir v v kp))
      (spit (au/failed-file dir "26.11.4") "x")
      (is (= ["26.11.2" "26.11.3"] (au/prune! dir)) "the failed one does not count")
      (is (= ["freememo-26.11.2.jar" "freememo-26.11.2.jar.sig" "freememo-26.11.3.jar" "freememo-26.11.3.jar.sig"
              "freememo-26.11.4.jar.failed"]
            (listing dir)))
      (finally (delete-tree! dir)))))

(deftest check-reports-ready-only-after-the-jar-is-verified
  (let [data (temp-dir) kp (key-pair) src (temp-dir) ready (atom [])]
    (try
      (au/reset-state!)
      (let [{:keys [jar sig]} (write-jar-and-sig! src "26.12.1" "new app" kp)
            files {"http://x/app.jar" (Files/readAllBytes (.toPath jar)) "http://x/app.jar.sig" (Files/readAllBytes (.toPath sig))}
            f (assoc feed "app_update" {"url" "http://x/app.jar" "sha256" (au/sha256-hex jar) "signature_url" "http://x/app.jar.sig"})
            opts {:data-dir data :installed "26.11.1" :public-key (.getPublic kp) :feed f
                  :open-stream #(stream-of (files %)) :on-ready #(swap! ready conj %)}]
        (is (nil? (au/ready-version)))
        (is (= :ready (au/check! opts)))
        (is (= ["26.12.1"] @ready))
        (is (= "26.12.1" (au/ready-version)))
        (testing "a later check finds the staged jar and does not download"
          (is (= :ready (au/check! (assoc opts :open-stream (fn [_] (throw (ex-info "no" {}))))))))
        (testing "a higher Runtime is skipped"
          (au/reset-state!)
          (is (= :skipped (au/check! (assoc opts :data-dir (temp-dir) :feed (assoc f "runtime" 99)))))))
      (testing "no public key turns App Update off"
        (au/reset-state!)
        (with-redefs [au/configured-public-key (constantly nil)]
          (is (= :off (au/check! {:data-dir data :installed "26.11.1" :feed feed})))))
      (finally (au/reset-state!) (delete-tree! data) (delete-tree! src)))))

(deftest the-web-app-offers-a-ready-update-until-it-is-dismissed
  ;; The banner strip asks `banner-state`. Before this, only the tray said that
  ;; an update waited, and a Mac that starts at login could wait for weeks.
  (try
    (testing "nothing is ready: no banner"
      (with-redefs [au/ready-version (constantly nil) au/restart-available? (constantly true)]
        (is (nil? (au/banner-state)))))
    (testing "a ready update under the bootstrap gives its version"
      (with-redefs [au/ready-version (constantly "26.11.15322") au/restart-available? (constantly true)]
        (is (= {:version "26.11.15322"} (au/banner-state)))))
    (testing "no bootstrap: the app cannot restart itself, so no banner"
      (with-redefs [au/ready-version (constantly "26.11.15322") au/restart-available? (constantly false)]
        (is (nil? (au/banner-state)))))
    (testing "a dismissed version stays hidden, and a later version shows again"
      (au/dismiss-banner! "26.11.15322")
      (with-redefs [au/ready-version (constantly "26.11.15322") au/restart-available? (constantly true)]
        (is (nil? (au/banner-state))))
      (with-redefs [au/ready-version (constantly "26.11.15400") au/restart-available? (constantly true)]
        (is (= {:version "26.11.15400"} (au/banner-state)))))
    (testing "restart! does nothing when nothing is ready"
      (with-redefs [au/ready-version (constantly nil) au/restart-available? (constantly true)]
        (is (false? (au/restart!)))))
    (finally (au/reset-state!))))
