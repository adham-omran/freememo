(ns freememo.desktop-version-test
  "plans/electron-local-db-sync.md 13.3, and the Runtime guard
   (plans/runtime-guard.md). No database."
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [freememo.desktop-version :as desktop-version])
  (:import [java.security MessageDigest]))

(deftest the-release-version-stays-out-of-git
  (let [version (get (json/parse-string (slurp "electron/package.json")) "version")]
    (is (= "0.0.0" version)
      "electron/scripts/dist.mjs gives electron-builder YYYY.M.N from git, so package.json holds a placeholder")))

;; ── The Runtime guard ──────────────────────────────────────────────
;;
;; An App Update replaces only freememo.jar. These inputs reach a user only
;; through a new installer. A change to them that the Runtime does not record
;; shipped silently on 2026-11-12: the fix was in the new installer, and no
;; installed app said that one existed.

(def installer-inputs
  "The files that make what only an installer replaces: the bootstrap, the
   jpackage options, the JRE modules, the bundled tools and PostgreSQL."
  ["tools/installer/bootstrap/FreememoBoot.java"
   "tools/installer/build.mjs"])

(defn- java-version-line
  "The JDK of the bundled JRE, from the installers workflow."
  []
  (->> (str/split-lines (slurp ".github/workflows/installers.yml"))
    (filter #(re-find #"^\s*JAVA_VERSION:" %))
    first
    str/trim))

(defn installer-digest
  "Post: the SHA-256 hex of the installer inputs and the JDK line."
  []
  (let [md (MessageDigest/getInstance "SHA-256")]
    (doseq [p installer-inputs]
      (.update md (.getBytes (str p "\n" (slurp (io/file p)) "\n") "UTF-8")))
    (.update md (.getBytes (java-version-line) "UTF-8"))
    (apply str (map #(format "%02x" %) (.digest md)))))

(def recorded
  "The Runtime decision for the installer inputs as they are now. Edit an
   input, and this test fails until you decide:
   - The installed app needs the change (a launcher, JRE, tool or PostgreSQL
     change): raise `freememo.desktop-version/runtime` by one. Each install
     then stops taking App Updates and shows \"Download the new installer\".
   - It does not (a comment, a build-only step): keep the Runtime.
   Then set both values here to what the test prints."
  {:runtime 2
   :sha256 "00aca10735972245f4bf25b44e3eb8ff7766b1e988d1318936e9c81b2892a282"})

(deftest each-installer-change-has-a-runtime-decision
  (let [now {:runtime desktop-version/runtime :sha256 (installer-digest)}]
    (is (= recorded now)
      (str "An installer input changed: " (str/join ", " installer-inputs)
           " or JAVA_VERSION in .github/workflows/installers.yml. An App Update"
           " cannot deliver it. Decide whether to raise"
           " freememo.desktop-version/runtime (see `recorded`), then record " (pr-str now)))))
