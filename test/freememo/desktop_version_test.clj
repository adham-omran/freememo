(ns freememo.desktop-version-test
  "plans/electron-local-db-sync.md 13.3. No database."
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [freememo.desktop-version :as desktop-version]))

(deftest the-sync-protocol-belongs-to-the-declared-major
  (is (= desktop-version/desktop-major (desktop-version/protocol-major desktop-version/sync-protocol))
    "13.3.1: change sync-protocol only together with desktop-major"))

(deftest the-installer-carries-the-declared-major
  (let [version (get (json/parse-string (slurp "electron/package.json")) "version")]
    (is (= desktop-version/desktop-major (parse-long (first (clojure.string/split version #"\."))))
      "electron/package.json's major is the major a license covers")))
