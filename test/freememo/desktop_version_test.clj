(ns freememo.desktop-version-test
  "plans/electron-local-db-sync.md 13.3. No database."
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [freememo.desktop-version :as desktop-version]))

(deftest the-release-version-stays-out-of-git
  (let [version (get (json/parse-string (slurp "electron/package.json")) "version")]
    (is (= "0.0.0" version)
      "electron/scripts/dist.mjs gives electron-builder YYYY.M.N from git, so package.json holds a placeholder")))
