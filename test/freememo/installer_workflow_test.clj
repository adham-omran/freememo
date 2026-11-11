(ns freememo.installer-workflow-test
  "Ticket 17: the installers workflow runs in the public repository, so every
   file it names must survive tools/publish-clean/EXCLUDED, and the asset names
   must follow the one documented pattern."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private workflow (io/file (str "." "github") "workflows" "installers.yml"))

(defn- excluded-paths []
  (->> (str/split-lines (slurp "tools/publish-clean/EXCLUDED"))
    (map str/trim)
    (remove #(or (str/blank? %) (str/starts-with? % "#")))))

(defn- excluded? [path]
  (some #(or (= path %) (str/starts-with? path (str % "/"))) (excluded-paths)))

(defn- repo-paths-in [text]
  (->> (re-seq #"(?:tools|src|resources|src-build|src-prod)/[A-Za-z0-9_./-]+|deps\.edn" text)
    (map #(str/replace % #"[.]+$" ""))
    distinct))

(deftest workflow-uses-only-public-paths
  (let [paths (repo-paths-in (slurp workflow))]
    (is (seq paths))
    (doseq [p paths]
      (testing p
        (is (not (excluded? p)) (str p " is excluded from the public history"))))
    (doseq [p ["tools/installer/build.mjs" "tools/installer/asset-names.mjs" "tools/installer/smoke.sh"
               "tools/release/prune.sh" "resources/public/freememo/icon-512.png"
               "resources/public/favicon.ico"]]
      (testing p
        (is (.exists (io/file p)))
        (is (not (excluded? p)))))))

(deftest workflow-publishes-only-after-every-asset-is-attached
  (let [text (slurp workflow)
        at (fn [s] (str/index-of text s))]
    (is (at "Check the draft lists every asset"))
    (is (< (at "Check the draft lists every asset") (at "--draft=false") (str/last-index-of text "prune.sh")))
    (is (str/includes? text "needs: [version, linux, macos, windows]")
        "the release job waits for every installer job")))

(deftest asset-names-follow-the-pattern
  (let [names (-> (.. (ProcessBuilder. ^java.util.List ["node" "tools/installer/asset-names.mjs" "26.11.13832"])
                    (redirectErrorStream true) start)
                (.getInputStream) slurp str/split-lines)]
    (is (= ["freememo-26.11.13832-macos-arm64.dmg" "freememo-26.11.13832-windows-x64.msi"
            "freememo-26.11.13832-linux-x64.deb" "freememo-26.11.13832-app.jar"]
           names))
    (is (every? #(re-matches #"freememo-[0-9]+[.][0-9]+[.][0-9]+-(macos-arm64[.]dmg|windows-x64[.]msi|linux-x64[.]deb|app[.]jar)" %) names))))
