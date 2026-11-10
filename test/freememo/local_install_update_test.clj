(ns freememo.local-install-update-test
  "Seam 1 for ticket 08 of plans/local-install.md: the update feed and the
   old-major message. A stub plays freememo.net (`freememo.local-install-stub`).
   The web app's banner and `GET /api/desktop/update` read the same function,
   `update-feed/banner-state`, so the test reads the endpoint through HTTP.
   Each test starts one Local Install JVM, about a minute."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.local-install-harness :as h]
            [freememo.local-install-stub :as stub]))

(def ^:private installed "2026.11.100")

(defn- feed [version]
  {"current_major" 1 "current_version" version
   "installers" {"macos" "https://example.test/mac.dmg"
                 "windows" "https://example.test/win.exe"
                 "linux" "https://example.test/linux.deb"}})

(defn- with-install
  "Run `(f li cookie stub)` with a Local Install at `installed`, and a stub that
   starts with `initial`."
  [initial f]
  (let [s (stub/start! initial)
        li (h/start-local-install! {:origin (:origin s) :env {"FREEMEMO_VERSION" installed}})]
    (try
      (f li (:cookie (h/sign-in! li)) s)
      (finally
        (when (h/alive? li) (h/stop! li))
        (stub/stop! s)
        (h/delete-tree! (:data-dir li))))))

(defn- banner-state [li cookie]
  (json/parse-string (:body (h/http (h/url li "/api/desktop/update") {:cookie cookie}))))

(defn- await-state
  "Post: the first banner state that satisfies `pred`, or the last one after 60 s."
  [li cookie pred]
  (let [deadline (+ (System/currentTimeMillis) 60000)]
    (loop []
      (let [state (banner-state li cookie)]
        (if (or (pred state) (> (System/currentTimeMillis) deadline))
          state
          (do (Thread/sleep 500) (recur)))))))

(defn- await-feed-read [s]
  (let [deadline (+ (System/currentTimeMillis) 60000)]
    (while (and (< (System/currentTimeMillis) deadline) (zero? (:feed-hits @(:state s) 0)))
      (Thread/sleep 250))
    (Thread/sleep 1500)))

(deftest a-newer-version-shows-update-available
  (with-install {:feed (feed "2026.12.5")}
    (fn [li cookie _]
      (let [state (await-state li cookie #(get % "update-available"))]
        (is (= {"version" "2026.12.5" "url" (str "https://example.test/"
                                              (if (str/includes? (str/lower-case (System/getProperty "os.name")) "mac")
                                                "mac.dmg" "linux.deb"))}
              (get state "update-available"))
          "the installer URL is the one for this OS")
        (is (false? (get state "update-required?")))))))

(deftest the-same-version-shows-nothing
  (with-install {:feed (feed installed)}
    (fn [li cookie s]
      (await-feed-read s)
      (is (= {"update-available" nil "update-required?" false} (banner-state li cookie))))))

(deftest a-feed-that-cannot-be-read-shows-nothing-and-logs-one-line
  (with-install {:feed nil}
    (fn [li cookie s]
      (await-feed-read s)
      (is (= {"update-available" nil "update-required?" false} (banner-state li cookie)))
      (is (= 1 (count (re-seq #"cannot read the update feed" (h/log-text li))))))))

(deftest a-refused-major-shows-update-to-keep-syncing
  (with-install {:feed (feed "2026.12.5")
                 :meta-status 409
                 :meta-body {"error" "update-required" "protocol" 2000}}
    (fn [li cookie _]
      (testing "the sync meta step answers 409, the sync state becomes :update-required"
        (let [state (await-state li cookie #(get % "update-required?"))]
          (is (true? (get state "update-required?"))))))))
