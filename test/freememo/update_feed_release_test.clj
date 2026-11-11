(ns freememo.update-feed-release-test
  "The server half of the update feed (plans/local-install/issues/18-feed-from-github-release.md).
   A fake GitHub (a local HttpServer) plays api.github.com. No real call is made."
  (:require [cheshire.core :as json]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [freememo.update-feed :as uf])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.net InetSocketAddress]))

(def ^:private rv "26.11.13832")

(defn- release
  "A GitHub release as the API gives it. `:without` drops asset kinds."
  [& {:keys [draft without base] :or {base "http://x"}}]
  {"tag_name" (str "v" rv) "draft" (boolean draft)
   "assets" (for [n (uf/required-asset-names rv)
                  :when (not (some #(= n (uf/asset-name rv %)) without))]
              {"name" n "browser_download_url" (str base "/dl/" n)})})

(def ^:private release-meta
  {"release_version" rv "runtime" 1 "sync_protocol" 1 "app_jar_sha256" "abc123"})

(defn- start-fake [!state]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
        reply (fn [^HttpExchange ex status body]
                (let [bytes (.getBytes (str body) "UTF-8")]
                  (.sendResponseHeaders ex status (count bytes))
                  (with-open [o (.getResponseBody ex)] (.write o bytes))))
        base (str "http://127.0.0.1:" (.getPort (.getAddress server)))]
    (.createContext server "/"
      (reify HttpHandler
        (handle [_ ex]
          (let [path (.getPath (.getRequestURI ex))
                {:keys [status release meta]} @!state]
            (swap! !state update :hits (fnil inc 0))
            (cond
              (not= 200 status) (reply ex status "{}")
              (str/ends-with? path "/releases/latest") (reply ex 200 (json/generate-string release))
              (str/ends-with? path "-release.json") (reply ex 200 (json/generate-string meta))
              :else (reply ex 404 "{}"))))))
    (.start server)
    {:server server :base base}))

(defn- with-fake
  "Run `(f !state opts)` against a fake GitHub that starts with `initial`."
  [initial f]
  (let [!state (atom (merge {:status 200 :release nil :meta release-meta} initial))
        {:keys [server base]} (start-fake !state)]
    (swap! !state update :release #(or % (release :base base)))
    (try (f !state {:api-base base :repo "o/r"})
         (finally (.stop ^HttpServer server 0)))))

(use-fixtures :each (fn [t] (uf/reset-release-cache!) (t)))

(deftest a-fake-release-gives-a-feed-with-every-field
  (with-fake {}
    (fn [_ opts]
      (let [feed (uf/feed-body opts)]
        (is (= rv (get feed "current_version")))
        (is (= #{"macos" "windows" "linux"} (set (keys (get feed "installers")))))
        (is (str/ends-with? (get-in feed ["installers" "windows"]) (str "freememo-" rv "-windows-x64.msi")))
        (is (str/ends-with? (get-in feed ["app_update" "url"]) (str "freememo-" rv "-app.jar")))
        (is (= "abc123" (get-in feed ["app_update" "sha256"])))
        (is (nil? (get-in feed ["app_update" "signature_url"])) "no signature yet")
        (is (= 1 (get feed "runtime")))
        (is (= 1 (get feed "sync_protocol")))))))

(deftest the-signature-url-is-given-when-the-asset-exists
  (let [rel (update (release) "assets" concat
              [{"name" (uf/asset-name rv :sig) "browser_download_url" "http://x/sig"}])]
    (is (= "http://x/sig" (get-in (uf/feed-from-release rel release-meta) ["app_update" "signature_url"])))))

(deftest a-draft-or-an-incomplete-release-is-not-used
  (is (nil? (uf/feed-from-release (release :draft true) release-meta)))
  (doseq [k [:macos :windows :linux :app :release-json]]
    (is (nil? (uf/feed-from-release (release :without [k]) release-meta)) (str "missing " k)))
  (is (nil? (uf/feed-from-release (release) (assoc release-meta "release_version" "1.2.3"))) "meta of another release")
  (is (nil? (uf/feed-from-release (release) (dissoc release-meta "runtime"))))
  (is (some? (uf/feed-from-release (release) release-meta))))

(deftest an-unusable-latest-release-answers-nothing
  (with-fake {:release (release :without [:linux])}
    (fn [_ opts] (is (nil? (uf/feed-body opts))))))

(deftest with-no-good-answer-the-route-answers-404
  (with-fake {:status 500}
    (fn [_ opts]
      (with-redefs [uf/feed-body (let [f uf/feed-body] (fn [] (f opts)))]
        (is (= 404 (:status (uf/feed-handler {}))))))))

(deftest github-is-asked-once-per-ttl
  (with-fake {}
    (fn [!state opts]
      (uf/feed-body opts) (uf/feed-body opts) (uf/feed-body opts)
      (is (= 2 (:hits @!state)) "one release call and one release.json call"))))

(deftest a-github-failure-gives-the-last-good-answer-and-logs-one-line
  (with-fake {}
    (fn [!state opts]
      (let [good (uf/feed-body opts)
            logs (atom [])]
        (is (some? good))
        (with-redefs [uf/cache-ttl-ms 0
                      uf/log-github-failure (fn [msg] (swap! logs conj msg))]
          (swap! !state assoc :status 503)
          (is (= good (uf/feed-body opts)))
          (is (= good (uf/feed-body opts)) "the retry delay holds the second call")
          (is (= 1 (count @logs)))
          (is (= 3 (:hits @!state)) "two calls for the good answer, one failed call"))))))

(deftest the-asset-lists-of-server-and-installer-agree
  (let [{:keys [exit out]} (sh/sh "node" "tools/installer/asset-names.mjs" rv)]
    (is (zero? exit))
    (is (= (set (str/split-lines out)) (set (uf/required-asset-names rv))))))
