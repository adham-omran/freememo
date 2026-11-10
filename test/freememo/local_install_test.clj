(ns freememo.local-install-test
  "Seam 1 for ticket 03 of plans/local-install.md: the Local Install JVM, started
   with a temporary data directory, seen only through HTTP.
   Needs the dev PostgreSQL on 5432 (for the server that plays freememo.net),
   a client build, and about a minute for each JVM start."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [freememo.local-install-harness :as h])
  (:import [java.lang ProcessHandle]
           [java.net InetAddress ServerSocket]))

(def ^:dynamic *server* nil)
(def ^:dynamic *li* nil)
(def ^:dynamic *sign-in* nil)

(defn- with-server-and-install [f]
  (let [server (h/start-test-server!)
        li (h/start-local-install! {:origin (:origin server)})]
    (try
      ;; The start's one token is spent here, once, by the first browser.
      (binding [*server* server *li* li *sign-in* (h/sign-in! li)]
        (f))
      (finally
        (when (h/alive? li) (h/stop! li))
        (h/stop-test-server! server)
        (h/delete-tree! (:data-dir li))))))

(use-fixtures :once with-server-and-install)

(defn- postgres-running-for?
  "True when a process runs with `dir` in its command line."
  [dir]
  (boolean
    (some #(str/includes? (.orElse (.commandLine (.info ^ProcessHandle %)) "") (.getPath dir))
      (iterator-seq (.iterator (ProcessHandle/allProcesses))))))

(defn- page-with-cookie [li cookie]
  (h/http (h/url li "/") {:cookie cookie}))

(deftest the-token-url-signs-in-and-the-page-shows-the-local-mark
  (let [{:keys [cookie location response]} *sign-in*
        set-cookie (first (get-in response [:headers "set-cookie"]))]
    (testing "the token is exchanged for a strict HttpOnly cookie and a redirect without the token"
      (is (= 302 (:status response)))
      (is (str/starts-with? cookie "fm_local="))
      (is (str/includes? set-cookie "HttpOnly"))
      (is (str/includes? set-cookie "SameSite=Strict"))
      (is (= "/" location)))
    (testing "the page carries the Local mark"
      (let [page (page-with-cookie *li* cookie)]
        (is (= 200 (:status page)))
        (is (str/includes? (:body page) "<title>FreeMemo Local"))))))

(deftest a-token-works-once
  (testing "the fixture spent the start's token, so a second use is refused"
    (is (= 401 (:status (h/http (:open-url *li*) {}))))))

(deftest the-gate-refuses-what-it-must
  (let [li *li*
        cookie (:cookie *sign-in*)]
    (testing "no cookie"
      (is (= 401 (:status (h/http (h/url li "/") {})))))
    (testing "a wrong Host, even with the cookie"
      (is (= 403 (:status (h/http (h/url li "/") {:cookie cookie :headers {"Host" "evil.example"}})))))
    (testing "a POST with a foreign Origin, even with the cookie"
      (is (= 403 (:status (h/http (h/url li "/api/desktop/sync-now")
                            {:method :post :cookie cookie :body "{}"
                             :headers {"Origin" "https://evil.example"}})))))
    (testing "a POST with the local Origin passes the gate"
      (is (not= 403 (:status (h/http (h/url li "/api/desktop/connectivity")
                               {:method :post :cookie cookie :body "{\"online\":true}"
                                :headers {"Origin" (str "http://127.0.0.1:" (:port li))}})))))))

(deftest a-restart-keeps-the-port-and-the-cookie-and-quit-leaves-nothing
  (let [dir (h/temp-dir "li-restart")
        first-run (h/start-local-install! {:data-dir dir :origin (:origin *server*)})]
    (try
      (let [{:keys [cookie]} (h/sign-in! first-run)]
        (is (some? cookie))
        (is (contains? #{0 143} (h/stop! first-run)) "exit 143 is SIGTERM after the Quit hook ran")
        (testing "Quit leaves no JVM and no PostgreSQL"
          (is (not (h/alive? first-run)))
          (is (not (postgres-running-for? dir))))
        (let [second-run (h/start-local-install! {:data-dir dir :port (h/free-port)})]
          (try
            (is (= (:port first-run) (:port second-run)))
            (is (= 200 (:status (page-with-cookie second-run cookie))))
            (finally (h/stop! second-run)))))
      (finally (h/delete-tree! dir)))))

(deftest a-taken-port-moves-to-the-next-free-port-and-stays-there
  (let [dir (h/temp-dir "li-port")
        taken (h/free-port)
        blocker (ServerSocket. taken 1 (InetAddress/getByName "127.0.0.1"))]
    (try
      (let [first-run (h/start-local-install! {:data-dir dir :port taken :origin (:origin *server*)})]
        (is (not= taken (:port first-run)))
        (h/stop! first-run)
        (.close blocker)
        (let [second-run (h/start-local-install! {:data-dir dir :port taken})]
          (try
            (is (= (:port first-run) (:port second-run)) "the moved port is kept")
            (finally (h/stop! second-run)))))
      (finally
        (.close blocker)
        (h/delete-tree! dir)))))

(deftest a-killed-jvm-is-recovered-at-the-next-start
  (let [dir (h/temp-dir "li-kill")
        first-run (h/start-local-install! {:data-dir dir :origin (:origin *server*)})]
    (try
      (h/kill! first-run)
      (is (.exists (io/file dir "pgdata" "postmaster.pid")) "the crash left its pid file")
      (let [second-run (h/start-local-install! {:data-dir dir})]
        (try
          (let [{:keys [cookie]} (h/sign-in! second-run)]
            (is (= 200 (:status (page-with-cookie second-run cookie)))))
          (finally (h/stop! second-run))))
      (finally
        (h/delete-tree! dir)))))
