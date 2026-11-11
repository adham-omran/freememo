(ns freememo.local-install-role-test
  "The peer role of a Local Install JVM (Windows test, 2026-11-11). The
   first-launch tray loaded freememo.config, and with it freememo.peer, before
   the launcher set FREEMEMO_PEER. The process then served as freememo.net:
   no gate, no sync, and a server on 0.0.0.0. No PostgreSQL, no child JVM."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [freememo.launch-env :as launch-env]
            [freememo.local-install :as li]
            [freememo.local-signin :as signin]
            [freememo.peer :as peer]))

(def ^:private role-keys ["FREEMEMO_PEER" "FREEMEMO_LOCAL_INSTALL" "ENC_KEY_SECRET" "COOKIE_SECURE" "AUTH_MODE"])

(defn- clear-role-env! []
  (doseq [k role-keys] (System/clearProperty (launch-env/property-name k))))

(defn- temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory "fm-role" (make-array java.nio.file.attribute.FileAttribute 0))))

(deftest the-role-is-set-before-the-sign-in-starts
  (let [dir (temp-dir) seen (atom nil)]
    (try
      (with-redefs-fn {#'li/sign-in-phase! (fn [_]
                                             (reset! seen (into {} (map (juxt identity launch-env/getenv)) role-keys))
                                             (throw (ex-info "stop here" {:stop true})))}
        #(is (thrown-with-msg? clojure.lang.ExceptionInfo #"stop here"
               (li/start! {:data-dir dir :port 47950 :no-tab true}))))
      (is (= "desktop" (get @seen "FREEMEMO_PEER")))
      (is (= "true" (get @seen "FREEMEMO_LOCAL_INSTALL")))
      (is (seq (get @seen "ENC_KEY_SECRET")))
      (finally
        (clear-role-env!)
        (doseq [f (reverse (file-seq dir))] (io/delete-file f true))))))

(deftest the-launcher-refuses-to-serve-as-freememo-net
  (with-redefs [peer/desktop? (constantly false) peer/local-install? (constantly false)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"refuses to start"
          (li/require-desktop-role!))))
  (with-redefs [peer/desktop? (constantly true) peer/local-install? (constantly false)]
    (is (thrown? clojure.lang.ExceptionInfo (li/require-desktop-role!))))
  (with-redefs [peer/desktop? (constantly true) peer/local-install? (constantly true)]
    (is (nil? (li/require-desktop-role!)))))

(deftest a-reload-of-the-callback-after-the-sign-in-opens-the-library
  (let [h (signin/wrap-signed-in-callback (fn [_] {:status 200 :body "app"}))]
    (testing "the sign-in ended on /callback?code=…; a reload goes to /"
      (let [r (h {:request-method :get :uri "/callback" :query-string "code=x&state=y"})]
        (is (= 302 (:status r)))
        (is (= "/" (get-in r [:headers "Location"])))))
    (is (= "app" (:body (h {:request-method :get :uri "/"}))))))
