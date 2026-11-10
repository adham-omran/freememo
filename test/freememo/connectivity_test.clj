(ns freememo.connectivity-test
  "Ticket 06 of plans/local-install.md: the JVM's own network check. No
   database: the check only reads and writes `freememo.desktop-link`."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [freememo.connectivity :as conn]
            [freememo.desktop-link :as link])
  (:import [com.sun.net.httpserver HttpHandler HttpServer]
           [java.net InetSocketAddress]))

(defn- reset-link [f]
  (link/configure! {:origin nil :token nil :user-id nil})
  (link/set-online! true)
  (f)
  (link/configure! {:origin nil :token nil :user-id nil})
  (link/set-online! true))

(use-fixtures :each reset-link)

(defn- stub-server
  "Post: a started HTTP server on a free port that answers `status`."
  ^HttpServer [port status]
  (doto (HttpServer/create (InetSocketAddress. "127.0.0.1" (int port)) 0)
    (.createContext "/" (reify HttpHandler
                          (handle [_ ex]
                            (.sendResponseHeaders ex status -1)
                            (.close ex))))
    (.start)))

(defn- free-port []
  (with-open [s (java.net.ServerSocket. 0)] (.getLocalPort s)))

(deftest reachable-means-any-http-answer
  (let [port (free-port)
        origin (str "http://127.0.0.1:" port)]
    (testing "nothing listens"
      (is (false? (conn/reachable? origin))))
    (testing "a 200 answer"
      (let [server (stub-server port 200)]
        (try (is (true? (conn/reachable? origin)))
             (finally (.stop server 0)))))
    (testing "a 503 answer still shows a live network"
      (let [server (stub-server port 503)]
        (try (is (true? (conn/reachable? origin)))
             (finally (.stop server 0)))))))

(deftest a-check-records-the-link-and-fires-the-reconnect-listener-once
  (let [reconnects (atom 0)
        offline-calls (atom 0)
        reachable (atom true)
        check #(conn/check! (fn [_] @reachable) (fn [] (swap! offline-calls inc)))]
    (link/configure! {:origin "http://freememo.test" :token "t" :user-id 1})
    (link/on-online! #(swap! reconnects inc))
    (testing "online stays online with no event"
      (is (true? (check)))
      (is (zero? @reconnects)))
    (testing "offline is recorded, and on-offline! runs once"
      (reset! reachable false)
      (is (false? (check)))
      (is (false? (link/online?)))
      (is (= 1 @offline-calls))
      (check)
      (is (= 1 @offline-calls) "a second offline check is not a change"))
    (testing "the way back runs the reconnect listeners once"
      (reset! reachable true)
      (is (true? (check)))
      (is (true? (link/online?)))
      (is (= 1 @reconnects))
      (check)
      (is (= 1 @reconnects)))))

(deftest a-check-without-an-origin-does-nothing
  (is (nil? (conn/check! (fn [_] (throw (ex-info "must not run" {}))) nil))))

(deftest a-failed-sync-reaches-every-listener-even-when-one-throws
  (let [seen (atom 0)]
    (link/on-sync-failed! (fn [] (throw (ex-info "bad listener" {}))))
    (link/on-sync-failed! #(swap! seen inc))
    (link/sync-failed!)
    (is (= 1 @seen))))
